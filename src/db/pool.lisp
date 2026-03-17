;;;; src/db/pool.lisp — Connection pooling with health checking and leak detection
(in-package :cauldron.db)

;;; ============================================================
;;; Pool Struct
;;; ============================================================

(defstruct pg-pool
  "A connection pool for PostgreSQL connections."
  ;; Configuration
  (host     "127.0.0.1" :type string)
  (port     5432 :type integer)
  (database "" :type string)
  (user     "" :type string)
  (password "" :type string)
  (min-size 2  :type integer)
  (max-size 10 :type integer)
  ;; State
  (idle-connections   '() :type list)
  (active-count       0   :type integer)
  (total-created      0   :type integer)
  (lock               (cauldron.runtime:make-lock "pg-pool-lock"))
  (semaphore          (cauldron.runtime:make-semaphore :count 0 :name "pg-pool-available"))
  (shutdown-p         nil :type boolean)
  ;; Stats
  (total-checkouts    0   :type integer)
  (total-checkins     0   :type integer)
  (total-errors       0   :type integer)
  (total-timeouts     0   :type integer)
  ;; Leak detection
  (leak-threshold-seconds 30 :type integer)
  (active-connections '() :type list)) ; list of (conn . checkout-time)

;;; ============================================================
;;; Pool Creation
;;; ============================================================

(defun make-pool (&key (host "127.0.0.1") (port 5432) database user (password "")
                       (min-size 2) (max-size 10)
                       (leak-threshold-seconds 30))
  "Create a connection pool and warm it up with MIN-SIZE connections."
  (unless database (error "make-pool: :database is required"))
  (unless user (error "make-pool: :user is required"))
  (when (< max-size min-size)
    (error "make-pool: max-size (~D) must be >= min-size (~D)" max-size min-size))
  (let ((pool (make-pg-pool
               :host host
               :port port
               :database database
               :user user
               :password password
               :min-size min-size
               :max-size max-size
               :leak-threshold-seconds leak-threshold-seconds)))
    ;; Warm up pool with min-size connections
    (loop for i from 0 below min-size
          do (handler-case
                 (let ((conn (create-pool-connection pool)))
                   (cauldron.runtime:with-lock ((pg-pool-lock pool))
                     (push conn (pg-pool-idle-connections pool)))
                   (cauldron.runtime:signal-semaphore (pg-pool-semaphore pool)))
               (error (e)
                 (format *error-output*
                         "~&[pg-pool] Warning: failed to create warm-up connection ~D: ~A~%"
                         i e))))
    pool))

(defun create-pool-connection (pool)
  "Create a new connection for the pool."
  (let ((conn (connect :host (pg-pool-host pool)
                       :port (pg-pool-port pool)
                       :database (pg-pool-database pool)
                       :user (pg-pool-user pool)
                       :password (pg-pool-password pool))))
    (cauldron.runtime:with-lock ((pg-pool-lock pool))
      (incf (pg-pool-total-created pool)))
    conn))

;;; ============================================================
;;; Checkout / Checkin
;;; ============================================================

(defun checkout (pool &key (timeout 5))
  "Get a connection from the pool. Creates a new one if under max-size.
Blocks up to TIMEOUT seconds if at capacity. Returns a pg-connection or signals an error."
  (when (pg-pool-shutdown-p pool)
    (error "Cannot checkout from a shut-down pool"))
  ;; First, try to get an idle connection or create a new one
  (let ((conn nil))
    ;; Try fast path: grab from idle list
    (cauldron.runtime:with-lock ((pg-pool-lock pool))
      (when (pg-pool-idle-connections pool)
        (setf conn (pop (pg-pool-idle-connections pool)))))
    ;; If no idle connection, try creating a new one
    (unless conn
      (let ((can-create nil))
        (cauldron.runtime:with-lock ((pg-pool-lock pool))
          (when (< (+ (pg-pool-active-count pool)
                      (length (pg-pool-idle-connections pool)))
                   (pg-pool-max-size pool))
            (setf can-create t)))
        (if can-create
            (handler-case
                (setf conn (create-pool-connection pool))
              (error (e)
                (cauldron.runtime:with-lock ((pg-pool-lock pool))
                  (incf (pg-pool-total-errors pool)))
                (error "Failed to create pool connection: ~A" e)))
            ;; At max capacity — wait for a return
            (progn
              (let ((acquired (cauldron.runtime:wait-semaphore
                               (pg-pool-semaphore pool) :timeout timeout)))
                (unless acquired
                  (cauldron.runtime:with-lock ((pg-pool-lock pool))
                    (incf (pg-pool-total-timeouts pool)))
                  (error "Connection pool checkout timed out after ~D seconds" timeout))
                ;; Grab from idle list
                (cauldron.runtime:with-lock ((pg-pool-lock pool))
                  (setf conn (pop (pg-pool-idle-connections pool)))))))))
    ;; Validate connection health
    (when conn
      (unless (validate-pool-connection conn)
        ;; Connection is dead, try to create a new one
        (ignore-errors (disconnect conn))
        (handler-case
            (setf conn (create-pool-connection pool))
          (error (e)
            (cauldron.runtime:with-lock ((pg-pool-lock pool))
              (incf (pg-pool-total-errors pool)))
            (error "Failed to replace dead pool connection: ~A" e)))))
    (unless conn
      (error "Failed to obtain a connection from the pool"))
    ;; Track checkout
    (let ((now (get-internal-real-time)))
      (setf (pg-connection-checkout-time conn) now)
      (cauldron.runtime:with-lock ((pg-pool-lock pool))
        (incf (pg-pool-active-count pool))
        (incf (pg-pool-total-checkouts pool))
        (push (cons conn now) (pg-pool-active-connections pool))))
    conn))

(defun checkin (pool conn)
  "Return a connection to the pool. Validates it's still usable."
  (when (pg-pool-shutdown-p pool)
    (ignore-errors (disconnect conn))
    (return-from checkin))
  ;; Remove from active list
  (cauldron.runtime:with-lock ((pg-pool-lock pool))
    (decf (pg-pool-active-count pool))
    (incf (pg-pool-total-checkins pool))
    (setf (pg-pool-active-connections pool)
          (remove conn (pg-pool-active-connections pool) :key #'car)))
  ;; Check for leak warning
  (let* ((now (get-internal-real-time))
         (held-seconds (/ (- now (pg-connection-checkout-time conn))
                          internal-time-units-per-second)))
    (when (> held-seconds (pg-pool-leak-threshold-seconds pool))
      (format *error-output*
              "~&[pg-pool] WARNING: connection held for ~,1F seconds (threshold: ~D)~%"
              held-seconds (pg-pool-leak-threshold-seconds pool))))
  ;; If connection is still alive and not in error transaction state, return to pool
  (cond
    ((and (pg-connection-alive-p conn)
          (not (char= (pg-connection-transaction-status conn) #\E)))
     ;; Reset connection state if in a transaction
     (when (pg-connection-in-transaction conn)
       (ignore-errors (execute-sql conn "ROLLBACK")))
     ;; Reset search_path to prevent tenant data leakage
     (ignore-errors (execute-sql conn "SET search_path TO public"))
     (cauldron.runtime:with-lock ((pg-pool-lock pool))
       (push conn (pg-pool-idle-connections pool)))
     (cauldron.runtime:signal-semaphore (pg-pool-semaphore pool)))
    (t
     ;; Connection is dead or in error state — discard it
     (ignore-errors (disconnect conn)))))

(defun validate-pool-connection (conn)
  "Quick validation that a connection is still alive.
Does NOT run a query — just checks the struct flag."
  (pg-connection-alive-p conn))

;;; ============================================================
;;; Pool Macro
;;; ============================================================

(defmacro with-pool-connection ((var pool) &body body)
  "Execute BODY with VAR bound to a connection checked out from POOL.
Ensures the connection is returned to the pool."
  (let ((pool-var (gensym "POOL")))
    `(let* ((,pool-var ,pool)
            (,var (checkout ,pool-var)))
       (unwind-protect (progn ,@body)
         (when ,var (checkin ,pool-var ,var))))))

;;; ============================================================
;;; Tenant-Aware Pool Access
;;; ============================================================

(defmacro with-tenant-connection ((var pool schema-name) &body body)
  "Execute BODY with VAR bound to a pool connection whose search_path is set
to SCHEMA-NAME (plus public as fallback). Resets search_path on return."
  (let ((pool-var (gensym "POOL"))
        (schema-var (gensym "SCHEMA")))
    `(let ((,pool-var ,pool)
           (,schema-var ,schema-name))
       (with-pool-connection (,var ,pool-var)
         (execute-sql ,var (format nil "SET search_path TO ~A, public" ,schema-var))
         ,@body))))

;;; ============================================================
;;; Pool Stats & Management
;;; ============================================================

(defun pool-stats (pool)
  "Return a hash-table of pool statistics."
  (let ((stats (make-hash-table :test #'equal)))
    (cauldron.runtime:with-lock ((pg-pool-lock pool))
      (setf (gethash "total-created" stats)   (pg-pool-total-created pool))
      (setf (gethash "idle" stats)            (length (pg-pool-idle-connections pool)))
      (setf (gethash "active" stats)          (pg-pool-active-count pool))
      (setf (gethash "total-checkouts" stats) (pg-pool-total-checkouts pool))
      (setf (gethash "total-checkins" stats)  (pg-pool-total-checkins pool))
      (setf (gethash "total-errors" stats)    (pg-pool-total-errors pool))
      (setf (gethash "total-timeouts" stats)  (pg-pool-total-timeouts pool))
      (setf (gethash "max-size" stats)        (pg-pool-max-size pool))
      (setf (gethash "min-size" stats)        (pg-pool-min-size pool)))
    stats))

(defun shutdown-pool (pool)
  "Shut down the pool: disconnect all idle connections, mark as shut down."
  (cauldron.runtime:with-lock ((pg-pool-lock pool))
    (setf (pg-pool-shutdown-p pool) t))
  ;; Disconnect all idle connections
  (let ((idle nil))
    (cauldron.runtime:with-lock ((pg-pool-lock pool))
      (setf idle (pg-pool-idle-connections pool))
      (setf (pg-pool-idle-connections pool) nil))
    (dolist (conn idle)
      (ignore-errors (disconnect conn))))
  ;; Warn about active connections
  (let ((active nil))
    (cauldron.runtime:with-lock ((pg-pool-lock pool))
      (setf active (pg-pool-active-connections pool)))
    (when active
      (format *error-output*
              "~&[pg-pool] WARNING: ~D connections still active at shutdown~%"
              (length active))))
  (values))
