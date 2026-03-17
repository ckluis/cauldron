;;;; src/cli/output.lisp — CLI output formatting
;;;; Supports JSON (for LLM consumption) and table (for humans).
;;;; Auto-detects format: JSON when stdout is not a TTY.

(in-package :cauldron.cli)

;;; ============================================================
;;; Output Format Detection
;;; ============================================================

(defvar *output-format* :auto
  "Output format: :auto, :json, or :table.
When :auto, uses :json if stdout is not a TTY, :table otherwise.")

(defun effective-format ()
  "Determine the actual output format to use."
  (case *output-format*
    (:auto (if (interactive-stream-p *standard-output*)
               :table
               :json))
    (otherwise *output-format*)))

;;; ============================================================
;;; Timing Support
;;; ============================================================

(defvar *emit-start-time* nil
  "When bound, holds the start time (internal-real-time) for timing output.")

(defmacro with-timing (&body body)
  "Execute BODY with *emit-start-time* bound for elapsed time tracking."
  `(let ((*emit-start-time* (get-internal-real-time)))
     ,@body))

;;; ============================================================
;;; Overflow Support
;;; ============================================================

(defvar *overflow-counter* 0
  "Counter for generating unique overflow temp file names.")

(defparameter *overflow-threshold* 200
  "Maximum number of rows to display before truncating.")

;;; ============================================================
;;; JSON Output
;;; ============================================================

(defun emit-json (data &key (stream *standard-output*) (pretty nil))
  "Write DATA as JSON to STREAM.
DATA can be a hash-table, alist, list, string, number, or boolean."
  (declare (ignore pretty))
  (let ((json (cauldron.json:encode data)))
    (write-string json stream)
    (terpri stream)
    (force-output stream)))

;;; ============================================================
;;; Table Output
;;; ============================================================

(defun emit-table (rows &key headers (stream *standard-output*))
  "Print ROWS as an aligned text table.
ROWS is a list of hash-tables or alists.
HEADERS is an optional list of column name strings; if omitted, derived from first row.
When rows > *overflow-threshold*, truncates display and writes full output to temp file."
  (when (null rows)
    (format stream "(no results)~%")
    (return-from emit-table))
  (let* ((total-rows (length rows))
         (overflow-p (> total-rows *overflow-threshold*))
         (display-rows (if overflow-p
                           (subseq rows 0 *overflow-threshold*)
                           rows))
         (cols (or headers (derive-headers (first rows))))
         (widths (compute-column-widths cols rows)))
    ;; Header
    (dolist (col cols)
      (format stream "~A" (pad-right (string-upcase col) (+ (column-width col cols widths) 2))))
    (terpri stream)
    ;; Separator
    (dolist (w widths)
      (format stream "~A  " (make-string w :initial-element #\-)))
    (terpri stream)
    ;; Data rows (possibly truncated)
    (dolist (row display-rows)
      (dolist (col cols)
        (let ((val (row-value row col)))
          (format stream "~A"
                  (pad-right (format nil "~A" (or val ""))
                             (+ (column-width col cols widths) 2)))))
      (terpri stream))
    ;; Overflow handling — write full output to temp file
    (when overflow-p
      (let ((file-id (incf *overflow-counter*)))
        (let ((path (format nil "/tmp/cauldron-output-~D.txt" file-id)))
          (let ((*overflow-threshold* most-positive-fixnum))
            (with-open-file (f path :direction :output :if-exists :supersede)
              (emit-table rows :headers headers :stream f)))
          (format stream "[Showing ~D of ~D rows. Full output: ~A]~%"
                  *overflow-threshold* total-rows path))))
    ;; Metadata footer
    (let ((elapsed (when *emit-start-time*
                     (round (* 1000 (/ (- (get-internal-real-time) *emit-start-time*)
                                       internal-time-units-per-second))))))
      (if elapsed
          (format stream "[rows:~D | ~Dms]~%" total-rows elapsed)
          (when (not overflow-p)
            ;; Only print row count footer when timing is bound
            nil)))
    (force-output stream)))

(defun derive-headers (row)
  "Extract column names from a row (hash-table or alist)."
  (etypecase row
    (hash-table
     (sort (loop for k being the hash-keys of row collect k) #'string<))
    (list
     (mapcar #'car row))))

(defun row-value (row col)
  "Get a column value from a row."
  (etypecase row
    (hash-table (gethash col row))
    (list (cdr (assoc col row :test #'string=)))))

(defun compute-column-widths (cols rows)
  "Compute minimum column widths from headers and data."
  (mapcar (lambda (col)
            (max (length col)
                 (loop for row in rows
                       maximize (length (format nil "~A" (or (row-value row col) ""))))))
          cols))

(defun column-width (col cols widths)
  "Get the width for COL."
  (let ((pos (position col cols :test #'string=)))
    (if pos (nth pos widths) (length col))))

(defun pad-right (string width)
  "Pad STRING with spaces to WIDTH."
  (if (>= (length string) width)
      string
      (concatenate 'string string
                   (make-string (- width (length string)) :initial-element #\Space))))

;;; ============================================================
;;; Error Output
;;; ============================================================

(defun emit-error (message &key use-hint (stream *standard-output*))
  "Emit an error message. In JSON mode, emits {\"status\":\"error\",\"message\":\"...\"}.
In table mode, prints Error: message. If USE-HINT provided, appends usage hint.
Always returns exit code 1."
  (case (effective-format)
    (:json
     (let ((envelope (make-hash-table :test 'equal)))
       (setf (gethash "status" envelope) "error")
       (setf (gethash "message" envelope) message)
       (when use-hint
         (setf (gethash "hint" envelope) use-hint))
       (emit-json envelope :stream stream)))
    (:table
     (format stream "Error: ~A~%" message)
     (when use-hint
       (format stream "Use: ~A~%" use-hint))
     (force-output stream)))
  1)

;;; ============================================================
;;; Universal Emit
;;; ============================================================

(defun emit (&key data message (status "ok") (stream *standard-output*))
  "Emit output in the appropriate format.
DATA is the payload (hash-table, list of hash-tables, etc.).
MESSAGE is an optional human-readable message.
STATUS is \"ok\" or \"error\"."
  (case (effective-format)
    (:json
     (let ((envelope (make-hash-table :test 'equal)))
       (setf (gethash "status" envelope) status)
       (when message (setf (gethash "message" envelope) message))
       (when data (setf (gethash "data" envelope) data))
       (emit-json envelope :stream stream)))
    (:table
     (when message
       (format stream "~A~%" message))
     (when data
       (etypecase data
         (hash-table
          ;; Single record — show as key: value pairs
          (maphash (lambda (k v)
                     (format stream "~A: ~A~%" k v))
                   data))
         (list
          (if (and (first data)
                   (or (hash-table-p (first data))
                       (consp (first (first data)))))
              (emit-table data :stream stream)
              (dolist (item data)
                (format stream "~A~%" item)))))))))
