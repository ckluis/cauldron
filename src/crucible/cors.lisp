;;;; src/crucible/cors.lisp — CORS middleware plug for Crucible
(in-package :cauldron.crucible)

(defun make-plug-cors (&key (allowed-origins '("*"))
                             (allowed-methods '(:get :post :put :patch :delete :options))
                             (allowed-headers '("Content-Type" "Authorization" "X-CSRF-Token"))
                             (allow-credentials nil)
                             (max-age 86400)
                             (expose-headers '()))
  "Create CORS plug. Preflight OPTIONS requests get 204 with headers + halt.
Normal requests get Access-Control-Allow-Origin added.
Returns a plug function (conn) → conn."
  (let ((methods-str (format nil "~{~A~^, ~}"
                             (mapcar (lambda (m) (string-upcase (string m))) allowed-methods)))
        (headers-str (format nil "~{~A~^, ~}" allowed-headers))
        (expose-str (when expose-headers
                      (format nil "~{~A~^, ~}" expose-headers))))
    (lambda (conn)
      (let ((origin (cdr (assoc "Origin" (conn-headers conn) :test #'string-equal))))
        (if (null origin)
            ;; No Origin header — pass through
            conn
            (let ((matched-origin (match-origin origin allowed-origins)))
              (if (null matched-origin)
                  ;; Origin not allowed — pass through without CORS headers
                  conn
                  ;; Origin allowed
                  (if (eq (conn-method conn) :options)
                      ;; Preflight request
                      (progn
                        (conn-put-resp-header conn "Access-Control-Allow-Origin" matched-origin)
                        (conn-put-resp-header conn "Access-Control-Allow-Methods" methods-str)
                        (conn-put-resp-header conn "Access-Control-Allow-Headers" headers-str)
                        (conn-put-resp-header conn "Access-Control-Max-Age"
                                              (format nil "~D" max-age))
                        (when allow-credentials
                          (conn-put-resp-header conn "Access-Control-Allow-Credentials" "true"))
                        (when expose-str
                          (conn-put-resp-header conn "Access-Control-Expose-Headers" expose-str))
                        (conn-put-resp-body conn "")
                        (halt-conn conn :status 204))
                      ;; Normal request
                      (progn
                        (conn-put-resp-header conn "Access-Control-Allow-Origin" matched-origin)
                        (when allow-credentials
                          (conn-put-resp-header conn "Access-Control-Allow-Credentials" "true"))
                        (when expose-str
                          (conn-put-resp-header conn "Access-Control-Expose-Headers" expose-str))
                        (conn-put-resp-header conn "Vary" "Origin")
                        conn)))))))))

(defun match-origin (origin allowed-origins)
  "Return the origin string to use in Access-Control-Allow-Origin,
or NIL if ORIGIN is not in ALLOWED-ORIGINS.
Wildcard \"*\" matches any origin but returns \"*\"."
  (cond
    ((member "*" allowed-origins :test #'string=) "*")
    ((member origin allowed-origins :test #'string=) origin)
    (t nil)))
