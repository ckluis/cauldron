;;;; src/alembic/escape.lisp — HTML entity escaping

(in-package :cauldron.alembic)

(defun escape-html (string)
  "Escape HTML special characters in STRING.
Escapes: & < > \" '  (the five required by OWASP)."
  (if (not (stringp string))
      (escape-html (princ-to-string string))
      (let ((needs-escape nil))
        ;; Fast path: check if escaping is needed at all
        (loop for char across string
              when (member char '(#\& #\< #\> #\" #\'))
                do (setf needs-escape t) (return))
        (if (not needs-escape)
            string
            (with-output-to-string (out)
              (loop for char across string
                    do (case char
                         (#\& (write-string "&amp;" out))
                         (#\< (write-string "&lt;" out))
                         (#\> (write-string "&gt;" out))
                         (#\" (write-string "&quot;" out))
                         (#\' (write-string "&#x27;" out))
                         (otherwise (write-char char out)))))))))

(defun escape-attribute-value (value)
  "Escape a value for use in an HTML attribute. Returns escaped string."
  (escape-html (if (stringp value) value (princ-to-string value))))
