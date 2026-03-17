;;;; test/http/multipart-test.lisp — Tests for multipart form-data parsing
(in-package :cauldron.test)

(defsuite :http-multipart)

;;; ============================================================
;;; Boundary extraction
;;; ============================================================

(deftest test-extract-boundary-basic
  (is-equal "----WebKitFormBoundary"
            (cauldron.http:extract-boundary
             "multipart/form-data; boundary=----WebKitFormBoundary")))

(deftest test-extract-boundary-quoted
  (is-equal "abc123"
            (cauldron.http:extract-boundary
             "multipart/form-data; boundary=\"abc123\"")))

(deftest test-extract-boundary-nil-for-non-multipart
  (is-nil (cauldron.http:extract-boundary "application/json")))

(deftest test-extract-boundary-nil-for-nil
  (is-nil (cauldron.http:extract-boundary nil)))

(deftest test-extract-boundary-case-insensitive
  (is-not-nil (cauldron.http:extract-boundary
               "Multipart/Form-Data; boundary=xyz")))

;;; ============================================================
;;; Multipart parsing
;;; ============================================================

(defun make-multipart-body (boundary parts)
  "Build a multipart body string from BOUNDARY and PARTS.
Each part is (name value &key filename content-type)."
  (with-output-to-string (out)
    (dolist (part parts)
      (destructuring-bind (name value &key filename content-type) part
        (format out "--~A~C~C" boundary #\Return #\Newline)
        (if filename
            (format out "Content-Disposition: form-data; name=\"~A\"; filename=\"~A\"~C~C"
                    name filename #\Return #\Newline)
            (format out "Content-Disposition: form-data; name=\"~A\"~C~C"
                    name #\Return #\Newline))
        (when content-type
          (format out "Content-Type: ~A~C~C" content-type #\Return #\Newline))
        (format out "~C~C" #\Return #\Newline)
        (format out "~A~C~C" value #\Return #\Newline)))
    (format out "--~A--~C~C" boundary #\Return #\Newline)))

(deftest test-parse-multipart-single-field
  (let* ((boundary "testboundary")
         (body (make-multipart-body boundary '(("name" "Alice"))))
         (parts (cauldron.http:parse-multipart body boundary)))
    (is-equal 1 (length parts))
    (is-equal "name" (cauldron.http:multipart-part-name (first parts)))
    (is-equal "Alice" (cauldron.http:multipart-part-body (first parts)))
    (is-nil (cauldron.http:multipart-part-filename (first parts)))))

(deftest test-parse-multipart-multiple-fields
  (let* ((boundary "myboundary")
         (body (make-multipart-body boundary
                 '(("first" "John") ("last" "Doe") ("age" "30"))))
         (parts (cauldron.http:parse-multipart body boundary)))
    (is-equal 3 (length parts))
    (is-equal "first" (cauldron.http:multipart-part-name (first parts)))
    (is-equal "John" (cauldron.http:multipart-part-body (first parts)))
    (is-equal "last" (cauldron.http:multipart-part-name (second parts)))
    (is-equal "Doe" (cauldron.http:multipart-part-body (second parts)))))

(deftest test-parse-multipart-file-upload
  (let* ((boundary "fileboundary")
         (body (make-multipart-body boundary
                 '(("file" "file-contents"
                    :filename "test.txt"
                    :content-type "text/plain"))))
         (parts (cauldron.http:parse-multipart body boundary)))
    (is-equal 1 (length parts))
    (let ((part (first parts)))
      (is-equal "file" (cauldron.http:multipart-part-name part))
      (is-equal "test.txt" (cauldron.http:multipart-part-filename part))
      (is-equal "text/plain" (cauldron.http:multipart-part-content-type part))
      ;; File bodies are octet vectors
      (is (typep (cauldron.http:multipart-part-body part) '(vector (unsigned-byte 8)))))))

(deftest test-parse-multipart-mixed-fields-and-files
  (let* ((boundary "mixedboundary")
         (body (make-multipart-body boundary
                 '(("title" "My Document")
                   ("file" "binary-data"
                    :filename "doc.pdf"
                    :content-type "application/pdf"))))
         (parts (cauldron.http:parse-multipart body boundary)))
    (is-equal 2 (length parts))
    ;; Text field
    (is-nil (cauldron.http:multipart-part-filename (first parts)))
    (is-equal "My Document" (cauldron.http:multipart-part-body (first parts)))
    ;; File field
    (is-equal "doc.pdf" (cauldron.http:multipart-part-filename (second parts)))))

(deftest test-parse-multipart-empty-body
  (let ((parts (cauldron.http:parse-multipart "" "boundary")))
    (is-nil parts)))

(deftest test-parse-multipart-no-match
  (let ((parts (cauldron.http:parse-multipart "no boundary here" "boundary")))
    (is-nil parts)))

(deftest test-parse-multipart-octets-input
  (let* ((boundary "octboundary")
         (body-str (make-multipart-body boundary '(("key" "value"))))
         (body-octets (let* ((len (length body-str))
                             (arr (make-array len :element-type '(unsigned-byte 8))))
                        (dotimes (i len arr)
                          (setf (aref arr i) (char-code (char body-str i))))))
         (parts (cauldron.http:parse-multipart body-octets boundary)))
    (is-equal 1 (length parts))
    (is-equal "key" (cauldron.http:multipart-part-name (first parts)))))

(deftest test-parse-multipart-empty-value
  (let* ((boundary "emptyboundary")
         (body (make-multipart-body boundary '(("field" ""))))
         (parts (cauldron.http:parse-multipart body boundary)))
    (is-equal 1 (length parts))
    (is-equal "" (cauldron.http:multipart-part-body (first parts)))))

;;; ============================================================
;;; plug-parse-body multipart integration
;;; ============================================================

(deftest test-parse-body-multipart
  (let* ((boundary "formboundary")
         (body (make-multipart-body boundary
                 '(("name" "Bob")
                   ("avatar" "imgdata"
                    :filename "avatar.png"
                    :content-type "image/png"))))
         (conn (cauldron.crucible:make-conn
                :method :post
                :path "/upload"
                :headers (list (cons "Content-Type"
                                     (format nil "multipart/form-data; boundary=~A" boundary)))
                :body body))
         (result (cauldron.crucible:plug-parse-body conn))
         (parsed-body (cauldron.crucible:conn-body result)))
    ;; Body should be alist of non-file fields
    (is (listp parsed-body))
    (is-equal "Bob" (cdr (assoc "name" parsed-body :test #'string=)))
    ;; File parts should be in :multipart-parts assign
    (let ((parts (cauldron.crucible:conn-get-assign result :multipart-parts)))
      (is-not-nil parts)
      (is-equal 2 (length parts)))))
