;;;; test/crucible/compress-test.lisp — Tests for response compression plug
(in-package :cauldron.test)

(defsuite :crucible-compress)

(deftest test-compress-html-response
  (let* ((body (make-string 500 :initial-element #\A))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Accept-Encoding" . "gzip, deflate"))
                :status 200
                :resp-headers '(("Content-Type" . "text/html"))))
         (conn (cauldron.crucible:conn-put-resp-body conn body)))
    (cauldron.crucible:compress-response conn)
    (is-not-nil (assoc "Content-Encoding" (cauldron.crucible:conn-resp-headers conn)
                       :test #'string=))
    (is-equal "gzip" (cdr (assoc "Content-Encoding" (cauldron.crucible:conn-resp-headers conn)
                                 :test #'string=)))))

(deftest test-compress-binary-skipped
  ;; image/png should not be compressed
  (let* ((body (make-string 500 :initial-element #\A))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/image.png"
                :headers '(("Accept-Encoding" . "gzip"))
                :status 200
                :resp-headers '(("Content-Type" . "image/png"))))
         (conn (cauldron.crucible:conn-put-resp-body conn body)))
    (cauldron.crucible:compress-response conn)
    (is-nil (assoc "Content-Encoding" (cauldron.crucible:conn-resp-headers conn)
                   :test #'string=))))

(deftest test-compress-small-body-skipped
  ;; Body under threshold should not be compressed
  (let* ((conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Accept-Encoding" . "gzip"))
                :status 200
                :resp-headers '(("Content-Type" . "text/html"))))
         (conn (cauldron.crucible:conn-put-resp-body conn "small")))
    (cauldron.crucible:compress-response conn)
    (is-nil (assoc "Content-Encoding" (cauldron.crucible:conn-resp-headers conn)
                   :test #'string=))))

(deftest test-compress-no-accept-encoding-skipped
  (let* ((body (make-string 500 :initial-element #\A))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '()
                :status 200
                :resp-headers '(("Content-Type" . "text/html"))))
         (conn (cauldron.crucible:conn-put-resp-body conn body)))
    (cauldron.crucible:compress-response conn)
    (is-nil (assoc "Content-Encoding" (cauldron.crucible:conn-resp-headers conn)
                   :test #'string=))))

(deftest test-compress-vary-header-added
  (let* ((body (make-string 500 :initial-element #\A))
         (conn (cauldron.crucible:make-conn
                :method :get :path "/"
                :headers '(("Accept-Encoding" . "gzip"))
                :status 200
                :resp-headers '(("Content-Type" . "text/html"))))
         (conn (cauldron.crucible:conn-put-resp-body conn body)))
    (cauldron.crucible:compress-response conn)
    (is-not-nil (assoc "Vary" (cauldron.crucible:conn-resp-headers conn)
                       :test #'string=))))
