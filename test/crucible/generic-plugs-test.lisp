;;;; test/crucible/generic-plugs-test.lisp — Tests for generic plugs
;;;; Phase 34E
(in-package :cauldron.test)

(defsuite :crucible-generic-plugs)

(deftest test-plug-json-content-type-exists
  (is (fboundp 'cauldron.crucible:plug-json-content-type)))

(deftest test-plug-html-content-type-exists
  (is (fboundp 'cauldron.crucible:plug-html-content-type)))

(deftest test-redirect-with-flash-exists
  (is (fboundp 'cauldron.crucible:redirect-with-flash)))

(deftest test-plug-json-content-type-sets-header
  (let ((conn (cauldron.crucible:make-conn :method :get :path "/")))
    (setf conn (cauldron.crucible:plug-json-content-type conn))
    (let ((ct (cdr (assoc "Content-Type" (cauldron.crucible:conn-resp-headers conn) :test #'string=))))
      (is (search "application/json" ct)))))

(deftest test-plug-html-content-type-sets-header
  (let ((conn (cauldron.crucible:make-conn :method :get :path "/")))
    (setf conn (cauldron.crucible:plug-html-content-type conn))
    (let ((ct (cdr (assoc "Content-Type" (cauldron.crucible:conn-resp-headers conn) :test #'string=))))
      (is (search "text/html" ct)))))
