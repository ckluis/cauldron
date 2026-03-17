;;;; test/crucible/pipeline-test.lisp — Pipeline and conn tests
(in-package :cauldron.test)

(defsuite :crucible-pipeline)

;;; --- Conn creation and field access ---

(deftest test-conn-creation
  (let ((conn (cauldron.crucible:make-conn :method :get :path "/test")))
    (is-equal :get (cauldron.crucible:conn-method conn))
    (is-equal "/test" (cauldron.crucible:conn-path conn))
    (is-equal 200 (cauldron.crucible:conn-status conn))))

(deftest test-conn-defaults
  (let ((conn (cauldron.crucible:make-conn)))
    (is-equal :get (cauldron.crucible:conn-method conn))
    (is-equal "/" (cauldron.crucible:conn-path conn))
    (is-nil (cauldron.crucible:conn-halted-p conn))))

;;; --- Assigns ---

(deftest test-conn-assigns
  (let ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-assign conn :user "Alice")
    (is-equal "Alice" (cauldron.crucible:conn-get-assign conn :user))))

(deftest test-conn-assign-returns-conn
  (let ((conn (cauldron.crucible:make-conn)))
    (let ((result (cauldron.crucible:conn-put-assign conn :key "val")))
      (is (eq conn result)))))

;;; --- Status and response ---

(deftest test-conn-put-status
  (let ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-status conn 404)
    (is-equal 404 (cauldron.crucible:conn-status conn))))

(deftest test-conn-put-resp-body
  (let ((conn (cauldron.crucible:make-conn)))
    (cauldron.crucible:conn-put-resp-body conn "hello")
    (is-equal "hello" (cauldron.crucible:conn-resp-body conn))))

;;; --- Pipeline runs plugs in order ---

(deftest test-pipeline-order
  (let ((order '()))
    (cauldron.crucible:make-pipeline
     :test-order
     (list (lambda (conn) (push 1 order) conn)
           (lambda (conn) (push 2 order) conn)
           (lambda (conn) (push 3 order) conn)))
    (let ((conn (cauldron.crucible:make-conn)))
      (cauldron.crucible:run-pipeline :test-order conn)
      (is-equal '(3 2 1) order))))

;;; --- Halted conn stops pipeline ---

(deftest test-halt-stops-pipeline
  (let ((ran-after nil))
    (cauldron.crucible:make-pipeline
     :test-halt
     (list (lambda (conn)
             (cauldron.crucible:halt-conn conn :status 403 :body "forbidden"))
           (lambda (conn)
             (setf ran-after t)
             conn)))
    (let* ((conn (cauldron.crucible:make-conn))
           (result (cauldron.crucible:run-pipeline :test-halt conn)))
      (is-true (cauldron.crucible:conn-halted-p result))
      (is-equal 403 (cauldron.crucible:conn-status result))
      (is-equal "forbidden" (cauldron.crucible:conn-resp-body result))
      (is-nil ran-after))))

;;; --- Pipeline modifies conn ---

(deftest test-pipeline-modifies-conn
  (cauldron.crucible:make-pipeline
   :test-modify
   (list (lambda (conn)
           (cauldron.crucible:conn-put-assign conn :processed t)
           (cauldron.crucible:conn-put-status conn 201)
           conn)))
  (let* ((conn (cauldron.crucible:make-conn))
         (result (cauldron.crucible:run-pipeline :test-modify conn)))
    (is-true (cauldron.crucible:conn-get-assign result :processed))
    (is-equal 201 (cauldron.crucible:conn-status result))))
