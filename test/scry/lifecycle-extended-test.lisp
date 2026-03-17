;;;; test/scry/lifecycle-extended-test.lisp — Extended Scry LiveView lifecycle tests
(in-package :cauldron.test)

(defsuite :scry-lifecycle-extended)

;;; --- Helper: define views for extended testing ---

(defun %define-extended-test-view ()
  "Define a view with all callbacks for extended lifecycle tests."
  (cauldron.scry:defscry ext-counter-live
    :mount (lambda (params session socket)
             (declare (ignore socket))
             (list :count (or (getf params :initial) 0)
                   :session-data session))
    :render (lambda (assigns)
              (cauldron.alembic:html
                (:div (:span (format nil "~D" (getf assigns :count))))))
    :handle-event (lambda (event assigns)
                    (cond
                      ((string-equal event "increment")
                       (list :count (1+ (getf assigns :count))))
                      ((string-equal event "reset")
                       (list :count 0))
                      (t assigns)))
    :handle-info (lambda (info assigns)
                   (if (getf info :new-count)
                       (list :count (getf info :new-count))
                       assigns))))

;;; --- defscry with nil callbacks ---

(deftest test-defscry-nil-mount
  "A view with no mount function should still register."
  (cauldron.scry:defscry ext-no-mount-view
    :render (lambda (assigns)
              (cauldron.alembic:html (:p "static"))))
  (let ((view (cauldron.scry::find-scry-view 'ext-no-mount-view)))
    (is-not-nil view "view registered")
    (is-nil (cauldron.scry::scry-view-mount-fn view) "mount-fn is nil")
    (is-not-nil (cauldron.scry::scry-view-render-fn view) "render-fn is set")))

(deftest test-defscry-nil-render
  "A view with no render function should register."
  (cauldron.scry:defscry ext-no-render-view
    :mount (lambda (params session socket)
             (declare (ignore params session socket))
             (list :x 1)))
  (let ((view (cauldron.scry::find-scry-view 'ext-no-render-view)))
    (is-not-nil view)
    (is-not-nil (cauldron.scry::scry-view-mount-fn view))
    (is-nil (cauldron.scry::scry-view-render-fn view))))

(deftest test-defscry-nil-handle-event
  "A view with no handle-event function should register."
  (cauldron.scry:defscry ext-no-event-view
    :mount (lambda (p s sk) (declare (ignore p s sk)) (list :x 1))
    :render (lambda (assigns)
              (cauldron.alembic:html (:p (format nil "~A" (getf assigns :x))))))
  (let ((view (cauldron.scry::find-scry-view 'ext-no-event-view)))
    (is-not-nil view)
    (is-nil (cauldron.scry::scry-view-handle-event-fn view))))

(deftest test-defscry-nil-handle-info
  "A view with no handle-info function should register."
  (%define-extended-test-view)
  (let ((view (cauldron.scry::find-scry-view 'ext-counter-live)))
    (is-not-nil (cauldron.scry::scry-view-handle-info-fn view) "handle-info-fn set")))

(deftest test-defscry-returns-name
  "defscry should return the view name symbol."
  (let ((result (cauldron.scry:defscry ext-return-test
                  :mount (lambda (p s sk) (declare (ignore p s sk)) nil))))
    (is-equal 'ext-return-test result)))

;;; --- scry-mount with various assign combinations ---

(deftest test-scry-mount-nil-assigns
  "Mount with no mount function produces nil assigns."
  (cauldron.scry:defscry ext-nil-assigns-view
    :render (lambda (assigns)
              (declare (ignore assigns))
              (cauldron.alembic:html (:p "empty"))))
  (multiple-value-bind (session html)
      (cauldron.scry:scry-mount 'ext-nil-assigns-view nil)
    (is-nil (cauldron.scry:scry-session-assigns session))
    (is (stringp html))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-mount-with-params
  "Mount function receives params and uses them for initial assigns."
  (%define-extended-test-view)
  (multiple-value-bind (session html)
      (cauldron.scry:scry-mount 'ext-counter-live '(:initial 42))
    (is-equal 42 (getf (cauldron.scry:scry-session-assigns session) :count))
    (is (search "42" html) "HTML contains initial count from params")
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-mount-with-session-data
  "Mount function receives session keyword arg."
  (%define-extended-test-view)
  (multiple-value-bind (session html)
      (cauldron.scry:scry-mount 'ext-counter-live nil :session '(:user "alice"))
    (declare (ignore html))
    (is-equal '(:user "alice")
              (getf (cauldron.scry:scry-session-assigns session) :session-data))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-mount-empty-html-without-render
  "Mount with no render function produces empty HTML string."
  (cauldron.scry:defscry ext-no-render-mount-view
    :mount (lambda (p s sk) (declare (ignore p s sk)) (list :val 99)))
  (multiple-value-bind (session html)
      (cauldron.scry:scry-mount 'ext-no-render-mount-view nil)
    (is-equal "" html "empty HTML when no render-fn")
    (is-equal 99 (getf (cauldron.scry:scry-session-assigns session) :val))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

;;; --- scry-handle-event edge cases ---

(deftest test-scry-handle-event-no-handler
  "Handle-event on a view with no handle-event-fn leaves assigns unchanged."
  (cauldron.scry:defscry ext-no-handler-view
    :mount (lambda (p s sk) (declare (ignore p s sk)) (list :count 5))
    :render (lambda (assigns)
              (cauldron.alembic:html
                (:span (format nil "~D" (getf assigns :count))))))
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'ext-no-handler-view nil)
    (declare (ignore _html))
    (cauldron.scry:scry-handle-event session "anything")
    (is-equal 5 (getf (cauldron.scry:scry-session-assigns session) :count)
              "assigns unchanged when no handle-event-fn")
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-handle-event-unknown-event-returns-assigns
  "An unhandled event string causes the fallback branch to return assigns as-is."
  (%define-extended-test-view)
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'ext-counter-live nil)
    (declare (ignore _html))
    (cauldron.scry:scry-handle-event session "unknown-event")
    (is-equal 0 (getf (cauldron.scry:scry-session-assigns session) :count)
              "assigns unchanged for unknown event")
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-handle-event-reset
  "Reset event sets count back to 0."
  (%define-extended-test-view)
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'ext-counter-live '(:initial 10))
    (declare (ignore _html))
    (is-equal 10 (getf (cauldron.scry:scry-session-assigns session) :count))
    (cauldron.scry:scry-handle-event session "reset")
    (is-equal 0 (getf (cauldron.scry:scry-session-assigns session) :count))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

;;; --- scry-handle-info ---

(deftest test-scry-handle-info-updates-assigns
  "handle-info with :new-count updates the session assigns."
  (%define-extended-test-view)
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'ext-counter-live nil)
    (declare (ignore _html))
    (cauldron.scry::scry-handle-info session (list :new-count 77))
    (is-equal 77 (getf (cauldron.scry:scry-session-assigns session) :count))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-handle-info-no-change
  "handle-info with no :new-count keeps assigns unchanged."
  (%define-extended-test-view)
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'ext-counter-live nil)
    (declare (ignore _html))
    (cauldron.scry::scry-handle-info session (list :unrelated "data"))
    (is-equal 0 (getf (cauldron.scry:scry-session-assigns session) :count))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

;;; --- Session struct accessors ---

(deftest test-scry-session-struct-accessors
  "Verify session struct fields are accessible."
  (let ((sess (cauldron.scry:make-scry-session
               :assigns '(:a 1 :b 2)
               :view 'test-view
               :socket :fake-socket)))
    (is-equal '(:a 1 :b 2) (cauldron.scry:scry-session-assigns sess))
    (is-equal 'test-view (cauldron.scry:scry-session-view sess))
    (is-equal :fake-socket (cauldron.scry:scry-session-socket sess))
    (is-nil (cauldron.scry:scry-session-subscriptions sess))
    (is (stringp (cauldron.scry:scry-session-id sess)))))
