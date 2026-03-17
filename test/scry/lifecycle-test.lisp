;;;; test/scry/lifecycle-test.lisp — Scry LiveView lifecycle tests
(in-package :cauldron.test)

(defsuite :scry-lifecycle)

;;; --- Helper: define a test counter view ---

(defun %define-test-counter-view ()
  "Define and return a counter view for testing. Cleans up after."
  (cauldron.scry:defscry test-counter-live
    :mount (lambda (params session socket)
             (declare (ignore params session socket))
             (list :count 0))
    :render (lambda (assigns)
              (cauldron.alembic:html
                (:div (:span (format nil "~D" (getf assigns :count))))))
    :handle-event (lambda (event assigns)
                    (cond
                      ((string-equal event "increment")
                       (list :count (1+ (getf assigns :count))))
                      ((string-equal event "decrement")
                       (list :count (1- (getf assigns :count))))
                      (t assigns)))))

;;; --- defscry + find-scry-view ---

(deftest test-scry-defscry-registers
  (%define-test-counter-view)
  (let ((view (cauldron.scry::find-scry-view 'test-counter-live)))
    (is-not-nil view "view registered in *scry-views*")
    (is-equal 'test-counter-live (cauldron.scry::scry-view-name view))
    (is-not-nil (cauldron.scry::scry-view-mount-fn view))
    (is-not-nil (cauldron.scry::scry-view-render-fn view))
    (is-not-nil (cauldron.scry::scry-view-handle-event-fn view))))

(deftest test-scry-find-view-unknown
  (is-nil (cauldron.scry::find-scry-view 'nonexistent-view-xyz)
          "unknown view returns NIL"))

;;; --- generate-session-id ---

(deftest test-scry-session-id-unique
  (let ((id1 (cauldron.scry::generate-session-id))
        (id2 (cauldron.scry::generate-session-id)))
    (is (stringp id1) "session ID is a string")
    (is (stringp id2))
    (is (not (string= id1 id2)) "session IDs are unique")
    (is (search "scry-" id1) "session ID contains prefix")))

;;; --- session CRUD ---

(deftest test-scry-session-register-find-remove
  (let ((sess (cauldron.scry:make-scry-session
               :assigns '(:x 1)
               :view 'test-view)))
    (let ((id (cauldron.scry:scry-session-id sess)))
      ;; Register
      (cauldron.scry::register-session sess)
      ;; Find
      (let ((found (cauldron.scry::find-session id)))
        (is-not-nil found "session found after register")
        (is-equal '(:x 1) (cauldron.scry:scry-session-assigns found)))
      ;; Remove
      (cauldron.scry::remove-session id)
      (is-nil (cauldron.scry::find-session id) "session gone after remove"))))

;;; --- scry-mount ---

(deftest test-scry-mount-returns-session-and-html
  (%define-test-counter-view)
  (multiple-value-bind (session html)
      (cauldron.scry:scry-mount 'test-counter-live nil)
    (is-not-nil session "mount returns a session")
    (is (typep session 'cauldron.scry:scry-session) "session is correct type")
    (is-equal '(:count 0) (cauldron.scry:scry-session-assigns session)
              "initial assigns from mount")
    (is (stringp html) "mount returns HTML string")
    (is (search "0" html) "HTML contains initial count")
    ;; Clean up
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

(deftest test-scry-mount-unknown-view-errors
  (signals-condition error
    (cauldron.scry:scry-mount 'totally-unknown-view nil)
    "mounting unknown view signals error"))

;;; --- scry-handle-event ---

(deftest test-scry-handle-event-updates-assigns
  (%define-test-counter-view)
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'test-counter-live nil)
    (declare (ignore _html))
    ;; Handle increment
    (multiple-value-bind (patches new-html)
        (cauldron.scry:scry-handle-event session "increment")
      (is-not-nil patches "handle-event returns patches")
      (is (stringp new-html) "returns new HTML")
      (is-equal '(:count 1) (cauldron.scry:scry-session-assigns session)
                "assigns updated to count=1"))
    ;; Clean up
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

;;; --- scry-render ---

(deftest test-scry-render-updates-last-tree
  (%define-test-counter-view)
  (multiple-value-bind (session _html)
      (cauldron.scry:scry-mount 'test-counter-live nil)
    (declare (ignore _html))
    (let ((tree1 (cauldron.scry::scry-session-last-tree session)))
      ;; Manually update assigns and re-render
      (setf (cauldron.scry:scry-session-assigns session) '(:count 99))
      (let ((new-tree (cauldron.scry:scry-render session)))
        (is-not-nil new-tree "render returns a tree")
        ;; last-tree should be updated
        (is (not (equal tree1 new-tree)) "last-tree updated after render")))
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))

;;; --- End-to-end lifecycle ---

(deftest test-scry-lifecycle-end-to-end
  "Mount counter at 0 → increment twice → verify patches and final state."
  (%define-test-counter-view)
  (multiple-value-bind (session html)
      (cauldron.scry:scry-mount 'test-counter-live nil)
    ;; Initial state
    (is (search "0" html) "initial HTML shows 0")
    (is-equal 0 (getf (cauldron.scry:scry-session-assigns session) :count))

    ;; First increment
    (multiple-value-bind (patches1 html1)
        (cauldron.scry:scry-handle-event session "increment")
      (declare (ignore patches1))
      (is (search "1" html1) "after first increment HTML shows 1")
      (is-equal 1 (getf (cauldron.scry:scry-session-assigns session) :count)))

    ;; Second increment
    (multiple-value-bind (patches2 html2)
        (cauldron.scry:scry-handle-event session "increment")
      (is-not-nil patches2 "second event produces patches")
      (is (search "2" html2) "after second increment HTML shows 2")
      (is-equal 2 (getf (cauldron.scry:scry-session-assigns session) :count)))

    ;; Decrement
    (cauldron.scry:scry-handle-event session "decrement")
    (is-equal 1 (getf (cauldron.scry:scry-session-assigns session) :count)
              "decrement works too")

    ;; Clean up
    (cauldron.scry::remove-session (cauldron.scry:scry-session-id session))))
