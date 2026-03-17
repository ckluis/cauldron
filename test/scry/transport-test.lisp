;;;; test/scry/transport-test.lisp — Scry wire format encoding/decoding tests
(in-package :cauldron.test)

(defsuite :scry-transport)

;;; --- encode-patch-for-wire ---

(deftest test-encode-patch-replace
  (let ((result (cauldron.scry::encode-patch-for-wire '(:replace (0 1) "new"))))
    (is-equal "replace" (first result))
    (is-equal '(0 1) (second result))
    (is-equal "new" (third result))))

(deftest test-encode-patch-insert
  (let ((result (cauldron.scry::encode-patch-for-wire '(:insert (2) "added"))))
    (is-equal "insert" (first result))
    (is-equal '(2) (second result))
    (is-equal "added" (third result))))

(deftest test-encode-patch-remove
  (let ((result (cauldron.scry::encode-patch-for-wire '(:remove (3)))))
    (is-equal "remove" (first result))
    (is-equal '(3) (second result))))

(deftest test-encode-patch-nil-path
  "NIL path becomes empty list."
  (let ((result (cauldron.scry::encode-patch-for-wire '(:replace nil "x"))))
    (is-equal '() (second result))))

;;; --- encode-patches ---

(deftest test-encode-patches-single
  (let ((json (cauldron.scry:encode-patches '((:replace (0) "val")))))
    (is (stringp json))
    ;; Should decode back correctly (JSON arrays → vectors)
    (let ((decoded (cauldron.json:decode json)))
      (is-equal "patch" (elt decoded 0))
      (is (> (length (elt decoded 1)) 0)))))

(deftest test-encode-patches-multiple
  (let* ((patches '((:replace (0) "a") (:insert (1) "b")))
         (json (cauldron.scry:encode-patches patches))
         (decoded (cauldron.json:decode json)))
    (is-equal "patch" (elt decoded 0))
    (is-equal 2 (length (elt decoded 1)))))

(deftest test-encode-patches-empty
  "Empty patch list encodes to [\"patch\", null] since nil → JSON null."
  (let* ((json (cauldron.scry:encode-patches '()))
         (decoded (cauldron.json:decode json)))
    (is-equal "patch" (elt decoded 0))
    ;; mapcar on empty list returns nil → encodes as null
    (is-equal :null (elt decoded 1))))

;;; --- encode-redirect ---

(deftest test-encode-redirect
  (let* ((json (cauldron.scry::encode-redirect "/login"))
         (decoded (cauldron.json:decode json)))
    (is-equal "redirect" (elt decoded 0))
    (is-equal "/login" (elt decoded 1))))

(deftest test-encode-redirect-with-params
  (let* ((json (cauldron.scry::encode-redirect "/login?next=/dashboard"))
         (decoded (cauldron.json:decode json)))
    (is-equal "/login?next=/dashboard" (elt decoded 1))))

;;; --- decode-client-message ---

(deftest test-decode-event-message
  (let* ((payload-ht (make-hash-table :test 'equal))
         (_ (setf (gethash "id" payload-ht) 1))
         (json (cauldron.json:encode (list "event" "click" payload-ht))))
    (declare (ignore _))
    (multiple-value-bind (msg-type event-name payload)
        (cauldron.scry:decode-client-message json)
      (is-equal "event" msg-type)
      (is-equal "click" event-name)
      (is-not-nil payload))))

(deftest test-decode-heartbeat-message
  (let ((json (cauldron.json:encode (list "heartbeat" "ping"))))
    (multiple-value-bind (msg-type event-name payload)
        (cauldron.scry:decode-client-message json)
      (is-equal "heartbeat" msg-type)
      (is-equal "ping" event-name)
      (is-nil payload))))

(deftest test-decode-short-message
  "Message with only 2 elements has nil payload."
  (let ((json (cauldron.json:encode (list "event" "submit"))))
    (multiple-value-bind (msg-type event-name payload)
        (cauldron.scry:decode-client-message json)
      (is-equal "event" msg-type)
      (is-equal "submit" event-name)
      (is-nil payload))))

(deftest test-decode-too-short-message
  "Single-element array returns nil."
  (let ((json (cauldron.json:encode (list "orphan"))))
    (is-nil (cauldron.scry:decode-client-message json))))

(deftest test-decode-roundtrip-patches
  "Encode patches and verify they roundtrip through JSON."
  (let* ((patches '((:replace (0 1) "hello")))
         (encoded (cauldron.scry:encode-patches patches))
         (decoded (cauldron.json:decode encoded)))
    (is-equal "patch" (elt decoded 0))
    (let ((wire-patch (elt (elt decoded 1) 0)))
      (is-equal "replace" (elt wire-patch 0)))))
