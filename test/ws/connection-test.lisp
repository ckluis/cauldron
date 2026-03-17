;;;; test/ws/connection-test.lisp — WebSocket connection pure-function tests
(in-package :cauldron.test)

(defsuite :ws-connection)

;;; --- ws-accept-key ---

(deftest test-ws-accept-key-rfc6455
  "Verify ws-accept-key produces correct SHA1+Base64 for known input."
  (let ((result (cauldron.ws:ws-accept-key "dGhlIHNhbXBsZSBub25jZQ==")))
    ;; SHA1("dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-5AB5A43ADE65")
    ;; = 62ebc1b385e06722fbe50c564a743a4cf54b8439
    ;; Base64 of that = YuvBs4XgZyL75QxWSnQ6TPVLhDk=
    (is-equal "YuvBs4XgZyL75QxWSnQ6TPVLhDk=" result
              "ws-accept-key produces correct SHA1+Base64")))

(deftest test-ws-accept-key-deterministic
  "Same input always produces same output."
  (let ((r1 (cauldron.ws:ws-accept-key "dGhlIHNhbXBsZSBub25jZQ=="))
        (r2 (cauldron.ws:ws-accept-key "dGhlIHNhbXBsZSBub25jZQ==")))
    (is-equal r1 r2 "deterministic output")))

(deftest test-ws-accept-key-different-inputs
  "Different keys produce different accept values."
  (let ((r1 (cauldron.ws:ws-accept-key "dGhlIHNhbXBsZSBub25jZQ=="))
        (r2 (cauldron.ws:ws-accept-key "AQIDBAUGBwgJCgsMDQ4PEA==")))
    (is (not (string= r1 r2)) "different inputs yield different outputs")))

;;; --- ws-connection struct ---

(deftest test-ws-connection-defaults
  (let ((conn (cauldron.ws:make-ws-connection)))
    (is-not-nil conn "connection created")
    (is-equal :open (cauldron.ws:ws-connection-state conn)
              "default state is :open")))

(deftest test-ws-connection-with-stream
  (let ((conn (cauldron.ws:make-ws-connection :stream :fake-stream :state :closing)))
    (is-equal :fake-stream (cauldron.ws:ws-connection-stream conn))
    (is-equal :closing (cauldron.ws:ws-connection-state conn))))
