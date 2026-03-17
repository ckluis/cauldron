;;;; test/logging/format-test.lisp — Tests for canonical log line formatting/parsing
(in-package :cauldron.test)

(defsuite :logging-format)

;;; --- format-canonical-line ---

(deftest test-format-canonical-line-basic
  (let ((line (cauldron.logging:format-canonical-line
               '((:method . "GET") (:path . "/users") (:status . 200)))))
    (is (stringp line))
    (is-not-nil (search "method=GET" line))
    (is-not-nil (search "path=/users" line))
    (is-not-nil (search "status=200" line))))

(deftest test-format-canonical-line-keywords
  "Keywords are converted to snake_case."
  (let ((line (cauldron.logging:format-canonical-line
               '((:request-id . "req_abc") (:duration-ms . 42)))))
    (is-not-nil (search "request_id=req_abc" line))
    (is-not-nil (search "duration_ms=42" line))))

(deftest test-format-canonical-line-nil-value
  (let ((line (cauldron.logging:format-canonical-line '((:error . nil)))))
    (is-not-nil (search "error=nil" line))))

(deftest test-format-canonical-line-integer-value
  (let ((line (cauldron.logging:format-canonical-line '((:sql-count . 4)))))
    (is-not-nil (search "sql_count=4" line))))

(deftest test-format-canonical-line-quoted-value
  "Strings with spaces should be quoted."
  (let ((line (cauldron.logging:format-canonical-line
               '((:error . "not found here")))))
    (is-not-nil (search "error=\"not found here\"" line))))

(deftest test-format-canonical-line-empty
  (let ((line (cauldron.logging:format-canonical-line '())))
    (is-equal "" line)))

(deftest test-format-canonical-line-multiple-pairs
  (let ((line (cauldron.logging:format-canonical-line
               '((:a . 1) (:b . 2) (:c . 3)))))
    (is-not-nil (search "a=1" line))
    (is-not-nil (search "b=2" line))
    (is-not-nil (search "c=3" line))))

;;; --- parse-canonical-line ---

(deftest test-parse-canonical-line-basic
  (let ((result (cauldron.logging:parse-canonical-line
                 "method=GET path=/users status=200")))
    (is-equal "GET" (cdr (assoc :method result)))
    (is-equal "/users" (cdr (assoc :path result)))
    (is-equal 200 (cdr (assoc :status result)))))

(deftest test-parse-canonical-line-integers
  (let ((result (cauldron.logging:parse-canonical-line
                 "sql_count=4 duration_ms=42")))
    (is-equal 4 (cdr (assoc :sql-count result)))
    (is-equal 42 (cdr (assoc :duration-ms result)))))

(deftest test-parse-canonical-line-nil
  (let ((result (cauldron.logging:parse-canonical-line "error=nil")))
    (is-nil (cdr (assoc :error result)))))

(deftest test-parse-canonical-line-quoted
  (let ((result (cauldron.logging:parse-canonical-line
                 "error=\"not found here\"")))
    (is-equal "not found here" (cdr (assoc :error result)))))

(deftest test-format-parse-roundtrip
  "Formatting then parsing should recover the original data."
  (let* ((original '((:request-id . "req_a1b2") (:method . "POST")
                     (:status . 201) (:duration-ms . 15) (:error . nil)))
         (line (cauldron.logging:format-canonical-line original))
         (parsed (cauldron.logging:parse-canonical-line line)))
    (is-equal "req_a1b2" (cdr (assoc :request-id parsed)))
    (is-equal "POST" (cdr (assoc :method parsed)))
    (is-equal 201 (cdr (assoc :status parsed)))
    (is-equal 15 (cdr (assoc :duration-ms parsed)))
    (is-nil (cdr (assoc :error parsed)))))

(deftest test-parse-canonical-line-empty
  (let ((result (cauldron.logging:parse-canonical-line "")))
    (is-nil result)))

(deftest test-parse-canonical-line-snake-to-keyword
  "snake_case keys should become hyphenated keywords."
  (let ((result (cauldron.logging:parse-canonical-line
                 "external_call_count=3")))
    (is-equal 3 (cdr (assoc :external-call-count result)))))
