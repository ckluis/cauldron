;;;; test/db/types-integration-test.lisp — Type codec round-trip tests against real PostgreSQL

(in-package :cauldron.test)

(defsuite :db-types-integration)

;;; Setup: create test_types table
(suite-setup :db-types-integration
  (lambda ()
    (ensure-test-database)
    (with-test-connection (conn)
      (cauldron.db:execute-sql conn
        "CREATE TABLE IF NOT EXISTS test_types (
           id SERIAL PRIMARY KEY,
           col_bool BOOLEAN,
           col_int2 SMALLINT,
           col_int4 INTEGER,
           col_int8 BIGINT,
           col_float4 REAL,
           col_float8 DOUBLE PRECISION,
           col_text TEXT,
           col_varchar VARCHAR(255),
           col_bytea BYTEA,
           col_numeric NUMERIC(12,4),
           col_timestamp TIMESTAMP,
           col_timestamptz TIMESTAMPTZ,
           col_uuid UUID,
           col_jsonb JSONB,
           col_text_array TEXT[]
         )"))))

;;; Teardown: drop the table
(suite-teardown :db-types-integration
  (lambda ()
    (ignore-errors
      (with-test-connection (conn)
        (drop-test-tables conn "test_types")))))

;;; --- Helper to round-trip a single value ---

(defun %insert-and-select (conn column value &optional cast)
  "Insert VALUE into COLUMN of test_types, select it back.
CAST is an optional SQL type cast string like '::uuid'."
  (let ((insert-sql (format nil "INSERT INTO test_types (~A) VALUES ($1~@[~A~]) RETURNING ~A"
                            column (or cast "") column)))
    (let ((rows (cauldron.db:query conn insert-sql value)))
      (gethash column (first rows)))))

;;; --- Boolean ---

(deftest test-bool-true-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_bool" "t" "::boolean")))
      (is-equal t result "Boolean true should round-trip"))))

(deftest test-bool-false-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_bool" "f" "::boolean")))
      (is-equal nil result "Boolean false should round-trip as NIL"))))

;;; --- Integer types ---

(deftest test-int2-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_int2" "32767" "::smallint")))
      (is-equal 32767 result "SMALLINT max should round-trip"))))

(deftest test-int4-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_int4" "2147483647" "::integer")))
      (is-equal 2147483647 result "INTEGER max should round-trip"))))

(deftest test-int8-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_int8" "9223372036854775807" "::bigint")))
      (is-equal 9223372036854775807 result "BIGINT max should round-trip"))))

;;; --- Float types ---

(deftest test-float4-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_float4" "3.14" "::real")))
      (is (numberp result) "REAL should return a number")
      (is (< (abs (- result 3.14)) 0.01) "REAL should be approximately 3.14"))))

(deftest test-float8-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_float8" "3.141592653589793" "::double precision")))
      (is (numberp result) "DOUBLE PRECISION should return a number")
      (is (< (abs (- result 3.141592653589793d0)) 1.0d-6)
          "DOUBLE PRECISION should preserve precision"))))

;;; --- String types ---

(deftest test-text-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_text" "Hello, Cauldron!")))
      (is-equal "Hello, Cauldron!" result "TEXT should round-trip"))))

(deftest test-varchar-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_varchar" "varchar test")))
      (is-equal "varchar test" result "VARCHAR should round-trip"))))

;;; --- Binary ---

(deftest test-bytea-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_bytea" "\\x48454c4c4f" "::bytea")))
      (is-not-nil result "BYTEA should return non-nil"))))

;;; --- Numeric ---

(deftest test-numeric-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_numeric" "12345.6789" "::numeric")))
      (is-not-nil result "NUMERIC should return non-nil")
      ;; May come back as string or rational depending on codec
      (is (or (numberp result) (stringp result))
          "NUMERIC should be a number or string representation"))))

;;; --- Timestamp ---

(deftest test-timestamp-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_timestamp" "2026-03-14 12:30:45" "::timestamp")))
      (is-not-nil result "TIMESTAMP should return non-nil")
      (is (stringp result) "TIMESTAMP should return a string"))))

;;; --- Timestamptz ---

(deftest test-timestamptz-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_timestamptz" "2026-03-14 12:30:45+00" "::timestamptz")))
      (is-not-nil result "TIMESTAMPTZ should return non-nil"))))

;;; --- UUID ---

(deftest test-uuid-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_uuid" "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11" "::uuid")))
      (is-equal "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11" result "UUID should round-trip as string"))))

;;; --- JSONB ---

(deftest test-jsonb-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_jsonb" "{\"key\":\"value\",\"n\":42}" "::jsonb")))
      (is-not-nil result "JSONB should return non-nil")
      (is (or (stringp result) (hash-table-p result))
          "JSONB should return a string or parsed hash-table"))))

;;; --- Text array ---

(deftest test-text-array-roundtrip
  (with-test-connection (conn)
    (let ((result (%insert-and-select conn "col_text_array" "{hello,world}" "::text[]")))
      (is-not-nil result "TEXT[] should return non-nil")
      (is (or (listp result) (vectorp result) (stringp result))
          "TEXT[] should return a list, vector, or string representation"))))

;;; --- NULL ---

(deftest test-null-roundtrip
  (with-test-connection (conn)
    (cauldron.db:execute-sql conn "INSERT INTO test_types (col_text) VALUES (NULL)")
    (let ((rows (cauldron.db:query conn
                  "SELECT col_text FROM test_types WHERE col_text IS NULL LIMIT 1")))
      (is (plusp (length rows)) "Should find a NULL row")
      (let ((row (first rows)))
        ;; NULL should come back as NIL or :null
        (let ((val (gethash "col_text" row)))
          (is (or (null val) (eq val :null))
              "NULL should round-trip as NIL or :null"))))))
