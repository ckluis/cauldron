;;;; bench/runner.lisp — Simple benchmark framework for Cauldron
;;;; Defines defbench, run-benchmarks, and bench-report.

(defpackage :cauldron.bench
  (:use :cl)
  (:export #:defbench
           #:run-benchmarks
           #:bench-report
           #:*benchmarks*))

(in-package :cauldron.bench)

;;; --- Benchmark registry ---

(defvar *benchmarks* '()
  "List of (name iterations thunk description) entries.")

(defvar *results* '()
  "List of (name iterations elapsed-seconds ops-per-sec) from last run.")

;;; --- Benchmark definition ---

(defmacro defbench (name iterations &body body)
  "Define a benchmark. ITERATIONS is how many times to run BODY.
Registers the benchmark for execution by run-benchmarks."
  (let ((desc (if (stringp (first body))
                  (first body)
                  (format nil "~A" name)))
        (actual-body (if (stringp (first body))
                         (rest body)
                         body)))
    `(progn
       ;; Remove any existing benchmark with same name
       (setf *benchmarks* (remove ',name *benchmarks* :key #'first))
       ;; Register new benchmark
       (push (list ',name ,iterations
                   (lambda ()
                     ,@actual-body)
                   ,desc)
             *benchmarks*))))

;;; --- Benchmark execution ---

(defun run-single-benchmark (name iterations thunk)
  "Run THUNK for ITERATIONS, return (values elapsed-seconds ops-per-sec).
Performs a warm-up pass of 10% of iterations (min 1) before timing."
  ;; Warm-up
  (let ((warmup-count (max 1 (floor iterations 10))))
    (dotimes (_ warmup-count)
      (funcall thunk)))
  ;; Timed run
  (let ((start (get-internal-real-time)))
    (dotimes (_ iterations)
      (funcall thunk))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (float (- end start))
                       (float internal-time-units-per-second)))
           (ops-per-sec (if (zerop elapsed)
                            most-positive-double-float
                            (/ (float iterations) elapsed))))
      (values elapsed ops-per-sec))))

(defun run-benchmarks ()
  "Run all registered benchmarks and print results.
Returns the results list."
  (setf *results* '())
  (let ((benchmarks (reverse *benchmarks*)))
    (format t "~%~A~%" (make-string 72 :initial-element #\=))
    (format t "  Cauldron Benchmark Suite~%")
    (format t "  SBCL ~A / ~A~%"
            (lisp-implementation-version)
            (machine-type))
    (format t "~A~%~%" (make-string 72 :initial-element #\=))
    (format t "~36A ~10A ~12A ~12A~%"
            "Benchmark" "Iterations" "Elapsed(s)" "Ops/sec")
    (format t "~A~%" (make-string 72 :initial-element #\-))
    (dolist (bench benchmarks)
      (destructuring-bind (name iterations thunk description) bench
        (declare (ignore description))
        (handler-case
            (multiple-value-bind (elapsed ops-sec)
                (run-single-benchmark name iterations thunk)
              (format t "~36A ~10D ~12,4F ~12,0F~%"
                      name iterations elapsed ops-sec)
              (push (list name iterations elapsed ops-sec) *results*))
          (error (c)
            (format t "~36A ~10D  ERROR: ~A~%"
                    name iterations c)
            (push (list name iterations -1 0) *results*)))))
    (format t "~A~%" (make-string 72 :initial-element #\-))
    (format t "~%Done. ~D benchmarks executed.~%~%"
            (length benchmarks))
    (setf *results* (nreverse *results*))
    *results*))

(defun bench-report ()
  "Print a summary report of the last benchmark run.
Groups results by category (derived from benchmark name prefix)."
  (unless *results*
    (format t "No benchmark results available. Run (run-benchmarks) first.~%")
    (return-from bench-report nil))
  (format t "~%~A~%" (make-string 72 :initial-element #\=))
  (format t "  Benchmark Report Summary~%")
  (format t "~A~%~%" (make-string 72 :initial-element #\=))
  (let ((total-ops 0)
        (total-time 0.0))
    (dolist (result *results*)
      (destructuring-bind (name iterations elapsed ops-sec) result
        (declare (ignore name))
        (when (plusp elapsed)
          (incf total-ops iterations)
          (incf total-time elapsed)
          (format t "  ~36A  ~,0F ops/sec~%"
                  (first result) ops-sec))))
    (format t "~%  Total operations: ~:D~%" total-ops)
    (format t "  Total time: ~,2F seconds~%" total-time)
    (when (plusp total-time)
      (format t "  Aggregate throughput: ~,0F ops/sec~%"
              (/ (float total-ops) total-time)))
    (format t "~A~%" (make-string 72 :initial-element #\=))))
