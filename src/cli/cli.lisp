;;;; src/cli/cli.lisp — CLI argument parsing, command registration, and dispatch
;;;; Provides defcommand macro for registering subcommands, parse-args for
;;;; extracting flags/options/positional args, and dispatch-command for routing.

(in-package :cauldron.cli)

;;; ============================================================
;;; Command Registry
;;; ============================================================

(defvar *commands* (make-hash-table :test 'equal)
  "Registry of CLI commands: name-string -> command struct.")

(defstruct cli-command
  "A registered CLI command."
  (name        "" :type string)
  (description "" :type string)
  (handler     nil :type (or null function))
  (options     '() :type list)    ; list of option specs
  (subcommands (make-hash-table :test 'equal) :type hash-table))

(defmacro defcommand (name (&key description options) &body body)
  "Define a CLI command. NAME is a string like \"company create\".
DESCRIPTION is a help string. OPTIONS is a list of option specs.
BODY receives a parsed-args plist and should return an exit code (0 = success).

Example:
  (defcommand \"company create\"
    (:description \"Create a new company\"
     :options ((\"--name\" :required t :description \"Company name\")
               (\"--type\" :default \"crm\" :description \"Business type\")))
    (let ((name (get-arg args \"--name\")))
      (emit :data (create-company name))
      0))"
  (let ((fn-name (intern (format nil "CLI-~A" (string-upcase (substitute #\- #\Space name))))))
    `(progn
       (defun ,fn-name (args)
         ,@body)
       (register-command ,name ,description ',options #',fn-name))))

(defun register-command (name description options handler)
  "Register a command in the global registry."
  (setf (gethash name *commands*)
        (make-cli-command :name name
                          :description description
                          :options options
                          :handler handler)))

(defun find-command (name)
  "Look up a command by name string."
  (gethash name *commands*))

;;; ============================================================
;;; Argument Parsing
;;; ============================================================

(defun parse-args (argv)
  "Parse command-line arguments into a structured plist.
Returns (:command \"name\" :flags (\"--verbose\") :options ((\"--name\" . \"value\")) :positional (\"arg1\"))

Conventions:
  --name value   -> named option
  --name=value   -> named option (alternative)
  --verbose      -> flag (boolean, no value)
  arg            -> positional argument"
  (let ((command-parts '())
        (flags '())
        (options '())
        (positional '())
        (i 0)
        (found-option nil))
    ;; First, collect command parts (words before first -- option)
    (loop while (< i (length argv))
          for arg = (nth i argv)
          do (cond
               ;; Option with = delimiter
               ((and (>= (length arg) 2)
                     (string= arg "--" :end1 2)
                     (position #\= arg))
                (setf found-option t)
                (let ((eq-pos (position #\= arg)))
                  (push (cons (subseq arg 0 eq-pos)
                              (subseq arg (1+ eq-pos)))
                        options))
                (incf i))
               ;; Option (starts with --)
               ((and (>= (length arg) 2) (string= arg "--" :end1 2))
                (setf found-option t)
                ;; Check if next arg is a value or another flag
                (if (and (< (1+ i) (length argv))
                         (let ((next (nth (1+ i) argv)))
                           (or (= 0 (length next))
                               (not (char= (char next 0) #\-)))))
                    (progn
                      (push (cons arg (nth (1+ i) argv)) options)
                      (incf i 2))
                    (progn
                      (push arg flags)
                      (incf i))))
               ;; Positional or command part
               (t
                (if found-option
                    (push arg positional)
                    (push arg command-parts))
                (incf i))))
    (list :command (format nil "~{~A~^ ~}" (nreverse command-parts))
          :flags (nreverse flags)
          :options (nreverse options)
          :positional (nreverse positional))))

(defun get-arg (parsed-args name &optional default)
  "Get a named option value from parsed args."
  (let ((pair (assoc name (getf parsed-args :options) :test #'string=)))
    (if pair (cdr pair) default)))

(defun get-flag (parsed-args name)
  "Check if a flag is set in parsed args."
  (member name (getf parsed-args :flags) :test #'string=))

(defun get-positional (parsed-args &optional (index 0))
  "Get a positional argument by index."
  (nth index (getf parsed-args :positional)))

;;; ============================================================
;;; Fuzzy Matching (Levenshtein Distance)
;;; ============================================================

(defun levenshtein-distance (s1 s2)
  "Compute the Levenshtein edit distance between strings S1 and S2."
  (let* ((len1 (length s1))
         (len2 (length s2))
         (matrix (make-array (list (1+ len1) (1+ len2)) :initial-element 0)))
    ;; Initialize first column and row
    (loop for i from 0 to len1 do (setf (aref matrix i 0) i))
    (loop for j from 0 to len2 do (setf (aref matrix 0 j) j))
    ;; Fill matrix
    (loop for i from 1 to len1 do
      (loop for j from 1 to len2 do
        (let ((cost (if (char= (char s1 (1- i)) (char s2 (1- j))) 0 1)))
          (setf (aref matrix i j)
                (min (1+ (aref matrix (1- i) j))          ; deletion
                     (1+ (aref matrix i (1- j)))           ; insertion
                     (+ (aref matrix (1- i) (1- j)) cost)))))) ; substitution
    (aref matrix len1 len2)))

(defun suggest-command (name)
  "Given a bad command NAME, find the closest registered command.
Returns the command name string if distance <= 3, or NIL."
  (let ((best-name nil)
        (best-dist most-positive-fixnum))
    (maphash (lambda (cmd-name cmd)
               (declare (ignore cmd))
               (let ((dist (levenshtein-distance name cmd-name)))
                 (when (< dist best-dist)
                   (setf best-dist dist
                         best-name cmd-name))))
             *commands*)
    (when (and best-name (<= best-dist 3))
      best-name)))

;;; ============================================================
;;; Command Usage Display
;;; ============================================================

(defun print-command-usage (command &key (stream *standard-output*))
  "Print usage information for a specific command, showing its options."
  (let ((name (cli-command-name command))
        (desc (cli-command-description command))
        (opts (cli-command-options command)))
    (format stream "Usage: cauldron ~A [options]~%~%" name)
    (when (and desc (string/= desc ""))
      (format stream "  ~A~%~%" desc))
    (when opts
      (format stream "Options:~%")
      (dolist (opt opts)
        (let* ((opt-name (first opt))
               (plist (rest opt))
               (required (getf plist :required))
               (default (getf plist :default))
               (description (getf plist :description "")))
          (format stream "  ~20A ~A~A~%"
                  opt-name
                  description
                  (cond
                    (required " (required)")
                    (default (format nil " (default: ~A)" default))
                    (t "")))))
      (terpri stream))))

;;; ============================================================
;;; Progressive Help
;;; ============================================================

(defun command-category (name)
  "Extract the category (first word) from a command name."
  (let ((space-pos (position #\Space name)))
    (if space-pos
        (subseq name 0 space-pos)
        name)))

(defun print-help (&key (stream *standard-output*))
  "Print help listing all registered commands, grouped by category."
  (format stream "Usage: cauldron <command> [options]~%~%")
  (let ((names (sort (loop for k being the hash-keys of *commands* collect k) #'string<))
        (groups (make-hash-table :test 'equal)))
    ;; Group commands by category
    (dolist (name names)
      (let ((category (command-category name)))
        (push name (gethash category groups))))
    ;; Sort categories and print
    (let ((categories (sort (loop for k being the hash-keys of groups collect k) #'string<)))
      (dolist (category categories)
        (let ((cat-label (concatenate 'string
                                      (string (char-upcase (char category 0)))
                                      (subseq category 1))))
          (format stream "~A:~%" cat-label))
        (let ((cmds (sort (copy-list (gethash category groups)) #'string<)))
          (dolist (name cmds)
            (let ((cmd (gethash name *commands*)))
              (format stream "  ~20A ~A~%" name (cli-command-description cmd)))))
        (terpri stream))))
  (format stream "Use --help with any command for more details.~%"))

;;; ============================================================
;;; Required Option Validation
;;; ============================================================

(defun missing-required-options (command parsed-args)
  "Return a list of required option names that are missing from PARSED-ARGS."
  (let ((opts (cli-command-options command))
        (missing '()))
    (dolist (opt opts)
      (let* ((opt-name (first opt))
             (plist (rest opt))
             (required (getf plist :required)))
        (when (and required (not (get-arg parsed-args opt-name)))
          (push opt-name missing))))
    (nreverse missing)))

;;; ============================================================
;;; Command Dispatch
;;; ============================================================

(defun dispatch-command (argv)
  "Parse ARGV and dispatch to the appropriate command handler.
Returns the command's exit code (integer)."
  (when (null argv)
    (print-help)
    (return-from dispatch-command 0))
  (let ((parsed (parse-args argv)))
    (let* ((cmd-name (getf parsed :command))
           (command (find-command cmd-name)))
      (cond
        ;; Help flag
        ((or (get-flag parsed "--help")
             (string= cmd-name "help")
             (string= cmd-name ""))
         ;; If --help is used with a specific command, show that command's usage
         (if (and (get-flag parsed "--help")
                  command)
             (progn (print-command-usage command) 0)
             (progn (print-help) 0)))
        ;; Known command
        (command
         ;; Validate required options
         (let ((missing (missing-required-options command parsed)))
           (if missing
               (progn
                 (format *error-output* "Missing required option~P: ~{~A~^, ~}~%~%"
                         (length missing) missing)
                 (print-command-usage command :stream *error-output*)
                 1)
               (handler-case
                   (funcall (cli-command-handler command) parsed)
                 (error (e)
                   (format *error-output* "Error: ~A~%" e)
                   1)))))
        ;; Try progressively shorter command names
        (t
         (let ((parts (split-command cmd-name)))
           (loop for len from (1- (length parts)) downto 1
                 for try-name = (format nil "~{~A~^ ~}" (subseq parts 0 len))
                 for try-cmd = (find-command try-name)
                 when try-cmd
                   do (let ((missing (missing-required-options try-cmd parsed)))
                        (if missing
                            (progn
                              (format *error-output* "Missing required option~P: ~{~A~^, ~}~%~%"
                                      (length missing) missing)
                              (print-command-usage try-cmd :stream *error-output*)
                              (return-from dispatch-command 1))
                            (return-from dispatch-command
                              (handler-case
                                  (funcall (cli-command-handler try-cmd) parsed)
                                (error (e)
                                  (format *error-output* "Error: ~A~%" e)
                                  1)))))))
         ;; Unknown command — try fuzzy matching
         (let ((suggestion (suggest-command cmd-name)))
           (if suggestion
               (format *error-output* "Unknown command: ~A~%Did you mean: ~A~%Use --help for available commands.~%"
                       cmd-name suggestion)
               (format *error-output* "Unknown command: ~A~%Use --help for available commands.~%"
                       cmd-name)))
         1)))))

(defun split-command (name)
  "Split a command name by spaces."
  (loop for start = 0 then (1+ end)
        for end = (position #\Space name :start start)
        collect (subseq name start (or end (length name)))
        while end))

(defun run-cli (&optional (argv (rest sb-ext:*posix-argv*)))
  "Entry point: parse args and dispatch. Exits with command's exit code."
  (sb-ext:exit :code (dispatch-command argv)))
