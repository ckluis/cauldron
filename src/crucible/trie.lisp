;;;; src/crucible/trie.lisp — Radix trie for URL route matching
;;;; Supports: literal segments, params (:id), wildcards (*path)
;;;; Priority: literal > param > wildcard

(in-package :cauldron.crucible)

;;; --- Trie node ---

(defstruct trie-node
  "A node in the route trie."
  (literal-children (make-hash-table :test 'equal) :type hash-table)  ; segment → node
  (param-child nil)                ; (:name . node) for :param segments
  (wildcard-child nil)             ; (:name . node) for *wildcard
  (handlers (make-hash-table :test 'eq) :type hash-table)  ; method → handler
  (pipeline nil))                  ; pipeline name for this node

(defun make-trie ()
  "Create an empty route trie."
  (make-trie-node))

(defun split-path (path)
  "Split a URL path into segments. '/foo/bar/baz' → ('foo' 'bar' 'baz')."
  (remove "" (loop for start = 0 then (1+ end)
                   for end = (position #\/ path :start start)
                   collect (subseq path start (or end (length path)))
                   while end)
         :test #'string=))

(defun segment-type (segment)
  "Classify a path segment. Returns :literal, :param, or :wildcard."
  (cond
    ((and (> (length segment) 0) (char= (char segment 0) #\:))
     :param)
    ((and (> (length segment) 0) (char= (char segment 0) #\*))
     :wildcard)
    (t :literal)))

(defun segment-name (segment)
  "Extract the parameter name from a :param or *wildcard segment."
  (subseq segment 1))

;;; --- Insertion ---

(defun trie-insert (trie method path handler &key pipeline)
  "Insert HANDLER for METHOD at PATH into TRIE.
PATH is a string like '/users/:id/posts'.
METHOD is a keyword like :GET, :POST, etc."
  (let ((segments (split-path path))
        (node trie))
    (dolist (seg segments)
      (ecase (segment-type seg)
        (:literal
         (let ((child (gethash seg (trie-node-literal-children node))))
           (unless child
             (setf child (make-trie-node))
             (setf (gethash seg (trie-node-literal-children node)) child))
           (setf node child)))
        (:param
         (let ((existing (trie-node-param-child node)))
           (unless existing
             (setf existing (cons (segment-name seg) (make-trie-node)))
             (setf (trie-node-param-child node) existing))
           (setf node (cdr existing))))
        (:wildcard
         (let ((existing (trie-node-wildcard-child node)))
           (unless existing
             (setf existing (cons (segment-name seg) (make-trie-node)))
             (setf (trie-node-wildcard-child node) existing))
           (setf node (cdr existing))
           ;; Wildcard consumes rest — no more segments after this
           (return)))))
    (setf (gethash method (trie-node-handlers node)) handler)
    (when pipeline
      (setf (trie-node-pipeline node) pipeline))
    trie))

;;; --- Matching ---

(defun trie-match (trie method path)
  "Match METHOD and PATH against TRIE.
Returns (values handler params pipeline) or (values nil nil nil).
PARAMS is an alist of (name . value) pairs.
Priority: literal > param > wildcard."
  (let ((segments (split-path path)))
    (labels ((match-segments (node segs params)
               (if (null segs)
                   ;; At terminal node — check for handler
                   ;; Priority: exact method > HEAD→GET fallback > :ANY
                   (let ((handler (or (gethash method (trie-node-handlers node))
                                      ;; HEAD falls back to GET (standard HTTP behavior)
                                      (when (eq method :head)
                                        (gethash :get (trie-node-handlers node)))
                                      (gethash :any (trie-node-handlers node)))))
                     (if handler
                         (values handler (nreverse params) (trie-node-pipeline node))
                         (values nil nil nil)))
                   (let ((seg (first segs))
                         (rest-segs (rest segs)))
                     ;; 1. Try literal match (highest priority)
                     (let ((literal-child (gethash seg (trie-node-literal-children node))))
                       (when literal-child
                         (multiple-value-bind (handler found-params pipeline)
                             (match-segments literal-child rest-segs params)
                           (when handler
                             (return-from match-segments
                               (values handler found-params pipeline))))))
                     ;; 2. Try param match
                     (let ((param-entry (trie-node-param-child node)))
                       (when param-entry
                         (let ((param-name (car param-entry))
                               (param-node (cdr param-entry)))
                           (multiple-value-bind (handler found-params pipeline)
                               (match-segments param-node rest-segs
                                               (cons (cons param-name seg) params))
                             (when handler
                               (return-from match-segments
                                 (values handler found-params pipeline)))))))
                     ;; 3. Try wildcard match (lowest priority)
                     (let ((wild-entry (trie-node-wildcard-child node)))
                       (when wild-entry
                         (let* ((wild-name (car wild-entry))
                                (wild-node (cdr wild-entry))
                                (wild-value (format nil "~{~A~^/~}" segs))
                                (handler (or (gethash method (trie-node-handlers wild-node))
                                             (gethash :any (trie-node-handlers wild-node)))))
                           (when handler
                             (return-from match-segments
                               (values handler
                                       (nreverse (cons (cons wild-name wild-value) params))
                                       (trie-node-pipeline wild-node)))))))
                     ;; No match
                     (values nil nil nil)))))
      (match-segments trie segments '()))))
