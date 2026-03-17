;;;; src/scry/diff.lisp — HTML tree diff engine for LiveView updates
(in-package :cauldron.scry)

;;; -------------------------------------------------------
;;; HTML Tree Diff Engine
;;;
;;; Computes minimal diffs between two HTML trees represented
;;; as s-expressions (Alembic format). Produces a list of
;;; JSON-serializable patches.
;;;
;;; Tree format:
;;;   (:tag :attr1 val1 :attr2 val2 child1 child2 ...)
;;;   "text content"
;;;   nil (empty)
;;;
;;; Patch format:
;;;   (:insert path node)
;;;   (:remove path)
;;;   (:replace path new-node)
;;;   (:update-attrs path attrs)
;;;   (:update-text path new-text)
;;;
;;; Path is a list of child indices, e.g. (0 2 1).
;;; -------------------------------------------------------

;;; --- Tree parsing utilities ---

(defun tag-node-p (node)
  "Return T if NODE is a tag node (list starting with a keyword)."
  (and (consp node) (keywordp (car node))))

(defun text-node-p (node)
  "Return T if NODE is a text node (string or number)."
  (or (stringp node) (numberp node)))

(defun node-tag (node)
  "Return the tag keyword of a tag node."
  (when (tag-node-p node)
    (car node)))

(defun parse-node (node)
  "Parse a tag node into (values tag attrs children).
ATTRS is a plist of keyword-value pairs. CHILDREN is the remaining elements."
  (when (tag-node-p node)
    (let ((tag (car node))
          (rest (cdr node))
          (attrs '())
          (children '()))
      ;; Collect keyword-value pairs as attributes
      (loop while (and rest (keywordp (car rest)))
            do (push (car rest) attrs)
               (push (cadr rest) attrs)
               (setf rest (cddr rest)))
      (setf attrs (nreverse attrs))
      (setf children rest)
      (values tag attrs children))))

(defun node-text (node)
  "Return the text content of a text node as a string."
  (cond
    ((stringp node) node)
    ((numberp node) (princ-to-string node))
    (t nil)))

;;; --- Attribute comparison ---

(defun attrs-equal-p (attrs1 attrs2)
  "Return T if two attribute plists are equal."
  (and (= (length attrs1) (length attrs2))
       (loop for (k v) on attrs1 by #'cddr
             always (equal v (getf attrs2 k)))))

(defun diff-attrs (old-attrs new-attrs)
  "Compute the changed attributes between OLD-ATTRS and NEW-ATTRS.
Returns a plist of attributes that differ (new values), or NIL if identical."
  (let ((changes '()))
    ;; Check for changed/added attrs in new
    (loop for (k v) on new-attrs by #'cddr
          unless (equal v (getf old-attrs k))
          do (push k changes)
             (push v changes))
    ;; Check for removed attrs (present in old, absent in new)
    (loop for rest on old-attrs by #'cddr
          for k = (car rest)
          unless (getf new-attrs k)
          do (push k changes)
             (push nil changes))
    (nreverse changes)))

;;; --- Node type classification ---

(defun type-of-node (node)
  "Return :tag, :text, or :nil for the node type."
  (cond
    ((null node) :nil)
    ((tag-node-p node) :tag)
    ((text-node-p node) :text)
    (t :other)))

;;; --- Core diff algorithm ---

(defun diff-trees (old-tree new-tree)
  "Compare OLD-TREE and NEW-TREE, producing a list of patches.
Each patch is a list: (type path data...).

Patch types:
  (:insert path node)        — insert node at path
  (:remove path)             — remove node at path
  (:replace path new-node)   — replace node at path
  (:update-attrs path attrs) — update attributes at path
  (:update-text path text)   — update text content at path

Path is a list of child indices. The diff is minimal: changing one row
in a 100-row table produces a small set of patches."
  (let ((patches '()))
    (labels ((collect (patch) (push patch patches))

             (diff-rec (old new path)
               (cond
                 ;; Both nil — no change
                 ((and (null old) (null new)) nil)
                 ;; Old nil, new exists — insert
                 ((null old)
                  (collect (list :insert (copy-list path) new)))
                 ;; New nil, old exists — remove
                 ((null new)
                  (collect (list :remove (copy-list path))))
                 ;; Both text nodes
                 ((and (text-node-p old) (text-node-p new))
                  (unless (equal (node-text old) (node-text new))
                    (collect (list :update-text (copy-list path) (node-text new)))))
                 ;; Type mismatch or different tags — replace whole subtree
                 ((or (not (eq (type-of-node old) (type-of-node new)))
                      (and (tag-node-p old) (tag-node-p new)
                           (not (eq (node-tag old) (node-tag new)))))
                  (collect (list :replace (copy-list path) new)))
                 ;; Same tag — diff attributes and children recursively
                 ((and (tag-node-p old) (tag-node-p new))
                  (multiple-value-bind (o-tag o-attrs o-children) (parse-node old)
                    (multiple-value-bind (n-tag n-attrs n-children) (parse-node new)
                      (declare (ignore o-tag n-tag))
                      ;; Attribute diff
                      (let ((attr-diff (diff-attrs o-attrs n-attrs)))
                        (when attr-diff
                          (collect (list :update-attrs (copy-list path) attr-diff))))
                      ;; Children diff
                      (diff-child-list o-children n-children path))))
                 ;; Fallback — replace
                 (t (collect (list :replace (copy-list path) new)))))

             (diff-child-list (old-children new-children path)
               (let ((old-len (length old-children))
                     (new-len (length new-children)))
                 (let ((common (min old-len new-len)))
                   ;; Diff children that exist in both lists
                   (loop for i from 0 below common
                         for old-child in old-children
                         for new-child in new-children
                         do (diff-rec old-child new-child (append path (list i))))
                   ;; Extra children in new → inserts
                   (loop for i from common below new-len
                         for new-child in (nthcdr common new-children)
                         do (collect (list :insert (append path (list i)) new-child)))
                   ;; Extra children in old → removes (from end, so indices stay valid)
                   (loop for i from (1- old-len) downto common
                         do (collect (list :remove (append path (list i)))))))))

      (diff-rec old-tree new-tree '()))
    (nreverse patches)))

;;; -------------------------------------------------------
;;; Apply patches to a tree (for testing / server-side replay)
;;; -------------------------------------------------------

(defun apply-diff (tree patches)
  "Apply PATCHES to TREE, returning the modified tree.
Primarily useful for testing the diff engine: given old-tree and patches,
(apply-diff old-tree (diff-trees old-tree new-tree)) should equal new-tree."
  (let ((result tree))
    (dolist (patch patches result)
      (setf result (apply-patch result patch)))))

(defun apply-patch (tree patch)
  "Apply a single PATCH to TREE."
  (let ((type (first patch))
        (path (second patch)))
    (if (null path)
        ;; Patch at root
        (ecase type
          (:replace (third patch))
          (:update-text (third patch))
          (:update-attrs
           (if (tag-node-p tree)
               (multiple-value-bind (tag attrs children) (parse-node tree)
                 (let ((new-attrs (merge-attrs attrs (third patch))))
                   (reconstruct-node tag new-attrs children)))
               tree))
          (:insert (third patch))
          (:remove nil))
        ;; Patch at nested path — descend into tree
        (apply-at-path tree path type (cddr patch)))))

(defun merge-attrs (old-attrs new-attrs)
  "Merge NEW-ATTRS into OLD-ATTRS. NIL values in NEW-ATTRS remove the attribute."
  (let ((result (copy-list old-attrs)))
    (loop for (k v) on new-attrs by #'cddr
          do (if (null v)
                 (setf result (remove-plist-key result k))
                 (setf (getf result k) v)))
    result))

(defun remove-plist-key (plist key)
  "Remove KEY and its value from PLIST."
  (loop for (k v) on plist by #'cddr
        unless (eq k key)
        collect k and collect v))

(defun reconstruct-node (tag attrs children)
  "Rebuild a tag node s-expression from its parsed components."
  (append (list tag) attrs children))

(defun apply-at-path (tree path type args)
  "Apply a patch operation at PATH within TREE, descending through tag nodes."
  (when (null tree) (return-from apply-at-path tree))
  (if (tag-node-p tree)
      (multiple-value-bind (tag attrs children) (parse-node tree)
        (let ((idx (first path))
              (rest-path (rest path)))
          (if (null rest-path)
              ;; At target depth — apply to child at idx
              (let ((new-children (apply-to-child children idx type args)))
                (reconstruct-node tag attrs new-children))
              ;; Not yet at target — descend into child at idx
              (let ((new-children
                      (loop for child in children
                            for i from 0
                            collect (if (= i idx)
                                        (apply-at-path child rest-path type args)
                                        child))))
                (reconstruct-node tag attrs new-children)))))
      tree))

(defun apply-to-child (children idx type args)
  "Apply a patch operation to the child at IDX in CHILDREN list."
  (ecase type
    (:insert
     (let ((new-node (first args)))
       (append (subseq children 0 (min idx (length children)))
               (list new-node)
               (when (< idx (length children))
                 (subseq children idx)))))
    (:remove
     (append (subseq children 0 idx)
             (when (< (1+ idx) (length children))
               (subseq children (1+ idx)))))
    (:replace
     (let ((new-node (first args)))
       (loop for child in children
             for i from 0
             collect (if (= i idx) new-node child))))
    (:update-text
     (let ((new-text (first args)))
       (loop for child in children
             for i from 0
             collect (if (= i idx) new-text child))))
    (:update-attrs
     (let ((attr-changes (first args)))
       (loop for child in children
             for i from 0
             collect (if (and (= i idx) (tag-node-p child))
                         (multiple-value-bind (ctag cattrs cchildren) (parse-node child)
                           (reconstruct-node ctag (merge-attrs cattrs attr-changes) cchildren))
                         child))))))
