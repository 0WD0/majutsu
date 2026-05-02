;;; majutsu-arrange.el --- Arrange commit graph for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides an Emacs-native editor for `jj arrange`-style
;; graph rewrite plans.  The buffer keeps a local arrange session; editing
;; commands update that session and rerender, and applying the session uses
;; a conservative CLI replay backend.

;;; Code:

(require 'majutsu)
(require 'majutsu-config)
(require 'cl-lib)
(require 'magit-section)
(require 'seq)
(require 'subr-x)

;;; Faces

(defface majutsu-arrange-selected-face
  '((t :inherit highlight))
  "Face used for the selected arrange node."
  :group 'majutsu)

(defface majutsu-arrange-subject-face
  '((t :inherit region))
  "Face used for nodes in the active arrange subject."
  :group 'majutsu)

(defface majutsu-arrange-context-face
  '((t :inherit shadow))
  "Face used for external context nodes in arrange buffers."
  :group 'majutsu)

(defface majutsu-arrange-keep-face
  '((t :inherit font-lock-constant-face))
  "Face used for nodes kept by an arrange plan."
  :group 'majutsu)

(defface majutsu-arrange-abandon-face
  '((t :inherit error :strike-through t))
  "Face used for nodes abandoned by an arrange plan."
  :group 'majutsu)

(defface majutsu-arrange-graph-face
  '((t :inherit magit-hash))
  "Face used for graph glyphs in arrange buffers."
  :group 'majutsu)

;;; Data model

(cl-defstruct (majutsu-arrange-node
               (:constructor majutsu-arrange-node-create))
  id
  change-id
  commit-id
  short-change-id
  short-commit-id
  description
  parents
  original-parents
  action
  role)

(cl-defstruct (majutsu-arrange-subject
               (:constructor majutsu-arrange-subject-create))
  ids
  roots
  heads
  incoming-edges
  outgoing-edges
  scope)

(cl-defstruct (majutsu-arrange-session
               (:constructor majutsu-arrange-session-create))
  root
  revset
  base-operation-id
  nodes
  target-ids
  external-parent-ids
  external-child-ids
  head-order
  current-order
  selected-id
  marked-ids
  subject-scope
  preview-anchor-ids
  operations
  dirty-p
  applying-p)

(cl-defstruct (majutsu-arrange-plan
               (:constructor majutsu-arrange-plan-create))
  base-operation-id
  final-parents
  final-actions
  operations
  affected-ids
  command-preview)

(defvar-local majutsu-arrange--session nil
  "Buffer-local arrange session.")

;;; Sections

(defvar-keymap majutsu-arrange-node-section-map
  :doc "Keymap for `jj-arrange-node' sections."
  "RET" #'majutsu-arrange-show)

(defclass majutsu-arrange-node-section (magit-section)
  ((keymap :initform 'majutsu-arrange-node-section-map)))

(setf (alist-get 'jj-arrange-node magit--section-type-alist)
      'majutsu-arrange-node-section)

;;; Machine template

(defconst majutsu-arrange--field-separator (string 30)
  "Field separator used by the arrange machine template.")

(defconst majutsu-arrange--list-separator (string 31)
  "List separator used by the arrange machine template.")

(defconst majutsu-arrange--default-revset "reachable(@, mutable())"
  "Fallback revset used when `revsets.arrange' is not configured.")

(defconst majutsu-arrange--node-template
  (let ((fs (format "%S" majutsu-arrange--field-separator))
        (ls (format "%S" majutsu-arrange--list-separator)))
    (string-join
     (list "change_id"
           fs
           "commit_id"
           fs
           "change_id.shortest(8)"
           fs
           "commit_id.shortest(8)"
           fs
           "description.first_line()"
           fs
           (format "parents.map(|p| p.commit_id()).join(%s)" ls)
           "\"\\n\"")
     " ++ "))
  "Template used to load arrange node metadata from jj.")

;;; Small helpers

(defun majutsu-arrange--id-set (ids)
  "Return a hash table set containing IDS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (id ids)
      (puthash id t table))
    table))

(defun majutsu-arrange--id-member-p (id ids)
  "Return non-nil when ID is a member of IDS."
  (member id ids))

(defun majutsu-arrange--push-unique (item list)
  "Append ITEM to LIST unless already present."
  (if (member item list)
      list
    (append list (list item))))

(defun majutsu-arrange--append-unique (list items)
  "Append ITEMS to LIST while preserving first occurrence order."
  (dolist (item items list)
    (setq list (majutsu-arrange--push-unique item list))))

(defun majutsu-arrange--session ()
  "Return the current arrange session or signal an error."
  (or majutsu-arrange--session
      (user-error "No arrange session in this buffer")))

(defun majutsu-arrange--node (session id)
  "Return node ID from SESSION, or nil."
  (gethash id (majutsu-arrange-session-nodes session)))

(defun majutsu-arrange--target-id-p (session id)
  "Return non-nil when ID is a target node in SESSION."
  (majutsu-arrange--id-member-p id (majutsu-arrange-session-target-ids session)))

(defun majutsu-arrange--current-id (session)
  "Return selected target id from SESSION."
  (or (majutsu-arrange-session-selected-id session)
      (car (majutsu-arrange-session-current-order session))
      (user-error "No target revision is selected")))

(defun majutsu-arrange--current-node (session)
  "Return selected target node from SESSION."
  (or (majutsu-arrange--node session (majutsu-arrange--current-id session))
      (user-error "Selected arrange node is unavailable")))

(defun majutsu-arrange--split-list-field (value)
  "Split list field VALUE using `majutsu-arrange--list-separator'."
  (if (or (null value) (string-empty-p value))
      nil
    (split-string value (regexp-quote majutsu-arrange--list-separator) t)))

(defun majutsu-arrange--parse-node-line (line &optional role)
  "Parse one machine template LINE into an arrange node with ROLE."
  (let* ((fields (majutsu--split-fields (or line "")
                                        majutsu-arrange--field-separator))
         (change-id (nth 0 fields))
         (commit-id (nth 1 fields))
         (short-change-id (nth 2 fields))
         (short-commit-id (nth 3 fields))
         (description (nth 4 fields))
         (parents (majutsu-arrange--split-list-field (nth 5 fields))))
    (when (and change-id commit-id (not (string-empty-p commit-id)))
      (majutsu-arrange-node-create
       :id commit-id
       :change-id change-id
       :commit-id commit-id
       :short-change-id short-change-id
       :short-commit-id short-commit-id
       :description (or description "")
       :parents parents
       :original-parents (copy-sequence parents)
       :action 'keep
       :role (or role 'target)))))

(defun majutsu-arrange--revset-for-commit-ids (ids)
  "Return a revset expression matching commit IDS."
  (when ids
    (string-join (mapcar (lambda (id) (format "commit_id(%s)" id)) ids)
                 " | ")))

(defun majutsu-arrange--first-revset-commit (revset)
  "Return the first commit id matching REVSET, or nil."
  (car (majutsu-jj-lines "log" "--no-graph" "--limit" "1"
                         "-r" revset
                         "-T" "commit_id ++ \"\\n\"")))

(defun majutsu-arrange--current-operation-id ()
  "Return the current jj operation id, or nil when unavailable."
  (car (majutsu-jj-lines "op" "log" "--no-graph" "--limit" "1"
                         "-T" "id ++ \"\\n\"")))

(defun majutsu-arrange--configured-default-revset ()
  "Return the configured arrange revset or the built-in fallback."
  (or (ignore-errors (majutsu-get "revsets.arrange"))
      majutsu-arrange--default-revset))

(defun majutsu-arrange--normalize-revsets (revsets)
  "Return one revset expression for REVSETS.

REVSETS may be nil, a single revset string, or a list of revset strings.  A list
is treated like `jj arrange [REVSETS]...': the individual revsets are unioned
together.  Nil or an empty list uses `revsets.arrange' with the built-in
fallback."
  (let* ((items (cond
                 ((null revsets) nil)
                 ((stringp revsets) (list revsets))
                 ((and (listp revsets)
                       (listp (car revsets))
                       (null (cdr revsets)))
                  (car revsets))
                 ((listp revsets) revsets)
                 (t (user-error "Invalid arrange revsets: %S" revsets))))
         (items (mapcar (lambda (revset)
                          (unless (stringp revset)
                            (user-error "Invalid arrange revset: %S" revset))
                          (string-trim revset))
                        items))
         (items (seq-remove #'string-empty-p items))
         (items (or items (list (majutsu-arrange--configured-default-revset)))))
    (if (cdr items)
        (string-join (mapcar (lambda (revset) (format "(%s)" revset))
                             items)
                     " | ")
      (car items))))

(defun majutsu-arrange--load-nodes (revset role)
  "Load arrange nodes for REVSET and assign ROLE."
  (when (and revset (not (string-empty-p revset)))
    (delq nil
          (mapcar (lambda (line)
                    (majutsu-arrange--parse-node-line line role))
                  (majutsu-jj-lines "log" "--no-graph" "-r" revset
                                    "-T" majutsu-arrange--node-template)))))

(defun majutsu-arrange--validate-target-revset (revset target-nodes)
  "Validate REVSET using already loaded TARGET-NODES."
  (unless target-nodes
    (user-error "No revisions to arrange"))
  (when-let* ((gap (majutsu-arrange--first-revset-commit
                    (format "connected((%s)) ~ (%s)" revset revset))))
    (user-error "Cannot arrange revset with gaps in; revision %s would need to be included"
                gap))
  (when-let* ((immutable (majutsu-arrange--first-revset-commit
                          (format "(%s) & immutable()" revset))))
    (user-error "Cannot arrange immutable revision %s" immutable)))

(defun majutsu-arrange--compute-external-parent-ids (target-nodes target-ids)
  "Return external parent ids for TARGET-NODES not in TARGET-IDS."
  (let ((target-set (majutsu-arrange--id-set target-ids))
        parents)
    (dolist (node target-nodes)
      (dolist (parent (majutsu-arrange-node-parents node))
        (unless (gethash parent target-set)
          (setq parents (majutsu-arrange--push-unique parent parents)))))
    parents))

(defun majutsu-arrange--merge-node! (session node)
  "Merge NODE into SESSION's node table without replacing target nodes."
  (let* ((nodes (majutsu-arrange-session-nodes session))
         (id (majutsu-arrange-node-id node))
         (existing (gethash id nodes)))
    (unless (and existing (eq (majutsu-arrange-node-role existing) 'target))
      (puthash id node nodes))))

(defun majutsu-arrange--compute-head-order (session)
  "Compute target head order for SESSION."
  (let* ((target-ids (majutsu-arrange-session-target-ids session))
         (target-set (majutsu-arrange--id-set target-ids))
         (non-heads (make-hash-table :test #'equal)))
    (dolist (id target-ids)
      (when-let* ((node (majutsu-arrange--node session id)))
        (dolist (parent (majutsu-arrange-node-parents node))
          (when (gethash parent target-set)
            (puthash parent t non-heads)))))
    (seq-remove (lambda (id) (gethash id non-heads)) target-ids)))

(defun majutsu-arrange--compute-current-order (session)
  "Compute target display order for SESSION."
  (let* ((target-ids (majutsu-arrange-session-target-ids session))
         (target-set (majutsu-arrange--id-set target-ids))
         (child-count (make-hash-table :test #'equal))
         (queued (make-hash-table :test #'equal))
         (emitted (make-hash-table :test #'equal))
         (queue nil)
         order)
    (dolist (id target-ids)
      (puthash id 0 child-count))
    (dolist (id target-ids)
      (when-let* ((node (majutsu-arrange--node session id)))
        (dolist (parent (majutsu-arrange-node-parents node))
          (when (gethash parent target-set)
            (puthash parent (1+ (gethash parent child-count 0)) child-count)))))
    (dolist (id (majutsu-arrange-session-head-order session))
      (when (gethash id target-set)
        (setq queue (append queue (list id)))
        (puthash id t queued)))
    (while queue
      (let ((id (pop queue)))
        (unless (gethash id emitted)
          (puthash id t emitted)
          (push id order)
          (when-let* ((node (majutsu-arrange--node session id)))
            (dolist (parent (majutsu-arrange-node-parents node))
              (when (gethash parent target-set)
                (let ((new-count (max 0 (1- (gethash parent child-count 0)))))
                  (puthash parent new-count child-count)
                  (when (and (zerop new-count)
                             (not (gethash parent queued)))
                    (setq queue (append queue (list parent)))
                    (puthash parent t queued)))))))))
    (setq order (nreverse order))
    ;; Be defensive around malformed/cyclic in-memory states.  The real jj
    ;; graph should be acyclic, but tests and future editing operations may
    ;; construct partial states.
    (dolist (id target-ids)
      (unless (gethash id emitted)
        (setq order (append order (list id)))))
    order))

(defun majutsu-arrange--refresh-derived-order (session)
  "Recompute derived order fields for SESSION."
  (setf (majutsu-arrange-session-head-order session)
        (majutsu-arrange--compute-head-order session))
  (setf (majutsu-arrange-session-current-order session)
        (majutsu-arrange--compute-current-order session))
  (unless (member (majutsu-arrange-session-selected-id session)
                  (majutsu-arrange-session-current-order session))
    (setf (majutsu-arrange-session-selected-id session)
          (car (majutsu-arrange-session-current-order session))))
  session)

(defun majutsu-arrange-build-session (revsets &optional directory)
  "Build an arrange session for REVSETS in DIRECTORY.

REVSETS may be nil, a single revset string, or a list of revset strings.  A list
matches `jj arrange [REVSETS]...' by arranging the union of all revsets."
  (let* ((root (majutsu--toplevel-safe directory))
         (default-directory root)
         (revset (majutsu-arrange--normalize-revsets revsets))
         (target-nodes (majutsu-arrange--load-nodes revset 'target)))
    (majutsu-arrange--validate-target-revset revset target-nodes)
    (let* ((target-ids (mapcar #'majutsu-arrange-node-id target-nodes))
           (external-parent-ids
            (majutsu-arrange--compute-external-parent-ids target-nodes target-ids))
           (external-parent-revset
            (majutsu-arrange--revset-for-commit-ids external-parent-ids))
           (external-parent-nodes
            (majutsu-arrange--load-nodes external-parent-revset 'external-parent))
           (external-child-revset
            (format "children((%s)) ~ (%s)" revset revset))
           (external-child-nodes
            (majutsu-arrange--load-nodes external-child-revset 'external-child))
           (external-child-ids (mapcar #'majutsu-arrange-node-id external-child-nodes))
           (nodes (make-hash-table :test #'equal))
           (session (majutsu-arrange-session-create
                     :root root
                     :revset revset
                     :base-operation-id (majutsu-arrange--current-operation-id)
                     :nodes nodes
                     :target-ids target-ids
                     :external-parent-ids external-parent-ids
                     :external-child-ids external-child-ids
                     :head-order nil
                     :current-order nil
                     :selected-id (car target-ids)
                     :marked-ids nil
                     :subject-scope 'subdag
                     :preview-anchor-ids nil
                     :operations nil
                     :dirty-p nil
                     :applying-p nil)))
      (dolist (node target-nodes)
        (puthash (majutsu-arrange-node-id node) node nodes))
      (dolist (node external-child-nodes)
        (majutsu-arrange--merge-node! session node))
      (dolist (node external-parent-nodes)
        (majutsu-arrange--merge-node! session node))
      (majutsu-arrange--refresh-derived-order session))))

;;; Graph editing

(defun majutsu-arrange--replace-id (value a-id b-id)
  "Swap A-ID and B-ID when VALUE matches either id."
  (cond ((equal value a-id) b-id)
        ((equal value b-id) a-id)
        (t value)))

(defun majutsu-arrange--swap-ids-in-list (values a-id b-id)
  "Return VALUES with A-ID and B-ID swapped."
  (mapcar (lambda (value) (majutsu-arrange--replace-id value a-id b-id)) values))

(defun majutsu-arrange--swap-commits (session a-id b-id)
  "Swap commits A-ID and B-ID in SESSION's planned graph."
  (unless (equal a-id b-id)
    (let ((a-node (majutsu-arrange--node session a-id))
          (b-node (majutsu-arrange--node session b-id)))
      (unless (and a-node b-node)
        (user-error "Cannot swap unavailable arrange nodes"))
      (maphash (lambda (_ node)
                 (setf (majutsu-arrange-node-parents node)
                       (majutsu-arrange--swap-ids-in-list
                        (majutsu-arrange-node-parents node) a-id b-id)))
               (majutsu-arrange-session-nodes session))
      (let ((a-parents (majutsu-arrange-node-parents a-node))
            (b-parents (majutsu-arrange-node-parents b-node)))
        (setf (majutsu-arrange-node-parents a-node) b-parents)
        (setf (majutsu-arrange-node-parents b-node) a-parents))
      (setf (majutsu-arrange-session-head-order session)
            (majutsu-arrange--swap-ids-in-list
             (majutsu-arrange-session-head-order session) a-id b-id))
      (majutsu-arrange--refresh-derived-order session))))

(defun majutsu-arrange--record-operation (session operation)
  "Append OPERATION to SESSION's operation log."
  (setf (majutsu-arrange-session-operations session)
        (append (majutsu-arrange-session-operations session) (list operation))))

(defun majutsu-arrange--mark-dirty (session)
  "Mark SESSION and current buffer dirty."
  (setf (majutsu-arrange-session-dirty-p session) t)
  (set-buffer-modified-p t))

(defun majutsu-arrange--children-of (session id)
  "Return all node ids whose current parent list contains ID."
  (let (children)
    (maphash (lambda (node-id node)
               (when (member id (majutsu-arrange-node-parents node))
                 (push node-id children)))
             (majutsu-arrange-session-nodes session))
    (nreverse children)))

(defun majutsu-arrange--target-children-of (session id)
  "Return target node ids whose current parent list contains ID."
  (seq-filter (lambda (child) (majutsu-arrange--target-id-p session child))
              (majutsu-arrange--children-of session id)))

(defun majutsu-arrange--ids-in-current-order (session ids)
  "Return IDS sorted by SESSION's current target order."
  (let ((set (majutsu-arrange--id-set ids))
        ordered)
    (dolist (id (majutsu-arrange-session-current-order session))
      (when (gethash id set)
        (push id ordered)))
    (nreverse ordered)))

(defun majutsu-arrange--descendant-ids (session id)
  "Return target-local descendants of ID, including ID."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (cl-labels ((visit
                  (node-id)
                  (unless (gethash node-id seen)
                    (puthash node-id t seen)
                    (push node-id result)
                    (dolist (child (majutsu-arrange--target-children-of session node-id))
                      (visit child)))))
      (visit id))
    (majutsu-arrange--ids-in-current-order session (nreverse result))))

(defun majutsu-arrange--stack-ids (session id)
  "Return the target-local linear stack around ID."
  (let ((ids (list id))
        (cursor id)
        done)
    (while (not done)
      (let ((children (majutsu-arrange--target-children-of session cursor)))
        (if (not (= (length children) 1))
            (setq done t)
          (let* ((child (car children))
                 (child-node (majutsu-arrange--node session child))
                 (target-parents (seq-filter
                                  (lambda (parent)
                                    (majutsu-arrange--target-id-p session parent))
                                  (majutsu-arrange-node-parents child-node))))
            (if (not (and (= (length target-parents) 1)
                          (equal (car target-parents) cursor)))
                (setq done t)
              (push child ids)
              (setq cursor child))))))
    (setq cursor id
          done nil)
    (while (not done)
      (let* ((node (majutsu-arrange--node session cursor))
             (parents (seq-filter
                       (lambda (parent)
                         (majutsu-arrange--target-id-p session parent))
                       (majutsu-arrange-node-parents node))))
        (if (not (= (length parents) 1))
            (setq done t)
          (let* ((parent (car parents))
                 (parent-children (majutsu-arrange--target-children-of session parent)))
            (if (not (and (= (length parent-children) 1)
                          (equal (car parent-children) cursor)))
                (setq done t)
              (setq ids (append ids (list parent)))
              (setq cursor parent))))))
    (majutsu-arrange--ids-in-current-order session ids)))

(defun majutsu-arrange--raw-subject-ids (session &optional scope)
  "Return current subject ids in SESSION for SCOPE without validation."
  (let* ((scope (or scope
                    (majutsu-arrange-session-subject-scope session)
                    'subdag))
         (selected (majutsu-arrange--current-id session))
         (marked (majutsu-arrange-session-marked-ids session)))
    (pcase scope
      ('single (list selected))
      ('stack (majutsu-arrange--stack-ids session selected))
      ('subdag (majutsu-arrange--descendant-ids session selected))
      ('branch (majutsu-arrange--descendant-ids session selected))
      ('marked (if marked
                   (majutsu-arrange--ids-in-current-order session marked)
                 (list selected)))
      (_ (list selected)))))

(defun majutsu-arrange--connected-ids-p (session ids)
  "Return non-nil when IDS are connected in SESSION's current graph."
  (or (null ids)
      (let* ((id-set (majutsu-arrange--id-set ids))
             (seen (make-hash-table :test #'equal))
             (queue (list (car ids))))
        (while queue
          (let ((id (pop queue)))
            (unless (gethash id seen)
              (puthash id t seen)
              (when-let* ((node (majutsu-arrange--node session id)))
                (dolist (parent (majutsu-arrange-node-parents node))
                  (when (and (gethash parent id-set)
                             (not (gethash parent seen)))
                    (push parent queue))))
              (dolist (child (majutsu-arrange--children-of session id))
                (when (and (gethash child id-set)
                           (not (gethash child seen)))
                  (push child queue))))))
        (seq-every-p (lambda (id) (gethash id seen)) ids))))

(defun majutsu-arrange--validate-subject-ids (session ids)
  "Validate IDS as a movable arrange subject in SESSION."
  (unless ids
    (user-error "No arrange subject selected"))
  (dolist (id ids)
    (unless (majutsu-arrange--target-id-p session id)
      (user-error "Arrange subject contains non-target revision %s" id)))
  (unless (majutsu-arrange--connected-ids-p session ids)
    (user-error "Arrange subject must be connected"))
  ids)

(defun majutsu-arrange--subject-edges (session ids)
  "Return (ROOTS HEADS INCOMING OUTGOING) for subject IDS in SESSION."
  (let* ((id-set (majutsu-arrange--id-set ids))
         roots heads incoming outgoing)
    (dolist (id ids)
      (let* ((node (majutsu-arrange--node session id))
             (parents (and node (majutsu-arrange-node-parents node)))
             (internal-parent-p nil))
        (dolist (parent parents)
          (if (gethash parent id-set)
              (setq internal-parent-p t)
            (push (cons id parent) incoming)))
        (unless internal-parent-p
          (push id roots))
        (let ((internal-child-p nil))
          (dolist (child (majutsu-arrange--children-of session id))
            (if (gethash child id-set)
                (setq internal-child-p t)
              (push (cons child id) outgoing)))
          (unless internal-child-p
            (push id heads)))))
    (list (nreverse roots) (nreverse heads)
          (nreverse incoming) (nreverse outgoing))))

(defun majutsu-arrange--make-subject (session &optional scope ids)
  "Build and validate an arrange subject for SESSION.
Use SCOPE and IDS when non-nil; otherwise use SESSION's current subject state."
  (let* ((scope (or scope
                    (majutsu-arrange-session-subject-scope session)
                    'subdag))
         (ids (majutsu-arrange--validate-subject-ids
               session
               (or ids (majutsu-arrange--raw-subject-ids session scope))))
         (edges (majutsu-arrange--subject-edges session ids)))
    (majutsu-arrange-subject-create
     :ids ids
     :roots (nth 0 edges)
     :heads (nth 1 edges)
     :incoming-edges (nth 2 edges)
     :outgoing-edges (nth 3 edges)
     :scope scope)))

(defun majutsu-arrange--current-subject-ids (session)
  "Return the current subject ids for rendering SESSION."
  (ignore-errors
    (majutsu-arrange-subject-ids (majutsu-arrange--make-subject session))))

(defun majutsu-arrange--replace-parent-set (parents parent-set replacements)
  "Return PARENTS with members of PARENT-SET replaced by REPLACEMENTS."
  (let (result)
    (dolist (parent parents)
      (if (gethash parent parent-set)
          (setq result (majutsu-arrange--append-unique result replacements))
        (setq result (majutsu-arrange--push-unique parent result))))
    result))

(defun majutsu-arrange--subject-old-root-parents (subject)
  "Return SUBJECT's old root parents outside the subject."
  (let (parents)
    (dolist (edge (majutsu-arrange-subject-incoming-edges subject))
      (setq parents (majutsu-arrange--push-unique (cdr edge) parents)))
    parents))

(defun majutsu-arrange--detach-subject (session subject)
  "Fill the old hole left by moving SUBJECT in SESSION."
  (let ((subject-set (majutsu-arrange--id-set
                      (majutsu-arrange-subject-ids subject)))
        (head-set (majutsu-arrange--id-set
                   (majutsu-arrange-subject-heads subject)))
        (old-root-parents (majutsu-arrange--subject-old-root-parents subject)))
    (maphash
     (lambda (id node)
       (unless (or (gethash id subject-set)
                   (eq (majutsu-arrange-node-role node) 'external-parent))
         (setf (majutsu-arrange-node-parents node)
               (majutsu-arrange--replace-parent-set
                (majutsu-arrange-node-parents node)
                head-set old-root-parents))))
     (majutsu-arrange-session-nodes session))))

(defun majutsu-arrange--set-subject-root-parents (session subject parents)
  "Set SUBJECT root parents to PARENTS in SESSION."
  (dolist (root (majutsu-arrange-subject-roots subject))
    (when-let* ((node (majutsu-arrange--node session root)))
      (setf (majutsu-arrange-node-parents node) (copy-sequence parents)))))

(defun majutsu-arrange--ensure-anchors (session subject anchor-ids)
  "Validate ANCHOR-IDS for moving SUBJECT in SESSION."
  (unless anchor-ids
    (user-error "No arrange anchor selected"))
  (let ((subject-set (majutsu-arrange--id-set
                      (majutsu-arrange-subject-ids subject))))
    (dolist (anchor anchor-ids)
      (unless (majutsu-arrange--node session anchor)
        (user-error "Arrange anchor %s is unavailable" anchor))
      (when (gethash anchor subject-set)
        (user-error "Arrange anchor must be outside the subject"))))
  anchor-ids)

(defun majutsu-arrange--move-subject-onto (session subject parent-ids)
  "Move SUBJECT onto PARENT-IDS in SESSION, preserving internal edges."
  (majutsu-arrange--ensure-anchors session subject parent-ids)
  (majutsu-arrange--detach-subject session subject)
  (majutsu-arrange--set-subject-root-parents session subject parent-ids)
  (majutsu-arrange--refresh-derived-order session))

(defun majutsu-arrange--insert-subject-after (session subject anchor-ids)
  "Insert SUBJECT after ANCHOR-IDS in SESSION, preserving internal edges."
  (majutsu-arrange--ensure-anchors session subject anchor-ids)
  (let ((subject-set (majutsu-arrange--id-set
                      (majutsu-arrange-subject-ids subject)))
        (anchor-set (majutsu-arrange--id-set anchor-ids))
        (heads (majutsu-arrange-subject-heads subject)))
    (majutsu-arrange--detach-subject session subject)
    (maphash
     (lambda (id node)
       (unless (or (gethash id subject-set)
                   (eq (majutsu-arrange-node-role node) 'external-parent))
         (setf (majutsu-arrange-node-parents node)
               (majutsu-arrange--replace-parent-set
                (majutsu-arrange-node-parents node)
                anchor-set heads))))
     (majutsu-arrange-session-nodes session))
    (majutsu-arrange--set-subject-root-parents session subject anchor-ids)
    (majutsu-arrange--refresh-derived-order session)))

(defun majutsu-arrange--insert-subject-before (session subject anchor-ids)
  "Insert SUBJECT before ANCHOR-IDS in SESSION, preserving internal edges."
  (majutsu-arrange--ensure-anchors session subject anchor-ids)
  (let ((heads (majutsu-arrange-subject-heads subject))
        (subject-set (majutsu-arrange--id-set
                      (majutsu-arrange-subject-ids subject)))
        (old-root-parents (majutsu-arrange--subject-old-root-parents subject))
        (new-root-parents nil))
    (dolist (anchor anchor-ids)
      (when-let* ((node (majutsu-arrange--node session anchor)))
        (dolist (parent (majutsu-arrange-node-parents node))
          (if (gethash parent subject-set)
              (setq new-root-parents
                    (majutsu-arrange--append-unique
                     new-root-parents old-root-parents))
            (setq new-root-parents
                  (majutsu-arrange--push-unique parent new-root-parents))))))
    (majutsu-arrange--detach-subject session subject)
    (majutsu-arrange--set-subject-root-parents session subject new-root-parents)
    (dolist (anchor anchor-ids)
      (when-let* ((node (majutsu-arrange--node session anchor)))
        (setf (majutsu-arrange-node-parents node) (copy-sequence heads))))
    (majutsu-arrange--refresh-derived-order session)))

(defun majutsu-arrange-swap-down ()
  "Swap the selected commit with its unique target parent."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (id (majutsu-arrange--current-id session))
         (node (majutsu-arrange--current-node session))
         (parents (majutsu-arrange-node-parents node)))
    (pcase parents
      (`(,parent)
       (if (not (majutsu-arrange--target-id-p session parent))
           (message "Selected revision's unique parent is outside the arrange target")
         (majutsu-arrange--swap-commits session id parent)
         (majutsu-arrange--record-operation session (list :swap parent id))
         (majutsu-arrange--mark-dirty session)
         (majutsu-arrange--rerender)))
      (_ (message "Selected revision does not have exactly one parent")))))

(defun majutsu-arrange-swap-up ()
  "Swap the selected commit with its unique target child."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (id (majutsu-arrange--current-id session))
         (children (majutsu-arrange--children-of session id)))
    (pcase children
      (`(,child)
       (if (not (majutsu-arrange--target-id-p session child))
           (message "Selected revision's unique child is outside the arrange target")
         (majutsu-arrange--swap-commits session id child)
         (majutsu-arrange--record-operation session (list :swap id child))
         (majutsu-arrange--mark-dirty session)
         (majutsu-arrange--rerender)))
      (_ (message "Selected revision does not have exactly one child")))))

(defun majutsu-arrange--move-selection (delta)
  "Move selection by DELTA in the current arrange session."
  (let* ((session (majutsu-arrange--session))
         (order (majutsu-arrange-session-current-order session))
         (id (majutsu-arrange--current-id session))
         (idx (or (cl-position id order :test #'equal) 0))
         (new-idx (min (1- (length order)) (max 0 (+ idx delta)))))
    (when (and order (not (= idx new-idx)))
      (setf (majutsu-arrange-session-selected-id session) (nth new-idx order))
      (majutsu-arrange--rerender))))

(defun majutsu-arrange-next ()
  "Move to the next target revision in the arrange buffer."
  (interactive)
  (majutsu-arrange--move-selection 1))

(defun majutsu-arrange-previous ()
  "Move to the previous target revision in the arrange buffer."
  (interactive)
  (majutsu-arrange--move-selection -1))

(defun majutsu-arrange--set-action (action)
  "Set selected target node ACTION."
  (let* ((session (majutsu-arrange--session))
         (node (majutsu-arrange--current-node session)))
    (setf (majutsu-arrange-node-action node) action)
    (majutsu-arrange--mark-dirty session)
    (majutsu-arrange--rerender)))

(defun majutsu-arrange-mark-abandon ()
  "Mark the selected revision for abandonment."
  (interactive)
  (majutsu-arrange--set-action 'abandon))

(defun majutsu-arrange-mark-keep ()
  "Mark the selected revision to be kept."
  (interactive)
  (majutsu-arrange--set-action 'keep))

(defun majutsu-arrange-toggle-mark ()
  "Toggle the selected target revision in the explicit arrange subject."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (id (majutsu-arrange--current-id session))
         (marked (majutsu-arrange-session-marked-ids session)))
    (if (member id marked)
        (setf (majutsu-arrange-session-marked-ids session)
              (remove id marked))
      (setf (majutsu-arrange-session-marked-ids session)
            (majutsu-arrange--push-unique id marked)))
    (setf (majutsu-arrange-session-subject-scope session) 'marked)
    (majutsu-arrange--rerender)))

(defun majutsu-arrange-clear-marks ()
  "Clear the explicit arrange subject marks."
  (interactive)
  (let ((session (majutsu-arrange--session)))
    (setf (majutsu-arrange-session-marked-ids session) nil)
    (when (eq (majutsu-arrange-session-subject-scope session) 'marked)
      (setf (majutsu-arrange-session-subject-scope session) 'subdag))
    (majutsu-arrange--rerender)))

(defun majutsu-arrange-cycle-subject-scope ()
  "Cycle the arrange subject scope."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (scopes (if (majutsu-arrange-session-marked-ids session)
                     '(subdag stack single marked branch)
                   '(subdag stack single branch)))
         (current (or (majutsu-arrange-session-subject-scope session) 'subdag))
         (tail (cdr (memq current scopes))))
    (setf (majutsu-arrange-session-subject-scope session)
          (or (car tail) (car scopes)))
    (message "Arrange subject scope: %s"
             (majutsu-arrange-session-subject-scope session))
    (majutsu-arrange--rerender)))

(defun majutsu-arrange--anchor-candidates (session subject)
  "Return completion candidates for anchors outside SUBJECT in SESSION."
  (let ((subject-set (majutsu-arrange--id-set
                      (majutsu-arrange-subject-ids subject)))
        candidates)
    (dolist (id (majutsu-arrange--display-order session))
      (unless (gethash id subject-set)
        (when-let* ((node (majutsu-arrange--node session id)))
          (push (cons (format "%s  %-8s  %s"
                              (pcase (majutsu-arrange-node-role node)
                                ('target "target ")
                                ('external-parent "parent ")
                                ('external-child "child  ")
                                (_ "node   "))
                              (or (majutsu-arrange-node-short-change-id node)
                                  (majutsu-arrange-node-short-commit-id node)
                                  "")
                              (or (majutsu-arrange-node-description node) ""))
                      id)
                candidates))))
    (nreverse candidates)))

(defun majutsu-arrange--read-anchor-ids (session subject prompt)
  "Read anchor ids for SUBJECT in SESSION with PROMPT.
If the current selection is outside SUBJECT, use it as the anchor."
  (let* ((selected (majutsu-arrange-session-selected-id session))
         (subject-set (majutsu-arrange--id-set
                       (majutsu-arrange-subject-ids subject))))
    (if (and selected (not (gethash selected subject-set)))
        (list selected)
      (let* ((candidates (majutsu-arrange--anchor-candidates session subject))
             (choice (completing-read prompt candidates nil t)))
        (list (cdr (assoc choice candidates)))))))

(defun majutsu-arrange--move-subject-command (operation anchor-prompt mutator)
  "Apply OPERATION using anchor read by ANCHOR-PROMPT and graph MUTATOR."
  (let* ((session (majutsu-arrange--session))
         (subject (majutsu-arrange--make-subject session))
         (anchors (majutsu-arrange--read-anchor-ids session subject anchor-prompt)))
    (funcall mutator session subject anchors)
    (majutsu-arrange--record-operation
     session
     (list operation
           (copy-sequence (majutsu-arrange-subject-ids subject))
           (copy-sequence anchors)))
    (majutsu-arrange--mark-dirty session)
    (majutsu-arrange--rerender)))

(defun majutsu-arrange-move-onto ()
  "Move the current arrange subject onto selected parent revisions."
  (interactive)
  (majutsu-arrange--move-subject-command
   :move-onto
   "Move subject onto: "
   #'majutsu-arrange--move-subject-onto))

(defun majutsu-arrange-insert-after ()
  "Insert the current arrange subject after selected anchor revisions."
  (interactive)
  (majutsu-arrange--move-subject-command
   :insert-after
   "Insert subject after: "
   #'majutsu-arrange--insert-subject-after))

(defun majutsu-arrange-insert-before ()
  "Insert the current arrange subject before selected anchor revisions."
  (interactive)
  (majutsu-arrange--move-subject-command
   :insert-before
   "Insert subject before: "
   #'majutsu-arrange--insert-subject-before))

;;; Plan and CLI backend

(defun majutsu-arrange--parent-changed-ids (session)
  "Return node ids whose planned parents differ from the loaded graph."
  (let (ids)
    (maphash
     (lambda (id node)
       (unless (eq (majutsu-arrange-node-role node) 'external-parent)
         (unless (equal (majutsu-arrange-node-parents node)
                        (majutsu-arrange-node-original-parents node))
           (setq ids (majutsu-arrange--push-unique id ids)))))
     (majutsu-arrange-session-nodes session))
    ids))

(defun majutsu-arrange--affected-ids (session)
  "Return arrange node ids affected by SESSION's plan."
  (let ((ids (majutsu-arrange--parent-changed-ids session)))
    (dolist (operation (majutsu-arrange-session-operations session))
      (pcase operation
        (`(:swap ,parent ,child)
         (setq ids (majutsu-arrange--push-unique parent ids))
         (setq ids (majutsu-arrange--push-unique child ids)))
        (`(:move-onto ,subject-ids ,_parent-ids)
         (setq ids (majutsu-arrange--append-unique ids subject-ids)))
        (`(:insert-after ,subject-ids ,_anchor-ids)
         (setq ids (majutsu-arrange--append-unique ids subject-ids)))
        (`(:insert-before ,subject-ids ,_anchor-ids)
         (setq ids (majutsu-arrange--append-unique ids subject-ids)))))
    (dolist (id (majutsu-arrange-session-target-ids session))
      (when-let* ((node (majutsu-arrange--node session id)))
        (when (eq (majutsu-arrange-node-action node) 'abandon)
          (setq ids (majutsu-arrange--push-unique id ids)))))
    ids))

(defun majutsu-arrange--make-plan (session)
  "Build an arrange plan from SESSION."
  (let ((parents (make-hash-table :test #'equal))
        (actions (make-hash-table :test #'equal)))
    (dolist (id (majutsu-arrange-session-target-ids session))
      (when-let* ((node (majutsu-arrange--node session id)))
        (puthash id (copy-sequence (majutsu-arrange-node-parents node)) parents)
        (puthash id (majutsu-arrange-node-action node) actions)))
    (majutsu-arrange-plan-create
     :base-operation-id (majutsu-arrange-session-base-operation-id session)
     :final-parents parents
     :final-actions actions
     :operations (copy-sequence (majutsu-arrange-session-operations session))
     :affected-ids (majutsu-arrange--affected-ids session)
     :command-preview (majutsu-arrange--compile-commands session))))

(defun majutsu-arrange--node-change-id (session id)
  "Return SESSION node ID's change id."
  (or (and-let* ((node (majutsu-arrange--node session id)))
        (majutsu-arrange-node-change-id node))
      (user-error "Arrange node %s is unavailable" id)))

(defun majutsu-arrange--subject-revset (session ids)
  "Return an exact revset for subject IDS in SESSION."
  (string-join (mapcar (lambda (id)
                         (majutsu-arrange--node-change-id session id))
                       ids)
               " | "))

(defun majutsu-arrange--destination-args (session flag ids)
  "Return repeated destination FLAG arguments for IDS in SESSION."
  (let (args)
    (dolist (id ids (nreverse args))
      (push flag args)
      (push (majutsu-arrange--node-change-id session id) args))))

(defun majutsu-arrange--compile-commands (session)
  "Compile SESSION's plan to a CLI replay command list."
  (let (commands)
    (dolist (operation (majutsu-arrange-session-operations session))
      (pcase operation
        (`(:swap ,parent ,child)
         (push (list "rebase"
                     "-r" (majutsu-arrange--node-change-id session parent)
                     "-A" (majutsu-arrange--node-change-id session child)
                     "--keep-divergent")
               commands))
        (`(:move-onto ,subject-ids ,parent-ids)
         (push (append (list "rebase"
                             "-r" (majutsu-arrange--subject-revset session subject-ids))
                       (majutsu-arrange--destination-args session "-o" parent-ids)
                       (list "--keep-divergent"))
               commands))
        (`(:insert-after ,subject-ids ,anchor-ids)
         (push (append (list "rebase"
                             "-r" (majutsu-arrange--subject-revset session subject-ids))
                       (majutsu-arrange--destination-args session "-A" anchor-ids)
                       (list "--keep-divergent"))
               commands))
        (`(:insert-before ,subject-ids ,anchor-ids)
         (push (append (list "rebase"
                             "-r" (majutsu-arrange--subject-revset session subject-ids))
                       (majutsu-arrange--destination-args session "-B" anchor-ids)
                       (list "--keep-divergent"))
               commands))))
    (let (abandon)
      (dolist (id (majutsu-arrange-session-target-ids session))
        (when-let* ((node (majutsu-arrange--node session id)))
          (when (eq (majutsu-arrange-node-action node) 'abandon)
            (push (majutsu-arrange-node-change-id node) abandon))))
      (when abandon
        (push (cons "abandon" (nreverse abandon)) commands)))
    (nreverse commands)))

(defun majutsu-arrange--command-argument-ids (session)
  "Return node ids used as CLI command arguments for SESSION."
  (let (ids)
    (dolist (operation (majutsu-arrange-session-operations session))
      (pcase operation
        (`(:swap ,parent ,child)
         (setq ids (majutsu-arrange--push-unique parent ids))
         (setq ids (majutsu-arrange--push-unique child ids)))
        (`(:move-onto ,subject-ids ,parent-ids)
         (setq ids (majutsu-arrange--append-unique ids subject-ids))
         (setq ids (majutsu-arrange--append-unique ids parent-ids)))
        (`(:insert-after ,subject-ids ,anchor-ids)
         (setq ids (majutsu-arrange--append-unique ids subject-ids))
         (setq ids (majutsu-arrange--append-unique ids anchor-ids)))
        (`(:insert-before ,subject-ids ,anchor-ids)
         (setq ids (majutsu-arrange--append-unique ids subject-ids))
         (setq ids (majutsu-arrange--append-unique ids anchor-ids)))))
    (dolist (id (majutsu-arrange-session-target-ids session))
      (when-let* ((node (majutsu-arrange--node session id)))
        (when (eq (majutsu-arrange-node-action node) 'abandon)
          (setq ids (majutsu-arrange--push-unique id ids)))))
    ids))

(defun majutsu-arrange--change-ids-for (session ids)
  "Return change ids for IDS in SESSION."
  (let (change-ids)
    (dolist (id ids)
      (setq change-ids
            (majutsu-arrange--push-unique
             (majutsu-arrange--node-change-id session id)
             change-ids)))
    change-ids))

(defun majutsu-arrange--affected-change-ids (session)
  "Return affected change ids for SESSION."
  (majutsu-arrange--change-ids-for session
                                   (majutsu-arrange--affected-ids session)))

(defun majutsu-arrange--ensure-current-operation (session)
  "Signal an error if SESSION is stale."
  (let ((base (majutsu-arrange-session-base-operation-id session))
        (current (majutsu-arrange--current-operation-id)))
    (when (and base current (not (equal base current)))
      (user-error "Repository changed since arrange session was created; reload before applying"))))

(defun majutsu-arrange--ambiguous-change-ids (change-ids)
  "Return CHANGE-IDS that do not resolve to exactly one commit."
  (let (ambiguous)
    (dolist (change-id change-ids)
      (unless (= (length (majutsu-jj-lines "log" "--no-graph" "-r" change-id
                                           "-T" "commit_id ++ \"\\n\""))
                 1)
        (push change-id ambiguous)))
    (nreverse ambiguous)))

(defun majutsu-arrange--preflight-apply (session commands)
  "Run preflight checks for applying SESSION with COMMANDS."
  (when (majutsu-arrange-session-applying-p session)
    (user-error "Arrange plan is already being applied"))
  (unless commands
    (user-error "No arrange changes to apply"))
  (majutsu-arrange--ensure-current-operation session)
  (let ((argument-change-ids
         (majutsu-arrange--change-ids-for
          session (majutsu-arrange--command-argument-ids session)))
        (affected-change-ids (majutsu-arrange--affected-change-ids session)))
    (when-let* ((ambiguous (majutsu-arrange--ambiguous-change-ids argument-change-ids)))
      (user-error "Cannot apply arrange plan with ambiguous change ids: %s"
                  (string-join ambiguous ", ")))
    (when affected-change-ids
      (let ((revset (string-join affected-change-ids " | ")))
        (when-let* ((immutable (majutsu-arrange--first-revset-commit
                                (format "(%s) & immutable()" revset))))
          (user-error "Cannot rewrite immutable revision %s" immutable))))))

(defun majutsu-arrange--reload-session (&optional session)
  "Reload SESSION's revset into the current arrange buffer."
  (let* ((old (or session (majutsu-arrange--session)))
         (revset (majutsu-arrange-session-revset old))
         (root (majutsu-arrange-session-root old)))
    (setq majutsu-arrange--session
          (majutsu-arrange-build-session revset root))))

(defun majutsu-arrange--start-command-sequence (commands buffer)
  "Start replaying COMMANDS for arrange BUFFER."
  (let ((root (buffer-local-value 'default-directory buffer)))
    (cl-labels
        ((finish-success
           ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (condition-case err
                     (majutsu-arrange--reload-session)
                   (error
                    (message "Arrange applied, but reload failed: %s"
                             (error-message-string err))
                    (setf (majutsu-arrange-session-dirty-p
                           (majutsu-arrange--session))
                          nil)))
                 (setf (majutsu-arrange-session-applying-p
                        (majutsu-arrange--session))
                       nil)
                 (set-buffer-modified-p nil)
                 (majutsu-arrange--rerender)))
             (majutsu-refresh))
           (message "Arrange plan applied"))
         (finish-failure
           (done exit-code)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setf (majutsu-arrange-session-applying-p
                      (majutsu-arrange--session))
                     nil)
               (majutsu-arrange--rerender)))
           (message "Arrange apply failed after %d command(s), exit code %s"
                    done exit-code))
         (run-next
           (remaining done)
           (if (null remaining)
               (finish-success)
             (if (not (buffer-live-p buffer))
                 (message "Arrange buffer was killed; stopping command sequence")
               (with-current-buffer buffer
                 (let* ((default-directory root)
                        (command (car remaining))
                        (process
                         (majutsu-start-jj
                          command nil
                          (lambda (_process exit-code)
                            (if (zerop exit-code)
                                (run-next (cdr remaining) (1+ done))
                              (finish-failure done exit-code))))))
                   (process-put process 'inhibit-refresh t)))))))
      (run-next commands 0))))

(defun majutsu-arrange-apply ()
  "Apply the current arrange plan using the CLI replay backend."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (commands (majutsu-arrange--compile-commands session)))
    (majutsu-arrange--preflight-apply session commands)
    (when (y-or-n-p (format "Apply arrange plan with %d jj command%s? "
                            (length commands)
                            (if (= (length commands) 1) "" "s")))
      (setf (majutsu-arrange-session-applying-p session) t)
      (majutsu-arrange--rerender)
      (majutsu-arrange--start-command-sequence commands (current-buffer)))))

;;; Rendering

(defun majutsu-arrange--display-order (session)
  "Return all arrange node ids in display order for SESSION."
  (append (majutsu-arrange-session-external-child-ids session)
          (majutsu-arrange-session-current-order session)
          (majutsu-arrange-session-external-parent-ids session)))

(defun majutsu-arrange--node-face (node selected)
  "Return face list for NODE, optionally SELECTED."
  (delq nil
        (list (and selected 'majutsu-arrange-selected-face)
              (pcase (majutsu-arrange-node-role node)
                ('external-parent 'majutsu-arrange-context-face)
                ('external-child 'majutsu-arrange-context-face)
                (_ nil))
              (pcase (majutsu-arrange-node-action node)
                ('abandon 'majutsu-arrange-abandon-face)
                ('keep 'majutsu-arrange-keep-face)
                (_ nil)))))

(defun majutsu-arrange--node-action-label (node)
  "Return display action label for NODE."
  (pcase (majutsu-arrange-node-role node)
    ('target (symbol-name (majutsu-arrange-node-action node)))
    ('external-parent "context")
    ('external-child "context")
    (_ "context")))

(defun majutsu-arrange--node-glyph (node)
  "Return graph/action glyph for NODE."
  (pcase (majutsu-arrange-node-role node)
    ('target (if (eq (majutsu-arrange-node-action node) 'abandon) "×" "○"))
    (_ "·")))

(defun majutsu-arrange--node-line (session id)
  "Return propertized display line for node ID in SESSION."
  (let* ((node (majutsu-arrange--node session id))
         (selected (equal id (majutsu-arrange-session-selected-id session)))
         (subject (member id (majutsu-arrange--current-subject-ids session)))
         (marker (if selected "▶" " "))
         (glyph (majutsu-arrange--node-glyph node))
         (action (majutsu-arrange--node-action-label node))
         (short (or (majutsu-arrange-node-short-change-id node)
                    (majutsu-arrange-node-short-commit-id node)
                    ""))
         (description (or (majutsu-arrange-node-description node) ""))
         (line (format "%s %s %-8s %-10s %s"
                       marker
                       (propertize glyph 'face 'majutsu-arrange-graph-face)
                       action
                       short
                       description)))
    (add-text-properties 0 (length line)
                         (list 'face (delq nil
                                           (cons (and subject
                                                      'majutsu-arrange-subject-face)
                                                 (majutsu-arrange--node-face
                                                  node selected)))
                               'majutsu-arrange-id id)
                         line)
    line))

(defun majutsu-arrange--insert-node (session id)
  "Insert node ID from SESSION as a flat section."
  (when-let* ((node (majutsu-arrange--node session id)))
    (let ((line (majutsu-arrange--node-line session id)))
      (magit-insert-section (jj-arrange-node id t)
        (magit-insert-heading line)))))

(defun majutsu-arrange--goto-id (id)
  "Move point to the line carrying arrange node ID."
  (when id
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (if (equal (get-text-property (point) 'majutsu-arrange-id) id)
            (setq found t)
          (forward-line 1)))
      found)))

(defun majutsu-arrange-refresh-buffer ()
  "Render the current arrange buffer from its session."
  (let* ((session (majutsu-arrange--session))
         (dirty (majutsu-arrange-session-dirty-p session))
         (applying (majutsu-arrange-session-applying-p session))
         (status (cond (applying "applying")
                       (dirty "dirty")
                       (t "clean"))))
    (magit-insert-section (arrange)
      (magit-insert-heading
        (format "Arrange: %s  [%s]" (majutsu-arrange-session-revset session) status))
      (insert (format "Subject: %s%s\n"
                      (or (majutsu-arrange-session-subject-scope session) 'subdag)
                      (if (majutsu-arrange-session-marked-ids session)
                          (format "  marks:%d"
                                  (length (majutsu-arrange-session-marked-ids session)))
                        "")))
      (insert "Keys: j/k move  S scope  m mark  A/B/o move DAG  J/K swap  a abandon  p keep  c apply  q cancel\n\n")
      (dolist (id (majutsu-arrange--display-order session))
        (majutsu-arrange--insert-node session id))
      (insert "\n"))))

(defun majutsu-arrange--rerender ()
  "Rerender the current arrange buffer preserving arrange dirty state."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (selected (majutsu-arrange-session-selected-id session))
         (dirty (majutsu-arrange-session-dirty-p session))
         (inhibit-read-only t))
    (erase-buffer)
    (majutsu-arrange-refresh-buffer)
    (majutsu-arrange--goto-id selected)
    (when (bound-and-true-p magit-root-section)
      (magit-section-show magit-root-section))
    (magit-section-update-highlight)
    (set-buffer-modified-p dirty)))

;;; Buffer commands

(defun majutsu-arrange-show ()
  "Show a short message for the arrange node at point."
  (interactive)
  (let* ((session (majutsu-arrange--session))
         (id (or (magit-section-value-if 'jj-arrange-node)
                 (majutsu-arrange--current-id session)))
         (node (majutsu-arrange--node session id)))
    (if node
        (message "%s %s" (majutsu-arrange-node-short-change-id node)
                 (majutsu-arrange-node-description node))
      (user-error "No arrange node at point"))))

(defun majutsu-arrange-cancel ()
  "Cancel the current arrange session and bury the buffer."
  (interactive)
  (let ((session (majutsu-arrange--session)))
    (when (or (not (majutsu-arrange-session-dirty-p session))
              (y-or-n-p "Discard arrange plan? "))
      (setf (majutsu-arrange-session-dirty-p session) nil)
      (set-buffer-modified-p nil)
      (majutsu-mode-bury-buffer))))

(defun majutsu-arrange-reload ()
  "Reload the arrange session from the repository, discarding local edits."
  (interactive)
  (let ((session (majutsu-arrange--session)))
    (when (or (not (majutsu-arrange-session-dirty-p session))
              (y-or-n-p "Discard arrange plan and reload? "))
      (majutsu-arrange--reload-session session)
      (set-buffer-modified-p nil)
      (majutsu-arrange--rerender))))

(defun majutsu-arrange--kill-buffer-query ()
  "Ask before killing a dirty arrange buffer."
  (let ((session majutsu-arrange--session))
    (or (not (and session (majutsu-arrange-session-dirty-p session)))
        (y-or-n-p "Kill buffer and discard arrange plan? "))))

;;; Mode and entry point

(defvar-keymap majutsu-arrange-mode-map
  :doc "Keymap for `majutsu-arrange-mode'."
  :parent majutsu-mode-map
  "j" #'majutsu-arrange-next
  "k" #'majutsu-arrange-previous
  "C-n" #'majutsu-arrange-next
  "C-p" #'majutsu-arrange-previous
  "J" #'majutsu-arrange-swap-down
  "K" #'majutsu-arrange-swap-up
  "m" #'majutsu-arrange-toggle-mark
  "M" #'majutsu-arrange-clear-marks
  "S" #'majutsu-arrange-cycle-subject-scope
  "A" #'majutsu-arrange-insert-after
  "B" #'majutsu-arrange-insert-before
  "o" #'majutsu-arrange-move-onto
  "a" #'majutsu-arrange-mark-abandon
  "p" #'majutsu-arrange-mark-keep
  "c" #'majutsu-arrange-apply
  "C-c C-c" #'majutsu-arrange-apply
  "q" #'majutsu-arrange-cancel
  "C-c C-k" #'majutsu-arrange-cancel
  "g" #'majutsu-arrange--rerender
  "G" #'majutsu-arrange-reload
  "RET" #'majutsu-arrange-show)

(define-derived-mode majutsu-arrange-mode majutsu-mode "Majutsu Arrange"
  "Major mode for editing a jj arrange plan."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (add-hook 'kill-buffer-query-functions
            #'majutsu-arrange--kill-buffer-query nil t))

;;;###autoload
(defun majutsu-arrange (&rest revsets)
  "Open a Majutsu arrange buffer for REVSETS.

REVSETS may be omitted, one revset string, or multiple strings.
Multiple strings match `jj arrange [REVSETS]...' by arranging their union.
With a prefix argument, prompt using Majutsu's standard revset reader."
  (interactive
   (if current-prefix-arg
       (list (majutsu-read-revset "Arrange revsets"
                                  (majutsu-arrange--configured-default-revset)
                                  '("arrange")))
     nil))
  (let* ((root (majutsu--toplevel-safe))
         (default-directory root)
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-arrange-mode nil
      :buffer (format "*majutsu-arrange: %s*" repo)
      :directory root
      (majutsu-arrange--session (majutsu-arrange-build-session revsets root)))))

(provide 'majutsu-arrange)
;;; majutsu-arrange.el ends here
