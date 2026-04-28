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
  operations
  dirty-p
  applying-p)

(cl-defstruct (majutsu-arrange-plan
               (:constructor majutsu-arrange-plan-create))
  base-operation-id
  final-parents
  final-actions
  operations
  affected-ids)

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
  (let* ((fields (split-string (or line "")
                               (regexp-quote majutsu-arrange--field-separator)
                               nil))
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

(defun majutsu-arrange-build-session (revset &optional directory)
  "Build an arrange session for REVSET in DIRECTORY."
  (let* ((root (majutsu--toplevel-safe directory))
         (default-directory root)
         (revset (or revset (majutsu-arrange--configured-default-revset)))
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

;;; Plan and CLI backend

(defun majutsu-arrange--affected-ids (session)
  "Return arrange node ids affected by SESSION's plan."
  (let (ids)
    (dolist (operation (majutsu-arrange-session-operations session))
      (pcase operation
        (`(:swap ,parent ,child)
         (setq ids (majutsu-arrange--push-unique parent ids))
         (setq ids (majutsu-arrange--push-unique child ids)))))
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
     :affected-ids (majutsu-arrange--affected-ids session))))

(defun majutsu-arrange--node-change-id (session id)
  "Return SESSION node ID's change id."
  (or (and-let* ((node (majutsu-arrange--node session id)))
        (majutsu-arrange-node-change-id node))
      (user-error "Arrange node %s is unavailable" id)))

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
               commands))))
    (let (abandon)
      (dolist (id (majutsu-arrange-session-target-ids session))
        (when-let* ((node (majutsu-arrange--node session id)))
          (when (eq (majutsu-arrange-node-action node) 'abandon)
            (push (majutsu-arrange-node-change-id node) abandon))))
      (when abandon
        (push (cons "abandon" (nreverse abandon)) commands)))
    (nreverse commands)))

(defun majutsu-arrange--affected-change-ids (session)
  "Return affected change ids for SESSION."
  (let (change-ids)
    (dolist (id (majutsu-arrange--affected-ids session))
      (setq change-ids
            (majutsu-arrange--push-unique
             (majutsu-arrange--node-change-id session id)
             change-ids)))
    change-ids))

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
  (let ((change-ids (majutsu-arrange--affected-change-ids session)))
    (when-let* ((ambiguous (majutsu-arrange--ambiguous-change-ids change-ids)))
      (user-error "Cannot apply arrange plan with ambiguous change ids: %s"
                  (string-join ambiguous ", ")))
    (when change-ids
      (let ((revset (string-join change-ids " | ")))
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
                         (list 'face (majutsu-arrange--node-face node selected)
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
      (insert "Keys: j/k move  J/K swap  a abandon  p keep  c apply  q cancel  G reload\n\n")
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
(defun majutsu-arrange (&optional revset)
  "Open a Majutsu arrange buffer for REVSET."
  (interactive
   (list (and current-prefix-arg
              (majutsu-read-revset "Arrange revset"
                                    (majutsu-arrange--configured-default-revset)
                                    '("arrange")))))
  (let* ((root (majutsu--toplevel-safe))
         (default-directory root)
         (revset (or revset (majutsu-arrange--configured-default-revset)))
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-arrange-mode nil
      :buffer (format "*majutsu-arrange: %s*" repo)
      :directory root
      (majutsu-arrange--session (majutsu-arrange-build-session revset root)))))

(provide 'majutsu-arrange)
;;; majutsu-arrange.el ends here
