;;; majutsu-workspace.el --- JJ workspace support for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of workspace/worktree UI flow are adapted from:
;; - Magit `lisp/magit-worktree.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors

;;; Commentary:

;; This library provides helpers and UI for `jj workspace` commands, inspired
;; by Magit's worktree support.
;;
;; Majutsu queries workspace data using a JSON-field
;; `jj workspace list -T ...' template and prefers the parsed workspace root
;; from that output when available.  It falls back to `jj workspace root --name'
;; (available since jj v0.38.0) when a root still needs to be resolved.
;; The trash flow rejects `.jj/repo' owners and binds local directory identity
;; across confirmation and `jj workspace forget'.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(require 'majutsu-completion)
(require 'majutsu-mode)
(require 'majutsu-process)
(require 'majutsu-template)

;;; Structured workspace template plan

(defconst majutsu-workspace--field-separator (string 30)
  "Separator inserted between template fields for parsing.
Each field is JSON-encoded first, so the separator can never occur in a
field even when a workspace name, description, or root contains control
characters.")

(defconst majutsu-workspace--default-fields
  '(current name change-id commit-id desc root)
  "Default fields transported by `jj workspace list -T ...'.")

(defun majutsu-workspace--parse-current (value)
  "Parse current workspace marker VALUE."
  (equal value "@"))

(defun majutsu-workspace--parse-desc (value)
  "Parse description VALUE."
  (or value ""))

(defun majutsu-workspace--parse-root (value)
  "Parse workspace root VALUE.
Return a normalized directory path, or nil if jj rendered an error string."
  (when (and value
             (not (string-empty-p value))
             (not (string-prefix-p "<Error:" value)))
    (majutsu-jj-expand-directory-from-jj value default-directory)))

(defconst majutsu-workspace--field-specs
  '((current
     :template [:if [:target :current_working_copy] "@" ""]
     :parse majutsu-workspace--parse-current)
    (name
     :template [:name]
     :parse identity)
    (change-id
     :template [:stringify [:target :change_id :shortest 8]]
     :parse identity)
    (commit-id
     :template [:stringify [:target :commit_id :shortest 8]]
     :parse identity)
    (desc
     :template [:if [:target :description]
                   [:method [:target :description] :first_line]
                   ""]
     :parse majutsu-workspace--parse-desc)
    (root
     :template [:root]
     :parse majutsu-workspace--parse-root))
  "Field metadata for structured workspace list parsing.")

(defun majutsu-workspace--field-key (field)
  "Return plist key symbol for FIELD."
  (intern (concat ":" (symbol-name field))))

(defun majutsu-workspace--field-spec (field)
  "Return normalized spec plist for FIELD."
  (or (cdr (assq field majutsu-workspace--field-specs))
      (user-error "Unknown workspace field %S" field)))

(defun majutsu-workspace--normalize-fields (&optional fields)
  "Return normalized workspace field list.
When FIELDS is nil, use `majutsu-workspace--default-fields'."
  (or fields majutsu-workspace--default-fields))

(defun majutsu-workspace--build-row-template-form (fields)
  "Return a row template form for FIELDS."
  (cons :join
        (cons majutsu-workspace--field-separator
              (mapcar (lambda (field)
                        (list :json
                              (plist-get (majutsu-workspace--field-spec field)
                                         :template)))
                      fields))))

(defun majutsu-workspace--compile-fields (&optional fields)
  "Compile workspace FIELDS into a template plan plist."
  (let* ((normalized (majutsu-workspace--normalize-fields fields))
         (specs (mapcar #'majutsu-workspace--field-spec normalized))
         (row-form (majutsu-workspace--build-row-template-form normalized))
         (template (majutsu-template-compile (vector row-form "\n") 'WorkspaceRef)))
    (list :fields normalized
          :specs specs
          :template template)))

(defconst majutsu-workspace--template-plan
  (majutsu-workspace--compile-fields majutsu-workspace--default-fields)
  "Compiled template plan for structured workspace list output.")

(defun majutsu-workspace--ensure-template-plan ()
  "Return the current workspace template plan."
  majutsu-workspace--template-plan)

;;; Parsing

(defun majutsu-workspace--split-fields (value)
  "Split VALUE by `majutsu-workspace--field-separator', preserving empties."
  (majutsu--split-fields value majutsu-workspace--field-separator))

(defun majutsu-workspace--decode-field (value)
  "Decode JSON string field VALUE, returning nil for invalid data."
  (condition-case nil
      (let ((decoded (json-parse-string (or value "")
                                        :null-object nil
                                        :false-object nil)))
        (and (stringp decoded) decoded))
    (error nil)))

(defun majutsu-workspace--parse-line (line &optional plan)
  "Parse a single workspace LINE using PLAN.
Return an entry plist, or nil if the line does not contain a workspace name."
  (let* ((plan (or plan (majutsu-workspace--ensure-template-plan)))
         (specs (plist-get plan :specs))
         (fields (majutsu-workspace--split-fields (or line "")))
         entry)
    (cl-loop for field in (plist-get plan :fields)
             for spec in specs
             for idx from 0
             do (setq entry
                      (plist-put entry
                                 (majutsu-workspace--field-key field)
                                 (funcall (plist-get spec :parse)
                                          (majutsu-workspace--decode-field
                                           (nth idx fields))))))
    (when-let* ((name (plist-get entry :name)))
      (unless (string-empty-p name)
        entry))))

(defun majutsu-workspace-parse-list-output (output &optional plan)
  "Parse `jj workspace list -T ...` OUTPUT into a list of entry plists.

Each entry contains:
- :name       workspace name (string)
- :current    non-nil when it is the current workspace
- :change-id  short change id of the workspace's working-copy commit
- :commit-id  short commit id of the workspace's working-copy commit
- :desc       first-line description of the working-copy commit
- :root       workspace root directory, or nil if unavailable"
  (let ((entries nil)
        (plan (or plan (majutsu-workspace--ensure-template-plan)))
        (output (ansi-color-filter-apply (or output ""))))
    (dolist (line (split-string output "\n" t))
      (when-let* ((entry (majutsu-workspace--parse-line line plan)))
        (push entry entries)))
    (nreverse entries)))

;;; Query helpers

(defun majutsu-workspace-list-entries (&optional directory)
  "Return workspace entries for DIRECTORY (defaults to current repo root).
Entries are parsed from `jj workspace list -T ...`."
  (let* ((default-directory (or directory default-directory))
         (plan (majutsu-workspace--ensure-template-plan))
         (output (with-temp-buffer
                   (majutsu-jj-insert "workspace" "list" "-T"
                                      (plist-get plan :template))
                   (buffer-string))))
    (majutsu-workspace-parse-list-output output plan)))

(defun majutsu-workspace--root-label (root &optional directory)
  "Return a plain display label for workspace ROOT.
DIRECTORY is the base directory used to prefer sibling-relative roots."
  (when root
    (let* ((base (file-name-as-directory (or directory default-directory)))
           (parent (file-name-directory (directory-file-name base)))
           (relative (and parent
                          (equal (file-remote-p root) (file-remote-p parent))
                          (ignore-errors (file-relative-name root parent)))))
      (cond
       ((and relative (not (string-prefix-p ".." relative)))
        (directory-file-name relative))
       (t
        (abbreviate-file-name (directory-file-name root)))))))

(defun majutsu-workspace--completion-suffix (entry &optional directory)
  "Return aligned completion suffix for workspace ENTRY.
DIRECTORY is the base directory used to label workspace roots."
  (majutsu-completion-annotation
   (majutsu-completion-column
    (if (plist-get entry :current) "current" "workspace")
    10 (if (plist-get entry :current) 'success 'majutsu-completion-key))
   (majutsu-completion-column
    (majutsu-workspace--root-label (plist-get entry :root) directory)
    28 'majutsu-completion-file-name)
   (majutsu-completion-column
    (plist-get entry :change-id)
    8 'majutsu-completion-number)
   (majutsu-completion-field
    (when-let* ((desc (plist-get entry :desc))
                ((not (string-empty-p desc))))
      desc)
    'majutsu-completion-documentation)))

(defun majutsu-workspace--completion-suffix-function (entries &optional directory)
  "Return candidate suffix function backed by workspace ENTRIES.
DIRECTORY is captured so suffix rendering stays stable in the minibuffer."
  (majutsu-completion-entry-suffix-function
   entries
   (lambda (entry)
     (majutsu-workspace--completion-suffix entry directory))))

(defun majutsu-workspace-candidate-data (&optional directory)
  "Return completion payload for workspace names in DIRECTORY."
  (let* ((default-directory (or directory default-directory))
         (entry-list (condition-case nil
                         (majutsu-workspace-list-entries default-directory)
                       (error nil)))
         (entries (make-hash-table :test #'equal))
         candidates)
    (dolist (entry entry-list)
      (when-let* ((name (plist-get entry :name)))
        (unless (gethash name entries)
          (push name candidates))
        (puthash name entry entries)))
    (list :category 'majutsu-workspace
          :candidates (nreverse candidates)
          :entry-list entry-list
          :entries entries
          :annotation-suffix-function
          (majutsu-workspace--completion-suffix-function
           entries default-directory))))

(defun majutsu-workspace--names (&optional directory)
  "Return a list of workspace names for DIRECTORY."
  (plist-get (majutsu-workspace-candidate-data directory) :candidates))

(defun majutsu-workspace-current-name (&optional directory)
  "Return current workspace name for DIRECTORY, or nil if it can't be determined."
  (when-let* ((entry (cl-find-if (lambda (ws)
                                   (plist-get ws :current))
                                 (majutsu-workspace-list-entries directory))))
    (plist-get entry :name)))

;;; Interactive helpers

(defun majutsu-workspace--section-entry ()
  "Return the current `jj-workspace' section entry plist, if any."
  (let ((value (magit-section-value-if 'jj-workspace)))
    (when (listp value)
      value)))

(defun majutsu-workspace--section-name ()
  "Return the current `jj-workspace' section name, if any."
  (when-let* ((value (magit-section-value-if 'jj-workspace)))
    (if (listp value)
        (plist-get value :name)
      value)))

(defvar majutsu-workspace-name-history nil
  "Minibuffer history for workspace-name input.")

(defun majutsu-workspace--read-name (&optional prompt root default)
  "Read a workspace name.

Prefer the workspace section at point, otherwise use completion over
the workspaces for ROOT."
  (or (majutsu-workspace--section-name)
      (let* ((root (or root default-directory))
             (payload (majutsu-workspace-candidate-data root))
             (current (cl-find-if (lambda (entry)
                                    (plist-get entry :current))
                                  (plist-get payload :entry-list)))
             (default (or default (plist-get current :name) ""))
             (prompt (or prompt "Workspace")))
        (majutsu-completing-read-payload prompt payload
                                         nil t nil 'majutsu-workspace-name-history
                                         default 'majutsu-workspace nil root))))

(defun majutsu-workspace--read-root (name &optional root)
  "Return the workspace root directory for NAME.

Prefer a structured `:root' value from the current workspace section when
available. Otherwise use `jj workspace root --name' to resolve the path. If
that fails \(e.g. workspace created before jj v0.38.0), try a sibling
directory of ROOT whose name matches NAME. Falls back to prompting the user."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (entry (majutsu-workspace--section-entry))
         (dir (or (and entry
                       (equal (plist-get entry :name) name)
                       (plist-get entry :root))
                  (majutsu-workspace--root-for-name name)
                  (majutsu-workspace--sibling-root name root)
                  (read-directory-name (format "Workspace root for %s: " name)
                                       (file-name-directory (directory-file-name root))
                                       nil t))))
    (file-name-as-directory dir)))

;;; Workspace root discovery

(defun majutsu-workspace--entry-root (name entries)
  "Return NAME's structured root from ENTRIES, or nil."
  (when-let* ((entry (cl-find name entries
                              :key (lambda (entry)
                                     (plist-get entry :name))
                              :test #'equal)))
    (plist-get entry :root)))

(defun majutsu-workspace--roots-for-names (names &optional root)
  "Return an alist mapping workspace NAMES to known root directories.
ROOT is the workspace root used to query sibling workspaces.  Missing
structured roots are resolved only with non-interactive helpers.  If a
root remains unavailable, the matching alist value is nil."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (entries (condition-case nil
                      (majutsu-workspace-list-entries root)
                    (error nil))))
    (mapcar (lambda (name)
              (cons name
                    (or (majutsu-workspace--entry-root name entries)
                        (let ((default-directory root))
                          (or (condition-case nil
                                  (majutsu-workspace--root-for-name name)
                                (error nil))
                              (majutsu-workspace--sibling-root name root))))))
            names)))

(defun majutsu-workspace--repo-store-p (root)
  "Return non-nil if ROOT owns the shared jj repo store."
  (and root
       (file-directory-p (expand-file-name ".jj/repo" root))))

(defun majutsu-workspace--directory-identity (attributes)
  "Return the stable local file identity from ATTRIBUTES."
  (and attributes (file-attribute-file-identifier attributes)))

(defun majutsu-workspace--prepare-trash-targets (root-alist)
  "Validate ROOT-ALIST and bind each existing root to its file identity.

ROOT-ALIST maps workspace names to directory paths.  The returned target
plists contain `:name', `:root', `:existed', and `:identity'.  Nil roots and
already-missing paths are allowed because `jj workspace forget' is also useful
after a directory was removed manually.

Remote roots are rejected before any file operation.  Existing targets must
be real directories, must not own the shared `.jj/repo' store, and must retain
the same device/inode identity throughout validation."
  (mapcar
   (lambda (entry)
     (pcase-let ((`(,name . ,root) entry))
       (cond
        ((or (null root) (string-empty-p root))
         (list :name name :root root :existed nil :identity nil))
        ((file-remote-p root)
         (user-error "Refusing to trash remote workspace %s at %s" name root))
        (t
         (let ((before (file-attributes root 'integer)))
           (if (null before)
               (list :name name :root root :existed nil :identity nil)
             (unless (eq (file-attribute-type before) t)
               (user-error "Refusing to trash workspace %s; %s is not a real directory"
                           name root))
             (when (majutsu-workspace--repo-store-p root)
               (user-error "Refusing to trash workspace %s; %s owns the shared .jj/repo store"
                           name root))
             (let* ((after (file-attributes root 'integer))
                    (before-id (majutsu-workspace--directory-identity before))
                    (after-id (majutsu-workspace--directory-identity after)))
               (unless (and before-id
                            (eq (file-attribute-type after) t)
                            (equal before-id after-id))
                 (user-error "Refusing to trash workspace %s; %s changed while it was validated"
                             name root))
               (list :name name :root root :existed t :identity before-id))))))))
   root-alist))

(defun majutsu-workspace--trash-target-failure (target reason)
  "Return a failure plist for TARGET with REASON."
  (list :name (plist-get target :name)
        :root (plist-get target :root)
        :reason reason))

(defun majutsu-workspace--quoted-root (root)
  "Return ROOT quoted with control characters escaped for messages."
  (let ((print-escape-control-characters t)
        (print-escape-newlines t))
    (prin1-to-string root)))

(defun majutsu-workspace--trash-roots (targets)
  "Move identity-bound TARGETS to the system trash.

TARGETS must come from `majutsu-workspace--prepare-trash-targets'.  Every
target is checked again after `jj workspace forget'.  A path that appeared,
disappeared, changed device/inode, or became a shared repo-store owner is not
deleted.  Continue after individual failures and return all failure plists."
  (let (failures)
    (dolist (target targets)
      (condition-case err
          (let* ((root (plist-get target :root))
                 (existed (plist-get target :existed))
                 (identity (plist-get target :identity))
                 (attributes (and root
                                  (not (string-empty-p root))
                                  (file-attributes root 'integer))))
            (cond
             ((and (not existed) (null attributes)))
             ((not existed)
              (push (majutsu-workspace--trash-target-failure
                     target "a filesystem object appeared there after confirmation")
                    failures))
             ((null attributes)
              (push (majutsu-workspace--trash-target-failure
                     target "the confirmed directory disappeared")
                    failures))
             ((or (not (eq (file-attribute-type attributes) t))
                  (not (equal identity
                              (majutsu-workspace--directory-identity attributes))))
              (push (majutsu-workspace--trash-target-failure
                     target "the directory identity changed after confirmation")
                    failures))
             ((majutsu-workspace--repo-store-p root)
              (push (majutsu-workspace--trash-target-failure
                     target "the directory now owns the shared .jj/repo store")
                    failures))
             (t
              (let ((delete-by-moving-to-trash t))
                (delete-directory root t t)))))
        (error
         (push (majutsu-workspace--trash-target-failure
                target (error-message-string err))
               failures))))
    (nreverse failures)))

(defun majutsu-workspace--signal-trash-failures (failures)
  "Signal one user error that reports every entry in FAILURES."
  (when failures
    (user-error
     "Workspace forget succeeded, but these directories were not moved to trash:\n%s\nMove them manually"
     (mapconcat
      (lambda (failure)
        (format "- %s (%s): %s"
                (plist-get failure :name)
                (majutsu-workspace--quoted-root
                 (plist-get failure :root))
                (plist-get failure :reason)))
      failures "\n"))))

(defun majutsu-workspace--trash-target-label (target)
  "Return a confirmation label for identity-bound TARGET."
  (let ((name (plist-get target :name))
        (root (plist-get target :root)))
    (if (and root (not (string-empty-p root)))
        (format "%s at %s" name (majutsu-workspace--quoted-root root))
      (format "%s (root unavailable)" name))))

(defun majutsu-workspace--transient-trash-p ()
  "Return non-nil when the active workspace transient has `--trash' enabled."
  (and (eq transient-current-command 'majutsu-workspace)
       (condition-case nil
           (member "--trash" (transient-args 'majutsu-workspace))
         (error nil))))

(defun majutsu-workspace--root-for-name (name)
  "Return the workspace root directory for NAME, or nil.

This calls `jj workspace root --name NAME' (available since jj v0.38.0)
and returns a directory name with a trailing slash.  Only the one newline
written by jj is removed; embedded or trailing newlines belonging to the
actual path are preserved."
  (let ((output (or (majutsu-jj-buffer-string
                     "workspace" "root" "--name" name)
                    "")))
    (when (string-suffix-p "\n" output)
      (setq output (substring output 0 -1)))
    (when (not (string-empty-p output))
      (majutsu-jj-expand-directory-from-jj output default-directory))))

(defun majutsu-workspace--sibling-root (name root)
  "Return a sibling directory of ROOT named NAME if it is a matching workspace.

Checks whether a directory named NAME exists alongside ROOT and
is itself a jj workspace whose current workspace name equals NAME.
Returns the directory path or nil."
  (let* ((parent (file-name-directory (directory-file-name root)))
         (candidate (file-name-as-directory (expand-file-name name parent))))
    (when (and (file-directory-p candidate)
               (majutsu-toplevel candidate)
               (equal (majutsu-workspace-current-name candidate) name))
      candidate)))

;;; UI: Workspaces section

(defvar-keymap majutsu-workspace-section-map
  :doc "Keymap for `jj-workspace' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-workspace-visit)

(defun majutsu-workspace--display-root (root)
  "Return a display string for workspace ROOT.
ROOT should be a normalized directory path or nil."
  (if (not root)
      (propertize "-" 'font-lock-face 'shadow)
    (propertize (majutsu-workspace--root-label root)
                'font-lock-face 'shadow
                'help-echo root)))

(defun majutsu-workspace--format-entry (entry name-width)
  "Format workspace ENTRY for insertion, padding name to NAME-WIDTH."
  (let* ((name (plist-get entry :name))
         (current (plist-get entry :current))
         (change-id (plist-get entry :change-id))
         (commit-id (plist-get entry :commit-id))
         (desc (plist-get entry :desc))
         (root (plist-get entry :root))
         (name-face (if current 'magit-branch-current 'magit-branch-local))
         (name-str (propertize name 'font-lock-face name-face))
         (pad (make-string (max 0 (- name-width (string-width name))) ?\s))
         (marker (if current "@ " "  ")))
    (concat marker
            name-str pad
            " "
            (propertize change-id 'font-lock-face 'magit-hash)
            " "
            (propertize commit-id 'font-lock-face 'magit-hash)
            (when (and desc (not (string-empty-p desc)))
              (concat " " desc))
            " "
            (majutsu-workspace--display-root root))))

(defun majutsu-workspace--insert-entries (entries &optional _show-single)
  "Insert workspace ENTRIES as magit sections.
_SHOW-SINGLE is ignored; filtering is handled by the caller."
  (when entries
    (let* ((name-width (apply #'max 0 (mapcar (lambda (e)
                                                (string-width (plist-get e :name)))
                                              entries))))
      (dolist (entry entries)
        (magit-insert-section (jj-workspace entry t)
          (magit-insert-heading
            (majutsu-workspace--format-entry entry name-width)))))))

(defun majutsu-workspace--wash-list (show-single _args)
  "Wash `jj workspace list' output into workspace sections.
SHOW-SINGLE matches the behavior of `majutsu-insert-workspaces'."
  (let* ((entries (majutsu-workspace-parse-list-output
                   (buffer-substring-no-properties (point-min) (point-max))
                   (majutsu-workspace--ensure-template-plan)))
         (visible (and entries (or show-single (length> entries 1)))))
    (delete-region (point-min) (point-max))
    (if (not visible)
        (magit-cancel-section)
      (magit-insert-heading (if (length> entries 1) "Workspaces" "Workspace"))
      (majutsu-workspace--insert-entries entries)
      (insert "\n"))))

;;;###autoload
(defun majutsu-insert-workspaces ()
  "Insert a Workspaces section.
When there is only one workspace, nothing is inserted unless called
from `majutsu-workspace-mode'."
  (let ((show-single (eq major-mode 'majutsu-workspace-mode))
        (plan (majutsu-workspace--ensure-template-plan)))
    (magit-insert-section (workspaces)
      (majutsu-jj-wash (lambda (args)
                         (majutsu-workspace--wash-list show-single args))
          nil
        "workspace" "list" "-T" (plist-get plan :template)))))

;;; Actions

;;;###autoload
(defun majutsu-workspace-visit (&optional directory)
  "Visit workspace at point.

If called with DIRECTORY, visit that directory. Otherwise, try to locate the
workspace root automatically; if not found, prompt for it."
  (interactive)
  (let* ((root default-directory)
         (dir (if directory
                  (file-name-as-directory (expand-file-name directory))
                (majutsu-workspace--read-root (majutsu-workspace--read-name "Workspace: " root) root))))
    (setq default-directory dir)
    (setq majutsu--default-directory dir)
    (if (majutsu-refresh)
        (dired dir))))

;;; Commands

;;;###autoload
(defun majutsu-workspace-list ()
  "Show workspaces in a dedicated buffer."
  (interactive)
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-workspace-mode nil
      :buffer (format "*majutsu-workspaces: %s*" repo))))

;;;###autoload
(defun majutsu-workspace-root ()
  "Show and copy the current workspace root directory."
  (interactive)
  (let ((root (or (majutsu-toplevel) "")))
    (when (and root (not (string-empty-p root)))
      (kill-new (directory-file-name root))
      (message "%s (copied)" root))))

;;;###autoload
(defun majutsu-workspace-update-stale ()
  "Update the current workspace if it has become stale."
  (interactive)
  (if (zerop (majutsu-run-jj "workspace" "update-stale"))
      (progn
        (message "Workspace updated"))
    (message "Workspace update failed")))

;;;###autoload
(defun majutsu-workspace-rename (workspace new-name)
  "Rename WORKSPACE to NEW-NAME.

Note that Jujutsu renames the workspace associated with the current
working directory, so this command may prompt for the workspace root
directory."
  (interactive
   (let* ((root (majutsu--toplevel-safe))
          (workspace (majutsu-workspace--read-name "Rename workspace: " root))
          (new-name (and workspace
                         (majutsu-completing-read
                          (format "Rename workspace (%s) to" workspace)
                          (majutsu-workspace--names root)
                          nil nil nil 'majutsu-workspace-name-history
                          workspace 'majutsu-workspace))))
     (list workspace new-name)))
  (when (and workspace (not (string-empty-p workspace))
             new-name (not (string-empty-p new-name)))
    (let* ((root (majutsu--toplevel-safe))
           (dir (majutsu-workspace--read-root workspace root))
           (default-directory dir))
      (if (zerop (majutsu-run-jj "workspace" "rename" new-name))
          (progn
            (message "Workspace renamed"))
        (message "Workspace rename failed")))))

;;;###autoload
(defun majutsu-workspace-forget (names &optional trash)
  "Forget workspaces NAMES.

This stops tracking the workspaces' working-copy commits in the repo.
With TRASH, also move the corresponding workspace directories to the
system trash after `jj workspace forget' succeeds.  TRASH accepts local
workspace roots only."
  (interactive
   (let* ((names (majutsu-workspace--names)))
     (list (majutsu-completing-read-multiple
            "Forget workspace(s)" names nil t nil
            'majutsu-workspace-name-history nil 'majutsu-workspace)
           (majutsu-workspace--transient-trash-p))))
  (when names
    (let* ((action (if trash 'workspace-trash 'workspace-forget))
           (root-alist (when trash
                         (majutsu-workspace--roots-for-names
                          names (majutsu--toplevel-safe))))
           (trash-targets (when trash
                            (majutsu-workspace--prepare-trash-targets root-alist))))
      (unless (majutsu-confirm action
                               (format "%s workspace(s) %s? "
                                       (if trash "Forget and trash" "Forget")
                                       (if trash
                                           (mapconcat
                                            #'majutsu-workspace--trash-target-label
                                            trash-targets ", ")
                                         (string-join names ", "))))
        (user-error "Forget canceled"))
      (if (zerop (apply #'majutsu-run-jj (append '("workspace" "forget") names)))
          (progn
            (when trash
              (majutsu-workspace--signal-trash-failures
               (majutsu-workspace--trash-roots trash-targets)))
            (message (if trash
                         "Workspace(s) forgotten; available directories trashed"
                       "Workspace(s) forgotten")))
        (message "Workspace forget failed")))))

;;;###autoload
(defun majutsu-workspace-add (destination &optional name revision sparse-patterns)
  "Add a workspace.

DESTINATION is where to create the new workspace.
Optional NAME, REVISION (revset), and SPARSE-PATTERNS correspond to
  `jj workspace add` options."
  (interactive
   (let* ((root (majutsu--toplevel-safe))
          (parent (file-name-directory (directory-file-name root)))
          (destination (read-directory-name "Create workspace at: " parent nil nil))
          (name (string-trim
                 (or (majutsu-completing-read
                      "Workspace name (empty = default)"
                      (majutsu-workspace--names root)
                      nil nil nil 'majutsu-workspace-name-history
                      nil 'majutsu-workspace)
                     "")))
          (revision (or (majutsu-read-optional-revset
                         "Parent revset (-r, empty = default)"
                         nil nil 'majutsu-read-revset-history)
                        ""))
          (sparse (majutsu-completing-read "Sparse patterns"
                                           '("copy" "full" "empty") nil t nil nil "copy")))
     (list destination
           (unless (string-empty-p name) name)
           (unless (string-empty-p revision) revision)
           (unless (equal sparse "copy") sparse))))
  (let* ((dest (expand-file-name destination))
         (args (append (list "workspace" "add" (majutsu-convert-filename-for-jj dest))
                       (and name (list "--name" name))
                       (and revision (list "--revision" revision))
                       (and sparse-patterns (list "--sparse-patterns" sparse-patterns))))
         (exit (apply #'majutsu-run-jj args)))
    (if (zerop exit)
        (progn
          (message "Workspace created in %s" dest)
          ;; Like Magit, visit the new workspace.
          (majutsu-workspace-visit dest))
      (message "Workspace creation failed"))))

;;; Transient

;;;###autoload(autoload 'majutsu-workspace "majutsu-workspace" nil t)
(transient-define-prefix majutsu-workspace ()
  "Internal transient for jj workspace operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  ["Workspace"
   ["Options"
    ("-t" "Trash workspace directory after forget" "--trash")]
   ["View"
    ("l" "List" majutsu-workspace-list)
    ("v" "Visit" majutsu-workspace-visit)
    ("r" "Root (copy)" majutsu-workspace-root)]
   ["Manage"
    ("a" "Add" majutsu-workspace-add)
    ("f" "Forget" majutsu-workspace-forget)
    ("n" "Rename" majutsu-workspace-rename)
    ("u" "Update stale (current)" majutsu-workspace-update-stale)]])

;;; Workspace list buffer

(defcustom majutsu-workspace-sections-hook
  (list #'majutsu-insert-workspaces)
  "Hook run to insert sections in the workspace buffer."
  :type 'hook
  :group 'majutsu)

(defvar-keymap majutsu-workspace-mode-map
  :doc "Keymap for `majutsu-workspace-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-workspace-mode majutsu-mode "Majutsu Workspaces"
  "Major mode for viewing jj workspaces."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-workspace-refresh-buffer ()
  "Refresh the workspace list buffer."
  (majutsu--assert-mode 'majutsu-workspace-mode)
  (magit-insert-section (workspace-list)
    (run-hooks 'majutsu-workspace-sections-hook)))

;;; _
(provide 'majutsu-workspace)
;;; majutsu-workspace.el ends here
