;;; majutsu-workspace.el --- JJ workspace support for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides helpers and UI for `jj workspace` commands, inspired
;; by Magit's worktree support.
;;
;; Majutsu queries workspace data using `jj workspace list -T ...' and resolves
;; workspace root paths via `jj workspace root --name' (available since jj
;; v0.38.0).  No `.jj/' internals are inspected.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'majutsu-mode)
(require 'majutsu-process)
(require 'majutsu-template)

;;; Templates

(defconst majutsu-workspace--field-separator (string 30)
  "Separator inserted between template fields for parsing.
We use an ASCII record separator so parsing stays robust.")

(defconst majutsu-workspace--line-regexp
  (concat "^\\([@]\\)?"                          ; 1: current marker (@ or nil)
          (regexp-quote majutsu-workspace--field-separator)
          "\\([^[:cntrl:]]+\\)"                  ; 2: name
          (regexp-quote majutsu-workspace--field-separator)
          "\\([a-z0-9]+\\)"                      ; 3: change-id
          (regexp-quote majutsu-workspace--field-separator)
          "\\([0-9a-f]+\\)"                      ; 4: commit-id
          (regexp-quote majutsu-workspace--field-separator)
          "\\(.*\\)"                             ; 5: description
          "$")
  "Regexp to match a workspace list line.
Matches the current marker (optional), name, change-id, commit-id,
and description.")

(defconst majutsu-workspace--list-template
  (majutsu-tpl
   [[:join "\x1e"
           [:if [:target :current_working_copy] "@"]
           [:name]
           [:target :change_id :shortest 8]
           [:target :commit_id :shortest 8]
           [:if [:target :description]
               [:method [:target :description] :first_line]]]
    "\n"]
   'WorkspaceRef)
  "Template used to render `jj workspace list` output for parsing.")

(defconst majutsu-workspace--names-template
  (majutsu-tpl
   [:concat [:name] "\n"]
   'WorkspaceRef)
  "Template that renders one workspace name per line.")

(defconst majutsu-workspace--current-name-template
  (majutsu-tpl
   [:concat
    [:if [:target :current_working_copy]
        [:name]]
    "\n"]
   'WorkspaceRef)
  "Template that renders the current workspace name and blanks for others.")

;;; Parsing

(defun majutsu-workspace-parse-list-output (output)
  "Parse `jj workspace list -T ...` OUTPUT into a list of plists.

Each entry contains:
- :name       workspace name (string)
- :current    non-nil when it is the current workspace
- :change-id  short change id of the workspace's working-copy commit
- :commit-id  short commit id of the workspace's working-copy commit
- :desc       first-line description of the working-copy commit"
  (let ((entries nil))
    (dolist (line (split-string (or output "") "\n" t))
      (let* ((fields (split-string line (regexp-quote majutsu-workspace--field-separator) nil))
             (marker (nth 0 fields))
             (name (nth 1 fields))
             (change-id (nth 2 fields))
             (commit-id (nth 3 fields))
             (desc (nth 4 fields)))
        (when (and name (not (string-empty-p name)))
          (push (list :name name
                      :current (equal marker "@")
                      :change-id change-id
                      :commit-id commit-id
                      :desc (or desc ""))
                entries))))
    (nreverse entries)))

;;; Query helpers

(defun majutsu-workspace-list-entries (&optional directory)
  "Return workspace entries for DIRECTORY (defaults to current repo root).
Entries are parsed from `jj workspace list -T ...`."
  (let* ((default-directory (or directory default-directory))
         (output (with-temp-buffer
                   (majutsu-jj-insert "workspace" "list" "-T" majutsu-workspace--list-template)
                   (buffer-string))))
    (majutsu-workspace-parse-list-output output)))

(defun majutsu-workspace--names (&optional directory)
  "Return a list of workspace names for DIRECTORY."
  (let* ((default-directory (or directory default-directory))
         (lines (majutsu-jj-lines "workspace" "list" "-T" majutsu-workspace--names-template)))
    (delete-dups lines)))

(defun majutsu-workspace-current-name (&optional directory)
  "Return current workspace name for DIRECTORY, or nil if it can't be determined."
  (let* ((default-directory (or directory default-directory))
         (lines (majutsu-jj-lines "workspace" "list" "-T" majutsu-workspace--current-name-template)))
    (car lines)))

;;; Interactive helpers

(defun majutsu-workspace--read-name (&optional prompt root default)
  "Read a workspace name.

Prefer the workspace section at point, otherwise use completion over
the workspaces for ROOT."
  (or (magit-section-value-if 'jj-workspace)
      (let* ((root (or root default-directory))
             (default (or default (majutsu-workspace-current-name root) ""))
             (prompt (or prompt "Workspace")))
        (majutsu-completing-read prompt (majutsu-workspace--names root) nil t nil nil default))))

(defun majutsu-workspace--read-root (name &optional root)
  "Return the workspace root directory for NAME.

Uses `jj workspace root --name' to resolve the path.  If that fails
\(e.g. workspace created before jj v0.38.0), tries a sibling directory
of ROOT whose name matches NAME.  Falls back to prompting the user."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (dir (or (majutsu-workspace--root-for-name name)
                  (majutsu-workspace--sibling-root name root)
                  (read-directory-name (format "Workspace root for %s: " name)
                                       (file-name-directory (directory-file-name root))
                                       nil t))))
    (file-name-as-directory (expand-file-name dir))))

;;; Workspace root discovery

(defun majutsu-workspace--root-for-name (name)
  "Return the workspace root directory for NAME, or nil.

This calls `jj workspace root --name NAME' (available since jj v0.38.0)
and returns a directory name with a trailing slash."
  (let ((line (car (majutsu-jj-lines "workspace" "root" "--name" name))))
    (when (and line (not (string-empty-p line)))
      (file-name-as-directory (expand-file-name line)))))

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

(defun majutsu-workspace--format-entry (entry name-width)
  "Format workspace ENTRY for insertion, padding name to NAME-WIDTH."
  (let* ((name (plist-get entry :name))
         (current (plist-get entry :current))
         (change-id (plist-get entry :change-id))
         (commit-id (plist-get entry :commit-id))
         (desc (plist-get entry :desc))
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
              (concat " " desc)))))

(defun majutsu-workspace--insert-entries (entries &optional _show-single)
  "Insert workspace ENTRIES as magit sections.
_SHOW-SINGLE is ignored; filtering is handled by the caller."
  (when entries
    (let* ((name-width (apply #'max 0 (mapcar (lambda (e)
                                                (string-width (plist-get e :name)))
                                              entries))))
      (dolist (entry entries)
        (let ((name (plist-get entry :name)))
          (magit-insert-section (jj-workspace name t)
            (magit-insert-heading
              (majutsu-workspace--format-entry entry name-width))))))))

(defun majutsu-workspace--wash-entry (name-width)
  "Wash the workspace list entry at point.
NAME-WIDTH is used to align columns.
This function follows the Magit wash pattern: uses `looking-at' to match
the current line, deletes it, and inserts a formatted section."
  (when (looking-at majutsu-workspace--line-regexp)
    (let* ((marker (match-string 1))
           (name (match-string 2))
           (change-id (match-string 3))
           (commit-id (match-string 4))
           (desc (match-string 5))
           (entry (list :name name
                        :current (equal marker "@")
                        :change-id change-id
                        :commit-id commit-id
                        :desc (or desc ""))))
      (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
      (magit-insert-section (jj-workspace name t)
        (magit-insert-heading
          (majutsu-workspace--format-entry entry name-width)))
      t)))

(defun majutsu-workspace--wash-list (show-single _args)
  "Wash `jj workspace list' output into workspace sections.
SHOW-SINGLE matches the behavior of `majutsu-insert-workspaces'.

This follows the Magit wash pattern:
1. First pass: scan buffer to count entries and compute column widths
2. Second pass: use `magit-wash-sequence' with `majutsu-workspace--wash-entry'
   to transform each line into a magit section."
  (let* ((entries nil)
         (max-name-width 0))
    ;; First pass: collect entries and compute max name width
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at majutsu-workspace--line-regexp)
          (let ((name (match-string 2)))
            (when name
              (push name entries)
              (setq max-name-width (max max-name-width (string-width name))))))
        (forward-line 1)))
    ;; Determine visibility
    (let ((visible (and entries (or show-single (length> entries 1)))))
      (if (not visible)
          (progn
            (delete-region (point-min) (point-max))
            (magit-cancel-section))
        ;; Second pass: wash sequence
        (goto-char (point-min))
        (magit-insert-heading (if (length> entries 1) "Workspaces" "Workspace"))
        (magit-wash-sequence (lambda ()
                               (majutsu-workspace--wash-entry max-name-width)))
        (insert "\n")))))

;;;###autoload
(defun majutsu-insert-workspaces ()
  "Insert a Workspaces section.
When there is only one workspace, nothing is inserted unless called
from `majutsu-workspace-mode'."
  (let ((show-single (eq major-mode 'majutsu-workspace-mode)))
    (magit-insert-section (workspaces)
      (majutsu-jj-wash (lambda (args)
                         (majutsu-workspace--wash-list show-single args))
          nil
        "workspace" "list" "-T" majutsu-workspace--list-template))))

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
          (new-name (read-string (format "Rename workspace (%s) to: " workspace)
                                 nil nil workspace)))
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
(defun majutsu-workspace-forget (names)
  "Forget workspaces NAMES.

This stops tracking the workspaces' working-copy commits in the repo. The
workspace directories are not touched on disk."
  (interactive
   (let* ((names (majutsu-workspace--names)))
     (list (majutsu-completing-read-multiple "Forget workspace(s)" names nil t))))
  (when names
    (unless (majutsu-confirm 'workspace-forget
                             (format "Forget workspace(s) %s? "
                                     (string-join names ", ")))
      (user-error "Forget canceled"))
    (if (zerop (apply #'majutsu-run-jj (append '("workspace" "forget") names)))
        (progn
          (message "Workspace(s) forgotten"))
      (message "Workspace forget failed"))))

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
          (name (string-trim (majutsu-read-string "Workspace name (empty = default)" nil nil "")))
          (revision (string-trim (majutsu-read-string "Parent revset (-r, empty = default)" nil nil "")))
          (sparse (majutsu-completing-read "Sparse patterns"
                                           '("copy" "full" "empty") nil t nil nil "copy")))
     (list destination
           (unless (string-empty-p name) name)
           (unless (string-empty-p revision) revision)
           (unless (equal sparse "copy") sparse))))
  (let* ((dest (expand-file-name destination))
         (args (append (list "workspace" "add" dest)
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

(transient-define-prefix majutsu-workspace ()
  "Internal transient for jj workspace operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  ["Workspace"
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
