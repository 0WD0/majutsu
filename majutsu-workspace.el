;;; majutsu-workspace.el --- JJ workspace support for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides helpers and UI for `jj workspace` commands, inspired
;; by Magit's worktree support.
;;
;; NOTE: Jujutsu's `workspace list` template context is `WorkspaceRef`.  The
;; documented methods are `.name()` and `.target()`, so Majutsu queries workspace
;; data using `jj workspace list -T ...` and does not inspect `.jj/` internals.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'majutsu-mode)
(require 'majutsu-process)
(require 'majutsu-template)

(declare-function majutsu-log "majutsu-log" ())
(declare-function majutsu-log-refresh "majutsu-log" ())

;;; Templates

(defconst majutsu-workspace--field-separator "\x1e"
  "Separator inserted between template fields for parsing.
We use an ASCII record separator so parsing stays robust.")

(defconst majutsu-workspace--list-template
  ;; NOTE: Don't use `separate()` here. We need stable field positions even
  ;; when some values are empty (e.g. the non-current marker).
  (tpl-compile
   [:join "\x1e"
          [:if [:target :current_working_copy] "@"]
          [:name]
          [:target :change_id :shortest 8]
          [:target :commit_id :shortest 8]
          [:if [:target :description]
              [:method [:target :description] :first_line]]
          "\n"]
   'WorkspaceRef)
  "Template used to render `jj workspace list` output for parsing.")

(defconst majutsu-workspace--names-template
  (tpl-compile
   [:concat [:name] "\n"]
   'WorkspaceRef)
  "Template that renders one workspace name per line.")

(defconst majutsu-workspace--current-name-template
  (tpl-compile
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
  (let* ((default-directory (or directory (majutsu--root)))
         (output (majutsu-run-jj "workspace" "list" "-T" majutsu-workspace--list-template)))
    (majutsu-workspace-parse-list-output output)))

(defun majutsu-workspace--names (&optional directory)
  "Return a list of workspace names for DIRECTORY."
  (let* ((default-directory (or directory (majutsu--root)))
         (output (majutsu-run-jj "workspace" "list" "-T" majutsu-workspace--names-template)))
    (delete-dups (split-string output "\n" t))))

(defun majutsu-workspace-current-name (&optional directory)
  "Return current workspace name for DIRECTORY, or nil if it can't be determined."
  (let* ((default-directory (or directory (majutsu--root)))
         (output (majutsu-run-jj "workspace" "list" "-T" majutsu-workspace--current-name-template)))
    (car (split-string output "\n" t))))

;;; Workspace root discovery

(defcustom majutsu-workspace-search-directories nil
  "Directories to search when locating workspace roots.

Majutsu cannot derive workspace root paths from `jj workspace list` (its
template context is `WorkspaceRef`, which only exposes name + target).
When visiting a workspace by name, Majutsu can try to find a matching
directory by checking subdirectories of each entry in this list.

If nil, fall back to searching the parent directory of the current workspace."
  :type '(repeat directory)
  :group 'majutsu)

(defun majutsu-workspace--candidate-search-dirs (root)
  "Return directories to search for workspace roots, based on ROOT."
  (or majutsu-workspace-search-directories
      (list (file-name-directory (directory-file-name root)))))

(defun majutsu-workspace--locate-root (workspace-name &optional root)
  "Try to locate WORKSPACE-NAME on disk and return its root directory, or nil.

This uses `jj` itself to verify candidates (no `.jj/` inspection)."
  (let* ((root (file-name-as-directory (or root (majutsu--root))))
         (search-dirs (majutsu-workspace--candidate-search-dirs root)))
    (or
     ;; Current workspace is trivial.
     (and (equal workspace-name (majutsu-workspace-current-name root))
          root)
     (seq-some
      (lambda (dir)
        (let* ((candidate (file-name-as-directory (expand-file-name workspace-name dir))))
          (when (file-directory-p candidate)
            (when (and (majutsu-toplevel candidate)
                       (equal (majutsu-workspace-current-name candidate) workspace-name))
              candidate))))
      search-dirs))))

;;; UI: Workspaces section

(defun majutsu-workspace--format-entry (entry name-width)
  "Format workspace ENTRY for insertion, padding name to NAME-WIDTH."
  (let* ((name (plist-get entry :name))
         (current (plist-get entry :current))
         (change-id (plist-get entry :change-id))
         (commit-id (plist-get entry :commit-id))
         (desc (plist-get entry :desc))
         (name-face (if current 'magit-branch-current 'magit-branch-local))
         (name-str (propertize name 'face name-face))
         (pad (make-string (max 0 (- name-width (string-width name))) ?\s))
         (marker (if current "@ " "  ")))
    (concat marker
            name-str pad
            " "
            (propertize change-id 'face 'magit-hash)
            " "
            (propertize commit-id 'face 'magit-hash)
            (when (and desc (not (string-empty-p desc)))
              (concat " " desc)))))

(defun majutsu-workspace--insert-entries (entries &optional show-single)
  "Insert workspace ENTRIES as magit sections.
If SHOW-SINGLE is nil, insert nothing when there is only one workspace."
  (when (and entries (or show-single (length> entries 1)))
    (let* ((name-width (apply #'max 0 (mapcar (lambda (e)
                                                (string-width (plist-get e :name)))
                                              entries))))
      (magit-insert-section (workspaces)
        (magit-insert-heading (if (length> entries 1) "Workspaces" "Workspace"))
        (dolist (entry entries)
          (let ((name (plist-get entry :name)))
            (magit-insert-section (jj-workspace name t)
              (magit-insert-heading
                (majutsu-workspace--format-entry entry name-width))
              (insert "\n"))))
        (insert "\n")))))

;;;###autoload
(defun majutsu-log-insert-workspaces ()
  "Insert a Workspaces section in the current log buffer."
  (majutsu-workspace--insert-entries (majutsu-workspace-list-entries) nil))

;;; Actions

;;;###autoload
(defun majutsu-workspace-visit (&optional directory)
  "Visit workspace at point.

If called with DIRECTORY, visit that directory. Otherwise, try to locate the
workspace root automatically; if not found, prompt for it."
  (interactive)
  (let* ((root (and (null directory) (majutsu--root)))
         (name (and (null directory)
                    (or (magit-section-value-if 'jj-workspace)
                        (completing-read "Workspace: " (majutsu-workspace--names root) nil t))))
         (dir (or directory
                  (majutsu-workspace--locate-root name root)
                  (read-directory-name (format "Workspace root for %s: " name)
                                       (file-name-directory (directory-file-name root))
                                       nil t))))
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (when name
      (let ((actual (and (majutsu-toplevel dir)
                         (majutsu-workspace-current-name dir))))
        (when (and actual (not (equal actual name)))
          (message "Warning: selected directory is workspace '%s' (expected '%s')" actual name))))
    (let ((default-directory dir))
      (if (fboundp 'majutsu-log)
          (majutsu-log)
        (dired dir)))))

;;; Commands

;;;###autoload
(defun majutsu-workspace-list ()
  "Show workspaces in a dedicated buffer."
  (interactive)
  (let* ((root (majutsu--root))
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
  (if (zerop (majutsu-call-jj "workspace" "update-stale"))
      (progn
        (message "Workspace updated")
        (when (fboundp 'majutsu-log-refresh)
          (majutsu-log-refresh)))
    (message "Workspace update failed")))

;;;###autoload
(defun majutsu-workspace-rename (new-name)
  "Rename the current workspace to NEW-NAME."
  (interactive
   (let* ((current (or (majutsu-workspace-current-name) ""))
          (prompt (if (string-empty-p current)
                      "Rename workspace to: "
                    (format "Rename workspace (%s) to: " current))))
     (list (read-string prompt nil nil current))))
  (when (and new-name (not (string-empty-p new-name)))
    (if (zerop (majutsu-call-jj "workspace" "rename" new-name))
        (progn
          (message "Workspace renamed")
          (when (fboundp 'majutsu-log-refresh)
            (majutsu-log-refresh))
          (when (derived-mode-p 'majutsu-workspace-mode)
            (majutsu-refresh)))
      (message "Workspace rename failed"))))

;;;###autoload
(defun majutsu-workspace-forget (names)
  "Forget workspaces NAMES.

This stops tracking the workspaces' working-copy commits in the repo. The
workspace directories are not touched on disk."
  (interactive
   (let* ((names (majutsu-workspace--names))
          (crm-separator (or (bound-and-true-p crm-separator) ", *")))
     (defvar crm-separator)
     (list (completing-read-multiple "Forget workspace(s): " names nil t))))
  (when names
    (when (and majutsu-confirm-critical-actions
               (not (yes-or-no-p (format "Forget workspace(s) %s? "
                                         (string-join names ", ")))))
      (user-error "Forget canceled"))
    (if (zerop (apply #'majutsu-call-jj (append '("workspace" "forget") names)))
        (progn
          (message "Workspace(s) forgotten")
          (when (fboundp 'majutsu-log-refresh)
            (majutsu-log-refresh))
          (when (derived-mode-p 'majutsu-workspace-mode)
            (majutsu-refresh)))
      (message "Workspace forget failed"))))

;;;###autoload
(defun majutsu-workspace-add (destination &optional name revision sparse-patterns)
  "Add a workspace.

DESTINATION is where to create the new workspace.
Optional NAME, REVISION (revset), and SPARSE-PATTERNS correspond to
`jj workspace add` options."
  (interactive
   (let* ((root (majutsu--root))
          (parent (file-name-directory (directory-file-name root)))
          (destination (read-directory-name "Create workspace at: " parent nil nil))
          (name (string-trim (read-string "Workspace name (empty = default): ")))
          (revision (string-trim (read-string "Parent revset (-r, empty = default): ")))
          (sparse (completing-read "Sparse patterns (copy/full/empty): "
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
         (exit (apply #'majutsu-call-jj args)))
    (if (zerop exit)
        (progn
          (message "Workspace created in %s" dest)
          (when (fboundp 'majutsu-log-refresh)
            (majutsu-log-refresh))
          ;; Like Magit, visit the new workspace.
          (majutsu-workspace-visit dest))
      (message "Workspace creation failed"))))

;;; Transient

;;;###autoload
(defun majutsu-workspace-transient ()
  "Transient for jj workspace operations."
  (interactive)
  (majutsu-workspace-transient--internal))

(transient-define-prefix majutsu-workspace-transient--internal ()
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
    ("n" "Rename (current)" majutsu-workspace-rename)
    ("u" "Update stale (current)" majutsu-workspace-update-stale)]])

;;; Workspace list buffer

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
  (let ((default-directory (majutsu--root)))
    (magit-insert-section (workspace-list)
      (majutsu-workspace--insert-entries (majutsu-workspace-list-entries) t))))

;;; _
(provide 'majutsu-workspace)
;;; majutsu-workspace.el ends here
