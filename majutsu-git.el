;;; majutsu-git.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library wraps jj's git-compatible commands and exposes them
;; through Majutsu transients.

;;; Code:

(require 'majutsu)
(require 'majutsu-mode)
(require 'seq)
(require 'subr-x)

(declare-function majutsu-jj-string "majutsu-process" (&rest args))
(declare-function majutsu--toplevel-safe "majutsu-jj" (&optional directory))
(declare-function majutsu-with-toplevel "majutsu-jj" (&rest body))

;;; majutsu-git

(defun majutsu-git--call (&rest args)
  "Call `jj git ARGS' synchronously, for side-effects."
  (apply #'majutsu-call-jj (append '("git") args)))

(defun majutsu-git--run (&rest args)
  "Call `jj git ARGS' synchronously, and refresh."
  (apply #'majutsu-run-jj (append '("git") args)))

(defun majutsu-git--start (args &optional success-msg finish-callback)
  "Start `jj git ARGS' asynchronously, for side-effects."
  (majutsu-start-jj (append '("git") args) success-msg finish-callback))

(defun majutsu-git--remote-names (&optional directory)
  "Return a list of Git remote names for DIRECTORY.

This calls `jj git remote list` and parses the first word of each line."
  (let ((default-directory (or directory default-directory)))
    (condition-case _
        (let* ((lines (or (majutsu-jj-lines "git" "remote" "list") '()))
               (names (delq nil
                            (mapcar (lambda (line)
                                      (car (split-string line "[ :\t]+" t)))
                                    lines))))
          (delete-dups names))
      (error nil))))

(defun majutsu-git--read-remote (prompt)
  (let ((remotes (or (majutsu-git--remote-names (ignore-errors (majutsu--toplevel-safe)))
                     '("origin"))))
    (majutsu-completing-read prompt remotes nil t)))

(defun majutsu-git--expand-option-arg (arg prefix)
  "If ARG begins with PREFIX, expand the file name part."
  (if (and (stringp arg) (string-prefix-p prefix arg))
      (concat prefix (expand-file-name (substring arg (length prefix))))
    arg))

;;;###autoload
(transient-define-suffix majutsu-git-push (args)
  "Push to git remote with ARGS."
  (interactive (list (transient-args 'majutsu-git-push-transient)))
  (majutsu--message-with-log "Pushing to remote...")
  (majutsu-git--start (append '("push") args) "Pushed to remote"))


;;;###autoload (autoload 'majutsu-git-fetch "majutsu-git" nil t)
(defun majutsu-git-fetch (args)
  "Fetch from git remote with ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-fetch-transient)))
  (majutsu--message-with-log "Fetching from remote...")
  (majutsu-git--start (append '("fetch") args) "Fetched from remote"))

(defun majutsu-git-remote-list ()
  "List Git remotes in a dedicated buffer."
  (interactive)
  (majutsu-setup-buffer #'majutsu-git-remote-list-mode nil
    :buffer "*Majutsu Git Remotes*"))

(defun majutsu-git--remote-line-name (line)
  "Return the remote name parsed from LINE."
  (let* ((raw (string-trim (substring-no-properties line)))
         (token (car (split-string raw "[ \t]+" t))))
    token))

(defun majutsu-git--wash-remote-list (_args)
  "Wash `jj git remote list' output into remote sections."
  (let ((count 0))
    (magit-wash-sequence
     (lambda ()
       (let* ((line (buffer-substring (line-beginning-position)
                                      (line-end-position)))
              (trimmed (string-trim (substring-no-properties line)))
              (name (and (not (string-empty-p trimmed))
                         (majutsu-git--remote-line-name line))))
         (delete-region (line-beginning-position)
                        (min (point-max) (1+ (line-end-position))))
         (when name
           (setq count (1+ count))
           (magit-insert-section (jj-git-remote name t)
             (magit-insert-heading line)
             (insert "\n")))
         t)))
    (if (zerop count)
        (magit-cancel-section)
      (insert "\n"))))

(defun majutsu-git-remote-list-refresh-buffer ()
  "Refresh the git remote list buffer."
  (majutsu--assert-mode 'majutsu-git-remote-list-mode)
  (magit-insert-section (git-remote-list)
    (majutsu-jj-wash #'majutsu-git--wash-remote-list nil
      "git" "remote" "list")))

(defvar-keymap majutsu-git-remote-list-mode-map
  :doc "Keymap for `majutsu-git-remote-list-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-git-remote-list-mode majutsu-mode "Majutsu Git Remotes"
  "Major mode for viewing git remotes."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-git-remote-add (args)
  "Add a Git remote. Prompts for name and URL; respects ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-remote-transient)))
  (let* ((remote (read-string "Remote name: "))
         (url (read-string (format "URL for %s: " remote)))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (cmd-args (append '("remote" "add")
                           (when fetch-tags (list fetch-tags))
                           (list remote url)))
         (exit (apply #'majutsu-git--run cmd-args)))
    (when (zerop exit)
      (message "Added remote %s" remote))))

(defun majutsu-git-remote-remove ()
  "Remove a Git remote and forget its bookmarks."
  (interactive)
  (let ((remote (majutsu-git--read-remote "Remove remote: ")))
    (when (and remote (not (string-empty-p remote)))
      (let* ((cmd-args (list "remote" "remove" remote))
             (exit (apply #'majutsu-git--run cmd-args)))
        (when (zerop exit)
          (message "Removed remote %s" remote))))))

(defun majutsu-git-remote-rename ()
  "Rename a Git remote."
  (interactive)
  (let* ((old (majutsu-git--read-remote "Rename remote: "))
         (new (read-string (format "New name for %s: " old))))
    (when (and (not (string-empty-p old)) (not (string-empty-p new)))
      (let* ((cmd-args (list "remote" "rename" old new))
             (exit (apply #'majutsu-git--run cmd-args)))
        (when (zerop exit)
          (message "Renamed remote %s -> %s" old new))))))

(defun majutsu-git-remote-set-url ()
  "Set URL of a Git remote."
  (interactive)
  (let* ((remote (majutsu-git--read-remote "Set URL for remote: "))
         (url (read-string (format "New URL for %s: " remote))))
    (when (and (not (string-empty-p remote)) (not (string-empty-p url)))
      (let* ((cmd-args (list "remote" "set-url" remote url))
             (exit (apply #'majutsu-git--run cmd-args)))
        (when (zerop exit)
          (message "Set URL for %s" remote))))))

(defun majutsu-git-clone (args)
  "Clone a Git repo into a new jj repo.
Prompts for SOURCE and optional DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-clone-transient)))
  (let* ((source (read-string "Source (URL or path): "))
         (dest   (let ((d (read-directory-name "Destination (optional): " nil nil t)))
                   (when (and d (not (string-empty-p (expand-file-name d))))
                     ;; If user picks current dir, treat as empty and let jj default
                     (let ((dd (directory-file-name d)))
                       (if (string= dd (directory-file-name default-directory)) nil dd)))))
         (cmd-args (append '("clone") args (list source) (and dest (list dest)))))
    (majutsu--message-with-log "Cloning repository...")
    (majutsu-git--start cmd-args "Clone completed")))

(defun majutsu-git-init (args)
  "Initialize a new Git-backed jj repo. Prompts for DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-init-transient)))
  (let* ((dest (file-name-as-directory
                (expand-file-name
                 (read-directory-name "Create repository in: " nil nil nil))))
         (args (mapcar (lambda (arg)
                         (majutsu-git--expand-option-arg arg "--git-repo="))
                       args))
         (cmd-args (append '("init") args (list dest))))
    (majutsu--message-with-log "Initializing repository...")
    (majutsu-git--start cmd-args "Init completed")))

(defun majutsu-git-export ()
  "Update the underlying Git repo with changes made in the repo."
  (interactive)
  (let ((exit (majutsu-git--run "export")))
    (when (zerop exit)
      (message "Exported to Git"))))

(defun majutsu-git-import ()
  "Update repo with changes made in the underlying Git repo."
  (interactive)
  (let ((exit (majutsu-git--run "import")))
    (when (zerop exit)
      (message "Imported from Git"))))

(defun majutsu-git-root ()
  "Show the underlying Git directory of the current repository."
  (interactive)
  (let* ((dir (string-trim (or (car (majutsu-jj-lines "git" "root")) ""))))
    (if (string-empty-p dir)
        (message "No underlying Git directory found")
      (kill-new dir)
      (message "Git root: %s (copied)" dir))))

(transient-define-argument majutsu-git-push:-b ()
  :description "Bookmark"
  :class 'transient-option
  :shortarg "-b"
  :argument "--bookmark="
  :multi-value 'repeat
  :reader #'majutsu-read-bookmarks)

;;; Git Transients

;;;###autoload
(transient-define-prefix majutsu-git-transient ()
  "Top-level transient for jj git operations."
  :man-page "jj-git"
  :transient-non-suffix t
  [:description "JJ Git"
   :class transient-columns
   ["Sync"
    ("p" "Push" majutsu-git-push-transient)
    ("f" "Fetch" majutsu-git-fetch-transient)
    ("e" "Export" majutsu-git-export)
    ("m" "Import" majutsu-git-import)]
   ["Remotes"
    ("r" "Manage remotes" majutsu-git-remote-transient)
    ("o" "Git root" majutsu-git-root)]
   ["Repository"
    ("c" "Clone" majutsu-git-clone-transient)
    ("i" "Init" majutsu-git-init-transient)]
   [("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-push-transient ()
  "Transient for jj git push."
  :man-page "jj-git-push"
  [:description "JJ Git Push"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote" "--remote=" :choices majutsu-git--remote-names)
    (majutsu-git-push:-b)
    ("-a" "All bookmarks" "--all")
    ("-t" "Tracked only" "--tracked")
    ("-D" "Deleted" "--deleted")
    ("-E" "Allow empty desc" "--allow-empty-description")
    ("-P" "Allow private" "--allow-private")
    ("-r" "Revisions" "--revisions=")
    ("-c" "Change" "--change=")
    ("-N" "Named X=REV" "--named=")
    ("-y" "Dry run" "--dry-run")]
   [("p" "Push" majutsu-git-push)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'majutsu-git-fetch-transient "majutsu-git" nil t)
(transient-define-prefix majutsu-git-fetch-transient ()
  "Transient for jj git fetch."
  :man-page "jj-git-fetch"
  [:description "JJ Git Fetch"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote" "--remote=" :choices majutsu-git--remote-names)
    ("-B" "Branch" "--branch=")
    ("-t" "Tracked only" "--tracked")
    ("-A" "All remotes" "--all-remotes")]
   [("f" "Fetch" majutsu-git-fetch)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-remote-transient ()
  "Transient for managing Git remotes."
  :man-page "jj-git-remote"
  [:description "JJ Git Remote"
   :class transient-columns
   ["Arguments (add)"
    ("-T" "Fetch tags" "--fetch-tags="
     :choices ("all" "included" "none"))]
   ["Actions"
    ("l" "List" majutsu-git-remote-list)
    ("a" "Add" majutsu-git-remote-add)
    ("d" "Remove" majutsu-git-remote-remove)
    ("r" "Rename" majutsu-git-remote-rename)
    ("u" "Set URL" majutsu-git-remote-set-url)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-clone-transient ()
  "Transient for jj git clone."
  :man-page "jj-git-clone"
  [:description "JJ Git Clone"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote name" "--remote=")
    ("-C" "Colocate" "--colocate")
    ("-x" "No colocate" "--no-colocate")
    ("-d" "Depth" "--depth=")
    ("-T" "Fetch tags" "--fetch-tags=" :choices ("all" "included" "none"))]
   [("c" "Clone" majutsu-git-clone)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-init-transient ()
  "Transient for jj git init."
  :man-page "jj-git-init"
  [:description "JJ Git Init"
   :class transient-columns
   ["Arguments"
    ("-C" "Colocate" "--colocate")
    ("-x" "No colocate" "--no-colocate")
    ("-g" "Use existing git repo" "--git-repo=")]
   [("i" "Init" majutsu-git-init)
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-git)
;;; majutsu-git.el ends here
