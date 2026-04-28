;;; majutsu-git.el --- Git integration commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library wraps jj's git-compatible commands and exposes them
;; through Majutsu transients.

;;; Code:

(require 'majutsu)
(require 'majutsu-remote)

(require 'seq)
(require 'subr-x)

(declare-function majutsu-repository-transient-prefix "majutsu-core")

;;; majutsu-git

(defun majutsu-git--start (args &optional success-msg finish-callback)
  "Start `jj git ARGS' asynchronously, for side-effects."
  (majutsu-start-jj (append '("git") args) success-msg finish-callback))

(defvar majutsu-git-url-history nil
  "Minibuffer history for Git remote URLs and paths.")

(defun majutsu-git--read-url-or-path (prompt)
  "Read a Git URL or local path with PROMPT."
  (majutsu-read-string prompt nil 'majutsu-git-url-history))

(defun majutsu-git--transient-read-url-or-path (prompt initial-input _history)
  "Read a Git URL/path transient value with PROMPT and INITIAL-INPUT."
  (majutsu-read-string prompt initial-input 'majutsu-git-url-history))

(defun majutsu-git--pathish-url-p (value)
  "Return non-nil if VALUE should be converted as a local path for jj."
  (and (stringp value)
       (or (file-remote-p value)
           (file-name-absolute-p value)
           (string= value "~")
           (string-prefix-p "~/" value))))

(defun majutsu-git--url-or-path-arg (value)
  "Return VALUE converted for jj when it is an Emacs local/remote path."
  (if (majutsu-git--pathish-url-p value)
      (majutsu-convert-filename-for-jj (expand-file-name value))
    value))

(defun majutsu-git--expand-option-arg (arg prefix)
  "If ARG begins with PREFIX, expand the file name part."
  (if (and (stringp arg) (string-prefix-p prefix arg))
      (concat prefix
              (majutsu-convert-filename-for-jj
               (expand-file-name (substring arg (length prefix)))))
    arg))

(defun majutsu-git--expand-url-option-arg (arg prefix)
  "If ARG begins with PREFIX, convert its URL/path value for jj."
  (if (and (stringp arg) (string-prefix-p prefix arg))
      (concat prefix (majutsu-git--url-or-path-arg
                      (substring arg (length prefix))))
    arg))

(defun majutsu-git--expand-remote-url-arg (arg)
  "Convert ARG as a remote URL option or positional URL/path."
  (let* ((expanded (majutsu-git--expand-url-option-arg arg "--fetch="))
         (expanded (majutsu-git--expand-url-option-arg expanded "--push=")))
    (if (equal expanded arg)
        (majutsu-git--url-or-path-arg arg)
      expanded)))

(transient-define-suffix majutsu-git-push (args)
  "Push to git remote with ARGS."
  (interactive (list (transient-args 'majutsu-git-push-transient)))
  (majutsu--message-with-log "Pushing to remote...")
  (majutsu-git--start (append '("push") args) "Pushed to remote"))

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
  (interactive (list (transient-args 'majutsu-git-remote-add-transient)))
  (let* ((args (mapcar (lambda (arg)
                         (majutsu-git--expand-url-option-arg arg "--push-url="))
                       args))
         (remote (majutsu-read-new-remote-name "Remote name"))
         (url (majutsu-git--url-or-path-arg
               (majutsu-git--read-url-or-path (format "URL for %s" remote))))
         (exit (majutsu-run-jj
                "git" (append '("remote" "add") args (list remote url)))))
    (when (zerop exit)
      (message "Added remote %s" remote))))

(defun majutsu-git-remote-remove ()
  "Remove a Git remote and forget its bookmarks."
  (interactive)
  (let* ((remote (majutsu-read-remote-name "Remove remote" t))
         (cmd-args (list "remote" "remove" remote))
         (exit (majutsu-run-jj "git" cmd-args)))
    (when (zerop exit)
      (message "Removed remote %s" remote))))

(defun majutsu-git-remote-rename ()
  "Rename a Git remote."
  (interactive)
  (let* ((old (majutsu-read-remote-name "Rename remote" t))
         (new (majutsu-read-new-remote-name (format "New name for %s" old)))
         (cmd-args (list "remote" "rename" old new))
         (exit (majutsu-run-jj "git" cmd-args)))
    (when (zerop exit)
      (message "Renamed remote %s -> %s" old new))))

(defun majutsu-git-remote-set-url (args)
  "Set URL of a Git remote."
  (interactive (list (transient-args 'majutsu-git-remote-set-url-transient)))
  (when-let* ((remote (majutsu-read-remote-name "Set URL for remote" t))
              (args (or args
                        (list (majutsu-git--read-url-or-path
                               (format "Fetch URL for %s" remote))))))
    (let* ((args (mapcar #'majutsu-git--expand-remote-url-arg args))
           (exit (majutsu-run-jj
                  "git" (append '("remote" "set-url") (list remote) args))))
      (when (zerop exit)
        (message "Set URL for %s" remote)))))

(defun majutsu-git-clone (args)
  "Clone a Git repo into a new jj repo.
Prompts for SOURCE and optional DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-clone-transient)))
  (let* ((source (majutsu-git--url-or-path-arg
                  (majutsu-git--read-url-or-path "Source (URL or path)")))
         (dest   (let ((d (read-directory-name "Destination (optional): " nil nil nil)))
                   (when (and d (not (string-empty-p (expand-file-name d))))
                     ;; If user picks current dir, treat as empty and let jj default
                     (let ((dd (directory-file-name d)))
                       (if (string= dd (directory-file-name default-directory)) nil dd)))))
         (dest-arg (and dest (majutsu-convert-filename-for-jj dest)))
         (cmd-args (append '("clone") args (list source) (and dest-arg (list dest-arg)))))
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
         (cmd-args (append '("init") args (list (majutsu-convert-filename-for-jj dest)))))
    (majutsu--message-with-log "Initializing repository...")
    (majutsu-git--start cmd-args "Init completed")))

(defun majutsu-git-export ()
  "Update the underlying Git repo with changes made in the repo."
  (interactive)
  (let ((exit (majutsu-run-jj "git" "export")))
    (when (zerop exit)
      (message "Exported to Git"))))

(defun majutsu-git-import ()
  "Update repo with changes made in the underlying Git repo."
  (interactive)
  (let ((exit (majutsu-run-jj "git" "import")))
    (when (zerop exit)
      (message "Imported from Git"))))

(defun majutsu-git-root ()
  "Show the underlying Git directory of the current repository."
  (interactive)
  (let* ((raw (string-trim (or (car (majutsu-jj-lines "git" "root")) "")))
         (dir (unless (string-empty-p raw)
                (majutsu-jj-expand-filename-from-jj raw default-directory))))
    (if (or (null dir) (string-empty-p dir))
        (message "No underlying Git directory found")
      (kill-new dir)
      (message "Git root: %s (copied)" dir))))

(transient-define-argument majutsu-git-push:-b ()
  :description "Bookmark"
  :class 'transient-option
  :shortarg "-b"
  :argument "--bookmark="
  :multi-value 'repeat
  :reader #'majutsu-read-bookmark-patterns)

(defun majutsu-git-push--read-revset (prompt initial-input _history)
  "Read revset for `jj git push --revisions='."
  (majutsu-read-revset prompt initial-input '("git" "push" "-r")))

(defun majutsu-git-push--read-change (prompt initial-input _history)
  "Read change id for `jj git push --change='."
  (majutsu-read-single-revset prompt initial-input '("git" "push" "-c")))

(transient-define-argument majutsu-git-push:--revisions ()
  :description "Revisions"
  :class 'transient-option
  :key "-r"
  :argument "--revisions="
  :multi-value 'repeat
  :prompt "Revisions: "
  :reader #'majutsu-git-push--read-revset)

(transient-define-argument majutsu-git-push:--change ()
  :description "Change"
  :class 'transient-option
  :key "-c"
  :argument "--change="
  :prompt "Change: "
  :reader #'majutsu-git-push--read-change)

(transient-define-argument majutsu-git-push:--named ()
  :description "Named X=REV"
  :class 'transient-option
  :key "-N"
  :argument "--named="
  :prompt "Named (X=REV): ")

(transient-define-argument majutsu-git-push:--option ()
  :description "Git option"
  :class 'transient-option
  :key "-o"
  :argument "--option="
  :multi-value 'repeat
  :prompt "Git push option: ")

(transient-define-argument majutsu-git-fetch:--remote ()
  :description "Remote"
  :class 'transient-option
  :key "-R"
  :argument "--remote="
  :multi-value 'repeat
  :prompt "Remote: "
  :choices #'majutsu-remote-names)

(transient-define-argument majutsu-git-fetch:--branch ()
  :description "Branch"
  :class 'transient-option
  :key "-b"
  :argument "--branch="
  :multi-value 'repeat
  :prompt "Branch: ")

(transient-define-argument majutsu-git-clone:--branch ()
  :description "Branch"
  :class 'transient-option
  :key "-b"
  :argument "--branch="
  :multi-value 'repeat
  :prompt "Branch: ")

(transient-define-argument majutsu-git-remote-add:--push-url ()
  :description "Push URL"
  :class 'transient-option
  :key "-P"
  :argument "--push-url="
  :prompt "Push URL: "
  :reader #'majutsu-git--transient-read-url-or-path)

(transient-define-argument majutsu-git-remote-set-url:--fetch ()
  :description "Fetch URL"
  :class 'transient-option
  :key "-f"
  :argument "--fetch="
  :prompt "Fetch URL: "
  :reader #'majutsu-git--transient-read-url-or-path)

(transient-define-argument majutsu-git-remote-set-url:--push ()
  :description "Push URL"
  :class 'transient-option
  :key "-p"
  :argument "--push="
  :prompt "Push URL: "
  :reader #'majutsu-git--transient-read-url-or-path)

(defun majutsu-git-push--repo-args (args)
  "Keep only stable `jj git push' ARGS for repository defaults."
  (seq-filter (lambda (arg)
                (or (string-prefix-p "--remote=" arg)
                    (member arg '("--all" "--tracked" "--deleted"
                                  "--allow-empty-description" "--allow-private"))))
              args))

(defun majutsu-git-fetch--repo-args (args)
  "Keep only stable `jj git fetch' ARGS for repository defaults."
  (seq-filter (lambda (arg)
                (or (string-prefix-p "--remote=" arg)
                    (member arg '("--tracked" "--all-remotes"))))
              args))

;;; Git Transients

;;;###autoload(autoload 'majutsu-git-transient "majutsu-git" nil t)
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
  :class 'majutsu-repository-transient-prefix
  :repo-namespace 'majutsu-git
  :repo-key 'majutsu-git-push
  :repo-filter #'majutsu-git-push--repo-args
  [:description "JJ Git Push"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote" "--remote=" :choices #'majutsu-remote-names)
    (majutsu-git-push:-b)
    ("-a" "All bookmarks" "--all")
    ("-t" "Tracked only" "--tracked")
    ("-D" "Deleted" "--deleted")
    ("-E" "Allow empty desc" "--allow-empty-description")
    ("-P" "Allow private" "--allow-private")
    (majutsu-git-push:--revisions)
    (majutsu-git-push:--change)
    (majutsu-git-push:--named)
    (majutsu-git-push:--option)
    ("-y" "Dry run" "--dry-run")]
   [("p" "Push" majutsu-git-push)
    ("W" "Save repo defaults" majutsu-transient-save-repository-defaults
     :transient t)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-fetch-transient ()
  "Transient for jj git fetch."
  :man-page "jj-git-fetch"
  :class 'majutsu-repository-transient-prefix
  :repo-namespace 'majutsu-git
  :repo-key 'majutsu-git-fetch
  :repo-filter #'majutsu-git-fetch--repo-args
  [:description "JJ Git Fetch"
   :class transient-columns
   ["Arguments"
    (majutsu-git-fetch:--remote)
    (majutsu-git-fetch:--branch)
    ("-t" "Tracked only" "--tracked")
    ("-A" "All remotes" "--all-remotes")]
   [("f" "Fetch" majutsu-git-fetch)
    ("W" "Save repo defaults" majutsu-transient-save-repository-defaults
     :transient t)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-remote-add-transient ()
  "Transient for adding a Git remote."
  :man-page "jj-git-remote-add"
  [:description "JJ Git Remote Add"
   :class transient-columns
   ["Arguments"
    ("-T" "Fetch tags" "--fetch-tags="
     :choices ("all" "included" "none"))
    (majutsu-git-remote-add:--push-url)]
   [("a" "Add" majutsu-git-remote-add)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-remote-set-url-transient ()
  "Transient for setting Git remote URLs."
  :man-page "jj-git-remote-set-url"
  [:description "JJ Git Remote Set URL"
   :class transient-columns
   ["Arguments"
    (majutsu-git-remote-set-url:--fetch)
    (majutsu-git-remote-set-url:--push)]
   [("u" "Set URL" majutsu-git-remote-set-url)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-remote-transient ()
  "Transient for managing Git remotes."
  :man-page "jj-git-remote"
  [:description "JJ Git Remote"
   :class transient-columns
   ["Actions"
    ("l" "List" majutsu-git-remote-list)
    ("a" "Add" majutsu-git-remote-add-transient)
    ("d" "Remove" majutsu-git-remote-remove)
    ("r" "Rename" majutsu-git-remote-rename)
    ("u" "Set URL" majutsu-git-remote-set-url-transient)
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
    ("-T" "Fetch tags" "--fetch-tags=" :choices ("all" "included" "none"))
    (majutsu-git-clone:--branch)]
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
