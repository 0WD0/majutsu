;;; majutsu-git.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library wraps jj's git-compatible commands and exposes them
;; through Majutsu transients.

;;; Code:

(require 'majutsu)

;;; majutsu-git

;;;###autoload (autoload 'majutsu-git-push "majutsu-git" nil t)
(transient-define-suffix majutsu-git-push (args)
  "Push to git remote with ARGS."
  (interactive (list (transient-args 'majutsu-git-push-transient)))
  (let* ((cmd-args (append '("git" "push") args))
         (success-msg "Successfully pushed to remote"))
    (majutsu--message-with-log "Pushing to remote...")
    (majutsu-start-jj
     cmd-args
     nil
     (lambda (process exit-code)
       (majutsu--handle-push-result cmd-args
                                    (majutsu--process-section-output process)
                                    success-msg
                                    exit-code)))))

(defun majutsu--handle-push-result (_cmd-args result success-msg exit-code)
  "Push result handler with bookmark analysis.

RESULT is the recorded output from the process buffer.  EXIT-CODE is
jj's exit status."
  (let ((trimmed-result (string-trim (or result ""))))
    (majutsu--debug "Push result: %s" trimmed-result)

    (cond
     ;; Check for bookmark push restrictions
     ((or (string-match-p "Refusing to create new remote bookmark" trimmed-result)
          (string-match-p "Refusing to push" trimmed-result)) ;; Generic refusal often linked to this in older versions
      ;; Extract bookmark names that couldn't be pushed
      (let ((bookmark-names (majutsu--extract-bookmark-names trimmed-result)))
        (if bookmark-names
            (message "💡 New bookmarks must be tracked first: 'jj bookmark track %s'"
                     (string-join bookmark-names ", "))
          (message "💡 New bookmarks must be tracked first (e.g. 'jj bookmark track name@remote')")))
      nil)

     ;; Check for creating new heads (often needs rebase or force, unrelated to tracking usually, but check context)
     ((string-match-p "would create new heads" trimmed-result)
      (message "💡 Push would create new heads. Fetch and rebase, or force push.")
      nil)

     ;; Check for authentication issues
     ((string-match-p "Permission denied\\|authentication failed\\|403" trimmed-result)
      (message "💡 Check your git credentials and repository permissions")
      nil)

     ;; Check for network issues
     ((string-match-p "Could not resolve hostname\\|Connection refused\\|timeout" trimmed-result)
      (message "💡 Check your network connection and remote URL")
      nil)

     ;; Check for non-fast-forward issues
     ((string-match-p "non-fast-forward\\|rejected.*fetch first" trimmed-result)
      (message "💡 Run 'jj git fetch' first to update remote tracking")
      nil)

     ;; Analyze majutsu-specific push patterns and provide contextual help
     ((and (integerp exit-code) (zerop exit-code)
           (string-match-p "Nothing changed" trimmed-result))
      (message "💡 Nothing to push - all bookmarks are up to date")
      t)

     ;; Error path: rely on process buffer + error summary message.
     ((and (integerp exit-code) (not (zerop exit-code)))
      nil)

     ;; Success case
     (t
      (when (string-empty-p trimmed-result)
        (message "%s" success-msg))
      t))))


;;;###autoload (autoload 'majutsu-git-fetch "majutsu-git" nil t)
(defun majutsu-git-fetch (args)
  "Fetch from git remote with ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-fetch-transient)))
  (majutsu--message-with-log "Fetching from remote...")
  (let* ((tracked? (member "--tracked" args))
         (all-remotes? (member "--all-remotes" args))

         (branch-args (seq-filter (lambda (arg) (string-prefix-p "--branch=" arg)) args))
         (remote-args (seq-filter (lambda (arg) (string-prefix-p "--remote=" arg)) args))

         (cmd-args (append '("git" "fetch")
                           (when tracked? '("--tracked"))
                           (when all-remotes? '("--all-remotes"))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--branch" (substring s (length "--branch="))))
                                                   branch-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--remote" (substring s (length "--remote="))))
						   remote-args)))))
    (majutsu-start-jj cmd-args "Fetched from remote")))

(defun majutsu--get-git-remotes ()
  "Return a list of Git remote names for the current repository.
Tries `jj git remote list' first, then falls back to `git remote'."
  (let* ((out (condition-case _
                  (majutsu-run-jj "git" "remote" "list")
                (error "")))
         (names (if (and out (not (string-empty-p out)))
                    (let* ((lines (split-string out "\n" t))
                           (names (mapcar (lambda (l)
                                            (car (split-string l "[ :\t]" t)))
                                          lines)))
                      (delete-dups (copy-sequence names)))
                  ;; Fallback to plain `git remote`
                  (with-temp-buffer
                    (let* ((exit (process-file "git" nil t nil "remote")))
                      (when (eq exit 0)
                        (split-string (buffer-string) "\n" t)))))))
    names))

(defun majutsu-git-remote-list ()
  "List Git remotes in a temporary buffer."
  (interactive)
  (let* ((output (majutsu-run-jj "git" "remote" "list"))
         (buf (get-buffer-create "*Majutsu Git Remotes*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (majutsu-display-buffer buf 'log)))

(defun majutsu-git-remote-add (args)
  "Add a Git remote. Prompts for name and URL; respects ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-remote-transient)))
  (let* ((remote (read-string "Remote name: "))
         (url (read-string (format "URL for %s: " remote)))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (cmd-args (append '("git" "remote" "add")
                           (when fetch-tags (list fetch-tags))
                           (list remote url)))
         (exit (apply #'majutsu-call-jj cmd-args)))
    (when (zerop exit)
      (message "Added remote %s" remote))))

(defun majutsu-git-remote-remove ()
  "Remove a Git remote and forget its bookmarks."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (remote (completing-read "Remove remote: " remotes nil t)))
    (when (and remote (not (string-empty-p remote)))
      (let* ((cmd-args (list "git" "remote" "remove" remote))
             (exit (apply #'majutsu-call-jj cmd-args)))
        (when (zerop exit)
          (message "Removed remote %s" remote))))))

(defun majutsu-git-remote-rename ()
  "Rename a Git remote."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (old (completing-read "Rename remote: " remotes nil t))
         (new (read-string (format "New name for %s: " old))))
    (when (and (not (string-empty-p old)) (not (string-empty-p new)))
      (let* ((cmd-args (list "git" "remote" "rename" old new))
             (exit (apply #'majutsu-call-jj cmd-args)))
        (when (zerop exit)
          (message "Renamed remote %s -> %s" old new))))))

(defun majutsu-git-remote-set-url ()
  "Set URL of a Git remote."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (remote (completing-read "Set URL for remote: " remotes nil t))
         (url (read-string (format "New URL for %s: " remote))))
    (when (and (not (string-empty-p remote)) (not (string-empty-p url)))
      (let* ((cmd-args (list "git" "remote" "set-url" remote url))
             (exit (apply #'majutsu-call-jj cmd-args)))
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
         (remote-name (let ((arg (seq-find (lambda (a) (string-prefix-p "--remote=" a)) args)))
                        (when arg (substring arg (length "--remote=")))))
         (depth (let ((arg (seq-find (lambda (a) (string-prefix-p "--depth=" a)) args)))
                  (when arg (substring arg (length "--depth=")))))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (colocate? (member "--colocate" args))
         (no-colocate? (member "--no-colocate" args))
         (cmd-args (append '("git" "clone")
                           (when remote-name (list "--remote" remote-name))
                           (when colocate? '("--colocate"))
                           (when no-colocate? '("--no-colocate"))
                           (when depth (list "--depth" depth))
                           (when fetch-tags (list fetch-tags))
                           (list source)
                           (when dest (list dest))))
         (exit (apply #'majutsu-call-jj cmd-args)))
    (when (zerop exit)
      (message "Clone completed"))))

(defun majutsu-git-init (args)
  "Initialize a new Git-backed jj repo. Prompts for DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-init-transient)))
  (let* ((dest (let ((d (read-directory-name "Destination (default .): " nil nil t)))
                 (if (and d (not (string-empty-p d))) (directory-file-name d) ".")))
         (git-repo (let ((arg (seq-find (lambda (a) (string-prefix-p "--git-repo=" a)) args)))
                     (when arg (substring arg (length "--git-repo=")))))
         (dest (directory-file-name (expand-file-name dest)))
         (git-repo (expand-file-name git-repo))
         (colocate? (member "--colocate" args))
         (no-colocate? (member "--no-colocate" args))
         (cmd-args (append '("git" "init")
                           (when colocate? '("--colocate"))
                           (when no-colocate? '("--no-colocate"))
                           (when git-repo (list "--git-repo" git-repo))
                           (list dest)))
         (exit (apply #'majutsu-call-jj cmd-args)))
    (when (zerop exit)
      (message "Init completed"))))

(defun majutsu-git-export ()
  "Update the underlying Git repo with changes made in the repo."
  (interactive)
  (let ((exit (majutsu-call-jj "git" "export")))
    (when (zerop exit)
      (message "Exported to Git"))))

(defun majutsu-git-import ()
  "Update repo with changes made in the underlying Git repo."
  (interactive)
  (let ((exit (majutsu-call-jj "git" "import")))
    (when (zerop exit)
      (message "Imported from Git"))))

(defun majutsu-git-root ()
  "Show the underlying Git directory of the current repository."
  (interactive)
  (let* ((dir (string-trim (majutsu-run-jj "git" "root"))))
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
    ("-R" "Remote" "--remote=" :choices majutsu--get-git-remotes)
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
    ("-R" "Remote" "--remote=" :choices majutsu--get-git-remotes)
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
