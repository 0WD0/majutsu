;;; majutsu-bookmark.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj bookmark commands and integrates them
;; with Majutsu's transient UI.

;;; Code:

(require 'majutsu)

;;; majutsu-bookmark

;;;###autoload
(defun majutsu-bookmark-transient ()
  "Transient for jj bookmark operations."
  (interactive)
  (majutsu-bookmark-transient--internal))

(defun majutsu-bookmarks-at-point (&optional bookmark-type)
  "Return a list of bookmark names at point."
  (let* ((rev (or (magit-section-value-if 'jj-commit) "@"))
         (args (append `("show" ,rev "--no-patch" "--ignore-working-copy"
                         "-T" ,(pcase bookmark-type
                                 ('remote "remote_bookmarks")
                                 ('local "local_bookmarks")
                                 (_ "bookmarks")))))
         (output (apply #'majutsu-run-jj args))
         (bookmarks (split-string output " " t)))
    (mapcar (lambda (s) (string-remove-suffix "*" s)) bookmarks)))

(defun majutsu-bookmark-at-point ()
  "Return a comma-separated string of bookmark names at point."
  (let ((bookmarks (majutsu-bookmarks-at-point)))
    (when bookmarks
      (string-join bookmarks ","))))

(defun majutsu--extract-bookmark-names (text)
  "Extract bookmark names from jj command output TEXT."
  (let ((names '())
        (start 0))
    (while (string-match "bookmark[: ]+\\([^ \n,]+\\)" text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (nreverse names)))

(defun majutsu--get-bookmark-names (&optional all-remotes)
  "Return bookmark names using --quiet to suppress hints.
When ALL-REMOTES is non-nil, include remote bookmarks formatted as NAME@REMOTE."
  (let* ((template (if all-remotes
                       "if(remote, name ++ '@' ++ remote ++ '\n', '')"
                     "name ++ '\n'"))
         (args (append '("bookmark" "list" "--quiet")
                       (and all-remotes '("--all"))
                       (list "-T" template))))
    (delete-dups (split-string (apply #'majutsu-run-jj args) "\n" t))))

;;;###autoload
(defun majutsu-bookmark-create ()
  "Create a new bookmark."
  (interactive)
  (let* ((revset (or (magit-section-value-if 'jj-commit) "@"))
         (name (read-string "Bookmark name: ")))
    (unless (string-empty-p name)
      (majutsu-call-jj "bookmark" "create" name "-r" revset))))

;;;###autoload
(defun majutsu-bookmark-delete ()
  "Delete a bookmark and propagate on next push."
  (interactive)
  (let* ((names (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category names 'majutsu-bookmark))
         (choice (and names (completing-read "Delete bookmark (propagates on push): " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (when (zerop (majutsu-call-jj "bookmark" "delete" choice))
        (message "Deleted bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-forget ()
  "Forget a bookmark (local only, no deletion propagation)."
  (interactive)
  (let* ((names (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category names 'majutsu-bookmark))
         (choice (and names (completing-read "Forget bookmark: " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (when (zerop (majutsu-call-jj "bookmark" "forget" choice))
        (message "Forgot bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-track ()
  "Track remote bookmark(s)."
  (interactive)
  (let* ((remote-bookmarks (majutsu--get-bookmark-names t))
         (table (majutsu--completion-table-with-category remote-bookmarks 'majutsu-bookmark))
         (choice (and remote-bookmarks (completing-read "Track remote bookmark: " table nil t))))
    (if (not choice)
        (message "No remote bookmarks found")
      (when (zerop (majutsu-call-jj "bookmark" "track" choice))
        (message "Tracking bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a temporary buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (let* ((args (append '("bookmark" "list" "--quiet") (and all '("--all"))))
         (output (apply #'majutsu-run-jj args))
         (buf (get-buffer-create "*Majutsu Bookmarks*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (majutsu-display-buffer buf 'log)))

;;;###autoload
(defun majutsu-read-bookmarks (prompt &optional init-input history)
  "Return interactive arguments for bookmark move commands."
  (let* ((existing (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
         (default (majutsu-bookmark-at-point)))
    (completing-read-multiple
     (if default
         (format "%s (default %s): " prompt default)
       (format "%s: " prompt))
     table nil t nil nil default)))

(defun majutsu--bookmark-move (names commit &optional allow-backwards)
  "Internal helper to move bookmark(s) NAMES to COMMIT.
When ALLOW-BACKWARDS is non-nil, include `--allow-backwards'."
  (when names
    (let ((args (append '("bookmark" "move")
                        (and allow-backwards '("--allow-backwards"))
                        (list "-t" commit)
                        names)))
      (when (zerop (apply #'majutsu-call-jj args))
        (message (if allow-backwards
                     "Moved bookmark(s) (allow backwards) to %s: %s"
                   "Moved bookmark(s) to %s: %s")
                 commit (string-join names ", "))))))

;;;###autoload
(defun majutsu-bookmark-move (names commit &optional allow-backwards)
  "Move existing bookmark(s) NAMES to COMMIT.
With optional ALLOW-BACKWARDS, pass `--allow-backwards' to jj."
  (interactive (list (majutsu-read-bookmarks "Move bookmark(s)") (majutsu-read-revset "Target revset")))
  (majutsu--bookmark-move names commit allow-backwards))

;;;###autoload
(defun majutsu-bookmark-move-allow-backwards (names commit)
  "Move bookmark(s) NAMES to COMMIT allowing backwards moves."
  (interactive (list (majutsu-read-bookmarks "Move bookmark(s)") (majutsu-read-revset "Target revset")))
  (majutsu--bookmark-move names commit t))

;;;###autoload
(defun majutsu-bookmark-rename (old new)
  "Rename bookmark OLD to NEW."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (old (completing-read "Rename bookmark: " table nil t))
          (new (read-string (format "New name for %s: " old))))
     (list old new)))
  (when (and (not (string-empty-p old)) (not (string-empty-p new)))
    (when (zerop (majutsu-call-jj "bookmark" "rename" old new))
      (message "Renamed bookmark '%s' -> '%s'" old new))))

;;;###autoload
(defun majutsu-bookmark-set (name commit)
  "Create or update bookmark NAME to point to COMMIT."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (name (completing-read "Set bookmark: " table nil nil))
          (at (or (magit-section-value-if 'jj-commit) "@"))
          (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
     (list name rev)))
  (when (zerop (majutsu-call-jj "bookmark" "set" name "-r" commit))
    (message "Set bookmark '%s' to %s" name commit)))

;;;###autoload
(defun majutsu-bookmark-untrack (names)
  "Stop tracking remote bookmark(s) NAMES (e.g., name@remote)."
  (interactive
   (let* ((remote-names (majutsu--get-bookmark-names t))
          (table (majutsu--completion-table-with-category remote-names 'majutsu-bookmark))
          (crm-separator (or (bound-and-true-p crm-separator) ", *"))
          (names (completing-read-multiple "Untrack remote bookmark(s): " table nil t)))
     (list names)))

  (defvar crm-separator)
  (when names
    (when (zerop (apply #'majutsu-call-jj (append '("bookmark" "untrack") names)))
      (message "Untracked: %s" (string-join names ", ")))))

(defun majutsu--completion-table-with-category (candidates category)
  "Wrap CANDIDATES with completion METADATA to set CATEGORY.
This prevents third-party UIs (e.g., icons for `bookmark') from
misclassifying Majutsu candidates."
  (let ((metadata `(metadata (category . ,category))))
    (cond
     ((fboundp 'completion-table-with-metadata)
      (completion-table-with-metadata candidates metadata))
     ((functionp candidates)
      (lambda (string pred action)
        (if (eq action 'metadata)
            metadata
          (funcall candidates string pred action))))
     (t
      (lambda (string pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action candidates string pred)))))))

;;; Bookmark Transient

(transient-define-prefix majutsu-bookmark-transient--internal ()
  "Internal transient for jj bookmark operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  ["Bookmark Operations"
   [
    ("l" "List bookmarks" majutsu-bookmark-list
     :description "Show bookmark list")
    ("c" "Create bookmark" majutsu-bookmark-create
     :description "Create new bookmark")]
   [
    ("s" "Set bookmark" majutsu-bookmark-set
     :description "Create/update to commit")
    ("m" "Move bookmark(s)" majutsu-bookmark-move
     :description "Move existing to commit")
    ("M" "Move bookmark(s) --allow-backwards" majutsu-bookmark-move-allow-backwards
     :description "Move allowing backwards")
    ("r" "Rename bookmark" majutsu-bookmark-rename
     :description "Rename existing bookmark")]
   [
    ("t" "Track remote" majutsu-bookmark-track
     :description "Track remote bookmark")
    ("u" "Untrack remote" majutsu-bookmark-untrack
     :description "Stop tracking remote")]
   [
    ("d" "Delete bookmark" majutsu-bookmark-delete
     :description "Delete (propagate)")
    ("f" "Forget bookmark" majutsu-bookmark-forget
     :description "Forget (local)")]
   [("q" "Quit" transient-quit-one)]] )

;;; _
(provide 'majutsu-bookmark)
;;; majutsu-bookmark.el ends here
