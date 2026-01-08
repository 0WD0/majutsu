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
(require 'json)
(require 'seq)
(require 'subr-x)

;;; majutsu-bookmark

(defun majutsu-bookmarks-at-point (&optional bookmark-type)
  "Return a list of bookmark names at point."
  (let* ((rev (or (magit-section-value-if 'jj-commit) "@"))
         (args (append `("show" ,rev "--no-patch" "--ignore-working-copy"
                         "-T" ,(pcase bookmark-type
                                 ('remote "remote_bookmarks")
                                 ('local "local_bookmarks")
                                 (_ "bookmarks")))))
         (output (apply #'majutsu-jj-string args))
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

(defun majutsu--bookmark-split-remote-ref (ref)
  "Split remote bookmark REF like NAME@REMOTE into (NAME . REMOTE).

Splits at the last \"@\"."
  (let ((ref (string-trim (substring-no-properties ref))))
    (if (string-match "\\`\\(.*\\)@\\([^@]+\\)\\'" ref)
        (cons (match-string 1 ref) (match-string 2 ref))
      (cons ref nil))))

(defun majutsu--bookmark--json-lines (text)
  "Parse TEXT as newline-separated JSON values.

Return a list of successfully parsed values."
  (let ((values nil))
    (dolist (line (split-string text "\n" t))
      (condition-case nil
          (push (json-parse-string line) values)
        (error nil)))
    (nreverse values)))

(defun majutsu--bookmark--remote-args (remotes)
  "Build repeated `--remote <REMOTE>` args from REMOTES."
  (apply #'append
         (mapcar (lambda (remote) (list "--remote" remote))
                 remotes)))

(defun majutsu--bookmark-remote-name-candidates ()
  "Return remote bookmark names for completion (unique, plain strings)."
  (let* ((template "if(remote && present, json(name) ++ \"\\n\", \"\")")
         (args '("bookmark" "list" "--quiet" "--all-remotes" "-T"))
         (names (majutsu--bookmark--json-lines
                 (apply #'majutsu-jj-string (append args (list template))))))
    (delete-dups (seq-filter #'stringp names))))

(defun majutsu--bookmark-git-remote-candidates ()
  "Return Git remote names for completion."
  (let* ((out (majutsu-jj-string "git" "remote" "list"))
         (lines (split-string out "\n" t))
         (names (delq nil
                      (mapcar (lambda (line)
                                (unless (string-match-p "\\`\\(Error\\|error\\|fatal\\):" line)
                                  (when (string-match "\\`\\([^ \t]+\\)" line)
                                    (match-string 1 line))))
                              lines))))
    (delete-dups names)))

(defun majutsu--get-bookmark-names (&optional scope)
  "Return bookmark names for completion.

SCOPE controls what to return:

- nil or `local': local bookmark names (e.g. \"main\")
- t or `remote': remote bookmark refs (e.g. \"main@origin\")
- `remote-tracked': tracked remote bookmark refs only
- `remote-untracked': untracked remote bookmark refs only"
  (let* ((scope (pcase scope
                  ((or 'nil 'local) 'local)
                  ('remote 'remote)
                  ('remote-tracked 'remote-tracked)
                  ('remote-untracked 'remote-untracked)
                  (_ (user-error "Unknown bookmark name scope: %S" scope))))
         (template (pcase scope
                     ('local
                      "if(!remote && present, name ++ \"\\n\", \"\")")
                     ('remote
                      "if(remote && present, name ++ \"@\" ++ remote ++ \"\\n\", \"\")")
                     ('remote-tracked
                      "if(remote && present && tracked, name ++ \"@\" ++ remote ++ \"\\n\", \"\")")
                     ('remote-untracked
                      "if(remote && present && !tracked, name ++ \"@\" ++ remote ++ \"\\n\", \"\")")))
         (args (append '("bookmark" "list" "--quiet")
                       (pcase scope
                         ((or 'remote 'remote-untracked) '("--all-remotes"))
                         ('remote-tracked '("--tracked"))
                         (_ nil))
                       (list "-T" template)))
         (names (split-string (apply #'majutsu-jj-string args) "\n" t)))
    (delete-dups names)))

;;;###autoload
(defun majutsu-bookmark-create ()
  "Create a new bookmark."
  (interactive)
  (let* ((revset (or (magit-section-value-if 'jj-commit) "@"))
         (name (read-string "Bookmark name: ")))
    (unless (string-empty-p name)
      (majutsu-run-jj "bookmark" "create" name "-r" revset))))

;;;###autoload
(defun majutsu-bookmark-delete ()
  "Delete a bookmark and propagate on next push."
  (interactive)
  (let* ((bookmarks (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category bookmarks 'majutsu-bookmark))
         (choice (and bookmarks (completing-read "Delete bookmark (propagates on push): " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (when (zerop (majutsu-run-jj "bookmark" "delete" choice))
        (message "Deleted bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-forget ()
  "Forget a bookmark (local only, no deletion propagation)."
  (interactive)
  (let* ((bookmarks (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category bookmarks 'majutsu-bookmark))
         (choice (and bookmarks (completing-read "Forget bookmark: " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (when (zerop (majutsu-run-jj "bookmark" "forget" choice))
        (message "Forgot bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-track ()
  "Track remote bookmark(s)."
  (interactive)
  (let* ((bookmark-patterns
          (completing-read-multiple
           "Track bookmark name(s)/pattern(s): "
           (majutsu--bookmark-remote-name-candidates) nil nil))
         (remote-patterns
          (completing-read-multiple
           "Remote(s)/pattern(s) (empty = all): "
           (majutsu--bookmark-git-remote-candidates) nil nil))
         (bookmark-patterns (seq-filter (lambda (s) (not (string-empty-p s)))
                                        bookmark-patterns))
         (remote-patterns (seq-filter (lambda (s) (not (string-empty-p s)))
                                      remote-patterns)))
    (if (null bookmark-patterns)
        (message "No bookmark name/pattern provided")
      (when (zerop (apply #'majutsu-run-jj
                          (append (list "bookmark" "track")
                                  bookmark-patterns
                                  (majutsu--bookmark--remote-args remote-patterns))))
        (message "Tracking remote bookmark(s): %s%s"
                 (string-join bookmark-patterns ", ")
                 (if remote-patterns
                     (format " (remote(s): %s)" (string-join remote-patterns ", "))
                   ""))))))

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a temporary buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (let* ((args (append '("bookmark" "list" "--quiet") (and all '("--all-remotes"))))
         (output (apply #'majutsu-jj-string args))
         (buf (get-buffer-create "*Majutsu Bookmarks*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (font-lock-mode 1)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (majutsu-display-buffer buf 'log)))

;;;###autoload
(defun majutsu-read-bookmarks (prompt &optional _init-input _history)
  "Return interactive arguments for bookmark move commands."
  (let* ((bookmark (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category bookmark 'majutsu-bookmark))
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
      (when (zerop (apply #'majutsu-run-jj args))
        (message (if allow-backwards
                     "Moved bookmark(s) (allow backwards) to %s: %s"
                   "Moved bookmark(s) to %s: %s")
                 commit (string-join names ", "))))))

;;;###autoload
(defun majutsu-bookmark-move (names commit &optional allow-backwards)
  "Move bookmark bookmark(s) NAMES to COMMIT.
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
   (let* ((bookmarks (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category bookmarks 'majutsu-bookmark))
          (old (and bookmarks (completing-read "Rename bookmark: " table nil t)))
          (new (read-string (format "New name for %s: " old))))
     (list old new)))
  (when (and (not (string-empty-p old)) (not (string-empty-p new)))
    (when (zerop (majutsu-run-jj "bookmark" "rename" old new))
      (message "Renamed bookmark '%s' -> '%s'" old new))))

;;;###autoload
(defun majutsu-bookmark-set (name commit)
  "Create or update bookmark NAME to point to COMMIT."
  (interactive
   (let* ((bookmarks (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category bookmarks 'majutsu-bookmark))
          (name (completing-read "Set bookmark: " table))
          (at (or (magit-section-value-if 'jj-commit) "@"))
          (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
     (list name rev)))
  (when (zerop (majutsu-run-jj "bookmark" "set" name "-r" commit))
    (message "Set bookmark '%s' to %s" name commit)))

;;;###autoload
(defun majutsu-bookmark-untrack (bookmarks &optional remotes)
  "Stop tracking remote bookmark(s).

BOOKMARKS are bookmark name patterns (glob/exact/regex/substring).
REMOTES are remote name patterns passed via repeated `--remote`."
  (interactive
   (list (completing-read-multiple
          "Untrack bookmark name(s)/pattern(s): "
          (majutsu--bookmark-remote-name-candidates))
         (completing-read-multiple
          "Remote(s)/pattern(s) (empty = all): "
          (majutsu--bookmark-git-remote-candidates))))
  (defvar crm-separator)
  (let* ((bookmarks (seq-filter (lambda (s) (not (string-empty-p s))) bookmarks))
         (remotes (seq-filter (lambda (s) (not (string-empty-p s))) (or remotes '()))))
    (when bookmarks
      (when (zerop (apply #'majutsu-run-jj
                          (append (list "bookmark" "untrack")
                                  bookmarks
                                  (majutsu--bookmark--remote-args remotes))))
        (message "Untracked: %s%s"
                 (string-join bookmarks ", ")
                 (if remotes
                     (format " (remote(s): %s)" (string-join remotes ", "))
                   ""))))))

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

(transient-define-prefix majutsu-bookmark ()
  "Internal transient for jj bookmark operations."
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
     :description "Move bookmark to commit")
    ("M" "Move bookmark(s) --allow-backwards" majutsu-bookmark-move-allow-backwards
     :description "Move allowing backwards")
    ("r" "Rename bookmark" majutsu-bookmark-rename
     :description "Rename bookmark")]
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
   [("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-bookmark)
;;; majutsu-bookmark.el ends here
