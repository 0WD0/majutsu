;;; majutsu-bookmark.el --- Bookmark commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements jj bookmark commands and integrates them
;; with Majutsu's transient UI.

;;; Code:

(require 'majutsu)

(require 'json)
(require 'seq)
(require 'subr-x)

;;; majutsu-bookmark
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

(defun majutsu--bookmark--remote-args (remotes)
  "Build repeated `--remote <REMOTE>` args from REMOTES."
  (apply #'append
         (mapcar (lambda (remote) (list "--remote" remote))
                 remotes)))

(defconst majutsu-bookmark--list-field-separator (char-to-string #x1f)
  "Field separator used by the structured bookmark list template.")

(defconst majutsu-bookmark--list-template
  (let ((sep " ++ \"\\x1f\" ++ "))
    (concat
     "name" sep
     "if(remote, remote, \"\")" sep
     "if(present, \"1\", \"0\")" sep
     "if(conflict, \"1\", \"0\")" sep
     "if(tracked, \"1\", \"0\")" sep
     "if(tracking_present, \"1\", \"0\")" sep
     "if(synced, \"1\", \"0\")" sep
     "if(normal_target, normal_target.change_id().shortest(8), \"\")" sep
     "if(normal_target, normal_target.commit_id().shortest(8), \"\")" sep
     "if(normal_target, normal_target.author().name(), \"\")" sep
     "if(normal_target, normal_target.author().timestamp().ago(), \"\")" sep
     "if(normal_target, normal_target.description().first_line(), \"\")" sep
     "if(tracking_present, tracking_ahead_count.exact(), \"\")" sep
     "if(tracking_present, tracking_behind_count.exact(), \"\")"
     " ++ \"\\n\""))
  "Template for `jj bookmark list' structured rows.")

(defcustom majutsu-bookmark-list-max-name-width 34
  "Maximum width of the bookmark name column in bookmark list buffers."
  :group 'majutsu
  :type 'integer)

(defcustom majutsu-bookmark-list-max-where-width 14
  "Maximum width of the local/remote column in bookmark list buffers."
  :group 'majutsu
  :type 'integer)

(defcustom majutsu-bookmark-list-max-author-width 18
  "Maximum width of the author column in bookmark list buffers."
  :group 'majutsu
  :type 'integer)

(defun majutsu-bookmark--non-empty (string)
  "Return STRING when it is a non-empty string, else nil."
  (and (stringp string) (not (string-empty-p string)) string))

(defun majutsu-bookmark--bool-field (field)
  "Return non-nil when structured bookmark FIELD is true."
  (string= field "1"))

(defun majutsu-bookmark-parse-list-output (output)
  "Parse structured `jj bookmark list' OUTPUT.

Return a list of plists with keys such as `:name', `:remote',
`:change-id', `:commit-id', `:author', and `:description'."
  (let ((entries nil))
    (dolist (line (split-string (or output "") "\n" t))
      (let ((fields (split-string line
                                  (regexp-quote majutsu-bookmark--list-field-separator)
                                  nil)))
        (when (>= (length fields) 14)
          (let ((name (nth 0 fields)))
            (when (majutsu-bookmark--non-empty name)
              (push (list :name name
                          :remote (majutsu-bookmark--non-empty (nth 1 fields))
                          :present (majutsu-bookmark--bool-field (nth 2 fields))
                          :conflict (majutsu-bookmark--bool-field (nth 3 fields))
                          :tracked (majutsu-bookmark--bool-field (nth 4 fields))
                          :tracking-present (majutsu-bookmark--bool-field (nth 5 fields))
                          :synced (majutsu-bookmark--bool-field (nth 6 fields))
                          :change-id (majutsu-bookmark--non-empty (nth 7 fields))
                          :commit-id (majutsu-bookmark--non-empty (nth 8 fields))
                          :author (majutsu-bookmark--non-empty (nth 9 fields))
                          :age (majutsu-bookmark--non-empty (nth 10 fields))
                          :description (or (nth 11 fields) "")
                          :ahead (majutsu-bookmark--non-empty (nth 12 fields))
                          :behind (majutsu-bookmark--non-empty (nth 13 fields)))
                    entries))))))
    (nreverse entries)))

(defun majutsu-bookmark--entry-at-point ()
  "Return bookmark entry plist at point, or nil."
  (let ((value (magit-section-value-if 'jj-bookmark)))
    (cond
     ((and (listp value) (plist-member value :name)) value)
     ((stringp value)
      (let ((ref (majutsu--bookmark-split-remote-ref value)))
        (list :name (car ref) :remote (cdr ref) :present t)))
     (t nil))))

(defun majutsu-bookmark--entry-ref (entry)
  "Return full bookmark ref for ENTRY."
  (let ((name (plist-get entry :name))
        (remote (plist-get entry :remote)))
    (if remote
        (format "%s@%s" name remote)
      name)))

(defun majutsu--bookmark-remote-name-candidates ()
  "Return remote bookmark names for completion (unique, plain strings)."
  (let* ((template "if(remote && present, json(name) ++ \"\\n\", \"\")")
         (args '("bookmark" "list" "--quiet" "--all-remotes" "-T"))
         (lines (majutsu-jj-lines args template))
         (names (delq nil
                      (mapcar (lambda (line)
                                (condition-case nil
                                    (json-parse-string line)
                                  (error nil)))
                              lines))))
    (delete-dups (seq-filter #'stringp names))))

(defun majutsu--bookmark-git-remote-candidates ()
  "Return Git remote names for completion."
  (let* ((lines (majutsu-jj-lines "git" "remote" "list"))
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
         (names (majutsu-jj-lines args)))
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
         (choice (and bookmarks (majutsu-completing-read
                                 "Delete bookmark (propagates on push)" bookmarks
                                 nil t nil nil nil 'majutsu-bookmark))))
    (if (not choice)
        (message "No bookmarks found")
      (when (zerop (majutsu-run-jj "bookmark" "delete" choice))
        (message "Deleted bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-forget ()
  "Forget a bookmark (local only, no deletion propagation)."
  (interactive)
  (let* ((bookmarks (majutsu--get-bookmark-names))
         (choice (and bookmarks (majutsu-completing-read
                                 "Forget bookmark" bookmarks
                                 nil t nil nil nil 'majutsu-bookmark))))
    (if (not choice)
        (message "No bookmarks found")
      (when (zerop (majutsu-run-jj "bookmark" "forget" choice))
        (message "Forgot bookmark '%s'" choice)))))

;;;###autoload
(defun majutsu-bookmark-track ()
  "Track remote bookmark(s)."
  (interactive)
  (let* ((bookmark-patterns
          (majutsu-completing-read-multiple
           "Track bookmark name(s)/pattern(s)"
           (majutsu--bookmark-remote-name-candidates) nil nil))
         (remote-patterns
          (majutsu-completing-read-multiple
           "Remote(s)/pattern(s) (empty = all)"
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

(defvar-local majutsu-bookmark--list-all nil
  "Non-nil when the bookmark list includes remote bookmarks.")

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a dedicated buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (majutsu-setup-buffer #'majutsu-bookmark-list-mode nil
    :buffer "*Majutsu Bookmarks*"
    (majutsu-bookmark--list-all (and all t))))

(defun majutsu-bookmark--list-args ()
  "Return arguments for `jj bookmark list'."
  (append '("bookmark" "list" "--quiet")
          (and majutsu-bookmark--list-all '("--all-remotes"))
          (list "-T" majutsu-bookmark--list-template)))

(defun majutsu-bookmark--bounded-width (entries getter minimum maximum)
  "Return display width for ENTRIES using GETTER, bounded by MINIMUM and MAXIMUM."
  (min maximum
       (apply #'max minimum
              (mapcar (lambda (entry)
                        (string-width (or (funcall getter entry) "")))
                      entries))))

(defun majutsu-bookmark--entry-where (entry)
  "Return local/remote location string for bookmark ENTRY."
  (if-let* ((remote (plist-get entry :remote)))
      (concat "@" remote)
    "local"))

(defun majutsu-bookmark--entry-state (entry)
  "Return state string for bookmark ENTRY."
  (cond
   ((plist-get entry :conflict) "conflict")
   ((not (plist-get entry :present)) "deleted")
   ((not (plist-get entry :remote)) "")
   ((plist-get entry :tracked)
    (if (plist-get entry :synced)
        "tracked"
      (let ((ahead (plist-get entry :ahead))
            (behind (plist-get entry :behind)))
        (if (or ahead behind)
            (format "+%s/-%s" (or ahead "?") (or behind "?"))
          "tracked"))))
   (t "untracked")))

(defun majutsu-bookmark--entry-state-face (entry)
  "Return face for bookmark ENTRY's state."
  (cond
   ((plist-get entry :conflict) 'error)
   ((not (plist-get entry :present)) 'warning)
   ((not (plist-get entry :remote)) 'shadow)
   ((plist-get entry :tracked) 'font-lock-keyword-face)
   (t 'shadow)))

(defun majutsu-bookmark--table-widths (entries)
  "Return column width plist for bookmark ENTRIES."
  (list :name (majutsu-bookmark--bounded-width
               entries (lambda (entry) (plist-get entry :name))
               4 majutsu-bookmark-list-max-name-width)
        :where (majutsu-bookmark--bounded-width
                entries #'majutsu-bookmark--entry-where
                5 majutsu-bookmark-list-max-where-width)
        :state (majutsu-bookmark--bounded-width
                entries #'majutsu-bookmark--entry-state
                5 12)
        :author (majutsu-bookmark--bounded-width
                 entries (lambda (entry) (plist-get entry :author))
                 6 majutsu-bookmark-list-max-author-width)
        :age (majutsu-bookmark--bounded-width
              entries (lambda (entry) (plist-get entry :age))
              3 14)))

(defun majutsu-bookmark--column (text width &optional face help)
  "Return TEXT padded or truncated to WIDTH.
Optionally apply FACE and HELP echo."
  (let* ((raw (or text ""))
         (display (if (string-empty-p raw) "-" raw))
         (truncated (truncate-string-to-width display width nil nil "…"))
         (padded (concat truncated
                         (make-string (max 0 (- width (string-width truncated))) ?\s))))
    (when face
      (setq padded (propertize padded 'font-lock-face face)))
    (when help
      (setq padded (propertize padded 'help-echo help)))
    padded))

(defun majutsu-bookmark--format-header (widths)
  "Return bookmark table header using WIDTHS."
  (propertize
   (concat (majutsu-bookmark--column "Name" (plist-get widths :name))
           " "
           (majutsu-bookmark--column "Where" (plist-get widths :where))
           " "
           (majutsu-bookmark--column "State" (plist-get widths :state))
           " Change   Commit   "
           (majutsu-bookmark--column "Author" (plist-get widths :author))
           " "
           (majutsu-bookmark--column "Age" (plist-get widths :age))
           " Description")
   'font-lock-face 'magit-section-heading))

(defun majutsu-bookmark--format-entry (entry widths)
  "Format bookmark ENTRY as one aligned table row using WIDTHS."
  (let* ((remote (plist-get entry :remote))
         (name (plist-get entry :name))
         (change-id (plist-get entry :change-id))
         (commit-id (plist-get entry :commit-id))
         (target-face (if change-id 'magit-hash 'shadow))
         (branch-face (if remote 'magit-branch-remote 'magit-branch-local)))
    (concat (majutsu-bookmark--column name (plist-get widths :name)
                                      branch-face (majutsu-bookmark--entry-ref entry))
            " "
            (majutsu-bookmark--column (majutsu-bookmark--entry-where entry)
                                      (plist-get widths :where)
                                      branch-face)
            " "
            (majutsu-bookmark--column (majutsu-bookmark--entry-state entry)
                                      (plist-get widths :state)
                                      (majutsu-bookmark--entry-state-face entry))
            " "
            (majutsu-bookmark--column change-id 8 target-face)
            " "
            (majutsu-bookmark--column commit-id 8 target-face)
            " "
            (majutsu-bookmark--column (plist-get entry :author)
                                      (plist-get widths :author)
                                      'magit-log-author)
            " "
            (majutsu-bookmark--column (plist-get entry :age)
                                      (plist-get widths :age)
                                      'magit-log-date)
            " "
            (or (plist-get entry :description) ""))))

(defun majutsu-bookmark--wash-list (_args)
  "Wash `jj bookmark list' output into bookmark sections."
  (let* ((entries (majutsu-bookmark-parse-list-output
                   (buffer-substring-no-properties (point-min) (point-max))))
         (widths (and entries (majutsu-bookmark--table-widths entries))))
    (delete-region (point-min) (point-max))
    (if (null entries)
        (magit-cancel-section)
      (magit-insert-heading
        (if majutsu-bookmark--list-all
            "Bookmarks (all remotes)"
          "Bookmarks"))
      (insert "\n" (majutsu-bookmark--format-header widths) "\n")
      (dolist (entry entries)
        (magit-insert-section (jj-bookmark entry t)
          (magit-insert-heading
            (majutsu-bookmark--format-entry entry widths))))
      (insert "\n"))))

(defun majutsu-bookmark-list-refresh-buffer ()
  "Refresh the bookmark list buffer."
  (majutsu--assert-mode 'majutsu-bookmark-list-mode)
  (magit-insert-section (bookmark-list)
    (majutsu-jj-wash #'majutsu-bookmark--wash-list nil
      (majutsu-bookmark--list-args))))

(defvar-keymap majutsu-bookmark-list-mode-map
  :doc "Keymap for `majutsu-bookmark-list-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-bookmark-list-mode majutsu-mode "Majutsu Bookmarks"
  "Major mode for viewing jj bookmarks."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

;;;###autoload
(defun majutsu-read-bookmarks (prompt &optional _init-input _history)
  "Return interactive arguments for bookmark move commands."
  (let* ((bookmarks (majutsu--get-bookmark-names))
         (default (majutsu-bookmark-at-point)))
    (majutsu-completing-read-multiple
     prompt bookmarks nil t nil nil default 'majutsu-bookmark)))

(defvar majutsu-bookmark-advance-pattern-history nil
  "Minibuffer history for `majutsu-bookmark-advance-patterns'.")

(defun majutsu--bookmark-read-advance-patterns ()
  "Read bookmark name patterns for `jj bookmark advance'."
  (let ((default (majutsu-bookmark-at-point)))
    (seq-filter (lambda (s) (not (string-empty-p s)))
                (majutsu-completing-read-multiple
                 "Advance bookmark name(s)/pattern(s)"
                 (majutsu--get-bookmark-names)
                 nil nil nil 'majutsu-bookmark-advance-pattern-history
                 default 'majutsu-bookmark))))

;;;###autoload
(defun majutsu-bookmark-advance (&optional arg1 arg2)
  "Advance bookmarks using jj's configured default selection.

If ARG1 is a string, use it as the target revset.  For backward
compatibility, if ARG2 is non-nil, use ARG2 as the target revset and
ignore ARG1.  Use `majutsu-bookmark-advance-patterns' for explicit
bookmark-name/pattern selection."
  (interactive)
  (let ((commit (or arg2 (and (stringp arg1) arg1))))
    (apply #'majutsu-run-jj
           (append '("bookmark" "advance")
                   (and commit (list "-t" commit))))))

;;;###autoload
(defun majutsu-bookmark-advance-to (commit)
  "Advance bookmarks using jj's default selection to COMMIT."
  (interactive (list (majutsu-read-revset "Advance to revset")))
  (majutsu-bookmark-advance commit))

;;;###autoload
(defun majutsu-bookmark-advance-patterns (names)
  "Advance bookmark name patterns NAMES using jj's default target."
  (interactive (list (majutsu--bookmark-read-advance-patterns)))
  (if names
      (majutsu-run-jj "bookmark" "advance" names)
    (message "No bookmark name/pattern provided")))

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
          (old (and bookmarks (majutsu-completing-read
                               "Rename bookmark" bookmarks
                               nil t nil nil nil 'majutsu-bookmark)))
          (new (majutsu-read-string (format "New name for %s" old))))
     (list old new)))
  (when (and (not (string-empty-p old)) (not (string-empty-p new)))
    (when (zerop (majutsu-run-jj "bookmark" "rename" old new))
      (message "Renamed bookmark '%s' -> '%s'" old new))))

;;;###autoload
(defun majutsu-bookmark-set (name commit)
  "Create or update bookmark NAME to point to COMMIT."
  (interactive
   (let* ((bookmarks (majutsu--get-bookmark-names))
          (name (majutsu-completing-read "Set bookmark" bookmarks
                                         nil nil nil nil nil 'majutsu-bookmark))
          (at (or (magit-section-value-if 'jj-commit) "@"))
          (rev (majutsu-read-string "Target revision" nil nil at)))
     (list name rev)))
  (when (zerop (majutsu-run-jj "bookmark" "set" name "-r" commit))
    (message "Set bookmark '%s' to %s" name commit)))

;;;###autoload
(defun majutsu-bookmark-untrack (bookmarks &optional remotes)
  "Stop tracking remote bookmark(s).

BOOKMARKS are bookmark name patterns (glob/exact/regex/substring).
REMOTES are remote name patterns passed via repeated `--remote`."
  (interactive
   (list (majutsu-completing-read-multiple
          "Untrack bookmark name(s)/pattern(s)"
          (majutsu--bookmark-remote-name-candidates))
         (majutsu-completing-read-multiple
          "Remote(s)/pattern(s) (empty = all)"
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

;;; Bookmark Transient

;;;###autoload(autoload 'majutsu-bookmark "majutsu-bookmark" nil t)
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
    ("a" "Advance bookmark(s)" majutsu-bookmark-advance
     :description "Advance default selection")
    ("A" "Advance bookmark(s) to revset" majutsu-bookmark-advance-to
     :description "Advance default selection to revset")
    ("p" "Advance name(s)/pattern(s)" majutsu-bookmark-advance-patterns
     :description "Advance name/pattern selection")
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
