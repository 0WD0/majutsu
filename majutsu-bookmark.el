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
(require 'majutsu-remote)

(require 'seq)
(require 'subr-x)

(declare-function majutsu-bookmark-at-point "majutsu-jj" (&optional bookmark-type))
(declare-function majutsu-revision-at-point "majutsu-jj" ())
(declare-function majutsu-edit-changeset "majutsu-edit" (&optional arg))

;;; Section Keymaps

(defvar-keymap majutsu-bookmark-section-map
  :doc "Keymap for `jj-bookmark' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset)

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

(defvar majutsu-bookmark-name-history nil
  "Minibuffer history for exact bookmark-name input.")

(defvar majutsu-bookmark-pattern-history nil
  "Minibuffer history for bookmark name-pattern input.")

(defconst majutsu-bookmark--completion-field-separator (string 31)
  "Separator inserted between bookmark completion fields.")

(defconst majutsu-bookmark--completion-template
  (let ((sep (format "\"%s\"" majutsu-bookmark--completion-field-separator)))
    (concat
     (string-join
      (list "name"
            sep "if(remote, remote, \"\")"
            sep "if(conflict, \"t\", \"\")"
            sep "if(present, \"t\", \"\")"
            sep "if(tracked, \"t\", \"\")"
            sep "if(synced, \"t\", \"\")")
      " ++ ")
     " ++ \"\\n\""))
  "Template used to collect bookmark completion metadata.")

(defun majutsu-bookmark--split-completion-fields (value)
  "Split bookmark completion VALUE.
Use `majutsu-bookmark--completion-field-separator' as the field separator."
  (if (not (stringp value))
      nil
    (let ((start 0)
          (len (length value))
          (sep (aref majutsu-bookmark--completion-field-separator 0))
          out)
      (dotimes (idx len)
        (when (eq (aref value idx) sep)
          (push (substring value start idx) out)
          (setq start (1+ idx))))
      (push (substring value start len) out)
      (nreverse out))))

(defun majutsu-bookmark--parse-completion-bool (value)
  "Parse bookmark completion boolean VALUE."
  (equal value "t"))

(defun majutsu-bookmark--parse-completion-line (line)
  "Parse one bookmark completion LINE into a plist."
  (let* ((fields (majutsu-bookmark--split-completion-fields (or line "")))
         (name (nth 0 fields))
         (remote (nth 1 fields)))
    (when (and (stringp name) (not (string-empty-p name)))
      (list :name name
            :remote (unless (string-empty-p remote) remote)
            :conflict (majutsu-bookmark--parse-completion-bool (nth 2 fields))
            :present (majutsu-bookmark--parse-completion-bool (nth 3 fields))
            :tracked (majutsu-bookmark--parse-completion-bool (nth 4 fields))
            :synced (majutsu-bookmark--parse-completion-bool (nth 5 fields))))))

(defun majutsu-bookmark--completion-entries (&optional directory)
  "Return structured bookmark completion entries for DIRECTORY."
  (let ((default-directory (or directory default-directory))
        entries)
    (dolist (line (majutsu-jj-lines "bookmark" "list" "--quiet" "--all-remotes"
                                    "-T" majutsu-bookmark--completion-template))
      (when-let* ((entry (majutsu-bookmark--parse-completion-line line)))
        (push entry entries)))
    (nreverse entries)))

(defun majutsu-bookmark--add-unique-string (items item)
  "Return ITEMS with ITEM appended unless it is already present."
  (if (member item items)
      items
    (append items (list item))))

(defun majutsu-bookmark-candidate-data (&optional candidates directory)
  "Return completion payload for bookmark CANDIDATES in DIRECTORY."
  (let ((default-directory (or directory default-directory))
        (entries (make-hash-table :test #'equal))
        local-candidates)
    (condition-case _
        (dolist (row (majutsu-bookmark--completion-entries default-directory))
          (let* ((name (plist-get row :name))
                 (remote (plist-get row :remote))
                 (entry (or (gethash name entries)
                            (list :name name
                                  :tracked-remotes nil
                                  :untracked-remotes nil))))
            (when (plist-get row :conflict)
              (setq entry (plist-put entry :conflict t)))
            (if remote
                (setq entry
                      (plist-put entry
                                 (if (plist-get row :tracked)
                                     :tracked-remotes
                                   :untracked-remotes)
                                 (majutsu-bookmark--add-unique-string
                                  (plist-get entry
                                             (if (plist-get row :tracked)
                                                 :tracked-remotes
                                               :untracked-remotes))
                                  remote)))
              (setq entry (plist-put entry :local t))
              (when (plist-get row :synced)
                (setq entry (plist-put entry :synced t)))
              (unless (member name local-candidates)
                (setq local-candidates (append local-candidates (list name)))))
            (puthash name entry entries)))
      (error nil))
    (list :category 'majutsu-bookmark
          :candidates (or candidates local-candidates)
          :entries entries)))

(defun majutsu--bookmark-base-names-from-scope (scope)
  "Return bookmark base names for SCOPE.
SCOPE should be one of the scopes accepted by
`majutsu--get-bookmark-names'.  Any `NAME@REMOTE' refs are normalized to
`NAME'."
  (delete-dups
   (mapcar (lambda (ref)
             (car (majutsu--bookmark-split-remote-ref ref)))
           (majutsu--get-bookmark-names scope))))

(defun majutsu--bookmark-forget-name-candidates ()
  "Return bookmark name candidates for `jj bookmark forget'."
  (delete-dups
   (append (majutsu--get-bookmark-names 'local)
           (majutsu--bookmark-base-names-from-scope 'remote))))

(defun majutsu--bookmark-track-name-candidates ()
  "Return bookmark name candidates for `jj bookmark track'."
  (majutsu--bookmark-base-names-from-scope 'remote-untracked))

(defun majutsu--bookmark-untrack-name-candidates ()
  "Return bookmark name candidates for `jj bookmark untrack'."
  (majutsu--bookmark-base-names-from-scope 'remote-tracked))

(defun majutsu--bookmarks-for-revision (rev &optional bookmark-type)
  "Return bookmark names for REV.
BOOKMARK-TYPE is forwarded to jj's bookmark template fields."
  (let* ((args (append `("show" ,rev "--no-patch" "--ignore-working-copy"
                         "-T" ,(pcase bookmark-type
                                 ('remote "remote_bookmarks")
                                 ('local "local_bookmarks")
                                 (_ "bookmarks")))))
         (lines (apply #'majutsu-jj-lines args))
         (bookmarks (split-string (string-join lines "\n") " " t)))
    (mapcar (lambda (s) (string-remove-suffix "*" s)) bookmarks)))

(defun majutsu--bookmark-patterns-for-revision-at-point (&optional bookmark-type)
  "Return bookmark patterns for the revision at point.
When no revision is available, fall back to the working copy revision @."
  (let ((bookmarks (majutsu--bookmarks-for-revision
                    (or (majutsu-revision-at-point) "@")
                    bookmark-type)))
    (when bookmarks
      (string-join bookmarks ","))))

(defun majutsu-read-bookmark-name (prompt &optional default require-match)
  "Read one exact bookmark name using PROMPT.
DEFAULT is preselected when non-nil.  If REQUIRE-MATCH is non-nil,
require an existing local bookmark name."
  (let ((default (or default (majutsu-bookmark-at-point)))
        (payload (majutsu-bookmark-candidate-data nil default-directory)))
    (majutsu-completing-read-payload prompt payload
                                     nil (or require-match 'any) nil
                                     'majutsu-bookmark-name-history
                                     default 'majutsu-bookmark nil
                                     default-directory)))

(defun majutsu-read-bookmark-names (prompt &optional candidates default require-match)
  "Read exact bookmark names with PROMPT.
CANDIDATES defaults to local bookmark names.  DEFAULT is preselected when
non-nil.  If REQUIRE-MATCH is non-nil, require existing local bookmark
names."
  (let ((default (or default (majutsu-bookmark-at-point)))
        (payload (majutsu-bookmark-candidate-data candidates default-directory)))
    (majutsu-completing-read-multiple-payload
     prompt payload
     nil (or require-match 'any) nil 'majutsu-bookmark-name-history
     default 'majutsu-bookmark nil default-directory)))

(defun majutsu-read-bookmark-pattern (prompt &optional default)
  "Read one bookmark name pattern using PROMPT.
DEFAULT is preselected when non-nil."
  (let ((default (or default
                     (majutsu-bookmark-at-point)
                     (majutsu--bookmark-patterns-for-revision-at-point)))
        (payload (majutsu-bookmark-candidate-data nil default-directory)))
    (majutsu-completing-read-payload prompt payload
                                     nil 'any nil
                                     'majutsu-bookmark-pattern-history
                                     default 'majutsu-bookmark nil
                                     default-directory)))

(defun majutsu-read-bookmark-patterns (prompt &optional _init-input _history candidates default)
  "Read bookmark name patterns with PROMPT.
CANDIDATES defaults to local bookmark names.  DEFAULT defaults to the
bookmark(s) at point."
  (let* ((default (or default
                      (majutsu-bookmark-at-point)
                      (majutsu--bookmark-patterns-for-revision-at-point)))
         (payload (majutsu-bookmark-candidate-data candidates default-directory)))
    (majutsu-completing-read-multiple-payload
     prompt payload
     nil nil nil 'majutsu-bookmark-pattern-history
     default 'majutsu-bookmark nil default-directory)))

;;;###autoload
(defun majutsu-bookmark-create (&optional names)
  "Create bookmarks NAMES at the current contextual revision."
  (interactive (list (majutsu-read-bookmark-names "Bookmark name(s)" nil nil nil)))
  (let ((revset (or (magit-section-value-if 'jj-commit) "@"))
        (names (cond
                ((null names) nil)
                ((stringp names) (list names))
                (t names))))
    (when names
      (majutsu-run-jj "bookmark" "create" names "-r" revset))))

;;;###autoload
(defun majutsu-bookmark-delete (names)
  "Delete bookmarks or bookmark patterns NAMES and propagate on next push."
  (interactive (list (majutsu-read-bookmark-patterns
                      "Delete bookmark(s)/pattern(s) (propagates on push)")))
  (if (null names)
      (message "No bookmark name/pattern provided")
    (when (zerop (majutsu-run-jj "bookmark" "delete" names))
      (message "Deleted bookmark(s): %s" (string-join names ", ")))))

;;;###autoload
(defun majutsu-bookmark-forget (names)
  "Forget bookmarks or bookmark patterns NAMES locally only."
  (interactive (list (majutsu-read-bookmark-patterns
                      "Forget bookmark(s)/pattern(s)"
                      nil nil
                      (majutsu--bookmark-forget-name-candidates)
                      nil)))
  (if (null names)
      (message "No bookmark name/pattern provided")
    (when (zerop (majutsu-run-jj "bookmark" "forget" names))
      (message "Forgot bookmark(s): %s" (string-join names ", ")))))

;;;###autoload
(defun majutsu-bookmark-track ()
  "Track remote bookmark(s)."
  (interactive)
  (let* ((bookmark-patterns (majutsu-read-bookmark-patterns
                             "Track bookmark name(s)/pattern(s)"
                             nil nil
                             (majutsu--bookmark-track-name-candidates)
                             nil))
         (remote-patterns (majutsu-read-remote-patterns
                           "Remote(s)/pattern(s) (empty = all)"
                           (majutsu-remote-names))))
    (if (null bookmark-patterns)
        (message "No bookmark name/pattern provided")
      (when (zerop (majutsu-run-jj "bookmark" "track"
                                   bookmark-patterns
                                   (majutsu--bookmark--remote-args remote-patterns)))
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

(defun majutsu-bookmark--line-name (line)
  "Return the bookmark name parsed from LINE."
  (let* ((raw (string-trim (substring-no-properties line)))
         (token (car (split-string raw "[ \t]+" t))))
    (when token
      (string-remove-suffix ":" token))))

(defun majutsu-bookmark--wash-list (_args)
  "Wash `jj bookmark list' output into bookmark sections."
  (let ((count 0))
    (magit-wash-sequence
     (lambda ()
       (let* ((line (buffer-substring (line-beginning-position)
                                      (line-end-position)))
              (trimmed (string-trim (substring-no-properties line)))
              (name (and (not (string-empty-p trimmed))
                         (majutsu-bookmark--line-name line))))
         (delete-region (line-beginning-position)
                        (min (point-max) (1+ (line-end-position))))
         (when name
           (setq count (1+ count))
           (magit-insert-section (jj-bookmark name t)
             (magit-insert-heading line)))
         t)))
    (if (zerop count)
        (magit-cancel-section)
      (insert "\n"))))

(defun majutsu-bookmark-list-refresh-buffer ()
  "Refresh the bookmark list buffer."
  (majutsu--assert-mode 'majutsu-bookmark-list-mode)
  (magit-insert-section (bookmark-list)
    (majutsu-jj-wash #'majutsu-bookmark--wash-list nil
      (append '("bookmark" "list" "--quiet")
              (and majutsu-bookmark--list-all '("--all-remotes"))))))

(defvar-keymap majutsu-bookmark-list-mode-map
  :doc "Keymap for `majutsu-bookmark-list-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-bookmark-list-mode majutsu-mode "Majutsu Bookmarks"
  "Major mode for viewing jj bookmarks."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

;;;###autoload
(defun majutsu-read-bookmarks (prompt &optional init-input history)
  "Read bookmark name patterns with PROMPT.
This is a compatibility wrapper around `majutsu-read-bookmark-patterns'."
  (majutsu-read-bookmark-patterns prompt init-input history))

;;;###autoload
(defun majutsu-bookmark-advance (&optional names revset)
  "Advance bookmark name patterns NAMES to REVSET.
When NAMES is nil, use jj's configured default selection.  When REVSET
is nil, use jj's configured default target revset.  Interactively, this
uses both defaults.

NAMES may be a string or a list of strings.  Use
`majutsu-bookmark-advance-to' and `majutsu-bookmark-advance-patterns' as
convenience wrappers for the common interactive forms."
  (interactive)
  (let ((names (cond
                ((null names) nil)
                ((stringp names) (list names))
                (t names))))
    (majutsu-run-jj "bookmark" "advance" names (and revset (list "-t" revset)))))

;;;###autoload
(defun majutsu-bookmark-advance-to (revset)
  "Advance bookmarks using jj's default selection to REVSET."
  (interactive (list (majutsu-read-revset "Advance to revset")))
  (majutsu-bookmark-advance nil revset))

;;;###autoload
(defun majutsu-bookmark-advance-patterns (names)
  "Advance bookmark name patterns NAMES using jj's default target revset."
  (interactive (list (majutsu-read-bookmark-patterns
                      "Advance bookmark name(s)/pattern(s)")))
  (if names
      (majutsu-bookmark-advance names)
    (message "No bookmark name/pattern provided")))

(defun majutsu--bookmark-move (names commit &optional allow-backwards)
  "Internal helper to move bookmark(s) NAMES to COMMIT.
When ALLOW-BACKWARDS is non-nil, include `--allow-backwards'."
  (when names
    (when (zerop (majutsu-run-jj "bookmark" "move" (and allow-backwards '("--allow-backwards")) "-t" commit names))
      (message (if allow-backwards
                   "Moved bookmark(s) (allow backwards) to %s: %s"
                 "Moved bookmark(s) to %s: %s")
               commit (string-join names ", ")))))

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
   (let* ((old (majutsu-read-bookmark-name "Rename bookmark" nil t))
          (new (majutsu-read-bookmark-name (format "New name for %s" old))))
     (list old new)))
  (when (and (not (string-empty-p old)) (not (string-empty-p new)))
    (when (zerop (majutsu-run-jj "bookmark" "rename" old new))
      (message "Renamed bookmark '%s' -> '%s'" old new))))

;;;###autoload
(defun majutsu-bookmark-set (names commit)
  "Create or update bookmarks NAMES to point to COMMIT."
  (interactive
   (let* ((at (or (magit-section-value-if 'jj-commit) "@"))
          (names (majutsu-read-bookmark-names "Set bookmark(s)"))
          (rev (majutsu-read-revset "Target revision" at)))
     (list names rev)))
  (when (and names (zerop (majutsu-run-jj "bookmark" "set" names (list "-r" commit))))
    (message "Set bookmark(s) to %s: %s" commit (string-join names ", "))))

;;;###autoload
(defun majutsu-bookmark-untrack (bookmarks &optional remotes)
  "Stop tracking remote bookmark(s).

BOOKMARKS are bookmark name patterns (glob/exact/regex/substring).
REMOTES are remote name patterns passed via repeated `--remote`."
  (interactive
   (list (majutsu-read-bookmark-patterns
          "Untrack bookmark name(s)/pattern(s)"
          nil nil
          (majutsu--bookmark-untrack-name-candidates)
          nil)
         (majutsu-read-remote-patterns
          "Remote(s)/pattern(s) (empty = all)"
          (majutsu-remote-names))))
  (defvar crm-separator)
  (let* ((remotes (seq-filter (lambda (s) (not (string-empty-p s))) (or remotes '()))))
    (when bookmarks
      (when (zerop (majutsu-run-jj "bookmark" "untrack" bookmarks (majutsu--bookmark--remote-args remotes)))
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
