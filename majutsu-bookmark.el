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
(require 'majutsu-ref)
(require 'majutsu-remote)
(require 'majutsu-template)

(require 'seq)
(require 'subr-x)

(declare-function majutsu-bookmark-at-point "majutsu-jj" (&optional bookmark-type))
(declare-function majutsu-revision-at-point "majutsu-jj" ())
(declare-function majutsu-edit-changeset "majutsu-edit" (&optional arg))

;;; Section Keymaps

(defvar-keymap majutsu-bookmark-section-map
  :doc "Keymap for `jj-bookmark' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset
  "<remap> <majutsu-delete-thing>" #'majutsu-bookmark-delete)

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
  (majutsu-ref-names 'bookmark scope))

(defvar majutsu-bookmark-name-history nil
  "Minibuffer history for exact bookmark-name input.")

(defvar majutsu-bookmark-pattern-history nil
  "Minibuffer history for bookmark name-pattern input.")

(defconst majutsu-bookmark--completion-field-separator
  majutsu-ref--completion-field-separator
  "Separator inserted between bookmark completion fields.")

(defconst majutsu-bookmark--completion-template
  majutsu-ref--completion-template
  "Template used to collect bookmark completion metadata.")

(defun majutsu-bookmark-candidate-data (&optional candidates directory)
  "Return completion payload for bookmark CANDIDATES in DIRECTORY."
  (majutsu-ref-candidate-data 'bookmark candidates directory))

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
    (majutsu-ref-read 'bookmark prompt payload
                      'majutsu-bookmark-name-history
                      default (or require-match 'any)
                      default-directory)))

(defun majutsu-read-bookmark-names (prompt &optional candidates default require-match)
  "Read exact bookmark names with PROMPT.
CANDIDATES defaults to local bookmark names.  DEFAULT is preselected when
non-nil.  If REQUIRE-MATCH is non-nil, require existing local bookmark
names."
  (let ((default (or default (majutsu-bookmark-at-point)))
        (payload (majutsu-bookmark-candidate-data candidates default-directory)))
    (majutsu-ref-read-multiple 'bookmark prompt payload
                               'majutsu-bookmark-name-history
                               default (or require-match 'any)
                               default-directory)))

(defun majutsu-read-bookmark-pattern (prompt &optional default)
  "Read one bookmark name pattern using PROMPT.
DEFAULT is preselected when non-nil."
  (let ((default (or default
                     (majutsu-bookmark-at-point)
                     (majutsu--bookmark-patterns-for-revision-at-point)))
        (payload (majutsu-bookmark-candidate-data nil default-directory)))
    (majutsu-ref-read 'bookmark prompt payload
                      'majutsu-bookmark-pattern-history
                      default 'any default-directory)))

(defun majutsu-read-bookmark-patterns (prompt &optional _init-input _history candidates default)
  "Read bookmark name patterns with PROMPT.
CANDIDATES defaults to local bookmark names.  DEFAULT defaults to the
bookmark(s) at point."
  (let* ((default (or default
                      (majutsu-bookmark-at-point)
                      (majutsu--bookmark-patterns-for-revision-at-point)))
         (payload (majutsu-bookmark-candidate-data candidates default-directory)))
    (majutsu-ref-read-multiple 'bookmark prompt payload
                               'majutsu-bookmark-pattern-history
                               default nil default-directory)))

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
  (interactive
   (let ((names (majutsu-read-bookmark-patterns
                 "Delete bookmark(s)/pattern(s) (propagates on push)")))
     (when names
       (unless (majutsu-confirm
                'bookmark-delete
                (format "Delete bookmark(s) %s and propagate on next push? "
                        (string-join names ", ")))
         (user-error "Delete canceled")))
     (list names)))
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

(defconst majutsu-bookmark--list-field-separator (string 30)
  "Field separator used by the bookmark list machine template.")

(defconst majutsu-bookmark--list-record-separator (string 29)
  "Record separator used by the bookmark list machine template.")

(defun majutsu-bookmark--list-record-form (&rest fields)
  "Return a template form for one machine-readable record with FIELDS.
This uses `join' rather than `separate' because empty fields must still
occupy their transport slots."
  `[:concat
    [:join ,majutsu-bookmark--list-field-separator ,@fields]
    ,majutsu-bookmark--list-record-separator])

(defun majutsu-bookmark--list-name-form ()
  "Return jj's bookmark-list ref-name template form."
  '[:if [:remote]
       [:if [:tracked]
           [:label "bookmark" ["@" [:remote]]]
         [:label "bookmark" [[:name] "@" [:remote]]]]
     [:label "bookmark" [:name]]])

(defun majutsu-bookmark--list-distance-part-form (label count)
  "Return one tracking-distance template fragment for LABEL and COUNT."
  `[:if [:not [:method ,count :zero]]
       [:if [:method ,count :exact]
           [,label " by " [:method ,count :exact] " commits"]
         [,label " by at least " [:method ,count :lower] " commits"]]])

(defun majutsu-bookmark--list-distances-form ()
  "Return jj's tracked-remote distance template form."
  `[:if [:tracking_present]
       [:surround "(" ")"
                  [:separate ", "
                             ,(majutsu-bookmark--list-distance-part-form
                               "ahead" '[:tracking_ahead_count])
                             ,(majutsu-bookmark--list-distance-part-form
                               "behind" '[:tracking_behind_count])]]])

(defun majutsu-bookmark--list-targets-form ()
  "Return jj's bookmark target summary template form."
  '[:if [:conflict]
       [" " [:label "conflict" "(conflicted)"] ":"]
     [": " [:call 'format_commit_summary_with_refs [:normal_target] ""]]])

(defun majutsu-bookmark--list-heading-form ()
  "Return jj-compatible bookmark-list heading template form."
  (let ((name (majutsu-bookmark--list-name-form))
        (targets (majutsu-bookmark--list-targets-form)))
    `[:if [:remote]
         [:if [:tracked]
             ["  " [:separate " " ,name
                              [:if [:present] ,(majutsu-bookmark--list-distances-form)]]
              [:if [:present] ,targets " (not created yet)"]]
           [,name ,targets]]
       [,name [:if [:present] ,targets " (deleted)"]]]))

(defun majutsu-bookmark--list-target-form (marker commit &optional ref-depth)
  "Return a conflict-target record template form.
MARKER is `-' or `+'.  COMMIT is a Commit-valued template form.  REF-DEPTH
selects the surrounding CommitRef `:self' binding."
  (let ((ref (if ref-depth `[:self ,ref-depth] '[:self])))
    (majutsu-bookmark--list-record-form
     "target"
     `[:method ,ref :name]
     `[:if [:method ,ref :remote] [:method ,ref :remote] ""]
     marker
     `[:method ,commit :commit_id]
     `["  " ,marker " " [:call 'format_commit_summary_with_refs ,commit ""]])))

(defun majutsu-bookmark--list-target-records-form ()
  "Return template form emitting conflict target records for the current ref."
  `[:if [:conflict]
       [[:map-join "" [:removed_targets] c
                   ,(majutsu-bookmark--list-target-form "-" 'c 1)]
        [:map-join "" [:added_targets] c
                   ,(majutsu-bookmark--list-target-form "+" 'c 1)]]
     ""])

(defconst majutsu-bookmark--list-template
  (majutsu-template-compile
   `[:concat
     ,(majutsu-bookmark--list-record-form
       "ref"
       [:name]
       [:if [:remote] [:remote] ""]
       [:if [:tracked] "t" ""]
       (majutsu-bookmark--list-heading-form))
     ,(majutsu-bookmark--list-target-records-form)]
   'CommitRef)
  "Template used to render and parse `jj bookmark list' output.

It emits one ref record containing exact section metadata plus jj-rendered
heading text, followed by conflict target records containing commit ids and
jj-rendered target lines.  Majutsu only sectionizes the records.")

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a dedicated buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (majutsu-setup-buffer #'majutsu-bookmark-list-mode nil
    :buffer "*Majutsu Bookmarks*"
    (majutsu-bookmark--list-all (and all t))))

(defun majutsu-bookmark--parse-list-record (record)
  "Parse one structured bookmark list RECORD into a plist."
  (let ((kind (majutsu--field-string
               (car (majutsu--split-fields
                     record majutsu-bookmark--list-field-separator 2)))))
    (pcase kind
      ("ref"
       (let* ((fields (majutsu--split-fields
                       record majutsu-bookmark--list-field-separator 5))
              (name (majutsu--field-string (nth 1 fields)))
              (remote (majutsu--field-string (nth 2 fields))))
         (unless (string-empty-p name)
           (list :kind 'ref
                 :name name
                 :remote (and (not (string-empty-p remote)) remote)
                 :tracked (majutsu--field-bool-p (nth 3 fields))
                 :heading (or (nth 4 fields) "")
                 :targets nil))))
      ("target"
       (let* ((fields (majutsu--split-fields
                       record majutsu-bookmark--list-field-separator 6))
              (name (majutsu--field-string (nth 1 fields)))
              (remote (majutsu--field-string (nth 2 fields))))
         (unless (string-empty-p name)
           (list :kind 'target
                 :name name
                 :remote (and (not (string-empty-p remote)) remote)
                 :commit-id (majutsu--field-string (nth 4 fields))
                 :line (or (nth 5 fields) ""))))))))

(defun majutsu-bookmark--same-ref-p (entry target)
  "Return non-nil when TARGET belongs to ENTRY."
  (and (equal (plist-get entry :name) (plist-get target :name))
       (equal (plist-get entry :remote) (plist-get target :remote))))

(defun majutsu-bookmark--parse-list-output (output)
  "Parse structured `jj bookmark list' OUTPUT into entry plists."
  (let (entries current)
    (dolist (record (majutsu--split-fields
                     output majutsu-bookmark--list-record-separator))
      (when-let* ((item (majutsu-bookmark--parse-list-record record)))
        (pcase (plist-get item :kind)
          ('ref
           (setq current item)
           (push item entries))
          ('target
           (when (and current (majutsu-bookmark--same-ref-p current item))
             (setf (plist-get current :targets)
                   (append (plist-get current :targets) (list item))))))))
    (nreverse entries)))

(defun majutsu-bookmark--entry-ref (entry)
  "Return the exact revision/ref name represented by bookmark ENTRY."
  (let ((name (plist-get entry :name))
        (remote (plist-get entry :remote)))
    (if remote
        (concat name "@" remote)
      name)))

(defun majutsu-bookmark--tracked-child-p (primary entry)
  "Return non-nil when ENTRY should be nested under PRIMARY."
  (and primary
       (plist-get entry :remote)
       (plist-get entry :tracked)
       (not (plist-get primary :remote))
       (equal (plist-get entry :name) (plist-get primary :name))))

(defun majutsu-bookmark--group-list-entries (entries)
  "Group bookmark list ENTRIES into primary entries with tracked children."
  (let (groups current)
    (dolist (entry entries)
      (if (majutsu-bookmark--tracked-child-p (plist-get current :primary) entry)
          (plist-put current :children
                     (append (plist-get current :children) (list entry)))
        (setq current (list :primary entry :children nil))
        (push current groups)))
    (nreverse groups)))

(defun majutsu-bookmark--insert-target (target)
  "Insert one conflicted bookmark TARGET as a commit section."
  (magit-insert-section (jj-commit (plist-get target :commit-id) t)
    (magit-insert-heading (or (plist-get target :line) ""))))

(defun majutsu-bookmark--insert-list-entry (entry &optional children)
  "Insert bookmark list ENTRY and optional CHILDREN as sections."
  (let ((magit-section-show-child-count nil))
    (magit-insert-section (jj-bookmark (majutsu-bookmark--entry-ref entry) t)
      (magit-insert-heading (or (plist-get entry :heading)
                                (majutsu-bookmark--entry-ref entry)))
      (dolist (target (plist-get entry :targets))
        (majutsu-bookmark--insert-target target))
      (dolist (child children)
        (majutsu-bookmark--insert-list-entry child)))))

(defun majutsu-bookmark--wash-list (_args)
  "Wash structured `jj bookmark list' output into bookmark sections."
  (let* ((entries (majutsu-bookmark--parse-list-output
                   (buffer-substring (point-min) (point-max))))
         (groups (majutsu-bookmark--group-list-entries entries))
         (inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (if (null entries)
        (magit-cancel-section)
      (dolist (group groups)
        (majutsu-bookmark--insert-list-entry
         (plist-get group :primary)
         (plist-get group :children)))
      (insert "\n"))))

(defun majutsu-bookmark-list-refresh-buffer ()
  "Refresh the bookmark list buffer."
  (majutsu--assert-mode 'majutsu-bookmark-list-mode)
  (magit-insert-section (bookmark-list)
    (majutsu-jj-wash #'majutsu-bookmark--wash-list nil
      (append '("bookmark" "list" "--quiet")
              (and majutsu-bookmark--list-all '("--all-remotes"))
              (list "-T" majutsu-bookmark--list-template)))))

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
   [("l" "List bookmarks" majutsu-bookmark-list
     :description "Show bookmark list")
    ("c" "Create bookmark" majutsu-bookmark-create
     :description "Create new bookmark")]
   [("a" "Advance bookmark(s)" majutsu-bookmark-advance
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
   [("t" "Track remote" majutsu-bookmark-track
     :description "Track remote bookmark")
    ("u" "Untrack remote" majutsu-bookmark-untrack
     :description "Stop tracking remote")]
   [("d" "Delete bookmark" majutsu-bookmark-delete
     :description "Delete (propagate)")
    ("f" "Forget bookmark" majutsu-bookmark-forget
     :description "Forget (local)")]])

;;; _
(provide 'majutsu-bookmark)
;;; majutsu-bookmark.el ends here
