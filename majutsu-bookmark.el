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
(require 'majutsu-row)
(require 'majutsu-template)

(require 'cl-lib)
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

(defvar majutsu-bookmark--compiled-template-cache nil
  "Cached compiled `jj bookmark list' row metadata.")

(defun majutsu-bookmark--invalidate-list-template (&rest _)
  "Invalidate the cached bookmark-list template."
  (setq majutsu-bookmark--compiled-template-cache nil))

(defmacro majutsu-bookmark-define-template (name template doc)
  "Define a bookmark-list template variable NAME with TEMPLATE and DOC."
  (declare (indent 1) (debug t) (doc-string 3))
  (let ((var-name (intern (format "majutsu-bookmark-list-template-%s" name))))
    `(progn
       (defcustom ,var-name ,template
         ,doc
         :type 'sexp
         :group 'majutsu
         :set (lambda (symbol value)
                (set-default symbol value)
                (setq majutsu-bookmark--compiled-template-cache nil)))
       (when (fboundp 'add-variable-watcher)
         (add-variable-watcher ',var-name
                               #'majutsu-bookmark--invalidate-list-template)))))

(majutsu-bookmark-define-template commit-summary
  [:method [:self] :format_commit_summary_with_refs ""]
  "Template used for bookmark-list commit summaries.")

(majutsu-bookmark-define-template heading
  [:if [:remote]
      [:if [:tracked]
          ["  " [:separate " "
                           [:majutsu-bookmark-list-name]
                           [:if [:present] [:majutsu-bookmark-list-tracking]]
                           [:if [:present]
                               [:majutsu-bookmark-list-target-summary]
                             "(not created yet)"]]]
        [:separate " " [:majutsu-bookmark-list-name] [:majutsu-bookmark-list-target-summary]]]
    [:separate " "
               [:majutsu-bookmark-list-name]
               [:if [:present]
                   [:majutsu-bookmark-list-target-summary]
                 "(deleted)"]]]
  "Template used for bookmark-list headings.")

(majutsu-template-defkeyword majutsu-bookmark-list-commit-summary Commit
  (:returns Template :doc "User-customizable commit summary for bookmark list entries.")
  majutsu-bookmark-list-template-commit-summary)

(majutsu-template-defkeyword majutsu-bookmark-list-name CommitRef
  (:returns Template :doc "Default bookmark-list ref name.")
  [:label "bookmark"
          [:if [:remote]
              [:if [:tracked]
                  ["@" [:remote]]
                [[:name] "@" [:remote]]]
            [:name]]])

(majutsu-template-defmethod majutsu-bookmark-list-distance-part SizeHint
  ((prefix Template))
  (:returns Template :doc "One compact tracked-distance fragment.")
  `[:if [:not [:zero]]
       [,prefix
        [:if [:exact] [:exact] [[:lower] "+"]]]])

(majutsu-template-defkeyword majutsu-bookmark-list-tracking CommitRef
  (:returns Template :doc "Compact tracked-remote distance summary.")
  [:if [:tracking_present]
      [:surround "(" ")"
                 [:separate "/"
                            [:method [:tracking_ahead_count]
                             :majutsu-bookmark-list-distance-part "+"]
                            [:method [:tracking_behind_count]
                             :majutsu-bookmark-list-distance-part "-"]]]])

(majutsu-template-defkeyword majutsu-bookmark-list-target-summary CommitRef
  (:returns Template :doc "Default bookmark-list target summary.")
  [:if [:conflict]
      [:label "conflict" "(conflicted):"]
    [:method [:normal_target] :majutsu-bookmark-list-commit-summary]])

(defun majutsu-bookmark--row-empty-to-nil (value &optional _ctx)
  "Return nil for empty bookmark row VALUE."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun majutsu-bookmark--row-bool (value &optional _ctx)
  "Decode bookmark row boolean VALUE."
  (and (stringp value)
       (member value '("t" "true" "1"))
       t))

(defun majutsu-bookmark--row-lines (value &optional _ctx)
  "Decode newline-separated bookmark row VALUE into plain strings."
  (when (and (stringp value) (not (string-empty-p value)))
    (mapcar #'substring-no-properties (split-string value "\n" t))))

(defcustom majutsu-bookmark-list-columns
  '((:field heading :module heading
     :template majutsu-bookmark-list-template-heading :face t)
    (:field conflict-details :module body
     :template
     [:if [:conflict]
         [:separate "\x1f"
                    [:method [:removed_targets]
                     :map [:lambda (target)
                                   [:concat "  - " [:majutsu-bookmark-list-commit-summary]]]
                     :join "\x1f"]
                    [:method [:added_targets]
                     :map [:lambda (target)
                                   [:concat "  + " [:majutsu-bookmark-list-commit-summary]]]
                     :join "\x1f"]]
       ""]
     :face t)
    (:field name :module metadata :template [:name] :face nil)
    (:field remote :module metadata :template [:remote] :face nil
     :post majutsu-bookmark--row-empty-to-nil)
    (:field tracked :module metadata
     :template [:if [:tracked] "t" ""] :face nil
     :post majutsu-bookmark--row-bool)
    (:field removed-target-ids :module metadata
     :template [:method [:removed_targets]
                :map [:lambda (target) [:commit_id]]
                :join "\x1f"]
     :face nil :post majutsu-bookmark--row-lines)
    (:field added-target-ids :module metadata
     :template [:method [:added_targets]
                :map [:lambda (target) [:commit_id]]
                :join "\x1f"]
     :face nil :post majutsu-bookmark--row-lines))
  "Flat row columns emitted by `jj bookmark list'."
  :type 'sexp
  :group 'majutsu
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq majutsu-bookmark--compiled-template-cache nil)))

(when (fboundp 'add-variable-watcher)
  (add-variable-watcher 'majutsu-bookmark-list-columns
                        #'majutsu-bookmark--invalidate-list-template))

(defun majutsu-bookmark--row-ref-section-value (entry)
  "Return section value for bookmark ref ENTRY."
  (let ((name (majutsu-row-column entry 'name))
        (remote (majutsu-row-column entry 'remote)))
    (if remote
        (concat name "@" remote)
      name)))

(defun majutsu-bookmark--target-heading (heading)
  "Return conflict target HEADING without bookmark row identity properties."
  (setq heading (copy-sequence heading))
  (remove-list-of-text-properties
   0 (length heading) majutsu-row--ui-properties heading)
  heading)

(defun majutsu-bookmark--row-profile ()
  "Return row profile for `majutsu-bookmark-list'."
  (majutsu-row-make-profile
   :name 'bookmark-list
   :self-type 'CommitRef
   :columns-var 'majutsu-bookmark-list-columns
   :entry-id-function #'majutsu-bookmark--row-ref-section-value
   :section-class 'jj-bookmark
   :section-value-function #'majutsu-bookmark--row-ref-section-value
   :section-hide t
   :show-child-count nil))

(defun majutsu-bookmark--compile-list-columns ()
  "Compile bookmark-list columns into row metadata."
  (majutsu-row-compile (majutsu-bookmark--row-profile)))

(defun majutsu-bookmark--ensure-list-template ()
  "Return cached row metadata for `jj bookmark list'."
  (or majutsu-bookmark--compiled-template-cache
      (setq majutsu-bookmark--compiled-template-cache
            (majutsu-bookmark--compile-list-columns))))

(defun majutsu-bookmark--list-template ()
  "Return cached row template used by `jj bookmark list'."
  (plist-get (majutsu-bookmark--ensure-list-template) :template))

(defun majutsu-bookmark--tracked-child-p (local entry)
  "Return non-nil when ENTRY is a tracked child of LOCAL."
  (and local
       (null (majutsu-row-column local 'remote))
       (majutsu-row-column entry 'remote)
       (majutsu-row-column entry 'tracked)
       (equal (majutsu-row-column local 'name)
              (majutsu-row-column entry 'name))))

(defun majutsu-bookmark--group-list-entries (entries)
  "Group flat bookmark ENTRIES into roots with tracked remotes."
  (let (groups current)
    (dolist (entry entries)
      (if (and current
               (majutsu-bookmark--tracked-child-p
                (plist-get current :root) entry))
          (let ((root (plist-get current :root)))
            (plist-put entry :parent root)
            (setf (plist-get current :tracked-remotes)
                  (append (plist-get current :tracked-remotes) (list entry))))
        (plist-put entry :parent nil)
        (setq current (list :root entry :tracked-remotes nil))
        (push current groups)))
    (nreverse groups)))

(defun majutsu-bookmark--conflict-targets (entry compiled)
  "Return conflict target rows for ENTRY, or `:invalid'.
Each target row is a cons of its full commit id and rendered heading.
Return nil when ENTRY has no conflict target data."
  (let* ((body (majutsu-row-render-body entry compiled t))
         (removed (majutsu-row-column entry 'removed-target-ids))
         (added (majutsu-row-column entry 'added-target-ids))
         (ids (append removed added))
         (lines (and body
                     (not (string-empty-p body))
                     (majutsu-row-split-by-separator body "\n"))))
    (cond
     ((and (null ids) (null lines)) nil)
     ((and (= (length ids) (length lines))
           (cl-loop for line in lines
                    for index from 0
                    for marker = (if (< index (length removed))
                                     "  - "
                                   "  + ")
                    always (string-prefix-p
                            marker (substring-no-properties line))))
      (cl-mapcar #'cons ids lines))
     (t :invalid))))

(defun majutsu-bookmark--insert-conflict-target (target)
  "Insert one bookmark conflict TARGET as a commit section."
  (magit-insert-section (jj-commit (car target) t)
    (magit-insert-heading
      (majutsu-bookmark--target-heading (cdr target)))))

(defun majutsu-bookmark--insert-list-entry
    (entry compiled &optional tracked-remotes)
  "Insert bookmark ENTRY and TRACKED-REMOTES using COMPILED."
  (let* ((targets (majutsu-bookmark--conflict-targets entry compiled))
         (valid-targets (and (not (eq targets :invalid)) targets))
         (body-inserter
          (and (or valid-targets tracked-remotes)
               (lambda ()
                 (mapc #'majutsu-bookmark--insert-conflict-target valid-targets)
                 (dolist (remote tracked-remotes)
                   (majutsu-bookmark--insert-list-entry remote compiled))))))
    (majutsu-row-insert-entry entry compiled body-inserter valid-targets)))

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a dedicated buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (majutsu-setup-buffer #'majutsu-bookmark-list-mode nil
    :buffer "*Majutsu Bookmarks*"
    (majutsu-bookmark--list-all (and all t))))

(defun majutsu-bookmark--wash-list (_args)
  "Wash structured `jj bookmark list' row output into bookmark sections."
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (parsed (majutsu-row-read-buffer compiled))
         (entries (plist-get parsed :entries))
         (groups (majutsu-bookmark--group-list-entries entries))
         (inhibit-read-only t))
    (majutsu-row-report-diagnostics (plist-get parsed :diagnostics))
    (delete-region (point-min) (point-max))
    (if (null entries)
        (magit-cancel-section)
      (dolist (group groups)
        (majutsu-bookmark--insert-list-entry
         (plist-get group :root) compiled (plist-get group :tracked-remotes)))
      (majutsu-row-set-buffer-data
       compiled entries (mapcar (lambda (group) (plist-get group :root))
                                groups))
      (insert "\n"))))

(defun majutsu-bookmark-list-refresh-buffer ()
  "Refresh the bookmark list buffer."
  (majutsu--assert-mode 'majutsu-bookmark-list-mode)
  (magit-insert-section (bookmark-list)
    (majutsu-jj-wash #'majutsu-bookmark--wash-list nil
      (append '("bookmark" "list" "--quiet")
              (and majutsu-bookmark--list-all '("--all-remotes"))
              (list "-T" (majutsu-bookmark--list-template))))))

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
