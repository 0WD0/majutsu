;;; majutsu-bookmark.el --- Bookmark commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
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
(require 'majutsu-jjosh)

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function majutsu-bookmark-at-point "majutsu-jj" (&optional bookmark-type))
(declare-function majutsu-revision-at-point "majutsu-jj" ())
(declare-function majutsu-edit "majutsu-edit" (&optional arg))

;;; Section Keymaps

(defvar-keymap majutsu-bookmark-section-map
  :doc "Keymap for `jj-bookmark' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit
  "<remap> <majutsu-delete-thing>" #'majutsu-bookmark-delete)

;;; majutsu-bookmark
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
  "Return bookmark candidates for this build's `jj bookmark forget'.
The jjosh build also offers exact remote refs."
  (if (majutsu-jjosh t)
      (delete-dups
       (append (majutsu--get-bookmark-names 'local)
               (majutsu--get-bookmark-names 'remote)))
    (majutsu--get-bookmark-names 'local)))

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
  "Forget bookmarks or bookmark patterns NAMES without recording a deletion.
Forgetting a local bookmark untracks corresponding remotes.  In the jjosh
build, NAMES may also contain exact NAME@REMOTE refs to forget until the
next fetch."
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

(defun majutsu-bookmark--track-ref (ref untrack)
  "Track REF, or untrack it when UNTRACK is non-nil.
REF is one exact NAME@REMOTE, not a bookmark or remote pattern."
  (pcase-let ((`(,name . ,remote) (majutsu--bookmark-split-remote-ref ref)))
    (unless (and remote (not (equal remote "git")))
      (user-error "Expected a network remote bookmark"))
    (majutsu-start-jj (list "bookmark" (if untrack "untrack" "track")
                            (concat "exact:" name) "--remote" (concat "exact:" remote)))))

(defun majutsu-bookmark-track-ref (ref)
  "Track one exact remote bookmark REF."
  (interactive (list (majutsu-read-string "Remote bookmark")))
  (majutsu-bookmark--track-ref ref nil))

(defun majutsu-bookmark-untrack-ref (ref)
  "Untrack one exact remote bookmark REF."
  (interactive (list (majutsu-read-string "Remote bookmark")))
  (majutsu-bookmark--track-ref ref t))

(majutsu-jjosh
  (defun majutsu-bookmark-forget-ref (ref)
    "Forget one exact remote bookmark REF using the jjosh extension.
REF is one exact NAME@REMOTE, including Git-tracking NAME@git."
    (interactive (list (majutsu-read-string "Remote bookmark")))
    (pcase-let ((`(,name . ,remote) (majutsu--bookmark-split-remote-ref ref)))
      (unless remote
        (user-error "Expected a remote bookmark"))
      (majutsu-start-jj (list "bookmark" "forget" (concat name "@" remote))))))

;;; Bookmark list

(defvar-local majutsu-bookmark--list-remotes nil
  "Git remotes for the current remote list refresh.
Each remote is a plist with `:name', `:value', `:url' and `:push-url';
`:value' is the name `jj git remote' accepts.  The jjosh build keeps only
the remotes of the repository scope here.")

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

(majutsu-jjosh
  (eval-and-compile
    (majutsu-template-define-type
     'ProjectRef
     :doc "Registered jjosh project."
     :converts '((Boolean . no) (Serialize . yes) (Template . yes))))
  (majutsu-template-defkeyword id ProjectRef
    (:returns String :doc "Stable project identifier (jjosh)."))
  (majutsu-template-defkeyword project CommitRef
    (:returns (:option ProjectRef)
     :doc "Project owning this reference's registered label (jjosh)."))
  (majutsu-template-defkeyword unscoped_name CommitRef
    (:returns RefSymbol :doc "Name without its project label (jjosh)."))
  (majutsu-template-defkeyword tracking_content_synced CommitRef
    (:returns Boolean
     :doc "Whether the remote and local ref deliver the same tree (jjosh).")))

(majutsu-jjosh
  (majutsu-template-defkeyword majutsu-bookmark-list-content-synced CommitRef
    (:returns Boolean
     :doc "Whether a project ref differs from its remote only in history.")
    ;; Repository history is the content of a repository-scope ref: there
    ;; the distances are exact, so only a project ref is compared by tree.
    [:and [:project] [:tracking_content_synced]]))

(majutsu-template-defkeyword majutsu-bookmark-list-commit-summary Commit
  (:returns Template :doc "User-customizable commit summary for bookmark list entries.")
  majutsu-bookmark-list-template-commit-summary)

(majutsu-template-defkeyword majutsu-bookmark-list-name CommitRef
  (:returns Template
   :doc "Bookmark name; the jjosh build leaves out the project label.")
  `[:label "bookmark" ,(or (majutsu-jjosh [:unscoped_name]) [:name])])

(majutsu-template-defmethod majutsu-bookmark-list-distance-part SizeHint
  ((prefix Template))
  (:returns Template :doc "One compact tracked-distance fragment.")
  `[:if [:not [:zero]]
       [,prefix
        [:if [:exact] [:exact] ["≥" [:lower]]]]])

(majutsu-template-defkeyword majutsu-bookmark-list-tracking CommitRef
  (:returns Template :doc "Tracking state and distances from the local perspective.")
  (let ((distances
         [:separate " "
                    [:label "diff added"
                            [:method [:tracking_behind_count]
                             :majutsu-bookmark-list-distance-part "↑"]]
                    [:label "diff modified"
                            [:method [:tracking_ahead_count]
                             :majutsu-bookmark-list-distance-part "↓"]]]))
    `[:if [:remote]
         [:if [:tracked]
             [:if [:tracking_present]
                 [:if [:synced]
                     [:label "elided" "≡"]
                   ,(or (majutsu-jjosh
                          `[:if [:majutsu-bookmark-list-content-synced]
                               [:label "elided" "≈"]
                             ,distances])
                        distances)]
               [:label "warning heading" "×"]]
           [:label "elided" "○"]]]))

(majutsu-template-defkeyword majutsu-bookmark-list-target-summary CommitRef
  (:returns Template :doc "Default bookmark-list target summary.")
  [:if [:present]
      [:if [:conflict]
          [:label "conflict" "(conflicted):"]
        [:method [:normal_target] :majutsu-bookmark-list-commit-summary]]
    [:if [:remote] "(not created yet)" "(deleted)"]])

(defconst majutsu-bookmark--tracking-help
  (concat "≡ synced; "
          (majutsu-jjosh "≈ same content, different history; ")
          "○ untracked; ↑ local-only commits; ↓ remote-only commits; "
          "×: deleted locally, still on the remote; ≥: at least this many commits")
  "Legend for the tracking states shown in the bookmark list.")

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
  `((:field ref :module heading
     :template [:majutsu-bookmark-list-name] :face t)
    ;; A remote bookmark shows `ref' under its remote and `at-remote' under
    ;; its local bookmark; Majutsu blanks the other one.
    (:field at-remote :module heading
     :template [:if [:remote] [:label "bookmark" ["@" [:remote]]]] :face t)
    ;; Majutsu fills this column on local rows with the state of their remotes.
    (:field remotes :module heading :template "" :face t)
    (:field tracking :module heading
     :template [:majutsu-bookmark-list-tracking] :face t)
    (:field summary :module heading
     :template [:majutsu-bookmark-list-target-summary] :face t)
    (:field conflict-details :module body
     :template
     [:if [:conflict]
         [:separate "\n"
                    [:method [:removed_targets]
                     :map [:lambda (target)
                                   [:concat "  - " [:majutsu-bookmark-list-commit-summary]]]
                     :join "\n"]
                    [:method [:added_targets]
                     :map [:lambda (target)
                                   [:concat "  + " [:majutsu-bookmark-list-commit-summary]]]
                     :join "\n"]]
       ""]
     :face t)
    (:field name :module metadata :template [:name] :face nil)
    (:field remote :module metadata :template [:remote] :face nil
     :post majutsu-bookmark--row-empty-to-nil)
    (:field tracked :module metadata
     :template [:if [:tracked] "t" ""] :face nil
     :post majutsu-bookmark--row-bool)
    (:field synced :module metadata
     :template [:if [:synced] "t" ""] :face nil
     :post majutsu-bookmark--row-bool)
    ,@(majutsu-jjosh
        '((:field content-synced :module metadata
           :template [:if [:majutsu-bookmark-list-content-synced] "t" ""] :face nil
           :post majutsu-bookmark--row-bool)
          (:field project :module metadata
           :template [:if [:project] [:method [:project] :id] ""] :face nil
           :post majutsu-bookmark--row-empty-to-nil)))
    ;; A deleted bookmark has one absent target, which has no commit id.
    (:field removed-target-ids :module metadata
     :template [:if [:conflict]
                   [:method [:removed_targets]
                    :map [:lambda (target) [:commit_id]]
                    :join "\n"]
                 ""]
     :face nil :post majutsu-bookmark--row-lines)
    (:field added-target-ids :module metadata
     :template [:if [:conflict]
                   [:method [:added_targets]
                    :map [:lambda (target) [:commit_id]]
                    :join "\n"]
                 ""]
     :face nil :post majutsu-bookmark--row-lines))
  "Flat row columns emitted by `jj bookmark list'.
The bookmark and remote lists both arrange these rows by the `name',
`remote', `tracked' and `synced' metadata fields, and by `project' in
the jjosh build."
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

;;;; Rows

;; jj lists refs flat.  The bookmark list puts each local bookmark on one row
;; that carries the state of its remotes, with the remote bookmarks of that
;; name as its children.  The remote list puts every remote bookmark under
;; its remote, including those that exist nowhere locally.

(defun majutsu-bookmark--git-entry-p (entry)
  "Return non-nil if ENTRY is a ref of the backing Git repository."
  (equal (majutsu-row-column entry 'remote) "git"))

(defun majutsu-bookmark--network-entry-p (entry)
  "Return non-nil if ENTRY is a bookmark on a configured remote."
  (and (majutsu-row-column entry 'remote)
       (not (majutsu-bookmark--git-entry-p entry))))

(defun majutsu-bookmark--local-rows (entries)
  "Return one cons (LOCAL . REMOTES) of row ENTRIES per local bookmark.
REMOTES are the remote bookmarks of the same name."
  ;; jj lists a local bookmark immediately before its remote bookmarks.
  (let (rows row)
    (dolist (entry entries)
      (cond
       ((null (majutsu-row-column entry 'remote))
        (setq row (list entry))
        (push row rows))
       ((not (equal (majutsu-row-column entry 'name)
                    (majutsu-row-column (car row) 'name))))
       ;; The Git mirror only matters when it disagrees with jj.
       ((and (majutsu-bookmark--git-entry-p entry)
             (majutsu-row-column entry 'synced)))
       (t (nconc row (list entry)))))
    (nreverse rows)))

;;;; Rendering

(defconst majutsu-bookmark--list-name-width 40
  "Widest bookmark name the list pads other names to.")

(defconst majutsu-bookmark--list-remotes-width 24
  "Widest remote summary the list pads other summaries to.")

(defconst majutsu-bookmark--list-tracking-width 16
  "Widest tracking state the lists pad other states to.")

(defun majutsu-bookmark--insert-heading (indent heading)
  "Insert HEADING as the current section's heading, displayed after INDENT.
Like row graph prefixes, the indentation is not part of the buffer text."
  (let ((start (point)))
    (magit-insert-heading heading)
    (majutsu-row-apply-line-prefix-span start (point) indent)))

(defun majutsu-bookmark--align-column (entries compiled field limit)
  "Pad FIELD of ENTRIES to their widest text that is no wider than LIMIT.
COMPILED supplies the row layout."
  (let* ((texts (mapcar (lambda (entry)
                          (or (majutsu-row-display-value entry compiled field) ""))
                        entries))
         (width (apply #'max 0
                       (seq-filter (lambda (width) (<= width limit))
                                   (mapcar #'string-width texts)))))
    (cl-mapc (lambda (entry text)
               (majutsu-row-set-display-value
                entry compiled field
                (concat text
                        (make-string (max 0 (- width (string-width text)))
                                     ?\s))))
             entries texts)))

(defun majutsu-bookmark--remote-summary (remotes compiled)
  "Return the tracking state of row entries REMOTES as one string.
COMPILED supplies the row layout."
  (mapconcat
   (lambda (entry)
     (let ((tracking (majutsu-row-display-value entry compiled 'tracking))
           (remote (propertize (majutsu-row-column entry 'remote)
                               'font-lock-face 'majutsu-bookmark-remote)))
       (if (and tracking (not (string-empty-p tracking)))
           (concat tracking " " remote)
         remote)))
   remotes ", "))

(defun majutsu-bookmark--align-rows (rows compiled)
  "Give local entries of ROWS aligned name and remote-state columns.
COMPILED supplies the row layout."
  (dolist (row rows)
    (majutsu-row-set-display-value
     (car row) compiled 'remotes
     (majutsu-bookmark--remote-summary (cdr row) compiled)))
  (let ((locals (mapcar #'car rows)))
    (majutsu-bookmark--align-column
     locals compiled 'ref majutsu-bookmark--list-name-width)
    (majutsu-bookmark--align-column
     locals compiled 'remotes majutsu-bookmark--list-remotes-width)))

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

(defun majutsu-bookmark--insert-conflict-target (target indent)
  "Insert one bookmark conflict TARGET after INDENT as a commit section."
  (magit-insert-section (jj-commit (car target) t)
    (majutsu-bookmark--insert-heading
     indent (majutsu-bookmark--target-heading (cdr target)))))

(defun majutsu-bookmark--insert-list-entry (entry compiled indent &optional remotes)
  "Insert bookmark ENTRY after INDENT using COMPILED.
Row entries REMOTES become its child sections.  Return the entries inserted."
  (let* ((targets (majutsu-bookmark--conflict-targets entry compiled))
         (valid-targets (and (not (eq targets :invalid)) targets))
         (child-indent (concat indent "  "))
         (inserted (list entry))
         (body-inserter
          (and (or valid-targets remotes)
               (lambda ()
                 (dolist (target valid-targets)
                   (majutsu-bookmark--insert-conflict-target target child-indent))
                 (majutsu-bookmark--align-column
                  remotes compiled 'at-remote
                  majutsu-bookmark--list-remotes-width)
                 (majutsu-bookmark--align-column
                  remotes compiled 'tracking
                  majutsu-bookmark--list-tracking-width)
                 (dolist (remote remotes)
                   ;; The parent row already names the bookmark.
                   (majutsu-row-set-display-value remote compiled 'ref "")
                   (nconc inserted
                          (majutsu-bookmark--insert-list-entry
                           remote compiled child-indent)))))))
    (plist-put entry :heading-prefixes (list indent))
    (let ((section (majutsu-row-insert-entry
                    entry compiled body-inserter valid-targets)))
      (oset section remote (majutsu-row-column entry 'remote))
      (oset section tracked (majutsu-row-column entry 'tracked))
      (when (or remotes (majutsu-row-column entry 'remote))
        (save-excursion
          (goto-char (oref section start))
          (put-text-property (point) (line-end-position)
                             'help-echo majutsu-bookmark--tracking-help))))
    inserted))

(defun majutsu-bookmark--remote-heading (remote)
  "Return a styled heading for the REMOTE plist, including its URLs."
  (let ((url (plist-get remote :url))
        (push (plist-get remote :push-url)))
    (concat (propertize (plist-get remote :name)
                        'font-lock-face 'majutsu-bookmark-remote)
            (and url (concat "  " url))
            (and push (not (equal push url))
                 (format " (push: %s)" push))
            (majutsu-jjosh
              (and-let* ((representation (plist-get remote :representation))
                         ((not (equal representation "whole"))))
                (concat "  " representation))))))

(defun majutsu-bookmark--insert-placeholder (text)
  "Insert TEXT as the only line of an empty list."
  (insert (propertize (concat text "\n") 'font-lock-face 'shadow)))

(defun majutsu-bookmark--insert-bookmarks (rows compiled indent)
  "Insert ROWS of `majutsu-bookmark--local-rows' after INDENT using COMPILED.
Return the row entries inserted."
  (majutsu-bookmark--align-rows rows compiled)
  (mapcan (lambda (row)
            (majutsu-bookmark--insert-list-entry
             (car row) compiled indent (cdr row)))
          rows))

(defun majutsu-bookmark--remote-counts (entries)
  "Return how many of the remote bookmark ENTRIES are tracked and untracked."
  (let* ((tracked (seq-count (lambda (entry) (majutsu-row-column entry 'tracked))
                             entries))
         (untracked (- (length entries) tracked)))
    (string-join
     (delq nil (list (and (> tracked 0) (format "%d tracked" tracked))
                     (and (> untracked 0) (format "%d untracked" untracked))))
     ", ")))

(defun majutsu-bookmark--insert-remote (remote entries compiled indent)
  "Insert the REMOTE plist after INDENT with its bookmark ENTRIES folded.
COMPILED supplies the row layout."
  (magit-insert-section (jj-git-remote (plist-get remote :value) (and entries t))
    (majutsu-bookmark--insert-heading
     indent
     (concat (majutsu-bookmark--remote-heading remote)
             (and entries
                  (propertize
                   (concat "  " (majutsu-bookmark--remote-counts entries))
                   'font-lock-face 'shadow))))
    ;; A dependency can have hundreds of bookmarks; render them on demand.
    (magit-insert-section-body
      (majutsu-bookmark--align-column
       entries compiled 'ref majutsu-bookmark--list-name-width)
      (majutsu-bookmark--align-column
       entries compiled 'tracking majutsu-bookmark--list-tracking-width)
      (dolist (entry entries)
        ;; The heading already names the remote.
        (majutsu-row-set-display-value entry compiled 'at-remote "")
        (majutsu-bookmark--insert-list-entry
         entry compiled (concat indent "  "))))))

(defun majutsu-bookmark--insert-remotes (remotes entries compiled indent)
  "Insert REMOTES after INDENT, each holding its share of remote ENTRIES.
A remote that only ENTRIES name follows the configured ones.  COMPILED
supplies the row layout."
  (let ((by-remote (seq-group-by (lambda (entry)
                                   (majutsu-row-column entry 'remote))
                                 entries)))
    (dolist (remote
             (append remotes
                     (mapcar (lambda (name) (list :name name :value name))
                             (seq-difference
                              (mapcar #'car by-remote)
                              (mapcar (lambda (remote) (plist-get remote :name))
                                      remotes)))))
      (majutsu-bookmark--insert-remote
       remote
       (alist-get (plist-get remote :name) by-remote nil nil #'equal)
       compiled indent))))

;;;; Projects

;; In the jjosh build remotes are configured per scope: the repository itself
;; or one registered project.  A ref belongs to the scope its name selects, so
;; both lists get one section per scope.

(majutsu-jjosh
  (defvar-local majutsu-bookmark--list-projects nil
    "Registered projects for the current list refresh.
Each project is a plist with `:id', `:name', `:path', `:labels' and
`:remotes'.")

  (defun majutsu-bookmark--unsynced-p (entry)
    "Return non-nil if remote ENTRY is tracked but delivers other content."
    (and (majutsu-row-column entry 'tracked)
         (not (majutsu-bookmark--git-entry-p entry))
         (not (majutsu-row-column entry 'synced))
         (not (majutsu-row-column entry 'content-synced))))

  (defun majutsu-bookmark--project-name (project)
    "Return the name of the PROJECT plist, or its id while unresolved."
    (or (plist-get project :name) (plist-get project :id)))

  (defun majutsu-bookmark--project-heading (project rows)
    "Return the heading of PROJECT, whose local bookmarks are ROWS."
    (let ((name (plist-get project :name))
          (label (car (plist-get project :labels)))
          (unsynced (seq-count
                     (lambda (row)
                       (seq-some #'majutsu-bookmark--unsynced-p (cdr row)))
                     rows)))
      (concat
       (if name
           (propertize name 'font-lock-face 'magit-section-heading)
         (propertize (concat (plist-get project :id) " (unresolved)")
                     'font-lock-face 'warning))
       (and label (not (equal label name))
            (propertize (concat " #" label) 'font-lock-face 'shadow))
       (and-let* ((path (plist-get project :path)))
         (propertize (concat "  " path) 'font-lock-face 'shadow))
       (and (> unsynced 0)
            (propertize (format "  %d unsynced" unsynced)
                        'font-lock-face 'warning)))))

  (defun majutsu-bookmark--scopes (entries)
    "Return an alist (PROJECT . ENTRIES) dividing row ENTRIES by scope.
PROJECT is nil for the repository, which comes first, and else a project
plist.  Every registered project is present, by name."
    (let* ((by-project (seq-group-by (lambda (entry)
                                       (majutsu-row-column entry 'project))
                                     entries))
           (projects
            (sort (append majutsu-bookmark--list-projects
                          ;; A ref can outlive its project's listing.
                          (mapcar (lambda (id) (list :id id :name id))
                                  (seq-difference
                                   (delq nil (mapcar #'car by-project))
                                   (mapcar (lambda (project)
                                             (plist-get project :id))
                                           majutsu-bookmark--list-projects))))
                  (lambda (a b)
                    (string< (majutsu-bookmark--project-name a)
                             (majutsu-bookmark--project-name b))))))
      (cons (cons nil (alist-get nil by-project))
            (mapcar (lambda (project)
                      (cons project
                            (alist-get (plist-get project :id) by-project
                                       nil nil #'equal)))
                    projects))))

  (defun majutsu-bookmark--insert-scope (project rows insert-body)
    "Insert the section of PROJECT, or of the repository when it is nil.
ROWS are the local bookmarks its heading reports on, and INSERT-BODY
inserts its contents."
    (if project
        (magit-insert-section (jj-project (majutsu-bookmark--project-name project))
          (magit-insert-heading
            (majutsu-bookmark--project-heading project rows))
          (funcall insert-body))
      (magit-insert-section (repository)
        (magit-insert-heading "Repository")
        (funcall insert-body)
        (insert "\n"))))

  (defun majutsu-bookmark--insert-bookmark-scopes (entries compiled)
    "Insert the local bookmarks among row ENTRIES under their scopes.
Return the row entries inserted."
    (let (displayed)
      (pcase-dolist (`(,project . ,scoped) (majutsu-bookmark--scopes entries))
        (when-let* ((rows (majutsu-bookmark--local-rows scoped)))
          (majutsu-bookmark--insert-scope
           project rows
           (lambda ()
             (push (majutsu-bookmark--insert-bookmarks rows compiled "  ")
                   displayed)))))
      (apply #'append (nreverse displayed))))

  (defun majutsu-bookmark--insert-remote-scopes (entries compiled)
    "Insert every project with its remotes, which hold remote ENTRIES.
The repository scope is listed when it has remotes of its own."
    (pcase-dolist (`(,project . ,scoped) (majutsu-bookmark--scopes entries))
      (let ((remotes (if project
                         (plist-get project :remotes)
                       majutsu-bookmark--list-remotes)))
        (when (or project remotes scoped)
          (majutsu-bookmark--insert-scope
           project nil
           (lambda ()
             (majutsu-bookmark--insert-remotes
              remotes scoped compiled "  "))))))))

(defun majutsu-bookmark--wash-list (_args)
  "Wash structured row output into local bookmark rows.
The jjosh build groups them by scope."
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (parsed (majutsu-row-read-buffer compiled))
         (entries (plist-get parsed :entries))
         (inhibit-read-only t))
    (majutsu-row-report-diagnostics (plist-get parsed :diagnostics))
    (delete-region (point-min) (point-max))
    (let ((displayed
           (if (majutsu-jjosh t)
               (majutsu-bookmark--insert-bookmark-scopes entries compiled)
             (majutsu-bookmark--insert-bookmarks
              (majutsu-bookmark--local-rows entries) compiled ""))))
      (unless displayed
        (majutsu-bookmark--insert-placeholder "No bookmarks"))
      (majutsu-row-set-buffer-data compiled displayed))))

(defun majutsu-bookmark--wash-remote-list (_args)
  "Wash structured row output into remotes holding their bookmarks.
The jjosh build groups them by scope."
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (parsed (majutsu-row-read-buffer compiled))
         (entries (seq-filter #'majutsu-bookmark--network-entry-p
                              (plist-get parsed :entries)))
         (inhibit-read-only t))
    (majutsu-row-report-diagnostics (plist-get parsed :diagnostics))
    (delete-region (point-min) (point-max))
    (if (majutsu-jjosh t)
        (majutsu-bookmark--insert-remote-scopes entries compiled)
      (majutsu-bookmark--insert-remotes
       majutsu-bookmark--list-remotes entries compiled ""))
    (when (bobp)
      (majutsu-bookmark--insert-placeholder "No remotes"))
    (majutsu-row-set-buffer-data compiled entries)))

;;;; Buffers

(defun majutsu-bookmark--read-remotes ()
  "Return the configured Git remotes as list remotes."
  (let* ((payload (majutsu-remote-candidate-data))
         (entries (plist-get payload :entries)))
    (mapcar (lambda (name)
              (let ((remote (gethash name entries)))
                (list :name name :value name
                      :url (plist-get remote :fetch-url)
                      :push-url (plist-get remote :push-url))))
            (plist-get payload :candidates))))

(majutsu-jjosh
  (defun majutsu-bookmark--read-projects ()
    "Return the repository's registered projects with their remotes."
    (let (projects)
      (dolist (line (majutsu-jj-lines "project" "list"
                                      "-T" "json(self) ++ \"\\n\""))
        ;; Metadata problems follow the projects as plain text.
        (when (string-prefix-p "{" line)
          (let ((project (json-parse-string line :object-type 'plist
                                            :array-type 'list
                                            :null-object nil)))
            (plist-put
             project :remotes
             (mapcar (lambda (remote)
                       (cl-flet ((text (key)
                                   (majutsu-bookmark--row-empty-to-nil
                                    (plist-get remote key))))
                         (list :name (plist-get remote :name)
                               :value (plist-get remote :qualified_name)
                               :url (text :url)
                               :push-url (text :push_url)
                               :representation (text :representation))))
                     (plist-get project :remotes)))
            (push project projects))))
      (nreverse projects)))

  (defun majutsu-bookmark--repository-remotes (remotes projects)
    "Return those of REMOTES that belong to no project of PROJECTS."
    (let ((owned (mapcan (lambda (project)
                           (mapcar (lambda (remote) (plist-get remote :value))
                                   (plist-get project :remotes)))
                         projects)))
      (seq-remove (lambda (remote) (member (plist-get remote :value) owned))
                  remotes)))

  (defun majutsu-bookmark--scoped-remote (entry remote)
    "Return the name `jj git remote' accepts for REMOTE of row ENTRY.
Return nil when ENTRY belongs to the repository scope, where REMOTE is
that name already."
    (and-let* ((id (majutsu-row-column entry 'project))
               (project (seq-find (lambda (project)
                                    (equal (plist-get project :id) id))
                                  majutsu-bookmark--list-projects))
               (found (seq-find (lambda (candidate)
                                  (equal (plist-get candidate :name) remote))
                                (plist-get project :remotes))))
      (plist-get found :value))))

(defun majutsu-bookmark--insert-rows (washer)
  "Insert the rows of `jj bookmark list' through WASHER."
  (majutsu-jj-wash washer nil
    (append '("bookmark" "list" "--quiet" "--all-remotes")
            (list "-T" (majutsu-bookmark--list-template)))))

(defun majutsu-bookmark--select-section (buffer type value)
  "Move point in BUFFER to the section of TYPE with VALUE, if it exists."
  (with-current-buffer buffer
    (let (found)
      (magit-map-sections
       (lambda (section)
         (when (and (not found)
                    (eq (oref section type) type)
                    (equal (oref section value) value))
           (setq found section))))
      (when found
        (let ((parent (oref found parent)))
          (while parent
            (magit-section-show parent)
            (setq parent (oref parent parent))))
        (goto-char (oref found start))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window (point)))))))

(defun majutsu-bookmark--local-name-at-point ()
  "Return the local name of the bookmark section at point, or nil."
  (and-let* ((ref (magit-section-value-if 'jj-bookmark)))
    (car (majutsu--bookmark-split-remote-ref ref))))

(defun majutsu-bookmark--remote-value-at-point ()
  "Return the name `jj git remote' accepts for the remote at point.
That is a remote line, or the remote of the remote bookmark at point."
  (or (majutsu-remote-at-point)
      (and-let* ((section (magit-current-section))
                 ((magit-section-match 'jj-bookmark section))
                 (remote (oref section remote))
                 ((not (equal remote "git"))))
        (or (majutsu-jjosh
              (majutsu-bookmark--scoped-remote (majutsu-row-entry-at-point)
                                               remote))
            remote))))

;;;###autoload
(defun majutsu-bookmark-list (&optional bookmark)
  "List local bookmarks with the state of their remotes in a dedicated buffer.
Move to BOOKMARK, interactively the local bookmark of the bookmark at
point.  The jjosh build lists bookmarks per scope: the repository and
each project."
  (interactive (list (majutsu-bookmark--local-name-at-point)))
  (let ((buffer (majutsu-setup-buffer #'majutsu-bookmark-list-mode nil
                  :buffer "*Majutsu Bookmarks*")))
    (when bookmark
      (majutsu-bookmark--select-section buffer 'jj-bookmark bookmark))
    buffer))

(defun majutsu-bookmark-list-refresh-buffer ()
  "Refresh the bookmark list buffer."
  (majutsu--assert-mode 'majutsu-bookmark-list-mode)
  (majutsu-jjosh
    (setq majutsu-bookmark--list-projects (majutsu-bookmark--read-projects)))
  (magit-insert-section (bookmark-list)
    (majutsu-bookmark--insert-rows #'majutsu-bookmark--wash-list)))

;;;###autoload
(defun majutsu-git-remote-list (&optional remote)
  "List Git remotes with the bookmarks they hold in a dedicated buffer.
Move to REMOTE, interactively the remote at point or the remote of the
remote bookmark at point.  The jjosh build lists remotes per scope: the
repository and each project."
  (interactive (list (majutsu-bookmark--remote-value-at-point)))
  (let ((buffer (majutsu-setup-buffer #'majutsu-git-remote-list-mode nil
                  :buffer "*Majutsu Remotes*")))
    (when remote
      (majutsu-bookmark--select-section buffer 'jj-git-remote remote))
    buffer))

(defun majutsu-git-remote-list-refresh-buffer ()
  "Refresh the remote list buffer."
  (majutsu--assert-mode 'majutsu-git-remote-list-mode)
  (let ((remotes (majutsu-bookmark--read-remotes)))
    (majutsu-jjosh
      (setq majutsu-bookmark--list-projects (majutsu-bookmark--read-projects)
            remotes (majutsu-bookmark--repository-remotes
                     remotes majutsu-bookmark--list-projects)))
    (setq majutsu-bookmark--list-remotes remotes))
  (magit-insert-section (remote-list)
    (majutsu-bookmark--insert-rows #'majutsu-bookmark--wash-remote-list)))

(defvar-keymap majutsu-git-remote-list-mode-map
  :doc "Keymap for `majutsu-git-remote-list-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-git-remote-list-mode majutsu-mode "Majutsu Remotes"
  "Major mode for viewing Git remotes and the bookmarks they hold."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

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
          (rev (majutsu-read-revset "Target revision" :default at)))
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
     :description "Forget (no push)")]])

;;; _
(provide 'majutsu-bookmark)
;;; majutsu-bookmark.el ends here
