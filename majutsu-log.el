;;; majutsu-log.el --- Log view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025-2026 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library builds the Majutsu log buffer: compiles jj templates,
;; parses log output, renders sections, and handles navigation.

;;; Code:

(require 'majutsu)
(require 'majutsu-row)

;;; Section Keymaps

(defvar-keymap majutsu-commit-section-map
  :doc "Keymap for `jj-commit' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset)

;;; Log State

(defun majutsu-log--get-value (mode &optional use-buffer-args)
  "Get log arguments for MODE.

Returns an (ARGS FILESETS) pair.  USE-BUFFER-ARGS follows
`majutsu-prefix-use-buffer-arguments' or
`majutsu-direct-use-buffer-arguments'."
  (setq use-buffer-args
        (pcase-exhaustive use-buffer-args
          ('prefix majutsu-prefix-use-buffer-arguments)
          ('direct majutsu-direct-use-buffer-arguments)
          ('nil majutsu-direct-use-buffer-arguments)
          ((or 'always 'selected 'current 'never)
           use-buffer-args)))
  (cond
   ((and (memq use-buffer-args '(always selected current))
         (eq major-mode mode))
    (list majutsu-buffer-log-args
          majutsu-buffer-log-filesets))
   ((and (memq use-buffer-args '(always selected))
         (when-let* ((buf (majutsu--get-mode-buffer mode (eq use-buffer-args 'selected))))
           (list (buffer-local-value 'majutsu-buffer-log-args buf)
                 (buffer-local-value 'majutsu-buffer-log-filesets buf)))))
   (t
    (list (majutsu-transient-default-value
           'majutsu-log mode
           'majutsu-log-current-arguments
           'majutsu-log-default-arguments)
          (or (get mode 'majutsu-log-current-filesets)
              (get mode 'majutsu-log-default-filesets))))))

(defun majutsu-log--set-value (mode args filesets &optional save)
  "Set current log values for MODE.

When SAVE is non-nil, also persist ARGS using `transient-values'."
  (setq args (seq-remove #'null (flatten-tree args)))
  (setq filesets
        (and filesets
             (seq-remove (lambda (s)
                           (or (null s)
                               (and (stringp s) (string-empty-p s))))
                         (flatten-tree filesets))))
  (if-let* ((config-id (majutsu-repository-config-id)))
      (majutsu-transient-put-repository-current-value
       'majutsu-log mode args config-id)
    (put mode 'majutsu-log-current-arguments args))
  (put mode 'majutsu-log-current-filesets filesets)
  (when save
    (setf (alist-get (majutsu-transient-global-default-key 'majutsu-log mode)
                     transient-values)
          args)
    (transient-save-values))
  (when (eq major-mode mode)
    (setq-local majutsu-buffer-log-args args)
    (setq-local majutsu-buffer-log-filesets filesets))
  nil)

(defvar-local majutsu-log--this-error nil
  "Last jj side-effect error summary for this log buffer.

This is set by process runners (see `majutsu-process-buffer') and
rendered by `majutsu-log-insert-error-header' on the next refresh.")

(defcustom majutsu-log-sections-hook
  (list #'majutsu-log-insert-error-header
        #'majutsu-log-insert-logs
        #'majutsu-log-insert-status
        #'majutsu-insert-workspaces)
  "Hook run to insert sections in the log buffer."
  :type 'hook
  :group 'majutsu)

(defun majutsu-log--format-summary (prefix)
  "Return PREFIX annotated with current log arguments."
  (pcase-let* ((`(,args ,filesets)
                (majutsu-log--get-value 'majutsu-log-mode 'current))
               (args (append args (and filesets (cons "--" filesets)))))
    (if args
        (format "%s (%s)" prefix (string-join args " "))
      prefix)))

(defun majutsu-log--heading-string ()
  "Return heading string for the log section."
  (majutsu-log--format-summary "Log Graph"))

(defun majutsu-log--transient-description ()
  "Return description string for the log transient."
  (majutsu-log--format-summary "JJ Log"))

;;; Log Template

(defconst majutsu-log--field-list-separator "\x1c"
  "Separator character inserted between list items inside a single field value.")

(defconst majutsu-log--field-line-separator
  majutsu-row-field-line-separator
  "Encoded newline separator used inside template field payloads.
Log records are transported as single lines, then this separator is
decoded back to literal newlines after field splitting.")

(defconst majutsu-log--field-default-modules
  '((id . metadata)
    (change-id . heading)
    (commit-id . metadata)
    (parent-ids . metadata)
    (bookmarks . heading)
    (tags . heading)
    (working-copies . heading)
    (flags . metadata)
    (git-head . heading)
    (signature . heading)
    (empty . heading)
    (description . heading)
    (author . tail)
    (timestamp . tail)
    (long-desc . body))
  "Default module placement for known log fields.")

(defconst majutsu-log--required-columns '(id commit-id parent-ids)
  "Fields that must exist in `majutsu-log-commit-columns'.
These fields are transported even when users omit them from visible layout,
so log semantics such as stable identity, commit-hash copying, and relation
navigation remain available.")

(defconst majutsu-log--default-column-postprocessors nil
  "Default postprocessors appended to every column instance.")

(defconst majutsu-log--field-default-postprocessors
  '((parent-ids . (majutsu-log-post-split-list-separator))
    (timestamp . (majutsu-log-post-remove-ago-suffix)))
  "Field-specific default postprocessors appended after global defaults.")

(defcustom majutsu-log-commit-columns
  '((:field change-id :module heading :face t)
    (:field bookmarks :module heading :face magit-branch-local)
    (:field tags :module heading :face magit-tag)
    (:field working-copies :module heading :face magit-branch-remote)
    (:field empty :module heading :face t)
    (:field git-head :module heading :face t)
    (:field description :module heading :face t)
    (:field author :module tail :face magit-log-author)
    (:field timestamp :module tail :face magit-log-date)
    (:field long-desc :module body :face t)
    (:field id :module metadata :face nil)
    (:field commit-id :module metadata :face nil)
    (:field flags :module metadata :face nil))
  "Field specification controlling log template and rendering.

Each element is a plist with at least `:field'. Supported keys:
- :field  - symbol identifying a known field.
- :module - one of `heading', `tail', `body', or `metadata'.
- :face   - t (preserve jj highlighting), nil (strip), or FACE (override).
- :post   - postprocessor function or function list for field value transforms.

When `:face' is omitted, it defaults to t.

`heading' is the only module that may emit physical newlines. Other modules
remain in the sequential payload tail and should encode logical newlines as
`majutsu-log--field-line-separator' (\\x1f)."
  :type '(repeat (plist :options (:field :module :face :post)))
  :group 'majutsu)

(defmacro majutsu-log-define-column (name template doc)
  "Define a log column template variable for NAME with default TEMPLATE and DOC.
The variable name will be `majutsu-log-template-NAME'.
Also registers a variable watcher to invalidate the template cache."
  (declare (indent 1) (debug t) (doc-string 3))
  (let ((var-name (intern (format "majutsu-log-template-%s" name))))
    `(progn
       (defcustom ,var-name ,template
         ,doc
         :type 'sexp
         :group 'majutsu)
       (when (fboundp 'add-variable-watcher)
         (add-variable-watcher ',var-name #'majutsu-log--invalidate-template-cache)))))

(majutsu-template-defkeyword git_head Commit
  (:returns Boolean :doc "Deprecated alias for .contained_in('first_parent(@)')")
  [:method [:self] :contained_in "first_parent(@)"])

(majutsu-template-defkeyword short-change-id Commit
  (:returns Template :doc "Shortest unique change id.")
  [:change_id :shortest 8])

(majutsu-template-defkeyword short-change-id-with-offset Commit
  (:returns Template :doc "Shortest unique change id with offset.")
  [[:short-change-id]
   [:label "change_offset" "/"]
   [:change_offset]])

(majutsu-template-defkeyword canonical-log-id Commit
  (:returns Template :doc "Canonical log id.")
  [:if [:or [:hidden]
            [:divergent]]
      [:commit_id :shortest 8]
    [:change_id :shortest 8]])

(majutsu-log-define-column id
  [:canonical-log-id]
  "Template for the commit-id column.")

(majutsu-log-define-column change-id
  [:label
   [:separate " "
              [:if [:current_working_copy] "working_copy"]
              [:if [:immutable] "immutable" "mutable"]
              [:if [:conflict] "conflicted"]]
   [:coalesce
    [:if [:hidden]
        [:label "hidden" [:short-change-id-with-offset]]]
    [:if [:divergent]
        [:label "divergent" [:short-change-id-with-offset]]]
    [:short-change-id]]]
  "Template for the change-id column.")

(majutsu-log-define-column commit-id
  [:commit_id :shortest 8]
  "Template for the commit-id column.")

(majutsu-log-define-column parent-ids
  `[:method
    [:map [:parents] p [:canonical-log-id]]
    :join ,majutsu-log--field-list-separator]
  "Template for the parent-ids metadata column.")

(majutsu-log-define-column bookmarks
  [:bookmarks]
  "Template for the bookmarks column.")

(majutsu-log-define-column tags
  [:tags]
  "Template for the tags column.")

(majutsu-log-define-column working-copies
  [:working_copies]
  "Template for the working-copies column.")

(majutsu-log-define-column flags
  [:separate " "
             [:if [:current_working_copy] "@"]
             [:if [:immutable] "immutable" "mutable"]
             [:if [:conflict] [:label "conflict" "conflict"]]
             [:if [:git_head] "git_head"]
             [:if [:root] "root"]
             [:if [:empty] "(empty)"]]
  "Template for the flags column.")

(majutsu-log-define-column git-head
  [:if [:git_head] [:label "git_head" "(git_head)"]]
  "Template for the git-head column.")

(majutsu-log-define-column signature
  [:if [:method [:call 'config "ui.show-cryptographic-signatures"] :as_boolean]
      [:if [:signature]
          [:label "signature status"
                  ["["
                   [:label [:signature :status]
                           [:coalesce
                            [:if [:== [:signature :status] "good"] "✓︎"]
                            [:if [:== [:signature :status] "unknown"] "?"]
                            "x"]]
                   "]"]]]]
  "Template for the signature column.")

(majutsu-log-define-column empty
  [:if [:empty]
      [:label "empty" "(empty)"]]
  "Template for the empty column.")

(majutsu-log-define-column description
  [:if [:description]
      [:method [:description] :first_line]
    [:label
     [:if [:empty] "empty"]
     [:label
      "description placeholder"
      "(no desc)"]]]
  "Template for the description column.")

(majutsu-log-define-column author
  [:author :name]
  "Template for the author column.")

(majutsu-log-define-column timestamp
  [:committer :timestamp :ago]
  "Template for the timestamp column.")

(majutsu-log-define-column long-desc
  [:description :lines :skip 1 :join "\x1f"]
  "Template for the long-desc column.
Newlines are encoded as an internal separator so ANSI/label styling
can survive transport through the single-line log format.")

(defvar majutsu-log--compiled-template-cache nil
  "Cached structure holding the compiled log template and column metadata.")

(defvar-local majutsu-log--cached-entries nil
  "Cached log entries for the current buffer.")

(defvar-local majutsu-log--entry-by-id nil
  "Hash table mapping visible log entry ids to parsed entry plists.")

(defvar-local majutsu-log--children-by-id nil
  "Hash table mapping visible parent ids to visible child id lists.")

(defvar-local majutsu-log--buffer-compiled nil
  "Compiled column/layout metadata used to render the current buffer.")

(defun majutsu-log--invalidate-template-cache (&rest _)
  "Reset cached compiled template when layout changes."
  (setq majutsu-log--compiled-template-cache nil)
  (setq majutsu-log--cached-entries nil)
  (setq majutsu-log--entry-by-id nil)
  (setq majutsu-log--children-by-id nil)
  (setq majutsu-log--buffer-compiled nil))

(defun majutsu-log-post-decode-line-separator (value &optional _ctx)
  "Decode `majutsu-log--field-line-separator' inside VALUE.

This is the default postprocessor for log fields so all modules can
transport logical newlines safely through single-line payload segments."
  (if (stringp value)
      (subst-char-in-string
       (aref majutsu-log--field-line-separator 0)
       ?\n
       value t)
    value))

(defun majutsu-log-post-split-list-separator (value &optional _ctx)
  "Split VALUE by `majutsu-log--field-list-separator'."
  (when (and (stringp value)
             (not (string-empty-p value)))
    (mapcar #'substring-no-properties
            (majutsu-row-split-by-separator
             value majutsu-log--field-list-separator))))

(defun majutsu-log-post-remove-ago-suffix (value &optional _ctx)
  "Trim a trailing \\=' ago\\=' suffix from VALUE."
  (if (stringp value)
      (string-remove-suffix " ago" value)
    value))

(defun majutsu-log--row-profile ()
  "Return the row profile for `majutsu-log'."
  (list :name 'log
        :self-type 'Commit
        :columns-var 'majutsu-log-commit-columns
        :default-modules majutsu-log--field-default-modules
        :required-fields majutsu-log--required-columns
        :default-postprocessors majutsu-log--default-column-postprocessors
        :field-postprocessors majutsu-log--field-default-postprocessors
        :template-function 'majutsu-log--column-template
        :decode-function 'majutsu-log-post-decode-line-separator
        :record-field-function 'majutsu-log--record-field
        :entry-id-function 'majutsu-log--entry-id
        :section-class 'jj-commit
        :section-value-function 'majutsu-log--entry-id
        :section-hide nil
        :tail-align t
        :compat-property-prefix 'majutsu-log))

(defun majutsu-log--column-template (field)
  "Return majutsu-template form for FIELD.
Looks up `majutsu-log-template-FIELD'."
  (let ((var (intern-soft (format "majutsu-log-template-%s" field))))
    (if (and var (boundp var))
        (symbol-value var)
      (user-error "Unknown column field %S" field))))

(defun majutsu-log--compile-columns (&optional columns)
  "Compile COLUMNS (or `majutsu-log-commit-columns') into a jj template string.
Returns a plist with :template, :columns, and :module-columns."
  (majutsu-row-compile (majutsu-log--row-profile) columns))

(defun majutsu-log--ensure-template ()
  "Return cached compiled template structure, recomputing if necessary."
  (or majutsu-log--compiled-template-cache
      (setq majutsu-log--compiled-template-cache
            (majutsu-log--compile-columns majutsu-log-commit-columns))))

(defun majutsu-log--build-args ()
  "Build argument list for `jj log' using current log variables."
  (pcase-let ((`(,args ,filesets)
               (majutsu-log--get-value 'majutsu-log-mode 'current)))
    (let ((cmd '("log")))
      (setq cmd (append cmd args))
      (setq cmd (append cmd (list "-T" (plist-get (majutsu-log--ensure-template) :template))))
      (setq cmd (append cmd filesets))
      cmd)))

;;; Log Parsing

(defun majutsu-log--apply-flags (entry value)
  "Set flag fields on ENTRY based on VALUE string."
  (dolist (flag (split-string (or value "") " " t))
    (pcase flag
      ("immutable" (setq entry (plist-put entry :immutable t)))
      ("mutable" (setq entry (plist-put entry :immutable nil)))
      ("conflict" (setq entry (plist-put entry :conflict t)))
      ("git_head" (setq entry (plist-put entry :git-head t)))
      ("root" (setq entry (plist-put entry :root t)))
      ("@" (setq entry (plist-put entry :current_working_copy t)))))
  entry)

(defun majutsu-log--record-field (entry field value)
  "Record canonical FIELD VALUE onto ENTRY plist and field map."
  (pcase field
    ('id
     (setq entry (plist-put entry :id value)))
    ('change-id
     (setq entry (plist-put entry :change-id value)))
    ('commit-id
     (setq entry (plist-put entry :commit-id value)))
    ('parent-ids
     (setq entry (plist-put entry :parent-ids value)))
    ('bookmarks
     (setq entry (plist-put entry :bookmarks value)))
    ('tags
     (setq entry (plist-put entry :tags value)))
    ('working-copies
     (setq entry (plist-put entry :working-copies value)))
    ('description
     (setq entry (plist-put entry :short-desc value)))
    ('author
     (setq entry (plist-put entry :author value)))
    ('timestamp
     (setq entry (plist-put entry :timestamp value)))
    ('long-desc
     (setq entry (plist-put entry :long-desc value)))
    ('flags
     (setq entry (majutsu-log--apply-flags entry value)))
    ('git-head
     (when (and value (not (string-empty-p value)))
       (setq entry (plist-put entry :git-head t))))
    ('signature
     (setq entry (plist-put entry :signature value)))
    ('empty
     (setq entry (plist-put entry :empty (not (string-empty-p value))))))
  (let ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (setq entry (plist-put entry :columns columns)))
  entry)

(defun majutsu-log--entry-id (entry)
  "Return stable section id string from ENTRY metadata fields."
  (or (let ((id (plist-get entry :id)))
        (and (stringp id)
             (not (string-empty-p (string-trim id)))
             (substring-no-properties id)))
      (let ((change-id (plist-get entry :change-id)))
        (and (stringp change-id)
             (not (string-empty-p (string-trim change-id)))
             (substring-no-properties change-id)))
      "unknown"))

(defun majutsu-log--rebuild-relation-indexes (&optional entries)
  "Rebuild visible relation indexes from ENTRIES.

When ENTRIES is nil, use `majutsu-log--cached-entries'."
  (let ((entries (or entries majutsu-log--cached-entries))
        (entry-by-id (make-hash-table :test #'equal))
        (children-by-id (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (puthash (majutsu-log--entry-id entry) entry entry-by-id))
    (dolist (entry entries)
      (let ((child-id (majutsu-log--entry-id entry)))
        (dolist (parent-id (plist-get entry :parent-ids))
          (when (and (stringp parent-id)
                     (not (string-empty-p parent-id)))
            (puthash parent-id
                     (append (gethash parent-id children-by-id) (list child-id))
                     children-by-id)))))
    (setq majutsu-log--entry-by-id entry-by-id)
    (setq majutsu-log--children-by-id children-by-id)))

(defun majutsu-log--ensure-relation-indexes ()
  "Ensure visible relation indexes are available in the current buffer."
  (unless (and (hash-table-p majutsu-log--entry-by-id)
               (hash-table-p majutsu-log--children-by-id))
    (majutsu-log--rebuild-relation-indexes)))

(defun majutsu-log--entry-for-id (id)
  "Return visible parsed entry for ID, or nil."
  (when (and id (not (string-empty-p id)))
    (majutsu-log--ensure-relation-indexes)
    (gethash id majutsu-log--entry-by-id)))

(defun majutsu-log--current-entry-id ()
  "Return current `jj-commit' section id or signal a user error."
  (or (magit-section-value-if 'jj-commit)
      (user-error "No changeset at point")))

(defun majutsu-log--current-compiled ()
  "Return compiled column/layout metadata for the current buffer."
  (or majutsu-log--buffer-compiled
      majutsu-log--compiled-template-cache
      (majutsu-log--ensure-template)))

;;;###autoload
(defun majutsu-log-copy-field ()
  "Copy the rendered value of the log field at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (majutsu-row-copy-field
   (majutsu-log--current-compiled)
   majutsu-log--cached-entries))

;;;###autoload
(defun majutsu-log-copy-module ()
  "Copy the rendered log module at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (majutsu-row-copy-module
   (majutsu-log--current-compiled)
   majutsu-log--cached-entries))

;;;###autoload
(defun majutsu-log-copy-entry-field ()
  "Copy a canonical field from the current log entry.

Unlike `majutsu-log-copy-field', this can target fields that are parsed and
stored on the entry but not currently visible in the rendered log line.  When
called interactively, prompt with completion over the current entry's
available canonical fields.  If the region is active, copy it literally using
`copy-region-as-kill'."
  (interactive)
  (majutsu-row-copy-entry-field
   (majutsu-log--current-compiled)
   majutsu-log--cached-entries))

;;;###autoload
(defun majutsu-log-copy-commit-id ()
  "Copy the current entry's commit hash.

This copies the canonical hidden `commit-id' field, even when it is not shown
in the visible log layout.  If the region is active, copy it literally using
`copy-region-as-kill'."
  (interactive)
  (majutsu-row-copy-commit-id
   (majutsu-log--current-compiled)
   majutsu-log--cached-entries))

(defun majutsu-log--visible-parent-ids (entry)
  "Return visible parent ids for ENTRY in current buffer order."
  (seq-filter #'majutsu-log--entry-for-id
              (delete-dups (copy-sequence (or (plist-get entry :parent-ids) nil)))))

(defun majutsu-log--visible-child-ids (entry)
  "Return visible child ids for ENTRY in current buffer order."
  (majutsu-log--ensure-relation-indexes)
  (seq-filter #'majutsu-log--entry-for-id
              (delete-dups (copy-sequence
                            (or (gethash (majutsu-log--entry-id entry)
                                         majutsu-log--children-by-id)
                                nil)))))

(defun majutsu-log--format-related-candidate (id)
  "Return display string for related revision ID."
  (if-let* ((entry (majutsu-log--entry-for-id id))
            (desc (plist-get entry :short-desc))
            ((not (string-empty-p (string-trim desc)))))
      (format "%s  %s" id (replace-regexp-in-string "\n+" " " desc nil t))
    id))

(defun majutsu-log--read-related-id (ids prompt)
  "Read one relation target from IDS using PROMPT.

When IDS contains a single element, return it without prompting."
  (let ((ids (delete-dups (copy-sequence ids))))
    (pcase ids
      (`() nil)
      (`(,id) id)
      (_
       (let* ((candidates (mapcar (lambda (id)
                                    (cons (majutsu-log--format-related-candidate id) id))
                                  ids))
              (choice (completing-read prompt (mapcar #'car candidates) nil t)))
         (cdr (assoc choice candidates)))))))

(defun majutsu-log--goto-related (ids prompt empty-message)
  "Jump to one of IDS using PROMPT, or signal EMPTY-MESSAGE."
  (if-let* ((target-id (majutsu-log--read-related-id ids prompt)))
      (unless (majutsu--goto-log-entry target-id)
        (user-error "Revision %s is not visible in the current log" target-id))
    (user-error "%s" empty-message)))

(defun majutsu-log-insert-error-header ()
  "Insert the message about the jj error that just occurred.

This function only knows about the last error that occurred when jj was
run for side-effects.  Refreshing the log buffer causes this section to
disappear again."
  (when majutsu-log--this-error
    (magit-insert-section (error 'jj)
      (insert (propertize (format "%-10s" "JJError! ")
                          'font-lock-face 'magit-section-heading))
      (insert (propertize majutsu-log--this-error 'font-lock-face 'error))
      (when-let* ((_ majutsu-show-process-buffer-hint)
                  (key (car (where-is-internal 'majutsu-process-buffer))))
        (insert (format "  [Type %s for details]" (key-description key))))
      (insert ?\n))
    (setq majutsu-log--this-error nil)))

(defun majutsu-log--refresh-tail-window (&optional window)
  "Refresh tail alignment for the current log buffer using WINDOW.
When WINDOW is nil, use the current row tail owner window."
  (when (derived-mode-p 'majutsu-log-mode)
    (setq window (or window (majutsu-row--tail-owner-window)))
    (majutsu-row-refresh-tail-spacers nil nil window)
    (when window
      (force-window-update window))))

(defun majutsu-log--after-text-scale-change ()
  "Refresh lightweight display alignment after text scaling changes."
  (majutsu-log--refresh-tail-window (selected-window)))

(defun majutsu-log--after-window-size-change (window)
  "Refresh tail alignment after the selected log WINDOW changes size."
  (when (eq window (selected-window))
    (majutsu-log--refresh-tail-window window)))

(defun majutsu-log--filter-buffer-substring (beg end &optional delete)
  "Filter copied log text between BEG and END.

When a copied region contains both heading and tail text, drop the tail text
from the copied result by default. Copying tail text alone preserves it."
  (majutsu-row-filter-buffer-substring
   beg end delete (majutsu-log--current-compiled)))

(defun majutsu-log--wash-logs (_args)
  "Wash jj log output in the current (narrowed) buffer region.

This function is meant to be used as a WASHER for `majutsu-jj-wash'."
  (let* ((compiled (majutsu-log--ensure-template))
         (entries nil))
    (setq majutsu-log--cached-entries nil)
    (majutsu-row-clear-buffer-data)
    (setq majutsu-log--entry-by-id nil)
    (setq majutsu-log--children-by-id nil)
    (setq-local majutsu-log--buffer-compiled compiled)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((entry (majutsu-row-wash-entry compiled)))
          (push entry entries)
        (magit-delete-line)))
    (setq majutsu-log--cached-entries (nreverse entries))
    (majutsu-row-set-buffer-data
     compiled
     majutsu-log--cached-entries)
    (majutsu-log--rebuild-relation-indexes majutsu-log--cached-entries)
    (insert "\n")))

(defun majutsu-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (magit-insert-section (lograph)
    (magit-insert-heading (majutsu-log--heading-string))
    (majutsu-jj-wash #'majutsu-log--wash-logs nil (majutsu-log--build-args))
    (insert "\n")))

;;; Log insert status

(defun majutsu-log--wash-status (_args)
  "Keep `jj status` output as-is in the current section."
  (goto-char (point-max)))

;; TODO: Enhance status output parsing to create sections per file and conflicts.
(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (magit-insert-section (status)
    (magit-insert-heading "Working Copy Status")
    (majutsu-jj-wash #'majutsu-log--wash-status nil "status")))

;;; Log insert conflicts

(defun majutsu-log-insert-conflicts ()
  "Insert conflicted files section."
  (let ((lines (majutsu-jj-lines "resolve" "--list")))
    (when lines
      (magit-insert-section (conflict)
        (magit-insert-heading "Unresolved Conflicts")
        (dolist (line lines)
          (let ((file (string-trim line)))
            (magit-insert-section (jj-file file)
              (magit-insert-heading (propertize file 'face 'error))
              (insert "\n"))))
        (insert "\n")))))

;;; Log Navigation

(defconst majutsu--show-id-template
  (majutsu-tpl [:canonical-log-id]))

(defun majutsu-current-id ()
  (when-let* ((output (majutsu-jj-string "log" "--no-graph" "-r" "@" "-T" majutsu--show-id-template)))
    (string-trim output)))

(defun majutsu-log-goto-@ ()
  "Jump to the current changeset (@)."
  (interactive)
  (majutsu--goto-log-entry (majutsu-current-id)))

(defun majutsu-log-goto-parent ()
  "Jump to a parent of the changeset at point.

When the current changeset has multiple visible parents, prompt for which
parent to visit."
  (interactive)
  (majutsu--assert-mode 'majutsu-log-mode)
  (let* ((entry-id (majutsu-log--current-entry-id))
         (entry (or (majutsu-log--entry-for-id entry-id)
                    (user-error "Changeset %s is not available in the current log" entry-id))))
    (majutsu-log--goto-related
     (majutsu-log--visible-parent-ids entry)
     "Go to parent: "
     "No parent revisions visible in the current log")))

(defun majutsu-log-goto-child ()
  "Jump to a child of the changeset at point.

When the current changeset has multiple visible children, prompt for which
child to visit."
  (interactive)
  (majutsu--assert-mode 'majutsu-log-mode)
  (let* ((entry-id (majutsu-log--current-entry-id))
         (entry (or (majutsu-log--entry-for-id entry-id)
                    (user-error "Changeset %s is not available in the current log" entry-id))))
    (majutsu-log--goto-related
     (majutsu-log--visible-child-ids entry)
     "Go to child: "
     "No child revisions visible in the current log")))

(defun majutsu-goto-commit (commit-id)
  "Jump to a specific COMMIT-ID in the log."
  (interactive "sCommit ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote commit-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Commit %s not found" commit-id))))

(defun majutsu-goto-change (change-id)
  "Jump to a specific CHANGE-ID in the log."
  (interactive "sChange ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote change-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Change %s not found" change-id))))

(defun majutsu--goto-log-entry (id)
  "Move point to the log entry section matching ID.
Return non-nil when the section could be located."
  (when-let* ((id (and id (string-trim id)))
              (_(not (string-empty-p id)))
              (section (majutsu-selection-find-section id 'jj-commit)))
    (magit-section-goto section)
    (goto-char (oref section start))
    t))

;;;###autoload
(defun majutsu-goto-next-changeset ()
  "Navigate to the next changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (< (point) (point-max)))
      (magit-section-forward)
      (when-let* ((section (magit-current-section)))
        (when (and (magit-section-match 'jj-commit section)
                   (> (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;;###autoload
(defun majutsu-goto-prev-changeset ()
  "Navigate to the previous changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (> (point) (point-min)))
      (magit-section-backward)
      (when-let* ((section (magit-current-section)))
        (when (and (magit-section-match 'jj-commit section)
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;; Log Mode

(defcustom majutsu-log-mode-hook (list #'bug-reference-mode)
  "Hook run after entering `majutsu-log-mode'."
  :group 'majutsu
  :type 'hook
  :options '(bug-reference-mode))

;;;###autoload(autoload 'majutsu-log-copy-transient "majutsu-log" nil t)
(majutsu-row-define-copy-transient
 majutsu-log-copy-transient
 "Transient for semantic copy commands in `majutsu-log-mode'."
 ("h" "Commit hash" majutsu-row-copy-commit-id))

(defvar-keymap majutsu-log-mode-map
  :doc "Keymap for `majutsu-log-mode'."
  :parent majutsu-mode-map
  "n" 'majutsu-goto-next-changeset
  "p" 'majutsu-goto-prev-changeset
  "[" 'majutsu-log-goto-parent
  "]" 'majutsu-log-goto-child
  "O" 'majutsu-new-dwim
  "D" 'majutsu-diff-dwim
  "Y" 'majutsu-duplicate-dwim
  "B" 'majutsu-new-with-before
  "A" 'majutsu-new-with-after)

(define-derived-mode majutsu-log-mode majutsu-mode "Majutsu Log"
  "Major mode for interacting with jj version control system."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (setq-local filter-buffer-substring-function #'majutsu-log--filter-buffer-substring)
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end-if-owner nil t)
  (add-hook 'text-scale-mode-hook #'majutsu-log--after-text-scale-change nil t)
  (add-hook 'window-size-change-functions #'majutsu-log--after-window-size-change nil t))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-log-mode))
  (list majutsu-buffer-log-args
        majutsu-buffer-log-filesets))

(defun majutsu-log-render ()
  "Render the log buffer using cached data."
  (magit-insert-section (logbuf)
    (run-hooks 'majutsu-log-sections-hook)
    (majutsu-selection-render)))

(defun majutsu-log-refresh-buffer ()
  "Refresh the current Majutsu log buffer."
  (majutsu--assert-mode 'majutsu-log-mode)
  (setq majutsu-log--cached-entries nil)
  (majutsu-row-clear-buffer-data)
  (setq majutsu-log--entry-by-id nil)
  (setq majutsu-log--children-by-id nil)
  (majutsu-log-render))

;;;###autoload
(defun majutsu-log-refresh ()
  "Refresh a majutsu log buffer for the current repository.
When called outside a log buffer, try to refresh an existing log
buffer for the same repository.  If none exists and the command
is invoked interactively, signal a user error instead of
mutating the wrong buffer."
  (interactive)
  (let* ((root (majutsu--buffer-root))
         (buffer (and root (majutsu--resolve-mode-buffer 'majutsu-log-mode root))))
    (cond
     (buffer
      (with-current-buffer buffer
        (majutsu-refresh-buffer)))
     ((called-interactively-p 'interactive)
      (user-error "Not in a Majutsu buffer; open one with `majutsu-log`"))
     (t
      (majutsu--debug "Skipping log refresh: no log buffer for %s" (or root "unknown repo"))))))

(defun majutsu-log-setup-buffer (&optional commit locked)
  "Set up a Majutsu log buffer and optionally focus COMMIT.

When LOCKED is non-nil, avoid reusing existing unlocked log buffers."
  (with-current-buffer
      (pcase-let ((`(,args ,filesets)
                   (majutsu-log--get-value 'majutsu-log-mode 'direct)))
        (majutsu-setup-buffer #'majutsu-log-mode locked
          (majutsu-buffer-log-args (copy-sequence args))
          (majutsu-buffer-log-filesets (copy-sequence filesets))))
    (when commit
      (unless (majutsu--goto-log-entry commit)
        (majutsu-log-goto-@)))
    (current-buffer)))

;;;###autoload
(defun majutsu-log (&optional directory)
  "Open the majutsu log buffer.

If the current directory isn't located within a jj repository, then
prompt for a directory.  If that directory isn't a repository either,
offer to create one using `jj git init`."
  (interactive
   (list (and (or current-prefix-arg (not (majutsu-toplevel)))
              (file-name-as-directory
               (expand-file-name
                (read-directory-name "Repository or directory: "
                                     nil nil nil))))))
  (let* ((default-directory (or directory default-directory))
         (topdir (majutsu-toplevel default-directory)))
    (cond
     (topdir
      (let ((default-directory topdir))
        (majutsu-log-setup-buffer)))
     ((y-or-n-p (format "Create jj repository in %s? "
                        (abbreviate-file-name default-directory)))
      (let* ((dest (file-name-as-directory (expand-file-name default-directory)))
             (default-directory dest)
             (_ (majutsu--assert-usable-jj))
             (jj (majutsu-jj--executable))
             (args (majutsu-process-jj-arguments (list "git" "init"
                                                       (majutsu-convert-filename-for-jj dest))))
             (exit nil)
             (out ""))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (coding-system-for-write 'utf-8-unix))
            (setq exit (apply #'majutsu-process-file jj nil t nil args)))
          (setq out (string-trim (buffer-string))))
        (if (zerop exit)
            (let ((default-directory dest))
              (majutsu-log-setup-buffer))
          (user-error "jj git init failed: %s"
                      (if (string-empty-p out)
                          (format "exit %s" exit)
                        out)))))
     (t
      (user-error "Abort")))))

;;; Commands

(defun majutsu-log--transient-read-revset (prompt initial-input history)
  "Read a log revision argument.

INITIAL-INPUT is the current revision filter and is inserted into the
minibuffer for editing.  Empty input clears the filter."
  (unless current-prefix-arg
    (majutsu-read-optional-revset
     prompt nil initial-input history '("log" "-r"))))

(defun majutsu-log-transient-reset ()
  "Reset log options to defaults."
  (interactive)
  (majutsu-log--set-value 'majutsu-log-mode nil nil)
  (transient-reset))


;;; Arguments
;;;; Prefix Classes

(defclass majutsu-log-prefix (transient-prefix)
  ((history-key :initform 'majutsu-log)
   (major-mode :initform 'majutsu-log-mode)))

;;;; Prefix Methods

(cl-defmethod transient-prefix-value ((obj majutsu-log-prefix))
  "Return (args files) from transient value."
  (let ((args (cl-call-next-method obj)))
    (list (seq-filter #'atom args)
          (cdr (assoc "--" args)))))

(cl-defmethod transient-init-value ((obj majutsu-log-prefix))
  (pcase-let ((`(,args ,filesets)
               (majutsu-log--get-value (oref obj major-mode) 'prefix)))
    (oset obj value (if filesets `(("--" ,@filesets) ,@args) args))))

(cl-defmethod transient-set-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command))))
      (majutsu-log--set-value mode args files)
      (transient--history-push obj)
      (majutsu-refresh))))

(cl-defmethod transient-save-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command))))
      (majutsu-log--set-value mode args files t)
      (transient--history-push obj)
      (majutsu-refresh))))

(cl-defmethod majutsu-transient--save-repository-defaults ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command))))
      (majutsu-transient-save-repository-value 'majutsu-log mode args)
      (put mode 'majutsu-log-current-filesets files)
      (transient--history-push obj)
      (when (eq major-mode mode)
        (setq-local majutsu-buffer-log-args args)
        (setq-local majutsu-buffer-log-filesets files))
      (majutsu-refresh)
      (message "Saved log arguments as repository defaults"))))

(transient-define-argument majutsu-log:-r ()
  :description "Revisions"
  :class 'transient-option
  :shortarg "-r"
  :argument "--revision="
  :prompt "Revisions"
  :always-read t
  :reader #'majutsu-log--transient-read-revset)

(transient-define-argument majutsu-log:--limit ()
  :description "Limit"
  :class 'transient-option
  :key "-n"
  :argument "--limit="
  :reader #'transient-read-number-N+)

(transient-define-argument majutsu-log:--reversed ()
  :description "Reverse order"
  :class 'transient-switch
  :key "-v"
  :argument "--reversed")

(transient-define-argument majutsu-log:--no-graph ()
  :description "Hide graph"
  :class 'transient-switch
  :key "-G"
  :argument "--no-graph")

(transient-define-argument majutsu-log:-- ()
  :description "Limit to filesets"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to filesets"
  :reader #'majutsu-read-files
  :multi-value t)

;;;###autoload(autoload 'majutsu-log-transient "majutsu-log" nil t)
(transient-define-prefix majutsu-log-transient ()
  "Transient interface for adjusting jj log options."
  :man-page "jj-log"
  :class 'majutsu-log-prefix
  :transient-non-suffix t
  [:description
   majutsu-log--transient-description
   ["Revisions"
    (majutsu-log:-r)
    (majutsu-log:--limit)
    (majutsu-log:--reversed)
    (majutsu-log:--no-graph)]
   ["Paths"
    (majutsu-log:--)]
   ["Actions"
    ("g" "buffer" majutsu-log-transient)
    ("s" "buffer and set defaults" transient-set-and-exit)
    ("w" "buffer and save defaults" transient-save-and-exit)
    ("W" "buffer and save repo defaults" majutsu-transient-save-repository-defaults
     :transient t)
    ("0" "Reset options" majutsu-log-transient-reset :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (cond
   ((not (eq transient-current-command 'majutsu-log-transient))
    (transient-setup 'majutsu-log-transient))
   (t
    (unless (derived-mode-p 'majutsu-log-mode)
      (user-error "Not in a Majutsu log buffer"))
    (pcase-let ((`(,args ,filesets) (transient-args transient-current-command)))
      (setq-local majutsu-buffer-log-args args)
      (setq-local majutsu-buffer-log-filesets filesets))
    (majutsu-refresh-buffer))))

;;; _
(provide 'majutsu-log)
;;; majutsu-log.el ends here
