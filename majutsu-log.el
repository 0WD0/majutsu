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

;;; Section Keymaps

(defvar-keymap majutsu-commit-section-map
  :doc "Keymap for `jj-commit' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset)

;;; Log State

(defun majutsu-log--get-value (mode &optional use-buffer-args)
  "Get log arguments for MODE.

Returns (args revsets filesets) triple.  USE-BUFFER-ARGS follows
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
          majutsu-buffer-log-revsets
          majutsu-buffer-log-filesets))
   ((and (memq use-buffer-args '(always selected))
         (when-let* ((buf (majutsu--get-mode-buffer mode (eq use-buffer-args 'selected))))
           (list (buffer-local-value 'majutsu-buffer-log-args buf)
                 (buffer-local-value 'majutsu-buffer-log-revsets buf)
                 (buffer-local-value 'majutsu-buffer-log-filesets buf)))))
   ((plist-member (symbol-plist mode) 'majutsu-log-current-arguments)
    (list (get mode 'majutsu-log-current-arguments)
          (get mode 'majutsu-log-current-revsets)
          (get mode 'majutsu-log-current-filesets)))
   ((when-let* ((elt (assq (intern (format "majutsu-log:%s" mode))
                           transient-values)))
      (list (cdr elt)
            (get mode 'majutsu-log-current-revsets)
            (get mode 'majutsu-log-current-filesets))))
   (t
    (list (get mode 'majutsu-log-default-arguments)
          (get mode 'majutsu-log-default-revsets)
          (get mode 'majutsu-log-default-filesets)))))

(defun majutsu-log--set-value (mode args revsets filesets &optional save)
  "Set current log values for MODE.

When SAVE is non-nil, also persist ARGS using `transient-values'."
  (setq args (seq-remove #'null (flatten-tree args)))
  (setq filesets
        (and filesets
             (seq-remove (lambda (s)
                           (or (null s)
                               (and (stringp s) (string-empty-p s))))
                         (flatten-tree filesets))))
  (put mode 'majutsu-log-current-arguments args)
  (put mode 'majutsu-log-current-revsets revsets)
  (put mode 'majutsu-log-current-filesets filesets)
  (when save
    (setf (alist-get (intern (format "majutsu-log:%s" mode)) transient-values) args)
    (transient-save-values))
  (when (eq major-mode mode)
    (setq-local majutsu-buffer-log-args args)
    (setq-local majutsu-buffer-log-revsets revsets)
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

(defun majutsu-log--args-member-p (args flag)
  (and args (member flag args)))

(defun majutsu-log--args-get-option (args opt)
  "Return OPT's value from ARGS, or nil.
Only supports simple OPT VALUE pairs."
  (let ((pos (seq-position args opt #'equal)))
    (and pos
         (nth (1+ pos) args))))

(defun majutsu-log--args-remove-option (args opt &optional takes-value)
  "Return ARGS with OPT removed.
When TAKES-VALUE is non-nil, also remove the following element."
  (let ((out nil))
    (while args
      (let ((a (pop args)))
        (if (equal a opt)
            (when takes-value
              (pop args))
          (push a out))))
    (nreverse out)))

(defun majutsu-log--args-toggle-flag (args flag)
  (if (member flag args)
      (remove flag args)
    (append args (list flag))))

(defun majutsu-log--args-set-option (args opt value)
  "Set OPT to VALUE inside ARGS (removing existing OPT)."
  (setq args (majutsu-log--args-remove-option args opt t))
  (if value
      (append args (list opt value))
    args))

(defun majutsu-log--summary-parts ()
  "Return a list of human-readable fragments describing current log buffer."
  (pcase-let* ((`(,args ,revsets ,filesets)
                (majutsu-log--get-value 'majutsu-log-mode 'current))
               (parts '()))
    (when revsets
      (push (format "rev=%s" revsets) parts))
    (when-let* ((limit (majutsu-log--args-get-option args "-n")))
      (push (format "limit=%s" limit) parts))
    (when (majutsu-log--args-member-p args "--reversed")
      (push "reversed" parts))
    (when (majutsu-log--args-member-p args "--no-graph")
      (push "no-graph" parts))
    (when filesets
      (push (if (= (length filesets) 1)
                (format "path=%s" (car filesets))
              (format "paths=%d" (length filesets)))
            parts))
    (nreverse parts)))

(defun majutsu-log--format-summary (prefix)
  "Return PREFIX annotated with active log state summary."
  (let ((parts (majutsu-log--summary-parts)))
    (if parts
        (format "%s (%s)" prefix (string-join parts ", "))
      prefix)))

(defun majutsu-log--heading-string ()
  "Return heading string for the log section."
  (majutsu-log--format-summary "Log Graph"))

(defun majutsu-log--transient-description ()
  "Return description string for the log transient."
  (majutsu-log--format-summary "JJ Log"))

;;; Log Template

(defconst majutsu-log--field-separator "\x1e"
  "Separator character inserted between fields inside each module payload.")

(defconst majutsu-log--field-list-separator "\x1c"
  "Separator character inserted between list items inside a single field value.")

(defconst majutsu-log--field-line-separator "\x1f"
  "Encoded newline separator used inside template field payloads.
Log records are transported as single lines, then this separator is
decoded back to literal newlines after field splitting.")

(defconst majutsu-log--record-marker "\x1d"
  "Control marker prefix for module boundaries inside log output.")

(defconst majutsu-log--entry-start-token (concat majutsu-log--record-marker "S")
  "Marker that starts a commit entry and heading payload.")

(defconst majutsu-log--entry-tail-token (concat majutsu-log--record-marker "T")
  "Marker that starts the tail payload.")

(defconst majutsu-log--entry-body-token (concat majutsu-log--record-marker "B")
  "Marker that starts the body payload.")

(defconst majutsu-log--entry-meta-token (concat majutsu-log--record-marker "M")
  "Marker that starts the metadata payload.")

(defconst majutsu-log--entry-end-token (concat majutsu-log--record-marker "E")
  "Marker that terminates a commit entry.")

(defconst majutsu-log--module-order '(heading tail body metadata)
  "Module parse/render order for sequential log payloads.")

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
            (majutsu-log--split-by-separator value majutsu-log--field-list-separator))))

(defun majutsu-log-post-remove-ago-suffix (value &optional _ctx)
  "Trim a trailing \\=' ago\\=' suffix from VALUE."
  (if (stringp value)
      (string-remove-suffix " ago" value)
    value))

(defun majutsu-log--default-module-for-field (field)
  "Return default module symbol for FIELD."
  (or (alist-get field majutsu-log--field-default-modules nil nil #'eq)
      (user-error "Field %S requires explicit :module" field)))

(defun majutsu-log--default-postprocessors-for-field (field)
  "Return default postprocessors for FIELD."
  (append majutsu-log--default-column-postprocessors
          (alist-get field majutsu-log--field-default-postprocessors nil nil #'eq)))

(defun majutsu-log--normalize-postprocessors (post field)
  "Normalize POST value for FIELD into a function list.

Omitted or `:default' values use the field defaults. Explicit functions are
appended after those defaults, while nil disables column postprocessing."
  (let* ((defaults (majutsu-log--default-postprocessors-for-field field))
         (fns (cond
               ((eq post :default) defaults)
               ((null post) nil)
               ((functionp post) (append defaults (list post)))
               ((and (listp post) (seq-every-p #'functionp post))
                (append defaults post))
               (t (user-error "Column %S has invalid :post %S" field post)))))
    (dolist (fn fns)
      (unless (functionp fn)
        (user-error "Column %S has non-callable postprocessor %S" field fn)))
    fns))

(defun majutsu-log--normalize-column-spec (spec)
  "Normalize a single column SPEC into a plist with defaults."
  (let* ((col (cond
               ((and (plistp spec) (plist-get spec :field)) spec)
               ((symbolp spec) (list :field spec))
               (t (user-error "Invalid column spec: %S" spec))))
         (field (plist-get col :field))
         (module (if (plist-member col :module)
                     (plist-get col :module)
                   (majutsu-log--default-module-for-field field)))
         (face (if (plist-member col :face)
                   (plist-get col :face)
                 t))
         (post (if (plist-member col :post)
                   (plist-get col :post)
                 :default)))
    (setq module (if (keywordp module)
                     (intern (substring (symbol-name module) 1))
                   module))
    (unless (memq module majutsu-log--module-order)
      (user-error "Column %S has invalid :module %S" field module))
    (unless (or (eq face t) (null face) (symbolp face))
      (user-error "Column %S has invalid :face %S" field face))
    (list :field field
          :module module
          :face face
          :post (majutsu-log--normalize-postprocessors post field))))

(defun majutsu-log--ensure-required-columns (columns)
  "Ensure required columns are present in COLUMNS list.
Missing required fields are appended with defaults."
  (let ((present (mapcar (lambda (c) (plist-get c :field)) columns)))
    (dolist (req majutsu-log--required-columns)
      (unless (memq req present)
        (setq columns (append columns (list (majutsu-log--normalize-column-spec req))))))
    columns))

(defun majutsu-log--module-columns (compiled module)
  "Return compiled column specs for MODULE from COMPILED metadata."
  (alist-get module (plist-get compiled :module-columns) nil nil #'eq))

(defun majutsu-log--assign-column-instances (columns)
  "Return COLUMNS with stable per-instance ids assigned."
  (cl-loop for column in columns
           for idx from 0
           collect (plist-put (copy-sequence column) :instance idx)))

(defun majutsu-log--column-template (field)
  "Return majutsu-template form for FIELD.
Looks up `majutsu-log-template-FIELD'."
  (let ((var (intern-soft (format "majutsu-log-template-%s" field))))
    (if (and var (boundp var))
        (symbol-value var)
      (user-error "Unknown column field %S" field))))

(defun majutsu-log--build-module-template-form (templates)
  "Return a template form joining TEMPLATES with field separators."
  (cond
   ((null templates) "")
   ((null (cdr templates)) (car templates))
   (t
    (let ((forms nil)
          (first t))
      (dolist (template templates)
        (unless first
          (setq forms (append forms (list majutsu-log--field-separator))))
        (setq first nil)
        (setq forms (append forms (list template))))
      (cons :concat forms)))))

(defun majutsu-log--compile-columns (&optional columns)
  "Compile COLUMNS (or `majutsu-log-commit-columns') into a jj template string.
Returns a plist with :template, :columns, and :module-columns."
  (let* ((normalized (mapcar #'majutsu-log--normalize-column-spec
                             (or columns majutsu-log-commit-columns)))
         (complete (majutsu-log--assign-column-instances
                    (majutsu-log--ensure-required-columns normalized)))
         (module-columns
          (mapcar (lambda (module)
                    (cons module
                          (seq-filter (lambda (c)
                                        (eq (plist-get c :module) module))
                                      complete)))
                  majutsu-log--module-order))
         (heading-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'heading module-columns nil nil #'eq))))
         (tail-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'tail module-columns nil nil #'eq))))
         (body-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'body module-columns nil nil #'eq))))
         (meta-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'metadata module-columns nil nil #'eq))))
         (compiled
          (majutsu-tpl
           `[:concat
             ,majutsu-log--entry-start-token
             ,heading-form
             ,majutsu-log--entry-tail-token
             ,tail-form
             ,majutsu-log--entry-body-token
             ,body-form
             ,majutsu-log--entry-meta-token
             ,meta-form
             ,majutsu-log--entry-end-token
             "\n"])))
    (list :template compiled
          :columns complete
          :module-columns module-columns)))

(defun majutsu-log--ensure-template ()
  "Return cached compiled template structure, recomputing if necessary."
  (or majutsu-log--compiled-template-cache
      (setq majutsu-log--compiled-template-cache
            (majutsu-log--compile-columns majutsu-log-commit-columns))))

(defun majutsu-log--build-args ()
  "Build argument list for `jj log' using current log variables."
  (pcase-let ((`(,args ,revsets ,filesets)
               (majutsu-log--get-value 'majutsu-log-mode 'current)))
    (let ((cmd '("log")))
      (setq cmd (append cmd args))
      (when revsets
        (setq cmd (append cmd (list "-r" revsets))))
      (setq cmd (append cmd (list "-T" (plist-get (majutsu-log--ensure-template) :template))))
      (setq cmd (append cmd filesets))
      cmd)))

;;; Log Parsing

(defun majutsu-log--split-by-separator (value separator)
  "Split VALUE by one-char string SEPARATOR, preserving empty fields."
  (if (not (stringp value))
      nil
    (let ((start 0)
          (len (length value))
          (sep (aref separator 0))
          out)
      (dotimes (idx len)
        (when (eq (aref value idx) sep)
          (push (substring value start idx) out)
          (setq start (1+ idx))))
      (push (substring value start len) out)
      (nreverse out))))

(defun majutsu-log--join-lines (lines)
  "Join LINES with literal newlines, preserving string properties."
  (if (null lines)
      ""
    (let ((out (car lines)))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" line))))))

(defun majutsu-log--line-token-position (token bol eol &optional start)
  "Return start position of TOKEN between BOL and EOL, or nil."
  (save-excursion
    (goto-char (or start bol))
    (when (search-forward token eol t)
      (- (point) (length token)))))

(defun majutsu-log--parse-trailing-payloads (payload)
  "Parse trailing payload segments from PAYLOAD string.

PAYLOAD is expected to start with either `majutsu-log--entry-tail-token'
(new format) or `majutsu-log--entry-body-token' (legacy format)."
  (cond
   ((string-prefix-p majutsu-log--entry-tail-token payload)
    (let* ((tail-start (length majutsu-log--entry-tail-token))
           (body-pos (string-match (regexp-quote majutsu-log--entry-body-token)
                                   payload tail-start))
           (meta-pos (and body-pos
                          (string-match (regexp-quote majutsu-log--entry-meta-token)
                                        payload (+ body-pos (length majutsu-log--entry-body-token)))))
           (end-pos (and meta-pos
                         (string-match (regexp-quote majutsu-log--entry-end-token)
                                       payload (+ meta-pos (length majutsu-log--entry-meta-token))))))
      (when (and body-pos meta-pos end-pos)
        (let ((trailing (substring payload (+ end-pos (length majutsu-log--entry-end-token)))))
          (when (string-empty-p trailing)
            (list :tail (substring payload tail-start body-pos)
                  :body (substring payload
                                   (+ body-pos (length majutsu-log--entry-body-token))
                                   meta-pos)
                  :metadata (substring payload
                                       (+ meta-pos (length majutsu-log--entry-meta-token))
                                       end-pos)))))))
   ((string-prefix-p majutsu-log--entry-body-token payload)
    (let* ((body-start (length majutsu-log--entry-body-token))
           (meta-pos (string-match (regexp-quote majutsu-log--entry-meta-token)
                                   payload body-start))
           (end-pos (and meta-pos
                         (string-match (regexp-quote majutsu-log--entry-end-token)
                                       payload (+ meta-pos (length majutsu-log--entry-meta-token))))))
      (when (and meta-pos end-pos)
        (let ((trailing (substring payload (+ end-pos (length majutsu-log--entry-end-token)))))
          (when (string-empty-p trailing)
            (list :tail ""
                  :body (substring payload body-start meta-pos)
                  :metadata (substring payload
                                       (+ meta-pos (length majutsu-log--entry-meta-token))
                                       end-pos)))))))))

(defun majutsu-log--split-module-values (payload count)
  "Split PAYLOAD into COUNT field values using `majutsu-log--field-separator'."
  (if (<= count 0)
      nil
    (let ((values (majutsu-log--split-by-separator (or payload "") majutsu-log--field-separator)))
      (cond
       ((< (length values) count)
        (append values (make-list (- count (length values)) "")))
       ((> (length values) count)
        (seq-take values count))
       (t values)))))

(defun majutsu-log--decode-transport-value (value)
  "Decode transport-level escapes inside VALUE before column postprocessing."
  (majutsu-log-post-decode-line-separator value))

(defun majutsu-log--apply-postprocessor (fn value ctx)
  "Apply postprocessor FN to VALUE with context CTX.

FN may accept either (VALUE) or (VALUE CTX). Errors return VALUE unchanged."
  (condition-case err
      (condition-case _
          (funcall fn value ctx)
        (wrong-number-of-arguments
         (funcall fn value)))
    (error
     (majutsu--debug "majutsu-log postprocessor failed (%S on %S): %s"
                     fn (plist-get ctx :field) (error-message-string err))
     value)))

(defun majutsu-log--apply-postprocessors (value postprocessors ctx)
  "Apply POSTPROCESSORS to VALUE with CTX sequentially."
  (let ((out value))
    (dolist (fn postprocessors out)
      (setq out (majutsu-log--apply-postprocessor fn out ctx)))))

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

(defun majutsu-log--canonical-field-value (field value)
  "Return canonical semantic value for FIELD based on VALUE."
  (majutsu-log--apply-postprocessors
   value
   (majutsu-log--default-postprocessors-for-field field)
   (list :field field :module 'canonical :raw-value value :canonical t)))

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

(defun majutsu-log--record-column-value (entry column value)
  "Record per-instance VALUE for COLUMN onto ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values)))
    (when instance
      (setf (alist-get instance column-values nil nil #'eql) value)
      (setq entry (plist-put entry :column-values column-values)))
    entry))

(defun majutsu-log--record-module-fields (entry module payload compiled)
  "Record MODULE PAYLOAD values into ENTRY using COMPILED module layout."
  (let* ((columns (majutsu-log--module-columns compiled module))
         (values (majutsu-log--split-module-values payload (length columns)))
         (stored nil))
    (cl-loop for column in columns
             for raw-value in values
             do (let* ((field (plist-get column :field))
                       (decoded (majutsu-log--decode-transport-value raw-value))
                       (canonical (majutsu-log--canonical-field-value field decoded)))
                  (setq entry (majutsu-log--record-field entry field canonical))
                  (let* ((ctx (list :field field
                                    :module module
                                    :column column
                                    :entry entry
                                    :raw-value decoded
                                    :canonical-value canonical))
                         (out (majutsu-log--apply-postprocessors decoded
                                                                 (plist-get column :post)
                                                                 ctx)))
                    (setq entry (majutsu-log--record-column-value entry column out))
                    (push out stored))))
    (let ((modules (plist-get entry :modules)))
      (setf (alist-get module modules nil nil #'eq) (nreverse stored))
      (setq entry (plist-put entry :modules modules)))
    entry))

(defun majutsu-log--parse-entry-at-point (compiled)
  "Parse one sequentially-encoded log entry at point using COMPILED.

Point must be at a line potentially containing `majutsu-log--entry-start-token'.
Returns entry plist and moves point past the consumed entry, or nil."
  (let* ((entry-beg (line-beginning-position))
         (bol entry-beg)
         (eol (line-end-position))
         (start-pos (majutsu-log--line-token-position majutsu-log--entry-start-token bol eol)))
    (when start-pos
      (let* ((indent (- start-pos bol))
             (heading-prefixes nil)
             (heading-segments nil)
             (trailing-payload nil)
             (done nil)
             (first-line t))
        (while (and (not done) (not (eobp)))
          (setq bol (line-beginning-position)
                eol (line-end-position))
          (let* ((prefix-end (min (+ bol indent) eol))
                 (prefix (buffer-substring bol prefix-end))
                 (content-start (if first-line
                                    (+ start-pos (length majutsu-log--entry-start-token))
                                  prefix-end))
                 (tail-pos (majutsu-log--line-token-position
                            majutsu-log--entry-tail-token bol eol content-start))
                 (body-pos (and (null tail-pos)
                                (majutsu-log--line-token-position
                                 majutsu-log--entry-body-token bol eol content-start)))
                 (segment-pos (or tail-pos body-pos)))
            (if segment-pos
                (progn
                  (push prefix heading-prefixes)
                  (push (buffer-substring content-start segment-pos) heading-segments)
                  (setq trailing-payload (buffer-substring segment-pos eol))
                  (setq done t)
                  (forward-line 1))
              (push prefix heading-prefixes)
              (push (buffer-substring content-start eol) heading-segments)
              (forward-line 1)
              (when (eobp)
                (setq done :incomplete))))
          (setq first-line nil))
        (when (eq done t)
          (when-let* ((payloads (majutsu-log--parse-trailing-payloads trailing-payload)))
            (let* ((entry (list :beg entry-beg
                                :indent indent
                                :columns nil
                                :column-values nil
                                :modules nil
                                :heading-prefixes (nreverse heading-prefixes)))
                   (heading-payload (majutsu-log--join-lines (nreverse heading-segments))))
              (setq entry (majutsu-log--record-module-fields entry 'heading heading-payload compiled))
              (setq entry (majutsu-log--record-module-fields
                           entry 'tail (plist-get payloads :tail) compiled))
              (setq entry (majutsu-log--record-module-fields
                           entry 'body (plist-get payloads :body) compiled))
              (setq entry (majutsu-log--record-module-fields
                           entry 'metadata (plist-get payloads :metadata) compiled))
              (let ((suffix-lines nil))
                ;; Preserve graph continuation lines between the current entry's
                ;; end marker and the next entry start marker. These lines stay
                ;; visible as part of the current section heading area.
                (while (and (not (eobp))
                            (let ((next-bol (line-beginning-position))
                                  (next-eol (line-end-position)))
                              (not (majutsu-log--line-token-position
                                    majutsu-log--entry-start-token next-bol next-eol))))
                  (push (buffer-substring (line-beginning-position) (line-end-position))
                        suffix-lines)
                  (forward-line 1))
                (setq entry (plist-put entry :suffix-lines (nreverse suffix-lines)))
                (setq entry (plist-put entry :end (point))))
              entry)))))))

(defun majutsu-log--parse-entries-in-buffer (compiled)
  "Parse all sequentially-encoded log entries in current buffer using COMPILED."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((entry (majutsu-log--parse-entry-at-point compiled)))
          (if entry
              (push entry entries)
            (forward-line 1)))))
    (nreverse entries)))

(defun majutsu-log--apply-line-prefix-span (start end line-prefix-str &optional wrap-prefix-str)
  "Apply display-only line/wrap prefix strings to START..END."
  (when (< start end)
    (add-text-properties
     start end
     (list 'line-prefix (or line-prefix-str "")
           'wrap-prefix (or wrap-prefix-str line-prefix-str "")))))

(defun majutsu-log--insert-prefixed-line (content prefix)
  "Insert CONTENT as one line with display-only PREFIX."
  (let ((start (point)))
    (insert (or content "") "\n")
    (majutsu-log--apply-line-prefix-span start (point) prefix)))

(defun majutsu-log--split-prefix-line (line prefix-width)
  "Split LINE into (PREFIX . CONTENT) using PREFIX-WIDTH characters."
  (let* ((width (max 0 (min (or prefix-width 0) (length (or line "")))))
         (text (or line "")))
    (cons (substring text 0 width)
          (substring text width))))

(defun majutsu-log--entry-column (entry field)
  "Return canonical value for FIELD stored on ENTRY."
  (alist-get field (plist-get entry :columns) nil nil #'eq))

(defun majutsu-log--entry-column-value (entry column)
  "Return per-instance value for COLUMN stored on ENTRY.

Fallback to the canonical field value when ENTRY predates per-instance storage."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values))
         (missing (make-symbol "majutsu-log-missing-instance"))
         (value (if instance
                    (alist-get instance column-values missing nil #'eql)
                  missing)))
    (if (eq value missing)
        (majutsu-log--entry-column entry (plist-get column :field))
      value)))

(defun majutsu-log--display-string (value)
  "Return VALUE converted to a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value)
    (mapconcat #'majutsu-log--display-string value " "))
   (t (format "%s" value))))

(defun majutsu-log--apply-face-policy (value face)
  "Apply FACE policy to VALUE and return display string."
  (let ((v (or value "")))
    (cond
     ((eq face t) v)
     ((null face) (substring-no-properties v))
     (t (propertize (substring-no-properties v) 'font-lock-face face)))))

(defun majutsu-log--content-properties (entry-id module &optional column)
  "Return content text properties for ENTRY-ID in MODULE.
When COLUMN is non-nil, also include field and column-instance identity."
  (append `(majutsu-log-module ,module
            majutsu-log-entry-id ,entry-id)
          (when column
            `(majutsu-log-field ,(plist-get column :field)
              majutsu-log-column ,(plist-get column :instance)))))

(defun majutsu-log--decoration-properties (entry-id module decoration)
  "Return decoration text properties for ENTRY-ID in MODULE."
  `(majutsu-log-module ,module
    majutsu-log-entry-id ,entry-id
    majutsu-log-decoration ,decoration))

(defun majutsu-log--propertize-content (text entry-id module &optional column)
  "Return TEXT tagged as MODULE content for ENTRY-ID and COLUMN."
  (if (stringp text)
      (apply #'propertize text
             (majutsu-log--content-properties entry-id module column))
    text))

(defun majutsu-log--propertize-decoration (text entry-id module decoration)
  "Return TEXT tagged as MODULE DECORATION for ENTRY-ID."
  (if (stringp text)
      (apply #'propertize text
             (majutsu-log--decoration-properties entry-id module decoration))
    text))

(defun majutsu-log--concat-heading-parts (parts)
  "Concatenate heading PARTS without adding spaces after newlines."
  (let ((out ""))
    (dolist (part parts out)
      (unless (string-empty-p part)
        (let ((need-space
               (and (> (length out) 0)
                    (not (eq (aref out (1- (length out))) ?\n))
                    (not (eq (aref part 0) ?\n)))))
          (setq out (concat out (if need-space " " "") part)))))))

(defun majutsu-log--single-line-string (value)
  "Return VALUE flattened to a single display line."
  (if (stringp value)
      (string-trim (replace-regexp-in-string "[\n\r]+" " " value nil t))
    value))

(defun majutsu-log--render-column-text (entry column &optional plain)
  "Return rendered text for ENTRY COLUMN.
When PLAIN is non-nil, strip all text properties from the result."
  (let* ((module (plist-get column :module))
         (face (plist-get column :face))
         (value (majutsu-log--display-string
                 (majutsu-log--entry-column-value entry column))))
    (when (eq module 'tail)
      (setq value (majutsu-log--single-line-string value)))
    (if plain
        (substring-no-properties value)
      (majutsu-log--apply-face-policy value face))))

(defun majutsu-log--render-module-parts (entry compiled module &optional annotate plain)
  "Return rendered ENTRY parts for MODULE using COMPILED.
When ANNOTATE is non-nil, add span properties for field/column identity.
When PLAIN is non-nil, strip presentation text properties first."
  (let ((entry-id (majutsu-log--entry-id entry))
        parts)
    (dolist (column (majutsu-log--module-columns compiled module))
      (let ((value (majutsu-log--render-column-text entry column plain)))
        (unless (if (eq module 'body)
                    (string-empty-p (string-trim value))
                  (string-empty-p value))
          (push (if annotate
                    (majutsu-log--propertize-content value entry-id module column)
                  value)
                parts))))
    (nreverse parts)))

(defun majutsu-log--render-heading-content (entry compiled &optional annotate plain)
  "Render ENTRY heading module content without graph prefixes."
  (let ((parts (majutsu-log--render-module-parts entry compiled 'heading annotate plain)))
    (if parts
        (majutsu-log--concat-heading-parts parts)
      "")))

(defun majutsu-log--render-heading-content-lines (entry compiled &optional annotate plain)
  "Render ENTRY heading module content lines without graph prefixes."
  (majutsu-log--split-by-separator
   (majutsu-log--render-heading-content entry compiled annotate plain)
   "\n"))

(defun majutsu-log--render-heading-lines (entry compiled)
  "Render ENTRY heading module as visible lines with graph prefixes."
  (let* ((content-lines (majutsu-log--render-heading-content-lines entry compiled))
         (prefixes (or (plist-get entry :heading-prefixes) (list "")))
         (last-prefix (or (car (last prefixes)) ""))
         (count (max (length content-lines) (length prefixes)))
         out)
    (cl-loop for idx below count
             do (let ((prefix (or (nth idx prefixes) last-prefix))
                      (line (or (nth idx content-lines) "")))
                  (push (concat prefix line) out)))
    (nreverse out)))

(defun majutsu-log--render-tail (entry compiled &optional annotate plain)
  "Render ENTRY tail module as a single-line auxiliary string."
  (let ((parts (majutsu-log--render-module-parts entry compiled 'tail annotate plain)))
    (when parts
      (majutsu-log--concat-heading-parts parts))))

(defun majutsu-log--render-body (entry compiled &optional annotate plain)
  "Render ENTRY body module as foldable multiline content."
  (let ((parts (majutsu-log--render-module-parts entry compiled 'body annotate plain)))
    (when parts
      (string-join parts "\n"))))

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

(defun majutsu-log--text-property-near-point (property &optional pos)
  "Return PROPERTY near POS, preferring the previous character.
This makes point at the end of a field behave like point on that field."
  (let ((pos (or pos (point))))
    (or (and (> pos (point-min))
             (get-text-property (1- pos) property))
        (get-text-property pos property))))

(defun majutsu-log--entry-at-point ()
  "Return the parsed log entry at point, or nil if unavailable."
  (or (when-let* ((entry-id (majutsu-log--text-property-near-point
                             'majutsu-log-entry-id)))
        (majutsu-log--entry-for-id entry-id))
      (when-let* ((entry-id (magit-section-value-if 'jj-commit)))
        (majutsu-log--entry-for-id entry-id))))

(defun majutsu-log--current-compiled ()
  "Return compiled column/layout metadata for the current buffer."
  (or majutsu-log--buffer-compiled
      majutsu-log--compiled-template-cache
      (majutsu-log--ensure-template)))

(defun majutsu-log--compiled-column-by-instance (compiled instance)
  "Return column spec from COMPILED identified by INSTANCE."
  (seq-find (lambda (column)
              (eql (plist-get column :instance) instance))
            (plist-get compiled :columns)))

(defun majutsu-log--field-copy-string (value)
  "Return canonical clipboard text for field VALUE."
  (cond
   ((null value) "")
   ((stringp value) (substring-no-properties value))
   ((listp value) (mapconcat #'majutsu-log--field-copy-string value "\n"))
   (t (format "%s" value))))

(defun majutsu-log--field-value-present-p (value)
  "Return non-nil if canonical field VALUE should be offered for copying."
  (cond
   ((null value) nil)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   (t t)))

(defun majutsu-log--entry-field-value (entry field &optional missing)
  "Return canonical FIELD value from ENTRY, or MISSING when unavailable."
  (alist-get field (plist-get entry :columns) missing nil #'eq))

(defun majutsu-log--entry-field-candidate-preview (entry field)
  "Return one-line preview string for FIELD on ENTRY."
  (let* ((text (majutsu-log--field-copy-string
                (majutsu-log--entry-field-value entry field)))
         (text (replace-regexp-in-string "[\n\r\t ]+" " " text nil t)))
    (if (> (length text) 48)
        (concat (substring text 0 48) "…")
      text)))

(defun majutsu-log--entry-copyable-fields (entry compiled)
  "Return copyable canonical field symbols for ENTRY using COMPILED order."
  (let ((fields nil)
        (seen nil))
    (dolist (column (plist-get compiled :columns))
      (let* ((field (plist-get column :field))
             (value (majutsu-log--entry-field-value entry field :majutsu-missing)))
        (when (and (not (memq field seen))
                   (not (eq value :majutsu-missing))
                   (majutsu-log--field-value-present-p value))
          (push field seen)
          (push field fields))))
    (dolist (cell (plist-get entry :columns))
      (let ((field (car cell))
            (value (cdr cell)))
        (when (and (not (memq field seen))
                   (majutsu-log--field-value-present-p value))
          (push field seen)
          (push field fields))))
    (nreverse fields)))

(defun majutsu-log--read-entry-field (entry compiled &optional prompt)
  "Read one canonical field from ENTRY using COMPILED.
When PROMPT is nil, use a default log-field prompt."
  (let* ((default-field (majutsu-log--text-property-near-point 'majutsu-log-field))
         (candidates (mapcar (lambda (field)
                               (cons (format "%s\t%s"
                                             field
                                             (majutsu-log--entry-field-candidate-preview entry field))
                                     field))
                             (majutsu-log--entry-copyable-fields entry compiled)))
         (default (car (rassoc default-field candidates)))
         (choice (completing-read (or prompt "Copy log field: ")
                                  (mapcar #'car candidates)
                                  nil t nil nil default)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown log field %S" choice))))

(defun majutsu-log--copy-entry-field-value (entry field)
  "Copy canonical FIELD value from ENTRY to the kill ring."
  (let ((value (majutsu-log--entry-field-value entry field :majutsu-missing)))
    (when (eq value :majutsu-missing)
      (user-error "Field %s is unavailable for the current entry" field))
    (unless (majutsu-log--field-value-present-p value)
      (user-error "Field %s is empty for the current entry" field))
    (majutsu-log--copy-string (majutsu-log--field-copy-string value))))

(defun majutsu-log--copy-string (string)
  "Copy STRING to the kill ring and echo it."
  (kill-new string)
  (message "%s" string))

;;;###autoload
(defun majutsu-copy-section-value ()
  "Copy the current section's stable value.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (if-let* ((section (magit-current-section))
              (value (oref section value)))
        (majutsu-log--copy-string (format "%s" value))
      (user-error "No section value at point"))))

;;;###autoload
(defun majutsu-log-copy-field ()
  "Copy the rendered value of the log field at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-log--current-compiled))
           (entry (majutsu-log--entry-at-point))
           (instance (majutsu-log--text-property-near-point 'majutsu-log-column))
           (field (majutsu-log--text-property-near-point 'majutsu-log-field))
           (module (majutsu-log--text-property-near-point 'majutsu-log-module))
           (column (or (and instance
                            (majutsu-log--compiled-column-by-instance compiled instance))
                       (and field module
                            (seq-find (lambda (candidate)
                                        (and (eq (plist-get candidate :field) field)
                                             (eq (plist-get candidate :module) module)))
                                      (plist-get compiled :columns))))))
      (unless entry
        (user-error "No log entry at point"))
      (unless column
        (user-error "No log field at point"))
      (majutsu-log--copy-string
       (majutsu-log--render-column-text entry column t)))))

;;;###autoload
(defun majutsu-log-copy-module ()
  "Copy the rendered log module at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-log--current-compiled))
           (entry (majutsu-log--entry-at-point))
           (module (majutsu-log--text-property-near-point 'majutsu-log-module))
           (text (pcase module
                   ('heading (majutsu-log--render-heading-content entry compiled nil t))
                   ('tail (or (majutsu-log--render-tail entry compiled nil t) ""))
                   ('body (or (majutsu-log--render-body entry compiled nil t) ""))
                   (_ nil))))
      (unless entry
        (user-error "No log entry at point"))
      (unless text
        (user-error "No log module at point"))
      (majutsu-log--copy-string text))))

;;;###autoload
(defun majutsu-log-copy-entry-field ()
  "Copy a canonical field from the current log entry.

Unlike `majutsu-log-copy-field', this can target fields that are parsed and
stored on the entry but not currently visible in the rendered log line.  When
called interactively, prompt with completion over the current entry's
available canonical fields.  If the region is active, copy it literally using
`copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-log--current-compiled))
           (entry (or (majutsu-log--entry-at-point)
                      (user-error "No log entry at point")))
           (field (majutsu-log--read-entry-field entry compiled)))
      (majutsu-log--copy-entry-field-value entry field))))

;;;###autoload
(defun majutsu-log-copy-commit-id ()
  "Copy the current entry's commit hash.

This copies the canonical hidden `commit-id' field, even when it is not shown
in the visible log layout.  If the region is active, copy it literally using
`copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let ((entry (or (majutsu-log--entry-at-point)
                     (user-error "No log entry at point"))))
      (majutsu-log--copy-entry-field-value entry 'commit-id))))

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

(defconst majutsu-log--tail-terminal-padding 1
  "Reserved terminal columns kept to the right of tail-aligned text.")

(defun majutsu-log--tail-owner-window ()
  "Return the window that currently owns tail layout for the current buffer.
Prefer the selected window when it is showing the current buffer, falling back
to any visible window for the buffer."
  (let ((selected (selected-window)))
    (if (eq (window-buffer selected) (current-buffer))
        selected
      (get-buffer-window (current-buffer) 0))))

(defun majutsu-log--tail-align-to-width (width &optional window)
  "Return a `:align-to' target that right-aligns content of WIDTH.
On graphical displays, return an absolute pixel column from the left edge of
WINDOW's text area when WINDOW is available.  On terminals, return an ordinary
column while reserving a small safety gap at the right edge.  Fall back to
right-relative alignment when WINDOW is unavailable."
  (cond
   ((and (display-graphic-p) window)
    (list (max 0 (- (window-body-width window t) width))))
   ((display-graphic-p)
    `(- right (,width)))
   (window
    (max 0 (- (window-body-width window)
              width
              majutsu-log--tail-terminal-padding)))
   (t
    `(- right ,(+ width majutsu-log--tail-terminal-padding)))))

(defun majutsu-log--tail-spacer-display (tail &optional window)
  "Return the spacer display spec used to right-align TAIL."
  (setq window (or window (majutsu-log--tail-owner-window)))
  `(space :align-to
    ,(majutsu-log--tail-align-to-width
      (if (and (display-graphic-p)
               (fboundp 'string-pixel-width))
          (string-pixel-width tail (current-buffer))
        (string-width tail))
      window)))

(defun majutsu-log--tail-width-at (pos &optional window)
  "Return displayed width of tail text following spacer at POS.
Use WINDOW when provided, falling back to the current tail owner window.  If no
suitable window is available, fall back to string-based measurement."
  (let ((tail (save-excursion
                (goto-char pos)
                (buffer-substring (1+ pos) (line-end-position))))
        (window (or window (majutsu-log--tail-owner-window))))
    (if (and (display-graphic-p) window)
        (save-excursion
          (goto-char (1+ pos))
          (car (window-text-pixel-size window (point) (line-end-position) t)))
      (if (and (display-graphic-p)
               (fboundp 'string-pixel-width))
          (save-excursion
            (goto-char pos)
            (string-pixel-width tail (current-buffer)))
        (string-width tail)))))

(defun majutsu-log--refresh-tail-spacers (&optional beg end window)
  "Recompute right-alignment display specs for tail spacers in BEG..END.
When WINDOW is non-nil, use that window as the tail layout owner."
  (setq window (or window (majutsu-log--tail-owner-window)))
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (save-excursion
        (let ((pos (or beg (point-min)))
              (end (or end (point-max))))
          (while (and (< pos end)
                      (setq pos (text-property-any pos end 'majutsu-log-tail-spacer t)))
            (put-text-property pos (1+ pos) 'display
                               `(space :align-to
                                 ,(majutsu-log--tail-align-to-width
                                   (majutsu-log--tail-width-at pos window)
                                   window)))
            (setq pos (1+ pos))))))))

(defun majutsu-log--refresh-tail-window (&optional window)
  "Refresh tail alignment for the current log buffer using WINDOW.
When WINDOW is nil, use `majutsu-log--tail-owner-window'."
  (when (derived-mode-p 'majutsu-log-mode)
    (setq window (or window (majutsu-log--tail-owner-window)))
    (majutsu-log--refresh-tail-spacers nil nil window)
    (when window
      (force-window-update window))))

(defun majutsu-log--after-text-scale-change ()
  "Refresh lightweight display alignment after text scaling changes."
  (majutsu-log--refresh-tail-window (selected-window)))

(defun majutsu-log--after-window-size-change (window)
  "Refresh tail alignment after the selected log WINDOW changes size."
  (when (eq window (selected-window))
    (majutsu-log--refresh-tail-window window)))

(defun majutsu-log--insert-heading-anchor-line (anchor-left tail entry-id prefix)
  "Insert ANCHOR-LEFT and right-aligned TAIL on one prefixed line."
  (let ((start (point))
        (spacer-pos nil)
        (window (majutsu-log--tail-owner-window)))
    (insert anchor-left)
    (when (and (stringp tail) (not (string-empty-p tail)))
      (setq spacer-pos (point))
      (insert " ")
      (add-text-properties
       spacer-pos (point)
       `(majutsu-log-module tail
         majutsu-log-entry-id ,entry-id
         majutsu-log-decoration tail-spacer
         majutsu-log-tail-spacer t
         display ,(majutsu-log--tail-spacer-display tail window)))
      (insert tail))
    (insert "\n")
    (majutsu-log--apply-line-prefix-span start (point) prefix)
    (when spacer-pos
      (majutsu-log--refresh-tail-spacers spacer-pos (1+ spacer-pos) window))))

(defun majutsu-log--string-has-module-p (string module)
  "Return non-nil if STRING contains text marked with MODULE."
  (let ((pos 0)
        found)
    (while (and (< pos (length string)) (not found))
      (setq found (eq (get-text-property pos 'majutsu-log-module string) module)
            pos (or (next-single-property-change pos 'majutsu-log-module string)
                    (length string))))
    found))

(defun majutsu-log--string-remove-module (string module)
  "Return STRING with text marked as MODULE removed."
  (let ((pos 0)
        parts)
    (while (< pos (length string))
      (let* ((next (or (next-single-property-change pos 'majutsu-log-module string)
                       (length string)))
             (current (get-text-property pos 'majutsu-log-module string)))
        (unless (eq current module)
          (push (substring string pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun majutsu-log--cleanup-copied-string (string)
  "Strip Majutsu log UI properties from copied STRING."
  (when (stringp string)
    (remove-list-of-text-properties
     0 (length string)
     '(majutsu-log-module
       majutsu-log-field
       majutsu-log-column
       majutsu-log-entry-id
       majutsu-log-decoration
       majutsu-log-tail-spacer
       line-prefix
       wrap-prefix
       display)
     string))
  string)

(defun majutsu-log--filter-buffer-substring (beg end &optional delete)
  "Filter copied log text between BEG and END.

When a copied region contains both heading and tail text, drop the tail text
from the copied result by default. Copying tail text alone preserves it."
  (let ((string (buffer-substring--filter beg end delete))
        (trim-tail nil))
    (when (and (stringp string)
               (majutsu-log--string-has-module-p string 'heading)
               (majutsu-log--string-has-module-p string 'tail))
      (setq string (majutsu-log--string-remove-module string 'tail))
      (setq trim-tail t))
    (setq string (majutsu-log--cleanup-copied-string string))
    (when (and trim-tail (stringp string))
      (setq string (replace-regexp-in-string "[ \t]+$" "" string)))
    string))

(defun majutsu-log--insert-entry (entry compiled)
  "Insert parsed ENTRY as a `jj-commit' section using COMPILED."
  (setq-local majutsu-log--buffer-compiled compiled)
  (let* ((id (majutsu-log--entry-id entry))
         (indent (or (plist-get entry :indent) 0))
         (prefixes (or (plist-get entry :heading-prefixes) (list "")))
         (last-prefix (or (car (last prefixes)) ""))
         (content-lines (majutsu-log--render-heading-content-lines entry compiled t))
         (count (max (length content-lines) (length prefixes)))
         (heading-lines nil)
         (tail (majutsu-log--render-tail entry compiled t))
         (suffix-lines (plist-get entry :suffix-lines))
         (body (majutsu-log--render-body entry compiled t))
         (has-body (and (stringp body)
                        (not (string-empty-p (string-trim body))))))
    (cl-loop for idx below count
             do (let ((prefix (majutsu-log--propertize-decoration
                               (or (nth idx prefixes) last-prefix)
                               id 'heading
                               (if (= idx 0) 'graph-prefix 'graph-carry)))
                      (line (or (nth idx content-lines) "")))
                  (push (cons prefix line) heading-lines)))
    (setq heading-lines (nreverse heading-lines))
    (magit-insert-section (jj-commit id t)
      (when heading-lines
        (majutsu-log--insert-heading-anchor-line (cdar heading-lines)
                                                 tail
                                                 id
                                                 (caar heading-lines)))
      (dolist (line (cdr heading-lines))
        (majutsu-log--insert-prefixed-line (cdr line) (car line)))
      (dolist (suffix-line suffix-lines)
        (pcase-let* ((`(,prefix . ,content)
                      (majutsu-log--split-prefix-line suffix-line indent))
                     (decorated-prefix
                      (majutsu-log--propertize-decoration prefix id 'heading 'graph-carry))
                     (decorated-content
                      (majutsu-log--propertize-content content id 'heading)))
          (majutsu-log--insert-prefixed-line decorated-content decorated-prefix)))
      (when has-body
        (magit-insert-heading)
        (let ((body-prefix (majutsu-log--propertize-decoration
                            (make-string indent ?\s)
                            id 'body 'body-prefix)))
          (magit-insert-section-body
            (let ((start (point)))
              (insert body)
              (insert "\n")
              (majutsu-log--apply-line-prefix-span start (point) body-prefix))))))))

(defun majutsu-log--wash-entry (compiled)
  "Wash the entry at point using COMPILED.

Return the parsed entry after replacing its raw protocol region with a
`jj-commit' section, or nil when point is not at an entry."
  (when-let* ((entry (save-excursion (majutsu-log--parse-entry-at-point compiled))))
    (let ((beg (plist-get entry :beg))
          (end (plist-get entry :end)))
      (delete-region beg end)
      (goto-char beg)
      (majutsu-log--insert-entry entry compiled)
      entry)))

(defun majutsu-log--wash-logs (_args)
  "Wash jj log output in the current (narrowed) buffer region.

This function is meant to be used as a WASHER for `majutsu-jj-wash'."
  (let* ((compiled (majutsu-log--ensure-template))
         (entries nil))
    (setq majutsu-log--cached-entries nil)
    (setq majutsu-log--entry-by-id nil)
    (setq majutsu-log--children-by-id nil)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((entry (majutsu-log--wash-entry compiled)))
          (push entry entries)
        (magit-delete-line)))
    (setq majutsu-log--cached-entries (nreverse entries))
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

;;;###autoload(autoload 'majutsu-log-copy-transient "majutsu-log" nil t)
(transient-define-prefix majutsu-log-copy-transient ()
  "Transient for semantic copy commands in `majutsu-log-mode'."
  [[("s" "Section value" majutsu-copy-section-value)
    ("f" "Visible field at point" majutsu-log-copy-field)
    ("F" "Entry field…" majutsu-log-copy-entry-field)
    ("h" "Commit hash" majutsu-log-copy-commit-id)
    ("m" "Visible module at point" majutsu-log-copy-module)]])

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
        majutsu-buffer-log-revsets
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
      (pcase-let ((`(,args ,revsets ,filesets)
                   (majutsu-log--get-value 'majutsu-log-mode 'direct)))
        (majutsu-setup-buffer #'majutsu-log-mode locked
          (majutsu-buffer-log-args (copy-sequence args))
          (majutsu-buffer-log-revsets revsets)
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

(defun majutsu-log-transient-set-revisions ()
  "Prompt for a revset and store it in the current log variables."
  (interactive)
  (let* ((current (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct)))
         (input (string-trim (read-from-minibuffer "Revset (empty to clear): " current))))
    (pcase-let ((`(,args ,_revsets ,filesets)
                 (majutsu-log--get-value 'majutsu-log-mode 'direct)))
      (majutsu-log--set-value 'majutsu-log-mode args
                              (unless (string-empty-p input) input)
                              filesets))
    (majutsu-log-transient--redisplay)))

(defun majutsu-log-transient-clear-revisions ()
  "Clear the stored revset."
  (interactive)
  (pcase-let ((`(,args ,_revsets ,filesets)
               (majutsu-log--get-value 'majutsu-log-mode 'direct)))
    (majutsu-log--set-value 'majutsu-log-mode args nil filesets))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-reset ()
  "Reset log options to defaults."
  (interactive)
  (majutsu-log--set-value 'majutsu-log-mode nil nil nil)
  (if (fboundp 'transient-reset)
      (transient-reset)
    (majutsu-log-transient--redisplay)))

(defun majutsu-log--toggle-desc (label flag)
  "Return LABEL annotated with ON/OFF state for FLAG in log args."
  (if (member flag (car (majutsu-log--get-value 'majutsu-log-mode 'direct)))
      (format "%s [on]" label)
    (format "%s [off]" label)))

(defun majutsu-log--value-desc (label value)
  "Return LABEL annotated with VALUE, when VALUE is non-nil."
  (if value
      (format "%s (%s)" label value)
    label))

(defun majutsu-log-transient--redisplay ()
  "Redisplay the log transient, compatible with older transient versions."
  (if (fboundp 'transient-redisplay)
      (transient-redisplay)
    (when (fboundp 'transient--redisplay)
      (transient--redisplay))))

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
  (pcase-let ((`(,args ,_revsets ,filesets)
               (majutsu-log--get-value (oref obj major-mode) 'prefix)))
    (oset obj value (if filesets `(("--" ,@filesets) ,@args) args))))

(cl-defmethod transient-set-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command)))
                (`(,_old-args ,revsets ,_filesets)
                 (majutsu-log--get-value mode 'direct)))
      (majutsu-log--set-value mode args revsets files)
      (transient--history-push obj)
      (majutsu-refresh))))

(cl-defmethod transient-save-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command)))
                (`(,_old-args ,revsets ,_filesets)
                 (majutsu-log--get-value mode 'direct)))
      (majutsu-log--set-value mode args revsets files t)
      (transient--history-push obj)
      (majutsu-refresh))))

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
  [:description majutsu-log--transient-description
   :class transient-columns
   ["Revisions"
    ("r" "Set revset" majutsu-log-transient-set-revisions
     :description (lambda ()
                    (majutsu-log--value-desc
                     "Set revset"
                     (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct))))
     :transient t)
    (majutsu-log:--limit)
    (majutsu-log:--reversed)
    (majutsu-log:--no-graph)
    ("R" "Clear revset" majutsu-log-transient-clear-revisions
     :if (lambda () (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct)))
     :transient t)
    ]
   ["Paths"
    (majutsu-log:--)]
   ["Actions"
    ("g" "buffer" majutsu-log-transient)
    ("s" "buffer and set defaults" transient-set-and-exit)
    ("w" "buffer and save defaults" transient-save-and-exit)
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
