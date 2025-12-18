;;; majutsu-log.el --- Log view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library builds the Majutsu log buffer: compiles jj templates,
;; parses log output, renders sections, and handles navigation.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-mode)
(require 'majutsu-process)
(require 'majutsu-template)
(require 'json)

;;; Section Keymaps

(defvar-keymap majutsu-commit-section-map
  :doc "Keymap for `jj-commit' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset-at-point)

;;; Transient Selection (value-keyed)

(cl-defstruct (majutsu-selection-category
               (:constructor majutsu-selection-category-create))
  key
  label
  face
  type
  values)

(cl-defstruct (majutsu-selection-session
               (:constructor majutsu-selection-session-create))
  categories
  overlays)

(defvar-local majutsu-selection--session nil
  "Active transient selection session for the current buffer.

Selections are keyed by the `value' of `majutsu-commit-section' instances
(i.e., the revset/id string shown in the log).  The session and overlays
are meant to exist only while a transient is active.")

(defun majutsu-selection-session-active-p ()
  "Return non-nil if a transient selection session is active."
  (and majutsu-selection--session t))

(defun majutsu-selection--normalize-value (value)
  (majutsu--normalize-id-value value))

(defun majutsu-selection--delete-all-overlays ()
  (when-let* ((session majutsu-selection--session))
    (let ((overlays (majutsu-selection-session-overlays session)))
      (maphash (lambda (_id ov)
                 (when (overlayp ov)
                   (delete-overlay ov)))
               overlays)
      (clrhash overlays))))

(defun majutsu-selection-session-end ()
  "End the active transient selection session and remove its overlays."
  (interactive)
  (when majutsu-selection--session
    (majutsu-selection--delete-all-overlays)
    (setq majutsu-selection--session nil)))

(defun majutsu-selection-session-begin (categories)
  "Begin a transient selection session in the current buffer.

CATEGORIES is a list of plists, each containing:
- :key   symbol identifying the selection bucket
- :label string displayed before selected commits
- :face  face (symbol or plist) applied to the label
- :type  either `single' or `multi' (defaults to `multi')"
  (majutsu-selection-session-end)
  (setq-local
   majutsu-selection--session
   (majutsu-selection-session-create
    :categories
    (mapcar
     (lambda (spec)
       (let* ((key (plist-get spec :key))
              (label (or (plist-get spec :label) ""))
              (face (plist-get spec :face))
              (type (or (plist-get spec :type) 'multi)))
         (unless (symbolp key)
           (user-error "Selection category :key must be a symbol: %S" spec))
         (unless (memq type '(single multi))
           (user-error "Selection category :type must be `single' or `multi': %S" spec))
         (majutsu-selection-category-create
          :key key :label label :face face :type type :values nil)))
     categories)
    :overlays (make-hash-table :test 'equal)))
  (majutsu-selection-render))

(defun majutsu-selection--category (key)
  (and majutsu-selection--session
       (seq-find (lambda (cat)
                   (eq (majutsu-selection-category-key cat) key))
                 (majutsu-selection-session-categories majutsu-selection--session))))

(defun majutsu-selection--require-category (key)
  (or (majutsu-selection--category key)
      (user-error "No such selection category: %S" key)))

(defun majutsu-selection-values (key)
  "Return selected commit values for category KEY."
  (let ((cat (majutsu-selection--category key)))
    (copy-sequence (and cat (majutsu-selection-category-values cat)))))

(defun majutsu-selection-count (key)
  "Return number of selected commits for category KEY."
  (length (majutsu-selection-values key)))

(defun majutsu-selection--selected-any-p (id)
  (and majutsu-selection--session
       (seq-some (lambda (cat)
                   (member id (majutsu-selection-category-values cat)))
                 (majutsu-selection-session-categories majutsu-selection--session))))

(defun majutsu-selection--labels-for (id)
  (when majutsu-selection--session
    (let (parts)
      (dolist (cat (majutsu-selection-session-categories majutsu-selection--session))
        (when (member id (majutsu-selection-category-values cat))
          (push (propertize (majutsu-selection-category-label cat)
                            'face (majutsu-selection-category-face cat))
                parts)))
      (when parts
        (concat (mapconcat #'identity (nreverse parts) " ") " ")))))

(defun majutsu-selection--targets-at-point ()
  (let ((values (or (magit-region-values 'jj-commit t)
                    (and-let* ((value (magit-section-value-if 'jj-commit)))
                      (list value)))))
    (seq-filter
     (lambda (v) (and v (not (string-empty-p v))))
     (mapcar #'majutsu-selection--normalize-value values))))

(defun majutsu-selection--overlay-range (section)
  (let ((start (oref section start))
        (end (or (oref section content) (oref section end))))
    (and start end (cons start end))))

(defun majutsu-selection--overlay-valid-p (ov)
  (and (overlayp ov)
       (overlay-buffer ov)))

(defun majutsu-selection--delete-overlay (id)
  (when-let* ((session majutsu-selection--session)
              (overlays (majutsu-selection-session-overlays session))
              (ov (gethash id overlays)))
    (when (overlayp ov)
      (delete-overlay ov))
    (remhash id overlays)))

(defun majutsu-selection--render-id (id)
  (when-let* ((session majutsu-selection--session))
    (let* ((labels (majutsu-selection--labels-for id))
           (overlays (majutsu-selection-session-overlays session))
           (existing (gethash id overlays)))
      (cond
       ((not labels)
        (majutsu-selection--delete-overlay id))
       (t
        (when (and existing (not (majutsu-selection--overlay-valid-p existing)))
          (remhash id overlays)
          (setq existing nil))
        (when-let* ((section (and (derived-mode-p 'majutsu-log-mode)
                                  (majutsu-find-revision-section id)))
                    (range (majutsu-selection--overlay-range section)))
          (pcase-let ((`(,start . ,end) range))
            (unless (and existing
                         (= (overlay-start existing) start)
                         (= (overlay-end existing) end))
              (when existing
                (delete-overlay existing))
              (setq existing (make-overlay start end nil t))
              (overlay-put existing 'evaporate t)
              (overlay-put existing 'priority '(nil . 50))
              (overlay-put existing 'majutsu-selection t)
              (puthash id existing overlays))
            (overlay-put existing 'before-string labels))))))))

(defun majutsu-selection-render ()
  "Re-render selection overlays for the current buffer."
  (when majutsu-selection--session
    (let* ((session majutsu-selection--session)
           (overlays (majutsu-selection-session-overlays session))
           (selected (make-hash-table :test 'equal)))
      (dolist (cat (majutsu-selection-session-categories session))
        (dolist (id (majutsu-selection-category-values cat))
          (puthash id t selected)))
      (maphash (lambda (id _ov)
                 (unless (gethash id selected)
                   (majutsu-selection--delete-overlay id)))
               overlays)
      (maphash (lambda (id _)
                 (majutsu-selection--render-id id))
               selected))))

(defun majutsu-selection-clear (&optional key)
  "Clear selections.

If KEY is non-nil, clear only that selection category.  Otherwise clear
all categories in the active session."
  (interactive)
  (when-let* ((session majutsu-selection--session))
    (if key
        (let ((cat (majutsu-selection--require-category key)))
          (setf (majutsu-selection-category-values cat) nil))
      (dolist (cat (majutsu-selection-session-categories session))
        (setf (majutsu-selection-category-values cat) nil)))
    (majutsu-selection-render)))

(defun majutsu-selection-toggle (key &optional values)
  "Toggle selected commits in category KEY.

VALUES defaults to the commit(s) at point or in the active region."
  (interactive)
  (let* ((cat (majutsu-selection--require-category key))
         (values (or values (majutsu-selection--targets-at-point))))
    (unless values
      (user-error "No changeset at point"))
    (pcase (majutsu-selection-category-type cat)
      ('single
       (let* ((new (car values))
              (old (car (majutsu-selection-category-values cat))))
         (setf (majutsu-selection-category-values cat)
               (if (and old (equal old new)) nil (list new)))
         (when old (majutsu-selection--render-id old))
         (when new (majutsu-selection--render-id new))))
      (_
       (let ((current (majutsu-selection-category-values cat))
             (changed nil))
         (dolist (id values)
           (if (member id current)
               (setq current (delete id current))
             (setq current (append current (list id))))
           (push id changed))
         (setf (majutsu-selection-category-values cat) current)
         (dolist (id changed)
           (majutsu-selection--render-id id)))))))

(defun majutsu-selection-select (key &optional values)
  "Toggle a single commit selection in category KEY.

KEY must be a `single' selection category.  VALUES defaults to the
commit(s) at point or in the active region."
  (interactive)
  (let* ((cat (majutsu-selection--require-category key))
         (values (or values (majutsu-selection--targets-at-point))))
    (unless values
      (user-error "No changeset at point"))
    (unless (eq (majutsu-selection-category-type cat) 'single)
      (user-error "Selection category %S is not single-select" key))
    (when (cdr values)
      (user-error "Selection category %S only accepts one changeset" key))
    (majutsu-selection-toggle key values)))

;;; Utilities

(defun majutsu-read-revset (prompt &optional default)
  "Prompt user with PROMPT to read a revision set string."
  (let ((default (or default (magit-section-value-if 'jj-commit) "@")))
    (read-string
     (if default
         (format "%s (default %s): " prompt default)
       (format "%s: " prompt))
     nil nil default)))

;;; Log State

(defconst majutsu-log--state-template
  '(:revisions nil
    :limit nil
    :reversed nil
    :no-graph nil
    :filesets nil)
  "Default plist template describing log view options.")

(defvar majutsu-log-state (copy-sequence majutsu-log--state-template)
  "Global default/last-used plist for jj log view options.")

(defvar-local majutsu-log-buffer-state nil
  "Buffer-local log view options for the current majutsu log buffer.")

(defvar-local majutsu-log--this-error nil
  "Last jj side-effect error summary for this log buffer.

This is set by process runners (see `majutsu-process-buffer') and
rendered by `majutsu-log-insert-error-header' on the next refresh.")

(defcustom majutsu-log-sections-hook '(majutsu-log-insert-error-header
                                       majutsu-log-insert-logs
                                       majutsu-log-insert-status)
  "Hook run to insert sections in the log buffer."
  :type 'hook
  :group 'majutsu)

(defun majutsu-log--state-default ()
  "Return a fresh copy of the default/last-used log state plist."
  (copy-sequence
   (or (get 'majutsu-log-mode 'majutsu-log-current-state)
       majutsu-log-state
       majutsu-log--state-template)))

(defun majutsu-log--state-current ()
  "Return the active log state plist for the current context.
Prefer the buffer-local state in a log buffer; otherwise fall
back to the global/default state."
  (cond
   ((and (boundp 'majutsu-log-buffer-state)
         majutsu-log-buffer-state)
    majutsu-log-buffer-state)
   ((derived-mode-p 'majutsu-log-mode)
    (setq-local majutsu-log-buffer-state (majutsu-log--state-default)))
   (t (majutsu-log--state-default))))

(defun majutsu-log--state-assign (state)
  "Persist STATE to all relevant caches (buffer, global, mode property)."
  (let ((copy (copy-sequence state)))
    ;; buffer-local (when applicable)
    (when (derived-mode-p 'majutsu-log-mode)
      (setq-local majutsu-log-buffer-state (copy-sequence copy)))
    ;; global fallback / last-used
    (setq majutsu-log-state (copy-sequence copy))
    ;; mode-level last-used (mirrors Magit pattern)
    (put 'majutsu-log-mode 'majutsu-log-current-state (copy-sequence copy))
    copy))

(defun majutsu-log--reset-state ()
  "Reset log view options to defaults (template)."
  (majutsu-log--state-assign majutsu-log--state-template))

(defun majutsu-log--state-get (key)
  "Return value for KEY in the active log state."
  (plist-get (majutsu-log--state-current) key))

(defun majutsu-log--state-set (key value)
  "Set KEY in the active log state to VALUE."
  (majutsu-log--state-assign
   (plist-put (copy-sequence (majutsu-log--state-current)) key value)))

(defun majutsu-log--summary-parts ()
  "Return a list of human-readable fragments describing current log state."
  (let ((parts '()))
    (when-let* ((rev (majutsu-log--state-get :revisions)))
      (push (format "rev=%s" rev) parts))
    (when-let* ((limit (majutsu-log--state-get :limit)))
      (push (format "limit=%s" limit) parts))
    (when (majutsu-log--state-get :reversed)
      (push "reversed" parts))
    (when (majutsu-log--state-get :no-graph)
      (push "no-graph" parts))
    (let ((paths (majutsu-log--state-get :filesets)))
      (when paths
        (push (if (= (length paths) 1)
                  (format "path=%s" (car paths))
                (format "paths=%d" (length paths)))
              parts)))
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
  "Separator character inserted between template fields.
We use an ASCII record separator so parsing stays robust while
remaining invisible in the rendered buffer.")

(defconst majutsu-log--required-columns '(change-id commit-id long-desc)
  "Columns that must always be present in the compiled template for parsing.")

(defcustom majutsu-log-commit-columns
  '((:field id :align left :visible nil)
    (:field change-id :align left)
    (:field bookmarks :align left)
    (:field tags :align left)
    (:field working-copies :align left)
    (:field empty :align left)
    (:field git-head :align left)
    (:field description :align left)
    (:field author :align right)
    (:field timestamp :align right)
    (:field commit-id :align right :visible nil)
    (:field flags :align left :visible nil)
    (:field long-desc :visible nil))
  "Column specification controlling how log rows are rendered.

Each element is a plist with at least `:field'. Supported keys:
- :field   - symbol identifying a known field (e.g. `change-id',
             `commit-id', `description', `author',
             `timestamp', `flags', `long-desc').
- :align   - one of `left', `right', or `center' (defaults to `left').
- :visible - non-nil to show in the buffer; nil keeps the field hidden
             but still present in the template for parsing.

Width is computed dynamically per buffer based on content; the optional
:width key is currently ignored. Required fields are injected
automatically even if omitted or hidden."
  :type '(repeat (plist :options (:field :align :width :visible)))
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

(majutsu-log-define-column id
  [:if [:or [:hidden] [:divergent]]
      [:commit_id :shortest 8]
    [:change_id :shortest 8]]
  "Template for the commit-id column.")

(majutsu-log-define-column change-id
  [:if [:hidden]
      [:label "hidden"
              [[:change_id :shortest 8]
               " hidden"]]
    [:label
     [:if [:divergent] "divergent"]
     [[:change_id :shortest 8]
      [:if [:divergent] "??"]]]]
  "Template for the change-id column.")

(majutsu-log-define-column commit-id
  [:commit_id :shortest 8]
  "Template for the commit-id column.")

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
  [:if [:git_head] [:label "git_head" ""]]
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
      [:label "empty" "∅"]]
  "Template for the empty column.")

(majutsu-log-define-column description
  [:if [:description]
      [:method [:description] :first_line]
    [:label
     [:if [:empty] "empty"]
     [:label
      "description placeholder"
      "□"]]]
  "Template for the description column.")

(majutsu-log-define-column author
  [:author :name]
  "Template for the author column.")

(majutsu-log-define-column timestamp
  [:committer :timestamp :ago]
  "Template for the timestamp column.")

(majutsu-log-define-column long-desc
  [:if [:description] [:json [:description]] [:json " "]]
  "Template for the long-desc column.
Note: This must return a valid JSON string (usually via :json) to be parsed correctly.")

(defvar majutsu-log--compiled-template-cache nil
  "Cached structure holding the compiled log template and column metadata.")

(defun majutsu-log--invalidate-template-cache (&rest _)
  "Reset cached compiled template when layout changes."
  (setq majutsu-log--compiled-template-cache nil)
  (setq majutsu-log--cached-entries nil))

(defun majutsu-log--normalize-column-spec (spec)
  "Normalize a single column SPEC into a plist with defaults."
  (let* ((col (cond
               ((and (plistp spec) (plist-get spec :field)) spec)
               ((symbolp spec) (list :field spec))
               (t (user-error "Invalid column spec: %S" spec))))
         (field (plist-get col :field))
         (align (or (plist-get col :align) 'left))
         (visible (if (plist-member col :visible)
                      (plist-get col :visible)
                    t)))
    (setq align (if (keywordp align) (intern (substring (symbol-name align) 1)) align))
    (unless (memq align '(left right center))
      (user-error "Column %S has invalid :align %S" field align))
    (list :field field :align align :visible visible)))

(defun majutsu-log--ensure-required-columns (columns)
  "Ensure required columns are present in COLUMNS list.
Missing required fields are appended as hidden columns."
  (let ((present (mapcar (lambda (c) (plist-get c :field)) columns)))
    (dolist (req majutsu-log--required-columns)
      (unless (memq req present)
        (setq columns (append columns (list (list :field req :visible nil :align 'left))))))
    columns))

(defun majutsu-log--column-template (field)
  "Return majutsu-template form for FIELD.
Looks up `majutsu-log-template-FIELD'."
  (let ((var (intern-soft (format "majutsu-log-template-%s" field))))
    (if (and var (boundp var))
        (symbol-value var)
      (user-error "Unknown column field %S" field))))

(defun majutsu-log--compile-columns (&optional columns)
  "Compile COLUMNS (or `majutsu-log-commit-columns') into a jj template string.
Returns a plist with :template, :columns, and :field-order."
  (let* ((normalized (mapcar #'majutsu-log--normalize-column-spec
                             (or columns majutsu-log-commit-columns)))
         (complete (majutsu-log--ensure-required-columns normalized))
         (field-order (mapcar (lambda (c) (plist-get c :field)) complete))
         (templates (mapcar (lambda (c)
                              (majutsu-log--column-template (plist-get c :field)))
                            complete))
         (segments (list majutsu-log--field-separator))
         (compiled nil))
    (dolist (tmpl templates)
      (setq segments (append segments (list tmpl majutsu-log--field-separator))))
    (setq compiled (tpl-compile (vconcat segments (list "\n"))))
    (list :template compiled
          :columns complete
          :field-order field-order)))

(defun majutsu-log--ensure-template ()
  "Return cached compiled template structure, recomputing if necessary."
  (or majutsu-log--compiled-template-cache
      (setq majutsu-log--compiled-template-cache
            (majutsu-log--compile-columns majutsu-log-commit-columns))))

(defun majutsu-log--build-args ()
  "Build argument list for `jj log' using current state."
  (let ((args '("log")))
    (when-let* ((rev (majutsu-log--state-get :revisions)))
      (setq args (append args (list "-r" rev))))
    (when-let* ((limit (majutsu-log--state-get :limit)))
      (setq args (append args (list "-n" limit))))
    (when (majutsu-log--state-get :reversed)
      (setq args (append args '("--reversed"))))
    (when (majutsu-log--state-get :no-graph)
      (setq args (append args '("--no-graph"))))
    (setq args (append args (list "-T" (plist-get (majutsu-log--ensure-template) :template))))
    (setq args (append args (majutsu-log--state-get :filesets)))
    args))

;;; Log Parsing

(defvar-local majutsu-log--cached-entries nil
  "Cached log entries for the current buffer.")

(defun majutsu-log--parse-json-safe (value)
  "Parse VALUE as JSON, returning nil on failure or blank strings."
  (when (and value (not (string-empty-p value)))
    (condition-case nil
        (json-parse-string value)
      (error nil))))

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

(defun majutsu-log--record-column (entry field value)
  "Record FIELD VALUE onto ENTRY plist and column map."
  (pcase field
    ('id
     (setq entry (plist-put entry :id value)))
    ('change-id
     (setq entry (plist-put entry :change-id value)))
    ('commit-id
     (setq entry (plist-put entry :commit-id value)))
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
     (setq value (string-remove-suffix " ago" value))
     (setq entry (plist-put entry :timestamp value)))
    ('long-desc
     (setq entry (plist-put entry :long-desc (majutsu-log--parse-json-safe value))))
    ('flags
     (setq entry (majutsu-log--apply-flags entry value)))
    ('git-head
     (when (and value (not (string-empty-p value)))
       (setq entry (plist-put entry :git-head t))))
    ('signature
     (setq entry (plist-put entry :signature value)))
    ('empty
     (setq entry (plist-put entry :empty (not (string-empty-p value))))))
  (let* ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (setq entry (plist-put entry :columns columns)))
  entry)

(defun majutsu-log--build-entry-from-elems (elems field-order line)
  "Construct a log ENTRY plist from ELEMS split by separator.
FIELD-ORDER describes which field name corresponds to each element
after the leading graph prefix."
  (let* ((prefix (car elems))
         (fields (cdr elems))
         (entry (list :prefix prefix
                      :line line
                      :elems elems
                      :columns nil)))
    (cl-loop for field in field-order
             for value in fields
             do (setq entry (majutsu-log--record-column entry field value)))
    entry))

(defun majutsu-parse-log-entries (&optional buf log-output)
  "Parse jj log output from BUF (defaults to `current-buffer').
If LOG-OUTPUT is provided, parse it.
If `majutsu-log--cached-entries' is set, return it.
Otherwise, run the command in BUF (or current buffer) to get output.

The parser keeps graph spacer lines (blank or connector rows) with the
preceding revision so `magit-section-forward' can jump between commits
instead of stopping on visual padding."
  (if (and majutsu-log--cached-entries (not log-output))
      majutsu-log--cached-entries
    (with-current-buffer (or buf (current-buffer))
      (let* ((args (majutsu-log--build-args))
             (compiled (majutsu-log--ensure-template))
             (field-order (plist-get compiled :field-order))
             (output (or log-output (apply #'majutsu-run-jj args))))
        (when (and output (not (string-empty-p output)))
          (let ((lines (split-string output "\n"))
                (entries '())
                (current nil)
                (pending nil))
            (dolist (line lines)
              (let* ((raw-elems (mapcar #'string-trim-right (split-string line majutsu-log--field-separator nil))))
                (if (> (length raw-elems) 1)
                    (progn
                      (when current
                        (when pending
                          (setq current (plist-put current :suffix-lines (nreverse pending)))
                          (setq pending nil))
                        (push current entries))
                      (setq current (majutsu-log--build-entry-from-elems raw-elems field-order line))
                      (setq pending nil))
                  (push line pending))))
            (when current
              (when pending
                (setq current (plist-put current :suffix-lines (nreverse pending))))
              (push current entries))
            (nreverse entries)))))))

(defun majutsu--indent-string (s column)
  "Insert STRING into the current buffer, indenting each line to COLUMN."
  (let ((indentation (make-string column ?\s))) ; Create a string of spaces for indentation
    (mapconcat (lambda (line)
                 (concat indentation line))
               (split-string s "\n")
               "\n"))) ; Join lines with newline, prefixed by indentation

(defun majutsu-log--entry-column (entry field)
  "Return string value for FIELD stored on ENTRY."
  (alist-get field (plist-get entry :columns) nil nil #'eq))

(defun majutsu-log--field-face (field)
  "Return face symbol for FIELD, or nil."
  (pcase field
    ((or 'id 'change-id 'commit-id) 'magit-hash)
    ('bookmarks 'magit-branch-local)
    ('tags 'magit-tag)
    ('working-copies 'magit-branch-remote)
    ('author 'magit-log-author)
    ('timestamp 'magit-log-date)
    ('flags 'font-lock-comment-face)
    (_ nil)))

(defun majutsu-log--compute-column-widths (entries compiled)
  "Compute display widths for visible columns.
Returns plist with:
- :right (alist field->width)  ; per right-aligned column max width
- :right-total                 ; total width of right block incl. spaces"
  (let* ((visible-cols (seq-filter (lambda (c) (plist-get c :visible))
                                   (plist-get compiled :columns)))
         (right-cols (seq-filter (lambda (c) (eq (plist-get c :align) 'right))
                                 visible-cols))
         (right-widths ()))
    (dolist (entry entries)
      (dolist (col right-cols)
        (let* ((field (plist-get col :field))
               (val (or (majutsu-log--entry-column entry field) ""))
               (w (string-width val)))
          (setf (alist-get field right-widths nil nil #'eq)
                (max w (or (alist-get field right-widths nil nil #'eq) 0))))))
    (let* ((right-total
            (let ((sum 0) (first t))
              (dolist (col right-cols)
                (let* ((field (plist-get col :field))
                       (w (or (alist-get field right-widths nil nil #'eq) 0)))
                  (unless first (setq sum (1+ sum)))
                  (setq first nil)
                  (setq sum (+ sum w))))
              sum)))
      (list :right right-widths :right-total right-total))))

(defun majutsu-log--pad-display (text width align)
  "Pad TEXT to WIDTH using ALIGN (`left' | `right' | `center')."
  (let* ((txt (or text ""))
         (len (string-width txt))
         (pad (max 0 (- width len))))
    (pcase align
      ('right (concat (make-string pad ?\s) txt))
      ('center (let* ((left (/ pad 2))
                      (right (- pad left)))
                 (concat (make-string left ?\s) txt (make-string right ?\s))))
      (_ (concat txt (make-string pad ?\s))))))

(defun majutsu-log--format-entry-line (entry compiled widths)
  "Return plist (:line string :margin string :desc-indent col).
Left fields follow graph width per-line; right fields are rendered for margin."
  (let* ((visible-cols (seq-filter (lambda (c) (plist-get c :visible))
                                   (plist-get compiled :columns)))
         (left-cols (seq-filter (lambda (c) (not (eq (plist-get c :align) 'right)))
                                visible-cols))
         (right-cols (seq-filter (lambda (c) (eq (plist-get c :align) 'right))
                                 visible-cols))
         (prefix (or (plist-get entry :prefix) ""))
         (parts (list (propertize prefix 'face 'magit-graph)))
         (current-width (string-width prefix))
         (desc-indent nil))
    ;; build left part without padding
    (dolist (col left-cols)
      (let* ((field (plist-get col :field))
             (raw (or (majutsu-log--entry-column entry field) ""))
             (face (majutsu-log--field-face field))
             (formatted (if face (propertize raw 'face face) raw)))
        (unless (string-empty-p formatted)
          (setq parts (append parts (list formatted)))
          (setq current-width (+ current-width 1 (string-width formatted)))
          (when (and (not desc-indent) (eq field 'description))
            (setq desc-indent (- current-width (string-width formatted)))))))
    (let* ((left-str (string-join parts " "))
           (left-len (string-width left-str))
           (right-parts '()))
      ;; build right block with per-column padding
      (dolist (col right-cols)
        (let* ((field (plist-get col :field))
               (raw (or (majutsu-log--entry-column entry field) ""))
               (face (majutsu-log--field-face field))
               (col-width (or (alist-get field (plist-get widths :right) nil nil #'eq)
                              (string-width raw)))
               (formatted (majutsu-log--pad-display raw col-width (plist-get col :align)))
               (formatted (if face (propertize formatted 'face face) formatted)))
          (push formatted right-parts)))
      (setq right-parts (nreverse right-parts))
      (let ((margin (string-join right-parts " ")))
        (list :line left-str
              :margin margin
              :desc-indent (or desc-indent left-len))))))

(defun majutsu-log--set-right-margin (width)
  "Set right margin WIDTH (in columns) for all windows showing current buffer."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (with-selected-window win
      (let ((left (car (window-margins win))))
        (if (and width (> width 0))
            (set-window-margins win left width)
          (set-window-margins win left nil))))))

(defun majutsu-log--make-margin-overlay (string)
  "Display STRING in the right margin of the current (or previous) line."
  (save-excursion
    (forward-line (if (bolp) -1 0))
    (let ((o (make-overlay (1+ (point)) (line-end-position) nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize " " 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

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

(defun majutsu-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (magit-insert-section (lograph)
    (magit-insert-heading (majutsu-log--heading-string))
    (let* ((compiled (majutsu-log--ensure-template))
           (entries (majutsu-parse-log-entries))
           (widths (majutsu-log--compute-column-widths entries compiled)))
      (majutsu-log--set-right-margin (plist-get widths :right-total))
      (dolist (entry entries)
        (let ((id (majutsu-selection--normalize-value (plist-get entry :id))))
          (magit-insert-section
              (jj-commit id t)
            (let* ((line-info (majutsu-log--format-entry-line entry compiled widths))
                   (heading (plist-get line-info :line))
                   (margin (plist-get line-info :margin))
                   (indent (plist-get line-info :desc-indent)))
              (magit-insert-heading
                (insert heading))
              (when margin
                (majutsu-log--make-margin-overlay margin))
              (when-let* ((long-desc (plist-get entry :long-desc))
                          (indented (majutsu--indent-string long-desc (or indent 0))))
                (magit-insert-section-body
                  (insert indented)
                  (insert "\n"))))
            (when-let* ((suffix-lines (plist-get entry :suffix-lines)))
              (dolist (suffix-line suffix-lines)
                (insert suffix-line)
                (insert "\n")))))))
    (insert "\n")))

;;; Log insert status

(defun majutsu--analyze-status-for-hints (status-output)
  "Analyze jj status output and provide helpful hints."
  (when (and status-output (not (string-empty-p status-output)))
    (cond
     ;; No changes
     ((string-match-p "The working copy is clean" status-output)
      (message "Working copy is clean - no changes to commit"))

     ;; Conflicts present
     ((string-match-p "There are unresolved conflicts" status-output)
      (message "💡 Resolve conflicts with 'jj resolve' or use diffedit (E/M)"))

     ;; Untracked files
     ((string-match-p "Untracked paths:" status-output)
      (message "💡 Add files with 'jj file track' or create .gitignore"))

     ;; Working copy changes
     ((string-match-p "Working copy changes:" status-output)
      (message "💡 Commit changes with 'jj commit' or describe with 'jj describe'")))))

(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (let ((status-output (majutsu-run-jj "status")))
    (when (and status-output (not (string-empty-p status-output)))
      (magit-insert-section (status)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)
        (insert "\n")
        ;; Analyze status and provide hints in the minibuffer
        (majutsu--analyze-status-for-hints status-output)))))

;;; Log insert diff

(defun majutsu-log-insert-diff ()
  "Insert jj diff with hunks into current buffer asynchronously."
  (let* ((section (magit-insert-section (diffbuf)
                    (magit-insert-heading "Working Copy Changes")
                    (insert "Loading diffs...\n")))
         (buf (current-buffer)))
    (majutsu-run-jj-async
     '("diff" "--git")
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (if (and output (not (string-empty-p output)))
                   (progn
                     (majutsu--insert-diff-hunks output)
                     (insert "\n"))
                 (insert "No changes.\n"))
               ;; Update section end marker
               (set-marker (oref section end) (point)))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (insert (format "Error loading diffs: %s\n" err))
               (set-marker (oref section end) (point))))))))))

;;; Log insert conflicts

(defun majutsu-log-insert-conflicts ()
  "Insert conflicted files section."
  (let ((output (majutsu-run-jj "resolve" "--list")))
    (when (and output (not (string-empty-p output)))
      (magit-insert-section (conflict)
        (magit-insert-heading "Unresolved Conflicts")
        (dolist (line (split-string output "\n" t))
          (let ((file (string-trim line)))
            (magit-insert-section (jj-file file)
              (magit-insert-heading (propertize file 'face 'error))
              (insert "\n"))))
        (insert "\n")))))

;;; Log Navigation

(defconst majutsu--show-id-template
  (tpl-compile [:if [:or [:hidden] [:divergent]]
                   [:commit_id :shortest 8]
                 [:change_id :shortest 8]]))

(defun majutsu-current-id ()
  (when-let* ((output (majutsu-run-jj "log" "--no-graph" "-r" "@" "-T" majutsu--show-id-template)))
    (string-trim output)))

(defun majutsu-log-goto-@ ()
  "Jump to the current changeset (@)."
  (interactive)
  (majutsu--goto-log-entry (majutsu-current-id)))

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
              (section (majutsu-find-revision-section id)))
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

(defun majutsu-find-revision-section (id)
  "Return the jj-commit section matching ID inside the log buffer."
  (let ((id (majutsu-selection--normalize-value (string-trim (or id "")))))
    (and id
         (not (string-empty-p id))
         (magit-get-section `((jj-commit . ,id) (lograph) (logbuf))))))

;;; Log Mode

(defvar-keymap majutsu-log-mode-map
  :doc "Keymap for `majutsu-log-mode'."
  :parent majutsu-mode-map
  "n" 'majutsu-goto-next-changeset
  "p" 'majutsu-goto-prev-changeset
  "B" 'majutsu-new-with-before
  "A" 'majutsu-new-with-after)

(define-derived-mode majutsu-log-mode majutsu-mode "Majutsu Log"
  "Major mode for interacting with jj version control system."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  ;; Initialize per-buffer log state (keeps behavior aligned with Magit's pattern).
  (setq-local majutsu-log-buffer-state (majutsu-log--state-default))
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end nil t))

(defun majutsu-log-render ()
  "Render the log buffer using cached data."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (logbuf)
      (run-hooks 'majutsu-log-sections-hook)
      (majutsu-selection-render))))

(defun majutsu-log--refresh-buffer (target-commit)
  "Implementation helper to refresh the current log buffer.
Assumes `current-buffer' is a `majutsu-log-mode' buffer."
  (majutsu--assert-mode 'majutsu-log-mode)
  (let* ((root (majutsu--root))
         (buf (current-buffer))
         (target-commit (or target-commit (magit-section-value-if 'jj-commit))))
    (setq-local majutsu--repo-root root)
    (setq default-directory root)
    (setq majutsu-log--cached-entries nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading..."))
    (majutsu-run-jj-async
     (majutsu-log--build-args)
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-log-mode)
             (setq majutsu-log--cached-entries (majutsu-parse-log-entries nil output))
             (majutsu-log-render)
             (unless (when target-commit (majutsu--goto-log-entry target-commit))
               (majutsu-log-goto-@))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-log-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Error: " err)))))))))

;;;###autoload
(defun majutsu-log-refresh (&optional commit)
  "Refresh a majutsu log buffer for the current repository.
When called outside a log buffer, try to refresh an existing log
buffer for the same repository.  If none exists and the command
is invoked interactively, signal a user error instead of
mutating the wrong buffer."
  (interactive)
  (let* ((root (majutsu--buffer-root))
         (buffer (majutsu--resolve-mode-buffer 'majutsu-log-mode root)))
    (cond
     (buffer
      (with-current-buffer buffer
        (majutsu-log--refresh-buffer commit)))
     ((called-interactively-p 'interactive)
      (user-error "No Majutsu log buffer for this repository; open one with `majutsu-log`"))
     (t
      (majutsu--debug "Skipping log refresh: no log buffer for %s" (or root "unknown repo"))))))

;;;###autoload
(defun majutsu-log ()
  "Open the majutsu log buffer."
  (interactive)
  (let* ((root (majutsu--root))
         (buffer (get-buffer-create (format "*majutsu: %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (majutsu-log-mode)
      (setq-local majutsu--repo-root root)
      (setq default-directory root)
      (majutsu-log-refresh))
    (majutsu-display-buffer buffer 'log)))

(defun majutsu-log--refresh-view ()
  "Refresh current log buffer or open a new one."
  (if (derived-mode-p 'majutsu-log-mode)
      (majutsu-log-refresh)
    (majutsu-log)))

;;; Commands

(defun majutsu-log-transient-set-revisions ()
  "Prompt for a revset and store it in log state."
  (interactive)
  (let* ((current (majutsu-log--state-get :revisions))
         (input (string-trim (read-from-minibuffer "Revset (empty to clear): " current))))
    (majutsu-log--state-set :revisions (unless (string-empty-p input) input))
    (majutsu-log-transient--redisplay)))

(defun majutsu-log-transient-clear-revisions ()
  "Clear the stored revset."
  (interactive)
  (majutsu-log--state-set :revisions nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-set-limit ()
  "Prompt for a numeric limit and store it in log state."
  (interactive)
  (let* ((current (majutsu-log--state-get :limit))
         (input (string-trim (read-from-minibuffer "Limit (empty to clear): " current))))
    (cond
     ((string-empty-p input)
      (majutsu-log--state-set :limit nil))
     ((string-match-p "\\`[0-9]+\\'" input)
      (majutsu-log--state-set :limit input))
     (t
      (user-error "Limit must be a positive integer"))))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-clear-limit ()
  "Clear the stored limit."
  (interactive)
  (majutsu-log--state-set :limit nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient--toggle (key)
  "Toggle boolean KEY in log state."
  (majutsu-log--state-set key (not (majutsu-log--state-get key)))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-toggle-reversed ()
  "Toggle reversed log ordering."
  (interactive)
  (majutsu-log-transient--toggle :reversed))

(defun majutsu-log-transient-toggle-no-graph ()
  "Toggle whether jj log should hide the ASCII graph."
  (interactive)
  (majutsu-log-transient--toggle :no-graph))

(defun majutsu-log-transient-add-path ()
  "Add a fileset/path filter to the log view."
  (interactive)
  (let* ((input (string-trim (read-from-minibuffer "Add path/pattern: ")))
         (paths (majutsu-log--state-get :filesets)))
    (when (and (not (string-empty-p input))
               (not (member input paths)))
      (majutsu-log--state-set :filesets (append paths (list input)))
      (majutsu-log-transient--redisplay))))

(defun majutsu-log-transient-clear-paths ()
  "Clear all path filters."
  (interactive)
  (majutsu-log--state-set :filesets nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-reset ()
  "Reset log state to defaults."
  (interactive)
  (majutsu-log--reset-state)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-apply ()
  "Apply the current log state by refreshing or opening the log view."
  (interactive)
  (majutsu-log--refresh-view))

(defun majutsu-log--toggle-desc (label key)
  "Return LABEL annotated with ON/OFF state for KEY."
  (if (majutsu-log--state-get key)
      (format "%s [on]" label)
    (format "%s [off]" label)))

(defun majutsu-log--value-desc (label key)
  "Return LABEL annotated with the string value stored at KEY."
  (if-let* ((value (majutsu-log--state-get key)))
      (format "%s (%s)" label value)
    label))

(defun majutsu-log--paths-desc ()
  "Return description for path filters."
  (let ((paths (majutsu-log--state-get :filesets)))
    (cond
     ((null paths) "Add path filter")
     ((= (length paths) 1) (format "Add path filter (%s)" (car paths)))
     (t (format "Add path filter (%d paths)" (length paths))))))

(defun majutsu-log-transient--redisplay ()
  "Redisplay the log transient, compatible with older transient versions."
  (if (fboundp 'transient-redisplay)
      (transient-redisplay)
    (when (fboundp 'transient--redisplay)
      (transient--redisplay))))

(transient-define-argument majutsu-log:-r ()
  :description (lambda ()
                 (majutsu-log--value-desc "Revset" :revisions))
  :class 'transient-option
  :shortarg "r"
  :argument "-r "
  :reader (lambda (&rest _)
            (let* ((current (majutsu-log--state-get :revisions))
                   (input (string-trim
                           (read-from-minibuffer "Revset (empty to clear): "
                                                 current))))
              (majutsu-log--state-set :revisions
                                      (unless (string-empty-p input) input))
              (majutsu-log--state-get :revisions))))

(transient-define-argument majutsu-log:-n ()
  :description (lambda ()
                 (majutsu-log--value-desc "Limit" :limit))
  :class 'transient-option
  :shortarg "n"
  :argument "-n "
  :reader (lambda (&rest _)
            (let* ((current (majutsu-log--state-get :limit))
                   (input (string-trim
                           (read-from-minibuffer "Limit (empty to clear): "
                                                 current))))
              (cond
               ((string-empty-p input)
                (majutsu-log--state-set :limit nil)
                nil)
               ((string-match-p "\\`[0-9]+\\'" input)
                (majutsu-log--state-set :limit input)
                input)
               (t (user-error "Limit must be a positive integer"))))))

;;;###autoload
(transient-define-prefix majutsu-log-transient ()
  "Transient interface for adjusting jj log options."
  :man-page "jj-log"
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-log--transient-description
   :class transient-columns
   ["Revisions"
    (majutsu-log:-r)
    (majutsu-log:-n)
    ("v" "Reverse order" majutsu-log-transient-toggle-reversed
     :description (lambda ()
                    (majutsu-log--toggle-desc "Reverse order" :reversed))
     :transient t)
    ("t" "Hide graph" majutsu-log-transient-toggle-no-graph
     :description (lambda ()
                    (majutsu-log--toggle-desc "Hide graph" :no-graph))
     :transient t)
    ("R" "Clear revset" majutsu-log-transient-clear-revisions
     :if (lambda () (majutsu-log--state-get :revisions))
     :transient t)
    ("N" "Clear limit" majutsu-log-transient-clear-limit
     :if (lambda () (majutsu-log--state-get :limit))
     :transient t)]
   ["Paths"
    ("a" "Add path filter" majutsu-log-transient-add-path
     :description majutsu-log--paths-desc
     :transient t)
    ("A" "Clear path filters" majutsu-log-transient-clear-paths
     :if (lambda () (majutsu-log--state-get :filesets))
     :transient t)]
   ["Actions"
    ("g" "Apply & refresh" majutsu-log-transient-apply)
    ("0" "Reset options" majutsu-log-transient-reset :transient t)
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-log)
;;; majutsu-log.el ends here
