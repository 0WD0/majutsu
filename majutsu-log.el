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
(require 'majutsu-selection)
(require 'majutsu-workspace)
(require 'json)

(defvar majutsu-buffer-log-args)
(defvar majutsu-buffer-log-revsets)
(defvar majutsu-buffer-log-filesets)

(defcustom majutsu-log-field-faces
  '((bookmarks . magit-branch-local)
    (tags . magit-tag)
    (working-copies . magit-branch-remote)
    (author . magit-log-author)
    (timestamp . magit-log-date)
    (flags . font-lock-comment-face))
  "Alist mapping log fields to face behavior.

Each entry is (FIELD . SPEC).  SPEC can be:

- t    Preserve existing font-lock-face properties produced
       by JJ and `ansi-color-apply-text-property-face'.
- nil  Remove face properties from that field.
- FACE Apply FACE to that field (overriding existing faces).

When a field is not present in this alist, it defaults to t."
  :type '(alist :tag "Field face behavior"
          :key-type (symbol :tag "Field")
          :value-type (choice (const :tag "Preserve existing faces" t)
                              (const :tag "No faces" nil)
                              (face :tag "Use this face")))
  :group 'majutsu)

;;; Section Keymaps

(defvar-keymap majutsu-commit-section-map
  :doc "Keymap for `jj-commit' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset)

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

(defcustom majutsu-log-sections-hook '(majutsu-log-insert-error-header
                                       majutsu-log-insert-logs
                                       majutsu-log-insert-status
                                       majutsu-log-insert-workspaces)
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
  [:json [:description :trim_end]]
  "Template for the long-desc column.
Note: This must return a valid JSON string (usually via :json)
to be parsed correctly.")

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
         (compiled (tpl-compile `[,majutsu-log--field-separator
                                  ,(vconcat (list :join majutsu-log--field-separator) templates)
                                  "\n"])))
    (list :template compiled
          :columns complete
          :field-order field-order)))

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
     (setq entry (plist-put entry :long-desc (string-join (cdr (string-lines (majutsu-log--parse-json-safe value))) "\n"))))
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
  (alist-get field majutsu-log-field-faces nil nil #'eq))

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
         (parts (list prefix))
         (current-width (string-width prefix))
         (desc-indent nil))
    ;; build left part without padding
    (dolist (col left-cols)
      (let* ((field (plist-get col :field))
             (raw (or (majutsu-log--entry-column entry field) ""))
             (face (majutsu-log--field-face field))
             (formatted (if face (propertize raw 'font-lock-face face) raw)))
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
               (formatted (if face (propertize formatted 'font-lock-face face) formatted)))
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

(defun majutsu-log--buffer-substring-trim-right (beg end)
  "Return buffer substring between BEG and END, trimming trailing spaces/tabs.

Preserve text properties on the retained portion."
  (save-excursion
    (goto-char end)
    (while (and (> (point) beg)
                (memq (char-before) '(?\s ?\t)))
      (backward-char 1))
    (buffer-substring beg (point))))

(defun majutsu-log--split-line-into-elems (bol eol)
  "Split the current line between BOL and EOL into elems preserving properties.

Return a list (PREFIX FIELD1 FIELD2 ...), trimming trailing whitespace
from each element like the old `string-trim-right' based implementation."
  (save-excursion
    (goto-char bol)
    (let ((sep majutsu-log--field-separator)
          (start bol)
          (elems nil))
      (while (search-forward sep eol t)
        (let ((seg-end (1- (point))))
          (push (majutsu-log--buffer-substring-trim-right start seg-end) elems)
          (setq start (point))))
      (push (majutsu-log--buffer-substring-trim-right start eol) elems)
      (nreverse elems))))

(defun majutsu-log--line-has-fields-p (bol eol)
  "Return non-nil if the line between BOL and EOL contains template fields."
  (save-excursion
    (goto-char bol)
    (search-forward majutsu-log--field-separator eol t)))

(defun majutsu-log--compute-column-widths-in-buffer (compiled)
  "Compute right-aligned column widths for COMPILED in the current buffer.

Return a plist like `majutsu-log--compute-column-widths'."
  (let* ((visible-cols (seq-filter (lambda (c) (plist-get c :visible))
                                   (plist-get compiled :columns)))
         (right-cols (seq-filter (lambda (c) (eq (plist-get c :align) 'right))
                                 visible-cols))
         (field-order (plist-get compiled :field-order))
         (right-widths nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (when (majutsu-log--line-has-fields-p bol eol)
            (let* ((elems (majutsu-log--split-line-into-elems bol eol))
                   (entry (majutsu-log--build-entry-from-elems elems field-order
                                                               (buffer-substring bol eol))))
              (dolist (col right-cols)
                (let* ((field (plist-get col :field))
                       (val (or (majutsu-log--entry-column entry field) ""))
                       (w (string-width val)))
                  (setf (alist-get field right-widths nil nil #'eq)
                        (max w (or (alist-get field right-widths nil nil #'eq) 0))))))))
        (forward-line 1)))
    (let ((right-total
           (let ((sum 0) (first t))
             (dolist (col right-cols)
               (let* ((field (plist-get col :field))
                      (w (or (alist-get field right-widths nil nil #'eq) 0)))
                 (unless first (setq sum (1+ sum)))
                 (setq first nil)
                 (setq sum (+ sum w))))
             sum)))
      (list :right right-widths :right-total right-total))))

(defun majutsu-log--wash-entry (compiled widths)
  "Wash the log entry at point using COMPILED and WIDTHS.

Assumes point is at the beginning of a commit line (a line containing
`majutsu-log--field-separator').  Replaces the raw line(s) with a
`jj-commit' section and moves point to the end of the inserted section."
  (let* ((field-order (plist-get compiled :field-order))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (elems (majutsu-log--split-line-into-elems bol eol))
         (line (buffer-substring bol eol))
         (entry (majutsu-log--build-entry-from-elems elems field-order line))
         (suffix-lines nil)
         (delete-end
          (save-excursion
            (forward-line 1)
            (while (and (not (eobp))
                        (let ((bol (line-beginning-position))
                              (eol (line-end-position)))
                          (not (majutsu-log--line-has-fields-p bol eol))))
              (push (buffer-substring (line-beginning-position) (line-end-position))
                    suffix-lines)
              (forward-line 1))
            (point))))
    (setq suffix-lines (nreverse suffix-lines))
    (delete-region bol delete-end)
    (goto-char bol)
    (let* ((id (majutsu-selection--normalize-value (plist-get entry :id)))
           (line-info (majutsu-log--format-entry-line entry compiled widths))
           (heading (plist-get line-info :line))
           (margin (plist-get line-info :margin))
           (indent (plist-get line-info :desc-indent))
           (long-desc (plist-get entry :long-desc))
           (has-body (and (stringp long-desc)
                          (not (string-empty-p (string-trim long-desc))))))
      (magit-insert-section (jj-commit id t)
        ;; Insert the visible, non-folded part of the section.  The log
        ;; contains "suffix lines" that are part of the ASCII graph but
        ;; don't carry record fields; those should remain visible even
        ;; when the section is folded.
        (insert heading)
        (insert "\n")
        (when margin
          (majutsu-log--make-margin-overlay margin))
        (dolist (suffix-line suffix-lines)
          (insert suffix-line)
          (insert "\n"))
        (when has-body
          ;; Start of foldable body: everything inserted after this point is
          ;; hidden when the section is collapsed.
          (magit-insert-heading)
          (let ((indented (majutsu--indent-string long-desc (or indent 0))))
            (magit-insert-section-body
              (insert indented)
              (insert "\n"))))))))

(defun majutsu-log--wash-logs (_args)
  "Wash jj log output in the current (narrowed) buffer region.

This function is meant to be used as a WASHER for `majutsu-jj-wash'."
  (let* ((compiled (majutsu-log--ensure-template))
         (widths (majutsu-log--compute-column-widths-in-buffer compiled)))
    (majutsu-log--set-right-margin (plist-get widths :right-total))
    (goto-char (point-min))
    ;; Convert raw output incrementally, one commit at a time, like Magit.
    (while (not (eobp))
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (cond
         ((majutsu-log--line-has-fields-p bol eol)
          (majutsu-log--wash-entry compiled widths))
         (t
          ;; Leading/trailing graph-only noise; drop it.
          (magit-delete-line)))))
    (insert "\n")))

(defun majutsu-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (magit-insert-section (lograph)
    (magit-insert-heading (majutsu-log--heading-string))
    (majutsu-jj-wash #'majutsu-log--wash-logs nil (majutsu-log--build-args))
    (insert "\n")))

;;; Log insert status

(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (let ((status-output (majutsu-run-jj "status")))
    (when (and status-output (not (string-empty-p status-output)))
      (magit-insert-section (status)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)))))

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
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end-if-owner nil t))

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
(defun majutsu-log ()
  "Open the majutsu log buffer."
  (interactive)
  (majutsu-log-setup-buffer))

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

(defun majutsu-log-transient-add-path ()
  "Add a fileset/path filter to the log view."
  (interactive)
  (let* ((input (string-trim (read-from-minibuffer "Add path/pattern: ")))
         (paths (caddr (majutsu-log--get-value 'majutsu-log-mode 'direct))))
    (when (and (not (string-empty-p input))
               (not (member input paths)))
      (pcase-let ((`(,args ,revsets ,_filesets)
                   (majutsu-log--get-value 'majutsu-log-mode 'direct)))
        (majutsu-log--set-value 'majutsu-log-mode args revsets (append paths (list input))))
      (majutsu-log-transient--redisplay))))

(defun majutsu-log-transient-clear-paths ()
  "Clear all path filters."
  (interactive)
  (pcase-let ((`(,args ,revsets ,_filesets)
               (majutsu-log--get-value 'majutsu-log-mode 'direct)))
    (majutsu-log--set-value 'majutsu-log-mode args revsets nil))
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

(defun majutsu-log--paths-desc ()
  "Return description for path filters."
  (let ((paths (caddr (majutsu-log--get-value 'majutsu-log-mode 'direct))))
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

;;; Arguments
;;;; Prefix Classes

(defclass majutsu-log-prefix (transient-prefix)
  ((history-key :initform 'majutsu-log)
   (major-mode :initform 'majutsu-log-mode)))

;;;; Prefix Methods

(cl-defmethod transient-init-value ((obj majutsu-log-prefix))
  (pcase-let ((`(,args ,_revsets ,_filesets)
               (majutsu-log--get-value (oref obj major-mode) 'prefix)))
    (oset obj value args)))

(cl-defmethod transient-set-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode))
         (args (transient-args (oref obj command))))
    (pcase-let ((`(,_old-args ,revsets ,filesets)
                 (majutsu-log--get-value mode 'direct)))
      (majutsu-log--set-value mode args revsets filesets)
      (transient--history-push obj)
      (majutsu-refresh))))

(cl-defmethod transient-save-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode))
         (args (transient-args (oref obj command))))
    (pcase-let ((`(,_old-args ,revsets ,filesets)
                 (majutsu-log--get-value mode 'direct)))
      (majutsu-log--set-value mode args revsets filesets t)
      (transient--history-push obj)
      (majutsu-refresh))))

(transient-define-argument majutsu-log:-n ()
  :description (lambda ()
                 (majutsu-log--value-desc
                  "Limit"
                  (majutsu-log--args-get-option
                   (car (majutsu-log--get-value 'majutsu-log-mode 'direct))
                   "-n")))
  :class 'transient-option
  :shortarg "-n"
  :argument "-n"
  :reader (lambda (&rest _)
            (pcase-let* ((`(,args ,_revsets ,_filesets)
                          (majutsu-log--get-value 'majutsu-log-mode 'direct))
                         (current (majutsu-log--args-get-option args "-n"))
                         (input (string-trim
                                 (read-from-minibuffer "Limit (empty to clear): "
                                                       current))))
              (cond
               ((string-empty-p input)
                nil)
               ((string-match-p "\\`[0-9]+\\'" input)
                input)
               (t (user-error "Limit must be a positive integer"))))))

(transient-define-argument majutsu-log:--reversed ()
  :description "Reverse order"
  :class 'transient-switch
  :key "v"
  :argument "--reversed")

(transient-define-argument majutsu-log:--no-graph ()
  :description "Hide graph"
  :class 'transient-switch
  :key "t"
  :argument "--no-graph")

;;;###autoload
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
    (majutsu-log:-n)
    (majutsu-log:--reversed)
    (majutsu-log:--no-graph)
    ("R" "Clear revset" majutsu-log-transient-clear-revisions
     :if (lambda () (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct)))
     :transient t)
    ]
   ["Paths"
    ("a" "Add path filter" majutsu-log-transient-add-path
     :description majutsu-log--paths-desc
     :transient t)
    ("A" "Clear path filters" majutsu-log-transient-clear-paths
     :if (lambda () (caddr (majutsu-log--get-value 'majutsu-log-mode 'direct)))
     :transient t)]
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
    (setq-local majutsu-buffer-log-args (transient-args transient-current-command))
    (majutsu-refresh-buffer))))

;;; _
(provide 'majutsu-log)
;;; majutsu-log.el ends here
