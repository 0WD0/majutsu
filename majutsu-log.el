;;; majutsu-log.el --- Log view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Log view, graph rendering, and navigation for Majutsu.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'majutsu-template)
(require 'magit-section)
(require 'transient)
(require 'json)

;;; Classes

(defclass majutsu-commits-section (magit-section) ())
(defclass majutsu-log-graph-section (magit-section) ())
(defclass majutsu-log-entry-section (magit-section)
  ((commit-id :initarg :commit-id)
   (change-id :initarg :change-id)
   (description :initarg :description)
   (bookmarks :initarg :bookmarks)))
(defclass majutsu-status-section (magit-section) ())
(defclass majutsu-conflict-section (magit-section) ())
(defclass majutsu-conflict-file-section (magit-section)
  ((file :initarg :file)))

(defun majutsu--section-change-id (section)
  "Return the change id recorded in SECTION, if available."
  (when (and section (object-of-class-p section 'majutsu-log-entry-section))
    (or (when (and (slot-exists-p section 'change-id)
                   (slot-boundp section 'change-id))
          (majutsu--normalize-id-value (oref section change-id)))
        (let ((entry (oref section value)))
          (when (listp entry)
            (majutsu--normalize-id-value
             (plist-get entry :change-id)))))))

(defun majutsu--section-commit-id (section)
  "Return the commit id recorded in SECTION, if available."
  (when section
    (when (and (slot-exists-p section 'commit-id)
               (slot-boundp section 'commit-id))
      (majutsu--normalize-id-value (oref section commit-id)))))

(cl-defmethod magit-section-ident-value ((section majutsu-log-entry-section))
  "Identify log entry sections by their change id."
  (or (majutsu--section-change-id section)
      (majutsu--section-commit-id section)
      (let ((entry (oref section value)))
        (when (listp entry)
          (or (plist-get entry :change-id)
              (plist-get entry :commit-id))))))

;;; Log State

(defconst majutsu-log--state-template
  '(:revisions nil
    :limit nil
    :reversed nil
    :no-graph nil
    :filesets nil)
  "Default plist template describing log view options.")

(defvar majutsu-log-state (copy-sequence majutsu-log--state-template)
  "Plist capturing the current jj log view options.")

(defcustom majutsu-log-sections-hook '(majutsu-log-insert-logs
                                       majutsu-log-insert-status
                                       majutsu-log-insert-diff)
  "Hook run to insert sections in the log buffer."
  :type 'hook
  :group 'majutsu)

(defun majutsu-log--reset-state ()
  "Reset log view options to defaults."
  (setq majutsu-log-state (copy-sequence majutsu-log--state-template)))

(defun majutsu-log--state-get (key)
  "Return value for KEY in `majutsu-log-state'."
  (plist-get majutsu-log-state key))

(defun majutsu-log--state-set (key value)
  "Set KEY in `majutsu-log-state' to VALUE."
  (setq majutsu-log-state (plist-put majutsu-log-state key value)))

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

(defconst majutsu-log--default-commit-columns
  '((:field change-id :align left)
    (:field refs :align left)
    (:field empty :align left)
    (:field git-head :align left)
    (:field description :align left)
    (:field author :align right)
    (:field timestamp :align right)
    (:field commit-id :align right :visible nil)
    (:field flags :align left :visible nil)
    (:field long-desc :visible nil))
  "Default column layout for log entries.")

(defcustom majutsu-log-commit-columns majutsu-log--default-commit-columns
  "Column specification controlling how log rows are rendered.

Each element is a plist with at least `:field'. Supported keys:
- :field   - symbol identifying a known field (e.g. `change-id',
             `commit-id', `refs', `description', `author',
             `timestamp', `flags', `long-desc').
- :align   - one of `left', `right', or `center' (defaults to `left').
- :visible - non-nil to show in the buffer; nil keeps the field hidden
             but still present in the template for parsing.

Width is computed dynamically per buffer based on content; the optional
:width key is currently ignored. Required fields are injected
automatically even if omitted or hidden."
  :type '(repeat (plist :options (:field :align :width :visible)))
  :group 'majutsu)

(defvar majutsu-log--compiled-template-cache nil
  "Cached structure holding the compiled log template and column metadata.")

(defun majutsu-log--invalidate-template-cache (&rest _)
  "Reset cached compiled template when layout changes."
  (setq majutsu-log--compiled-template-cache nil)
  (setq majutsu-log--cached-entries nil))

(when (fboundp 'add-variable-watcher)
  (add-variable-watcher 'majutsu-log-commit-columns
                        #'majutsu-log--invalidate-template-cache))

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
  "Return majutsu-template form for FIELD."
  (pcase field
    ('change-id [:if [:hidden]
                    [:label "hidden"
                            [[:change_id :shortest 8]
                             " hidden"]]
                  [:label
                   [:if [:divergent] "divergent"]
                   [[:change_id :shortest 8]
                    [:if [:divergent] "??"]]]])
    ('commit-id [:commit_id :shortest 8])
    ('refs [:separate " " [:bookmarks] [:tags] [:working_copies]])
    ('bookmarks [:bookmarks])
    ('tags [:tags])
    ('working-copies [:working_copies])
    ('flags [:separate " "
                       [:if [:current_working_copy] "@"]
                       [:if [:immutable] "immutable" "mutable"]
                       [:if [:conflict] [:label "conflict" "conflict"]]
                       [:if [:git_head] "git_head"]
                       [:if [:root] "root"]
                       [:if [:empty] "(empty)"]])
    ('git-head [:if [:git_head] [:label "git_head" ""]])
    ('signature [:if [:method [:call 'config "ui.show-cryptographic-signatures"] :as_boolean]
                    [:if [:signature]
                        [:label "signature status"
                                ["["
                                 [:label [:signature :status]
                                         [:coalesce
                                          [:if [:== [:signature :status] "good"] "✓︎"]
                                          [:if [:== [:signature :status] "unknown"] "?"]
                                          "x"]]
                                 "]"]]]])
    ('empty [:if [:empty]
                [:label "empty" "∅"]])
    ('description [:if [:description]
                      [:method [:description] :first_line]
                    [:label
                     [:if [:empty] "empty"]
                     [:label
                      "description placeholder"
                      "□"]]])
    ('author [:author :name])
    ('timestamp [:committer :timestamp :ago])
    ('long-desc [:if [:description] [:json [:description]] [:json " "]])
    (_ (user-error "Unknown column field %S" field))))

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
  (let* ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (setq entry (plist-put entry :columns columns)))
  (pcase field
    ('change-id
     (setq entry (plist-put entry :change-id value)))
    ('commit-id
     (setq entry (plist-put entry :commit-id value)))
    ('refs
     (setq entry (plist-put entry :bookmarks value)))
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
    ((or 'change-id 'commit-id) 'magit-hash)
    ('refs 'magit-branch-remote)
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
        (setq parts (append parts (list formatted)))
        (setq current-width (+ current-width 1 (string-width formatted)))
        (when (and (not desc-indent) (eq field 'description))
          (setq desc-indent (- current-width (string-width formatted))))))
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

(defun majutsu-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (magit-insert-section (majutsu-log-graph-section)
    (magit-insert-heading (majutsu-log--heading-string))
    (let* ((compiled (majutsu-log--ensure-template))
           (entries (majutsu-parse-log-entries))
           (widths (majutsu-log--compute-column-widths entries compiled)))
      (majutsu-log--set-right-margin (plist-get widths :right-total))
      (dolist (entry entries)
        (magit-insert-section
            (majutsu-log-entry-section entry t
                                       :commit-id  (plist-get entry :commit-id)
                                       :change-id  (plist-get entry :change-id)
                                       :description (plist-get entry :short-desc)
                                       :bookmarks  (plist-get entry :bookmarks))
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
              (insert "\n"))))))
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
      (magit-insert-section (majutsu-status-section)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)
        (insert "\n")
        ;; Analyze status and provide hints in the minibuffer
        (majutsu--analyze-status-for-hints status-output)))))

;;; Log insert diff

(defun majutsu-log-insert-diff ()
  "Insert jj diff with hunks into current buffer asynchronously."
  (let* ((section (magit-insert-section (majutsu-diff-section)
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
      (magit-insert-section (majutsu-conflict-section)
        (magit-insert-heading "Unresolved Conflicts")
        (dolist (line (split-string output "\n" t))
          (let ((file (string-trim line)))
            (magit-insert-section (majutsu-conflict-file-section file nil :file file)
              (magit-insert-heading (propertize file 'face 'error))
              (insert "\n"))))
        (insert "\n")))))

;;; Log Navigation

(defconst majutsu--show-change-id-template
  (tpl-compile [:change_id :shortest 8]))

(defconst majutsu--show-commit-id-template
  (tpl-compile [:commit_id :shortest 8]))

(defun majutsu-current-change-id ()
  (majutsu-run-jj "log" "--no-graph" "-r" "@" "-T" majutsu--show-change-id-template))

(defun majutsu-current-commit-id ()
  (majutsu-run-jj "log" "--no-graph" "-r" "@" "-T" majutsu--show-commit-id-template))

(defun majutsu-log-goto-@ ()
  "Jump to the current changeset (@)."
  (interactive)
  (majutsu--goto-log-entry (majutsu-current-change-id)
                           (majutsu-current-commit-id)))

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

(defun majutsu--goto-log-entry (change-id &optional commit-id)
  "Move point to the log entry section matching CHANGE-ID.
When CHANGE-ID is nil, fall back to COMMIT-ID.
Return non-nil when the section could be located."
  (when-let* ((section (majutsu--find-log-entry-section change-id commit-id)))
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
        (when (and (eq (oref section type) 'majutsu-log-entry-section)
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
        (when (and (eq (oref section type) 'majutsu-log-entry-section)
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

(defun majutsu--find-log-entry-section (change-id commit-id)
  "Return the log entry section matching CHANGE-ID or COMMIT-ID, or nil."
  (when magit-root-section
    (let (found)
      (cl-labels ((walk (section)
                    (when section
                      (when (and (object-of-class-p section 'majutsu-log-entry-section)
                                 (or (and change-id
                                          (let ((section-change (majutsu--section-change-id section)))
                                            (and section-change
                                                 (equal section-change change-id))))
                                     (and commit-id
                                          (slot-boundp section 'commit-id)
                                          (equal (oref section commit-id) commit-id))))
                        (setq found section))
                      (dolist (child (oref section children))
                        (unless found
                          (walk child))))))
        (walk magit-root-section))
      found)))

(defun majutsu-log--commit-only-at-point ()
  "Return the raw commit id at point, or nil if unavailable."
  (when-let* ((section (magit-current-section)))
    (majutsu--section-commit-id section)))

(defun majutsu-log--ids-at-point ()
  "Return a plist (:change .. :commit .. :section ..) describing ids at point."
  (when-let* ((section (magit-current-section)))
    (let ((change (majutsu--section-change-id section))
          (commit (majutsu--section-commit-id section)))
      (when (or change commit)
        (list :change change :commit commit :section section)))))

(defun majutsu-log--revset-at-point ()
  "Return the preferred revset (change id if possible) at point."
  (when-let* ((ids (majutsu-log--ids-at-point)))
    (let ((change (plist-get ids :change))
          (commit (plist-get ids :commit)))
      (if (and change (string-suffix-p "?" change))
          (or commit change)
        (or change commit)))))

(defun majutsu-log--change-id-at-point ()
  "Return change id for the log entry at point, or nil otherwise."
  (majutsu--section-change-id (magit-current-section)))

(defun majutsu-log--commit-id-at-point ()
  "Get the changeset ID at point as a plain string (no text properties)."
  (or (majutsu-log--commit-only-at-point)
      (majutsu-log--change-id-at-point)))

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
  ;; Clear rebase selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-rebase-clear-selections nil t)
  ;; Clear new selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-new-clear-selections nil t)
  ;; Clear squash selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-squash-clear-selections nil t)
  ;; Clear duplicate selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-duplicate-clear-selections nil t))

(defun majutsu-log-render ()
  "Render the log buffer using cached data."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (magit-root-section)
      (run-hooks 'majutsu-log-sections-hook))))

(defun majutsu-log--refresh-buffer (target-change target-commit)
  "Implementation helper to refresh the current log buffer.
Assumes `current-buffer' is a `majutsu-log-mode' buffer."
  (majutsu--assert-mode 'majutsu-log-mode)
  (let* ((root (majutsu--root))
         (buf (current-buffer))
         (target-change (or target-change (majutsu-log--change-id-at-point)))
         (target-commit (or target-commit (majutsu-log--commit-id-at-point))))
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
             (unless (when target-change (majutsu--goto-log-entry target-change target-commit))
               (majutsu-log-goto-@))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-log-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Error: " err)))))))))

(defun majutsu-log-refresh (&optional target-change target-commit _ignore-auto _noconfirm)
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
        (majutsu-log--refresh-buffer target-change target-commit)))
     ((called-interactively-p 'interactive)
      (user-error "No Majutsu log buffer for this repository; open one with `majutsu-log`"))
     (t
      (majutsu--debug "Skipping log refresh: no log buffer for %s" (or root "unknown repo"))))))

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
    (majutsu--display-buffer-for-editor buffer)))

(defun majutsu-log--refresh-view ()
  "Refresh current log buffer or open a new one."
  (if (derived-mode-p 'majutsu-log-mode)
      (majutsu-log-refresh)
    (majutsu-log)))

;;; Operation Log

(defclass majutsu-op-log-entry-section (magit-section)
  ((op-id :initarg :op-id)
   (user :initarg :user)
   (time :initarg :time)
   (description :initarg :description)))

(defconst majutsu--op-log-template
  (tpl-compile
   [:separate "\x1e"
              [:call 'id.short]
              [:user]
              [:method [:call 'time.end] :format "%Y-%m-%d %H:%M"]
              [:description]
              "\n"]
   'Operation))

(defvar-local majutsu-op-log--cached-entries nil
  "Cached operation log entries.")

(defun majutsu-parse-op-log-entries (&optional buf log-output)
  "Parse jj op log output."
  (if (and majutsu-op-log--cached-entries (not log-output))
      majutsu-op-log--cached-entries
    (with-current-buffer (or buf (current-buffer))
      (let* ((args (list "op" "log" "--no-graph" "-T" majutsu--op-log-template))
             (output (or log-output (apply #'majutsu-run-jj args))))
        (when (and output (not (string-empty-p output)))
          (let ((lines (split-string output "\n" t))
                (entries '()))
            (dolist (line lines)
              (seq-let (id user time desc) (split-string line "\x1e")
                (push (list :op-id id :user user :time time :desc desc) entries)))
            (nreverse entries)))))))

(defun majutsu-op-log-insert-entries ()
  "Insert operation log entries."
  (magit-insert-section (majutsu-log-graph-section)
    (magit-insert-heading "Operation Log")
    (dolist (entry (majutsu-parse-op-log-entries))
      (magit-insert-section
          (majutsu-op-log-entry-section entry t
                                        :op-id (plist-get entry :op-id))
        (magit-insert-heading
          (format "%-12s %-15s %-16s %s"
                  (propertize (plist-get entry :op-id) 'face 'font-lock-constant-face)
                  (propertize (plist-get entry :user) 'face 'font-lock-variable-name-face)
                  (propertize (plist-get entry :time) 'face 'font-lock-comment-face)
                  (plist-get entry :desc)))
        (insert "\n")))))

(defun majutsu-op-log-render ()
  "Render the op log buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (magit-root-section)
      (majutsu-op-log-insert-entries))))

(defun majutsu-op-log-refresh ()
  "Refresh the op log buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-log-mode)
  (let ((root (majutsu--root))
        (buf (current-buffer)))
    (setq-local majutsu--repo-root root)
    (setq default-directory root)
    (setq majutsu-op-log--cached-entries nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading..."))
    (majutsu-run-jj-async
     (list "op" "log" "--no-graph" "-T" majutsu--op-log-template)
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-op-log-mode)
             (setq majutsu-op-log--cached-entries (majutsu-parse-op-log-entries nil output))
             (majutsu-op-log-render)))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-op-log-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Error: " err)))))))))

(defvar-keymap majutsu-op-log-mode-map
  :doc "Keymap for `majutsu-op-log-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-op-log-mode majutsu-mode "Majutsu Op Log"
  "Major mode for viewing jj operation log."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-op-log ()
  "Open the majutsu operation log."
  (interactive)
  (let* ((root (majutsu--root))
         (buffer (get-buffer-create (format "*majutsu-op: %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (majutsu-op-log-mode)
      (setq-local majutsu--repo-root root)
      (setq default-directory root)
      (majutsu-op-log-refresh))
    (majutsu--display-buffer-for-editor buffer)))
(provide 'majutsu-log)
;;; majutsu-log.el ends here
