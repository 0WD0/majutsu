;;; majutsu-graph-entry.el --- Graph entry pipeline helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared graph-entry protocol support for Majutsu graph views.
;; jj renders the graph; Majutsu embeds module tokens inside graph nodes,
;; parses the output back into entries, and renders real Magit sections.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit-section)
(require 'majutsu-section)
(require 'majutsu-template)

(defconst majutsu-graph-entry-field-separator "\x1e"
  "Separator inserted between fields inside one module payload.")

(defconst majutsu-graph-entry-field-line-separator "\x1f"
  "Encoded newline separator used inside template field payloads.")

(defconst majutsu-graph-entry-record-marker "\x1d"
  "Control marker prefix for module boundaries inside graph output.")

(defconst majutsu-graph-entry-start-token
  (concat majutsu-graph-entry-record-marker "S")
  "Marker that starts an entry and heading payload.")

(defconst majutsu-graph-entry-tail-token
  (concat majutsu-graph-entry-record-marker "T")
  "Marker that starts the tail payload.")

(defconst majutsu-graph-entry-body-token
  (concat majutsu-graph-entry-record-marker "B")
  "Marker that starts the body payload.")

(defconst majutsu-graph-entry-meta-token
  (concat majutsu-graph-entry-record-marker "M")
  "Marker that starts the metadata payload.")

(defconst majutsu-graph-entry-end-token
  (concat majutsu-graph-entry-record-marker "E")
  "Marker that terminates an entry.")

(defconst majutsu-graph-entry-module-order '(heading tail body metadata)
  "Module parse/render order for sequential graph-entry payloads.")

(defconst majutsu-graph-entry--tail-terminal-padding 1
  "Terminal columns reserved to the right of tail-aligned text.")

(defvar-local majutsu-graph-entry-buffer-compiled nil
  "Compiled graph-entry metadata for the current buffer.")

(defvar-local majutsu-graph-entry-cached-entries nil
  "Parsed graph entries for the current buffer.")

(defun majutsu-graph-entry-default-tokens ()
  "Return the default graph-entry token plist."
  (list :start majutsu-graph-entry-start-token
        :tail majutsu-graph-entry-tail-token
        :body majutsu-graph-entry-body-token
        :metadata majutsu-graph-entry-meta-token
        :end majutsu-graph-entry-end-token))

(defun majutsu-graph-entry--tokens (profile)
  "Return token plist for PROFILE."
  (or (plist-get profile :tokens)
      (majutsu-graph-entry-default-tokens)))

(defun majutsu-graph-entry--profile (compiled)
  "Return graph-entry profile from COMPILED."
  (plist-get compiled :profile))

(defun majutsu-graph-entry--token (compiled key)
  "Return token KEY from COMPILED."
  (plist-get (plist-get compiled :tokens) key))

(defun majutsu-graph-entry-post-decode-line-separator (value &optional _ctx)
  "Decode field line separators inside VALUE."
  (if (stringp value)
      (subst-char-in-string
       (aref majutsu-graph-entry-field-line-separator 0)
       ?\n
       value t)
    value))

(defun majutsu-graph-entry--default-module-for-field (profile field)
  "Return default module for FIELD using PROFILE."
  (or (alist-get field (plist-get profile :default-modules) nil nil #'eq)
      (user-error "Field %S requires explicit :module" field)))

(defun majutsu-graph-entry--default-postprocessors-for-field (profile field)
  "Return default postprocessors for FIELD using PROFILE."
  (append (plist-get profile :default-postprocessors)
          (alist-get field (plist-get profile :field-postprocessors) nil nil #'eq)))

(defun majutsu-graph-entry-normalize-postprocessors (profile post field)
  "Normalize POST for FIELD using PROFILE into a function list."
  (let* ((defaults (majutsu-graph-entry--default-postprocessors-for-field
                    profile field))
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

(defun majutsu-graph-entry-normalize-column-spec (profile spec)
  "Normalize one column SPEC using PROFILE."
  (let* ((col (cond
               ((and (plistp spec) (plist-get spec :field)) spec)
               ((symbolp spec) (list :field spec))
               (t (user-error "Invalid column spec: %S" spec))))
         (field (plist-get col :field))
         (module (if (plist-member col :module)
                     (plist-get col :module)
                   (majutsu-graph-entry--default-module-for-field
                    profile field)))
         (face (if (plist-member col :face)
                   (plist-get col :face)
                 t))
         (post (if (plist-member col :post)
                   (plist-get col :post)
                 :default))
         (template (plist-get col :template)))
    (setq module (if (keywordp module)
                     (intern (substring (symbol-name module) 1))
                   module))
    (unless (memq module majutsu-graph-entry-module-order)
      (user-error "Column %S has invalid :module %S" field module))
    (unless (or (eq face t) (null face) (symbolp face))
      (user-error "Column %S has invalid :face %S" field face))
    (append (list :field field
                  :module module
                  :face face
                  :post (majutsu-graph-entry-normalize-postprocessors
                         profile post field))
            (when (plist-member col :template)
              (list :template template)))))

(defun majutsu-graph-entry-ensure-required-columns (profile columns)
  "Ensure required PROFILE columns are present in COLUMNS."
  (let ((present (mapcar (lambda (column) (plist-get column :field)) columns)))
    (dolist (req (plist-get profile :required-fields))
      (let ((field (if (and (plistp req) (plist-get req :field))
                       (plist-get req :field)
                     req)))
        (unless (memq field present)
          (setq columns
                (append columns
                        (list (majutsu-graph-entry-normalize-column-spec
                               profile req)))))))
    columns))

(defun majutsu-graph-entry-module-columns (compiled module)
  "Return compiled column specs for MODULE from COMPILED."
  (alist-get module (plist-get compiled :module-columns) nil nil #'eq))

(defun majutsu-graph-entry-assign-column-instances (columns)
  "Return COLUMNS with stable per-instance ids assigned."
  (cl-loop for column in columns
           for idx from 0
           collect (plist-put (copy-sequence column) :instance idx)))

(defun majutsu-graph-entry--column-template (profile column)
  "Return template form for COLUMN using PROFILE."
  (or (plist-get column :template)
      (let ((fn (plist-get profile :template-function))
            (field (plist-get column :field)))
        (if fn
            (funcall fn field)
          (user-error "Column %S has no :template" field)))))

(defun majutsu-graph-entry-build-module-template-form (templates)
  "Return a template form joining TEMPLATES with field separators."
  (cond
   ((null templates) "")
   ((null (cdr templates)) (car templates))
   (t
    (let ((forms nil)
          (first t))
      (dolist (template templates)
        (unless first
          (setq forms (append forms (list majutsu-graph-entry-field-separator))))
        (setq first nil)
        (setq forms (append forms (list template))))
      (cons :concat forms)))))

(defun majutsu-graph-entry-compile (profile &optional columns)
  "Compile PROFILE COLUMNS into a jj template and layout metadata."
  (let* ((source-columns
          (or columns
              (when-let* ((var (plist-get profile :columns-var)))
                (symbol-value var))))
         (normalized (mapcar (lambda (spec)
                               (majutsu-graph-entry-normalize-column-spec
                                profile spec))
                             source-columns))
         (complete (majutsu-graph-entry-assign-column-instances
                    (majutsu-graph-entry-ensure-required-columns
                     profile normalized)))
         (module-columns
          (mapcar (lambda (module)
                    (cons module
                          (seq-filter (lambda (column)
                                        (eq (plist-get column :module) module))
                                      complete)))
                  majutsu-graph-entry-module-order))
         (tokens (majutsu-graph-entry--tokens profile))
         (module-form
          (lambda (module)
            (majutsu-graph-entry-build-module-template-form
             (mapcar (lambda (column)
                       (majutsu-graph-entry--column-template profile column))
                     (alist-get module module-columns nil nil #'eq)))))
         (form `[:concat
                 ,(plist-get tokens :start)
                 ,(funcall module-form 'heading)
                 ,(plist-get tokens :tail)
                 ,(funcall module-form 'tail)
                 ,(plist-get tokens :body)
                 ,(funcall module-form 'body)
                 ,(plist-get tokens :metadata)
                 ,(funcall module-form 'metadata)
                 ,(plist-get tokens :end)
                 "\n"])
         (template (majutsu-template-compile form (plist-get profile :self-type))))
    (list :profile profile
          :template template
          :columns complete
          :module-columns module-columns
          :tokens tokens)))

(defun majutsu-graph-entry-split-by-separator (value separator)
  "Split VALUE by one-character SEPARATOR, preserving empty fields."
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

(defun majutsu-graph-entry-join-lines (lines)
  "Join LINES with literal newlines, preserving string properties."
  (if (null lines)
      ""
    (let ((out (car lines)))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" line))))))

(defun majutsu-graph-entry-line-token-position (token bol eol &optional start)
  "Return start position of TOKEN between BOL and EOL, or nil."
  (save-excursion
    (goto-char (or start bol))
    (when (search-forward token eol t)
      (- (point) (length token)))))

(defun majutsu-graph-entry-parse-trailing-payloads (compiled payload)
  "Parse tail, body, and metadata segments from PAYLOAD using COMPILED."
  (let ((tail-token (majutsu-graph-entry--token compiled :tail))
        (body-token (majutsu-graph-entry--token compiled :body))
        (meta-token (majutsu-graph-entry--token compiled :metadata))
        (end-token (majutsu-graph-entry--token compiled :end)))
    (when (string-prefix-p tail-token payload)
      (let* ((tail-start (length tail-token))
             (body-pos (string-match (regexp-quote body-token)
                                     payload tail-start))
             (meta-pos (and body-pos
                            (string-match (regexp-quote meta-token)
                                          payload (+ body-pos (length body-token)))))
             (end-pos (and meta-pos
                           (string-match (regexp-quote end-token)
                                         payload (+ meta-pos (length meta-token))))))
        (when (and body-pos meta-pos end-pos)
          (let ((trailing (substring payload (+ end-pos (length end-token)))))
            (when (string-empty-p trailing)
              (list :tail (substring payload tail-start body-pos)
                    :body (substring payload (+ body-pos (length body-token)) meta-pos)
                    :metadata (substring payload
                                         (+ meta-pos (length meta-token))
                                         end-pos)))))))))

(defun majutsu-graph-entry-split-module-values (payload count)
  "Split PAYLOAD into COUNT field values."
  (if (<= count 0)
      nil
    (let ((values (majutsu-graph-entry-split-by-separator
                   (or payload "")
                   majutsu-graph-entry-field-separator)))
      (cond
       ((< (length values) count)
        (append values (make-list (- count (length values)) "")))
       ((> (length values) count)
        (seq-take values count))
       (t values)))))

(defun majutsu-graph-entry--decode-transport-value (profile value)
  "Decode transport-level escapes in VALUE using PROFILE."
  (let ((fn (or (plist-get profile :decode-function)
                #'majutsu-graph-entry-post-decode-line-separator)))
    (funcall fn value)))

(defun majutsu-graph-entry-apply-postprocessor (fn value ctx)
  "Apply postprocessor FN to VALUE with CTX."
  (condition-case err
      (condition-case _
          (funcall fn value ctx)
        (wrong-number-of-arguments
         (funcall fn value)))
    (error
     (message "majutsu graph-entry postprocessor failed (%S on %S): %s"
              fn (plist-get ctx :field) (error-message-string err))
     value)))

(defun majutsu-graph-entry-apply-postprocessors (value postprocessors ctx)
  "Apply POSTPROCESSORS to VALUE with CTX sequentially."
  (let ((out value))
    (dolist (fn postprocessors out)
      (setq out (majutsu-graph-entry-apply-postprocessor fn out ctx)))))

(defun majutsu-graph-entry--canonical-field-value (profile field value)
  "Return canonical FIELD VALUE using PROFILE postprocessors."
  (majutsu-graph-entry-apply-postprocessors
   value
   (majutsu-graph-entry--default-postprocessors-for-field profile field)
   (list :field field :module 'canonical :raw-value value :canonical t)))

(defun majutsu-graph-entry-record-canonical-field (entry field value)
  "Record canonical FIELD VALUE onto ENTRY."
  (let ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (plist-put entry :columns columns)))

(defun majutsu-graph-entry--record-field (profile entry field value)
  "Record FIELD VALUE on ENTRY using PROFILE."
  (if-let* ((fn (plist-get profile :record-field-function)))
      (funcall fn entry field value)
    (majutsu-graph-entry-record-canonical-field entry field value)))

(defun majutsu-graph-entry-record-column-value (entry column value)
  "Record per-instance VALUE for COLUMN onto ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values)))
    (when instance
      (setf (alist-get instance column-values nil nil #'eql) value)
      (setq entry (plist-put entry :column-values column-values)))
    entry))

(defun majutsu-graph-entry-record-module-fields (entry module payload compiled)
  "Record MODULE PAYLOAD values into ENTRY using COMPILED layout."
  (let* ((profile (majutsu-graph-entry--profile compiled))
         (columns (majutsu-graph-entry-module-columns compiled module))
         (values (majutsu-graph-entry-split-module-values payload (length columns)))
         (stored nil))
    (cl-loop for column in columns
             for raw-value in values
             do (let* ((field (plist-get column :field))
                       (decoded (majutsu-graph-entry--decode-transport-value
                                 profile raw-value))
                       (canonical (majutsu-graph-entry--canonical-field-value
                                   profile field decoded)))
                  (setq entry (majutsu-graph-entry--record-field
                               profile entry field canonical))
                  (let* ((ctx (list :field field
                                    :module module
                                    :column column
                                    :entry entry
                                    :raw-value decoded
                                    :canonical-value canonical))
                         (out (majutsu-graph-entry-apply-postprocessors
                               decoded (plist-get column :post) ctx)))
                    (setq entry (majutsu-graph-entry-record-column-value
                                 entry column out))
                    (push out stored))))
    (let ((modules (plist-get entry :modules)))
      (setf (alist-get module modules nil nil #'eq) (nreverse stored))
      (setq entry (plist-put entry :modules modules)))
    entry))

(defun majutsu-graph-entry-parse-at-point (compiled)
  "Parse one sequentially encoded graph entry at point using COMPILED."
  (let* ((entry-beg (line-beginning-position))
         (start-token (majutsu-graph-entry--token compiled :start))
         (tail-token (majutsu-graph-entry--token compiled :tail))
         (bol entry-beg)
         (eol (line-end-position))
         (start-pos (majutsu-graph-entry-line-token-position
                     start-token bol eol)))
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
                                    (+ start-pos (length start-token))
                                  prefix-end))
                 (segment-pos (majutsu-graph-entry-line-token-position
                               tail-token bol eol content-start)))
            (if segment-pos
                (progn
                  (push prefix heading-prefixes)
                  (push (buffer-substring content-start segment-pos)
                        heading-segments)
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
          (when-let* ((payloads (majutsu-graph-entry-parse-trailing-payloads
                                 compiled trailing-payload)))
            (let* ((profile (majutsu-graph-entry--profile compiled))
                   (entry (list :beg entry-beg
                                :graph-entry-profile (plist-get profile :name)
                                :indent indent
                                :columns nil
                                :column-values nil
                                :modules nil
                                :heading-prefixes (nreverse heading-prefixes)))
                   (heading-payload
                    (majutsu-graph-entry-join-lines
                     (nreverse heading-segments))))
              (setq entry (majutsu-graph-entry-record-module-fields
                           entry 'heading heading-payload compiled))
              (setq entry (majutsu-graph-entry-record-module-fields
                           entry 'tail (plist-get payloads :tail) compiled))
              (setq entry (majutsu-graph-entry-record-module-fields
                           entry 'body (plist-get payloads :body) compiled))
              (setq entry (majutsu-graph-entry-record-module-fields
                           entry 'metadata (plist-get payloads :metadata) compiled))
              (let ((suffix-lines nil))
                (while (and (not (eobp))
                            (let ((next-bol (line-beginning-position))
                                  (next-eol (line-end-position)))
                              (not (majutsu-graph-entry-line-token-position
                                    start-token next-bol next-eol))))
                  (push (buffer-substring (line-beginning-position)
                                          (line-end-position))
                        suffix-lines)
                  (forward-line 1))
                (setq entry (plist-put entry :suffix-lines
                                       (nreverse suffix-lines)))
                (setq entry (plist-put entry :end (point))))
              entry)))))))

(defun majutsu-graph-entry-parse-buffer (compiled)
  "Parse all sequentially encoded graph entries in current buffer."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((entry (majutsu-graph-entry-parse-at-point compiled)))
          (if entry
              (push entry entries)
            (forward-line 1)))))
    (nreverse entries)))

(defun majutsu-graph-entry-apply-line-prefix-span
    (start end line-prefix-str &optional wrap-prefix-str)
  "Apply display-only prefix strings to START..END."
  (when (< start end)
    (add-text-properties
     start end
     (list 'line-prefix (or line-prefix-str "")
           'wrap-prefix (or wrap-prefix-str line-prefix-str "")))))

(defun majutsu-graph-entry-insert-prefixed-line (content prefix)
  "Insert CONTENT as one line with display-only PREFIX."
  (let ((start (point)))
    (insert (or content "") "\n")
    (majutsu-graph-entry-apply-line-prefix-span start (point) prefix)))

(defun majutsu-graph-entry-split-prefix-line (line prefix-width)
  "Split LINE into (PREFIX . CONTENT) using PREFIX-WIDTH characters."
  (let* ((width (max 0 (min (or prefix-width 0) (length (or line "")))))
         (text (or line "")))
    (cons (substring text 0 width)
          (substring text width))))

(defun majutsu-graph-entry-column (entry field)
  "Return canonical value for FIELD stored on ENTRY."
  (alist-get field (plist-get entry :columns) nil nil #'eq))

(defun majutsu-graph-entry-column-value (entry column)
  "Return per-instance value for COLUMN stored on ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values))
         (missing (make-symbol "majutsu-graph-entry-missing-instance"))
         (value (if instance
                    (alist-get instance column-values missing nil #'eql)
                  missing)))
    (if (eq value missing)
        (majutsu-graph-entry-column entry (plist-get column :field))
      value)))

(defun majutsu-graph-entry-current-compiled (&optional compiled)
  "Return COMPILED or the current buffer's graph-entry layout."
  (or compiled
      majutsu-graph-entry-buffer-compiled
      (user-error "No graph-entry layout in current buffer")))

(defun majutsu-graph-entry-text-property-near-point (property &optional pos)
  "Return PROPERTY near POS, preferring the previous character."
  (let ((pos (or pos (point))))
    (or (and (> pos (point-min))
             (get-text-property (1- pos) property))
        (get-text-property pos property))))

(defun majutsu-graph-entry-entry-id (entry compiled)
  "Return the stable entry id for ENTRY using COMPILED."
  (funcall (plist-get (majutsu-graph-entry--profile compiled)
                   :entry-id-function)
           entry))

(defun majutsu-graph-entry-entry-for-id (id compiled &optional entries)
  "Return parsed graph entry ID from ENTRIES using COMPILED."
  (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
         (entries (or entries majutsu-graph-entry-cached-entries))
         (profile (majutsu-graph-entry--profile compiled))
         (entry-id-function (plist-get profile :entry-id-function))
         (section-value-function (plist-get profile :section-value-function)))
    (when (and id (not (string-empty-p id)))
      (seq-find (lambda (entry)
                  (let ((entry-id (funcall entry-id-function entry))
                        (section-value (and section-value-function
                                            (funcall section-value-function entry))))
                    (or (equal id entry-id)
                        (and section-value (equal id section-value)))))
                entries))))

(defun majutsu-graph-entry-entry-at-point (&optional compiled entries)
  "Return the parsed graph entry at point, or nil if unavailable."
  (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
         (entries (or entries majutsu-graph-entry-cached-entries))
         (profile (majutsu-graph-entry--profile compiled))
         (section-class (plist-get profile :section-class)))
    (or (when-let* ((entry-id (majutsu-graph-entry-text-property-near-point
                               'majutsu-graph-entry-id)))
          (majutsu-graph-entry-entry-for-id entry-id compiled entries))
        (when-let* ((section-value (and section-class
                                        (magit-section-value-if section-class))))
          (majutsu-graph-entry-entry-for-id section-value compiled entries)))))

(defun majutsu-graph-entry-field-copy-string (value)
  "Return canonical clipboard text for field VALUE."
  (cond
   ((null value) "")
   ((stringp value) (substring-no-properties value))
   ((listp value) (mapconcat #'majutsu-graph-entry-field-copy-string value "\n"))
   (t (format "%s" value))))

(defun majutsu-graph-entry-field-value-present-p (value)
  "Return non-nil if canonical field VALUE should be offered for copying."
  (cond
   ((null value) nil)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   (t t)))

(defun majutsu-graph-entry-entry-field-value (entry field &optional missing)
  "Return canonical FIELD value from ENTRY, or MISSING when unavailable."
  (alist-get field (plist-get entry :columns) missing nil #'eq))

(defun majutsu-graph-entry-entry-field-candidate-preview (entry field)
  "Return one-line preview string for FIELD on ENTRY."
  (let* ((text (majutsu-graph-entry-field-copy-string
                (majutsu-graph-entry-entry-field-value entry field)))
         (text (replace-regexp-in-string "[\n\r\t ]+" " " text nil t)))
    (if (> (length text) 48)
        (concat (substring text 0 48) "…")
      text)))

(defun majutsu-graph-entry-entry-copyable-fields (entry compiled)
  "Return copyable canonical field symbols for ENTRY using COMPILED order."
  (let ((fields nil)
        (seen nil))
    (dolist (column (plist-get (majutsu-graph-entry-current-compiled compiled) :columns))
      (let* ((field (plist-get column :field))
             (value (majutsu-graph-entry-entry-field-value entry field :majutsu-missing)))
        (when (and (not (memq field seen))
                   (not (eq value :majutsu-missing))
                   (majutsu-graph-entry-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (dolist (cell (plist-get entry :columns))
      (let ((field (car cell))
            (value (cdr cell)))
        (when (and (not (memq field seen))
                   (majutsu-graph-entry-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (nreverse fields)))

(defun majutsu-graph-entry-read-entry-field (entry compiled &optional prompt)
  "Read one canonical field from ENTRY using COMPILED.
When PROMPT is nil, use a default graph-entry prompt."
  (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
         (default-field (majutsu-graph-entry-text-property-near-point
                         'majutsu-graph-entry-field))
         (candidates (mapcar (lambda (field)
                               (cons (format "%s\t%s"
                                             field
                                             (majutsu-graph-entry-entry-field-candidate-preview
                                              entry field))
                                     field))
                             (majutsu-graph-entry-entry-copyable-fields entry compiled)))
         (default (car (rassoc default-field candidates)))
         (choice (completing-read (or prompt "Copy entry field: ")
                                  (mapcar #'car candidates)
                                  nil t nil nil default)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown entry field %S" choice))))

(defun majutsu-graph-entry-copy-string (string)
  "Copy STRING to the kill ring and echo it."
  (kill-new string)
  (message "%s" string))

(defun majutsu-graph-entry-copy-entry-field-value (entry field &optional _compiled)
  "Copy canonical FIELD value from ENTRY to the kill ring."
  (let ((value (majutsu-graph-entry-entry-field-value entry field :majutsu-missing)))
    (when (eq value :majutsu-missing)
      (user-error "Field %s is unavailable for the current entry" field))
    (unless (majutsu-graph-entry-field-value-present-p value)
      (user-error "Field %s is empty for the current entry" field))
    (majutsu-graph-entry-copy-string
     (majutsu-graph-entry-field-copy-string value))))

(defun majutsu-graph-entry-copy-field (&optional compiled entries)
  "Copy the rendered value of the graph-entry field at point."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
           (entries (or entries majutsu-graph-entry-cached-entries))
           (entry (or (majutsu-graph-entry-entry-at-point compiled entries)
                      (user-error "No graph entry at point")))
           (instance (majutsu-graph-entry-text-property-near-point
                      'majutsu-graph-entry-column))
           (field (majutsu-graph-entry-text-property-near-point
                   'majutsu-graph-entry-field))
           (module (majutsu-graph-entry-text-property-near-point
                    'majutsu-graph-entry-module))
           (column (or (and instance
                            (seq-find (lambda (candidate)
                                        (eql (plist-get candidate :instance) instance))
                                      (plist-get compiled :columns)))
                       (and field module
                            (seq-find (lambda (candidate)
                                        (and (eq (plist-get candidate :field) field)
                                             (eq (plist-get candidate :module) module)))
                                      (plist-get compiled :columns))))))
      (unless column
        (user-error "No graph field at point"))
      (majutsu-graph-entry-copy-string
       (majutsu-graph-entry-render-column-text entry column t)))))

(defun majutsu-graph-entry-copy-module (&optional compiled entries)
  "Copy the rendered graph-entry module at point."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
           (entries (or entries majutsu-graph-entry-cached-entries))
           (entry (or (majutsu-graph-entry-entry-at-point compiled entries)
                      (user-error "No graph entry at point")))
           (module (majutsu-graph-entry-text-property-near-point
                    'majutsu-graph-entry-module))
           (text (pcase module
                   ('heading (majutsu-graph-entry-render-heading-content
                              entry compiled nil t))
                   ('tail (or (majutsu-graph-entry-render-tail entry compiled nil t)
                              ""))
                   ('body (or (majutsu-graph-entry-render-body entry compiled nil t)
                              ""))
                   (_ nil))))
      (unless text
        (user-error "No graph module at point"))
      (majutsu-graph-entry-copy-string text))))

(defun majutsu-graph-entry-copy-entry-field (&optional compiled entries)
  "Copy a canonical field from the current graph entry."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
           (entries (or entries majutsu-graph-entry-cached-entries))
           (entry (or (majutsu-graph-entry-entry-at-point compiled entries)
                      (user-error "No graph entry at point")))
           (field (majutsu-graph-entry-read-entry-field entry compiled)))
      (majutsu-graph-entry-copy-entry-field-value entry field compiled))))

(defun majutsu-graph-entry-copy-commit-id (&optional compiled entries)
  "Copy the current graph entry's commit hash."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-graph-entry-current-compiled compiled))
           (entries (or entries majutsu-graph-entry-cached-entries))
           (entry (or (majutsu-graph-entry-entry-at-point compiled entries)
                      (user-error "No graph entry at point"))))
      (majutsu-graph-entry-copy-entry-field-value entry 'commit-id compiled))))

(defmacro majutsu-graph-entry-define-copy-transient (name doc)
  "Define NAME as a graph-entry copy transient with DOC."
  `(transient-define-prefix ,name ()
     ,doc
     [[("s" "Section value" majutsu-copy-section-value)
       ("f" "Visible field at point" majutsu-graph-entry-copy-field)
       ("F" "Entry field…" majutsu-graph-entry-copy-entry-field)
       ("h" "Commit hash" majutsu-graph-entry-copy-commit-id)
       ("m" "Visible module at point" majutsu-graph-entry-copy-module)]]))

;;;###autoload(autoload 'majutsu-graph-entry-copy-transient "majutsu-graph-entry" nil t)
(majutsu-graph-entry-define-copy-transient
 majutsu-graph-entry-copy-transient
 "Transient for semantic copy commands in graph-entry buffers.")

(defun majutsu-graph-entry-display-string (value)
  "Return VALUE converted to a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value) (mapconcat #'majutsu-graph-entry-display-string value " "))
   (t (format "%s" value))))

(defun majutsu-graph-entry-apply-face-policy (value face)
  "Apply FACE policy to VALUE and return display string."
  (let ((text (or value "")))
    (cond
     ((eq face t) text)
     ((null face) (substring-no-properties text))
     (t (propertize (substring-no-properties text) 'font-lock-face face)))))

(defun majutsu-graph-entry--compat-prefixes (compiled)
  "Return compatibility property prefixes for COMPILED."
  (let* ((profile (majutsu-graph-entry--profile compiled))
         (prefixes (or (plist-get profile :compat-property-prefixes)
                       (plist-get profile :compat-property-prefix))))
    (ensure-list prefixes)))

(defun majutsu-graph-entry--property-symbol (prefix suffix)
  "Return property symbol PREFIX-SUFFIX."
  (intern (format "%s-%s" prefix suffix)))

(defun majutsu-graph-entry--append-compat-properties (props compiled values)
  "Append compatibility property VALUES to PROPS for COMPILED."
  (dolist (prefix (majutsu-graph-entry--compat-prefixes compiled) props)
    (while values
      (let ((suffix (pop values))
            (value (pop values)))
        (setq props
              (append props
                      (list (majutsu-graph-entry--property-symbol prefix suffix)
                            value)))))))

(defun majutsu-graph-entry-content-properties
    (compiled entry-id module &optional column)
  "Return content properties for ENTRY-ID in MODULE."
  (let ((props `(majutsu-graph-entry-profile
                 ,(plist-get (majutsu-graph-entry--profile compiled) :name)
                 majutsu-graph-entry-module ,module
                 majutsu-graph-entry-id ,entry-id)))
    (when column
      (setq props (append props
                          `(majutsu-graph-entry-field ,(plist-get column :field)
                            majutsu-graph-entry-column ,(plist-get column :instance)))))
    (majutsu-graph-entry--append-compat-properties
     props compiled
     (append (list 'module module 'entry-id entry-id)
             (when column
               (list 'field (plist-get column :field)
                     'column (plist-get column :instance)))))))

(defun majutsu-graph-entry-decoration-properties
    (compiled entry-id module decoration)
  "Return decoration properties for ENTRY-ID in MODULE."
  (majutsu-graph-entry--append-compat-properties
   `(majutsu-graph-entry-profile
     ,(plist-get (majutsu-graph-entry--profile compiled) :name)
     majutsu-graph-entry-module ,module
     majutsu-graph-entry-id ,entry-id
     majutsu-graph-entry-decoration ,decoration)
   compiled
   (list 'module module 'entry-id entry-id 'decoration decoration)))

(defun majutsu-graph-entry-tail-spacer-properties (compiled entry-id display)
  "Return tail spacer properties for ENTRY-ID and DISPLAY."
  (majutsu-graph-entry--append-compat-properties
   `(majutsu-graph-entry-module tail
     majutsu-graph-entry-id ,entry-id
     majutsu-graph-entry-decoration tail-spacer
     majutsu-graph-entry-tail-spacer t
     display ,display)
   compiled
   (list 'module 'tail
         'entry-id entry-id
         'decoration 'tail-spacer
         'tail-spacer t)))

(defun majutsu-graph-entry-propertize-content
    (text compiled entry-id module &optional column)
  "Return TEXT tagged as MODULE content for ENTRY-ID and COLUMN."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-graph-entry-content-properties
              compiled entry-id module column))
    text))

(defun majutsu-graph-entry-propertize-decoration
    (text compiled entry-id module decoration)
  "Return TEXT tagged as MODULE DECORATION for ENTRY-ID."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-graph-entry-decoration-properties
              compiled entry-id module decoration))
    text))

(defun majutsu-graph-entry-concat-heading-parts (parts)
  "Concatenate heading PARTS without spaces after newlines."
  (let ((out ""))
    (dolist (part parts out)
      (unless (string-empty-p part)
        (let ((need-space
               (and (> (length out) 0)
                    (not (eq (aref out (1- (length out))) ?\n))
                    (not (eq (aref part 0) ?\n)))))
          (setq out (concat out (if need-space " " "") part)))))))

(defun majutsu-graph-entry-single-line-string (value)
  "Return VALUE flattened to a single display line."
  (if (stringp value)
      (string-trim (replace-regexp-in-string "[\n\r]+" " " value nil t))
    value))

(defun majutsu-graph-entry-render-column-text (entry column &optional plain)
  "Return rendered text for ENTRY COLUMN."
  (let* ((module (plist-get column :module))
         (face (plist-get column :face))
         (value (majutsu-graph-entry-display-string
                 (majutsu-graph-entry-column-value entry column))))
    (when (eq module 'tail)
      (setq value (majutsu-graph-entry-single-line-string value)))
    (if plain
        (substring-no-properties value)
      (majutsu-graph-entry-apply-face-policy value face))))

(defun majutsu-graph-entry-render-module-parts
    (entry compiled module &optional annotate plain)
  "Return rendered ENTRY parts for MODULE using COMPILED."
  (let* ((profile (majutsu-graph-entry--profile compiled))
         (entry-id (funcall (plist-get profile :entry-id-function) entry))
         parts)
    (dolist (column (majutsu-graph-entry-module-columns compiled module))
      (let ((value (majutsu-graph-entry-render-column-text entry column plain)))
        (unless (if (eq module 'body)
                    (string-empty-p (string-trim value))
                  (string-empty-p value))
          (push (if annotate
                    (majutsu-graph-entry-propertize-content
                     value compiled entry-id module column)
                  value)
                parts))))
    (nreverse parts)))

(defun majutsu-graph-entry-render-heading-content
    (entry compiled &optional annotate plain)
  "Render ENTRY heading module content without graph prefixes."
  (let ((parts (majutsu-graph-entry-render-module-parts
                entry compiled 'heading annotate plain)))
    (if parts
        (majutsu-graph-entry-concat-heading-parts parts)
      "")))

(defun majutsu-graph-entry-render-heading-content-lines
    (entry compiled &optional annotate plain)
  "Render ENTRY heading module content lines without graph prefixes."
  (majutsu-graph-entry-split-by-separator
   (majutsu-graph-entry-render-heading-content entry compiled annotate plain)
   "\n"))

(defun majutsu-graph-entry-render-heading-lines (entry compiled)
  "Render ENTRY heading module as visible lines with graph prefixes."
  (let* ((content-lines (majutsu-graph-entry-render-heading-content-lines
                         entry compiled))
         (prefixes (or (plist-get entry :heading-prefixes) (list "")))
         (last-prefix (or (car (last prefixes)) ""))
         (count (max (length content-lines) (length prefixes)))
         out)
    (cl-loop for idx below count
             do (let ((prefix (or (nth idx prefixes) last-prefix))
                      (line (or (nth idx content-lines) "")))
                  (push (concat prefix line) out)))
    (nreverse out)))

(defun majutsu-graph-entry-render-tail (entry compiled &optional annotate plain)
  "Render ENTRY tail module as a single-line auxiliary string."
  (let ((parts (majutsu-graph-entry-render-module-parts
                entry compiled 'tail annotate plain)))
    (when parts
      (majutsu-graph-entry-concat-heading-parts parts))))

(defun majutsu-graph-entry-render-body (entry compiled &optional annotate plain)
  "Render ENTRY body module as foldable multiline content."
  (let ((parts (majutsu-graph-entry-render-module-parts
                entry compiled 'body annotate plain)))
    (when parts
      (string-join parts "\n"))))

(defun majutsu-graph-entry--tail-owner-window ()
  "Return the window that currently owns tail layout."
  (let ((selected (selected-window)))
    (if (eq (window-buffer selected) (current-buffer))
        selected
      (get-buffer-window (current-buffer) 0))))

(defun majutsu-graph-entry-tail-align-to-width (width &optional window)
  "Return a display target that right-aligns content of WIDTH."
  (cond
   ((and (display-graphic-p) window)
    (list (max 0 (- (window-body-width window t) width))))
   ((display-graphic-p)
    `(- right (,width)))
   (window
    (max 0 (- (window-body-width window)
              width
              majutsu-graph-entry--tail-terminal-padding)))
   (t
    `(- right ,(+ width majutsu-graph-entry--tail-terminal-padding)))))

(defun majutsu-graph-entry-tail-spacer-display (tail &optional window)
  "Return the spacer display spec used to right-align TAIL."
  (setq window (or window (majutsu-graph-entry--tail-owner-window)))
  `(space :align-to
    ,(majutsu-graph-entry-tail-align-to-width
      (if (display-graphic-p)
          (string-pixel-width tail)
        (string-width tail))
      window)))

(defun majutsu-graph-entry--tail-width-at (pos &optional window)
  "Return displayed width of tail text following spacer at POS."
  (let ((tail (save-excursion
                (goto-char pos)
                (buffer-substring (1+ pos) (line-end-position))))
        (window (or window (majutsu-graph-entry--tail-owner-window))))
    (if (and (display-graphic-p) window)
        (save-excursion
          (goto-char (1+ pos))
          (car (window-text-pixel-size window (point) (line-end-position) t)))
      (if (display-graphic-p)
          (save-excursion
            (goto-char pos)
            (string-pixel-width tail))
        (string-width tail)))))

(defun majutsu-graph-entry-refresh-tail-spacers (&optional beg end window)
  "Recompute right-alignment display specs in BEG..END."
  (setq window (or window (majutsu-graph-entry--tail-owner-window)))
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (save-excursion
        (let ((pos (or beg (point-min)))
              (end (or end (point-max))))
          (while (and (< pos end)
                      (setq pos (text-property-any
                                 pos end 'majutsu-graph-entry-tail-spacer t)))
            (put-text-property
             pos (1+ pos) 'display
             `(space :align-to
               ,(majutsu-graph-entry-tail-align-to-width
                 (majutsu-graph-entry--tail-width-at pos window)
                 window)))
            (setq pos (1+ pos))))))))

(defun majutsu-graph-entry--insert-heading-anchor-line
    (anchor-left tail entry-id prefix compiled)
  "Insert ANCHOR-LEFT and optional TAIL on one prefixed line."
  (let* ((profile (majutsu-graph-entry--profile compiled))
         (tail-align (plist-get profile :tail-align))
         (start (point))
         (spacer-pos nil)
         (window (majutsu-graph-entry--tail-owner-window)))
    (insert anchor-left)
    (when (and (stringp tail) (not (string-empty-p tail)))
      (setq spacer-pos (point))
      (insert " ")
      (add-text-properties
       spacer-pos (point)
       (majutsu-graph-entry-tail-spacer-properties
        compiled entry-id
        (and tail-align
             (majutsu-graph-entry-tail-spacer-display tail window))))
      (insert tail))
    (insert "\n")
    (majutsu-graph-entry-apply-line-prefix-span start (point) prefix)
    (when (and spacer-pos tail-align)
      (majutsu-graph-entry-refresh-tail-spacers spacer-pos (1+ spacer-pos)
                                                window))))

(defun majutsu-graph-entry-insert-entry (entry compiled)
  "Insert parsed ENTRY as a Magit section using COMPILED."
  (let* ((profile (majutsu-graph-entry--profile compiled))
         (entry-id-fn (plist-get profile :entry-id-function))
         (section-value-fn (or (plist-get profile :section-value-function)
                               entry-id-fn))
         (id (funcall entry-id-fn entry))
         (section-class (or (plist-get profile :section-class) 'magit-section))
         (section-value (funcall section-value-fn entry))
         (section-hide (plist-get profile :section-hide))
         (indent (or (plist-get entry :indent) 0))
         (prefixes (or (plist-get entry :heading-prefixes) (list "")))
         (last-prefix (or (car (last prefixes)) ""))
         (content-lines (majutsu-graph-entry-render-heading-content-lines
                         entry compiled t))
         (count (max (length content-lines) (length prefixes)))
         (heading-lines nil)
         (tail (majutsu-graph-entry-render-tail entry compiled t))
         (suffix-lines (plist-get entry :suffix-lines))
         (body (majutsu-graph-entry-render-body entry compiled t))
         (has-body (and (stringp body)
                        (not (string-empty-p (string-trim body))))))
    (cl-loop for idx below count
             do (let ((prefix (majutsu-graph-entry-propertize-decoration
                               (or (nth idx prefixes) last-prefix)
                               compiled id 'heading
                               (if (= idx 0) 'graph-prefix 'graph-carry)))
                      (line (or (nth idx content-lines) "")))
                  (push (cons prefix line) heading-lines)))
    (setq heading-lines (nreverse heading-lines))
    (magit-insert-section ((eval section-class) section-value section-hide)
      (when heading-lines
        (majutsu-graph-entry--insert-heading-anchor-line
         (cdar heading-lines) tail id (caar heading-lines) compiled))
      (dolist (line (cdr heading-lines))
        (majutsu-graph-entry-insert-prefixed-line (cdr line) (car line)))
      (dolist (suffix-line suffix-lines)
        (pcase-let* ((`(,prefix . ,content)
                      (majutsu-graph-entry-split-prefix-line suffix-line indent))
                     (decorated-prefix
                      (majutsu-graph-entry-propertize-decoration
                       prefix compiled id 'heading 'graph-carry))
                     (decorated-content
                      (majutsu-graph-entry-propertize-content
                       content compiled id 'heading)))
          (majutsu-graph-entry-insert-prefixed-line
           decorated-content decorated-prefix)))
      (when has-body
        (magit-insert-heading)
        (let ((body-prefix (majutsu-graph-entry-propertize-decoration
                            (make-string indent ?\s)
                            compiled id 'body 'body-prefix)))
          (magit-insert-section-body
            (let ((start (point)))
              (insert body)
              (insert "\n")
              (majutsu-graph-entry-apply-line-prefix-span
               start (point) body-prefix))))))))

(defun majutsu-graph-entry-wash-entry (compiled)
  "Wash the entry at point using COMPILED."
  (when-let* ((entry (save-excursion
                       (majutsu-graph-entry-parse-at-point compiled))))
    (let ((beg (plist-get entry :beg))
          (end (plist-get entry :end)))
      (delete-region beg end)
      (goto-char beg)
      (majutsu-graph-entry-insert-entry entry compiled)
      entry)))

(defun majutsu-graph-entry-wash-buffer (compiled)
  "Wash all graph entries in the current narrowed buffer."
  (let (entries)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((entry (majutsu-graph-entry-wash-entry compiled)))
          (push entry entries)
        (magit-delete-line)))
    (nreverse entries)))

(defun majutsu-graph-entry-string-has-module-p (string module)
  "Return non-nil if STRING contains text marked with MODULE."
  (let ((pos 0)
        found)
    (while (and (< pos (length string)) (not found))
      (setq found (eq (get-text-property
                       pos 'majutsu-graph-entry-module string)
                      module)
            pos (or (next-single-property-change
                     pos 'majutsu-graph-entry-module string)
                    (length string))))
    found))

(defun majutsu-graph-entry-string-remove-module (string module)
  "Return STRING with text marked as MODULE removed."
  (let ((pos 0)
        parts)
    (while (< pos (length string))
      (let* ((next (or (next-single-property-change
                        pos 'majutsu-graph-entry-module string)
                       (length string)))
             (current (get-text-property
                       pos 'majutsu-graph-entry-module string)))
        (unless (eq current module)
          (push (substring string pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun majutsu-graph-entry-cleanup-copied-string (string &optional compiled)
  "Strip graph-entry UI properties from copied STRING."
  (when (stringp string)
    (let ((props '(majutsu-graph-entry-profile
                   majutsu-graph-entry-module
                   majutsu-graph-entry-field
                   majutsu-graph-entry-column
                   majutsu-graph-entry-id
                   majutsu-graph-entry-decoration
                   majutsu-graph-entry-tail-spacer
                   line-prefix
                   wrap-prefix
                   display)))
      (when compiled
        (dolist (prefix (majutsu-graph-entry--compat-prefixes compiled))
          (dolist (suffix '(module field column entry-id decoration tail-spacer))
            (push (majutsu-graph-entry--property-symbol prefix suffix) props))))
      (remove-list-of-text-properties 0 (length string) props string)))
  string)

(defun majutsu-graph-entry-filter-buffer-substring
    (beg end &optional delete compiled)
  "Filter copied graph-entry text between BEG and END."
  (let ((string (buffer-substring--filter beg end delete))
        (trim-tail nil))
    (when (and (stringp string)
               (majutsu-graph-entry-string-has-module-p string 'heading)
               (majutsu-graph-entry-string-has-module-p string 'tail))
      (setq string (majutsu-graph-entry-string-remove-module string 'tail))
      (setq trim-tail t))
    (setq string (majutsu-graph-entry-cleanup-copied-string string compiled))
    (when (and trim-tail (stringp string))
      (setq string (replace-regexp-in-string "[ \t]+$" "" string)))
    string))

(provide 'majutsu-graph-entry)

;;; majutsu-graph-entry.el ends here
