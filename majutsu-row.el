;;; majutsu-row.el --- Structured row rendering for Majutsu  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared row protocol for Majutsu log, evolog, and op-log views.
;; jj renders the graph; Majutsu embeds module tokens inside graph nodes,
;; parses the output back into entries, renders real Magit sections, and
;; provides semantic copy commands for structured entry fields.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit-section)
(require 'majutsu-template)
(require 'majutsu-section)

;;; Transport constants

(defconst majutsu-row-field-separator "\x1e"
  "Separator inserted between fields inside one module payload.")

(defconst majutsu-row-field-line-separator "\x1f"
  "Encoded newline separator used inside template field payloads.")

(defconst majutsu-row-record-marker "\x1d"
  "Control marker prefix for module boundaries inside graph output.")

(defconst majutsu-row-start-token
  (concat majutsu-row-record-marker "S")
  "Marker that starts an entry and heading payload.")

(defconst majutsu-row-tail-token
  (concat majutsu-row-record-marker "T")
  "Marker that starts the tail payload.")

(defconst majutsu-row-body-token
  (concat majutsu-row-record-marker "B")
  "Marker that starts the body payload.")

(defconst majutsu-row-meta-token
  (concat majutsu-row-record-marker "M")
  "Marker that starts the metadata payload.")

(defconst majutsu-row-end-token
  (concat majutsu-row-record-marker "E")
  "Marker that terminates an entry.")

(defconst majutsu-row-module-order '(heading tail body metadata)
  "Module parse/render order for sequential row payloads.")

(defconst majutsu-row--tail-terminal-padding 1
  "Terminal columns reserved to the right of tail-aligned text.")

;;; UI text-property names (stripped during copy)

(defconst majutsu-row--ui-properties
  '(majutsu-row-module
    majutsu-row-field
    majutsu-row-column
    majutsu-row-entry-id
    majutsu-row-decoration
    majutsu-row-profile
    majutsu-row-tail-spacer
    line-prefix
    wrap-prefix
    display)
  "Text properties removed from copied row text.")

;;; Buffer-local variables

(defvar-local majutsu-row-buffer-compiled nil
  "Compiled row metadata for the current buffer.")

(defvar-local majutsu-row-cached-entries nil
  "Parsed row entries for the current buffer.")

(defvar-local majutsu-row-entry-index nil
  "Hash table mapping stable entry ids to cached entries.")

(defvar-local majutsu-row-section-value-index nil
  "Hash table mapping section values to cached entries.")

;;; Token helpers

(defun majutsu-row-default-tokens ()
  "Return the default row token plist."
  (list :start majutsu-row-start-token
        :tail majutsu-row-tail-token
        :body majutsu-row-body-token
        :metadata majutsu-row-meta-token
        :end majutsu-row-end-token))

(defun majutsu-row--tokens (profile)
  "Return token plist for PROFILE."
  (or (plist-get profile :tokens)
      (majutsu-row-default-tokens)))

(defun majutsu-row--profile (compiled)
  "Return row profile from COMPILED."
  (plist-get compiled :profile))

(defun majutsu-row--token (compiled key)
  "Return token KEY from COMPILED."
  (plist-get (plist-get compiled :tokens) key))

;;; Current compiled access

(defun majutsu-row-current-compiled (&optional compiled)
  "Return COMPILED or the current buffer's compiled row metadata."
  (or compiled
      majutsu-row-buffer-compiled
      (user-error "No compiled row data in current buffer")))

;;; Post-decode

(defun majutsu-row-post-decode-line-separator (value &optional _ctx)
  "Decode field line separators inside VALUE."
  (if (stringp value)
      (subst-char-in-string
       (aref majutsu-row-field-line-separator 0)
       ?\n
       value t)
    value))

;;; Default module/field helpers

(defun majutsu-row--default-module-for-field (profile field)
  "Return default module for FIELD using PROFILE."
  (or (alist-get field (plist-get profile :default-modules) nil nil #'eq)
      (user-error "Field %S requires explicit :module" field)))

(defun majutsu-row--default-postprocessors-for-field (profile field)
  "Return default postprocessors for FIELD using PROFILE."
  (append (plist-get profile :default-postprocessors)
          (alist-get field (plist-get profile :field-postprocessors) nil nil #'eq)))

;;; Column normalization

(defun majutsu-row-normalize-postprocessors (profile post field)
  "Normalize POST for FIELD using PROFILE into a function list."
  (let* ((defaults (majutsu-row--default-postprocessors-for-field
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

(defun majutsu-row-normalize-column-spec (profile spec)
  "Normalize one column SPEC using PROFILE."
  (let* ((col (cond
               ((and (plistp spec) (plist-get spec :field)) spec)
               ((symbolp spec) (list :field spec))
               (t (user-error "Invalid column spec: %S" spec))))
         (field (plist-get col :field))
         (module (if (plist-member col :module)
                     (plist-get col :module)
                   (majutsu-row--default-module-for-field
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
    (unless (memq module majutsu-row-module-order)
      (user-error "Column %S has invalid :module %S" field module))
    (unless (or (eq face t) (null face) (symbolp face))
      (user-error "Column %S has invalid :face %S" field face))
    (append (list :field field
                  :module module
                  :face face
                  :post (majutsu-row-normalize-postprocessors
                         profile post field))
            (when (plist-member col :template)
              (list :template template)))))

(defun majutsu-row-ensure-required-columns (profile columns)
  "Ensure required PROFILE columns are present in COLUMNS."
  (let ((present (mapcar (lambda (column) (plist-get column :field)) columns)))
    (dolist (req (plist-get profile :required-fields))
      (let ((field (if (and (plistp req) (plist-get req :field))
                       (plist-get req :field)
                     req)))
        (unless (memq field present)
          (setq columns
                (append columns
                        (list (majutsu-row-normalize-column-spec
                               profile req)))))))
    columns))

;;; Module columns (from compiled)

(defun majutsu-row-module-columns (compiled module)
  "Return compiled column specs for MODULE from COMPILED."
  (alist-get module (plist-get compiled :module-columns) nil nil #'eq))

;;; Layout columns (from layout plist)

(defun majutsu-row-layout-columns (layout)
  "Return column specs from LAYOUT."
  (plist-get layout :columns))

(defun majutsu-row-column-by-instance (layout instance)
  "Return column spec from LAYOUT identified by INSTANCE."
  (seq-find (lambda (column)
              (eql (plist-get column :instance) instance))
            (majutsu-row-layout-columns layout)))

(defun majutsu-row-assign-column-instances (columns)
  "Return COLUMNS with stable per-instance ids assigned."
  (cl-loop for column in columns
           for idx from 0
           collect (plist-put (copy-sequence column) :instance idx)))

;;; Compile

(defun majutsu-row--column-template (profile column)
  "Return template form for COLUMN using PROFILE."
  (or (plist-get column :template)
      (let ((fn (plist-get profile :template-function))
            (field (plist-get column :field)))
        (if fn
            (funcall fn field)
          (user-error "Column %S has no :template" field)))))

(defun majutsu-row--copy-layout (profile columns)
  "Return copy layout for PROFILE COLUMNS."
  (list :name (plist-get profile :name)
        :columns columns
        :entry-id-function (plist-get profile :entry-id-function)
        :section-value-function (plist-get profile :section-value-function)
        :section-class (plist-get profile :section-class)))

(defun majutsu-row-build-module-template-form (templates)
  "Return a template form joining TEMPLATES with field separators."
  (cond
   ((null templates) "")
   ((null (cdr templates)) (car templates))
   (t
    (let ((forms nil)
          (first t))
      (dolist (template templates)
        (unless first
          (setq forms (append forms (list majutsu-row-field-separator))))
        (setq first nil)
        (setq forms (append forms (list template))))
      (cons :concat forms)))))

(defun majutsu-row-compile (profile &optional columns)
  "Compile PROFILE COLUMNS into a jj template and layout metadata."
  (let* ((source-columns
          (or columns
              (when-let* ((var (plist-get profile :columns-var)))
                (symbol-value var))))
         (normalized (mapcar (lambda (spec)
                               (majutsu-row-normalize-column-spec
                                profile spec))
                             source-columns))
         (complete (majutsu-row-assign-column-instances
                    (majutsu-row-ensure-required-columns
                     profile normalized)))
         (module-columns
          (mapcar (lambda (module)
                    (cons module
                          (seq-filter (lambda (column)
                                        (eq (plist-get column :module) module))
                                      complete)))
                  majutsu-row-module-order))
         (tokens (majutsu-row--tokens profile))
         (module-form
          (lambda (module)
            (majutsu-row-build-module-template-form
             (mapcar (lambda (column)
                       (majutsu-row--column-template profile column))
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
          :tokens tokens
          :copy-layout (majutsu-row--copy-layout profile complete))))

;;; String splitting/joining

(defun majutsu-row-split-by-separator (value separator)
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

(defun majutsu-row-join-lines (lines)
  "Join LINES with literal newlines, preserving string properties."
  (if (null lines)
      ""
    (let ((out (car lines)))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" line))))))

;;; Parse helpers

(defun majutsu-row-line-token-position (token bol eol &optional start)
  "Return start position of TOKEN between BOL and EOL, or nil."
  (save-excursion
    (goto-char (or start bol))
    (when (search-forward token eol t)
      (- (point) (length token)))))

(defun majutsu-row-parse-trailing-payloads (compiled payload)
  "Parse tail, body, and metadata segments from PAYLOAD using COMPILED."
  (let ((tail-token (majutsu-row--token compiled :tail))
        (body-token (majutsu-row--token compiled :body))
        (meta-token (majutsu-row--token compiled :metadata))
        (end-token (majutsu-row--token compiled :end)))
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

(defun majutsu-row-split-module-values (payload count)
  "Split PAYLOAD into COUNT field values."
  (if (<= count 0)
      nil
    (let ((values (majutsu-row-split-by-separator
                   (or payload "")
                   majutsu-row-field-separator)))
      (cond
       ((< (length values) count)
        (append values (make-list (- count (length values)) "")))
       ((> (length values) count)
        (seq-take values count))
       (t values)))))

;;; Record helpers

(defun majutsu-row--decode-transport-value (profile value)
  "Decode transport-level escapes in VALUE using PROFILE."
  (let ((fn (or (plist-get profile :decode-function)
                #'majutsu-row-post-decode-line-separator)))
    (funcall fn value)))

(defun majutsu-row-apply-postprocessor (fn value ctx)
  "Apply postprocessor FN to VALUE with CTX."
  (condition-case err
      (condition-case _
          (funcall fn value ctx)
        (wrong-number-of-arguments
         (funcall fn value)))
    (error
     (message "majutsu row postprocessor failed (%S on %S): %s"
              fn (plist-get ctx :field) (error-message-string err))
     value)))

(defun majutsu-row-apply-postprocessors (value postprocessors ctx)
  "Apply POSTPROCESSORS to VALUE with CTX sequentially."
  (let ((out value))
    (dolist (fn postprocessors out)
      (setq out (majutsu-row-apply-postprocessor fn out ctx)))))

(defun majutsu-row--canonical-field-value (profile field value)
  "Return canonical FIELD VALUE using PROFILE postprocessors."
  (majutsu-row-apply-postprocessors
   value
   (majutsu-row--default-postprocessors-for-field profile field)
   (list :field field :module 'canonical :raw-value value :canonical t)))

(defun majutsu-row-record-canonical-field (entry field value)
  "Record canonical FIELD VALUE onto ENTRY."
  (let ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (plist-put entry :columns columns)))

(defun majutsu-row--record-field (profile entry field value)
  "Record FIELD VALUE on ENTRY using PROFILE."
  (if-let* ((fn (plist-get profile :record-field-function)))
      (funcall fn entry field value)
    (majutsu-row-record-canonical-field entry field value)))

(defun majutsu-row-record-column-value (entry column value)
  "Record per-instance VALUE for COLUMN onto ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values)))
    (when instance
      (setf (alist-get instance column-values nil nil #'eql) value)
      (setq entry (plist-put entry :column-values column-values)))
    entry))

(defun majutsu-row-record-module-fields (entry module payload compiled)
  "Record MODULE PAYLOAD values into ENTRY using COMPILED layout."
  (let* ((profile (majutsu-row--profile compiled))
         (columns (majutsu-row-module-columns compiled module))
         (values (majutsu-row-split-module-values payload (length columns)))
         (stored nil))
    (cl-loop for column in columns
             for raw-value in values
             do (let* ((field (plist-get column :field))
                       (decoded (majutsu-row--decode-transport-value
                                 profile raw-value))
                       (canonical (majutsu-row--canonical-field-value
                                   profile field decoded)))
                  (setq entry (majutsu-row--record-field
                               profile entry field canonical))
                  (let* ((ctx (list :field field
                                    :module module
                                    :column column
                                    :entry entry
                                    :raw-value decoded
                                    :canonical-value canonical))
                         (out (majutsu-row-apply-postprocessors
                               decoded (plist-get column :post) ctx)))
                    (setq entry (majutsu-row-record-column-value
                                 entry column out))
                    (push out stored))))
    (let ((modules (plist-get entry :modules)))
      (setf (alist-get module modules nil nil #'eq) (nreverse stored))
      (setq entry (plist-put entry :modules modules)))
    entry))

;;; Parse

(defun majutsu-row-parse-at-point (compiled)
  "Parse one sequentially encoded row entry at point using COMPILED."
  (let* ((entry-beg (line-beginning-position))
         (start-token (majutsu-row--token compiled :start))
         (tail-token (majutsu-row--token compiled :tail))
         (bol entry-beg)
         (eol (line-end-position))
         (start-pos (majutsu-row-line-token-position
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
                 (segment-pos (majutsu-row-line-token-position
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
          (when-let* ((payloads (majutsu-row-parse-trailing-payloads
                                 compiled trailing-payload)))
            (let* ((profile (majutsu-row--profile compiled))
                   (entry (list :beg entry-beg
                                :row-profile (plist-get profile :name)
                                :indent indent
                                :columns nil
                                :column-values nil
                                :modules nil
                                :heading-prefixes (nreverse heading-prefixes)))
                   (heading-payload
                    (majutsu-row-join-lines
                     (nreverse heading-segments))))
              (setq entry (majutsu-row-record-module-fields
                           entry 'heading heading-payload compiled))
              (setq entry (majutsu-row-record-module-fields
                           entry 'tail (plist-get payloads :tail) compiled))
              (setq entry (majutsu-row-record-module-fields
                           entry 'body (plist-get payloads :body) compiled))
              (setq entry (majutsu-row-record-module-fields
                           entry 'metadata (plist-get payloads :metadata) compiled))
              (let ((suffix-lines nil))
                (while (and (not (eobp))
                            (let ((next-bol (line-beginning-position))
                                  (next-eol (line-end-position)))
                              (not (majutsu-row-line-token-position
                                    start-token next-bol next-eol))))
                  (push (buffer-substring (line-beginning-position)
                                          (line-end-position))
                        suffix-lines)
                  (forward-line 1))
                (setq entry (plist-put entry :suffix-lines
                                       (nreverse suffix-lines)))
                (setq entry (plist-put entry :end (point))))
              entry)))))))

(defun majutsu-row-parse-buffer (compiled)
  "Parse all sequentially encoded row entries in current buffer."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((entry (majutsu-row-parse-at-point compiled)))
          (if entry
              (push entry entries)
            (forward-line 1)))))
    (nreverse entries)))

;;; Line prefix/insertion helpers

(defun majutsu-row-apply-line-prefix-span
    (start end line-prefix-str &optional wrap-prefix-str)
  "Apply display-only prefix strings to START..END."
  (when (< start end)
    (add-text-properties
     start end
     (list 'line-prefix (or line-prefix-str "")
           'wrap-prefix (or wrap-prefix-str line-prefix-str "")))))

(defun majutsu-row-insert-prefixed-line (content prefix)
  "Insert CONTENT as one line with display-only PREFIX."
  (let ((start (point)))
    (insert (or content "") "\n")
    (majutsu-row-apply-line-prefix-span start (point) prefix)))

(defun majutsu-row-split-prefix-line (line prefix-width)
  "Split LINE into (PREFIX . CONTENT) using PREFIX-WIDTH characters."
  (let* ((width (max 0 (min (or prefix-width 0) (length (or line "")))))
         (text (or line "")))
    (cons (substring text 0 width)
          (substring text width))))

;;; Column access

(defun majutsu-row-column (entry field)
  "Return canonical value for FIELD stored on ENTRY."
  (alist-get field (plist-get entry :columns) nil nil #'eq))

(defun majutsu-row-column-value (entry column)
  "Return per-instance value for COLUMN stored on ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values))
         (missing (make-symbol "majutsu-row-missing-instance"))
         (value (if instance
                    (alist-get instance column-values missing nil #'eql)
                  missing)))
    (if (eq value missing)
        (majutsu-row-column entry (plist-get column :field))
      value)))

;;; Display helpers

(defun majutsu-row-display-string (value)
  "Return VALUE converted to a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value) (mapconcat #'majutsu-row-display-string value " "))
   (t (format "%s" value))))

(defun majutsu-row-single-line-string (value)
  "Return VALUE flattened to a single display line."
  (if (stringp value)
      (string-trim (replace-regexp-in-string "[\n\r]+" " " value nil t))
    value))

(defun majutsu-row-concat-heading-parts (parts)
  "Concatenate heading PARTS without spaces after newlines."
  (let ((out ""))
    (dolist (part parts out)
      (unless (string-empty-p part)
        (let ((need-space
               (and (> (length out) 0)
                    (not (eq (aref out (1- (length out))) ?\n))
                    (not (eq (aref part 0) ?\n)))))
          (setq out (concat out (if need-space " " "") part)))))))

;;; Face helpers

(defun majutsu-row-apply-face-policy (value face)
  "Apply FACE policy to VALUE and return display string."
  (let ((text (or value "")))
    (cond
     ((eq face t) text)
     ((null face) (substring-no-properties text))
     (t (propertize (substring-no-properties text) 'font-lock-face face)))))

;;; Compat property helpers

(defun majutsu-row--compat-prefixes (compiled)
  "Return compatibility property prefixes for COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (prefixes (or (plist-get profile :compat-property-prefixes)
                       (plist-get profile :compat-property-prefix))))
    (ensure-list prefixes)))

(defun majutsu-row--property-symbol (prefix suffix)
  "Return property symbol PREFIX-SUFFIX."
  (intern (format "%s-%s" prefix suffix)))

(defun majutsu-row--append-compat-properties (props compiled values)
  "Append compatibility property VALUES to PROPS for COMPILED."
  (dolist (prefix (majutsu-row--compat-prefixes compiled) props)
    (while values
      (let ((suffix (pop values))
            (value (pop values)))
        (setq props
              (append props
                      (list (majutsu-row--property-symbol prefix suffix)
                            value)))))))

;;; Content/deco properties

(defun majutsu-row-content-properties
    (compiled entry-id module &optional column)
  "Return content properties for ENTRY-ID in MODULE."
  (let ((props `(majutsu-row-profile
                 ,(plist-get (majutsu-row--profile compiled) :name)
                 majutsu-row-module ,module
                 majutsu-row-entry-id ,entry-id)))
    (when column
      (setq props (append props
                          `(majutsu-row-field ,(plist-get column :field)
                            majutsu-row-column ,(plist-get column :instance)))))
    (majutsu-row--append-compat-properties
     props compiled
     (append (list 'module module 'entry-id entry-id)
             (when column
               (list 'field (plist-get column :field)
                     'column (plist-get column :instance)))))))

(defun majutsu-row-decoration-properties
    (compiled entry-id module decoration)
  "Return decoration properties for ENTRY-ID in MODULE."
  (majutsu-row--append-compat-properties
   (append
    `(majutsu-row-profile
      ,(plist-get (majutsu-row--profile compiled) :name)
      majutsu-row-module ,module
      majutsu-row-entry-id ,entry-id
      majutsu-row-decoration ,decoration))
   compiled
   (list 'module module 'entry-id entry-id 'decoration decoration)))

(defun majutsu-row-tail-spacer-properties (compiled entry-id display)
  "Return tail spacer properties for ENTRY-ID and DISPLAY."
  (majutsu-row--append-compat-properties
   (append
    `(majutsu-row-module tail
      majutsu-row-entry-id ,entry-id
      majutsu-row-decoration tail-spacer
      majutsu-row-tail-spacer t
      display ,display))
   compiled
   (list 'module 'tail
         'entry-id entry-id
         'decoration 'tail-spacer
         'tail-spacer t)))

;;; Propertize helpers

(defun majutsu-row-propertize-content
    (text compiled entry-id module &optional column)
  "Return TEXT tagged as MODULE content for ENTRY-ID and COLUMN."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-row-content-properties
              compiled entry-id module column))
    text))

(defun majutsu-row-propertize-decoration
    (text compiled entry-id module decoration)
  "Return TEXT tagged as MODULE DECORATION for ENTRY-ID."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-row-decoration-properties
              compiled entry-id module decoration))
    text))

;;; Column text rendering

(defun majutsu-row-render-column-text (entry column &optional plain)
  "Return rendered text for ENTRY COLUMN.
When PLAIN is non-nil, omit faces and text properties."
  (let* ((module (plist-get column :module))
         (face (plist-get column :face))
         (value (majutsu-row-display-string
                 (majutsu-row-column-value entry column))))
    (when (eq module 'tail)
      (setq value (majutsu-row-single-line-string value)))
    (if plain
        (substring-no-properties value)
      (majutsu-row-apply-face-policy value face))))

;;; Module rendering

(defun majutsu-row-render-module-parts
    (entry compiled module &optional annotate plain)
  "Return rendered ENTRY parts for MODULE using COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (entry-id (funcall (plist-get profile :entry-id-function) entry))
         parts)
    (dolist (column (majutsu-row-module-columns compiled module))
      (let ((value (majutsu-row-render-column-text entry column plain)))
        (unless (if (eq module 'body)
                    (string-empty-p (string-trim value))
                  (string-empty-p value))
          (push (if annotate
                    (majutsu-row-propertize-content
                     value compiled entry-id module column)
                  value)
                parts))))
    (nreverse parts)))

(defun majutsu-row-render-heading-content
    (entry compiled &optional annotate plain)
  "Render ENTRY heading module content without graph prefixes."
  (let ((parts (majutsu-row-render-module-parts
                entry compiled 'heading annotate plain)))
    (if parts
        (majutsu-row-concat-heading-parts parts)
      "")))

(defun majutsu-row-render-heading-content-lines
    (entry compiled &optional annotate plain)
  "Render ENTRY heading module content lines without graph prefixes."
  (majutsu-row-split-by-separator
   (majutsu-row-render-heading-content entry compiled annotate plain)
   "\n"))

(defun majutsu-row-render-heading-lines (entry compiled)
  "Render ENTRY heading module as visible lines with graph prefixes."
  (let* ((content-lines (majutsu-row-render-heading-content-lines
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

(defun majutsu-row-render-tail (entry compiled &optional annotate plain)
  "Render ENTRY tail module as a single-line auxiliary string."
  (let ((parts (majutsu-row-render-module-parts
                entry compiled 'tail annotate plain)))
    (when parts
      (majutsu-row-concat-heading-parts parts))))

(defun majutsu-row-render-body (entry compiled &optional annotate plain)
  "Render ENTRY body module as foldable multiline content."
  (let ((parts (majutsu-row-render-module-parts
                entry compiled 'body annotate plain)))
    (when parts
      (string-join parts "\n"))))

;;; Tail alignment

(defun majutsu-row--tail-owner-window ()
  "Return the window that currently owns tail layout."
  (let ((selected (selected-window)))
    (if (eq (window-buffer selected) (current-buffer))
        selected
      (get-buffer-window (current-buffer) 0))))

(defun majutsu-row-tail-align-to-width (width &optional window)
  "Return a display target that right-aligns content of WIDTH."
  (cond
   ((and (display-graphic-p) window)
    (list (max 0 (- (window-body-width window t) width))))
   ((display-graphic-p)
    `(- right (,width)))
   (window
    (max 0 (- (window-body-width window)
              width
              majutsu-row--tail-terminal-padding)))
   (t
    `(- right ,(+ width majutsu-row--tail-terminal-padding)))))

(defun majutsu-row-tail-spacer-display (tail &optional window)
  "Return the spacer display spec used to right-align TAIL."
  (setq window (or window (majutsu-row--tail-owner-window)))
  `(space :align-to
    ,(majutsu-row-tail-align-to-width
      (if (display-graphic-p)
          (string-pixel-width tail)
        (string-width tail))
      window)))

(defun majutsu-row--tail-width-at (pos &optional window)
  "Return displayed width of tail text following spacer at POS."
  (let ((tail (save-excursion
                (goto-char pos)
                (buffer-substring (1+ pos) (line-end-position))))
        (window (or window (majutsu-row--tail-owner-window))))
    (if (and (display-graphic-p) window)
        (save-excursion
          (goto-char (1+ pos))
          (car (window-text-pixel-size window (point) (line-end-position) t)))
      (if (display-graphic-p)
          (save-excursion
            (goto-char pos)
            (string-pixel-width tail))
        (string-width tail)))))

(defun majutsu-row-refresh-tail-spacers (&optional beg end window)
  "Recompute right-alignment display specs in BEG..END."
  (setq window (or window (majutsu-row--tail-owner-window)))
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (save-excursion
        (let ((pos (or beg (point-min)))
              (end (or end (point-max))))
          (while (and (< pos end)
                      (setq pos (text-property-any
                                 pos end 'majutsu-row-tail-spacer t)))
            (put-text-property
             pos (1+ pos) 'display
             `(space :align-to
               ,(majutsu-row-tail-align-to-width
                 (majutsu-row--tail-width-at pos window)
                 window)))
            (setq pos (1+ pos))))))))

;;; Entry insertion

(defun majutsu-row--insert-heading-anchor-line
    (anchor-left tail entry-id prefix compiled)
  "Insert ANCHOR-LEFT and optional TAIL on one prefixed line."
  (let* ((profile (majutsu-row--profile compiled))
         (tail-align (plist-get profile :tail-align))
         (start (point))
         (spacer-pos nil)
         (window (majutsu-row--tail-owner-window)))
    (insert anchor-left)
    (when (and (stringp tail) (not (string-empty-p tail)))
      (setq spacer-pos (point))
      (insert " ")
      (add-text-properties
       spacer-pos (point)
       (majutsu-row-tail-spacer-properties
        compiled entry-id
        (and tail-align
             (majutsu-row-tail-spacer-display tail window))))
      (insert tail))
    (insert "\n")
    (majutsu-row-apply-line-prefix-span start (point) prefix)
    (when (and spacer-pos tail-align)
      (majutsu-row-refresh-tail-spacers spacer-pos (1+ spacer-pos)
                                        window))))

(defun majutsu-row-insert-entry (entry compiled)
  "Insert parsed ENTRY as a Magit section using COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
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
         (content-lines (majutsu-row-render-heading-content-lines
                         entry compiled t))
         (count (max (length content-lines) (length prefixes)))
         (heading-lines nil)
         (tail (majutsu-row-render-tail entry compiled t))
         (suffix-lines (plist-get entry :suffix-lines))
         (body (majutsu-row-render-body entry compiled t))
         (has-body (and (stringp body)
                        (not (string-empty-p (string-trim body))))))
    (cl-loop for idx below count
             do (let ((prefix (majutsu-row-propertize-decoration
                               (or (nth idx prefixes) last-prefix)
                               compiled id 'heading
                               (if (= idx 0) 'graph-prefix 'graph-carry)))
                      (line (or (nth idx content-lines) "")))
                  (push (cons prefix line) heading-lines)))
    (setq heading-lines (nreverse heading-lines))
    (magit-insert-section ((eval section-class) section-value section-hide)
      (when heading-lines
        (majutsu-row--insert-heading-anchor-line
         (cdar heading-lines) tail id (caar heading-lines) compiled))
      (dolist (line (cdr heading-lines))
        (majutsu-row-insert-prefixed-line (cdr line) (car line)))
      (dolist (suffix-line suffix-lines)
        (pcase-let* ((`(,prefix . ,content)
                      (majutsu-row-split-prefix-line suffix-line indent))
                     (decorated-prefix
                      (majutsu-row-propertize-decoration
                       prefix compiled id 'heading 'graph-carry))
                     (decorated-content
                      (majutsu-row-propertize-content
                       content compiled id 'heading)))
          (majutsu-row-insert-prefixed-line
           decorated-content decorated-prefix)))
      (when has-body
        (magit-insert-heading)
        (let ((body-prefix (majutsu-row-propertize-decoration
                            (make-string indent ?\s)
                            compiled id 'body 'body-prefix)))
          (magit-insert-section-body
            (let ((start (point)))
              (insert body)
              (insert "\n")
              (majutsu-row-apply-line-prefix-span
               start (point) body-prefix))))))))

;;; Wash

(defun majutsu-row-wash-entry (compiled)
  "Wash the entry at point using COMPILED."
  (when-let* ((entry (save-excursion
                       (majutsu-row-parse-at-point compiled))))
    (let ((beg (plist-get entry :beg))
          (end (plist-get entry :end)))
      (delete-region beg end)
      (goto-char beg)
      (majutsu-row-insert-entry entry compiled)
      entry)))

(defun majutsu-row-wash-buffer (compiled)
  "Wash all row entries in the current narrowed buffer."
  (let (entries)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((entry (majutsu-row-wash-entry compiled)))
          (push entry entries)
        (magit-delete-line)))
    (nreverse entries)))

;;; Copy property listing

(defun majutsu-row--extra-copy-properties (&optional compiled)
  "Return extra copy cleanup properties for COMPILED (beyond base UI props)."
  (let ((props nil))
    (when compiled
      (dolist (prefix (majutsu-row--compat-prefixes compiled))
        (dolist (suffix '(module field column entry-id decoration tail-spacer))
          (push (majutsu-row--property-symbol prefix suffix) props))))
    props))

;;; Copy string cleanup

(defun majutsu-row-cleanup-copied-string (string &optional extra-properties)
  "Strip row and EXTRA-PROPERTIES from copied STRING."
  (when (stringp string)
    (remove-list-of-text-properties
     0 (length string)
     (append majutsu-row--ui-properties extra-properties)
     string))
  string)

(defun majutsu-row-string-has-module-p (string module)
  "Return non-nil if STRING contains text marked with MODULE."
  (let ((pos 0)
        found)
    (while (and (< pos (length string)) (not found))
      (setq found (eq (get-text-property
                       pos 'majutsu-row-module string)
                      module)
            pos (or (next-single-property-change
                     pos 'majutsu-row-module string)
                    (length string))))
    found))

(defun majutsu-row-string-remove-module (string module)
  "Return STRING with text marked as MODULE removed."
  (let ((pos 0)
        parts)
    (while (< pos (length string))
      (let* ((next (or (next-single-property-change
                        pos 'majutsu-row-module string)
                       (length string)))
             (current (get-text-property
                       pos 'majutsu-row-module string)))
        (unless (eq current module)
          (push (substring string pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun majutsu-row-filter-buffer-substring
    (beg end &optional delete extra-properties)
  "Filter copied row text between BEG and END.
Drops tail text when both heading and tail are present in the copied region."
  (let ((string (buffer-substring--filter beg end delete))
        (trim-tail nil))
    (when (and (stringp string)
               (majutsu-row-string-has-module-p string 'heading)
               (majutsu-row-string-has-module-p string 'tail))
      (setq string (majutsu-row-string-remove-module string 'tail))
      (setq trim-tail t))
    (setq string (majutsu-row-cleanup-copied-string
                  string extra-properties))
    (when (and trim-tail (stringp string))
      (setq string (replace-regexp-in-string "[ \t]+$" "" string)))
    string))

;;; Copy index building

(defun majutsu-row-entry-id (entry layout)
  "Return stable entry id for ENTRY using LAYOUT."
  (if-let* ((fn (plist-get layout :entry-id-function)))
      (funcall fn entry)
    (majutsu-row-column entry 'id)))

(defun majutsu-row-section-value (entry layout)
  "Return Magit section value for ENTRY using LAYOUT."
  (if-let* ((fn (plist-get layout :section-value-function)))
      (funcall fn entry)
    (majutsu-row-entry-id entry layout)))

(defun majutsu-row-build-indexes (layout entries)
  "Return (ENTRY-INDEX . SECTION-VALUE-INDEX) for LAYOUT ENTRIES."
  (let ((entry-index (make-hash-table :test #'equal))
        (section-value-index (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (when-let* ((id (majutsu-row-entry-id entry layout)))
        (puthash id entry entry-index))
      (when-let* ((value (majutsu-row-section-value entry layout)))
        (puthash value entry section-value-index)))
    (cons entry-index section-value-index)))

;;; Copy buffer data management

(defun majutsu-row-set-buffer-data (compiled entries)
  "Set current buffer row COMPILED and ENTRIES."
  (let* ((layout (plist-get compiled :copy-layout))
         (indexes (majutsu-row-build-indexes layout entries)))
    (setq-local majutsu-row-buffer-compiled compiled)
    (setq-local majutsu-row-cached-entries entries)
    (setq-local majutsu-row-entry-index (car indexes))
    (setq-local majutsu-row-section-value-index (cdr indexes))))

(defun majutsu-row-clear-buffer-data ()
  "Clear current buffer row data."
  (setq-local majutsu-row-cached-entries nil)
  (setq-local majutsu-row-entry-index nil)
  (setq-local majutsu-row-section-value-index nil))

;;; Copy text-property lookup

(defun majutsu-row-text-property-near-point (property &optional pos)
  "Return PROPERTY near POS, preferring the previous character."
  (let ((pos (or pos (point))))
    (or (and (> pos (point-min))
             (get-text-property (1- pos) property))
        (get-text-property pos property))))

;;; Copy entry lookup

(defun majutsu-row-entry-for-id (id layout &optional entries)
  "Return entry ID using LAYOUT and optional ENTRIES."
  (when (and id (not (and (stringp id) (string-empty-p id))))
    (or (and (not entries)
             (hash-table-p majutsu-row-entry-index)
             (gethash id majutsu-row-entry-index))
        (seq-find (lambda (entry)
                    (equal id (majutsu-row-entry-id entry layout)))
                  (or entries majutsu-row-cached-entries)))))

(defun majutsu-row-entry-for-section-value (value layout &optional entries)
  "Return entry with section VALUE using LAYOUT and optional ENTRIES."
  (when value
    (or (and (not entries)
             (hash-table-p majutsu-row-section-value-index)
             (gethash value majutsu-row-section-value-index))
        (seq-find (lambda (entry)
                    (equal value (majutsu-row-section-value entry layout)))
                  (or entries majutsu-row-cached-entries)))))

(defun majutsu-row-entry-at-point (&optional compiled entries)
  "Return cached row entry at point, or nil."
  (let* ((compiled (majutsu-row-current-compiled compiled))
         (layout (plist-get compiled :copy-layout))
         (section-class (plist-get layout :section-class)))
    (or (when-let* ((entry-id (majutsu-row-text-property-near-point
                               'majutsu-row-entry-id)))
          (majutsu-row-entry-for-id entry-id layout entries))
        (when-let* ((section-value (and section-class
                                        (magit-section-value-if section-class))))
          (majutsu-row-entry-for-section-value
           section-value layout entries)))))

;;; Copy field value access

(defun majutsu-row-plist-field-value (entry field &optional missing)
  "Return FIELD from plist ENTRY, or MISSING when unavailable."
  (let ((key (intern (concat ":" (symbol-name field)))))
    (if (plist-member entry key)
        (plist-get entry key)
      missing)))

(defun majutsu-row-field-copy-string (value)
  "Return canonical clipboard text for field VALUE."
  (cond
   ((null value) "")
   ((stringp value) (substring-no-properties value))
   ((listp value) (mapconcat #'majutsu-row-field-copy-string value "\n"))
   (t (format "%s" value))))

(defun majutsu-row-field-value-present-p (value)
  "Return non-nil if canonical field VALUE should be offered for copying."
  (cond
   ((null value) nil)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   (t t)))

(defun majutsu-row-entry-field-candidate-preview (entry field _layout)
  "Return one-line preview string for FIELD on ENTRY using LAYOUT."
  (let* ((text (majutsu-row-field-copy-string
                (majutsu-row-column entry field)))
         (text (replace-regexp-in-string "[\n\r\t ]+" " " text nil t)))
    (if (> (length text) 48)
        (concat (substring text 0 48) "…")
      text)))

(defun majutsu-row-entry-copyable-fields (entry layout)
  "Return copyable canonical field symbols for ENTRY using LAYOUT order."
  (let ((fields nil)
        (seen nil)
        (missing (make-symbol "majutsu-row-missing-field")))
    (dolist (column (majutsu-row-layout-columns layout))
      (let* ((field (plist-get column :field))
             (value (majutsu-row-column entry field)))
        (when (and (not (memq field seen))
                   (not (eq value missing))
                   (majutsu-row-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (dolist (cell (plist-get entry :columns))
      (let ((field (car cell))
            (value (cdr cell)))
        (when (and (not (memq field seen))
                   (majutsu-row-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (nreverse fields)))

(defun majutsu-row-read-entry-field (entry layout &optional prompt)
  "Read one canonical field from ENTRY using LAYOUT."
  (let* ((default-field (majutsu-row-text-property-near-point
                         'majutsu-row-field))
         (candidates (mapcar (lambda (field)
                               (cons (format "%s\t%s"
                                             field
                                             (majutsu-row-entry-field-candidate-preview
                                              entry field layout))
                                     field))
                             (majutsu-row-entry-copyable-fields
                              entry layout)))
         (default (car (rassoc default-field candidates)))
         (choice (completing-read (or prompt "Copy entry field: ")
                                  (mapcar #'car candidates)
                                  nil t nil nil default)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown entry field %S" choice))))

;;; Copy string helper

(defun majutsu-row-copy-string (string)
  "Copy STRING to the kill ring and echo it."
  (kill-new string)
  (message "%s" string))

(defun majutsu-row-entry-field-value-to-kill (entry field)
  "Copy canonical FIELD value from ENTRY to the kill ring."
  (let* ((value (majutsu-row-column entry field)))
    (unless value
      (user-error "Field %s is unavailable for the current entry" field))
    (unless (majutsu-row-field-value-present-p value)
      (user-error "Field %s is empty for the current entry" field))
    (majutsu-row-copy-string
     (majutsu-row-field-copy-string value))))

;;; Interactive copy commands

;;;###autoload
(defun majutsu-row-copy-field (&optional compiled entries)
  "Copy the rendered value of the structured field at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (layout (plist-get compiled :copy-layout))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point")))
           (instance (majutsu-row-text-property-near-point
                      'majutsu-row-column))
           (field (majutsu-row-text-property-near-point
                   'majutsu-row-field))
           (module (majutsu-row-text-property-near-point
                    'majutsu-row-module))
           (column (or (and instance
                            (majutsu-row-column-by-instance
                             layout instance))
                       (and field module
                            (seq-find (lambda (candidate)
                                        (and (eq (plist-get candidate :field) field)
                                             (eq (plist-get candidate :module) module)))
                                      (majutsu-row-layout-columns layout))))))
      (unless column
        (user-error "No entry field at point"))
      (majutsu-row-copy-string
       (majutsu-row-render-column-text entry column t)))))

;;;###autoload
(defun majutsu-row-copy-module (&optional compiled entries)
  "Copy the rendered structured module at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point")))
           (module (majutsu-row-text-property-near-point
                    'majutsu-row-module))
           (text (cl-case module
                   (heading (majutsu-row-render-heading-content
                             entry compiled nil t))
                   (tail (or (majutsu-row-render-tail entry compiled nil t)
                             ""))
                   (body (or (majutsu-row-render-body entry compiled nil t)
                             ""))
                   (t nil))))
      (unless text
        (user-error "No entry module at point"))
      (majutsu-row-copy-string text))))

;;;###autoload
(defun majutsu-row-copy-entry-field (&optional compiled entries)
  "Copy a canonical field from the current structured entry.

Unlike `majutsu-row-copy-field', this can target fields that are parsed and
stored on the entry but not currently visible.  When called interactively,
prompts with completion over the current entry's available canonical fields.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (layout (plist-get compiled :copy-layout))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point")))
           (field (majutsu-row-read-entry-field entry layout)))
      (majutsu-row-entry-field-value-to-kill entry field))))

;;;###autoload
(defun majutsu-row-copy-commit-id (&optional compiled entries)
  "Copy the current structured entry's commit hash.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point"))))
      (majutsu-row-entry-field-value-to-kill entry 'commit-id))))

;;; Copy transient macro

(defmacro majutsu-row-define-copy-transient (name doc &rest suffixes)
  "Define NAME as a copy transient with DOC and extra SUFFIXES."
  `(transient-define-prefix ,name ()
     ,doc
     [[("s" "Section value" majutsu-copy-section-value)
       ("f" "Visible field at point" majutsu-row-copy-field)
       ("F" "Entry field…" majutsu-row-copy-entry-field)
       ("m" "Visible module at point" majutsu-row-copy-module)
       ,@suffixes]]))

;;;###autoload(autoload 'majutsu-row-copy-transient "majutsu-row" nil t)
(majutsu-row-define-copy-transient
 majutsu-row-copy-transient
 "Transient for semantic copy commands in structured row buffers."
 ("h" "Commit hash" majutsu-row-copy-commit-id))

(provide 'majutsu-row)
;;; majutsu-row.el ends here
