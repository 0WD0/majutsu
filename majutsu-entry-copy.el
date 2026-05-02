;;; majutsu-entry-copy.el --- Structured entry copy helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared semantic copy protocol for structured Majutsu entry buffers.
;; Views provide a layout, cached entries, and tagged rendered text; this
;; library provides O(1) entry lookup, field/module rendering for clipboard
;; text, and reusable copy transients.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit-section)
(require 'majutsu-section)

(defvar-local majutsu-entry-copy-buffer-layout nil
  "Structured entry copy layout for the current buffer.")

(defvar-local majutsu-entry-copy-cached-entries nil
  "Structured entries available for copy commands in the current buffer.")

(defvar-local majutsu-entry-copy-entry-index nil
  "Hash table mapping stable entry ids to cached entries.")

(defvar-local majutsu-entry-copy-section-value-index nil
  "Hash table mapping section values to cached entries.")

(defconst majutsu-entry-copy--ui-properties
  '(majutsu-entry-copy-module
    majutsu-entry-copy-field
    majutsu-entry-copy-column
    majutsu-entry-copy-entry-id
    majutsu-entry-copy-decoration
    line-prefix
    wrap-prefix
    display)
  "Text properties removed from copied entry text.")

(defun majutsu-entry-copy-current-layout (&optional layout)
  "Return LAYOUT or the current buffer's entry-copy layout."
  (or layout
      majutsu-entry-copy-buffer-layout
      (user-error "No entry-copy layout in current buffer")))

(defun majutsu-entry-copy-layout-columns (layout)
  "Return column specs from entry-copy LAYOUT."
  (plist-get (majutsu-entry-copy-current-layout layout) :columns))

(defun majutsu-entry-copy-entry-id (entry layout)
  "Return stable entry id for ENTRY using LAYOUT."
  (if-let* ((fn (plist-get (majutsu-entry-copy-current-layout layout)
                           :entry-id-function)))
      (funcall fn entry)
    (majutsu-entry-copy-entry-field-value entry 'id nil layout)))

(defun majutsu-entry-copy-section-value (entry layout)
  "Return Magit section value for ENTRY using LAYOUT."
  (let ((layout (majutsu-entry-copy-current-layout layout)))
    (if-let* ((fn (plist-get layout :section-value-function)))
        (funcall fn entry)
      (majutsu-entry-copy-entry-id entry layout))))

(defun majutsu-entry-copy-build-indexes (layout entries)
  "Return (ENTRY-INDEX . SECTION-VALUE-INDEX) for LAYOUT ENTRIES."
  (let ((entry-index (make-hash-table :test #'equal))
        (section-value-index (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (when-let* ((id (majutsu-entry-copy-entry-id entry layout)))
        (puthash id entry entry-index))
      (when-let* ((value (majutsu-entry-copy-section-value entry layout)))
        (puthash value entry section-value-index)))
    (cons entry-index section-value-index)))

(defun majutsu-entry-copy-set-buffer-data (layout entries)
  "Set current buffer entry-copy LAYOUT and ENTRIES."
  (let ((indexes (majutsu-entry-copy-build-indexes layout entries)))
    (setq-local majutsu-entry-copy-buffer-layout layout)
    (setq-local majutsu-entry-copy-cached-entries entries)
    (setq-local majutsu-entry-copy-entry-index (car indexes))
    (setq-local majutsu-entry-copy-section-value-index (cdr indexes))))

(defun majutsu-entry-copy-clear-buffer-data ()
  "Clear current buffer entry-copy data."
  (setq-local majutsu-entry-copy-cached-entries nil)
  (setq-local majutsu-entry-copy-entry-index nil)
  (setq-local majutsu-entry-copy-section-value-index nil))

(defun majutsu-entry-copy-text-property-near-point (property &optional pos)
  "Return PROPERTY near POS, preferring the previous character."
  (let ((pos (or pos (point))))
    (or (and (> pos (point-min))
             (get-text-property (1- pos) property))
        (get-text-property pos property))))

(defun majutsu-entry-copy-entry-for-id (id layout &optional entries)
  "Return entry ID using LAYOUT and optional ENTRIES."
  (let ((layout (majutsu-entry-copy-current-layout layout)))
    (when (and id (not (and (stringp id) (string-empty-p id))))
      (or (and (not entries)
               (hash-table-p majutsu-entry-copy-entry-index)
               (gethash id majutsu-entry-copy-entry-index))
          (seq-find (lambda (entry)
                      (equal id (majutsu-entry-copy-entry-id entry layout)))
                    (or entries majutsu-entry-copy-cached-entries))))))

(defun majutsu-entry-copy-entry-for-section-value (value layout &optional entries)
  "Return entry with section VALUE using LAYOUT and optional ENTRIES."
  (let ((layout (majutsu-entry-copy-current-layout layout)))
    (when value
      (or (and (not entries)
               (hash-table-p majutsu-entry-copy-section-value-index)
               (gethash value majutsu-entry-copy-section-value-index))
          (seq-find (lambda (entry)
                      (equal value (majutsu-entry-copy-section-value entry layout)))
                    (or entries majutsu-entry-copy-cached-entries))))))

(defun majutsu-entry-copy-entry-at-point (&optional layout entries)
  "Return cached structured entry at point, or nil."
  (let* ((layout (majutsu-entry-copy-current-layout layout))
         (section-class (plist-get layout :section-class)))
    (or (when-let* ((entry-id (majutsu-entry-copy-text-property-near-point
                               'majutsu-entry-copy-entry-id)))
          (majutsu-entry-copy-entry-for-id entry-id layout entries))
        (when-let* ((section-value (and section-class
                                        (magit-section-value-if section-class))))
          (majutsu-entry-copy-entry-for-section-value
           section-value layout entries)))))

(defun majutsu-entry-copy-entry-field-value
    (entry field &optional missing layout)
  "Return canonical FIELD value from ENTRY, or MISSING when unavailable."
  (let ((layout (majutsu-entry-copy-current-layout layout)))
    (if-let* ((fn (plist-get layout :field-value-function)))
        (funcall fn entry field missing)
      (alist-get field (plist-get entry :columns) missing nil #'eq))))

(defun majutsu-entry-copy-plist-field-value (entry field &optional missing)
  "Return FIELD from plist ENTRY, or MISSING when unavailable."
  (let ((key (intern (concat ":" (symbol-name field)))))
    (if (plist-member entry key)
        (plist-get entry key)
      missing)))

(defun majutsu-entry-copy-column-value (entry column &optional layout)
  "Return per-instance value for ENTRY COLUMN using LAYOUT."
  (let* ((layout (majutsu-entry-copy-current-layout layout))
         (instance (plist-get column :instance))
         (column-values (plist-get entry :column-values))
         (missing (make-symbol "majutsu-entry-copy-missing-instance"))
         (value (if instance
                    (alist-get instance column-values missing nil #'eql)
                  missing)))
    (if (eq value missing)
        (majutsu-entry-copy-entry-field-value
         entry (plist-get column :field) nil layout)
      value)))

(defun majutsu-entry-copy-column-by-instance (layout instance)
  "Return column spec from LAYOUT identified by INSTANCE."
  (seq-find (lambda (column)
              (eql (plist-get column :instance) instance))
            (majutsu-entry-copy-layout-columns layout)))

(defun majutsu-entry-copy-display-string (value)
  "Return VALUE converted to a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value) (mapconcat #'majutsu-entry-copy-display-string value " "))
   (t (format "%s" value))))

(defun majutsu-entry-copy-single-line-string (value)
  "Return VALUE flattened to a single display line."
  (if (stringp value)
      (string-trim (replace-regexp-in-string "[\n\r]+" " " value nil t))
    value))

(defun majutsu-entry-copy-concat-heading-parts (parts)
  "Concatenate heading PARTS without spaces after newlines."
  (let ((out ""))
    (dolist (part parts out)
      (unless (string-empty-p part)
        (let ((need-space
               (and (> (length out) 0)
                    (not (eq (aref out (1- (length out))) ?\n))
                    (not (eq (aref part 0) ?\n)))))
          (setq out (concat out (if need-space " " "") part)))))))

(defun majutsu-entry-copy-render-column-text (entry column &optional layout)
  "Return plain rendered text for ENTRY COLUMN using LAYOUT."
  (let* ((layout (majutsu-entry-copy-current-layout layout))
         (module (plist-get column :module))
         (value (majutsu-entry-copy-display-string
                 (majutsu-entry-copy-column-value entry column layout))))
    (when (eq module 'tail)
      (setq value (majutsu-entry-copy-single-line-string value)))
    (substring-no-properties value)))

(defun majutsu-entry-copy-module-columns (layout module)
  "Return LAYOUT columns assigned to MODULE."
  (seq-filter (lambda (column)
                (eq (plist-get column :module) module))
              (majutsu-entry-copy-layout-columns layout)))

(defun majutsu-entry-copy-render-module-parts (entry layout module)
  "Return plain rendered ENTRY parts for MODULE using LAYOUT."
  (let (parts)
    (dolist (column (majutsu-entry-copy-module-columns layout module))
      (let ((value (majutsu-entry-copy-render-column-text entry column layout)))
        (unless (if (eq module 'body)
                    (string-empty-p (string-trim value))
                  (string-empty-p value))
          (push value parts))))
    (nreverse parts)))

(defun majutsu-entry-copy-render-module (entry layout module)
  "Return plain rendered ENTRY MODULE using LAYOUT."
  (pcase module
    ('heading
     (majutsu-entry-copy-concat-heading-parts
      (majutsu-entry-copy-render-module-parts entry layout 'heading)))
    ('tail
     (when-let* ((parts (majutsu-entry-copy-render-module-parts
                         entry layout 'tail)))
       (majutsu-entry-copy-concat-heading-parts parts)))
    ('body
     (when-let* ((parts (majutsu-entry-copy-render-module-parts
                         entry layout 'body)))
       (string-join parts "\n")))
    (_ nil)))

(defun majutsu-entry-copy-content-properties (layout entry-id module &optional column)
  "Return text properties for ENTRY-ID content in MODULE."
  (let ((props `(majutsu-entry-copy-module ,module
                 majutsu-entry-copy-entry-id ,entry-id)))
    (when column
      (setq props (append props
                          `(majutsu-entry-copy-field ,(plist-get column :field)
                            majutsu-entry-copy-column ,(plist-get column :instance)))))
    props))

(defun majutsu-entry-copy-decoration-properties (_layout entry-id module decoration)
  "Return text properties for ENTRY-ID DECORATION in MODULE."
  `(majutsu-entry-copy-module ,module
    majutsu-entry-copy-entry-id ,entry-id
    majutsu-entry-copy-decoration ,decoration))

(defun majutsu-entry-copy-propertize-content
    (text layout entry-id module &optional column)
  "Return TEXT tagged as MODULE content for ENTRY-ID and COLUMN."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-entry-copy-content-properties
              layout entry-id module column))
    text))

(defun majutsu-entry-copy-propertize-decoration
    (text layout entry-id module decoration)
  "Return TEXT tagged as MODULE DECORATION for ENTRY-ID."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-entry-copy-decoration-properties
              layout entry-id module decoration))
    text))

(defun majutsu-entry-copy-field-copy-string (value)
  "Return canonical clipboard text for field VALUE."
  (cond
   ((null value) "")
   ((stringp value) (substring-no-properties value))
   ((listp value) (mapconcat #'majutsu-entry-copy-field-copy-string value "\n"))
   (t (format "%s" value))))

(defun majutsu-entry-copy-field-value-present-p (value)
  "Return non-nil if canonical field VALUE should be offered for copying."
  (cond
   ((null value) nil)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   (t t)))

(defun majutsu-entry-copy-entry-field-candidate-preview (entry field layout)
  "Return one-line preview string for FIELD on ENTRY using LAYOUT."
  (let* ((text (majutsu-entry-copy-field-copy-string
                (majutsu-entry-copy-entry-field-value entry field nil layout)))
         (text (replace-regexp-in-string "[\n\r\t ]+" " " text nil t)))
    (if (> (length text) 48)
        (concat (substring text 0 48) "…")
      text)))

(defun majutsu-entry-copy-entry-copyable-fields (entry layout)
  "Return copyable canonical field symbols for ENTRY using LAYOUT order."
  (let ((fields nil)
        (seen nil)
        (missing (make-symbol "majutsu-entry-copy-missing-field")))
    (dolist (column (majutsu-entry-copy-layout-columns layout))
      (let* ((field (plist-get column :field))
             (value (majutsu-entry-copy-entry-field-value
                     entry field missing layout)))
        (when (and (not (memq field seen))
                   (not (eq value missing))
                   (majutsu-entry-copy-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (dolist (cell (plist-get entry :columns))
      (let ((field (car cell))
            (value (cdr cell)))
        (when (and (not (memq field seen))
                   (majutsu-entry-copy-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (nreverse fields)))

(defun majutsu-entry-copy-read-entry-field (entry layout &optional prompt)
  "Read one canonical field from ENTRY using LAYOUT."
  (let* ((layout (majutsu-entry-copy-current-layout layout))
         (default-field (majutsu-entry-copy-text-property-near-point
                         'majutsu-entry-copy-field))
         (candidates (mapcar (lambda (field)
                               (cons (format "%s\t%s"
                                             field
                                             (majutsu-entry-copy-entry-field-candidate-preview
                                              entry field layout))
                                     field))
                             (majutsu-entry-copy-entry-copyable-fields
                              entry layout)))
         (default (car (rassoc default-field candidates)))
         (choice (completing-read (or prompt "Copy entry field: ")
                                  (mapcar #'car candidates)
                                  nil t nil nil default)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown entry field %S" choice))))

(defun majutsu-entry-copy-string (string)
  "Copy STRING to the kill ring and echo it."
  (kill-new string)
  (message "%s" string))

(defun majutsu-entry-copy-entry-field-value-to-kill (entry field &optional layout)
  "Copy canonical FIELD value from ENTRY to the kill ring."
  (let* ((layout (majutsu-entry-copy-current-layout layout))
         (missing (make-symbol "majutsu-entry-copy-missing-field"))
         (value (majutsu-entry-copy-entry-field-value
                 entry field missing layout)))
    (when (eq value missing)
      (user-error "Field %s is unavailable for the current entry" field))
    (unless (majutsu-entry-copy-field-value-present-p value)
      (user-error "Field %s is empty for the current entry" field))
    (majutsu-entry-copy-string
     (majutsu-entry-copy-field-copy-string value))))

;;;###autoload
(defun majutsu-entry-copy-field (&optional layout entries)
  "Copy the rendered value of the structured field at point."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((layout (majutsu-entry-copy-current-layout layout))
           (entry (or (majutsu-entry-copy-entry-at-point layout entries)
                      (user-error "No entry at point")))
           (instance (majutsu-entry-copy-text-property-near-point
                      'majutsu-entry-copy-column))
           (field (majutsu-entry-copy-text-property-near-point
                   'majutsu-entry-copy-field))
           (module (majutsu-entry-copy-text-property-near-point
                    'majutsu-entry-copy-module))
           (column (or (and instance
                            (majutsu-entry-copy-column-by-instance
                             layout instance))
                       (and field module
                            (seq-find (lambda (candidate)
                                        (and (eq (plist-get candidate :field) field)
                                             (eq (plist-get candidate :module) module)))
                                      (majutsu-entry-copy-layout-columns layout))))))
      (unless column
        (user-error "No entry field at point"))
      (majutsu-entry-copy-string
       (majutsu-entry-copy-render-column-text entry column layout)))))

;;;###autoload
(defun majutsu-entry-copy-module (&optional layout entries)
  "Copy the rendered structured module at point."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((layout (majutsu-entry-copy-current-layout layout))
           (entry (or (majutsu-entry-copy-entry-at-point layout entries)
                      (user-error "No entry at point")))
           (module (majutsu-entry-copy-text-property-near-point
                    'majutsu-entry-copy-module))
           (text (and module
                      (majutsu-entry-copy-render-module entry layout module))))
      (unless text
        (user-error "No entry module at point"))
      (majutsu-entry-copy-string text))))

;;;###autoload
(defun majutsu-entry-copy-entry-field (&optional layout entries)
  "Copy a canonical field from the current structured entry."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((layout (majutsu-entry-copy-current-layout layout))
           (entry (or (majutsu-entry-copy-entry-at-point layout entries)
                      (user-error "No entry at point")))
           (field (majutsu-entry-copy-read-entry-field entry layout)))
      (majutsu-entry-copy-entry-field-value-to-kill entry field layout))))

;;;###autoload
(defun majutsu-entry-copy-commit-id (&optional layout entries)
  "Copy the current structured entry's commit hash."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((layout (majutsu-entry-copy-current-layout layout))
           (entry (or (majutsu-entry-copy-entry-at-point layout entries)
                      (user-error "No entry at point"))))
      (majutsu-entry-copy-entry-field-value-to-kill entry 'commit-id layout))))

(defun majutsu-entry-copy-string-has-module-p (string module)
  "Return non-nil if STRING contains text marked with MODULE."
  (let ((pos 0)
        found)
    (while (and (< pos (length string)) (not found))
      (setq found (eq (get-text-property
                       pos 'majutsu-entry-copy-module string)
                      module)
            pos (or (next-single-property-change
                     pos 'majutsu-entry-copy-module string)
                    (length string))))
    found))

(defun majutsu-entry-copy-string-remove-module (string module)
  "Return STRING with text marked as MODULE removed."
  (let ((pos 0)
        parts)
    (while (< pos (length string))
      (let* ((next (or (next-single-property-change
                        pos 'majutsu-entry-copy-module string)
                       (length string)))
             (current (get-text-property
                       pos 'majutsu-entry-copy-module string)))
        (unless (eq current module)
          (push (substring string pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun majutsu-entry-copy-cleanup-copied-string (string &optional extra-properties)
  "Strip entry-copy and EXTRA-PROPERTIES from copied STRING."
  (when (stringp string)
    (remove-list-of-text-properties
     0 (length string)
     (append majutsu-entry-copy--ui-properties extra-properties)
     string))
  string)

(defun majutsu-entry-copy-filter-buffer-substring
    (beg end &optional delete extra-properties)
  "Filter copied structured entry text between BEG and END."
  (let ((string (buffer-substring--filter beg end delete))
        (trim-tail nil))
    (when (and (stringp string)
               (majutsu-entry-copy-string-has-module-p string 'heading)
               (majutsu-entry-copy-string-has-module-p string 'tail))
      (setq string (majutsu-entry-copy-string-remove-module string 'tail))
      (setq trim-tail t))
    (setq string (majutsu-entry-copy-cleanup-copied-string
                  string extra-properties))
    (when (and trim-tail (stringp string))
      (setq string (replace-regexp-in-string "[ \t]+$" "" string)))
    string))

(defmacro majutsu-entry-copy-define-transient (name doc &rest suffixes)
  "Define NAME as an entry-copy transient with DOC and extra SUFFIXES."
  `(transient-define-prefix ,name ()
     ,doc
     [[("s" "Section value" majutsu-copy-section-value)
       ("f" "Visible field at point" majutsu-entry-copy-field)
       ("F" "Entry field…" majutsu-entry-copy-entry-field)
       ("m" "Visible module at point" majutsu-entry-copy-module)
       ,@suffixes]]))

;;;###autoload(autoload 'majutsu-entry-copy-transient "majutsu-entry-copy" nil t)
(majutsu-entry-copy-define-transient
 majutsu-entry-copy-transient
 "Transient for semantic copy commands in structured entry buffers."
 ("h" "Commit hash" majutsu-entry-copy-commit-id))

(provide 'majutsu-entry-copy)
;;; majutsu-entry-copy.el ends here
