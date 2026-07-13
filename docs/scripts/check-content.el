;;; check-content.el --- Verify Majutsu documentation contracts -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defun majutsu-docs--fail (format-string &rest args)
  (error "%s" (apply #'format format-string args)))

(defun majutsu-docs--parse-org (path)
  (with-temp-buffer
    (insert-file-contents path)
    (setq-local buffer-file-name path)
    (org-mode)
    (org-with-wide-buffer
     (org-update-radio-target-regexp))
    (org-element-parse-buffer)))

(defun majutsu-docs--headline-property (headline property)
  (org-element-property property headline))

(defun majutsu-docs--top-level-headlines (ast)
  (org-element-map ast 'headline
    (lambda (headline)
      (when (= 1 (org-element-property :level headline)) headline))))

(defun majutsu-docs--page-roots (ast)
  (org-element-map ast 'headline
    (lambda (headline)
      (when (org-element-property :EXPORT_FILE_NAME headline) headline))))

(defun majutsu-docs--owner-page (headline pages &optional exclude)
  (let ((position (org-element-property :begin headline)))
    (car
     (sort
      (seq-filter
       (lambda (page)
         (and (not (eq page exclude))
              (<= (org-element-property :begin page) position)
              (< position (org-element-property :end page))))
       pages)
      (lambda (left right)
        (> (org-element-property :level left)
           (org-element-property :level right)))))))

(defun majutsu-docs--duplicates (values)
  (let ((seen (make-hash-table :test #'equal)) duplicates)
    (dolist (value values)
      (if (gethash value seen)
          (push value duplicates)
        (puthash value t seen)))
    (delete-dups (nreverse duplicates))))

(defun majutsu-docs--check-pages (root)
  (let* ((org-path (expand-file-name "docs/majutsu.org" root))
         (ast (majutsu-docs--parse-org org-path))
         (pages (majutsu-docs--top-level-headlines ast))
         (page-roots (majutsu-docs--page-roots ast))
         custom-ids routes)
    (dolist (page pages)
      (unless (org-element-property :EXPORT_FILE_NAME page)
        (majutsu-docs--fail
         "top-level headline %S is not a page root"
         (org-element-property :raw-value page))))
    (dolist (page page-roots)
      (let ((title (org-element-property :raw-value page))
            (custom-id (majutsu-docs--headline-property page :CUSTOM_ID))
            (route (majutsu-docs--headline-property page :EXPORT_FILE_NAME))
            (description (majutsu-docs--headline-property page :DESCRIPTION)))
        (unless (and (stringp title) (not (string-empty-p (string-trim title))))
          (majutsu-docs--fail "page root at source position %d has no title"
                              (org-element-property :begin page)))
        (unless (and (stringp custom-id)
                     (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" custom-id))
          (majutsu-docs--fail "%s has unsafe or missing CUSTOM_ID %S"
                              title custom-id))
        (unless (and (stringp route)
                     (string-match-p
                      "\\`\\(?:guide\\|workflows\\|repository\\|extend\\|reference\\)/[a-z0-9][a-z0-9-]*\\'"
                      route))
          (majutsu-docs--fail "%s has unsafe or missing EXPORT_FILE_NAME %S"
                              title route))
        (unless (and (stringp description)
                     (not (string-empty-p (string-trim description))))
          (majutsu-docs--fail "%s has no DESCRIPTION" title))
        (when (> (org-element-property :level page) 1)
          (unless (majutsu-docs--owner-page page page-roots page)
            (majutsu-docs--fail "nested page %s has no owning page" custom-id)))))
    (setq custom-ids
          (mapcar (lambda (page)
                    (org-element-property :CUSTOM_ID page))
                  page-roots)
          routes
          (mapcar (lambda (page)
                    (org-element-property :EXPORT_FILE_NAME page))
                  page-roots))
    (let ((duplicates (majutsu-docs--duplicates custom-ids)))
      (when duplicates
        (majutsu-docs--fail "duplicate page CUSTOM_IDs: %S" duplicates)))
    (let ((duplicates (majutsu-docs--duplicates routes)))
      (when duplicates
        (majutsu-docs--fail "duplicate page routes: %S" duplicates)))
    (list :pages (length pages)
          :published-pages (length page-roots)
          :nested-pages (- (length page-roots) (length pages)))))

(defun majutsu-docs--check-variable-entries (root)
  (let* ((org-path (expand-file-name "docs/majutsu.org" root))
         (ast (majutsu-docs--parse-org org-path))
         (variables
          (delq
           nil
           (org-element-map ast 'item
             (lambda (item)
               (when-let* ((tag (org-element-property :tag item))
                           (text (org-element-interpret-data tag))
                           ((string-match
                             "\\`User Option:[[:space:]]+\\([^[:space:]]+\\)\\'"
                             text)))
                 (match-string 1 text))))))
         (definitions (make-hash-table :test #'equal)))
    (dolist (path (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (re-search-forward
                "^(defcustom[[:space:]\n]+\\([^[:space:]()\n]+\\)" nil t)
          (puthash (match-string 1) path definitions))))
    (let ((duplicates (majutsu-docs--duplicates variables)))
      (when duplicates
        (majutsu-docs--fail "duplicate User Option entries: %S" duplicates)))
    (dolist (variable variables)
      (unless (gethash variable definitions)
        (majutsu-docs--fail
         "User Option entry %s has no source defcustom" variable)))
    (list :variables (length variables))))

(defun majutsu-docs--reference-attribute-text (plain-list)
  (let ((value (org-element-property :attr_reference plain-list)))
    (cond
     ((stringp value) value)
     ((listp value) (mapconcat #'identity value " "))
     (t nil))))

(defun majutsu-docs--reference-attribute (name text)
  (when (and text
             (string-match
              (format ":%s[[:space:]]+\\(?:\"\\([^\"]+\\)\"\\|\\([^[:space:]]+\\)\\)"
                      (regexp-quote name))
              text))
    (or (match-string 1 text) (match-string 2 text))))

(defun majutsu-docs--check-key-reference-attributes (root)
  (let* ((org-path (expand-file-name "docs/majutsu.org" root))
         (ast (majutsu-docs--parse-org org-path))
         (allowed-fields '("kind" "interface" "mode" "scope" "prefix" "state"))
         (allowed-kinds '("command-binding" "transient-argument" "key-binding"))
         (list-count 0)
         (key-count 0)
         conflict-prefix-seen)
    (org-element-map ast 'plain-list
      (lambda (plain-list)
        (let* ((items (seq-filter
                       (lambda (node) (eq (org-element-type node) 'item))
                       (org-element-contents plain-list)))
               (key-tags
                (delq nil
                      (mapcar
                       (lambda (item)
                         (when-let* ((tag (org-element-property :tag item))
                                     (text (string-trim
                                            (org-element-interpret-data tag)))
                                     ((string-prefix-p "Key:" text)))
                           text))
                       items))))
          (when key-tags
            (let* ((position (org-element-property :begin plain-list))
                   (attributes (majutsu-docs--reference-attribute-text plain-list))
                   (kind (majutsu-docs--reference-attribute "kind" attributes))
                   (interface (majutsu-docs--reference-attribute "interface" attributes))
                   (fields nil)
                   (start 0))
              (cl-incf list-count)
              (cl-incf key-count (length key-tags))
              (unless attributes
                (majutsu-docs--fail
                 "Key list at source position %d lacks #+attr_reference"
                 position))
              (unless (member kind allowed-kinds)
                (majutsu-docs--fail
                 "Key list at source position %d has unsupported reference kind %S"
                 position kind))
              (unless (and interface (not (string-empty-p interface)))
                (majutsu-docs--fail
                 "Key list at source position %d lacks a reference interface"
                 position))
              (while (string-match ":\\([a-z][a-z-]*\\)" attributes start)
                (push (match-string 1 attributes) fields)
                (setq start (match-end 0)))
              (dolist (field (delete-dups fields))
                (unless (member field allowed-fields)
                  (majutsu-docs--fail
                   "Key list at source position %d has unsupported reference field %s"
                   position field)))
              (when (member "Key: n / p (majutsu-conflict-next / majutsu-conflict-prev)"
                            key-tags)
                (setq conflict-prefix-seen
                      (equal "C-c ^"
                             (majutsu-docs--reference-attribute
                              "prefix" attributes)))))))))
    (unless conflict-prefix-seen
      (majutsu-docs--fail
       "Vanilla conflict bindings must author their C-c ^ prefix"))
    (list :key-lists list-count :keys key-count)))

(defun majutsu-docs-check (root)
  (let ((pages (majutsu-docs--check-pages root))
        (variables (majutsu-docs--check-variable-entries root))
        (references (majutsu-docs--check-key-reference-attributes root)))
    (princ
     (format
      "DOCS_CONTENT_CHECK=PASS root_pages=%d published_pages=%d nested_pages=%d variables=%d key_lists=%d keys=%d\n"
      (plist-get pages :pages)
      (plist-get pages :published-pages)
      (plist-get pages :nested-pages)
      (plist-get variables :variables)
      (plist-get references :key-lists)
      (plist-get references :keys)))))

(condition-case error-data
    (let ((root (or (pop command-line-args-left) default-directory)))
      (majutsu-docs-check (file-name-as-directory (expand-file-name root)))
      (kill-emacs 0))
  (error
   (princ (format "DOCS_CONTENT_CHECK=FAIL error=%S\n" error-data)
          #'external-debugging-output)
   (kill-emacs 1)))

;;; check-content.el ends here
