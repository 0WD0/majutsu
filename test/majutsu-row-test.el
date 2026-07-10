;;; majutsu-row-test.el --- Tests for row rendering  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the shared row protocol.

;;; Code:

(require 'ert)
(require 'magit-section)
(require 'transient)
(require 'majutsu-row)

(defconst majutsu-row-test--profile
  (majutsu-row-make-profile
   :name 'test
   :self-type 'Commit
   :entry-id-function (lambda (entry)
                        (or (majutsu-row-column entry 'id)
                            "unknown"))
   :section-class 'magit-section
   :section-value-function (lambda (entry)
                             (majutsu-row-column entry 'id)))
  "Row profile used by tests.")

(defvar majutsu-row-test--columns nil
  "Dynamically bound flat column list used by row tests.")

(defun majutsu-row-test--compiled (&optional columns)
  "Return compiled test row metadata for COLUMNS."
  (let ((majutsu-row-test--columns
         (or columns
             '((:field title :module heading :template "Title" :face nil)
               (:field author :module tail :template "Alice" :face nil)
               (:field body :module body :template "Body" :face nil)
               (:field id :module metadata :template "id" :face nil))))
        (profile (append majutsu-row-test--profile
                         (list :columns-var 'majutsu-row-test--columns))))
    (majutsu-row-compile profile)))

(defun majutsu-row-test--detail-entry-id (entry)
  "Return a derived test entry id for ENTRY."
  (concat (majutsu-row-column entry 'id) "#detail"))

(ert-deftest majutsu-row-compile-groups-explicit-metadata ()
  "Compiled layouts should group explicit metadata fields."
  (let* ((compiled (majutsu-row-test--compiled))
         (metadata-fields
          (mapcar (lambda (column) (plist-get column :field))
                  (majutsu-row-module-columns compiled 'metadata)))
         (template (prin1-to-string (plist-get compiled :template))))
    (should (equal metadata-fields '(id)))
    (should (string-match-p (regexp-quote "\\x1dS") template))
    (should (string-match-p (regexp-quote "\\x1dT") template))
    (should (string-match-p (regexp-quote "\\x1dB") template))
    (should (string-match-p (regexp-quote "\\x1dM") template))
    (should (string-match-p (regexp-quote "\\x1dE") template))))

(ert-deftest majutsu-row-make-profile-applies-shared-defaults ()
  "Profile helper should seed shared defaults."
  (let ((profile (majutsu-row-make-profile
                  :name 'test
                  :self-type 'Commit
                  :columns-var 'majutsu-row-test--columns
                  :section-hide t
                  :tail-align t)))
    (should (eq (plist-get profile :decode-function)
                'majutsu-row-post-decode-line-separator))
    (should-not (plist-get profile :default-postprocessors))
    (should-not (plist-get profile :field-postprocessors))
    (should (eq (plist-get profile :show-child-count) :inherit))
    (should (eq (plist-get profile :section-hide) t))
    (should (eq (plist-get profile :tail-align) t))))

(ert-deftest majutsu-row-split-module-values-preserves-last-field-separators ()
  "A module's last field should retain any additional separator bytes."
  (should (equal (majutsu-row-split-module-values
                  (concat "first" majutsu-row-field-separator
                          "last" majutsu-row-field-separator "value")
                  2)
                 (list "first"
                       (concat "last" majutsu-row-field-separator "value")))))

(ert-deftest majutsu-row-template-form-preserves-duplicate-column-templates ()
  "Template construction should respect each compiled column instance."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field body :module heading :template "Heading body" :face nil)
             (:field body :module body :template "Entry body" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (form (majutsu-row-template-form compiled)))
    (should (equal form
                   `[:concat
                     ,majutsu-row-start-token
                     "Heading body"
                     ,majutsu-row-tail-token
                     ""
                     ,majutsu-row-body-token
                     "Entry body"
                     ,majutsu-row-meta-token
                     "id"
                     ,majutsu-row-end-token
                     "\n"]))))

(ert-deftest majutsu-row-compile-rejects-column-instance ()
  "Column declarations should not use compiler-internal :instance."
  (let* ((columns
          '((:field title :module heading :template "Title" :face nil
                    :instance 0)))
         (profile (append majutsu-row-test--profile
                          (list :columns-var 'majutsu-row-test--columns)))
         (majutsu-row-test--columns columns))
    (should-error (majutsu-row-compile profile) :type 'user-error)))

(ert-deftest majutsu-row-compile-rejects-duplicate-module-field ()
  "A flat row should have one occurrence of a field in each module."
  (let* ((columns
          '((:field title :module heading :template "One" :face nil)
            (:field title :module heading :template "Two" :face nil)))
         (profile (append majutsu-row-test--profile
                          (list :columns-var 'majutsu-row-test--columns)))
         (majutsu-row-test--columns columns))
    (should-error (majutsu-row-compile profile) :type 'user-error)))

(ert-deftest majutsu-row-profile-drives-section-identity ()
  "Row profile resolvers should drive section identity."
  (let* ((columns
          '((:field title :module heading :template "Detail" :face nil)
            (:field id :module metadata :template "detail-id" :face nil)))
         (profile (majutsu-row-make-profile
                   :name 'test
                   :self-type 'Commit
                   :columns-var 'majutsu-row-test--columns
                   :entry-id-function #'majutsu-row-test--detail-entry-id
                   :section-class 'test-detail-section
                   :section-value-function
                   (lambda (entry)
                     (majutsu-row-column entry 'id))))
         (majutsu-row-test--columns columns)
         (compiled (majutsu-row-compile profile))
         (raw (concat majutsu-row-start-token
                      "Detail"
                      majutsu-row-tail-token
                      majutsu-row-body-token
                      majutsu-row-meta-token
                      "detail-id"
                      majutsu-row-end-token))
         entry)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (equal (majutsu-row-entry-id entry compiled) "detail-id#detail"))
    (should (equal (majutsu-row-section-value entry compiled) "detail-id"))
    (should (eq (majutsu-row-entry-section-class entry compiled)
                'test-detail-section))))

(ert-deftest majutsu-row-compile-requires-columns-var ()
  "Profiles should name exactly one flat column source."
  (should-error (majutsu-row-compile majutsu-row-test--profile)
                :type 'user-error))

(ert-deftest majutsu-row-parse-preserves-shape ()
  "Parser should preserve graph prefixes, modules, metadata, and suffix lines."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               "○ " majutsu-row-start-token
               "Title line 1\n"
               "│ Title line 2" majutsu-row-tail-token
               "Alice"
               majutsu-row-body-token
               "Body 1" majutsu-row-field-line-separator "Body 2"
               majutsu-row-meta-token
               "id-1"
               majutsu-row-end-token "\n"
               "│\n"))
         entry)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))
    (should (equal (plist-get entry :suffix-lines) '("│")))
    (should-not (majutsu-row-column entry 'title))
    (should (equal (majutsu-row-column-value
                    entry (car (majutsu-row-module-columns compiled 'heading)))
                   "Title line 1\nTitle line 2"))
    (should (equal (majutsu-row-render-tail entry compiled) "Alice"))
    (should (equal (majutsu-row-render-body entry compiled)
                   "Body 1\nBody 2"))
    (should (equal (majutsu-row-column entry 'id) "id-1"))))

(ert-deftest majutsu-row-metadata-is-the-only-entry-field-source ()
  "Visible occurrences must not overwrite semantic metadata fields."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Visible" :face nil)
             (:field title :module metadata :template "machine" :face nil))))
         (raw (concat majutsu-row-start-token
                      "Visible"
                      majutsu-row-tail-token
                      majutsu-row-body-token
                      majutsu-row-meta-token
                      "machine"
                      majutsu-row-end-token))
         entry)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (equal (majutsu-row-column entry 'title) "machine"))
    (should (equal (majutsu-row-column-value
                    entry (car (majutsu-row-module-columns compiled 'heading)))
                   "Visible"))))

(ert-deftest majutsu-row-parser-skips-token-only-graph-carrier ()
  "A trailing template newline must not create an empty graph heading line."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat
               "○ " majutsu-row-start-token "First\n"
               "│ Second\n"
               "│ " majutsu-row-tail-token majutsu-row-body-token
               majutsu-row-meta-token "id-1" majutsu-row-end-token "\n"))
         entry)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))
    (should (equal (majutsu-row-column-value
                    entry (car (majutsu-row-module-columns compiled 'heading)))
                   "First\nSecond"))))

(ert-deftest majutsu-row-parser-preserves-real-blank-heading-line ()
  "Two trailing newlines should retain one visible blank graph line."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat
               "○ " majutsu-row-start-token "First\n"
               "│ \n"
               "│ " majutsu-row-tail-token majutsu-row-body-token
               majutsu-row-meta-token "id-1" majutsu-row-end-token "\n"))
         entry)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))
    (should (equal (majutsu-row-column-value
                    entry (car (majutsu-row-module-columns compiled 'heading)))
                   "First\n"))))

(ert-deftest majutsu-row-parser-recovers-after-malformed-record ()
  "An incomplete row should be diagnosed without hiding later valid rows."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat
               majutsu-row-start-token "Broken"
               majutsu-row-tail-token majutsu-row-body-token
               majutsu-row-meta-token "broken\n"
               majutsu-row-start-token "Good"
               majutsu-row-tail-token majutsu-row-body-token
               majutsu-row-meta-token "good-id" majutsu-row-end-token "\n"))
         parsed)
    (with-temp-buffer
      (insert raw)
      (setq parsed (majutsu-row-read-buffer compiled)))
    (should (= (length (plist-get parsed :entries)) 1))
    (should (equal (majutsu-row-column
                    (car (plist-get parsed :entries)) 'id)
                   "good-id"))
    (should (plist-get parsed :diagnostics))))

(ert-deftest majutsu-row-insert-entry-creates-magit-section ()
  "Renderer should create a Magit section and display-only graph prefix."
  (let* ((compiled (majutsu-row-test--compiled))
         (entry (list :columns '((title . "Title")
                                 (author . "Alice")
                                 (id . "id-1"))
                      :heading-prefixes '("○ ")
                      :column-values '((0 . "Title")
                                       (1 . "Alice")
                                       (3 . "id-1")))))
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
                     "Title Alice"))
      (should (equal (substring-no-properties
                      (get-text-property (point) 'line-prefix))
                     "○ "))
      (should (equal (oref (magit-current-section) value) "id-1"))
      (search-forward "Title")
      (backward-char 5)
      (should (eq (get-text-property (point) 'majutsu-row-field)
                  'title))
      (should (eq (get-text-property (point) 'majutsu-row-module)
                  'heading)))))

(ert-deftest majutsu-row-filter-drops-tail-from-copied-heading ()
  "Copy filter should drop tail text when heading text is present."
  (let* ((compiled (majutsu-row-test--compiled))
         (entry (list :columns '((title . "Title")
                                 (author . "Alice")
                                 (id . "id-1"))
                      :heading-prefixes '("○ ")
                      :column-values '((0 . "Title")
                                       (1 . "Alice")
                                       (3 . "id-1")))))
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-row-insert-entry entry compiled)
      (should (equal (majutsu-row-filter-buffer-substring
                      (point-min) (point-max) nil)
                     "Title\n")))))

(ert-deftest majutsu-row-clear-buffer-data-clears-compiled ()
  "Clearing row data should remove all buffer-local row state."
  (let ((compiled (majutsu-row-test--compiled)))
    (with-temp-buffer
      (setq-local majutsu-row-buffer-compiled compiled)
      (setq-local majutsu-row-cached-entries '(entry))
      (setq-local majutsu-row-cached-roots '(root))
      (setq-local majutsu-row-entry-index (make-hash-table))
      (setq-local majutsu-row-section-ident-index (make-hash-table))
      (majutsu-row-clear-buffer-data)
      (should-not majutsu-row-buffer-compiled)
      (should-not majutsu-row-cached-entries)
      (should-not majutsu-row-cached-roots)
      (should-not majutsu-row-entry-index)
      (should-not majutsu-row-section-ident-index))))

(ert-deftest majutsu-row-copy-field-copies-visible-field ()
  "Copying a visible field should use row text properties."
  (let* ((compiled (majutsu-row-test--compiled))
         (entry (list :columns '((title . "Title")
                                 (author . "Alice")
                                 (id . "id-1"))
                      :heading-prefixes '("○ ")
                      :column-values '((0 . "Title")
                                       (1 . "Alice")
                                       (3 . "id-1"))))
         copied)
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-row-set-buffer-data compiled (list entry))
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Alice")
      (backward-char 5)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-row-copy-field))
      (should (equal copied "Alice")))))

(ert-deftest majutsu-row-copy-module-copies-visible-module ()
  "Copying a module should render it without graph decoration."
  (let* ((compiled (majutsu-row-test--compiled))
         (entry (list :columns '((title . "Title")
                                 (author . "Alice")
                                 (id . "id-1"))
                      :heading-prefixes '("○ ")
                      :column-values '((0 . "Title")
                                       (1 . "Alice")
                                       (3 . "id-1"))))
         copied)
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-row-set-buffer-data compiled (list entry))
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-row-copy-module))
      (should (equal copied "Title")))))

(ert-deftest majutsu-row-copy-entry-field-copies-hidden-field ()
  "Copying an entry field should allow hidden canonical metadata."
  (let* ((compiled (majutsu-row-test--compiled))
         (entry (list :columns '((title . "Title")
                                 (author . "Alice")
                                 (id . "id-1"))
                      :heading-prefixes '("○ ")
                      :column-values '((0 . "Title")
                                       (1 . "Alice")
                                       (3 . "id-1"))))
         copied)
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-row-set-buffer-data compiled (list entry))
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (let ((annotation-function
                          (plist-get completion-extra-properties
                                     :annotation-function)))
                     (should (eq (plist-get completion-extra-properties
                                            :category)
                                 'majutsu-row-field))
                     (should (equal (mapcar #'car collection)
                                    '("title" "author" "id")))
                     (should (equal (funcall annotation-function "id")
                                    " id-1")))
                   "id")))
        (majutsu-row-copy-entry-field))
      (should (equal copied "id-1")))))

(ert-deftest majutsu-row-copy-transient-has-copy-actions ()
  "Shared row copy transient should expose semantic copy actions."
  (should (transient-get-suffix 'majutsu-row-copy-transient "s"))
  (should (transient-get-suffix 'majutsu-row-copy-transient "f"))
  (should (transient-get-suffix 'majutsu-row-copy-transient "F"))
  (should (transient-get-suffix 'majutsu-row-copy-transient "h"))
  (should (transient-get-suffix 'majutsu-row-copy-transient "m")))

(provide 'majutsu-row-test)

;;; majutsu-row-test.el ends here
