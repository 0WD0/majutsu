;;; majutsu-row-test.el --- Tests for row rendering  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the shared row protocol.

;;; Code:

(require 'ert)
(require 'magit-section)
(require 'transient)
(require 'majutsu-row)

(defconst majutsu-row-test--profile
  (list :name 'test
        :self-type 'Commit
        :default-modules '((title . heading)
                           (author . tail)
                           (body . body)
                           (id . metadata))
        :required-fields '(id)
        :template-function (lambda (field) (symbol-name field))
        :entry-id-function (lambda (entry)
                             (or (majutsu-row-column entry 'id)
                                 "unknown"))
        :section-class 'magit-section
        :section-value-function (lambda (entry)
                                  (majutsu-row-column entry 'id))
        :tail-align nil)
  "Row profile used by tests.")

(defun majutsu-row-test--compiled (&optional columns)
  "Return a compiled test row layout."
  (majutsu-row-compile
   majutsu-row-test--profile
   (or columns
       '((:field title :module heading :template "Title" :face nil)
         (:field author :module tail :template "Alice" :face nil)
         (:field body :module body :template "Body" :face nil)))))

(defun majutsu-row-test--payload (&rest fields)
  "Join FIELDS with the row field separator."
  (string-join fields majutsu-row-field-separator))

(ert-deftest majutsu-row-compile-adds-required-metadata ()
  "Compiled layouts should append hidden required metadata fields."
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
    (should (equal (majutsu-row-column entry 'title)
                   "Title line 1\nTitle line 2"))
    (should (equal (majutsu-row-render-tail entry compiled) "Alice"))
    (should (equal (majutsu-row-render-body entry compiled)
                   "Body 1\nBody 2"))
    (should (equal (majutsu-row-column entry 'id) "id-1"))))

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
                      (point-min) (point-max) nil compiled)
                     "Title\n")))))

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
      (setq-local majutsu-row-buffer-compiled compiled)
      (setq-local majutsu-row-cached-entries (list entry))
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
      (setq-local majutsu-row-buffer-compiled compiled)
      (setq-local majutsu-row-cached-entries (list entry))
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
      (setq-local majutsu-row-buffer-compiled compiled)
      (setq-local majutsu-row-cached-entries (list entry))
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (or (seq-find (lambda (candidate)
                                   (string-match-p "id" candidate))
                                 candidates)
                       (car candidates)))))
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
