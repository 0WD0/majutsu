;;; majutsu-graph-entry-test.el --- Tests for graph entries -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the shared graph-entry protocol.

;;; Code:

(require 'ert)
(require 'magit-section)
(require 'majutsu-graph-entry)

(defconst majutsu-graph-entry-test--profile
  (list :name 'test
        :self-type 'Commit
        :default-modules '((title . heading)
                           (author . tail)
                           (body . body)
                           (id . metadata))
        :required-fields '(id)
        :template-function (lambda (field) (symbol-name field))
        :entry-id-function (lambda (entry)
                             (or (majutsu-graph-entry-column entry 'id)
                                 "unknown"))
        :section-class 'magit-section
        :section-value-function (lambda (entry)
                                  (majutsu-graph-entry-column entry 'id))
        :tail-align nil)
  "Graph-entry profile used by tests.")

(defun majutsu-graph-entry-test--compiled (&optional columns)
  "Return a compiled test graph-entry layout."
  (majutsu-graph-entry-compile
   majutsu-graph-entry-test--profile
   (or columns
       '((:field title :module heading :template "Title" :face nil)
         (:field author :module tail :template "Alice" :face nil)
         (:field body :module body :template "Body" :face nil)))))

(defun majutsu-graph-entry-test--payload (&rest fields)
  "Join FIELDS with the graph-entry field separator."
  (string-join fields majutsu-graph-entry-field-separator))

(ert-deftest majutsu-graph-entry-compile-adds-required-metadata ()
  "Compiled layouts should append hidden required metadata fields."
  (let* ((compiled (majutsu-graph-entry-test--compiled))
         (metadata-fields
          (mapcar (lambda (column) (plist-get column :field))
                  (majutsu-graph-entry-module-columns compiled 'metadata)))
         (template (prin1-to-string (plist-get compiled :template))))
    (should (equal metadata-fields '(id)))
    (should (string-match-p (regexp-quote "\\x1dS") template))
    (should (string-match-p (regexp-quote "\\x1dT") template))
    (should (string-match-p (regexp-quote "\\x1dB") template))
    (should (string-match-p (regexp-quote "\\x1dM") template))
    (should (string-match-p (regexp-quote "\\x1dE") template))))

(ert-deftest majutsu-graph-entry-parse-preserves-graph-entry-shape ()
  "Parser should preserve graph prefixes, modules, metadata, and suffix lines."
  (let* ((compiled (majutsu-graph-entry-test--compiled))
         (raw (concat
               "○ " majutsu-graph-entry-start-token
               "Title line 1\n"
               "│ Title line 2" majutsu-graph-entry-tail-token
               "Alice"
               majutsu-graph-entry-body-token
               "Body 1" majutsu-graph-entry-field-line-separator "Body 2"
               majutsu-graph-entry-meta-token
               "id-1"
               majutsu-graph-entry-end-token "\n"
               "│\n"))
         entry)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-graph-entry-parse-at-point compiled)))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))
    (should (equal (plist-get entry :suffix-lines) '("│")))
    (should (equal (majutsu-graph-entry-column entry 'title)
                   "Title line 1\nTitle line 2"))
    (should (equal (majutsu-graph-entry-render-tail entry compiled) "Alice"))
    (should (equal (majutsu-graph-entry-render-body entry compiled)
                   "Body 1\nBody 2"))
    (should (equal (majutsu-graph-entry-column entry 'id) "id-1"))))

(ert-deftest majutsu-graph-entry-insert-entry-creates-magit-section ()
  "Renderer should create a Magit section and display-only graph prefix."
  (let* ((compiled (majutsu-graph-entry-test--compiled))
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
      (majutsu-graph-entry-insert-entry entry compiled)
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
      (should (eq (get-text-property (point) 'majutsu-graph-entry-field)
                  'title))
      (should (eq (get-text-property (point) 'majutsu-graph-entry-module)
                  'heading)))))

(ert-deftest majutsu-graph-entry-filter-drops-tail-from-copied-heading ()
  "Copy filter should drop tail text when heading text is present."
  (let* ((compiled (majutsu-graph-entry-test--compiled))
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
      (majutsu-graph-entry-insert-entry entry compiled)
      (should (equal (majutsu-graph-entry-filter-buffer-substring
                      (point-min) (point-max) nil compiled)
                     "Title\n")))))

(provide 'majutsu-graph-entry-test)

;;; majutsu-graph-entry-test.el ends here
