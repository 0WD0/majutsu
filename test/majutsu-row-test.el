;;; majutsu-row-test.el --- Tests for row rendering  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the shared row protocol.

;;; Code:

(require 'ert)
(require 'ansi-color)
(require 'magit-section)
(require 'majutsu-row)
(require 'majutsu-mode)

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

(ert-deftest majutsu-row-split-module-values-rejects-wrong-field-count ()
  "Extra or missing fields invalidate the complete row."
  (dolist (payload '("first\u001elast\u001eextra" "first" ""))
    (should-error (majutsu-row-split-module-values payload 2)
                  :type 'majutsu-row-invalid-record))
  (should-error (majutsu-row-split-module-values "unexpected" 0)
                :type 'majutsu-row-invalid-record))

(ert-deftest majutsu-row-visible-fields-preserve-faces ()
  "Display text retains jj's spans without interpreting JSON-like text."
  (let* ((wire (concat (propertize "A\\n\\u001d" 'font-lock-face 'error)
                       majutsu-row-line-separator
                       (propertize "\"B\"" 'font-lock-face 'warning)))
         (decoded (majutsu-row--decode-transport-value
                   majutsu-row-test--profile 'tail wire)))
    (should (equal decoded "A\\n\\u001d\n\"B\""))
    (should (eq (get-text-property 0 'font-lock-face decoded) 'error))
    (should (eq (get-text-property (1- (length decoded))
                                   'font-lock-face decoded)
                'warning))))

(ert-deftest majutsu-row-invalid-records-never-create-sections ()
  "Reject complete but corrupt records instead of retaining partial metadata."
  (let ((compiled (majutsu-row-test--compiled)))
    (dolist (metadata '("unquoted" "null" "\"bad\\q\"" "\"raw\nnewline\""
                       "\"extra\"\x1e\"field\""))
      (with-temp-buffer
        (magit-section-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (insert majutsu-row-start-token "Title"
                    majutsu-row-tail-token "Alice"
                    majutsu-row-body-token "\nBody"
                    majutsu-row-meta-token metadata
                    majutsu-row-end-token "\n")
            (should-not (majutsu-row-wash-buffer compiled))))
        (should-not majutsu-row-cached-entries)
        (should-not (oref magit-root-section children))))))

(ert-deftest majutsu-row-decode-hook-receives-decoded-data ()
  "The profile hook runs after framing has been removed and validated."
  (let ((data "line\n\x1f\x1e\x1dM@"))
    (should
     (equal (majutsu-row--decode-transport-value
             (majutsu-row-make-profile
              :decode-function (lambda (value) (concat "[" value "]")))
             'metadata (json-serialize data))
            (concat "[" data "]")))))

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
                      (json-serialize "detail-id")
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
  "Parser preserves physical prefixes and leaves raw lines outside entries."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               "○ " majutsu-row-start-token
               "Title line 1\n"
               "│ Title line 2" majutsu-row-tail-token
               "Alice"
               majutsu-row-body-token
               "\n├─Body 1\n│ Body 2"
               majutsu-row-meta-token
               (json-serialize "id-1")
               majutsu-row-end-token "\n"
               "│\n"))
         entry parsed)
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)
            parsed (majutsu-row-read-buffer compiled)))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))
    (should (equal (plist-get entry :body-prefixes) '("├─" "│ ")))
    (should (equal (plist-get parsed :events)
                   (list entry "│\n")))
    (should (= (plist-get entry :end) (- (1+ (length raw)) 2)))
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
                      (json-serialize "machine")
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

(ert-deftest majutsu-row-parser-preserves-real-blank-heading-line ()
  "A trailing heading newline retains its visible blank graph line."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat
               "○ " majutsu-row-start-token "First\n"
               "│ " majutsu-row-tail-token majutsu-row-body-token
               majutsu-row-meta-token "\"id-1\"" majutsu-row-end-token "\n"))
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
               majutsu-row-meta-token "\"broken\"\n"
               majutsu-row-start-token "Good"
               majutsu-row-tail-token majutsu-row-body-token
               majutsu-row-meta-token "\"good-id\"" majutsu-row-end-token "\n"))
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

(ert-deftest majutsu-row-clear-buffer-data-clears-compiled ()
  "Clearing row data should remove all buffer-local row state."
  (let ((compiled (majutsu-row-test--compiled)))
    (with-temp-buffer
      (setq-local majutsu-row-buffer-compiled compiled)
      (setq-local majutsu-row-cached-entries '(entry))
      (setq-local majutsu-row-entry-index (make-hash-table))
      (setq-local majutsu-row-section-ident-index (make-hash-table))
      (majutsu-row-clear-buffer-data)
      (should-not majutsu-row-buffer-compiled)
      (should-not majutsu-row-cached-entries)
      (should-not majutsu-row-entry-index)
      (should-not majutsu-row-section-ident-index))))

(ert-deftest majutsu-row-multiple-body-columns-preserve-blank-lines ()
  "Empty columns add no lines; real blank lines retain their own prefixes."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field first :module body :template "first" :face nil)
             (:field middle :module body :template "" :face nil)
             (:field last :module body :template "last" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat "○ " majutsu-row-start-token "Title"
                      majutsu-row-tail-token majutsu-row-body-token
                      "\n│ first\n│ \n├─second"
                      majutsu-row-field-separator
                      majutsu-row-field-separator
                      "\n╰─last\n  "
                      majutsu-row-meta-token "\"id\""
                      majutsu-row-end-token "\n"))
         entry)
    (with-temp-buffer
      (insert raw)
      (setq entry (car (majutsu-row-parse-buffer compiled))))
    (should (equal (plist-get entry :body-prefixes)
                   '("│ " "│ " "├─" "╰─" "  ")))
    (should (equal (mapcar (lambda (column)
                            (majutsu-row-column-value entry column))
                          (majutsu-row-module-columns compiled 'body))
                   '("first\n\nsecond" "" "last\n")))
    (with-temp-buffer
      (magit-section-mode)
      (majutsu-row-set-buffer-data compiled (list entry))
      (let ((inhibit-read-only t))
        (majutsu-row-insert-entry entry compiled))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "Title\nfirst\n\nsecond\nlast\n\n"))
      (goto-char (point-min))
      (forward-line)
      (dolist (prefix (plist-get entry :body-prefixes))
        (should (equal (substring-no-properties
                        (get-text-property (point) 'line-prefix))
                       prefix))
        (forward-line))
      (goto-char (point-min))
      (search-forward "second")
      (should (eq (get-text-property (1- (point)) 'majutsu-row-field) 'first))
      (backward-char)
      (let (copied)
        (cl-letf (((symbol-function 'kill-new)
                   (lambda (text) (setq copied text))))
          (majutsu-row-copy-field)
          (should (equal copied "first\n\nsecond"))
          (majutsu-row-copy-module)
          (should (equal copied "first\n\nsecond\nlast\n"))))
      (search-forward "last")
      (should (eq (get-text-property (1- (point)) 'majutsu-row-field) 'last)))))

(ert-deftest majutsu-row-body-prefix-projection-preserves-successive-states ()
  "Omit padding while preserving junction order, faces and terminal blanks."
  (let* ((compiled (majutsu-row-test--compiled))
         (prefixes (list (propertize "│   " 'font-lock-face 'warning)
                         (propertize "├─╮ " 'font-lock-face 'success)
                         (propertize "├─╮   " 'font-lock-face 'error)
                         "│ " "╰─╯ " "    " "  "))
         (entry (list :heading-prefixes
                      (list (propertize "│ " 'font-lock-face 'error))
                      :body-prefixes prefixes
                      :column-values
                      '((0 . "Title") (1 . "Alice")
                        (2 . "one\ntwo\nthree\nfour\nfive\nsix\nseven")
                        (3 . "id")))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (majutsu-row-insert-entry entry compiled))
      (let ((projection (get-text-property
                         (point-min) 'majutsu-section-collapsed-graph)))
        (should (equal (substring-no-properties projection)
                       "├─╮ \n╰─╯ \n    "))
        (should (eq (get-text-property 0 'font-lock-face projection) 'success))
        (should-not (text-property-not-all
                     0 (length projection) 'line-prefix nil projection))
        (should-not (text-property-not-all
                     0 (length projection) 'wrap-prefix nil projection)))
      (goto-char (point-min))
      (forward-line)
      (dolist (prefix prefixes)
        (let ((rendered (get-text-property (point) 'line-prefix)))
          (should (equal (substring-no-properties rendered)
                         (substring-no-properties prefix)))
          (should (equal (get-text-property 0 'font-lock-face rendered)
                         (get-text-property 0 'font-lock-face prefix))))
        (forward-line))
      (let ((copied (majutsu-row-filter-buffer-substring
                     (point-min) (point-max))))
        (should (equal copied "Title\none\ntwo\nthree\nfour\nfive\nsix\nseven\n"))
        (dolist (property '(line-prefix wrap-prefix display majutsu-row-field
                           majutsu-row-module majutsu-row-entry-id
                           majutsu-section-collapsed-graph))
          (should-not (text-property-not-all
                       0 (length copied) property nil copied)))))))

(ert-deftest majutsu-row-body-postprocessors-must-preserve-line-shape ()
  "Reject removed, added or newly hidden body lines with a diagnostic."
  (dolist (case '(("one\ntwo" "one") ("one" "one\ntwo")
                  ("one" "") ("" "one") ("one" " \t")))
    (let* ((replacement (cadr case))
           (compiled
            (majutsu-row-test--compiled
             `((:field title :module heading :template "Title" :face nil)
               (:field body :module body :template "Body" :face nil
                :post ,(lambda (_value) replacement))
               (:field id :module metadata :template "id" :face nil))))
           (raw (concat majutsu-row-start-token "Title"
                        majutsu-row-tail-token majutsu-row-body-token
                        (unless (string-empty-p (car case))
                          (concat "\n" (car case)))
                        majutsu-row-meta-token "\"must-not-be-actionable\""
                        majutsu-row-end-token "\n")))
      (with-temp-buffer
        (insert raw)
        (let ((parsed (majutsu-row-read-buffer compiled)))
          (should-not (plist-get parsed :entries))
          (should-not (plist-get parsed :events))
          (should (plist-get parsed :diagnostics)))))))

(ert-deftest majutsu-row-malformed-multiline-body-does-not-retarget-next-row ()
  "A truncated body is discarded, including its text and partial metadata."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               "○ " majutsu-row-start-token "Broken"
               majutsu-row-tail-token "Alice" majutsu-row-body-token
               "\n├─broken body\n│ orphan text"
               majutsu-row-meta-token "\"wrong-id\"\n"
               "○ " majutsu-row-start-token "Good"
               majutsu-row-tail-token "Bob" majutsu-row-body-token
               "\n│ good body"
               majutsu-row-meta-token "\"good-id\"" majutsu-row-end-token "\n")))
    (with-temp-buffer
      (insert raw)
      (let* ((parsed (majutsu-row-read-buffer compiled))
             (entries (plist-get parsed :entries)))
        (should (plist-get parsed :diagnostics))
        (should (equal (mapcar (lambda (entry)
                                (majutsu-row-column entry 'id))
                              entries)
                       '("good-id")))
        (should (equal (plist-get parsed :events) entries))
        (should (equal (majutsu-row-render-body (car entries) compiled)
                       "good body"))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (insert raw)
          (goto-char (point-min))
          (majutsu-row-wash-buffer compiled)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "Good Bob\ngood body\n"))
      (should (equal (mapcar (lambda (section) (oref section value))
                            (oref magit-root-section children))
                     '("good-id")))
      (should-not (string-match-p "[\x1d-\x1f]" (buffer-string))))))

(ert-deftest majutsu-row-projection-distinguishes-blank-from-absent ()
  "A terminating empty prefix is a real graph line, unlike unchanged geometry."
  (let ((compiled (majutsu-row-test--compiled)))
    (dolist (case '(("│ " "" "") ("│ " "│   " nil) ("" "" nil)))
      (let ((entry (list :heading-prefixes (list (nth 0 case))
                         :body-prefixes (list (nth 1 case))
                         :column-values
                         '((0 . "Title") (1 . "Alice")
                           (2 . "Body") (3 . "id")))))
        (with-temp-buffer
          (magit-section-mode)
          (let ((inhibit-read-only t))
            (majutsu-row-insert-entry entry compiled))
          (should (equal (get-text-property
                          (point-min) 'majutsu-section-collapsed-graph)
                         (nth 2 case))))))))

(ert-deftest majutsu-row-empty-lines-keep-the-body-text-column ()
  "Trimmed blank prefixes align the gutter without inventing graph edges."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field body :module body :template "Body" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat "○    " majutsu-row-start-token "Title"
                      majutsu-row-tail-token majutsu-row-body-token
                      "\n├─╮  First\n"
                      (propertize "│ │" 'font-lock-face 'shadow)
                      "\n~    Last\n\n     End"
                      majutsu-row-meta-token "\"id\"" majutsu-row-end-token "\n")))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (insert raw)
          (goto-char (point-min))
          (majutsu-row-wash-buffer compiled)))
      (let* ((entry (car majutsu-row-cached-entries))
             (section (car (oref magit-root-section children))))
        (should (equal (plist-get entry :body-prefixes)
                       '("├─╮  " "│ │" "~    " "" "     ")))
        (goto-char (oref section content))
        (dolist (expected '("├─╮  " "│ │  " "~    " "     " "     "))
          (should (equal (get-text-property (point) 'line-prefix) expected))
          (should (= (string-width (get-text-property (point) 'line-prefix)) 5))
          (forward-line))
        (goto-char (oref section content))
        (forward-line)
        (should (eq (get-text-property
                     0 'font-lock-face (get-text-property (point) 'line-prefix))
                    'shadow))
        (should (equal (filter-buffer-substring
                        (oref section content) (oref section end))
                       "First\n\nLast\n\nEnd\n"))))))

(defun majutsu-row-test--graph-record (title id)
  "Return a framed test record with a foldable body for TITLE and ID."
  (concat "│ " majutsu-row-start-token title
          majutsu-row-tail-token majutsu-row-body-token
          "\n│ Body " title
          majutsu-row-meta-token (json-serialize id)
          majutsu-row-end-token "\n"))

(ert-deftest majutsu-row-graph-lines-are-display-only ()
  "Graph lines have no cursor positions, clipboard text or section actions."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field body :module body :template "Body" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (middle (propertize "├─╮\n│ ~ (elided revisions)\n╰─╯\n"
                             'font-lock-face 'shadow))
         (raw (concat "╭─\n"
                      (majutsu-row-test--graph-record "First" "first")
                      middle
                      (majutsu-row-test--graph-record "Second" "second")
                      "╯")))
    (with-temp-buffer
      (majutsu-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (magit-insert-section (lograph)
            (insert raw)
            (goto-char (point-min))
            (majutsu-row-wash-buffer compiled))))
      (should (equal (filter-buffer-substring (point-min) (point-max))
                     "First\nBody First\nSecond\nBody Second\n"))
      (let* ((graph (car (oref magit-root-section children)))
             (first (car (oref graph children)))
             (second (cadr (oref graph children)))
             (decorations (reverse majutsu-row--graph-overlays)))
        (should (equal (mapcar (lambda (section) (oref section value))
                              (oref graph children))
                       '("first" "second")))
        (should (equal (mapcar (lambda (overlay)
                                (substring-no-properties
                                 (or (overlay-get overlay 'before-string)
                                     (overlay-get overlay 'after-string))))
                              decorations)
                       (list "╭─\n│ " (concat middle "│ ") "╯\n")))
        (should (eq (get-text-property
                     0 'font-lock-face
                     (overlay-get (cadr decorations) 'before-string))
                    'shadow))
        (goto-char (oref first start))
        (forward-line 2)
        (should (eq (magit-current-section) second))
        (should (= (current-column) 0))
        (magit-section-update-highlight t)
        (dolist (overlay magit-section-highlight-overlays)
          (should (<= (oref second start) (overlay-start overlay)))
          (should (<= (overlay-end overlay) (oref second end))))
        (call-interactively (key-binding (kbd "TAB")))
        (should (oref second hidden))
        (should-not (oref graph hidden))
        (should-not (oref first hidden))
        (should (eq (magit-current-section) second))
        (majutsu-section-show second)
        (goto-char (oref second start))
        (forward-line -1)
        (should (eq (magit-current-section) first))))))

(ert-deftest majutsu-row-terminal-graph-keeps-following-section-prefix ()
  "Appending sections cannot turn terminal graph into prefix suppression."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field body :module body :template "Body" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (record (majutsu-row-test--graph-record "First" "first"))
         (following (list :heading-prefixes '(">> ")
                          :columns '((title . "Following") (id . "following")))))
    ;; Cover a completed record, an all-raw stream, and an empty stream.
    ;; Log refresh appends separators, while other consumers append sections.
    (dolist (stream (list (concat record "╯") "╯" ""))
      (dolist (separator '("" "\n" "\n\n"))
        (with-temp-buffer
          (majutsu-mode)
          (let (boundary first next)
            (let ((inhibit-read-only t))
              (magit-insert-section (root)
                (insert stream)
                (goto-char (point-min))
                (majutsu-row-wash-buffer compiled)
                (setq boundary (point))
                (insert separator)
                (setq next (majutsu-row-insert-entry following compiled))))
            (majutsu-row--finish-graph-anchor nil)
            (when (string-prefix-p record stream)
              (setq first (car (oref magit-root-section children))))
            (should (= (length (oref magit-root-section children))
                       (if first 2 1)))
            (let ((decoration (car majutsu-row--graph-overlays)))
              (if (string-empty-p stream)
                  (should-not decoration)
                ;; Redisplay emits the anchor prefix before display strings.
                ;; The graph must precede that prefix, not inherit or lose it.
                (should
                 (equal
                  (concat (get-char-property boundary 'line-prefix)
                          (or (overlay-get decoration 'before-string)
                              (overlay-get decoration 'after-string)))
                  (concat "╯\n" (get-text-property boundary 'line-prefix))))
                (should (<= (overlay-end decoration) (1+ boundary)))
                (let ((text (or (overlay-get decoration 'before-string)
                                (overlay-get decoration 'after-string))))
                  (should (eq (lookup-key (get-text-property 0 'keymap text)
                                          (kbd "<mouse-1>"))
                              'ignore))
                  (should (equal (ensure-list (get-text-property 0 'face text))
                                 '(default))))))
            (should (equal (get-char-property (1+ (oref next start)) 'line-prefix)
                           ">> "))
            (goto-char (oref next start))
            (should (eq (magit-current-section) next))
            (should (= (current-column) 0))
            (when first
              (magit-section-hide first)
              (should (oref first hidden))
              (magit-section-show first)
              (goto-char (oref first start))
              (forward-line (+ 2 (length separator)))
              (should (= (point) (oref next start))))
            (should (equal (filter-buffer-substring (point-min) (point-max))
                           (concat (when first "First\nBody First\n")
                                   separator "Following\n")))
            (goto-char (point-min))
            (should-not (search-forward "╯" nil t))))))))

(ert-deftest majutsu-row-graph-decorations-do-not-survive-refresh ()
  "Refreshing removes old graph strings, including a terminal anchor at EOF."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face nil)
             (:field body :module body :template "Body" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (record (majutsu-row-test--graph-record "First" "first")))
    (with-temp-buffer
      (majutsu-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (insert "╭─\n" record "╯\n")
          (goto-char (point-min))
          (majutsu-row-wash-buffer compiled))
        (let ((old majutsu-row--graph-overlays))
          (erase-buffer)
          (majutsu-row-clear-buffer-data)
          (magit-insert-section (root)
            (insert record)
            (goto-char (point-min))
            (majutsu-row-wash-buffer compiled))
          (should-not (seq-some #'overlay-buffer old))
          (should-not majutsu-row--graph-overlays)
          (should (equal (filter-buffer-substring (point-min) (point-max))
                         "First\nBody First\n")))))))

(ert-deftest majutsu-row-graph-colors-do-not-inherit-the-anchor ()
  "ANSI graph colors survive display strings without leaking heading styles."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Title" :face t)
             (:field body :module body :template "Body" :face t)
             (:field id :module metadata :template "id" :face nil))))
         (raw (concat "│  \e[34m(elided revisions)\e[0m\n"
                      "\e[1;32m○\e[0m " majutsu-row-start-token
                      "\e[1;35mHeading\e[0m"
                      majutsu-row-tail-token majutsu-row-body-token
                      "\n\e[36m╷\e[0m \e[1;35mBody\e[0m"
                      majutsu-row-meta-token "\"id\"" majutsu-row-end-token "\n"))
         caption-face node-face edge-face heading-face)
    (with-temp-buffer
      (majutsu-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (insert raw)
          (let ((ansi-color-apply-face-function
                 #'ansi-color-apply-text-property-face))
            (ansi-color-apply-on-region (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward "(elided")
          (setq caption-face (get-text-property (1- (point)) 'font-lock-face))
          (search-forward "○")
          (setq node-face (get-text-property (1- (point)) 'font-lock-face))
          (search-forward "Heading")
          (setq heading-face (get-text-property (1- (point)) 'font-lock-face))
          (search-forward "╷")
          (setq edge-face (get-text-property (1- (point)) 'font-lock-face))
          (goto-char (point-min))
          (majutsu-row-wash-buffer compiled)))
      (let* ((section (car (oref magit-root-section children)))
             (prefix (get-text-property (oref section start) 'line-prefix))
             (body-prefix (get-text-property (oref section content) 'line-prefix))
             (decoration (overlay-get (car majutsu-row--graph-overlays)
                                      'before-string)))
        ;; Nil is not a neutral face on a display string: it inherits the
        ;; anchor's magenta foreground and bold weight.  `default' resets both.
        (should (equal (ensure-list (get-text-property 0 'face decoration))
                       '(default)))
        (should (equal (get-text-property 3 'face decoration)
                       (list caption-face 'default)))
        (should (equal (get-text-property 0 'face prefix)
                       (append node-face '(default))))
        (should (equal (ensure-list (get-text-property 1 'face prefix))
                       '(default)))
        (should (equal (get-text-property (- (length decoration) 2)
                                          'face decoration)
                       (append node-face '(default))))
        (should (equal (get-text-property 0 'face body-prefix)
                       (list edge-face 'default)))
        (should (equal (ensure-list (get-text-property 1 'face body-prefix))
                       '(default)))
        (should (equal (get-text-property (oref section start) 'font-lock-face)
                       heading-face))
        (should-not (get-text-property (oref section start) 'face))
        (majutsu-section-hide section)
        (let* ((overlay (seq-find (lambda (ov) (overlay-get ov 'invisible))
                                 (overlays-at (1- (oref section content)))))
               (folded (overlay-get overlay 'before-string)))
          (should (equal (get-text-property 1 'face folded)
                         (list edge-face 'default)))
          (should (equal (ensure-list (get-text-property 2 'face folded))
                         '(default))))))))

(provide 'majutsu-row-test)

;;; majutsu-row-test.el ends here
