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

(defvar majutsu-row-test--layout nil
  "Dynamically bound row layout used by layout tests.")

(defun majutsu-row-test--compiled (&optional columns)
  "Return a compiled test row layout."
  (let ((majutsu-row-test--layout
         `(:columns ,(or columns
                         '((:field title :module heading :template "Title" :face nil)
                           (:field author :module tail :template "Alice" :face nil)
                           (:field body :module body :template "Body" :face nil)
                           (:field id :module metadata :template "id" :face nil)))))
        (profile (append majutsu-row-test--profile
                         (list :layout-var 'majutsu-row-test--layout))))
    (majutsu-row-compile profile)))

(defun majutsu-row-test--payload (&rest fields)
  "Join FIELDS with the row field separator."
  (string-join fields majutsu-row-field-separator))

(defun majutsu-row-test--yes-p (value &optional _ctx)
  "Return non-nil when VALUE is yes."
  (equal value "yes"))

(defun majutsu-row-test--detail-entry-id (entry)
  "Return a derived test entry id for ENTRY."
  (concat (majutsu-row-column entry 'id) "#detail"))

(defun majutsu-row-test--raw-entry (title id &optional body prefix children)
  "Return one raw test entry with TITLE, ID, BODY, PREFIX, and CHILDREN."
  (concat (or prefix "")
          majutsu-row-start-token
          title
          majutsu-row-tail-token
          majutsu-row-body-token
          (or body "")
          (and children majutsu-row-children-token)
          (or children "")
          majutsu-row-meta-token
          (majutsu-row-test--payload id)
          majutsu-row-end-token))

(defun majutsu-row-test--inline-child-stream (&rest rows)
  "Return a transport child stream containing ROWS."
  (concat majutsu-row-push-token
          (apply #'concat rows)
          majutsu-row-pop-token))

(defun majutsu-row-test--raw-role-entry (role title id &optional parent-id body prefix children)
  "Return one raw role-bearing test entry."
  (concat (or prefix "")
          majutsu-row-start-token
          majutsu-row-role-token
          (symbol-name role)
          majutsu-row-role-token
          title
          majutsu-row-tail-token
          majutsu-row-body-token
          (or body "")
          (and children majutsu-row-children-token)
          (or children "")
          majutsu-row-meta-token
          (majutsu-row-test--payload id (or parent-id ""))
          majutsu-row-end-token))

(defun majutsu-row-test--collect-sections (&optional section)
  "Return SECTION and all descendants, defaulting to `magit-root-section'."
  (let ((section (or section magit-root-section)))
    (cons section
          (apply #'append
                 (mapcar #'majutsu-row-test--collect-sections
                         (oref section children))))))

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
                  :layout-var 'majutsu-row-test--layout
                  :section-hide t
                  :tail-align t)))
    (should (eq (plist-get profile :decode-function)
                'majutsu-row-post-decode-line-separator))
    (should-not (plist-get profile :default-postprocessors))
    (should-not (plist-get profile :field-postprocessors))
    (should (eq (plist-get profile :show-child-count) :inherit))
    (should (eq (plist-get profile :section-hide) t))
    (should (eq (plist-get profile :tail-align) t))))

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

(ert-deftest majutsu-row-template-form-emits-explicit-children-token ()
  "Template construction should encode children using an explicit token."
  (let* ((compiled
          (majutsu-row-test--compiled
           '((:field title :module heading :template "Heading" :face nil)
             (:field id :module metadata :template "id" :face nil))))
         (form (majutsu-row-template-form
                compiled
                :children "CHILDREN"
                :newline nil)))
    (should (equal form
                   `[:concat
                     ,majutsu-row-start-token
                     "Heading"
                     ,majutsu-row-tail-token
                     ""
                     ,majutsu-row-body-token
                     ""
                     ,majutsu-row-children-token
                     "CHILDREN"
                     ,majutsu-row-meta-token
                     "id"
                     ,majutsu-row-end-token]))))

(ert-deftest majutsu-row-compile-rejects-layout-instance ()
  "Layout declarations should not use compiler-internal :instance."
  (let* ((layout
          '(:columns
            ((title :module heading :template "Title" :face nil
                    :instance 0))))
         (profile (append majutsu-row-test--profile
                          (list :layout-var 'majutsu-row-test--layout)))
         (majutsu-row-test--layout layout))
    (should-error (majutsu-row-compile profile) :type 'user-error)))

(ert-deftest majutsu-row-role-drives-section-identity ()
  "Layout roles should be transported and drive section metadata."
  (let* ((layout
          '(:role test-detail
            :entry-id #'majutsu-row-test--detail-entry-id
            :section-class test-detail-section
            :section-value id
            :columns
            ((title :module heading :template "Detail" :face nil)
             (id :module metadata :template "detail-id" :face nil))))
         (profile (append majutsu-row-test--profile
                          (list :layout-var 'majutsu-row-test--layout)))
         (majutsu-row-test--layout layout)
         (compiled (majutsu-row-compile profile))
         (raw (concat majutsu-row-start-token
                      majutsu-row-role-token "test-detail"
                      majutsu-row-role-token
                      "Detail"
                      majutsu-row-tail-token
                      majutsu-row-body-token
                      majutsu-row-meta-token
                      "detail-id"
                      majutsu-row-end-token))
         entry)
    (should (string-match-p (regexp-quote "test-detail")
                            (plist-get compiled :template)))
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (eq (majutsu-row-role entry) 'test-detail))
    (should (equal (majutsu-row-entry-id entry compiled) "detail-id#detail"))
    (should (equal (majutsu-row-section-value entry compiled) "detail-id"))
    (should (eq (majutsu-row-entry-section-class entry compiled)
                'test-detail-section))))

(ert-deftest majutsu-row-entry-at-current-section-uses-magit-ident ()
  "Current-section lookup should honor full Magit section identity."
  (let* ((layout
          (list :schema
                '((title :module heading :face nil)
                  (id :module metadata :face nil)
                  (parent-id :module metadata :face nil))
                :role 'parent
                :section-class 'parent-section
                :section-value 'id
                :columns '((title "Parent")
                           (id "parent")
                           (parent-id ""))
                :children
                (list :nodes
                      (list
                       (list :role 'child
                             :section-class 'child-section
                             :section-value 'id
                             :columns '((title "Child")
                                        (id "child")
                                        (parent-id "parent")))))))
         (profile (append majutsu-row-test--profile
                          (list :layout-var 'majutsu-row-test--layout)))
         (majutsu-row-test--layout layout)
         (compiled (majutsu-row-compile profile))
         (raw (concat
               (majutsu-row-test--raw-role-entry
                'parent "Parent 1" "p1" "" nil "○ "
                (majutsu-row-test--inline-child-stream
                 (majutsu-row-test--raw-role-entry
                  'child "Child 1" "shared" "p1")))
               "\n"
               (majutsu-row-test--raw-role-entry
                'parent "Parent 2" "p2" "" nil "○ "
                (majutsu-row-test--inline-child-stream
                 (majutsu-row-test--raw-role-entry
                  'child "Child 2" "shared" "p2")))
               "\n"))
         entries
         second-child
         entry)
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (insert raw))
      (setq entries (majutsu-row-wash-buffer compiled))
      (setq second-child
            (seq-find
             (lambda (section)
               (and (eq (oref section type) 'child-section)
                    (let ((parent (oref section parent)))
                      (and parent
                           (eq (oref parent type) 'parent-section)
                           (equal (oref parent value) "p2")))))
             (majutsu-row-test--collect-sections)))
      (should second-child)
      (goto-char (oref second-child start))
      (setq entry (majutsu-row--entry-at-current-section))
      (should (equal (majutsu-row-column entry 'parent-id) "p2")))))

(ert-deftest majutsu-row-layout-template-form-builds-child-row-tree ()
  "Layout lowering should keep child rows as full field-bearing rows."
  (let* ((layout
          '(:schema
            ((title :module heading :face nil)
             (id :module metadata :face nil))
            :columns
            ((title "Parent")
             (id "parent"))
            :children
            (:nodes
             ((:each [:parents]
               :as p
               :columns
               ((title [:description :first_line])
                (body :module body :face nil
                      :template [:description])
                (id [:change_id :short 8])))))))
         (profile (append majutsu-row-test--profile
                          (list :layout-var 'majutsu-row-test--layout)))
         (majutsu-row-test--layout layout)
         (compiled (majutsu-row-compile profile))
         (template (plist-get compiled :template)))
    (should (equal (mapcar (lambda (column) (plist-get column :field))
                           (plist-get compiled :columns))
                   '(title id body)))
    (should (string-match-p (regexp-quote "self.parents().map(|p|") template))
    (should (string-match-p (regexp-quote "p.description().first_line()") template))
    (should (string-match-p (regexp-quote "p.change_id().short(8)") template))
    (should (string-match-p (regexp-quote "\\x1DC") template))
    (should (string-match-p (regexp-quote "\\x1D[") template))
    (should (string-match-p (regexp-quote "\\x1D]") template))))

(ert-deftest majutsu-row-layout-node-local-columns-configure-schema ()
  "Child-local columns should configure schema like root columns."
  (let* ((layout
          '(:schema
            ((title :module heading :face nil)
             (id :module metadata :face nil))
            :columns
            ((title "Parent")
             (id "parent"))
            :children
            (:nodes
             ((:columns
               ((flag :module metadata :face nil
                      :post majutsu-row-test--yes-p
                      :template "yes")))))))
         (profile (append majutsu-row-test--profile
                          (list :layout-var 'majutsu-row-test--layout)))
         (majutsu-row-test--layout layout)
         (compiled (majutsu-row-compile profile))
         entry)
    (should (equal (mapcar (lambda (column) (plist-get column :field))
                           (plist-get compiled :columns))
                   '(title id flag)))
    (with-temp-buffer
      (insert (concat majutsu-row-start-token
                      "Title"
                      majutsu-row-tail-token
                      majutsu-row-body-token
                      majutsu-row-meta-token
                      (majutsu-row-test--payload "parent" "yes")
                      majutsu-row-end-token))
      (goto-char (point-min))
      (setq entry (majutsu-row-parse-at-point compiled)))
    (should (eq (majutsu-row-column entry 'flag) t))))

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

(ert-deftest majutsu-row-read-buffer-builds-stack-tree ()
  "Stream parser should use inline child streams to build children."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               (majutsu-row-test--raw-entry
                "Parent" "parent" nil "○ "
                (majutsu-row-test--inline-child-stream
                 (majutsu-row-test--raw-entry "Child" "child" nil)))
               "\n"
               (majutsu-row-test--raw-entry "Sibling" "sibling" nil "○ ")
               "\n"))
         parsed roots entries parent child sibling)
    (with-temp-buffer
      (insert raw)
      (setq parsed (majutsu-row-read-buffer compiled)))
    (setq roots (plist-get parsed :roots)
          entries (plist-get parsed :entries)
          parent (car roots)
          child (car (plist-get parent :children))
          sibling (cadr roots))
    (should (equal (mapcar (lambda (entry)
                             (majutsu-row-column entry 'id))
                           entries)
                   '("parent" "child" "sibling")))
    (should (= (length roots) 2))
    (should (equal (majutsu-row-column parent 'id) "parent"))
    (should (equal (majutsu-row-column child 'id) "child"))
    (should (eq (plist-get child :parent) parent))
    (should (= (plist-get child :depth) 1))
    (should (equal (majutsu-row-column sibling 'id) "sibling"))))

(ert-deftest majutsu-row-read-buffer-pop-restores-parent ()
  "Multiple inline children should stay under the same parent."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               (majutsu-row-test--raw-entry
                "Parent" "parent" nil "○ "
                (majutsu-row-test--inline-child-stream
                 (majutsu-row-test--raw-entry "Child 1" "child-1" nil)
                 (majutsu-row-test--raw-entry "Child 2" "child-2" nil)))
               "\n"))
         parsed parent children)
    (with-temp-buffer
      (insert raw)
      (setq parsed (majutsu-row-read-buffer compiled)))
    (setq parent (car (plist-get parsed :roots))
          children (plist-get parent :children))
    (should (equal (mapcar (lambda (entry)
                             (majutsu-row-column entry 'id))
                           children)
                   '("child-1" "child-2")))
    (should (seq-every-p (lambda (entry)
                           (eq (plist-get entry :parent) parent))
                         children))))

(ert-deftest majutsu-row-wash-buffer-reports-diagnostics ()
  "Washing should surface parser diagnostics instead of dropping them."
  (let ((compiled (majutsu-row-test--compiled))
        reported)
    (with-temp-buffer
      (insert majutsu-row-push-token "\n")
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq reported (apply #'format format-string args)))))
        (majutsu-row-wash-buffer compiled)))
    (should (string-match-p "Row push without previous entry" reported))))

(ert-deftest majutsu-row-wash-buffer-caches-roots-in-stream-order ()
  "Washing should accumulate roots in the order they are streamed."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               (majutsu-row-test--raw-entry
                "Parent" "parent" nil "○ "
                (majutsu-row-test--inline-child-stream
                 (majutsu-row-test--raw-entry "Child" "child" nil)))
               "\n"
               (majutsu-row-test--raw-entry "Sibling" "sibling" nil "○ ")
               "\n"))
         entries)
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (test-root)
        (insert raw)
        (setq entries (majutsu-row-wash-buffer compiled)))
      (should (equal (mapcar (lambda (entry)
                               (majutsu-row-column entry 'id))
                             entries)
                     '("parent" "child" "sibling")))
      (should (equal (mapcar (lambda (entry)
                               (majutsu-row-column entry 'id))
                             majutsu-row-cached-roots)
                     '("parent" "sibling")))
      (goto-char (point-min))
      (should (search-forward "Parent" nil t))
      (should (search-forward "Sibling" nil t)))))

(ert-deftest majutsu-row-insert-forest-creates-nested-magit-sections ()
  "Renderer should put child entries inside the parent section body."
  (let* ((compiled (majutsu-row-test--compiled))
         (raw (concat
               (majutsu-row-test--raw-entry
                "Parent" "parent" "Parent body" "○ "
                (majutsu-row-test--inline-child-stream
                 (majutsu-row-test--raw-entry "Child" "child" nil)))
               "\n"))
         parsed parent-section child-section)
    (with-temp-buffer
      (insert raw)
      (setq parsed (majutsu-row-read-buffer compiled)))
    (with-temp-buffer
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (test-root)
        (majutsu-row-insert-forest (plist-get parsed :roots) compiled))
      (goto-char (point-min))
      (search-forward "Parent")
      (setq parent-section (magit-current-section))
      (goto-char (point-min))
      (search-forward "Child")
      (setq child-section (magit-current-section))
      (should (equal (oref parent-section value) "parent"))
      (should (equal (oref child-section value) "child"))
      (should (eq (oref child-section parent) parent-section))
      (should (memq child-section (oref parent-section children)))
      (should (< (save-excursion
                   (goto-char (point-min))
                   (search-forward "Parent body")
                   (point))
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "Child")
                   (point)))))))

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
                          (plist-get completion-extra-properties :annotation-function)))
                     (should (eq (plist-get completion-extra-properties :category)
                                 'majutsu-row-field))
                     (should (equal collection '("title" "author" "id")))
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
