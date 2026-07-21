;;; majutsu-log-test.el --- Tests for majutsu log protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for marker-based sequential log parsing/render metadata.

;;; Code:

(require 'ert)
(require 'majutsu-log)
(require 'majutsu-section)
(require 'majutsu-jj-integration)

(defconst majutsu-log-test--required-columns
  '((:field id :module metadata :face nil)
    (:field commit-id :module metadata :face nil)
    (:field parent-ids :module metadata :face nil))
  "Hidden metadata fields required by log parser tests.")

(defun majutsu-log-test--column-template (field)
  "Return test template for log FIELD."
  (let ((var (intern-soft (format "majutsu-log-template-%s" field))))
    (if (and var (boundp var))
        (symbol-value var)
      (user-error "Unknown log test field %S" field))))

(defun majutsu-log-test--ensure-required-columns (columns)
  "Return COLUMNS with required hidden metadata appended."
  (let ((out (copy-sequence columns)))
    (dolist (spec majutsu-log-test--required-columns out)
      (let ((field (plist-get spec :field)))
        (unless (seq-some
                 (lambda (column)
                   (and (eq (plist-get column :field) field)
                        (eq (plist-get column :module) 'metadata)))
                 out)
          (setq out (append out (list spec))))))))

(defun majutsu-log-test--column-with-template (spec)
  "Return test column SPEC with a concrete template."
  (let* ((column (copy-sequence spec))
         (field (plist-get column :field)))
    (if (plist-member column :template)
        column
      (append column (list :template
                           (majutsu-log-test--column-template field))))))

(defun majutsu-log-test--compile-columns (columns)
  "Compile test COLUMNS through a temporary flat column list."
  (let ((majutsu-log-commit-columns
         (mapcar #'majutsu-log-test--column-with-template
                 (majutsu-log-test--ensure-required-columns columns))))
    (majutsu-row-compile (majutsu-log--row-profile))))

(defun majutsu-log-test--base-compiled ()
  "Return a compact compiled layout for parser tests."
  (majutsu-log-test--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field long-desc :module body :face t)
     (:field id :module metadata :face nil)
     (:field timestamp :module metadata :face nil)
     (:field flags :module metadata :face nil))))

(defun majutsu-log-test--heading-aux-compiled ()
  "Return a layout with auxiliary heading fields."
  (majutsu-log-test--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field author :module heading :face nil)
     (:field timestamp :module heading :face nil)
     (:field id :module metadata :face nil))))

(defun majutsu-log-test--tail-compiled ()
  "Return a layout with auxiliary tail fields."
  (majutsu-log-test--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field author :module tail :face nil)
     (:field timestamp :module tail :face nil)
     (:field long-desc :module body :face t)
     (:field id :module metadata :face nil))))

(defun majutsu-log-test--relations-compiled ()
  "Return a layout that includes relation metadata."
  (majutsu-log-test--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field description :module metadata :face nil)
     (:field id :module metadata :face nil)
     (:field parent-ids :module metadata :face nil))))

(defun majutsu-log-test--parse-entries (compiled raw)
  "Parse RAW entries with COMPILED layout and return parsed entries."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (majutsu-row-parse-buffer compiled)))

(defun majutsu-log-test--transport-value (value)
  "Return transport encoding for test VALUE."
  (json-serialize
   (cond
    ((null value) "")
    ((stringp value) value)
    ((listp value) (string-join value majutsu-log--field-list-separator))
    (t (format "%s" value)))))

(ert-deftest majutsu-log-insert-conflicts/inherited-conflicts-remain-navigable ()
  "Group inherited conflicts even when the merge has no changes-in diff."
  (majutsu-jj-integration-with-repo repo
    (let ((paths '("src/one.el" "src/two.el" "tests/unit/one.el"))
          base left)
      (dolist (path paths)
        (let ((file (majutsu-jj-integration-file repo path)))
          (make-directory (file-name-directory file) t)
          (majutsu-jj-integration-write-text file "base\n")))
      (majutsu-jj-integration-commit repo "base")
      (setq base (majutsu-jj-resolve-single-commit "@-"))
      (dolist (path paths)
        (majutsu-jj-integration-write-text
         (majutsu-jj-integration-file repo path) "left\n"))
      (majutsu-jj-integration-commit repo "left")
      (setq left (majutsu-jj-resolve-single-commit "@-"))
      (majutsu-jj-integration-call repo "new" base "-m" "right")
      (dolist (path paths)
        (majutsu-jj-integration-write-text
         (majutsu-jj-integration-file repo path) "right\n"))
      (majutsu-jj-integration-call repo "new" left "@" "-m" "merge")
      (should (equal (majutsu-jj-integration-output repo "diff" "--summary") ""))
      (with-temp-buffer
        (majutsu-log-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (majutsu-log-insert-conflicts)))
        (let* ((conflicts (car (oref magit-root-section children)))
               (directories (oref conflicts children))
               (src (car directories))
               (tests (cadr directories)))
          (should (equal (mapcar (lambda (section) (oref section value)) directories)
                         '("src/" "tests/unit/")))
          (should (equal (majutsu-file-tree-files conflicts) paths))
          (magit-map-sections
           (lambda (section)
             (when (magit-section-match 'jj-file section)
               (goto-char (oref section start))
               (should (equal (majutsu-file-at-point) (oref section value)))))
           conflicts)
          (majutsu-section-hide src)
          (should (equal (majutsu-file-tree-files src)
                         '("src/one.el" "src/two.el")))
          (dolist (file (oref src children))
            (should (invisible-p (oref file start))))
          (should-not (invisible-p (oref (car (oref tests children)) start))))))))

(ert-deftest majutsu-log-insert-conflicts/escapes-labels-without-changing-targets ()
  "Control characters cannot split headings or alter file action targets."
  (let ((path "dir\nname/sub\tfolder/file\rname.el"))
    (cl-letf (((symbol-function 'majutsu-jj-conflicted-files)
               (lambda (&optional _rev _filesets)
                 (list (list :path path :sides 3)))))
      (with-temp-buffer
        (majutsu-log-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (majutsu-log-insert-conflicts)))
        (let* ((conflicts (car (oref magit-root-section children)))
               (directory (car (oref conflicts children)))
               (file (car (oref directory children))))
          (should (equal (oref directory value) "dir\nname/sub\tfolder/"))
          (dolist (section (list directory file))
            (should (= (count-lines (oref section start) (oref section content)) 1)))
          (goto-char (oref file start))
          (should (equal (majutsu-file-at-point) path))
          (should (equal (get-text-property (point) 'help-echo) path)))))))

(defun majutsu-log-test--metadata-payload (compiled values)
  "Return metadata payload for COMPILED using VALUES alist."
  (string-join
   (mapcar (lambda (column)
             (majutsu-log-test--transport-value
              (alist-get (plist-get column :field) values nil nil #'eq)))
           (majutsu-row-module-columns compiled 'metadata))
   majutsu-row-field-separator))

(defun majutsu-log-test--column (compiled field module)
  "Return FIELD occurrence in MODULE from COMPILED."
  (seq-find (lambda (column)
              (and (eq (plist-get column :field) field)
                   (eq (plist-get column :module) module)))
            (plist-get compiled :columns)))

(defun majutsu-log-test--raw-entry (id title &optional parent-ids)
  "Return one raw relation-aware log entry for ID and TITLE."
  (let ((compiled (majutsu-log-test--relations-compiled)))
    (concat
     "○ " majutsu-row-start-token id
     majutsu-row-field-separator title
     majutsu-row-tail-token
     majutsu-row-body-token
     majutsu-row-meta-token
     (majutsu-log-test--metadata-payload
      compiled
      `((id . ,id)
        (commit-id . ,(concat "commit-" id))
        (description . ,title)
        (parent-ids . ,parent-ids)))
     majutsu-row-end-token
     "\n")))

(defun majutsu-log-test--post-by-module (value ctx)
  "Return a module-specific projection of VALUE using CTX."
  (pcase (plist-get ctx :module)
    ('heading (format "[%s]" value))
    ('body (upcase value))
    (_ value)))

(ert-deftest majutsu-log/integration-identities-survive-display-customization ()
  "Hidden and divergent rows retain exact targets despite custom display IDs."
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-call repo "describe" "-m" "before")
    (let* ((old (majutsu-jj-integration-output
                 repo "log" "--no-graph" "-r" "@" "-T" "commit_id"))
           (change (majutsu-jj-integration-output
                    repo "log" "--no-graph" "-r" "@" "-T" "change_id"))
           (majutsu-log-template-description "DISPLAY")
           (majutsu-log-template-commit-id "display-id")
           (compiled (majutsu-log--ensure-template)))
      (majutsu-jj-integration-call repo "describe" "-m" "after")
      (let ((new (majutsu-jj-integration-output
                  repo "log" "--no-graph" "-r" "@" "-T" "commit_id")))
        (cl-labels
            ((entry (revision)
               (car (majutsu-log-test--parse-entries
                     compiled
                     (majutsu-jj-integration-output
                      repo "--ignore-working-copy" "--color=never"
                      "log" "--no-graph" "-r" revision
                      "-T" (plist-get compiled :template))))))
          (let ((hidden (entry old))
                (visible (entry "@")))
            (should (equal (majutsu-log--entry-id hidden) old))
            (should (equal (majutsu-log--entry-id visible) change))
            (should (equal (majutsu-row-column hidden 'description) "before"))
            (should (equal (majutsu-row-column visible 'description) "after"))
            (should (equal (majutsu-row-column visible 'commit-id) new))
            (should (equal
                     (majutsu-row-column-value
                      visible (majutsu-log-test--column compiled 'description 'heading))
                     "DISPLAY"))
            (should (equal
                     (majutsu-row-column-value
                      visible (majutsu-log-test--column compiled 'commit-id 'tail))
                     "display-id")))
          (majutsu-jj-integration-call repo "new" old "-m" "side")
          (dolist (commit (list old new))
            (should (equal (majutsu-log--entry-id (entry commit)) commit)))
          (should (equal (majutsu-row-column (entry "@") 'parent-ids)
                         (list old))))))))

(ert-deftest majutsu-log-entry-id-uses-canonical-log-id ()
  "Entry ids should use the transported canonical log id."
  (should (equal (majutsu-log--entry-id
                  '(:columns ((id . "change-full")
                              (change-id . "change-full")
                              (commit-id . "commit-full"))))
                 "change-full"))
  (should (equal (majutsu-log--entry-id
                  '(:columns ((change-id . "change-full")
                              (commit-id . "commit-full"))))
                 "change-full")))


(ert-deftest majutsu-log-parse-entry-with-graph-and-multiline-heading ()
  "Parser should handle graph prefixes and multiline heading payload."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               "○ " majutsu-row-start-token "chg" majutsu-row-field-separator
               "Title line 1\n│ Title line 2" majutsu-row-tail-token
               majutsu-row-body-token
               "\n│ body line 1\n│ body line 2"
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-123")
                  (timestamp . "2m ago")
                  (flags . "@ immutable")))
               majutsu-row-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (equal (majutsu-row-column entry 'id) "id-123"))
    (should-not (majutsu-row-column entry 'change-id))
    (should (equal (majutsu-row-column-value
                    entry (majutsu-log-test--column
                           compiled 'description 'heading))
                   "Title line 1\nTitle line 2"))
    (should (equal (majutsu-row-column-value
                    entry (majutsu-log-test--column compiled 'long-desc 'body))
                   "body line 1\nbody line 2"))
    (should (equal (majutsu-row-column entry 'timestamp) "2m"))
    (should (equal (majutsu-row-column entry 'flags) "@ immutable"))
    (should (= (plist-get entry :indent) 2))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))))

(ert-deftest majutsu-log-parse-entry-without-graph ()
  "Parser should also support --no-graph output (indent = 0)."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               majutsu-row-start-token "chg" majutsu-row-field-separator
               "Top\nTail" majutsu-row-tail-token
               majutsu-row-body-token "\npayload"
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-999")
                  (timestamp . "1h ago")))
               majutsu-row-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (= 0 (plist-get entry :indent)))
    (should (equal (majutsu-row-column-value
                    entry (majutsu-log-test--column
                           compiled 'description 'heading))
                   "Top\nTail"))
    (should (equal (majutsu-row-column-value
                    entry (majutsu-log-test--column compiled 'long-desc 'body))
                   "payload"))
    (should (equal (majutsu-row-column entry 'timestamp) "1h"))
    (should (equal (plist-get entry :heading-prefixes) '("" "")))))

(ert-deftest majutsu-log-parse-entry-with-tail-module ()
  "Parser should decode tail payloads separately from body and metadata."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (raw (concat
               "○ " majutsu-row-start-token "chg" majutsu-row-field-separator "Title"
               majutsu-row-tail-token
               "Alice" majutsu-row-field-separator "2m ago"
               majutsu-row-body-token
               "\n│ body line 1\n│ body line 2"
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-123")))
               majutsu-row-end-token "\n"))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (should (equal (majutsu-row-column entry 'id) "id-123"))
    (should-not (majutsu-row-column entry 'author))
    (should-not (majutsu-row-column entry 'timestamp))
    (should (equal (majutsu-row-column-value
                    entry (majutsu-log-test--column compiled 'long-desc 'body))
                   "body line 1\nbody line 2"))
    (should (equal (majutsu-row-render-tail entry compiled) "Alice 2m"))))

(ert-deftest majutsu-log-parse-entry-preserves-between-entry-lines ()
  "Raw lines remain ordered events outside both adjacent entries."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               "○ " majutsu-row-start-token "chg1" majutsu-row-field-separator "Title1"
               majutsu-row-tail-token
               majutsu-row-body-token
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-1")))
               majutsu-row-end-token "\n"
               "│ carry-line\n"
               "○ " majutsu-row-start-token "chg2" majutsu-row-field-separator "Title2"
               majutsu-row-tail-token
               majutsu-row-body-token
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-2")))
               majutsu-row-end-token "\n"))
         (parsed (with-temp-buffer
                   (insert raw)
                   (majutsu-row-read-buffer compiled)))
         (entries (plist-get parsed :entries))
         (first (nth 0 entries))
         (second (nth 1 entries)))
    (should (= 2 (length entries)))
    (should (equal (majutsu-row-column first 'id) "id-1"))
    (should (equal (majutsu-row-column second 'id) "id-2"))
    (should (equal (plist-get parsed :events)
                   (list first "│ carry-line\n" second)))
    (should (= (plist-get first :end)
               (1+ (string-match (regexp-quote "│ carry-line\n") raw))))))

(ert-deftest majutsu-log-parse-entry-records-raw-region ()
  "Parser should record the raw entry region for streaming washing."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (first-raw (concat
                     "○ " majutsu-row-start-token "chg1" majutsu-row-field-separator "Title1"
                     majutsu-row-tail-token
                     majutsu-row-body-token
                     majutsu-row-meta-token
                     (majutsu-log-test--metadata-payload
                      compiled
                      '((id . "id-1")))
                     majutsu-row-end-token "\n"))
         (raw (concat first-raw "│ carry-line\n"
                      "○ " majutsu-row-start-token "chg2" majutsu-row-field-separator "Title2"
                      majutsu-row-tail-token
                      majutsu-row-body-token
                      majutsu-row-meta-token
                      (majutsu-log-test--metadata-payload
                       compiled
                       '((id . "id-2")))
                      majutsu-row-end-token "\n")))
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (let ((entry (majutsu-row-parse-at-point compiled)))
        (should entry)
        (should (= (plist-get entry :beg) (point-min)))
        (should (equal (buffer-substring-no-properties
                        (plist-get entry :beg)
                        (plist-get entry :end))
                       first-raw))))))

(ert-deftest majutsu-log-parse-entry-parent-ids ()
  "Parser should decode parent ids into a visible id list."
  (let* ((compiled (majutsu-log-test--relations-compiled))
         (raw (majutsu-log-test--raw-entry "child" "Child" '("parent-a" "parent-b")))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (should (equal (majutsu-row-column entry 'id) "child"))
    (should (equal (majutsu-row-column entry 'parent-ids)
                   '("parent-a" "parent-b")))))

(ert-deftest majutsu-log-rebuild-relation-indexes ()
  "Visible entries should index full canonical log ids."
  (let* ((entries
          (list (list :columns '((id . "change-child-a")
                                 (commit-id . "commit-child-a")
                                 (parent-ids . ("change-parent"))))
                (list :columns '((id . "change-child-b")
                                 (commit-id . "commit-child-b")
                                 (parent-ids . ("change-parent"))))
                (list :columns '((id . "change-parent")
                                 (commit-id . "commit-parent")
                                 (parent-ids . nil))))))
    (with-temp-buffer
      (majutsu-log--rebuild-relation-indexes entries)
      (should (equal (majutsu-row-column
                      (gethash "change-parent" majutsu-log--entry-by-id)
                      'commit-id)
                     "commit-parent"))
      (should (equal (gethash "change-parent" majutsu-log--children-by-id)
                     '("change-child-a" "change-child-b"))))))

(ert-deftest majutsu-log-postprocessor-runs-per-column-instance ()
  "Module-specific `:post' results should be stored per column instance."
  (let* ((compiled
          (majutsu-log-test--compile-columns
           '((:field description :module heading :face nil :post majutsu-log-test--post-by-module)
             (:field description :module body :face nil :post majutsu-log-test--post-by-module)
             (:field id :module metadata :face nil))))
         (raw (concat
               "○ " majutsu-row-start-token "desc"
               majutsu-row-tail-token
               majutsu-row-body-token "\n│ desc"
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "row-id")))
               majutsu-row-end-token "\n"))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (majutsu-row-insert-entry entry compiled))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "[desc]\nDESC\n"))
      (should (equal (get-text-property (point-min) 'line-prefix) "○ "))
      (goto-char (point-min))
      (forward-line)
      (should (equal (get-text-property (point) 'line-prefix) "│ ")))))

(ert-deftest majutsu-log-insert-entry-with-auxiliary-heading-fields ()
  "Real section headings include additional heading fields in order."
  (let* ((compiled (majutsu-log-test--heading-aux-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Very\nLong")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ " "│ "))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (majutsu-row-insert-entry entry compiled))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "chg Title Very\nLong 2m\n"))
      (should (equal (get-text-property (point-min) 'line-prefix) "○ "))
      (goto-char (point-min))
      (forward-line)
      (should (equal (get-text-property (point) 'line-prefix) "│ ")))))

(ert-deftest majutsu-log-insert-entry-renders-tail-on-anchor-line ()
  "Tail rendering should stay on the anchor line and use align-to spacing."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title\nMore")
                                 (author . "Very\nLong")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ " "│ "))))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
                     "chg Title Very Long 2m"))
      (let ((prefix (get-text-property (point-min) 'line-prefix)))
        (should (stringp prefix))
        (should (equal (substring-no-properties prefix) "○ ")))
      (search-forward "chg")
      (backward-char 3)
      (should (eq (get-text-property (point) 'majutsu-row-field) 'change-id))
      (should (equal (get-text-property (point) 'majutsu-row-entry-id) "id-123"))
      (search-forward "Very Long 2m")
      (let ((spacer-pos (- (point) (length "Very Long 2m") 1)))
        (should (eq (get-text-property spacer-pos 'majutsu-row-module) 'tail))
        (should (eq (get-text-property spacer-pos 'majutsu-row-decoration)
                    'tail-spacer))
        (should (equal (get-text-property spacer-pos 'display)
                       (majutsu-row-tail-spacer-display "Very Long 2m"))))
      (forward-line 1)
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
                     "More"))
      (let ((prefix (get-text-property (point) 'line-prefix)))
        (should (stringp prefix))
        (should (equal (substring-no-properties prefix) "│ "))))))

(ert-deftest majutsu-log-tail-spacer-display-uses-pixels-on-gui ()
  "Tail spacer display should use absolute pixel targets on GUI."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _display) t))
            ((symbol-function 'string-pixel-width) (lambda (_string &optional _buffer) 37))
            ((symbol-function 'window-body-width) (lambda (&optional _window pixelwise) (if pixelwise 200 80))))
    (with-temp-buffer
      (should (equal (majutsu-row-tail-spacer-display "tail" 'fake-window)
                     '(space :align-to (163)))))))

(ert-deftest majutsu-log-tail-spacer-display-uses-columns-on-terminal ()
  "Tail spacer display should use absolute columns on terminal frames."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _display) nil))
            ((symbol-function 'window-body-width) (lambda (&optional _window &rest _args) 80)))
    (with-temp-buffer
      (should (equal (majutsu-row-tail-spacer-display "tail" 'fake-window)
                     '(space :align-to 75))))))

(ert-deftest majutsu-log-refresh-tail-spacers-recomputes-display ()
  "Refreshing tail spacers should recompute their align-to display specs."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ "))))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'string-pixel-width)
                 (lambda (_string &optional _buffer) 25)))
        (majutsu-row-insert-entry entry compiled))
      (goto-char (point-min))
      (search-forward "Alice 2m")
      (let ((spacer-pos (- (point) (length "Alice 2m") 1)))
        (should (equal (get-text-property spacer-pos 'display)
                       '(space :align-to (- right (25)))))
        (setq buffer-read-only t)
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _args) 'fake-window))
                  ((symbol-function 'window-body-width)
                   (lambda (&optional _window pixelwise)
                     (if pixelwise 200 80)))
                  ((symbol-function 'window-text-pixel-size)
                   (lambda (_window _from _to
                                    &optional _x-limit _y-limit _mode-lines _ignore-line-at-end)
                     '(61 . 1))))
          (majutsu-row-refresh-tail-spacers))
        (should (equal (get-text-property spacer-pos 'display)
                       '(space :align-to (139))))))))

(ert-deftest majutsu-log-refresh-tail-spacers-uses-terminal-columns ()
  "Refreshing tail spacers should use column targets on terminal frames."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ "))))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) nil)))
        (majutsu-row-insert-entry entry compiled))
      (goto-char (point-min))
      (search-forward "Alice 2m")
      (let ((spacer-pos (- (point) (length "Alice 2m") 1)))
        (setq buffer-read-only t)
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&optional _display) nil))
                  ((symbol-function 'get-buffer-window)
                   (lambda (&rest _args) 'fake-window))
                  ((symbol-function 'window-body-width)
                   (lambda (&optional _window &rest _args) 80)))
          (majutsu-row-refresh-tail-spacers))
        (should (equal (get-text-property spacer-pos 'display)
                       '(space :align-to 71)))))))

(ert-deftest majutsu-log-refresh-tail-spacers-prefers-explicit-window ()
  "Refreshing tail spacers should use the explicitly provided window."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ ")))
         seen)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'string-pixel-width)
                 (lambda (_string &optional _buffer) 25)))
        (majutsu-row-insert-entry entry compiled))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&optional _display) t))
                ((symbol-function 'majutsu-row--tail-owner-window)
                 (lambda ()
                   (error "should not resolve tail owner when WINDOW is explicit")))
                ((symbol-function 'window-body-width)
                 (lambda (&optional window pixelwise)
                   (setq seen window)
                   (if pixelwise 200 80)))
                ((symbol-function 'window-text-pixel-size)
                 (lambda (window _from _to
                                 &optional _x-limit _y-limit _mode-lines _ignore-line-at-end)
                   (setq seen window)
                   '(61 . 1))))
        (majutsu-row-refresh-tail-spacers nil nil 'explicit-window))
      (should (eq seen 'explicit-window)))))

(ert-deftest majutsu-log-window-size-change-refreshes-resized-log-window ()
  "The buffer-local resize hook should refresh the window it receives."
  (let (refreshed)
    (with-temp-buffer
      (majutsu-log-mode)
      (cl-letf (((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer)))
                ((symbol-function 'majutsu-log--refresh-tail-window)
                 (lambda (window) (setq refreshed window))))
        (majutsu-log--after-window-size-change 'resized-log-window))
      (should (eq refreshed 'resized-log-window)))))
(ert-deftest majutsu-log-filter-buffer-substring-drops-tail-when-heading-present ()
  "Copying mixed heading+tail text should drop the tail by default."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ "))))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (let ((copied (filter-buffer-substring (line-beginning-position)
                                             (line-end-position))))
        (should (equal copied "chg Title"))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-row-module nil copied))
        (should-not (text-property-not-all 0 (length copied) 'display nil copied))
        (should-not (text-property-not-all 0 (length copied) 'line-prefix nil copied))
        (should-not (text-property-not-all 0 (length copied) 'wrap-prefix nil copied))))))

(ert-deftest majutsu-log-filter-buffer-substring-preserves-tail-only ()
  "Copying only tail text should keep the tail contents."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ "))))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Alice 2m")
      (let* ((end (point))
             (beg (- end (length "Alice 2m")))
             (copied (filter-buffer-substring beg end)))
        (should (equal copied "Alice 2m"))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-row-module nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-row-field nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-row-column nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-row-entry-id nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-row-decoration nil copied))
        (should-not (text-property-not-all 0 (length copied) 'display nil copied))
        (should-not (text-property-not-all 0 (length copied) 'line-prefix nil copied))
        (should-not (text-property-not-all 0 (length copied) 'wrap-prefix nil copied))))))

(ert-deftest majutsu-copy-section-value-copies-canonical-log-id ()
  "`majutsu-copy-section-value' should copy the full canonical log id."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "change-full"
                      :columns '((change-id . "change-full")
                                 (id . "change-full")
                                 (commit-id . "commit-full")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
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
        (majutsu-copy-section-value))
      (should (equal copied "change-full")))))

(ert-deftest majutsu-row-copy-field-copies-log-field-value-at-point ()
  "`majutsu-row-copy-field' should copy the rendered log field value at point."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (majutsu-log-commit-columns nil)
         (majutsu-log--compiled-template-cache compiled)
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
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

(ert-deftest majutsu-row-copy-module-copies-log-heading-content-without-decoration ()
  "`majutsu-row-copy-module' should copy log heading content without graph/tail decoration."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (majutsu-log-commit-columns nil)
         (majutsu-log--compiled-template-cache compiled)
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title\nMore")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ " "│ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
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
      (should (equal copied "chg Title\nMore")))))

(ert-deftest majutsu-row-copy-entry-field-copies-log-hidden-metadata ()
  "`majutsu-row-copy-entry-field' should copy hidden log canonical metadata fields."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (majutsu-log-commit-columns nil)
         (majutsu-log--compiled-template-cache compiled)
         (entry (list :id "id-123"
                      :commit-id "230dd059e1b059aefcda37d0a668f2f08f6e5a13"
                      :columns '((change-id . "chg")
                                 (id . "id-123")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m")
                                 (commit-id . "230dd059e1b059aefcda37d0a668f2f08f6e5a13"))
                      :heading-prefixes '("○ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
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
                     (should (assoc "commit-id" collection))
                     (should (equal
                              (funcall annotation-function "commit-id")
                              " 230dd059e1b059aefcda37d0a668f2f08f6e5a13")))
                   "commit-id")))
        (majutsu-row-copy-entry-field))
      (should (equal copied "230dd059e1b059aefcda37d0a668f2f08f6e5a13")))))

(ert-deftest majutsu-filesets-split-transient-value/splits-structured-args ()
  "Transient fileset helpers should keep args and filesets separate."
  (dolist (case '(((("--" "src/a.el") "--from=A" "--to=B")
                   ("--from=A" "--to=B")
                   ("src/a.el"))
                  (("--from=A" "--to=B")
                   ("--from=A" "--to=B")
                   nil)))
    (pcase-let ((`(,value ,args ,filesets) case))
      (should (equal (majutsu-filesets-split-transient-value value)
                     (list args filesets))))))

(ert-deftest majutsu-filesets-build-and-append/keeps-boundaries-explicit ()
  "Filesets should be structured for transients and separated for jj."
  (should (equal (majutsu-filesets-build-transient-value
                  '("--from=A") '("src/a.el"))
                 '(("--" "src/a.el") "--from=A")))
  (should (equal (majutsu-jj-append-filesets
                  '("--from=A" "--to=B") '("src/a.el"))
                 '("--from=A" "--to=B" "--" "src/a.el"))))

(ert-deftest majutsu-log-transient-read-revset/prefills-current-value ()
  "Log -r should prefill the current value and let empty input clear it."
  (let (seen-reader
        current-prefix-arg)
    (cl-letf (((symbol-function 'majutsu-read-revset)
               (lambda (prompt &rest keys)
                 (setq seen-reader
                       (list prompt
                             (plist-get keys :allow-empty)
                             (plist-get keys :initial-input)
                             (plist-get keys :history)
                             (plist-get keys :completion-args)))
                 "new() | mine()")))
      (should (equal (majutsu-log--transient-read-revset
                      "Revisions: " "old()" 'history)
                     "new() | mine()"))
      (should (equal seen-reader
                     '("Revisions: " t "old()" history ("log" "-r")))))))

(ert-deftest majutsu-log-filesets/stay-in-originating-buffer ()
  "Reusing log options must not copy another buffer's one-off file filter."
  (with-temp-buffer
    (majutsu-log-mode)
    (majutsu-log--set-value
     'majutsu-log-mode '("--no-graph") '("src/one.el"))
    (should (equal (majutsu-log--get-value 'majutsu-log-mode 'current)
                   '(("--no-graph") ("src/one.el"))))
    (let ((source (current-buffer)))
      (with-temp-buffer
        (cl-letf (((symbol-function 'majutsu--get-mode-buffer)
                   (lambda (&rest _) source)))
          (should (equal (majutsu-log--get-value 'majutsu-log-mode 'always)
                         '(("--no-graph") nil))))))))

(ert-deftest majutsu-log-reset/leaves-saved-defaults-alone ()
  "The real reset key clears buffer and popup, not stored defaults."
  (let ((transient-values '((majutsu-log:majutsu-log-mode "--no-graph")))
        (transient-history nil)
        (majutsu-prefix-use-buffer-arguments 'current))
    (save-window-excursion
      (with-temp-buffer
        (majutsu-log-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (insert "Log\n")))
        (setq-local majutsu-buffer-log-args '("--no-graph"))
        (setq-local majutsu-buffer-log-filesets '("src/one.el"))
        (switch-to-buffer (current-buffer))
        (cl-letf (((symbol-function 'transient-save-values)
                   (lambda () (ert-fail "Reset tried to save defaults")))
                  ((symbol-function 'majutsu-refresh-buffer) #'ignore))
          (unwind-protect
              (progn
                (call-interactively #'majutsu-log-transient)
                (execute-kbd-macro "0")
                (should-not (transient-get-value))
                (should-not majutsu-buffer-log-args)
                (should-not majutsu-buffer-log-filesets)
                (should (equal transient-values
                               '((majutsu-log:majutsu-log-mode "--no-graph")))))
            (when transient--prefix
              (execute-kbd-macro (kbd "C-g")))))))))

(ert-deftest majutsu-log-transient-read-revset/empty-input-clears ()
  "Empty log -r input should clear the ordinary revision argument."
  (let (current-prefix-arg)
    (cl-letf (((symbol-function 'majutsu-read-revset)
               (lambda (&rest _args) nil)))
      (should-not (majutsu-log--transient-read-revset
                   "Revisions: " "old()" 'history)))))

(ert-deftest majutsu-log-wash-logs-streams-and-caches-entries ()
  "Washing should transform the buffer incrementally and cache entries."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (raw (concat
               "○ " majutsu-row-start-token
               "chg1" majutsu-row-field-separator "Title1"
               majutsu-row-tail-token
               "Alice" majutsu-row-field-separator "2m ago"
               majutsu-row-body-token
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-1")))
               majutsu-row-end-token "\n"
               "○ " majutsu-row-start-token
               "chg2" majutsu-row-field-separator "Title2"
               majutsu-row-tail-token
               "Bob" majutsu-row-field-separator "1h ago"
               majutsu-row-body-token
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-2")))
               majutsu-row-end-token "\n"))
         (majutsu--default-directory "/tmp/test-repo/")
         (majutsu-log-commit-columns nil)
         (majutsu-log--compiled-template-cache compiled))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (lograph)
        (insert raw)
        (goto-char (point-min))
        (majutsu-log--wash-logs nil)
        (let ((output (buffer-string)))
          (should (string-match-p "Title1 Alice 2m" output))
          (should (string-match-p "Title2 Bob 1h" output))
          (should-not (string-match-p (regexp-quote majutsu-row-start-token) output))
          (should (= 2 (length majutsu-row-cached-entries))))))))

(ert-deftest majutsu-log-goto-parent-selects-specific-parent ()
  "Parent navigation should prompt when multiple visible parents exist."
  (let* ((compiled (majutsu-log-test--relations-compiled))
         (raw (concat (majutsu-log-test--raw-entry "child" "Merge child" '("parent-a" "parent-b"))
                      (majutsu-log-test--raw-entry "parent-a" "Parent A")
                      (majutsu-log-test--raw-entry "parent-b" "Parent B")))
         (majutsu--default-directory "/tmp/test-repo/")
         (majutsu-log-commit-columns nil)
         (majutsu-log--compiled-template-cache compiled))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (lograph)
        (insert raw)
        (goto-char (point-min))
        (majutsu-log--wash-logs nil))
      (should (majutsu--goto-log-entry "child"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (let ((annotation-function
                          (plist-get completion-extra-properties
                                     :annotation-function)))
                     (should (eq (plist-get completion-extra-properties
                                            :category)
                                 'majutsu-revision))
                     (should (equal (mapcar #'car collection)
                                    '("parent-a" "parent-b")))
                     (should (equal (funcall annotation-function "parent-b")
                                    " Parent B")))
                   "parent-b")))
        (majutsu-log-goto-parent))
      (should (equal (magit-section-value-if 'jj-commit) "parent-b")))))

(ert-deftest majutsu-log-goto-child-selects-specific-child ()
  "Child navigation should prompt when multiple visible children exist."
  (let* ((compiled (majutsu-log-test--relations-compiled))
         (raw (concat (majutsu-log-test--raw-entry "child-a" "Child A" '("parent"))
                      (majutsu-log-test--raw-entry "child-b" "Child B" '("parent"))
                      (majutsu-log-test--raw-entry "parent" "Parent")))
         (majutsu--default-directory "/tmp/test-repo/")
         (majutsu-log-commit-columns nil)
         (majutsu-log--compiled-template-cache compiled))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (lograph)
        (insert raw)
        (goto-char (point-min))
        (majutsu-log--wash-logs nil))
      (should (majutsu--goto-log-entry "parent"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (let ((annotation-function
                          (plist-get completion-extra-properties
                                     :annotation-function)))
                     (should (eq (plist-get completion-extra-properties
                                            :category)
                                 'majutsu-revision))
                     (should (equal (mapcar #'car collection)
                                    '("child-a" "child-b")))
                     (should (equal (funcall annotation-function "child-b")
                                    " Child B")))
                   "child-b")))
        (majutsu-log-goto-child))
      (should (equal (magit-section-value-if 'jj-commit) "child-b")))))

(ert-deftest majutsu-log/real-jj-reserved-bytes-cannot-retarget-rows ()
  "Descriptions, names and custom columns cannot inject actionable row framing."
  (majutsu-jj-integration-with-repo repo
    (let* ((payload (concat "Title\nbody" majutsu-row-meta-token "@"
                            majutsu-row-end-token
                            majutsu-row-start-token
                            majutsu-row-tail-token
                            majutsu-row-body-token
                            "\x1e\x1f\"\\\t\rX\n"))
           (author (concat "Author" "\u001dM@\u001dE\u001e\u001f!"))
           (custom (concat "\"Custom\\" "\u001dS\u001dT\u001dB\u001dM@\u001dE\u001e\u001f"))
           (shown-author "Author\\u001dM@\\u001dE\\u001e\\u001f!")
           (shown-custom
            (concat "\"Custom\\"
                    "\\u001dS\\u001dT\\u001dB\\u001dM@\\u001dE\\u001e\\u001f"))
           (shown-description
            (concat "Title\nbody\\u001dM@\\u001dE\\u001dS\\u001dT\\u001dB"
                    "\\u001e\\u001f\"\\\t\rX\n"))
           (process-environment
            (cons (concat "JJ_USER=" author) process-environment)))
      ;; Create the revision under JJ_USER rather than rewriting its author:
      ;; describe's author flags are version-dependent.  Keep controls inside
      ;; the name, with no NUL or newline and no trailing whitespace.
      (majutsu-jj-integration-call repo "new")
      (majutsu-jj-integration-commit repo payload)
      (let* ((id (majutsu-jj-integration-output
                  repo "log" "--no-graph" "-r" "@-" "-T" "change_id"))
             (description (majutsu-jj-integration-output
                           repo "log" "--no-graph" "-r" "@-" "-T" "description"))
             (compiled
              (majutsu-log-test--compile-columns
               `((:field change-id :module heading :face t)
                 (:field description :module heading :template [:description] :face t)
                 (:field custom :module heading
                  :template [:label "row-test" ,custom] :face t)
                 (:field author :module tail :template [:author :name] :face nil)
                 (:field long-desc :module body :face t)
                 (:field description :module metadata
                  :template [:description] :face nil)
                 (:field author :module metadata
                  :template [:author :name] :face nil)
                 (:field custom :module metadata :template ,custom :face nil))))
             (majutsu-log-commit-columns nil)
             (majutsu-log--compiled-template-cache compiled))
        (dolist (graph-args '(nil ("--no-graph")))
          (let ((output (apply #'majutsu-jj-integration-output repo
                               (append
                                majutsu-row-protocol-global-args
                                '("--color=always" "--config=colors.row-test=\"red\""
                                  "log" "-r" "@ | @-")
                                graph-args
                                (list "-T" (plist-get compiled :template))))))
            (with-temp-buffer
              (majutsu-log-mode)
              (let ((inhibit-read-only t))
                (magit-insert-section (lograph)
                  (insert output)
                  (let ((ansi-color-apply-face-function
                         #'ansi-color-apply-text-property-face))
                    (ansi-color-apply-on-region (point-min) (point-max)))
                  (goto-char (point-min))
                  (majutsu-log--wash-logs nil)))
              (should (= (length majutsu-row-cached-entries) 2))
              (should (majutsu--goto-log-entry id))
              (should (equal (majutsu-revision-at-point) id))
              (should (equal (magit-section-value-if 'jj-commit) id))
              (let ((entry (majutsu-row-entry-at-point)))
                (should (equal (majutsu-row-column entry 'description) description))
                (should (equal (majutsu-row-column entry 'author) author))
                (should (equal (majutsu-row-column entry 'custom) custom))
                (should (equal
                         (majutsu-row-column-value
                          entry (majutsu-log-test--column compiled 'author 'tail))
                         shown-author))
                (should (equal
                         (majutsu-row-column-value
                          entry (majutsu-log-test--column compiled 'description 'heading))
                         shown-description))
                (should (equal
                         (majutsu-row-render-body entry compiled)
                         (string-join (cdr (split-string
                                           (string-remove-suffix "\n" shown-description)
                                           "\n"))
                                      "\n")))
                (let ((visible (majutsu-row-column-value
                                entry (majutsu-log-test--column
                                       compiled 'custom 'heading))))
                  (should (equal visible shown-custom))
                  ;; Ordinary punctuation and text retain their native labels.
                  (should (get-text-property 0 'font-lock-face visible))
                  (should (equal (get-text-property 0 'font-lock-face visible)
                                 (get-text-property 1 'font-lock-face visible)))))
              (search-forward "Title")
              (should (equal (majutsu-revision-at-point) id))
              (should (equal (magit-section-value-if 'jj-commit) id)))))))))

(ert-deftest majutsu-log/real-jj-multiline-body-retains-merge-geometry ()
  "Physical body lines survive template transport and washing with jj's graph."
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-call repo "describe" "-m" "Left")
    (let ((left (majutsu-jj-integration-output
                 repo "log" "--no-graph" "-r" "@" "-T" "commit_id")))
      (majutsu-jj-integration-call repo "new" "root()" "-m" "Right")
      (majutsu-jj-integration-call repo "new" left "@" "-m" "Merge"))
    (let* ((body "BODY one\n\nBODY three\nBODY four")
           (compiled
            (majutsu-log-test--compile-columns
             '((:field description :module heading :face nil)
               (:field long-desc :module body
                :template "BODY one\n\nBODY three" :face nil)
               (:field blank :module body :template " \t\n " :face nil)
               (:field last :module body :template "BODY four" :face nil))))
           (majutsu-log-commit-columns nil)
           (majutsu-log--compiled-template-cache compiled)
           (id (majutsu-jj-integration-output
                repo "log" "--no-graph" "-r" "@" "-T" "change_id"))
           (raw (apply #'majutsu-jj-integration-output repo
                       (append majutsu-row-protocol-global-args
                               (list "--color=never" "log" "-r" "ancestors(@)"
                                     "-T" (plist-get compiled :template)))))
           expected-prefixes)
      ;; Read prefixes directly from jj's physical output, independently of
      ;; row parsing, so a renderer that guesses a carry prefix cannot pass.
      (with-temp-buffer
        (insert raw)
        (goto-char (point-min))
        (search-forward majutsu-row-body-token)
        (forward-line)
        (dolist (text (split-string body "\n" nil))
          (let ((beg (point)))
            (if (string-empty-p text)
                (end-of-line)
              (search-forward text (line-end-position))
              (backward-char (length text)))
            (push (buffer-substring-no-properties beg (point))
                  expected-prefixes))
          (forward-line)))
      (setq expected-prefixes (nreverse expected-prefixes))
      (should (> (length (delete-dups
                         (mapcar #'string-trim-right expected-prefixes)))
                 1))
      (with-temp-buffer
        (majutsu-log-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (lograph)
            (insert raw)
            (goto-char (point-min))
            (majutsu-log--wash-logs nil)))
        (should (majutsu--goto-log-entry id))
        (let* ((entry (majutsu-row-entry-at-point))
               (section (magit-current-section)))
          (should (equal (plist-get entry :body-prefixes) expected-prefixes))
          (should (equal (majutsu-row-render-body entry compiled) body))
          (goto-char (oref section content))
          (dolist (prefix expected-prefixes)
            (let ((display (get-text-property (point) 'line-prefix)))
              (should (equal (string-trim-right display)
                             (string-trim-right prefix)))
              (should (= (string-width display)
                         (string-width (car (plist-get entry :heading-prefixes))))))
            (forward-line))
          (let ((copied (filter-buffer-substring
                         (oref section content) (oref section end))))
            (should (equal copied (concat body "\n")))
            (should-not (text-property-not-all
                         0 (length copied) 'line-prefix nil copied))))
        (should (equal (length majutsu-row-cached-entries) 4))))))

;;; majutsu-log-test.el ends here
