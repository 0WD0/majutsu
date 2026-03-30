;;; majutsu-log-test.el --- Tests for majutsu log protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for marker-based sequential log parsing/render metadata.

;;; Code:

(require 'ert)
(require 'majutsu-log)

(defun majutsu-log-test--base-compiled ()
  "Return a compact compiled layout for parser tests."
  (majutsu-log--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field long-desc :module body :face t)
     (:field id :module metadata :face nil)
     (:field timestamp :module metadata :face nil)
     (:field flags :module metadata :face nil))))

(defun majutsu-log-test--heading-aux-compiled ()
  "Return a layout with auxiliary heading fields."
  (majutsu-log--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field author :module heading :face nil)
     (:field timestamp :module heading :face nil)
     (:field id :module metadata :face nil))))

(defun majutsu-log-test--tail-compiled ()
  "Return a layout with auxiliary tail fields."
  (majutsu-log--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field author :module tail :face nil)
     (:field timestamp :module tail :face nil)
     (:field long-desc :module body :face t)
     (:field id :module metadata :face nil))))

(defun majutsu-log-test--relations-compiled ()
  "Return a layout that includes relation metadata."
  (majutsu-log--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field id :module metadata :face nil)
     (:field parent-ids :module metadata :face nil))))

(defun majutsu-log-test--parse-entries (compiled raw)
  "Parse RAW entries with COMPILED layout and return parsed entries."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (majutsu-log--parse-entries-in-buffer compiled)))

(defun majutsu-log-test--raw-entry (id title &optional parent-ids)
  "Return one raw relation-aware log entry for ID and TITLE."
  (concat
   "○ " majutsu-log--entry-start-token id majutsu-log--field-separator title
   majutsu-log--entry-tail-token
   majutsu-log--entry-body-token
   majutsu-log--entry-meta-token id
   (when parent-ids
     (concat majutsu-log--field-separator
             (string-join parent-ids majutsu-log--field-list-separator)))
   majutsu-log--entry-end-token
   "\n"))

(defun majutsu-log-test--post-wrap (value _ctx)
  "Wrap VALUE in square brackets for postprocessor tests."
  (format "[%s]" value))

(defun majutsu-log-test--post-by-module (value ctx)
  "Return a module-specific projection of VALUE using CTX."
  (pcase (plist-get ctx :module)
    ('heading (format "[%s]" value))
    ('body (upcase value))
    (_ value)))

(ert-deftest majutsu-log-long-desc-template-uses-line-separator ()
  "Default long-desc template should encode line breaks with \x1f."
  (should (equal majutsu-log-template-long-desc
                 [:description :lines :skip 1 :join "\x1f"])))

(ert-deftest majutsu-log-canonical-log-id-template-uses-bound-self ()
  "Canonical log id helper should compile against the current Commit self."
  (should (equal (majutsu-template-compile majutsu-log-template-id 'Commit)
                 "if((self.hidden() || self.divergent()), self.commit_id().shortest(8), self.change_id().shortest(8))")))

(ert-deftest majutsu-log-parent-ids-template-composes-map-and-join ()
  "Parent ids template should map canonical ids before joining them."
  (should (equal (majutsu-template-compile majutsu-log-template-parent-ids 'Commit)
                 "self.parents().map(|p| if((p.hidden() || p.divergent()), p.commit_id().shortest(8), p.change_id().shortest(8))).join(\"\\x1C\")")))

(ert-deftest majutsu-log-default-column-schema-contains-module-and-face ()
  "Normalized column specs should include module/face/post metadata."
  (let ((spec (majutsu-log--normalize-column-spec 'description)))
    (should (eq (plist-get spec :field) 'description))
    (should (eq (plist-get spec :module) 'heading))
    (should (eq (plist-get spec :face) t))
    (should (equal (plist-get spec :post)
                   (majutsu-log--default-postprocessors-for-field 'description)))))

(ert-deftest majutsu-log-explicit-default-postprocessors-keep-field-defaults ()
  "Explicit `:post :default' should retain field-specific defaults."
  (let ((spec (majutsu-log--normalize-column-spec '(:field parent-ids :post :default))))
    (should (equal (plist-get spec :post)
                   (majutsu-log--default-postprocessors-for-field 'parent-ids)))))

(ert-deftest majutsu-log-compile-columns-emits-sequential-markers ()
  "Compiled log template should include S/T/B/M/E markers in order."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (tpl (prin1-to-string (plist-get compiled :template)))
         (s-pos (string-match (regexp-quote "\\x1dS") tpl))
         (t-pos (string-match (regexp-quote "\\x1dT") tpl))
         (b-pos (string-match (regexp-quote "\\x1dB") tpl))
         (m-pos (string-match (regexp-quote "\\x1dM") tpl))
         (e-pos (string-match (regexp-quote "\\x1dE") tpl)))
    (should s-pos)
    (should t-pos)
    (should b-pos)
    (should m-pos)
    (should e-pos)
    (should (< s-pos t-pos))
    (should (< t-pos b-pos))
    (should (< b-pos m-pos))
    (should (< m-pos e-pos))))

(ert-deftest majutsu-log-post-decode-line-separator-restores-faces ()
  "\x1f decoding should keep surrounding text properties."
  (let* ((sep (aref majutsu-log--field-line-separator 0))
         (encoded (concat (propertize "A" 'font-lock-face 'error)
                          (string sep)
                          (propertize "B" 'font-lock-face 'warning)))
         (decoded (majutsu-log-post-decode-line-separator encoded)))
    (should (equal decoded "A\nB"))
    (should (eq (get-text-property 0 'font-lock-face decoded) 'error))
    (should (eq (get-text-property 2 'font-lock-face decoded) 'warning))))

(ert-deftest majutsu-log-parse-entry-with-graph-and-multiline-heading ()
  "Parser should handle graph prefixes and multiline heading payload."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               "○ " majutsu-log--entry-start-token "chg" majutsu-log--field-separator "Title line 1\n"
               "│ Title line 2" majutsu-log--entry-tail-token
               majutsu-log--entry-body-token
               "body line 1" majutsu-log--field-line-separator "body line 2"
               majutsu-log--entry-meta-token
               "id-123" majutsu-log--field-separator "2m ago" majutsu-log--field-separator "@ immutable"
               majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (equal (plist-get entry :id) "id-123"))
    (should (equal (plist-get entry :change-id) "chg"))
    (should (equal (plist-get entry :short-desc) "Title line 1\nTitle line 2"))
    (should (equal (plist-get entry :long-desc) "body line 1\nbody line 2"))
    (should (equal (plist-get entry :timestamp) "2m"))
    (should (plist-get entry :current_working_copy))
    (should (plist-get entry :immutable))
    (should (= (plist-get entry :indent) 2))
    (should (equal (plist-get entry :heading-prefixes) '("○ " "│ ")))))

(ert-deftest majutsu-log-parse-entry-without-graph ()
  "Parser should also support --no-graph output (indent = 0)."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               majutsu-log--entry-start-token "chg" majutsu-log--field-separator "Top\n"
               "Tail" majutsu-log--entry-tail-token majutsu-log--entry-body-token "payload"
               majutsu-log--entry-meta-token "id-999" majutsu-log--field-separator "1h ago"
               majutsu-log--field-separator "" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (= 0 (plist-get entry :indent)))
    (should (equal (plist-get entry :short-desc) "Top\nTail"))
    (should (equal (plist-get entry :long-desc) "payload"))
    (should (equal (plist-get entry :timestamp) "1h"))
    (should (equal (plist-get entry :heading-prefixes) '("" "")))))

(ert-deftest majutsu-log-parse-entry-with-tail-module ()
  "Parser should decode tail payloads separately from body and metadata."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (raw (concat
               "○ " majutsu-log--entry-start-token "chg" majutsu-log--field-separator "Title"
               majutsu-log--entry-tail-token
               "Alice" majutsu-log--field-separator "2m ago"
               majutsu-log--entry-body-token
               "body line 1" majutsu-log--field-line-separator "body line 2"
               majutsu-log--entry-meta-token
               "id-123"
               majutsu-log--entry-end-token "\n"))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (should (equal (plist-get entry :id) "id-123"))
    (should (equal (plist-get entry :author) "Alice"))
    (should (equal (plist-get entry :timestamp) "2m"))
    (should (equal (plist-get entry :long-desc) "body line 1\nbody line 2"))
    (should (equal (majutsu-log--render-tail entry compiled) "Alice 2m"))))

(ert-deftest majutsu-log-parse-entry-preserves-between-entry-lines ()
  "Lines between E of one entry and S of next entry stay with previous entry."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               "○ " majutsu-log--entry-start-token "chg1" majutsu-log--field-separator "Title1"
               majutsu-log--entry-tail-token
               majutsu-log--entry-body-token
               majutsu-log--entry-meta-token "id-1" majutsu-log--field-separator ""
               majutsu-log--field-separator "" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"
               "│ carry-line\n"
               "○ " majutsu-log--entry-start-token "chg2" majutsu-log--field-separator "Title2"
               majutsu-log--entry-tail-token
               majutsu-log--entry-body-token
               majutsu-log--entry-meta-token "id-2" majutsu-log--field-separator ""
               majutsu-log--field-separator "" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (first (nth 0 entries))
         (second (nth 1 entries)))
    (should (= 2 (length entries)))
    (should (equal (plist-get first :id) "id-1"))
    (should (equal (plist-get second :id) "id-2"))
    (should (equal (plist-get first :suffix-lines) '("│ carry-line")))
    (should (null (plist-get second :suffix-lines)))))

(ert-deftest majutsu-log-parse-entry-records-raw-region ()
  "Parser should record the raw entry region for streaming washing."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (first-raw (concat
                     "○ " majutsu-log--entry-start-token "chg1" majutsu-log--field-separator "Title1"
                     majutsu-log--entry-tail-token
                     majutsu-log--entry-body-token
                     majutsu-log--entry-meta-token "id-1" majutsu-log--field-separator ""
                     majutsu-log--field-separator "" majutsu-log--field-separator ""
                     majutsu-log--entry-end-token "\n"
                     "│ carry-line\n"))
         (raw (concat first-raw
                      "○ " majutsu-log--entry-start-token "chg2" majutsu-log--field-separator "Title2"
                      majutsu-log--entry-tail-token
                      majutsu-log--entry-body-token
                      majutsu-log--entry-meta-token "id-2" majutsu-log--field-separator ""
                      majutsu-log--field-separator "" majutsu-log--field-separator ""
                      majutsu-log--entry-end-token "\n")))
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (let ((entry (majutsu-log--parse-entry-at-point compiled)))
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
    (should (equal (plist-get entry :id) "child"))
    (should (equal (plist-get entry :parent-ids) '("parent-a" "parent-b")))))

(ert-deftest majutsu-log-rebuild-relation-indexes ()
  "Visible entries should produce parent and child lookup indexes."
  (let* ((entries (list (list :id "child-a" :parent-ids '("parent"))
                        (list :id "child-b" :parent-ids '("parent"))
                        (list :id "parent" :parent-ids nil))))
    (with-temp-buffer
      (setq-local majutsu-log--cached-entries entries)
      (majutsu-log--rebuild-relation-indexes entries)
      (should (equal (plist-get (gethash "parent" majutsu-log--entry-by-id) :id)
                     "parent"))
      (should (equal (gethash "parent" majutsu-log--children-by-id)
                     '("child-a" "child-b"))))))

(ert-deftest majutsu-log-postprocessor-runs-per-field ()
  "Field-level :post handlers should run after parsing."
  (let* ((compiled
          (majutsu-log--compile-columns
           `((:field description :module heading :face t :post majutsu-log-test--post-wrap)
             (:field id :module metadata :face nil))))
         (raw (concat
               "○ " majutsu-log--entry-start-token "desc"
               majutsu-log--entry-tail-token
               majutsu-log--entry-body-token
               majutsu-log--entry-meta-token "row-id" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (equal (plist-get entry :short-desc) "desc"))
    (should (equal (plist-get entry :id) "row-id"))
    (should (equal (mapconcat #'substring-no-properties
                              (majutsu-log--render-heading-lines entry compiled)
                              "\n")
                   "○ [desc]"))))

(ert-deftest majutsu-log-postprocessor-runs-per-column-instance ()
  "Module-specific `:post' results should be stored per column instance."
  (let* ((compiled
          (majutsu-log--compile-columns
           '((:field description :module heading :face nil :post majutsu-log-test--post-by-module)
             (:field description :module body :face nil :post majutsu-log-test--post-by-module)
             (:field id :module metadata :face nil))))
         (raw (concat
               "○ " majutsu-log--entry-start-token "desc"
               majutsu-log--entry-tail-token
               majutsu-log--entry-body-token "desc"
               majutsu-log--entry-meta-token "row-id" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (should (equal (plist-get entry :short-desc) "desc"))
    (should (equal (mapconcat #'substring-no-properties
                              (majutsu-log--render-heading-lines entry compiled)
                              "\n")
                   "○ [desc]"))
    (should (equal (majutsu-log--render-body entry compiled) "DESC"))))

(ert-deftest majutsu-log-render-heading-lines-with-auxiliary-heading-fields ()
  "Heading rendering should include additional heading fields in order."
  (let* ((compiled (majutsu-log-test--heading-aux-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (description . "Title")
                                 (author . "Very\nLong")
                                 (timestamp . "2m"))
                      :heading-prefixes '("" "")))
         (heading (mapconcat #'substring-no-properties
                             (majutsu-log--render-heading-lines entry compiled)
                             "\n")))
    (should (equal heading "chg Title Very\nLong 2m"))))

(ert-deftest majutsu-log-insert-entry-renders-tail-on-anchor-line ()
  "Tail rendering should stay on the anchor line and use align-to spacing."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (description . "Title\nMore")
                                 (author . "Very\nLong")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ " "│ "))))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (majutsu-log--insert-entry entry compiled)
      (goto-char (point-min))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
                     "○ chg Title Very Long 2m"))
      (should (eq (get-text-property (point-min) 'majutsu-log-decoration)
                  'graph-prefix))
      (should (equal (get-text-property (point-min) 'majutsu-log-entry-id)
                     "id-123"))
      (search-forward "chg")
      (backward-char 3)
      (should (eq (get-text-property (point) 'majutsu-log-field) 'change-id))
      (should (equal (get-text-property (point) 'majutsu-log-entry-id) "id-123"))
      (search-forward "Very Long 2m")
      (let ((spacer-pos (- (point) (length "Very Long 2m") 1)))
        (should (eq (get-text-property spacer-pos 'majutsu-log-module) 'tail))
        (should (eq (get-text-property spacer-pos 'majutsu-log-decoration)
                    'tail-spacer))
        (should (eq (car (get-text-property spacer-pos 'display)) 'space)))
      (forward-line 1)
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
                     "│ More"))
      (should (eq (get-text-property (point) 'majutsu-log-decoration)
                  'graph-carry)))))

(ert-deftest majutsu-log-filter-buffer-substring-drops-tail-when-heading-present ()
  "Copying mixed heading+tail text should drop the tail by default."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ "))))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (majutsu-log--insert-entry entry compiled)
      (goto-char (point-min))
      (let ((copied (filter-buffer-substring (line-beginning-position)
                                             (line-end-position))))
        (should (equal copied "○ chg Title"))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-module nil copied))
        (should-not (text-property-not-all 0 (length copied) 'display nil copied))))))

(ert-deftest majutsu-log-filter-buffer-substring-preserves-tail-only ()
  "Copying only tail text should keep the tail contents."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ "))))
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (majutsu-log--insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Alice 2m")
      (let* ((end (point))
             (beg (- end (length "Alice 2m")))
             (copied (filter-buffer-substring beg end)))
        (should (equal copied "Alice 2m"))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-module nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-field nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-column nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-entry-id nil copied))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-decoration nil copied))
        (should-not (text-property-not-all 0 (length copied) 'display nil copied))))))

(ert-deftest majutsu-copy-section-value-copies-current-commit-id ()
  "`majutsu-copy-section-value' should copy the current commit section id."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (setq-local majutsu-log--cached-entries (list entry))
      (majutsu-log--insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-copy-section-value))
      (should (equal copied "id-123")))))

(ert-deftest majutsu-log-copy-field-copies-field-value-at-point ()
  "`majutsu-log-copy-field' should copy the rendered field value at point."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (description . "Title")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (setq-local majutsu-log--cached-entries (list entry))
      (majutsu-log--insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Alice")
      (backward-char 5)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-log-copy-field))
      (should (equal copied "Alice")))))

(ert-deftest majutsu-log-copy-module-copies-heading-content-without-decoration ()
  "`majutsu-log-copy-module' should copy heading content without graph/tail decoration."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (entry (list :id "id-123"
                      :columns '((change-id . "chg")
                                 (description . "Title\nMore")
                                 (author . "Alice")
                                 (timestamp . "2m"))
                      :heading-prefixes '("○ " "│ ")))
         copied)
    (with-temp-buffer
      (require 'magit-section)
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (setq-local majutsu-log--cached-entries (list entry))
      (majutsu-log--insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-log-copy-module))
      (should (equal copied "chg Title\nMore")))))

(ert-deftest majutsu-log-copy-transient-has-copy-actions ()
  "Log copy transient should expose section/field/module copy commands."
  (should (transient-get-suffix 'majutsu-log-copy-transient "s"))
  (should (transient-get-suffix 'majutsu-log-copy-transient "f"))
  (should (transient-get-suffix 'majutsu-log-copy-transient "m")))

(ert-deftest majutsu-dispatch-exposes-log-copy-transient ()
  "Dispatcher should expose the log copy transient entry."
  (should (transient-get-suffix 'majutsu-dispatch "w"))
  (should-not (lookup-key majutsu-log-mode-map (kbd "w"))))

(ert-deftest majutsu-log-wash-logs-streams-and-caches-entries ()
  "Washing should transform the buffer incrementally and cache entries."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (raw (concat
               "○ " majutsu-log--entry-start-token
               "chg1" majutsu-log--field-separator "Title1"
               majutsu-log--entry-tail-token
               "Alice" majutsu-log--field-separator "2m ago"
               majutsu-log--entry-body-token
               majutsu-log--entry-meta-token "id-1" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"
               "○ " majutsu-log--entry-start-token
               "chg2" majutsu-log--field-separator "Title2"
               majutsu-log--entry-tail-token
               "Bob" majutsu-log--field-separator "1h ago"
               majutsu-log--entry-body-token
               majutsu-log--entry-meta-token "id-2" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (majutsu--default-directory "/tmp/test-repo/")
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
          (should-not (string-match-p (regexp-quote majutsu-log--entry-start-token) output))
          (should (= 2 (length majutsu-log--cached-entries))))))))

(ert-deftest majutsu-log-goto-parent-selects-specific-parent ()
  "Parent navigation should prompt when multiple visible parents exist."
  (let* ((compiled (majutsu-log-test--relations-compiled))
         (raw (concat (majutsu-log-test--raw-entry "child" "Merge child" '("parent-a" "parent-b"))
                      (majutsu-log-test--raw-entry "parent-a" "Parent A")
                      (majutsu-log-test--raw-entry "parent-b" "Parent B")))
         (majutsu--default-directory "/tmp/test-repo/")
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
                 (lambda (_prompt candidates &rest _)
                   (seq-find (lambda (candidate)
                               (string-match-p "parent-b" candidate))
                             candidates))))
        (majutsu-log-goto-parent))
      (should (equal (magit-section-value-if 'jj-commit) "parent-b")))))

(ert-deftest majutsu-log-goto-child-selects-specific-child ()
  "Child navigation should prompt when multiple visible children exist."
  (let* ((compiled (majutsu-log-test--relations-compiled))
         (raw (concat (majutsu-log-test--raw-entry "child-a" "Child A" '("parent"))
                      (majutsu-log-test--raw-entry "child-b" "Child B" '("parent"))
                      (majutsu-log-test--raw-entry "parent" "Parent")))
         (majutsu--default-directory "/tmp/test-repo/")
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
                 (lambda (_prompt candidates &rest _)
                   (seq-find (lambda (candidate)
                               (string-match-p "child-b" candidate))
                             candidates))))
        (majutsu-log-goto-child))
      (should (equal (magit-section-value-if 'jj-commit) "child-b")))))

(ert-deftest majutsu-log-apply-face-policy-modes ()
  "Face policy should support preserve, strip, and override."
  (let* ((raw (propertize "x" 'font-lock-face 'error))
         (preserve (majutsu-log--apply-face-policy raw t))
         (strip (majutsu-log--apply-face-policy raw nil))
         (override (majutsu-log--apply-face-policy raw 'warning)))
    (should (eq (get-text-property 0 'font-lock-face preserve) 'error))
    (should-not (get-text-property 0 'font-lock-face strip))
    (should (eq (get-text-property 0 'font-lock-face override) 'warning))))

;;; majutsu-log-test.el ends here
