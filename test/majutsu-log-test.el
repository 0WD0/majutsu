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
(require 'majutsu-section)

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
    (majutsu-row-parse-buffer compiled)))

(defun majutsu-log-test--transport-value (value)
  "Return transport encoding for test VALUE."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value) (string-join value majutsu-log--field-list-separator))
   (t (format "%s" value))))

(defun majutsu-log-test--module-payload (compiled module values)
  "Return transport payload for MODULE in COMPILED using VALUES alist."
  (string-join
   (mapcar (lambda (column)
             (majutsu-log-test--transport-value
              (alist-get (plist-get column :field) values nil nil #'eq)))
           (majutsu-row-module-columns compiled module))
   majutsu-row-field-separator))

(defun majutsu-log-test--metadata-payload (compiled values)
  "Return metadata payload for COMPILED using VALUES alist."
  (majutsu-log-test--module-payload compiled 'metadata values))

(defun majutsu-log-test--raw-entry (id title &optional parent-ids)
  "Return one raw relation-aware log entry for ID and TITLE."
  (let ((compiled (majutsu-log-test--relations-compiled)))
    (concat
     "○ " majutsu-row-start-token id majutsu-row-field-separator title
     majutsu-row-tail-token
     majutsu-row-body-token
     majutsu-row-meta-token
     (majutsu-log-test--metadata-payload
      compiled
      `((id . ,id)
        (parent-ids . ,parent-ids)))
     majutsu-row-end-token
     "\n")))

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
  (let ((spec (majutsu-row-normalize-column-spec
               (majutsu-log--row-profile) 'description)))
    (should (eq (plist-get spec :field) 'description))
    (should (eq (plist-get spec :module) 'heading))
    (should (eq (plist-get spec :face) t))
    (should (equal (plist-get spec :post) nil))))

(ert-deftest majutsu-log-explicit-default-postprocessors-keep-field-defaults ()
  "Explicit `:post :default' should retain field-specific defaults."
  (let ((spec (majutsu-row-normalize-column-spec
               (majutsu-log--row-profile)
               '(:field parent-ids :post :default))))
    (should (equal (plist-get spec :post)
                   '(majutsu-log-post-split-list-separator)))))

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

(ert-deftest majutsu-log-compile-columns-adds-required-hidden-metadata-fields ()
  "Compiling columns should preserve hidden transport fields needed by log semantics."
  (let* ((compiled (majutsu-log--compile-columns
                    '((:field description :module heading :face t))))
         (fields (mapcar (lambda (column) (plist-get column :field))
                         (majutsu-row-module-columns compiled 'metadata))))
    (should (equal fields '(id commit-id parent-ids)))))

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
               "○ " majutsu-row-start-token "chg" majutsu-row-field-separator "Title line 1\n"
               "│ Title line 2" majutsu-row-tail-token
               majutsu-row-body-token
               "body line 1" majutsu-log--field-line-separator "body line 2"
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
               majutsu-row-start-token "chg" majutsu-row-field-separator "Top\n"
               "Tail" majutsu-row-tail-token majutsu-row-body-token "payload"
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
    (should (equal (plist-get entry :short-desc) "Top\nTail"))
    (should (equal (plist-get entry :long-desc) "payload"))
    (should (equal (plist-get entry :timestamp) "1h"))
    (should (equal (plist-get entry :heading-prefixes) '("" "")))))

(ert-deftest majutsu-log-parse-entry-with-tail-module ()
  "Parser should decode tail payloads separately from body and metadata."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (raw (concat
               "○ " majutsu-row-start-token "chg" majutsu-row-field-separator "Title"
               majutsu-row-tail-token
               "Alice" majutsu-row-field-separator "2m ago"
               majutsu-row-body-token
               "body line 1" majutsu-log--field-line-separator "body line 2"
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "id-123")))
               majutsu-row-end-token "\n"))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (should (equal (plist-get entry :id) "id-123"))
    (should (equal (plist-get entry :author) "Alice"))
    (should (equal (plist-get entry :timestamp) "2m"))
    (should (equal (plist-get entry :long-desc) "body line 1\nbody line 2"))
    (should (equal (majutsu-row-render-tail entry compiled) "Alice 2m"))))

(ert-deftest majutsu-log-parse-entry-preserves-between-entry-lines ()
  "Lines between E of one entry and S of next entry stay with previous entry."
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
                     "○ " majutsu-row-start-token "chg1" majutsu-row-field-separator "Title1"
                     majutsu-row-tail-token
                     majutsu-row-body-token
                     majutsu-row-meta-token
                     (majutsu-log-test--metadata-payload
                      compiled
                      '((id . "id-1")))
                     majutsu-row-end-token "\n"
                     "│ carry-line\n"))
         (raw (concat first-raw
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
               "○ " majutsu-row-start-token "desc"
               majutsu-row-tail-token
               majutsu-row-body-token
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "row-id")))
               majutsu-row-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (equal (plist-get entry :short-desc) "desc"))
    (should (equal (plist-get entry :id) "row-id"))
    (should (equal (mapconcat #'substring-no-properties
                              (majutsu-row-render-heading-lines entry compiled)
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
               "○ " majutsu-row-start-token "desc"
               majutsu-row-tail-token
               majutsu-row-body-token "desc"
               majutsu-row-meta-token
               (majutsu-log-test--metadata-payload
                compiled
                '((id . "row-id")))
               majutsu-row-end-token "\n"))
         (entry (car (majutsu-log-test--parse-entries compiled raw))))
    (should (equal (plist-get entry :short-desc) "desc"))
    (should (equal (mapconcat #'substring-no-properties
                              (majutsu-row-render-heading-lines entry compiled)
                              "\n")
                   "○ [desc]"))
    (should (equal (majutsu-row-render-body entry compiled) "DESC"))))

(ert-deftest majutsu-log-render-heading-lines-with-auxiliary-heading-fields ()
  "Heading rendering should include additional heading fields in order."
  (let* ((compiled (majutsu-log-test--heading-aux-compiled))
         (entry (list :columns '((change-id . "chg")
                                 (description . "Title")
                                 (author . "Very\nLong")
                                 (timestamp . "2m"))
                      :heading-prefixes '("" "")))
         (heading (mapconcat #'substring-no-properties
                             (majutsu-row-render-heading-lines entry compiled)
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
      (should (eq (get-text-property (point) 'majutsu-log-field) 'change-id))
      (should (equal (get-text-property (point) 'majutsu-log-entry-id) "id-123"))
      (search-forward "Very Long 2m")
      (let ((spacer-pos (- (point) (length "Very Long 2m") 1)))
        (should (eq (get-text-property spacer-pos 'majutsu-log-module) 'tail))
        (should (eq (get-text-property spacer-pos 'majutsu-log-decoration)
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

(ert-deftest majutsu-log-mode-installs-tail-refresh-hooks ()
  "Log mode should install local refresh hooks for scale and window changes."
  (with-temp-buffer
    (majutsu-log-mode)
    (should (memq #'majutsu-log--after-text-scale-change text-scale-mode-hook))
    (should (memq #'majutsu-log--after-window-size-change window-size-change-functions))))

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
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (let ((copied (filter-buffer-substring (line-beginning-position)
                                             (line-end-position))))
        (should (equal copied "chg Title"))
        (should-not (text-property-not-all 0 (length copied) 'majutsu-log-module nil copied))
        (should-not (text-property-not-all 0 (length copied) 'display nil copied))
        (should-not (text-property-not-all 0 (length copied) 'line-prefix nil copied))
        (should-not (text-property-not-all 0 (length copied) 'wrap-prefix nil copied))))))

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
      (majutsu-row-insert-entry entry compiled)
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
        (should-not (text-property-not-all 0 (length copied) 'display nil copied))
        (should-not (text-property-not-all 0 (length copied) 'line-prefix nil copied))
        (should-not (text-property-not-all 0 (length copied) 'wrap-prefix nil copied))))))

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
      (majutsu-row-insert-entry entry compiled)
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
         (majutsu-log--compiled-template-cache compiled)
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
      (majutsu-row-insert-entry entry compiled)
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
         (majutsu-log--compiled-template-cache compiled)
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
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-log-copy-module))
      (should (equal copied "chg Title\nMore")))))

(ert-deftest majutsu-log-copy-entry-field-copies-hidden-metadata ()
  "`majutsu-log-copy-entry-field' should copy hidden canonical metadata fields."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (majutsu-log--compiled-template-cache compiled)
         (entry (list :id "id-123"
                      :commit-id "230dd059e1b059aefcda37d0a668f2f08f6e5a13"
                      :columns '((change-id . "chg")
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
      (setq-local majutsu-log--cached-entries (list entry))
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
                                   (string-match-p "commit-id" candidate))
                                 candidates)
                       (car candidates)))))
        (majutsu-log-copy-entry-field))
      (should (equal copied "230dd059e1b059aefcda37d0a668f2f08f6e5a13")))))

(ert-deftest majutsu-log-copy-commit-id-copies-hidden-hash ()
  "`majutsu-log-copy-commit-id' should copy the canonical hidden commit hash."
  (let* ((compiled (majutsu-log-test--tail-compiled))
         (majutsu-log--compiled-template-cache compiled)
         (entry (list :id "id-123"
                      :commit-id "230dd059e1b059aefcda37d0a668f2f08f6e5a13"
                      :columns '((change-id . "chg")
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
      (setq-local majutsu-log--cached-entries (list entry))
      (majutsu-row-insert-entry entry compiled)
      (goto-char (point-min))
      (search-forward "Title")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args))))
        (majutsu-log-copy-commit-id))
      (should (equal copied "230dd059e1b059aefcda37d0a668f2f08f6e5a13")))))

(ert-deftest majutsu-repository-config-id/reads-jj-config-id-file ()
  "Repository identity should use jj's secure repo config id."
  (let* ((root (file-name-as-directory (make-temp-file "majutsu-repo" t)))
         (config-id "0123456789abcdefabcd")
         (config-file (expand-file-name ".jj/repo/config-id" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory config-file) t)
          (write-region config-id nil config-file nil 'silent)
          (cl-letf (((symbol-function 'majutsu-toplevel)
                     (lambda (&optional _directory) root)))
            (should (equal (majutsu-repository-config-id) config-id))))
      (delete-directory root t))))

(ert-deftest majutsu-repository-config-id/create-delegates-to-jj ()
  "Creating repository identity should ask jj, then read config-id."
  (let* ((dir (file-name-as-directory (make-temp-file "majutsu-repo" t)))
         (file (expand-file-name "config-id" dir))
         (config-id "fedcba9876543210abcd")
         seen-command)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-repository-config-id-file)
                   (lambda (&optional _root) file))
                  ((symbol-function 'majutsu-jj-string)
                   (lambda (&rest args)
                     (setq seen-command args)
                     (write-region config-id nil file nil 'silent)
                     "/tmp/unused/config.toml")))
          (should (equal (majutsu-repository-config-id t) config-id))
          (should (equal seen-command '("config" "path" "--repo"))))
      (delete-directory dir t))))

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

(ert-deftest majutsu-transient-default-value/prefers-repository-defaults ()
  "Generic transient defaults should prefer repo-local values."
  (let ((transient-values nil)
        (config-id "0123456789abcdefabcd")
        (mode 'majutsu-test-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-repository-config-id)
                   (lambda (&optional _create) config-id)))
          (put mode 'majutsu-test-default-arguments '("default"))
          (put mode 'majutsu-test-current-arguments '("global-session"))
          (setf (alist-get (majutsu-transient-global-default-key 'majutsu-test mode)
                           transient-values)
                '("global-saved"))
          (setf (alist-get (majutsu-transient-repository-default-key 'majutsu-test mode)
                           transient-values)
                '("repo-saved"))
          (should (equal (majutsu-transient-default-value
                          'majutsu-test mode
                          'majutsu-test-current-arguments
                          'majutsu-test-default-arguments)
                         '("repo-saved")))
          (majutsu-transient-put-repository-current-value
           'majutsu-test mode '("repo-session") config-id)
          (should (equal (majutsu-transient-default-value
                          'majutsu-test mode
                          'majutsu-test-current-arguments
                          'majutsu-test-default-arguments)
                         '("repo-session"))))
      (put mode 'majutsu-test-current-repository-values nil)
      (put mode 'majutsu-test-current-arguments nil)
      (put mode 'majutsu-test-default-arguments nil))))

(ert-deftest majutsu-transient-save-repository-value/persists-under-repo-key ()
  "Repository-local transient saves should use the generic repo key."
  (let ((transient-values nil)
        (config-id "0123456789abcdefabcd")
        (mode 'majutsu-test-mode)
        saved)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-repository-config-id)
                   (lambda (&optional _create) config-id))
                  ((symbol-function 'transient-save-values)
                   (lambda () (setq saved t))))
          (majutsu-transient-save-repository-value
           'majutsu-test mode '("--revision=mine()"))
          (let ((key (majutsu-transient-repository-default-key 'majutsu-test mode)))
            (should (equal (cdr (assq key transient-values))
                           '("--revision=mine()")))
            (should (equal (majutsu-transient-repository-current-value
                            'majutsu-test mode config-id)
                           '("--revision=mine()")))))
      (put mode 'majutsu-test-current-repository-values nil))
    (should saved)))

(ert-deftest majutsu-log-transient-read-revset/prefills-current-value ()
  "Log -r should prefill the current value and let empty input clear it."
  (let (seen-reader
        current-prefix-arg)
    (cl-letf (((symbol-function 'majutsu-read-optional-revset)
               (lambda (prompt default initial-input history completion-args)
                 (setq seen-reader (list prompt default initial-input history completion-args))
                 "new() | mine()")))
      (should (equal (majutsu-log--transient-read-revset
                      "Revisions: " "old()" 'history)
                     "new() | mine()"))
      (should (equal seen-reader
                     '("Revisions: " nil "old()" history ("log" "-r")))))))

(ert-deftest majutsu-log--r-argument/uses-standard-revset-reader ()
  "The log -r infix should be a normal transient argument."
  (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
    (let ((obj (seq-find (lambda (suffix)
                           (equal (oref suffix key) "-r"))
                         (transient-suffixes 'majutsu-log-transient))))
      (should obj)
      (should (eq (oref obj reader) #'majutsu-log--transient-read-revset))
      (should (equal (oref obj argument) "--revision=")))))

(ert-deftest majutsu-log-build-args/uses-revision-argument-directly ()
  "Log -r should live in ARGS, with filesets after --."
  (let ((majutsu-log--compiled-template-cache '(:template "TPL")))
    (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
      (unwind-protect
          (progn
            (majutsu-log--set-value
             'majutsu-log-mode '("--revision=mine()" "--no-graph") '("src"))
            (should (equal (majutsu-log--build-args)
                           '("log" "--revision=mine()" "--no-graph"
                             "-T" "TPL" "--" "src"))))
        (majutsu-log--set-value 'majutsu-log-mode nil nil)))))

(ert-deftest majutsu-log-transient-read-revset/empty-input-clears ()
  "Empty log -r input should clear the ordinary revision argument."
  (let (current-prefix-arg)
    (cl-letf (((symbol-function 'majutsu-read-optional-revset)
               (lambda (&rest _args) nil)))
      (should-not (majutsu-log--transient-read-revset
                   "Revisions: " "old()" 'history)))))

(ert-deftest majutsu-log-repo-default-action/is-available ()
  "The log transient should expose generic repository-local defaults."
  (let ((suffix (transient-get-suffix 'majutsu-log-transient "W")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'majutsu-transient-save-repository-defaults))))

(ert-deftest majutsu-log-transient/does-not-need-clear-revisions-action ()
  "Log -r clears through empty input, so there is no separate R action."
  (should-not (ignore-errors
                (transient-get-suffix 'majutsu-log-transient "R"))))

(ert-deftest majutsu-log-copy-transient-has-copy-actions ()
  "Log copy transient should expose visible and hidden-field copy commands."
  (should (transient-get-suffix 'majutsu-log-copy-transient "s"))
  (should (transient-get-suffix 'majutsu-log-copy-transient "f"))
  (should (transient-get-suffix 'majutsu-log-copy-transient "F"))
  (should (transient-get-suffix 'majutsu-log-copy-transient "h"))
  (should (transient-get-suffix 'majutsu-log-copy-transient "m")))

(ert-deftest majutsu-dispatch-exposes-log-copy-transient ()
  "Dispatcher should expose the log copy transient entry."
  (should (transient-get-suffix 'majutsu-dispatch "w"))
  (should-not (lookup-key majutsu-log-mode-map (kbd "w"))))

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

;;; majutsu-log-test.el ends here
