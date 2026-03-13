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
     (:field timestamp :module right-margin :face t)
     (:field id :module metadata :face nil)
     (:field flags :module metadata :face nil))))

(defun majutsu-log-test--margin-compiled ()
  "Return a layout with explicit fixed-width right-margin columns."
  (majutsu-log--compile-columns
   '((:field change-id :module heading :face t)
     (:field description :module heading :face t)
     (:field author :module right-margin :face nil :width 8 :align left)
     (:field timestamp :module right-margin :face nil :width 6 :align right)
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
   majutsu-log--entry-body-token
   majutsu-log--entry-right-token
   majutsu-log--entry-meta-token id
   (when parent-ids
     (concat majutsu-log--field-separator
             (string-join parent-ids majutsu-log--field-list-separator)))
   majutsu-log--entry-end-token
   "\n"))

(defun majutsu-log-test--post-wrap (value _ctx)
  "Wrap VALUE in square brackets for postprocessor tests."
  (format "[%s]" value))

(ert-deftest majutsu-log-long-desc-template-uses-line-separator ()
  "Default long-desc template should encode line breaks with \x1f."
  (should (equal majutsu-log-template-long-desc
                 [:description :lines :skip 1 :join "\x1f"])))

(ert-deftest majutsu-log-default-column-schema-contains-module-and-face ()
  "Normalized column specs should include module/face/post metadata."
  (let ((spec (majutsu-log--normalize-column-spec 'description)))
    (should (eq (plist-get spec :field) 'description))
    (should (eq (plist-get spec :module) 'heading))
    (should (eq (plist-get spec :face) t))
    (should (equal (plist-get spec :post)
                   majutsu-log--default-column-postprocessors))))

(ert-deftest majutsu-log-compile-columns-emits-sequential-markers ()
  "Compiled log template should include S/B/R/M/E markers in order."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (tpl (prin1-to-string (plist-get compiled :template)))
         (s-pos (string-match (regexp-quote "\\x1dS") tpl))
         (b-pos (string-match (regexp-quote "\\x1dB") tpl))
         (r-pos (string-match (regexp-quote "\\x1dR") tpl))
         (m-pos (string-match (regexp-quote "\\x1dM") tpl))
         (e-pos (string-match (regexp-quote "\\x1dE") tpl)))
    (should s-pos)
    (should b-pos)
    (should r-pos)
    (should m-pos)
    (should e-pos)
    (should (< s-pos b-pos))
    (should (< b-pos r-pos))
    (should (< r-pos m-pos))
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
               "│ Title line 2" majutsu-log--entry-body-token
               "body line 1" majutsu-log--field-line-separator "body line 2"
               majutsu-log--entry-right-token "2m ago"
               majutsu-log--entry-meta-token "id-123" majutsu-log--field-separator "@ immutable"
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
               "Tail" majutsu-log--entry-body-token "payload"
               majutsu-log--entry-right-token "1h ago"
               majutsu-log--entry-meta-token "id-999" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (= 0 (plist-get entry :indent)))
    (should (equal (plist-get entry :short-desc) "Top\nTail"))
    (should (equal (plist-get entry :long-desc) "payload"))
    (should (equal (plist-get entry :timestamp) "1h"))
    (should (equal (plist-get entry :heading-prefixes) '("" "")))))

(ert-deftest majutsu-log-parse-entry-preserves-between-entry-lines ()
  "Lines between E of one entry and S of next entry stay with previous entry."
  (let* ((compiled (majutsu-log-test--base-compiled))
         (raw (concat
               "○ " majutsu-log--entry-start-token "chg1" majutsu-log--field-separator "Title1"
               majutsu-log--entry-body-token
               majutsu-log--entry-right-token
               majutsu-log--entry-meta-token "id-1" majutsu-log--field-separator ""
               majutsu-log--entry-end-token "\n"
               "│ carry-line\n"
               "○ " majutsu-log--entry-start-token "chg2" majutsu-log--field-separator "Title2"
               majutsu-log--entry-body-token
               majutsu-log--entry-right-token
               majutsu-log--entry-meta-token "id-2" majutsu-log--field-separator ""
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
                     majutsu-log--entry-body-token
                     majutsu-log--entry-right-token
                     majutsu-log--entry-meta-token "id-1" majutsu-log--field-separator ""
                     majutsu-log--entry-end-token "\n"
                     "│ carry-line\n"))
         (raw (concat first-raw
                      "○ " majutsu-log--entry-start-token "chg2" majutsu-log--field-separator "Title2"
                      majutsu-log--entry-body-token
                      majutsu-log--entry-right-token
                      majutsu-log--entry-meta-token "id-2" majutsu-log--field-separator ""
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
               majutsu-log--entry-body-token
               majutsu-log--entry-right-token
               majutsu-log--entry-meta-token "row-id"
               majutsu-log--entry-end-token "\n"))
         (entries (majutsu-log-test--parse-entries compiled raw))
         (entry (car entries)))
    (should (= 1 (length entries)))
    (should (equal (plist-get entry :short-desc) "[desc]"))
    (should (equal (plist-get entry :id) "row-id"))))

(ert-deftest majutsu-log-render-right-margin-uses-fixed-widths ()
  "Right margin should use configured fixed widths without scanning entries."
  (let* ((compiled (majutsu-log-test--margin-compiled))
         (entry (list :columns '((author . "VeryLongAuthor")
                                 (timestamp . "2m"))))
         (margin (substring-no-properties
                  (majutsu-log--render-right-margin entry compiled))))
    (should (= (string-width margin)
               (majutsu-log--right-margin-total-width compiled)))
    (should-not (string-match-p "VeryLongAuthor" margin))
    (should (string-suffix-p "    2m" margin))))

(ert-deftest majutsu-log-wash-logs-streams-and-caches-entries ()
  "Washing should transform the buffer incrementally and cache entries."
  (let* ((compiled (majutsu-log-test--margin-compiled))
         (raw (concat
               "○ " majutsu-log--entry-start-token "chg1" majutsu-log--field-separator "Title1"
               majutsu-log--entry-body-token
               majutsu-log--entry-right-token "Alice" majutsu-log--field-separator "2m ago"
               majutsu-log--entry-meta-token "id-1"
               majutsu-log--entry-end-token "\n"
               "○ " majutsu-log--entry-start-token "chg2" majutsu-log--field-separator "Title2"
               majutsu-log--entry-body-token
               majutsu-log--entry-right-token "Bob" majutsu-log--field-separator "1h ago"
               majutsu-log--entry-meta-token "id-2"
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
          (should (string-match-p "Title1" output))
          (should (string-match-p "Title2" output))
          (should-not (string-match-p (regexp-quote majutsu-log--entry-start-token) output))
          (should (= 2 (length majutsu-log--cached-entries)))
          (should (= (majutsu-log--right-margin-total-width compiled)
                     majutsu-log--right-margin-width)))))))

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
