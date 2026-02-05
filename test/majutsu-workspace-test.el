;;; majutsu-workspace-test.el --- Tests for majutsu workspace helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for parsing and small helpers in majutsu-workspace.el.

;;; Code:

(require 'ert)
(require 'majutsu-workspace)

(ert-deftest majutsu-workspace-parse-list-output/basic ()
  "Parse structured `jj workspace list -T ...` output."
  (let* ((sep "\x1e")
         (output (concat
                  "@" sep "default" sep "wnurqwps" sep "6acd46b7" sep "Main wc" "\n"
                  ""  sep "w2"      sep "lvolzxkz" sep "32e07e11" sep "" "\n"))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 2))
    (should (equal (plist-get (nth 0 entries) :name) "default"))
    (should (equal (plist-get (nth 0 entries) :current) t))
    (should (equal (plist-get (nth 0 entries) :change-id) "wnurqwps"))
    (should (equal (plist-get (nth 0 entries) :commit-id) "6acd46b7"))
    (should (equal (plist-get (nth 0 entries) :desc) "Main wc"))
    (should (equal (plist-get (nth 1 entries) :name) "w2"))
    (should-not (plist-get (nth 1 entries) :current))
    (should (equal (plist-get (nth 1 entries) :desc) ""))))

(ert-deftest majutsu-workspace-visit/binds-default-directory ()
  "Ensure visiting another workspace updates buffer context."
  (let ((new-dir (make-temp-file "majutsu-test-" t))
        seen-default
        seen-root)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'majutsu-refresh)
                     (lambda ()
                       (setq seen-default default-directory)
                       (setq seen-root majutsu--default-directory))))
            (let ((default-directory "/tmp/"))
              (majutsu-workspace-visit new-dir)))
          (should (equal seen-default (file-name-as-directory (expand-file-name new-dir))))
          (should (equal seen-root (file-name-as-directory (expand-file-name new-dir)))))
      (delete-directory new-dir t))))

;;; Wash tests

(ert-deftest majutsu-workspace-line-regexp-matches-current-workspace ()
  "Test that the line regexp correctly matches current workspace line."
  (let* ((sep (string 30))
         (line (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main workspace\n")))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (should (looking-at majutsu-workspace--line-regexp))
      (should (equal (match-string 1) "@"))
      (should (equal (match-string 2) "default"))
      (should (equal (match-string 3) "wqps1234"))
      (should (equal (match-string 4) "6acd46b7"))
      (should (equal (match-string 5) "Main workspace")))))

(ert-deftest majutsu-workspace-line-regexp-matches-other-workspace ()
  "Test that the line regexp correctly matches non-current workspace line."
  (let* ((sep (string 30))
         (line (concat sep "feature-x" sep "lvolzxkz" sep "32e07e11" sep "Add feature\n")))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (should (looking-at majutsu-workspace--line-regexp))
      (should (null (match-string 1)))  ; No current marker
      (should (equal (match-string 2) "feature-x"))
      (should (equal (match-string 3) "lvolzxkz"))
      (should (equal (match-string 4) "32e07e11"))
      (should (equal (match-string 5) "Add feature")))))

(ert-deftest majutsu-workspace-line-regexp-matches-empty-description ()
  "Test that the line regexp handles empty descriptions."
  (let* ((sep (string 30))
         (line (concat sep "test-ws" sep "abcd1234" sep "5678efab" sep "\n")))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (should (looking-at majutsu-workspace--line-regexp))
      (should (equal (match-string 2) "test-ws"))
      (should (equal (match-string 5) "")))))  ; Empty description

(ert-deftest majutsu-workspace-wash-entry-transforms-line ()
  "Test that wash-entry transforms a single line into a magit section."
  (let* ((sep (string 30))
         (line (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main workspace\n"))
         (majutsu--default-directory "/tmp/test-repo/"))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)  ; Allow modifications during wash
      ;; Wrap in magit-insert-section as in actual use
      (magit-insert-section (jj-workspace)
        (insert line)
        (goto-char (point-min))
        ;; Wash the entry
        (let ((result (majutsu-workspace--wash-entry 8)))
          (should result)  ; Should return t on success
          ;; The original line should be replaced
          (should-not (string-match-p sep (buffer-string)))
          ;; Should contain the workspace name
          (should (string-match-p "default" (buffer-string)))
          ;; Should contain the change ID
          (should (string-match-p "wqps1234" (buffer-string))))))))

(ert-deftest majutsu-workspace-wash-entry-returns-nil-on-no-match ()
  "Test that wash-entry returns nil when there's no matching line."
  (let ((line "invalid line without proper format\n"))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (let ((result (majutsu-workspace--wash-entry 8)))
        (should (null result))  ; Should return nil
        ;; Buffer should be unchanged
        (should (equal (buffer-string) line))))))

(ert-deftest majutsu-workspace-wash-list-transforms-buffer ()
  "Test that wash-list transforms the entire buffer into sections."
  (let* ((sep (string 30))
         (lines (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main\n"
                        sep "feature" sep "lvolzxkz" sep "32e07e11" sep "Feature work\n"))
         (majutsu--default-directory "/tmp/test-repo/"))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)  ; Allow modifications during wash
      ;; Wrap in magit-insert-section as in actual use
      (magit-insert-section (workspaces)
        (insert lines)
        (goto-char (point-min))
        ;; Wash the list
        (majutsu-workspace--wash-list t nil)
        ;; Check that separators are gone
        (should-not (string-match-p sep (buffer-string)))
        ;; Check that both workspaces are present
        (should (string-match-p "default" (buffer-string)))
        (should (string-match-p "feature" (buffer-string)))
        ;; Check that heading is present
        (should (string-match-p "Workspaces" (buffer-string)))))))

(ert-deftest majutsu-workspace-wash-list-hides-single-workspace ()
  "Test that wash-list hides single workspace when show-single is nil."
  (let* ((sep (string 30))
         (line (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main\n")))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert line)
        (goto-char (point-min))
        ;; With show-single=nil, should cancel the section
        (majutsu-workspace--wash-list nil nil)
        ;; Buffer should be empty after cancel (or root placeholder).
        (should (member (buffer-string) '("" "(empty)\n")))))))

(provide 'majutsu-workspace-test)
;;; majutsu-workspace-test.el ends here
