;;; majutsu-workspace-test.el --- Tests for majutsu-workspace helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for parsing and small helpers in majutsu-workspace.el.

;;; Code:

(require 'ert)
(require 'majutsu-workspace)

(ert-deftest majutsu-workspace-template-plan/default-fields ()
  "The default workspace template plan should transport the default fields."
  (let ((plan (majutsu-workspace--ensure-template-plan)))
    (should (equal (plist-get plan :fields)
                   majutsu-workspace--default-fields))))

(ert-deftest majutsu-workspace-parse-list-output/basic ()
  "Parse structured `jj workspace list -T ...` output."
  (let* ((sep majutsu-workspace--field-separator)
         (default-directory "/tmp/main/")
         (output (concat
                  "@" sep "default" sep "wnurqwps" sep "6acd46b7" sep "Main wc" sep "/tmp/main" "\n"
                  ""  sep "w2"      sep "lvolzxkz" sep "32e07e11" sep ""        sep "/tmp/w2"   "\n"))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 2))
    (should (equal (plist-get (nth 0 entries) :name) "default"))
    (should (equal (plist-get (nth 0 entries) :current) t))
    (should (equal (plist-get (nth 0 entries) :change-id) "wnurqwps"))
    (should (equal (plist-get (nth 0 entries) :commit-id) "6acd46b7"))
    (should (equal (plist-get (nth 0 entries) :desc) "Main wc"))
    (should (equal (plist-get (nth 0 entries) :root) "/tmp/main/"))
    (should (equal (plist-get (nth 1 entries) :name) "w2"))
    (should-not (plist-get (nth 1 entries) :current))
    (should (equal (plist-get (nth 1 entries) :desc) ""))
    (should (equal (plist-get (nth 1 entries) :root) "/tmp/w2/"))))

(ert-deftest majutsu-workspace-parse-list-output/root-error-normalizes-to-nil ()
  "Workspace root template errors should normalize to nil."
  (let* ((sep majutsu-workspace--field-separator)
         (default-directory "/tmp/main/")
         (output (concat
                  "@" sep "default" sep "wnurqwps" sep "6acd46b7" sep "Main wc"
                  sep "<Error: Failed to resolve workspace root>" "\n"))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 1))
    (should (null (plist-get (car entries) :root)))))

(ert-deftest majutsu-workspace-parse-list-output/root-preserves-remote-prefix ()
  "Structured workspace roots should keep the current TRAMP prefix."
  (let ((default-directory "/ssh:demo:/tmp/main/"))
    (let* ((sep majutsu-workspace--field-separator)
           (output (concat "@" sep "default" sep "wnurqwps" sep "6acd46b7" sep "Main wc"
                           sep "/home/demo/repo-main" "\n"))
           (entries (majutsu-workspace-parse-list-output output)))
      (should (equal (plist-get (car entries) :root)
                     "/ssh:demo:/home/demo/repo-main/")))))

(ert-deftest majutsu-workspace--names/uses-structured-list-entries ()
  "Workspace names should be derived from structured entries."
  (cl-letf (((symbol-function 'majutsu-workspace-list-entries)
             (lambda (&optional _directory)
               '((:name "ws-a")
                 (:name "ws-b")
                 (:name "ws-a")))))
    (should (equal (majutsu-workspace--names)
                   '("ws-a" "ws-b")))))

(ert-deftest majutsu-workspace-current-name/uses-structured-list-entries ()
  "Current workspace name should be derived from structured entries."
  (cl-letf (((symbol-function 'majutsu-workspace-list-entries)
             (lambda (&optional _directory)
               '((:name "ws-a" :current nil)
                 (:name "ws-b" :current t)))))
    (should (equal (majutsu-workspace-current-name) "ws-b"))))

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

(ert-deftest majutsu-workspace-wash-list-transforms-buffer ()
  "Structured wash should transform the buffer into workspace sections."
  (let* ((sep majutsu-workspace--field-separator)
         (lines (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main"
                        sep "/workspaces/default" "\n"
                        sep "feature" sep "lvolzxkz" sep "32e07e11" sep "Feature work"
                        sep "/workspaces/feature" "\n"))
         (default-directory "/tmp/test-repo/")
         (majutsu--default-directory "/tmp/test-repo/"))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert lines)
        (goto-char (point-min))
        (majutsu-workspace--wash-list t nil)
        (should-not (string-match-p sep (buffer-string)))
        (should (string-match-p "Workspaces" (buffer-string)))
        (should (string-match-p "default" (buffer-string)))
        (should (string-match-p "feature" (buffer-string)))
        (should (string-match-p "/workspaces/default" (buffer-string)))
        (should (string-match-p "/workspaces/feature" (buffer-string)))))))

(ert-deftest majutsu-workspace-wash-list-renders-missing-root-placeholder ()
  "Missing structured roots should render as a fixed placeholder."
  (let* ((sep majutsu-workspace--field-separator)
         (lines (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main"
                        sep "<Error: Failed to resolve workspace root>" "\n"))
         (default-directory "/tmp/test-repo/")
         (majutsu--default-directory "/tmp/test-repo/"))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert lines)
        (goto-char (point-min))
        (majutsu-workspace--wash-list t nil)
        (should (string-match-p "default" (buffer-string)))
        (should (string-match-p "-" (buffer-string)))))))

(ert-deftest majutsu-workspace-wash-list-hides-single-workspace ()
  "Wash-list should hide a single workspace when show-single is nil."
  (let* ((sep majutsu-workspace--field-separator)
         (line (concat "@" sep "default" sep "wqps1234" sep "6acd46b7" sep "Main"
                       sep "/workspaces/default" "\n")))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert line)
        (goto-char (point-min))
        (majutsu-workspace--wash-list nil nil)
        (should (member (buffer-string) '("" "(empty)\n")))))))

;;; Root-for-name tests

(ert-deftest majutsu-workspace--root-for-name/returns-directory ()
  "Test that root-for-name returns a directory path from jj output."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args) '("/home/user/repo-secondary"))))
    (should (equal (majutsu-workspace--root-for-name "secondary")
                   "/home/user/repo-secondary/"))))

(ert-deftest majutsu-workspace--root-for-name/preserves-remote-prefix ()
  "Workspace roots discovered on TRAMP should keep remote host prefix."
  (let ((default-directory "/ssh:demo:/tmp/"))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:")))
              ((symbol-function 'majutsu-jj-lines)
               (lambda (&rest _args) '("/home/demo/repo-secondary"))))
      (should (equal (majutsu-workspace--root-for-name "secondary")
                     "/ssh:demo:/home/demo/repo-secondary/")))))

(ert-deftest majutsu-workspace--root-for-name/returns-nil-on-error ()
  "Test that root-for-name returns nil when jj fails (no output)."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args) nil)))
    (should (null (majutsu-workspace--root-for-name "nonexistent")))))

(ert-deftest majutsu-workspace--read-root/prefers-section-root ()
  "Read-root should reuse a structured root from the current section first."
  (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
             (lambda () '(:name "secondary" :root "/tmp/secondary/")))
            ((symbol-function 'majutsu-workspace--root-for-name)
             (lambda (&rest _args)
               (ert-fail "should not consult jj workspace root when section root exists"))))
    (should (equal (majutsu-workspace--read-root "secondary")
                   "/tmp/secondary/"))))

(ert-deftest majutsu-workspace--read-root/uses-jj-workspace-root ()
  "Test that read-root delegates to jj workspace root --name."
  (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
             (lambda () nil))
            ((symbol-function 'majutsu-workspace--root-for-name)
             (lambda (name)
               (when (equal name "secondary") "/tmp/secondary/"))))
    (should (equal (majutsu-workspace--read-root "secondary")
                   "/tmp/secondary/"))))

(ert-deftest majutsu-workspace--read-root/preserves-remote-prefix ()
  "Read-root should keep remote host prefix when using absolute localname."
  (let ((root "/ssh:demo:/tmp/main/"))
    (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
               (lambda () nil))
              ((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path root)
                            (null identification))
                   "/ssh:demo:")))
              ((symbol-function 'majutsu-workspace--root-for-name)
               (lambda (_name) "/ssh:demo:/home/demo/repo-secondary/")))
      (should (equal (majutsu-workspace--read-root "secondary" root)
                     "/ssh:demo:/home/demo/repo-secondary/")))))

(ert-deftest majutsu-workspace--sibling-root/finds-matching-sibling ()
  "Test that sibling-root finds a sibling dir that is the named workspace."
  (let* ((parent (make-temp-file "majutsu-test-" t))
         (root (file-name-as-directory (expand-file-name "main" parent)))
         (sibling (file-name-as-directory (expand-file-name "feature" parent))))
    (unwind-protect
        (progn
          (make-directory root t)
          (make-directory sibling t)
          (cl-letf (((symbol-function 'majutsu-toplevel)
                     (lambda (&optional dir) dir))
                    ((symbol-function 'majutsu-workspace-current-name)
                     (lambda (&optional dir)
                       (when (string= (file-name-as-directory dir) sibling)
                         "feature"))))
            (should (equal (majutsu-workspace--sibling-root "feature" root)
                           sibling))))
      (delete-directory parent t))))

(ert-deftest majutsu-workspace--sibling-root/returns-nil-when-no-match ()
  "Test that sibling-root returns nil when no matching sibling exists."
  (let* ((parent (make-temp-file "majutsu-test-" t))
         (root (file-name-as-directory (expand-file-name "main" parent))))
    (unwind-protect
        (progn
          (make-directory root t)
          (cl-letf (((symbol-function 'majutsu-toplevel)
                     (lambda (&optional _dir) nil))
                    ((symbol-function 'majutsu-workspace-current-name)
                     (lambda (&optional _dir) nil)))
            (should (null (majutsu-workspace--sibling-root "nonexistent" root)))))
      (delete-directory parent t))))

(ert-deftest majutsu-workspace--read-root/falls-back-to-sibling ()
  "Test that read-root uses sibling-root when root-for-name fails."
  (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
             (lambda () nil))
            ((symbol-function 'majutsu-workspace--root-for-name)
             (lambda (_name) nil))
            ((symbol-function 'majutsu-workspace--sibling-root)
             (lambda (name _root)
               (when (equal name "feature") "/tmp/feature/"))))
    (should (equal (majutsu-workspace--read-root "feature")
                   "/tmp/feature/"))))

(ert-deftest majutsu-workspace-add/uses-local-destination-for-jj ()
  "Workspace add should pass a local destination path to remote jj.
The Emacs-facing path remains unchanged for visiting the new workspace."
  (let (seen-args seen-visit)
    (cl-letf (((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (_path) "/tmp/feature"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'majutsu-workspace-visit)
               (lambda (dir)
                 (setq seen-visit dir))))
      (majutsu-workspace-add "/ssh:demo:/tmp/feature")
      (should (equal seen-args '("workspace" "add" "/tmp/feature")))
      (should (equal seen-visit (expand-file-name "/ssh:demo:/tmp/feature"))))))

(provide 'majutsu-workspace-test)
;;; majutsu-workspace-test.el ends here
