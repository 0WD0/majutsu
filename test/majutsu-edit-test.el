;;; majutsu-edit-test.el --- Tests for majutsu-edit  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for edit and diffedit helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-edit)

(ert-deftest majutsu-edit-test-diffedit-root-detects-instructions ()
  "Diffedit root should be detected by JJ-INSTRUCTIONS file."
  (let ((root (make-temp-file "majutsu-diffedit" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "JJ-INSTRUCTIONS" root) nil 'silent)
          (let* ((nested (expand-file-name "right/file.txt" root))
                 (dir (file-name-directory nested)))
            (make-directory dir t)
            (write-region "" nil nested nil 'silent)
            (should (equal (file-name-as-directory root)
                           (file-name-as-directory
                            (majutsu-edit--diffedit-root nested))))))
      (delete-directory root t))))

(ert-deftest majutsu-edit-test-finish-on-save-calls-with-editor ()
  "Finish-on-save should call with-editor-finish when active."
  (let ((called nil))
    (with-temp-buffer
      (setq-local server-buffer-clients '(dummy))
      (with-editor-mode 1)
      (let ((majutsu-edit-finish-on-save t))
        (cl-letf (((symbol-function 'with-editor-finish)
                   (lambda (&optional _force)
                     (setq called t))))
          (majutsu-edit--finish-on-save))))
    (should called)))

(ert-deftest majutsu-edit-test-run-diffedit-normalizes-absolute-file ()
  "Diffedit should normalize absolute file targets to repo-relative paths."
  (let (seen-target seen-args seen-default)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/tmp/repo/"))
              ((symbol-function 'majutsu-jj--editor-command-config)
               (lambda (_key target &optional _editor-command)
                 (setq seen-target target)
                 "CFG"))
              ((symbol-function 'majutsu-run-jj-async)
               (lambda (&rest args)
                 (setq seen-default default-directory)
                 (setq seen-args args)
                 0)))
      (let ((default-directory "/tmp/repo/test/"))
        (majutsu-edit--run-diffedit
         '("--from" "a-" "--to" "a" "--" "/tmp/repo/test/majutsu-file-test.el"))))
    (should (equal seen-default "/tmp/repo/"))
    (should (equal seen-target "$right/test/majutsu-file-test.el"))
    (should (equal seen-args
                   '("diffedit" "--config" "CFG"
                     "--from" "a-" "--to" "a"
                     "--" "test/majutsu-file-test.el")))))

(ert-deftest majutsu-edit-test-run-diffedit-rejects-outside-absolute-file ()
  "Diffedit should reject absolute file targets outside repository."
  (cl-letf (((symbol-function 'majutsu--toplevel-safe)
             (lambda (&optional _directory) "/tmp/repo/"))
            ((symbol-function 'majutsu-run-jj-async)
             (lambda (&rest _args)
               (ert-fail "Should not execute diffedit for outside path"))))
    (let ((default-directory "/tmp/repo/"))
      (should-error
       (majutsu-edit--run-diffedit
        '("--from" "a-" "--to" "a" "--" "/tmp/other/file.el"))
       :type 'user-error))))

(ert-deftest majutsu-edit-test-replace-diffedit-file-arg ()
  "Diffedit args should carry exactly the normalized file after `--'."
  (should (equal (majutsu-edit--replace-diffedit-file-arg
                  '("--from" "a-" "--to" "a" "--" "old/file.el")
                  "test/majutsu-file-test.el")
                 '("--from" "a-" "--to" "a" "--" "test/majutsu-file-test.el")))
  (should (equal (majutsu-edit--replace-diffedit-file-arg
                  '("--from" "a-" "--to" "a")
                  "test/majutsu-file-test.el")
                 '("--from" "a-" "--to" "a" "--" "test/majutsu-file-test.el"))))

(provide 'majutsu-edit-test)
;;; majutsu-edit-test.el ends here
