;;; majutsu-diffedit-test.el --- Tests for majutsu-diffedit  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for diffedit helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-diffedit)

(ert-deftest majutsu-diffedit-test-root-detects-instructions ()
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
                            (majutsu-diffedit--root nested))))))
      (delete-directory root t))))

(ert-deftest majutsu-diffedit-test-finish-on-save-calls-with-editor ()
  "Finish-on-save should call with-editor-finish when active."
  (let ((called nil))
    (with-temp-buffer
      (setq-local server-buffer-clients '(dummy))
      (with-editor-mode 1)
      (let ((majutsu-diffedit-finish-on-save t))
        (cl-letf (((symbol-function 'with-editor-finish)
                   (lambda (&optional _force)
                     (setq called t))))
          (majutsu-diffedit--finish-on-save))))
    (should called)))

(ert-deftest majutsu-diffedit-test-run-with-editor-normalizes-absolute-file ()
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
        (majutsu-diffedit-run-with-editor
         '("--from" "a-" "--to" "a")
         "/tmp/repo/test/majutsu-file-test.el")))
    (should (equal seen-default "/tmp/repo/"))
    (should (equal seen-target "$right/test/majutsu-file-test.el"))
    (should (equal seen-args
                   '("diffedit" "--config" "CFG"
                     "--from" "a-" "--to" "a"
                     "--" "file:\"test/majutsu-file-test.el\"")))))

(ert-deftest majutsu-diffedit-test-run-with-editor-rejects-outside-absolute-file ()
  "Diffedit should reject absolute file targets outside repository."
  (cl-letf (((symbol-function 'majutsu--toplevel-safe)
             (lambda (&optional _directory) "/tmp/repo/"))
            ((symbol-function 'majutsu-run-jj-async)
             (lambda (&rest _args)
               (ert-fail "Should not execute diffedit for outside path"))))
    (let ((default-directory "/tmp/repo/"))
      (should-error
       (majutsu-diffedit-run-with-editor
        '("--from" "a-" "--to" "a")
        "/tmp/other/file.el")
       :type 'user-error))))

(ert-deftest majutsu-diffedit-test-build-args-keeps-files-separate ()
  "Diffedit option args should not include file filters."
  (should (equal (majutsu-diffedit--build-args "a-" "a")
                 '("--from" "a-" "--to" "a"))))

(provide 'majutsu-diffedit-test)
;;; majutsu-diffedit-test.el ends here
