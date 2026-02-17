;;; majutsu-annotate-test.el --- Tests for annotate helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for annotate navigation helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-annotate)

(ert-deftest majutsu-annotate-file-exists-p/uses-exact-path-match ()
  "Return non-nil only when jj output exactly matches FILE."
  (let (captured-path)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq captured-path (nth 4 args))
                 '("src/file.el"))))
      (should (majutsu-annotate--file-exists-p "rev" "src/file.el"))
      (should (equal captured-path (majutsu-jj-fileset-quote "src/file.el"))))))

(ert-deftest majutsu-annotate-file-exists-p/rejects-prefix-or-warning-output ()
  "Return nil when jj output does not contain an exact path match."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args) '("src/file.el.bak"))))
    (should-not (majutsu-annotate--file-exists-p "rev" "src/file.el")))
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args) '("Warning: No matching entries"))))
    (should-not (majutsu-annotate--file-exists-p "rev" "src/file.el"))))

(ert-deftest majutsu-annotate-file-exists-p/runs-from-repo-root ()
  "Run file existence checks from the repository root directory."
  (let* ((root (file-name-as-directory (make-temp-file "majutsu-annotate-root-" t)))
         (docs (expand-file-name "docs/" root))
         captured-default-directory
         captured-path)
    (unwind-protect
        (progn
          (make-directory docs t)
          (let ((default-directory docs))
            (cl-letf (((symbol-function 'majutsu-toplevel)
                       (lambda (&optional _directory) root))
                      ((symbol-function 'majutsu-jj-lines)
                       (lambda (&rest args)
                         (setq captured-default-directory (symbol-value 'default-directory))
                         (setq captured-path (nth 4 args))
                         '("docs/majutsu.org"))))
              (should (majutsu-annotate--file-exists-p "rev" "docs/majutsu.org"))
              (should (equal captured-default-directory root))
              (should (equal captured-path
                             (majutsu-jj-fileset-quote "docs/majutsu.org"))))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest majutsu-annotate-visit-other-file/stops-when-target-file-missing ()
  "Do not open a blob buffer if parent revision doesn't contain FILE."
  (with-temp-buffer
    (let ((chunk (make-majutsu-annotate-chunk :prev-rev "abc123" :orig-line 1))
          find-file-called)
      (setq-local majutsu-buffer-blob-path "src/file.el")
      (cl-letf (((symbol-function 'majutsu-annotate-current-chunk)
                 (lambda () chunk))
                ((symbol-function 'majutsu-annotate--file-exists-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'majutsu-find-file)
                 (lambda (&rest _args)
                   (setq find-file-called t))))
        (should-error (majutsu-annotate-visit-other-file) :type 'user-error)
        (should-not find-file-called)))))

(ert-deftest majutsu-annotate-show-commit/calls-diff-revset-with-change-id ()
  "Show-commit should open the diff for the current chunk's change-id."
  (let ((chunk (make-majutsu-annotate-chunk :orig-rev "abc123"))
        called-args)
    (cl-letf (((symbol-function 'majutsu-annotate-current-chunk)
               (lambda () chunk))
              ((symbol-function 'majutsu-diff-revset)
               (lambda (&rest args)
                 (setq called-args args))))
      (majutsu-annotate-show-commit)
      (should (equal called-args '("abc123"))))))

(provide 'majutsu-annotate-test)
;;; majutsu-annotate-test.el ends here
