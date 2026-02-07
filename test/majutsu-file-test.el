;; -*- lexical-binding: t; -*-
;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for majutsu file helpers.

;;; Code:

(require 'ert)
(require 'majutsu-file)

(ert-deftest majutsu-file-revset-for-files-quotes-path ()
  "Paths with single quotes should be fileset-quoted."
  (should (equal (majutsu-file--revset-for-files "rev" "test'file" 'prev)
                 "::rev-&files(file:\"test'file\")")))

(ert-deftest majutsu-file-prev-change/handles-nil-output ()
  "Return nil when jj output is nil."
  (cl-letf (((symbol-function 'majutsu-jj-string) (lambda (&rest _args) nil)))
    (should-not (majutsu-file-prev-change "rev" "path"))))

(ert-deftest majutsu-file-next-change/handles-nil-output ()
  "Return nil when jj output is nil."
  (cl-letf (((symbol-function 'majutsu-jj-string) (lambda (&rest _args) nil)))
    (should-not (majutsu-file-next-change "rev" "path"))))

(ert-deftest majutsu-find-file-noselect/filesystem-relative-to-root ()
  "Relative FILE is resolved from the repository root for filesystem visits."
  (let* ((root (make-temp-file "majutsu-file-root-" t))
         (sub (expand-file-name "sub" root))
         (root-target (expand-file-name "note.txt" root))
         (sub-target (expand-file-name "note.txt" sub))
         (buf nil))
    (unwind-protect
        (progn
          (make-directory sub t)
          (with-temp-file root-target
            (insert "root"))
          (with-temp-file sub-target
            (insert "hello"))
          (let ((default-directory sub))
            (cl-letf (((symbol-function 'majutsu-file--root)
                       (lambda () root)))
              (setq buf (majutsu-find-file-noselect nil "note.txt"))))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (equal buffer-file-name root-target))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory root t))))

(ert-deftest majutsu-find-file-noselect/blob-relative-to-root ()
  "Relative FILE becomes a root-relative blob path."
  (let* ((root (make-temp-file "majutsu-file-root-" t))
         (sub (expand-file-name "sub" root))
         (target (expand-file-name "note.txt" sub))
         (buf nil))
    (unwind-protect
        (progn
          (make-directory sub t)
          (with-temp-file target
            (insert "hello"))
          (let ((default-directory sub))
            (cl-letf (((symbol-function 'majutsu-file--root)
                       (lambda () root))
                      ((symbol-function 'majutsu-file--resolve-single-rev)
                       (lambda (_rev) "abcdef123456"))
                      ((symbol-function 'majutsu-file--short-id)
                       (lambda (_id) "abcdef12"))
                      ((symbol-function 'majutsu-file-revert-buffer)
                       (lambda (&rest _args) nil)))
              (setq buf (majutsu-find-file-noselect "@" "sub/note.txt" t))))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (equal majutsu-buffer-blob-root root))
            (should (equal majutsu-buffer-blob-path "sub/note.txt"))
            (should (equal (buffer-name) "sub/note.txt@~abcdef12~"))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory root t))))

(ert-deftest majutsu-find-file-noselect/rejects-outside-repo-for-blob ()
  "Blob views reject paths outside the repository root."
  (let* ((root (make-temp-file "majutsu-file-root-" t))
         (outside (make-temp-file "majutsu-file-outside-" nil ".txt")))
    (unwind-protect
        (let ((default-directory root))
          (cl-letf (((symbol-function 'majutsu-file--root)
                     (lambda () root)))
            (should-error (majutsu-find-file-noselect "@" outside t)
                          :type 'user-error)))
      (delete-directory root t)
      (delete-file outside))))

(provide 'majutsu-file-test)
