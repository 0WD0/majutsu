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

(ert-deftest majutsu-blob-edit-start/enables-editable-mode ()
  "Starting blob edit mode should make blob buffer writable."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
    (majutsu-blob-mode 1)
    (setq buffer-read-only t)
    (majutsu-blob-edit-start)
    (should majutsu-blob-edit-mode)
    (should-not buffer-read-only)
    (should (equal majutsu-blob-edit--original-content "old"))))

(ert-deftest majutsu-blob-edit-start/does-not-force-evil-insert-state ()
  "Starting blob edit mode from Evil binding should stay in normal state."
  (let (insert-called)
    (with-temp-buffer
      (insert "old")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq insert-called t))))
        (majutsu-blob-edit-start)
        (should majutsu-blob-edit-mode)
        (should-not insert-called)))))

(ert-deftest majutsu-blob-edit-mode/toggles-cursor-type ()
  "Editable mode should switch cursor type and restore it on exit."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
    (setq-local cursor-type 'box)
    (let ((majutsu-blob-edit-cursor-type 'bar))
      (majutsu-blob-edit-mode 1)
      (should (eq cursor-type 'bar))
      (majutsu-blob-edit-mode -1)
      (should (eq cursor-type 'box)))))

(ert-deftest majutsu-blob-edit-mode/toggles-evil-normal-cursor ()
  "Editable mode should mirror Dirvish-style Evil cursor changes."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
    (setq-local cursor-type '(box . 4))
    (setq-local evil-local-mode t)
    (setq-local evil-normal-state-cursor '(box . 4))
    (let ((majutsu-blob-edit-cursor-type 'hollow))
      (majutsu-blob-edit-mode 1)
      (should (eq cursor-type 'hollow))
      (should (eq evil-normal-state-cursor 'hollow))
      (majutsu-blob-edit-mode -1)
      (should (equal cursor-type '(box . 4)))
      (should (equal evil-normal-state-cursor '(box . 4))))))

(ert-deftest majutsu-blob-edit-apply-diffedit/noninteractive-copies-content ()
  "Blob apply should run non-interactive diffedit via cp editor command."
  (let (editor-command editor-target run-args copied)
    (with-temp-buffer
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (cl-letf (((symbol-function 'majutsu-jj--editor-command-config)
                 (lambda (_key target &optional command)
                   (setq editor-target target)
                   (setq editor-command command)
                   "CFG"))
                ((symbol-function 'majutsu-run-jj)
                 (lambda (&rest args)
                   (setq run-args args)
                   (setq copied
                         (with-temp-buffer
                           (insert-file-contents (cadr editor-command))
                           (buffer-string)))
                   0)))
        (should (zerop (majutsu-blob-edit--apply-diffedit "after")))))
    (should (equal editor-target "$right/src/a.el"))
    (should (equal (car editor-command) "cp"))
    (should (equal copied "after"))
    (should-not (file-exists-p (cadr editor-command)))
    (should (equal run-args
                   '("diffedit" "--config" "CFG"
                     "--from" "rev-" "--to" "rev"
                     "--" "src/a.el")))))

(ert-deftest majutsu-blob-edit-write-contents/calls-diffedit-apply ()
  "Editable blob save should apply edits via non-interactive diffedit helper."
  (let (seen-content)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (erase-buffer)
      (insert "after")
      (cl-letf (((symbol-function 'majutsu-blob-edit--apply-diffedit)
                 (lambda (content)
                   (setq seen-content content)
                   0)))
        (should (majutsu-blob-edit--write-contents))
        (should (equal seen-content "after"))
        (should-not majutsu-blob-edit-mode)))))

(ert-deftest majutsu-blob-edit-exit/no-changes-disables-mode ()
  "Exit should just leave editable mode when there are no changes."
  (with-temp-buffer
    (insert "stable")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
    (majutsu-blob-mode 1)
    (majutsu-blob-edit-mode 1)
    (set-buffer-modified-p nil)
    (majutsu-blob-edit-exit)
    (should-not majutsu-blob-edit-mode)))

(ert-deftest majutsu-blob-edit-exit/uses-content-delta-not-modified-flag ()
  "Exit should still treat content deltas as changes when modified flag is nil."
  (let (finished aborted)
    (with-temp-buffer
      (insert "stable")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (insert "!")
      ;; Simulate a stale/cleared modified flag.
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'majutsu-blob-edit-finish)
                 (lambda () (setq finished t)))
                ((symbol-function 'majutsu-blob-edit-abort)
                 (lambda () (setq aborted t))))
        (majutsu-blob-edit-exit)
        (should finished)
        (should-not aborted)))))

(ert-deftest majutsu-blob-edit-exit/modified-can-finish ()
  "Exit should finish when user confirms save."
  (let (finished aborted)
    (with-temp-buffer
      (insert "stable")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (insert "!")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'majutsu-blob-edit-finish)
                 (lambda () (setq finished t)))
                ((symbol-function 'majutsu-blob-edit-abort)
                 (lambda () (setq aborted t))))
        (majutsu-blob-edit-exit)
        (should finished)
        (should-not aborted)))))

(ert-deftest majutsu-blob-edit-exit/modified-can-abort ()
  "Exit should abort when user declines save."
  (let (finished aborted)
    (with-temp-buffer
      (insert "stable")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (insert "!")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'majutsu-blob-edit-finish)
                 (lambda () (setq finished t)))
                ((symbol-function 'majutsu-blob-edit-abort)
                 (lambda () (setq aborted t))))
        (majutsu-blob-edit-exit)
        (should aborted)
        (should-not finished)))))

(ert-deftest majutsu-blob-mode-map-uses-editable-entry ()
  "Blob key `e` should enter editable blob mode."
  (should (eq (lookup-key majutsu-blob-mode-map (kbd "e"))
              #'majutsu-blob-edit-start)))

(ert-deftest majutsu-blob-edit-mode-map-has-exit ()
  "Editable blob mode should bind C-x C-q to exit command."
  (should (eq (lookup-key majutsu-blob-edit-mode-map (kbd "C-x C-q"))
              #'majutsu-blob-edit-exit)))

(provide 'majutsu-file-test)
