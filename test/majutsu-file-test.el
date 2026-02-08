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

(ert-deftest majutsu-blob-edit-write-contents/calls-diffedit ()
  "Editable blob save should queue pending content and invoke diffedit."
  (let ((majutsu-blob-edit--pending-edits nil)
        called)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (erase-buffer)
      (insert "after")
      (cl-letf (((symbol-function 'majutsu-ediff-edit)
                 (lambda (&optional args)
                   (setq called args))))
        (should (majutsu-blob-edit--write-contents))
        (should (equal called nil))
        (should-not majutsu-blob-edit-mode)
        (should (equal (plist-get (car majutsu-blob-edit--pending-edits) :file)
                       "src/a.el"))))))

(ert-deftest majutsu-blob-edit-apply-pending/writes-right-side-buffer ()
  "Applying pending blob edits should replace opened right-side content."
  (let* ((root (make-temp-file "majutsu-diffedit-" t))
         (right-file (expand-file-name "right/src/a.el" root))
         (saved-content nil)
         (majutsu-blob-edit--pending-edits
          (list (list :file "src/a.el"
                      :line 1
                      :column 2
                      :content "abcdef"))))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "JJ-INSTRUCTIONS" root) nil 'silent)
          (make-directory (file-name-directory right-file) t)
          (write-region "old" nil right-file nil 'silent)
          (with-temp-buffer
            (setq buffer-file-name right-file)
            (insert "old")
            (cl-letf (((symbol-function 'save-buffer)
                       (lambda (&optional _arg)
                         (setq saved-content
                               (buffer-substring-no-properties (point-min)
                                                               (point-max))))))
              (majutsu-blob-edit--apply-pending)
              (should (equal saved-content "abcdef"))
              (should (null majutsu-blob-edit--pending-edits))
              (should (= (current-column) 2)))))
      (delete-directory root t))))

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
