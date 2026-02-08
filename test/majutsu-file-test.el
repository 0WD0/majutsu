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
                      ((symbol-function 'majutsu-file--resolve-single-rev-info)
                       (lambda (_rev) '(:change-id "abcdef123456"
                                       :commit-id "0123456789ab")))
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

(ert-deftest majutsu-find-file-read-args/always-prompts-for-path ()
  "Find-file args should always call path reader with context default."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-file--root)
               (lambda () "/tmp/repo"))
              ((symbol-function 'majutsu-file--default-revset)
               (lambda () "@"))
              ((symbol-function 'majutsu-read-revset)
               (lambda (_prompt _default) "chosen-rev"))
              ((symbol-function 'majutsu-file--path-at-point)
               (lambda (_root) "src/at-point.el"))
              ((symbol-function 'majutsu-file--read-path)
               (lambda (revset root default)
                 (setq seen (list revset root default))
                 "picked.el")))
      (should (equal (majutsu-find-file-read-args "Find file")
                     '("chosen-rev" "picked.el")))
      (should (equal seen '("chosen-rev" "/tmp/repo" "src/at-point.el"))))))

(ert-deftest majutsu-file-revert-buffer/noop-when-revision-unchanged ()
  "Revert should be a no-op when change-id and commit-id are unchanged."
  (let (called)
    (with-temp-buffer
      (insert "same\n")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "chg1")
      (setq-local majutsu-buffer-blob-change-id "chg1")
      (setq-local majutsu-buffer-blob-commit-id "c1")
      (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (_revset)
                   '(:change-id "chg1" :commit-id "c1")))
                ((symbol-function 'majutsu-jj-insert)
                 (lambda (&rest _args)
                   (setq called t))))
        (should-not (majutsu-file-revert-buffer nil t))
        (should-not called)
        (should (equal (buffer-string) "same\n"))))))

(ert-deftest majutsu-file-revert-buffer/same-change-uses-offset ()
  "When commit changes within same change-id, revert should use diff offset."
  (let (offset-args)
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\nfive\nsix\n")
      (goto-char (point-min))
      (forward-line 2)
      (move-to-column 2)
      (setq-local majutsu-buffer-blob-root "/tmp/repo")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "chg1")
      (setq-local majutsu-buffer-blob-change-id "chg1")
      (setq-local majutsu-buffer-blob-commit-id "c1")
      (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
      (setq-local majutsu-blob-mode nil)
      (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (_revset)
                   '(:change-id "chg1" :commit-id "c2")))
                ((symbol-function 'majutsu-diff-visit--offset)
                 (lambda (&rest args)
                   (setq offset-args args)
                   5))
                ((symbol-function 'majutsu-jj-insert)
                 (lambda (&rest _args)
                   (insert "aaaa\nbbbb\ncccc\ndddd\neeee\nffff\n")))
                ((symbol-function 'normal-mode)
                 (lambda (&optional _find-file) nil)))
        (majutsu-file-revert-buffer nil t)
        (should (equal offset-args
                       '("/tmp/repo" "src/a.el" "c1" "c2" 3)))
        (should (equal majutsu-buffer-blob-commit-id "c2"))
        (should majutsu-blob-mode)
        (should (= (line-number-at-pos) 5))
        (should (= (current-column) 2))))))

(ert-deftest majutsu-file-revert-buffer/change-change-jumps-top-and-renames ()
  "When change-id changes, revert jumps to top and updates buffer name."
  (with-temp-buffer
    (rename-buffer "temp-blob-buffer" t)
    (insert "one\ntwo\n")
    (goto-char (point-max))
    (setq-local majutsu-buffer-blob-root "/tmp/repo")
    (setq-local majutsu-buffer-blob-revset "@")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "oldchange")
    (setq-local majutsu-buffer-blob-change-id "oldchange")
    (setq-local majutsu-buffer-blob-commit-id "cold")
    (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
    (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
               (lambda (_revset)
                 '(:change-id "newchangeid" :commit-id "cnew")))
              ((symbol-function 'majutsu-jj-insert)
               (lambda (&rest _args)
                 (insert "n1\nn2\n")))
              ((symbol-function 'normal-mode)
               (lambda (&optional _find-file) nil)))
      (majutsu-file-revert-buffer nil t)
      (should (equal majutsu-buffer-blob-revision "newchangeid"))
      (should (equal (buffer-name) "src/a.el@~newchang~"))
      (should (= (line-number-at-pos) 1))
      (should (= (current-column) 0)))))

(ert-deftest majutsu-file-revert-buffer/detects-hidden-local-drift ()
  "Revert should prompt when content changed but modified flag is cleared."
  (let (asked)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "chg1")
      (setq-local majutsu-buffer-blob-change-id "chg1")
      (setq-local majutsu-buffer-blob-commit-id "c1")
      (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
      (erase-buffer)
      (insert "drifted")
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args)
                   (setq asked t)
                   nil))
                ((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (&rest _args)
                   (ert-fail "Should not resolve revision when user declines"))))
        (should-not (majutsu-file-revert-buffer nil nil))
        (should asked)
        (should (equal (buffer-string) "drifted"))))))

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
    (should-not majutsu-blob-mode)
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

(ert-deftest majutsu-blob-visit-magit/uses-commit-id ()
  "Open magit blob view using resolved commit id and path."
  (let* ((root (file-name-as-directory (make-temp-file "majutsu-blob-magit-" t)))
         (other (file-name-as-directory (make-temp-file "majutsu-blob-default-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu-buffer-blob-root root)
          (setq-local majutsu-buffer-blob-path "src/a.el")
          (setq-local majutsu-buffer-blob-revset "@")
          (setq-local majutsu-buffer-blob-revision "change-id")
          (setq-local majutsu-buffer-blob-change-id "change-id")
          (setq-local majutsu-buffer-blob-commit-id nil)
          (setq default-directory other)
          (insert "zero\nfirst\nsecond\n")
          (goto-char (point-min))
          (forward-line 1)
          (move-to-column 2)
          (majutsu-blob-mode 1)
          (let ((line (line-number-at-pos))
                (col (current-column))
                seen-rev seen-file seen-dir opened-buf)
            (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                       (lambda (_revset)
                         '(:change-id "change-id" :commit-id "deadbeef")))
                      ((symbol-function 'magit-find-file)
                       (lambda (rev file)
                          (setq opened-buf (generate-new-buffer " *magit-blob*"))
                         (with-current-buffer opened-buf
                           (insert "zero\nfirst\nsecond\n"))
                         (setq seen-rev rev
                               seen-file file
                               seen-dir default-directory)
                         opened-buf)))
               (majutsu-blob-visit-magit)
               (should (equal seen-rev "deadbeef"))
               (should (equal seen-file "src/a.el"))
               (should (equal seen-dir root))
               (should (equal majutsu-buffer-blob-commit-id "deadbeef"))
               (with-current-buffer opened-buf
                 (should (= (line-number-at-pos) line))
                 (should (= (current-column) col)))
               (when (buffer-live-p opened-buf)
                 (kill-buffer opened-buf)))))
      (delete-directory root t)
      (delete-directory other t))))

(ert-deftest majutsu-blob-visit-magit/prefers-cached-commit-id ()
  "Use cached blob commit-id without re-resolving revset."
  (let ((root (file-name-as-directory (make-temp-file "majutsu-blob-magit-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu-buffer-blob-root root)
          (setq-local majutsu-buffer-blob-path "src/a.el")
          (setq-local majutsu-buffer-blob-revset "@")
          (setq-local majutsu-buffer-blob-revision "change-id")
          (setq-local majutsu-buffer-blob-change-id "change-id")
          (setq-local majutsu-buffer-blob-commit-id "cachedc0ffee")
          (majutsu-blob-mode 1)
          (let (seen-rev opened-buf)
            (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                       (lambda (&rest _args)
                         (ert-fail "Should not resolve when commit-id is cached")))
                      ((symbol-function 'magit-find-file)
                       (lambda (rev _file)
                         (setq seen-rev rev)
                         (setq opened-buf (generate-new-buffer " *magit-blob*"))
                         opened-buf)))
              (majutsu-blob-visit-magit)
              (should (equal seen-rev "cachedc0ffee"))
              (when (buffer-live-p opened-buf)
                (kill-buffer opened-buf)))))
      (delete-directory root t))))

(ert-deftest majutsu-blob-visit-magit/errors-when-unresolved ()
  "Signal an error when revset does not resolve to a commit id."
  (let ((root (file-name-as-directory (make-temp-file "majutsu-blob-magit-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu-buffer-blob-root root)
          (setq-local majutsu-buffer-blob-path "src/a.el")
          (setq-local majutsu-buffer-blob-revset "@")
          (setq-local majutsu-buffer-blob-revision "change-id")
          (setq-local majutsu-buffer-blob-change-id "change-id")
          (setq-local majutsu-buffer-blob-commit-id nil)
          (majutsu-blob-mode 1)
          (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                      (lambda (_revset) nil))
                     ((symbol-function 'magit-find-file)
                      (lambda (&rest _args) (error "unexpected"))))
            (should-error (majutsu-blob-visit-magit) :type 'user-error)))
      (delete-directory root t))))

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
  "Editable blob save should apply edits and refresh revision metadata."
  (let (seen-content)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-revision "rev")
      (setq-local majutsu-buffer-blob-change-id "old-change")
      (setq-local majutsu-buffer-blob-commit-id "old-commit")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (erase-buffer)
      (insert "after")
      (cl-letf (((symbol-function 'majutsu-blob-edit--apply-diffedit)
                 (lambda (content)
                   (setq seen-content content)
                   0))
                ((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (_revset)
                   '(:change-id "new-change" :commit-id "new-commit"))))
        (should (majutsu-blob-edit--write-contents))
        (should (equal seen-content "after"))
        (should-not majutsu-blob-edit-mode)
        (should (equal majutsu-buffer-blob-revision "new-change"))
        (should (equal majutsu-buffer-blob-change-id "new-change"))
        (should (equal majutsu-buffer-blob-commit-id "new-commit"))
        (should (equal majutsu-buffer-blob-content-hash
                       (secure-hash 'sha1 "after")))))))

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
    (should-not majutsu-blob-edit-mode)
    (should majutsu-blob-mode)))

(ert-deftest majutsu-blob-edit-finish/no-changes-disables-mode ()
  "Finish should leave editable mode when no changes exist."
  (with-temp-buffer
    (insert "stable")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
    (majutsu-blob-mode 1)
    (majutsu-blob-edit-mode 1)
    (majutsu-blob-edit-finish)
    (should-not majutsu-blob-edit-mode)
    (should majutsu-blob-mode)))

(ert-deftest majutsu-blob-edit-finish/changed-with-cleared-modified-applies ()
  "Finish should still apply when content changed but modified flag is nil."
  (let (seen-content)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (erase-buffer)
      (insert "after")
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'majutsu-blob-edit--apply-diffedit)
                 (lambda (content)
                   (setq seen-content content)
                   0))
                ((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (_revset)
                   '(:change-id "new-change" :commit-id "new-commit"))))
        (majutsu-blob-edit-finish)
        (should (equal seen-content "after"))
        (should-not majutsu-blob-edit-mode)
        (should majutsu-blob-mode)))))

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

(ert-deftest majutsu-blob-edit-exit/decline-save-restores-entry-point ()
  "Declining save on exit should restore original content and point."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
    (majutsu-blob-mode 1)
    (goto-char (point-min))
    (forward-line 1)
    (move-to-column 1)
    (let ((entry-point (point))
          (original (buffer-string)))
      (majutsu-blob-edit-mode 1)
      (goto-char (point-max))
      (insert "changed")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (majutsu-blob-edit-exit))
      (should-not majutsu-blob-edit-mode)
      (should majutsu-blob-mode)
      (should (equal (buffer-string) original))
      (should (= (point) entry-point)))))

(ert-deftest majutsu-blob-mode-map-uses-editable-entry ()
  "Blob key `e` should enter editable blob mode."
  (should (eq (lookup-key majutsu-blob-mode-map (kbd "e"))
              #'majutsu-blob-edit-start)))

(ert-deftest majutsu-blob-edit-mode-map-has-exit ()
  "Editable blob mode should bind C-x C-q to exit command."
  (should (eq (lookup-key majutsu-blob-edit-mode-map (kbd "C-x C-q"))
              #'majutsu-blob-edit-exit)))

(provide 'majutsu-file-test)
