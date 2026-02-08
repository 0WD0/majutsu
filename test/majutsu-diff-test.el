;;; majutsu-diff-test.el --- Tests for diff section rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for diff section rendering.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu)

(ert-deftest majutsu-diff-inserts-toggleable-sections ()
  "Diff sections should create headings so users can toggle them."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (diff (string-join
                  '("diff --git a/foo b/foo"
                    "index 1234567..89abcde 100644"
                    "--- a/foo"
                    "+++ b/foo"
                    "@@ -1 +1 @@"
                    "-foo"
                    "+bar")
                  "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-heading "Diff")
        (majutsu--insert-diff-hunks diff))
      (let* ((root magit-root-section)
             (file-section (car (oref root children)))
             (hunk-section (car (oref file-section children))))
        (should (eieio-object-p file-section))
        (should (oref file-section content))
        (should (eieio-object-p hunk-section))
        (should (oref hunk-section content))))))

(ert-deftest majutsu-diff-wash-diffs-parses-diffstat-and-diff ()
  "Diff washer should parse combined `--stat' and `--git' output."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (propertize
                    (string-join
                     '("foo | 1 +"
                       "1 file changed, 1 insertion(+), 0 deletions(-)"
                       "diff --git a/foo b/foo"
                       "index 1234567..89abcde 100644"
                       "--- a/foo"
                       "+++ b/foo"
                       "@@ -1 +1 @@"
                       "-foo"
                       "+bar")
                     "\n")
                    'fontified nil)))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-diff-wash-diffs '("--stat")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (children (oref diff-root children))
             (diffstat (seq-find (lambda (sec) (eq (oref sec type) 'diffstat))
                                 children))
             (diffstat-file (seq-find (lambda (sec)
                                        (eq (oref sec type) 'jj-file))
                                      (oref diffstat children)))
             (diff-file (seq-find (lambda (sec)
                                    (and (eq (oref sec type) 'jj-file)
                                         (equal (oref sec value) "foo")))
                                  children)))
        (should (eieio-object-p diffstat))
        (should (eieio-object-p diffstat-file))
        (should (seq-find (lambda (sec)
                            (and (eq (oref sec type) 'jj-file)
                                 (equal (oref sec value) "foo")))
                          (oref diffstat children)))
        (should (eieio-object-p diff-file))
        (should (oref diff-file content))
        (should-not (text-properties-at 0 (oref diffstat-file value)))
        (should-not (text-properties-at 0 (oref diff-file value)))))))

(ert-deftest majutsu-diff-remembered-args-filters-only-formatting-options ()
  "Only diff formatting options should be remembered per buffer."
  (should (equal (majutsu-diff--remembered-args
                  '("--stat"
                    "-r" "@-"
                    "--from" "main"
                    "--context=5"
                    "--ignore-all-space"))
                 '("--stat" "--context=5" "--ignore-all-space"))))

(ert-deftest majutsu-diff-set-buffer-args-does-not-clear-filesets ()
  "Updating diff args must not clear existing filesets unless requested."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-filesets '("a" "b"))
    (cl-letf (((symbol-function 'majutsu-diff-refresh-buffer) #'ignore))
      (majutsu-diff--set-buffer-args '("--summary")))
    (should (equal majutsu-buffer-diff-filesets '("a" "b")))
    (should (equal majutsu-buffer-diff-args '("--summary")))))

(ert-deftest majutsu-diff-dwim-uses-transient-args-when-active ()
  "When called from the transient, DWIM should use current transient args."
  (let ((transient-current-command 'majutsu-diff)
        (majutsu-direct-use-buffer-arguments 'never)
        called-args
        called-files
        called-range)
    (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
               (lambda (args range filesets &rest _)
                 (setq called-args args
                       called-files filesets
                       called-range range)))
              ((symbol-function 'majutsu-diff--dwim)
               (lambda () '(commit . "abc123")))
              ((symbol-function 'transient-args)
               (lambda (&rest _) (list '("--context=9" "--stat") nil nil))))
      (call-interactively #'majutsu-diff-dwim)
      (should (equal called-args '("--context=9" "--stat")))
      (should (equal called-files nil))
      (should (equal called-range '("--revisions=abc123"))))))

(ert-deftest majutsu-diff-dwim-inherits-current-buffer-range ()
  "DWIM should reuse range and filesets from the current diff buffer."
  (let ((majutsu-direct-use-buffer-arguments 'current)
        called-args
        called-files
        called-range)
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-args '("--stat"))
      (setq-local majutsu-buffer-diff-range '("--from=main" "--to=dev"))
      (setq-local majutsu-buffer-diff-filesets '("foo" "bar"))
      (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
                 (lambda (args range filesets &rest _)
                   (setq called-args args
                         called-files filesets
                         called-range range)))
                ((symbol-function 'majutsu-diff--dwim)
                 (lambda () '(commit . "abc123"))))
        (call-interactively #'majutsu-diff-dwim)
        (should (equal called-args '("--stat")))
        (should (equal called-range '("--from=main" "--to=dev")))
        (should (equal called-files '("foo" "bar")))))))

(ert-deftest majutsu-diff-dwim-does-not-inherit-range-from-other-buffer ()
  "DWIM should not reuse range or filesets from other diff buffers."
  (let ((majutsu-direct-use-buffer-arguments 'always)
        called-files
        called-range)
    (let ((diff-buf (generate-new-buffer " *majutsu diff test*")))
      (unwind-protect
          (progn
            (with-current-buffer diff-buf
              (majutsu-diff-mode)
              (setq-local majutsu-buffer-diff-args '("--stat"))
              (setq-local majutsu-buffer-diff-range '("--from=main" "--to=dev"))
              (setq-local majutsu-buffer-diff-filesets '("foo" "bar")))
            (with-temp-buffer
              (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
                         (lambda (_args range filesets &rest _)
                           (setq called-range range
                                 called-files filesets)))
                        ((symbol-function 'majutsu-diff--dwim)
                         (lambda () '(commit . "abc123"))))
                (call-interactively #'majutsu-diff-dwim)
                (should (equal called-range '("--revisions=abc123")))
                (should (equal called-files nil)))))
        (when (buffer-live-p diff-buf)
          (kill-buffer diff-buf))))))

(ert-deftest majutsu-diff-refine-hunk-default-disabled ()
  "Diff refinement should be disabled by default."
  (with-temp-buffer
    (majutsu-diff-mode)
    (should-not majutsu-diff-refine-hunk)))

(ert-deftest majutsu-diff-toggle-refine-hunk-updates ()
  "Toggling refinement should update local state and trigger refresh."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let ((calls 0))
      (cl-letf (((symbol-function 'majutsu-diff--update-hunk-refinement)
                 (lambda (&rest _)
                   (setq calls (1+ calls)))))
        (should-not majutsu-diff-refine-hunk)
        (majutsu-diff-toggle-refine-hunk nil)
        (should (eq majutsu-diff-refine-hunk t))
        (should (= calls 1))
        (majutsu-diff-toggle-refine-hunk t)
        (should (eq majutsu-diff-refine-hunk 'all))
        (should (= calls 2))
        (majutsu-diff-toggle-refine-hunk t)
        (should (eq majutsu-diff-refine-hunk t))
        (should (= calls 3))
        (majutsu-diff-toggle-refine-hunk nil)
        (should-not majutsu-diff-refine-hunk)
        (should (= calls 4))))))

(ert-deftest majutsu-diff-color-words-goto-from-uses-removed-block ()
  "For shared color-words lines, removed block should target old side."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) 'removed)))
    (should (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-goto-from-uses-added-block ()
  "For shared color-words lines, added block should target new side."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) 'added)))
    (should-not (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-goto-from-falls-back-to-line-shape ()
  "When side cannot be inferred, keep line-shape fallback behavior."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) nil)))
    (should (majutsu-diff--color-words-goto-from '(:from-line 10)))
    (should-not (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-column-uses-side-aware-helper ()
  "Color-words column helper should receive side selection."
  (let ((info '(:content-column 4))
        called)
    (cl-letf (((symbol-function 'majutsu-color-words-column-at-point)
               (lambda (goto-from &optional _pos _info)
                 (setq called goto-from)
                 17)))
      (should (= (majutsu-diff--color-words-column info t) 17))
      (should called)
      (should (= (majutsu-diff--color-words-column info nil) 17))
      (should-not called))))

(provide 'majutsu-diff-test)

;;; majutsu-diff-test.el ends here
