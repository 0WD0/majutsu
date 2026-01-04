;;; majutsu-diff-test.el --- Tests for diff section rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Version: 1.0.0
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
           (output (string-join
                    '("foo | 1 +"
                      "1 file changed, 1 insertion(+), 0 deletions(-)"
                      "diff --git a/foo b/foo"
                      "index 1234567..89abcde 100644"
                      "--- a/foo"
                      "+++ b/foo"
                      "@@ -1 +1 @@"
                      "-foo"
                      "+bar")
                    "\n")))
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
             (diff-file (seq-find (lambda (sec)
                                    (and (eq (oref sec type) 'jj-file)
                                         (equal (oref sec value) "foo")))
                                  children)))
        (should (eieio-object-p diffstat))
        (should (seq-find (lambda (sec)
                            (and (eq (oref sec type) 'jj-file)
                                 (equal (oref sec value) "foo")))
                          (oref diffstat children)))
        (should (eieio-object-p diff-file))
        (should (oref diff-file content))))))

(ert-deftest majutsu-diff-remembered-args-filters-only-formatting-options ()
  "Only diff formatting options should be remembered per buffer."
  (should (equal (majutsu-diff--remembered-args
                  '("--stat"
                    "-r" "@-"
                    "--from" "main"
                    "--context=5"
                    "--tool" ":git"
                    "--ignore-all-space"))
                 '("--stat" "--context=5" "--tool" ":git" "--ignore-all-space"))))

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
  (let ((transient-current-command 'majutsu-diff-transient--internal)
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
               (lambda (&rest _) '("--context=9" "--stat"))))
      (call-interactively #'majutsu-diff-dwim)
      (should (equal called-args '("--context=9" "--stat")))
      (should (equal called-files nil))
      (should (equal called-range '(:type revisions :revisions "abc123"))))))

(ert-deftest majutsu-diff-range-arguments-encode-revisions-and-from-to ()
  "Range arguments should produce jj-friendly CLI args."
  (should (equal (majutsu-diff--range-arguments nil) nil))
  (should (equal (majutsu-diff--range-arguments '(:type nil)) nil))
  (should (equal (majutsu-diff--range-arguments '(:type revisions :revisions "abc123"))
                 '("--revisions=abc123")))
  (should (equal (majutsu-diff--range-arguments '(:type revisions :revisions ("@-" "main")))
                 '("--revisions=@-" "--revisions=main")))
  (should (equal (majutsu-diff--range-arguments '(:type from-to :from "@-" :to "@"))
                 '("--from=@-" "--to=@"))))

(ert-deftest majutsu-diff-args-revsets-extracts-revisions ()
  "Revsets should be extracted from various transient argument encodings."
  (should (equal (majutsu-diff--args-revsets '("--revisions=@-" "--revisions=main"))
                 '("@-" "main")))
  (should (equal (majutsu-diff--args-revsets '("-r" "@-" "-rmain" "--revisions" "dev"))
                 '("@-" "main" "dev")))
  (should (equal (majutsu-diff--args-revsets '(("-r" . "@-") ("--revisions" . "main")))
                 '("@-" "main"))))

(provide 'majutsu-diff-test)

;;; majutsu-diff-test.el ends here
