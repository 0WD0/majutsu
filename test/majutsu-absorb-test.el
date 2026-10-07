;;; majutsu-absorb-test.el --- Tests for absorb transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for absorb source context and partial patch execution.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-absorb)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-absorb-default-args-from-diff-revisions ()
  "Use diff --revisions context as default --from for absorb."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=abc123"))
    (should (equal (majutsu-absorb--default-args)
                   '("--from=abc123")))))

(ert-deftest majutsu-absorb-default-args-from-diff-from ()
  "Keep --from from diff range when available."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=main" "--to=@"))
    (should (equal (majutsu-absorb--default-args)
                   '("--from=main")))))

(ert-deftest majutsu-absorb-arguments-default-from-point ()
  "Outside transient, default --from to commit at point."
  (let ((transient-current-command nil))
    (cl-letf (((symbol-function 'magit-section-value-if)
               (lambda (&rest _) "kxzptvry")))
      (should (equal (majutsu-absorb-arguments)
                     '("--from=kxzptvry"))))))

(ert-deftest majutsu-absorb/patch-selection-keeps-unselected-changes ()
  "Hunk, file, and region selections absorb into their respective ancestors."
  (majutsu-jj-integration-with-repo repo
    (let ((majutsu-jj-global-arguments '("--no-pager" "--color=never"))
          (majutsu-process-popup-time -1)
          (buffers-before (buffer-list))
          process)
      (unwind-protect
          (save-window-excursion
            (dolist (file '("region.txt" "hunk.txt" "whole.txt" "outside.txt"))
              (majutsu-jj-integration-write-text
               (expand-file-name file repo) "base\n"))
            (majutsu-jj-integration-call repo "describe" "-m" "A")
            (let ((a (majutsu-jj-string "log" "--no-graph" "-r" "@" "-T" "change_id")))
              (majutsu-jj-integration-call repo "new" "-m" "B")
              (majutsu-jj-integration-write-text
               (expand-file-name "hunk.txt" repo) "parent\n")
              (let ((b (majutsu-jj-string "log" "--no-graph" "-r" "@" "-T" "change_id")))
                (majutsu-jj-integration-call repo "new" "-m" "source")
                (majutsu-jj-integration-write-text
                 (expand-file-name "region.txt" repo)
                 "base\nselected region\nunselected region\n")
                (dolist (file '("hunk.txt" "whole.txt" "outside.txt"))
                  (majutsu-jj-integration-write-text
                   (expand-file-name file repo) "changed\n"))
                (let ((source (majutsu-jj-string
                               "log" "--no-graph" "-r" "@" "-T" "change_id")))
                  (majutsu-jj-integration-call repo "new" "-m" "child")
                  (with-current-buffer
                      (majutsu-diff-setup-buffer
                       '("--git") (list "-r" source) nil)
                    (let ((transient-current-command 'majutsu-absorb))
                      (goto-char
                       (oref (majutsu-interactive--file-section-for-file "hunk.txt")
                             content))
                      (search-forward "@@")
                      (majutsu-interactive-toggle-hunk)
                      (goto-char
                       (oref (majutsu-interactive--file-section-for-file "whole.txt")
                             start))
                      (majutsu-interactive-toggle-file)
                      (goto-char
                       (oref (majutsu-interactive--file-section-for-file "region.txt")
                             content))
                      (search-forward "+selected region")
                      (beginning-of-line)
                      (let ((transient-mark-mode t))
                        (set-mark (line-beginning-position 2))
                        (setq mark-active t)
                        (majutsu-interactive-toggle-region)))
                    ;; A patch must not be applied to the working child or to
                    ;; several --from arguments, even if jj accepts a revset.
                    (let ((operation (majutsu-jj-operation-id repo)))
                      (should-error (majutsu-absorb-execute '("--from=@"))
                                    :type 'user-error)
                      (should-error
                       (majutsu-absorb-execute
                        (list (concat "--from=" source) (concat "--from=" b)))
                       :type 'user-error)
                      (should (equal operation (majutsu-jj-operation-id repo)))
                      (should (majutsu-interactive-has-selections-p)))
                    (setq process
                          (majutsu-absorb-execute
                           (append
                            (majutsu-absorb--default-args)
                            (list (concat "--into=" a) (concat "--into=" b)
                                  '("--" "region.txt" "hunk.txt" "whole.txt")))))
                    (let ((deadline (+ (float-time) 10)))
                      (while (and (or (process-live-p process)
                                      (majutsu-interactive-has-selections-p))
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.05))
                      (should-not (process-live-p process))
                      (should (zerop (process-exit-status process)))
                      (should-not (majutsu-interactive-has-selections-p))))
                  (dolist (expectation
                           `((,a "region.txt" "base\nselected region\n")
                             (,a "whole.txt" "changed\n")
                             (,a "hunk.txt" "base\n")
                             (,b "hunk.txt" "changed\n")
                             (,b "outside.txt" "base\n")
                             (,source "region.txt" "base\nselected region\nunselected region\n")
                             (,source "outside.txt" "changed\n")))
                    (pcase-let ((`(,revision ,file ,content) expectation))
                      (should (equal (majutsu-jj-integration-output
                                      repo "file" "show" "-r" revision file)
                                     content))))))))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (dolist (buffer (seq-difference (buffer-list) buffers-before))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-absorb/patch-source-rejects-aggregate-diff ()
  "An aggregate diff is not a valid source for one absorb patch."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=A" "--to=B"))
    (should-error
     (majutsu-absorb--check-patch-source
      '("--from=A") (majutsu-diff-single-source-commit))
     :type 'user-error)))

(provide 'majutsu-absorb-test)
;;; majutsu-absorb-test.el ends here
