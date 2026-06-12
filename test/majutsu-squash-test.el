;;; majutsu-squash-test.el --- Tests for squash transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for squash argument defaults and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-squash)

(ert-deftest majutsu-squash-values/parses-prefix-values ()
  "Parse transient-style option values before fileset separator."
  (should (equal (majutsu-squash--values
                  '("--from=a" "--from=b" "--" "--from=c")
                  "--from=")
                 '("a" "b"))))

(ert-deftest majutsu-squash-arguments/use-transient-args-without-defaults ()
  "Opening the transient should not prefill point/diff defaults into args."
  (let ((transient-current-command 'majutsu-squash))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () '("--from=point"))))
      (should-not (majutsu-squash-arguments)))))

(ert-deftest majutsu-squash-default-args/from-diff-revisions ()
  "Use diff --revisions context as default squash source."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::D"))
    (let ((transient--original-buffer (current-buffer)))
      (should (equal (majutsu-squash--default-args)
                     '("--from=B::D"))))))

(ert-deftest majutsu-squash-default-args/does-not-inherit-diff-from-to ()
  "Do not inherit arbitrary diff --from/--to range for squash."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=A" "--to=D"))
    (let ((transient--original-buffer (current-buffer)))
      (should-not (majutsu-squash--default-args)))))

(ert-deftest majutsu-squash-default-args/from-log-region ()
  "Use selected log commits as default sources."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) '("B" "C")))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) "ignored")))
    (should (equal (majutsu-squash--default-args)
                   '("--from=B" "--from=C")))))

(ert-deftest majutsu-squash-default-args/from-log-point ()
  "Use commit at point as default source."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) "B")))
    (should (equal (majutsu-squash--default-args)
                   '("--from=B")))))

(ert-deftest majutsu-squash-patch-source/rejects-arbitrary-from-to-diff ()
  "Squash patch selection is unavailable for arbitrary from/to diff buffers."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=A" "--to=B"))
    (should-not (majutsu-squash--patch-source-revset (current-buffer)))))

(ert-deftest majutsu-squash-execute/runs-jj-squash-with-inferred-destination ()
  "Execute non-patch squash through `majutsu-run-jj-with-editor'."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute '("--from=B"))
      (should (equal called
                     '(("squash" "--from=B" "--into=parents(roots((B)))")))))))

(ert-deftest majutsu-squash-execute/patch-removes-native-interactive-tool-args ()
  "Patch mode injects Majutsu's tool and strips native tool args."
  (let (called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) "PATCH"))
              ((symbol-function 'majutsu-squash--patch-source-revset)
               (lambda (&rest _) "B"))
              ((symbol-function 'majutsu-interactive-run-with-patch)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute '("--from=B" "--interactive" "--tool=meld" "--tool" "vimdiff"))
      (should (equal called
                     '("squash"
                       ("--from=B" "--into=parents(roots((B)))")
                       "PATCH" t)))
      (should cleared))))

(ert-deftest majutsu-squash-execute/patch-rejects-different-source ()
  "Patch mode should not apply the current diff patch to another source."
  (let (cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) "PATCH"))
              ((symbol-function 'majutsu-squash--patch-source-revset)
               (lambda (&rest _) "B"))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (should-error (majutsu-squash-execute '("--from=C")) :type 'user-error)
      (should-not cleared))))

(provide 'majutsu-squash-test)
;;; majutsu-squash-test.el ends here
