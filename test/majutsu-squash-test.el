;;; majutsu-squash-test.el --- Tests for squash transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for squash argument defaults and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-squash)

(ert-deftest majutsu-squash-arguments/use-transient-args-without-defaults ()
  "Opening the transient should not prefill point/diff defaults into args."
  (let ((transient-current-command 'majutsu-squash))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () '("--from=point"))))
      (should-not (majutsu-squash-arguments)))))

(ert-deftest majutsu-squash-normalize/defaults-to-working-copy-parent ()
  "No source defaults to @ and its external parent."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () nil))
            ((symbol-function 'majutsu-squash--default-args)
             (lambda () nil)))
    (should (equal (majutsu-squash--normalize-args nil)
                   '("--from=@" "--into=parents(roots((@)))")))))

(ert-deftest majutsu-squash-normalize/uses-context-default-source-at-execution ()
  "Execution-time context defaults become --from when user selected no source."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () nil))
            ((symbol-function 'majutsu-squash--default-args)
             (lambda () '("--from=B"))))
    (should (equal (majutsu-squash--normalize-args nil)
                   '("--from=B" "--into=parents(roots((B)))")))))

(ert-deftest majutsu-squash-normalize/uses-point-as-contextual-destination ()
  "When source is already selected, point is encoded as preferred destination."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () "B")))
    (should (equal (majutsu-squash--normalize-args '("--from=C"))
                   '("--from=C"
                     "--into=coalesce((B) ~ ((C)), parents(roots((C))))")))))

(ert-deftest majutsu-squash-normalize/point-source-falls-back-to-parent-expression ()
  "If point is also the source, the coalesce expression falls back to parent."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () "B")))
    (should (equal (majutsu-squash--normalize-args '("--from=B"))
                   '("--from=B"
                     "--into=coalesce((B) ~ ((B)), parents(roots((B))))")))))

(ert-deftest majutsu-squash-normalize/keeps-explicit-destination ()
  "Do not infer destination when user selected one."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () "B")))
    (should (equal (majutsu-squash--normalize-args '("--from=C" "--into=A"))
                   '("--from=C" "--into=A")))))

(ert-deftest majutsu-squash-normalize/defaults-source-to-at-with-explicit-destination ()
  "When destination is explicit but source is omitted, source defaults to @."
  (cl-letf (((symbol-function 'majutsu-squash--default-args)
             (lambda () '("--from=B"))))
    (should (equal (majutsu-squash--normalize-args '("--into=A"))
                   '("--into=A" "--from=@")))))

(ert-deftest majutsu-squash-normalize/inserts-destination-before-filesets ()
  "Infer --into before the fileset separator."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () nil)))
    (should (equal (majutsu-squash--normalize-args '("--from=B" "--" "file"))
                   '("--from=B" "--into=parents(roots((B)))" "--" "file")))))

(ert-deftest majutsu-squash-normalize/keeps-literal-none-source-for-jj-noop ()
  "Do not add --into for literal --from=none(); let jj keep no-op behavior."
  (cl-letf (((symbol-function 'majutsu-squash--point-revision)
             (lambda () "B")))
    (should (equal (majutsu-squash--normalize-args '("--from=none()"))
                   '("--from=none()")))))

(ert-deftest majutsu-squash-default-args/from-diff-revisions ()
  "Use diff --revisions context as default squash source."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::D"))
    (should (equal (majutsu-squash--default-args)
                   '("--from=B::D")))))

(ert-deftest majutsu-squash-default-args/does-not-inherit-diff-from-to ()
  "Do not inherit arbitrary diff --from/--to range for squash."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=A" "--to=D"))
    (should-not (majutsu-squash--default-args))))

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

(ert-deftest majutsu-squash-execute/runs-jj-squash-with-normalized-args ()
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

(ert-deftest majutsu-squash-execute/defaults-to-working-copy-parent ()
  "No source defaults to @ and its external parent."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () nil))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute nil)
      (should (equal called
                     '(("squash" "--from=@" "--into=parents(roots((@)))")))))))

(ert-deftest majutsu-squash-execute/uses-context-default-source ()
  "Execution-time context defaults become --from when user selected no source."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () '("--from=B")))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute nil)
      (should (equal called
                     '(("squash" "--from=B" "--into=parents(roots((B)))")))))))

(ert-deftest majutsu-squash-execute/keeps-explicit-destination ()
  "Do not infer destination when user selected one."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () "B")))
      (majutsu-squash-execute '("--from=C" "--into=A"))
      (should (equal called
                     '(("squash" "--from=C" "--into=A")))))))

(ert-deftest majutsu-squash-execute/keeps-literal-none-source-for-jj-noop ()
  "Do not add --into for literal --from=none(); let jj keep no-op behavior."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () "B")))
      (majutsu-squash-execute '("--from=none()"))
      (should (equal called
                     '(("squash" "--from=none()")))))))

(ert-deftest majutsu-squash-execute/places-structured-filesets-after-options ()
  "Transient fileset groups should be emitted after completed options."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute '(("--" "majutsu-squash.el") "--from=B"))
      (should (equal called
                     '(("squash" "--from=B" "--into=parents(roots((B)))"
                        "--" "majutsu-squash.el")))))))

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
                       nil
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
