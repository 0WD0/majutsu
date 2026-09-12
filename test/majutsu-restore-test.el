;;; majutsu-restore-test.el --- Tests for restore transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for restore command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-restore)

(defun majutsu-restore-test--jj-call (program directory &rest args)
  "Run PROGRAM with ARGS in DIRECTORY and return its standard output.
Signal an ERT failure if the command exits unsuccessfully."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory directory))
           (exit (apply #'call-process
                        program nil t nil
                        (append '("--no-pager" "--color=never"
                                  "--config" "user.name=\"Majutsu Test\""
                                  "--config" "user.email=\"majutsu@example.invalid\"")
                                args))))
      (unless (zerop exit)
        (ert-fail (format "jj failed (%d): %s\n%s"
                          exit (string-join args " ") (buffer-string))))
      (buffer-string))))

(ert-deftest majutsu-restore-default-args/inherits-diff-endpoints ()
  "Explicit diff endpoints should remain Restore endpoints in every spelling."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("-f" "A" "--to=C"))
    (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (revset) (concat "id-" revset))))
      (should (equal (majutsu-restore--default-args)
                     '("--from=A" "--to=C")))
      (should (equal (plist-get (majutsu-restore--diff-context)
                                :patch-context)
                     '(:endpoints "id-A" "id-C"))))))

(ert-deftest majutsu-restore-default-args/maps-single-revision-to-changes-in ()
  "A singleton revision diff should keep jj's `--changes-in' merge semantics."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=mine()"))
    (let (called)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (revset)
                   (setq called revset)
                   "commit-id")))
        (should (equal (majutsu-restore--default-args)
                       '("--changes-in=commit-id")))
        (should (equal called "mine()"))))))

(ert-deftest majutsu-restore-default-args/rejects-aggregate-revision-diff ()
  "Do not invent Restore endpoints for a revision-range diff."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (&rest _) nil)))
      (should-not (majutsu-restore--default-args))
      (should (string-match-p
               "exactly one commit"
               majutsu-restore--unsafe-diff-context)))))

(ert-deftest majutsu-restore-execute/blocks-implicit-operation-for-aggregate-diff ()
  "An aggregate diff must not silently fall back to bare `jj restore'."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let (called)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-run-jj)
                 (lambda (&rest args)
                   (setq called args)
                   0)))
        (should-not (majutsu-restore--default-args))
        (should-error (majutsu-restore-execute nil) :type 'user-error)
        (should-not called)))))

(ert-deftest majutsu-restore-execute/allows-explicit-context-for-aggregate-diff ()
  "Users can explicitly choose a Restore operation from an aggregate diff."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let (called)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-run-jj)
                 (lambda (&rest args)
                   (setq called args)
                   0))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (majutsu-restore-execute '("--from=A" "--to=C"))
        (should (equal called '("restore" "--from=A" "--to=C")))))))

(ert-deftest majutsu-restore-execute/blocks-editor-with-implicit-aggregate-context ()
  "A jj editor must not silently operate on @ from an aggregate diff."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let (started)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-diff-editor-start)
                 (lambda (&rest args) (setq started args))))
        (should-error (majutsu-restore-execute '("-i")) :type 'user-error)
        (should-not started)))))

(ert-deftest majutsu-restore-execute/allows-editor-with-explicit-aggregate-context ()
  "A jj editor may run after the user chooses explicit Restore endpoints."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let (started)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-diff-editor-start)
                 (lambda (&rest args) (setq started args))))
        (majutsu-restore-execute '("-i" "--from=A" "--to=C"))
        (should (equal (cl-subseq started 0 3)
                       '("restore" ("-i" "--from=A" "--to=C") nil)))))))

(ert-deftest majutsu-restore-execute/places-structured-filesets-after-options ()
  "Execute restore with transient filesets after option arguments."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-restore-execute '(("--" "src/a.el") "--from=@-" "--to=@"))
      (should (equal called
                     '("restore" "--from=@-" "--to=@"
                       "--" "src/a.el"))))))

(ert-deftest majutsu-restore-pin-patch-context/uses-canonical-commits ()
  "Restore patch execution replaces dynamic context revsets with commit IDs."
  (should
   (equal (majutsu-restore--pin-patch-context
           '("--changes-in=topic" "--restore-descendants")
           '(:changes-in "commit-id"))
          '("--restore-descendants" "--changes-in=commit-id")))
  (should
   (equal (majutsu-restore--pin-patch-context
           '("--from=A" "--into=B" "--restore-descendants")
           '(:endpoints "source-id" "destination-id"))
          '("--restore-descendants"
            "--from=source-id" "--to=destination-id"))))

(ert-deftest majutsu-restore-execute/passes-patch-context-and-owner ()
  "Keep Restore's inverse-patch flags separate from its selection owner."
  (let (plan-args)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest args)
                 (setq plan-args args)
                 '(:base right :payload-root left :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-restore--check-patch-context)
               (lambda (&rest _) nil)))
      (majutsu-restore-execute '("--from=@-" "--to=@"))
      (should (equal plan-args
                     '(nil complement majutsu-restore))))))

(ert-deftest majutsu-restore-execute/accepts-equivalent-patch-context ()
  "Equivalent revsets may identify the same rendered Restore source."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("-r" "source"))
    (let (called)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (revset)
                   (and (member revset '("source" "equivalent")) "commit")))
                ((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
                 (lambda (&rest _) '(:base right :payload-root left :patch "PATCH" :file-ops nil)))
                ((symbol-function 'majutsu-interactive-run-replay-plan)
                 (lambda (&rest args) (setq called args))))
        (majutsu-restore-execute '("--changes-in=equivalent"))
        (should (equal called
                       '("restore" ("--changes-in=commit") nil (:base right :payload-root left :patch "PATCH" :file-ops nil))))))))

(ert-deftest majutsu-restore-execute/rejects-different-patch-context ()
  "A patch rendered for one tree pair cannot be applied to another."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("-r" "source"))
    (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (revset) (concat "id-" revset)))
              ((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base right :payload-root left :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _)
                 (ert-fail "Mismatched patch context must not run"))))
      (should-error
       (majutsu-restore-execute '("--changes-in=other"))
       :type 'user-error))))

(ert-deftest majutsu-restore-execute/routes-jj-editor-flags-to-diff-editor ()
  "Restore routes every jj diff-editor spelling without rewriting its tool."
  (let ((origin (current-buffer)))
    (dolist (editor-args '(("-i")
                           ("--interactive")
                           ("--tool" "meld")
                           ("--tool=meld")))
      (let (called)
        (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
                   (lambda (&rest _) nil))
                  ((symbol-function 'majutsu-diff-editor-start)
                   (lambda (&rest args)
                     (setq called args))))
          (majutsu-restore-execute
           (append '(("--" "src/a.el") "--from=@-" "--to=@") editor-args))
          (should (equal (cl-subseq called 0 4)
                         (list "restore"
                               (append '("--from=@-" "--to=@") editor-args)
                               '("src/a.el")
                               :origin-buffer)))
          (should (eq (nth 4 called) origin)))))))

(ert-deftest majutsu-restore-execute/explicit-jj-editor-wins-over-patch-selection ()
  "An explicit jj editor route must leave a Majutsu patch selection intact."
  (let ((origin (current-buffer)) called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base right :payload-root left :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-diff-editor-start)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _)
                 (ert-fail "Should not replace an explicit jj editor")))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-restore-execute
       '(("--" "src/a.el") "--from=@-" "--tool" "meld"))
      (should (equal (cl-subseq called 0 4)
                     '("restore" ("--from=@-" "--tool" "meld")
                       ("src/a.el") :origin-buffer)))
      (should (eq (nth 4 called) origin))
      (should-not cleared))))

(ert-deftest majutsu-restore-transient/exposes-tool-infix ()
  "Restore should expose jj's --tool option."
  (should (transient-get-suffix 'majutsu-restore "=t")))

(provide 'majutsu-restore-test)
;;; majutsu-restore-test.el ends here
