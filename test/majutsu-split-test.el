;;; majutsu-split-test.el --- Tests for split transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for split command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-split)

(ert-deftest majutsu-split-default-args/inherits-resolved-revision ()
  "A diff resolving to one revision should become one Split source."
  (with-temp-buffer
    (majutsu-diff-mode)
    (cl-letf (((symbol-function 'majutsu-diff--revision-metadata)
               (lambda () '(:change-id "resolved"))))
      (should (equal (majutsu-split--default-args)
                     '("--revision=resolved"))))))

(ert-deftest majutsu-split-default-args/rejects-incompatible-diff-ranges ()
  "Split must not inherit diffs which do not resolve to one revision."
  (with-temp-buffer
    (majutsu-diff-mode)
    (dolist (range '(("--from=A" "--to=B")
                     ("--revisions=B::D")))
      (setq-local majutsu-buffer-diff-range range)
      (cl-letf (((symbol-function 'majutsu-diff--revision-metadata)
                 (lambda () nil)))
        (should-not (majutsu-split--default-args))
        (should-not (majutsu-split--diff-source-revision))))))

(ert-deftest majutsu-split-transient/exposes-tool-infix-under-equals-prefix ()
  "The jj tool chooser uses Magit's `=t' convention, not jj's `-t'."
  (should (transient-get-suffix 'majutsu-split "=t"))
  (should-not
   (condition-case nil
       (transient-get-suffix 'majutsu-split "-t")
     (error nil))))
(ert-deftest majutsu-split-execute/routes-jj-editor-flags-to-diff-editor ()
  "Keep each explicit jj editor flag when routing split to its session."
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
          (majutsu-split-execute
           (append '(("--" "src/a.el") "--revision=@") editor-args))
          (should (equal (cl-subseq called 0 4)
                         (list "split"
                               (append '("--revision=@") editor-args)
                               '("src/a.el")
                               :origin-buffer)))
          (should (eq (nth 4 called) origin)))))))
(ert-deftest majutsu-split-execute/routes-no-fileset-to-diff-editor ()
  "Let jj perform its default interactive split without a fileset."
  (let ((origin (current-buffer)) called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-diff-editor-start)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-split-execute '("--revision=@"))
      (should (equal (cl-subseq called 0 4)
                     '("split" ("--revision=@") nil :origin-buffer)))
      (should (eq (nth 4 called) origin)))))
(ert-deftest majutsu-split-execute/explicit-jj-editor-wins-over-patch-selection ()
  "An explicit jj editor request must not consume a Majutsu patch selection."
  (let ((origin (current-buffer)) called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-diff-editor-start)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _)
                 (ert-fail "Should not replace an explicit jj editor")))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute
       '(("--" "src/a.el") "--revision=@" "--tool" "meld"))
      (should (equal (cl-subseq called 0 4)
                     '("split" ("--revision=@" "--tool" "meld")
                       ("src/a.el") :origin-buffer)))
      (should (eq (nth 4 called) origin))
      (should-not cleared))))

(ert-deftest majutsu-split-execute/places-structured-filesets-after-options ()
  "Run a noninteractive fileset split through the ordinary runner."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@"))
      (should (equal called
                     '(("split" "--revision=@" "--" "src/a.el")))))))

(ert-deftest majutsu-split-execute/patch-keeps-filesets-after-options ()
  "Patch split should still pass transient filesets after option arguments."
  (let (called cleared stripped)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right
                                   :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-diff-editor-strip-interactive-arguments)
               (lambda (args)
                 (setq stripped args)
                 args))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-split--diff-source-revision)
               (lambda (&rest _) "@"))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@"))
      (should (equal called
                     '("split" ("--revision=@") ("src/a.el")
                       (:base left :payload-root right
                        :patch "PATCH" :file-ops nil))))
      (should (equal stripped '("--revision=@")))
      (should-not cleared))))


(ert-deftest majutsu-split-execute/file-op-only-forwards-filesets ()
  "File-op-only splits use the custom tool and preserve current filesets."
  (let ((ops '((:action delete :path "gone.bin"))) called cleared stripped)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) (list :base 'left :payload-root 'right
                                       :patch nil :file-ops ops)))
              ((symbol-function 'majutsu-diff-editor-strip-interactive-arguments)
               (lambda (args)
                 (setq stripped args)
                 args))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args) (setq called args)))
              ((symbol-function 'majutsu-split--diff-source-revision)
               (lambda (&rest _) "@"))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute
       '(("--" "bin/gone.bin") "--revision=@"))
      (should (equal called
                     (list "split" '("--revision=@") '("bin/gone.bin")
                           (list :base 'left :payload-root 'right
                                 :patch nil :file-ops ops))))
      (should (equal stripped '("--revision=@")))
      (should-not cleared))))

(ert-deftest majutsu-split-execute/patch-rejects-different-source ()
  "Patch mode must not apply the displayed diff to another revision."
  (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
             (lambda (&rest _) '(:base left :payload-root right
                                 :patch "PATCH" :file-ops nil)))
            ((symbol-function 'majutsu-split--diff-source-revision)
             (lambda (&rest _) "B")))
    (should-error (majutsu-split-execute '("--revision=C"))
                  :type 'user-error)))

(provide 'majutsu-split-test)
;;; majutsu-split-test.el ends here
