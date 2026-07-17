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

(ert-deftest majutsu-split-execute/places-structured-filesets-after-options ()
  "Execute split with transient filesets after option arguments."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@" "--interactive"))
      (should (equal called
                     '(("split" "--revision=@" "--interactive"
                        "--" "src/a.el")))))))

(ert-deftest majutsu-split-execute/patch-keeps-filesets-after-options ()
  "Patch split should still pass transient filesets after option arguments."
  (let (called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right
                                  :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-split--diff-source-revision)
               (lambda (&rest _) "@"))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@" "--interactive"))
      (should (equal called
                     '("split" ("--revision=@") ("src/a.el")
                       (:base left :payload-root right
                              :patch "PATCH" :file-ops nil))))
      (should cleared))))

(ert-deftest majutsu-split-execute/file-op-only-forwards-filesets-and-strips-tool ()
  "File-op-only splits use the custom tool and preserve current filesets."
  (let ((ops '((:action delete :path "gone.bin"))) called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) (list :base 'left :payload-root 'right
                                      :patch nil :file-ops ops)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args) (setq called args)))
              ((symbol-function 'majutsu-split--diff-source-revision)
               (lambda (&rest _) "@"))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute
       '(("--" "bin/gone.bin") "--revision=@" "-i"
         "--tool" "meld" "--tool=vimdiff"))
      (should (equal called
                     (list "split" '("--revision=@") '("bin/gone.bin")
                           (list :base 'left :payload-root 'right
                                 :patch nil :file-ops ops))))
      (should cleared))))

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
