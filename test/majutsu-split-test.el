;;; majutsu-split-test.el --- Tests for split transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for split command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-split)

(ert-deftest majutsu-split-execute/places-structured-filesets-after-options ()
  "Execute split with transient filesets after option arguments."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-operation-if-selected)
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
    (cl-letf (((symbol-function 'majutsu-interactive-build-operation-if-selected)
               (lambda (&rest _) '(:patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-interactive-run-with-patch)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@" "--interactive"))
      (should (equal called
                     '("split" ("--revision=@") ("src/a.el") "PATCH" t nil)))
      (should cleared))))

(ert-deftest majutsu-split-execute/file-op-only-forwards-filesets-and-strips-tool ()
  "File-op-only splits use the custom tool and preserve current filesets."
  (let ((ops '((:action delete :path "gone.bin"))) called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-operation-if-selected)
               (lambda (&rest _) (list :patch nil :file-ops ops)))
              ((symbol-function 'majutsu-interactive-run-with-patch)
               (lambda (&rest args) (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute
       '(("--" "bin/gone.bin") "--revision=@" "-i"
         "--tool" "meld" "--tool=vimdiff"))
      (should (equal called
                     (list "split" '("--revision=@") '("bin/gone.bin")
                           nil t ops)))
      (should cleared))))

(provide 'majutsu-split-test)
;;; majutsu-split-test.el ends here
