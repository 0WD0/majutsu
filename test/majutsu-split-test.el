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
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
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
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) "PATCH"))
              ((symbol-function 'majutsu-interactive-run-with-patch)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@" "--interactive"))
      (should (equal called
                     '("split" ("--revision=@") ("src/a.el") "PATCH" t)))
      (should cleared))))

(provide 'majutsu-split-test)
;;; majutsu-split-test.el ends here
