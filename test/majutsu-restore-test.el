;;; majutsu-restore-test.el --- Tests for restore transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for restore command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-restore)

(ert-deftest majutsu-restore-execute/places-structured-filesets-after-options ()
  "Execute restore with transient filesets after option arguments."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
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

(ert-deftest majutsu-restore-execute/patch-keeps-filesets-after-options ()
  "Patch restore should still pass transient filesets after option arguments."
  (let (called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
               (lambda (&rest _) "PATCH"))
              ((symbol-function 'majutsu-interactive-run-with-patch)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-restore-execute '(("--" "src/a.el") "--from=@-" "--to=@"))
      (should (equal called
                     '("restore" ("--from=@-" "--to=@") ("src/a.el") "PATCH")))
      (should cleared))))

(provide 'majutsu-restore-test)
;;; majutsu-restore-test.el ends here
