;;; majutsu-evil-test.el --- Tests for majutsu-evil  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Evil-specific keymaps.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-conflict)
(require 'majutsu-evil)

(ert-deftest majutsu-evil-test-before-map-side-bindings ()
  "Before map should route digits to the before side."
  (let ((command (lookup-key majutsu-conflict-evil-before-map (kbd "7")))
        call)
    (should (commandp command))
    (cl-letf (((symbol-function 'majutsu-conflict-keep-side)
               (lambda (side before)
                 (setq call (list side before)))))
      (call-interactively command))
    (should (equal call '(7 t)))))

(ert-deftest majutsu-evil-test-resolve-map-side-bindings ()
  "Resolve map should route digits to the after side."
  (let ((command (lookup-key majutsu-conflict-evil-resolve-map (kbd "4")))
        call)
    (should (commandp command))
    (cl-letf (((symbol-function 'majutsu-conflict-keep-side)
               (lambda (side before)
                 (setq call (list side before)))))
      (call-interactively command))
    (should (equal call '(4 nil)))))

(provide 'majutsu-evil-test)
;;; majutsu-evil-test.el ends here
