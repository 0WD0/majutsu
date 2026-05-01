;;; majutsu-evil-test.el --- Tests for majutsu-evil  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Evil-specific keymaps.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'majutsu-conflict)
(require 'majutsu-evil)
(require 'majutsu-op)

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

(ert-deftest majutsu-evil-test-initial-state-includes-op-modes ()
  "Operation modes should get the configured Evil initial state."
  (let (calls)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (mode state)
                 (push (list mode state) calls))))
      (let ((majutsu-evil-initial-state 'normal))
        (majutsu-evil--set-initial-state)))
    (should (member '(majutsu-op-log-mode normal) calls))
    (should (member '(majutsu-op-show-mode normal) calls))
    (should (member '(majutsu-op-diff-mode normal) calls))))

(ert-deftest majutsu-evil-test-op-mode-keybindings ()
  "Operation mode maps should receive Evil-specific bindings."
  (let ((featurep-original (symbol-function 'featurep))
        calls)
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (if (eq feature 'evil)
                     t
                   (funcall featurep-original feature subfeature))))
              ((symbol-function 'evil-normalize-keymaps)
               (lambda (&rest _)))
              ((symbol-function 'evil-define-key*)
               (lambda (state keymap &rest bindings)
                 (push (list state keymap bindings) calls))))
      (unwind-protect
          (majutsu-evil--define-mode-keys)
        (dolist (hook '(majutsu-blob-mode-hook
                        majutsu-blob-edit-mode-hook
                        majutsu-conflict-mode-hook
                        majutsu-annotate-mode-hook))
          (remove-hook hook #'evil-normalize-keymaps))))
    (should (seq-some
             (lambda (call)
               (and (eq (nth 0 call) 'normal)
                    (eq (nth 1 call) majutsu-op-log-mode-map)
                    (equal (nth 2 call)
                           (list (kbd "RET") #'majutsu-op-log-show-at-point
                                 (kbd "d") #'majutsu-op-log-show-at-point))))
             calls))
    (should (seq-some
             (lambda (call)
               (and (eq (nth 0 call) 'normal)
                    (eq (nth 1 call) majutsu-op-show-mode-map)
                    (equal (nth 2 call)
                           (list (kbd "RET") #'majutsu-op-show-default-action
                                 (kbd "d") #'majutsu-op-show-diff-at-point
                                 (kbd "v") #'majutsu-op-show-evolog-at-point))))
             calls))
    (should (seq-some
             (lambda (call)
               (and (eq (nth 0 call) 'normal)
                    (eq (nth 1 call) majutsu-op-diff-mode-map)
                    (equal (nth 2 call)
                           (list (kbd "RET") #'majutsu-op-show-default-action
                                 (kbd "d") #'majutsu-op-show-diff-at-point
                                 (kbd "v") #'majutsu-op-show-evolog-at-point))))
             calls))))

(provide 'majutsu-evil-test)
;;; majutsu-evil-test.el ends here
