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
(require 'majutsu-evolog)
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

(ert-deftest majutsu-evil-test-initial-state-includes-op-and-evolog-modes ()
  "Operation and evolog modes should get the configured Evil initial state."
  (let (calls)
    (cl-letf (((symbol-function 'evil-set-initial-state)
               (lambda (mode state)
                 (push (list mode state) calls))))
      (let ((majutsu-evil-initial-state 'normal))
        (majutsu-evil--set-initial-state)))
    (should (member '(majutsu-op-log-mode normal) calls))
    (should (member '(majutsu-op-diff-mode normal) calls))
    (should (member '(majutsu-evolog-mode normal) calls))))

(ert-deftest majutsu-evil-test-duplicate-stays-dispatch-only ()
  "Evil setup should not install direct duplicate bindings."
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
    (should-not (seq-some
                 (lambda (call)
                   (and (eq (nth 0 call) 'normal)
                        (member (kbd "y") (nth 2 call))))
                 calls))
    (should-not (seq-some
                 (lambda (call)
                   (and (eq (nth 0 call) 'normal)
                        (member (kbd "Y") (nth 2 call))))
                 calls))))

(ert-deftest majutsu-evil-test-dispatch-keys-match-evil-bindings ()
  "Evil setup should update dispatcher suffix keys for visible help."
  (let ((original (copy-tree (get 'majutsu-dispatch 'transient--layout) t)))
    (unwind-protect
        (progn
          (setq majutsu-evil--dispatch-keys-changed nil)
          (majutsu-evil--adjust-dispatch)
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-abandon))
                                     :key)
                         "x"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-log-transient))
                                     :key)
                         "L"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-revert))
                                     :key)
                         "_"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-workspace))
                                     :key)
                         "*"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-undo))
                                     :key)
                         "u"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-redo))
                                     :key)
                         "C-r"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-process-buffer))
                                     :key)
                         "`"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-duplicate))
                                     :key)
                         "y"))
          (should (equal (plist-get (cdr (transient-get-suffix
                                           'majutsu-dispatch 'majutsu-duplicate-dwim))
                                     :key)
                         "Y")))
      (put 'majutsu-dispatch 'transient--layout original)
      (setq majutsu-evil--dispatch-keys-changed nil
            majutsu-evil--dispatch-layout-backup nil))))

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
                           (list (kbd "d") #'majutsu-op-diff-transient
                                 (kbd "u") #'majutsu-op-log-restore-at-point
                                 (kbd "r") #'majutsu-op-log-revert-at-point))))
             calls))
    (should (seq-some
             (lambda (call)
               (and (eq (nth 0 call) 'normal)
                    (eq (nth 1 call) majutsu-op-diff-mode-map)
                    (equal (nth 2 call)
                           (list (kbd "RET") #'majutsu-op-diff-default-action
                                 (kbd "v") #'majutsu-op-diff-evolog-at-point))))
             calls))))

(provide 'majutsu-evil-test)
;;; majutsu-evil-test.el ends here
