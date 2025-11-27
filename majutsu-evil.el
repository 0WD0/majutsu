;;; majutsu-evil.el --- Evil bindings for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Optional Evil integration for Majutsu.  This file defines native Evil
;; keybindings so Majutsu users no longer need an external Doom module or
;; evil-collection recipe to get a consistent experience.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-commands)
(require 'majutsu-log)
(require 'majutsu-transient)

(eval-when-compile
  ;; Silence byte-compile when Evil isn't installed at build time.
  (unless (require 'evil nil t)
    (defun evil-define-key* (&rest _args) nil)
    (defun evil-set-initial-state (&rest _args) nil)))

(defgroup majutsu-evil nil
  "Customization group for Majutsu's Evil integration."
  :group 'majutsu
  :prefix "majutsu-evil-")

(defcustom majutsu-evil-enable-integration t
  "If non-nil, install Majutsu's Evil bindings automatically."
  :type 'boolean
  :group 'majutsu-evil)

(defcustom majutsu-evil-initial-state 'normal
  "Initial Evil state used for Majutsu buffers.
When nil, Majutsu leaves Evil's state untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Visual" visual)
          (const :tag "Insert" insert)
          (const :tag "Emacs" emacs)
          (const :tag "Replace" replace)
          (symbol :tag "Custom state"))
  :group 'majutsu-evil)

(defun majutsu-evil--define-keys (states keymap &rest bindings)
  "Define Evil BINDINGS for each state in STATES on KEYMAP.
STATES can be a symbol or list.  KEYMAP can be a symbol or list of
symbols/maps.  Mirrors `evil-collection-define-key' to defer any
macro expansion until Evil is actually present."
  (declare (indent 2)
           (states keymap &rest bindings))
  (when (and (featurep 'evil) (fboundp 'evil-define-key*))
    (let* ((states (if (listp states) states (list states)))
           (maps (if (listp keymap) keymap (list keymap))))
      (dolist (state states)
        (dolist (map maps)
          (let ((mapobj (cond
                         ((and (symbolp map) (boundp map)
                               (keymapp (symbol-value map)))
                          (symbol-value map))
                         ((keymapp map) map))))
            (when mapobj
              (apply #'evil-define-key* state mapobj bindings))))))))

(defun majutsu-evil--set-initial-state ()
  "Register initial Evil states for Majutsu modes."
  (when (and majutsu-evil-initial-state
             (fboundp 'evil-set-initial-state))
    (dolist (mode '(majutsu-mode
                    majutsu-log-mode
                    majutsu-op-log-mode
                    majutsu-diff-mode))
      (evil-set-initial-state mode majutsu-evil-initial-state))))

(defun majutsu-evil--define-mode-keys ()
  "Install Evil keybindings for Majutsu maps."
  ;; Normal/visual/motion share the same bindings for navigation commands.
  (majutsu-evil--define-keys '(normal visual motion) 'majutsu-mode-map
    (kbd "R") #'majutsu-refresh
    (kbd "g r") #'majutsu-refresh
    (kbd "c") #'majutsu-describe
    (kbd "C") #'majutsu-commit
    (kbd "o") #'majutsu-new-transient
    (kbd "e") #'majutsu-edit-changeset
    (kbd "u") #'majutsu-undo
    (kbd "C-r") #'majutsu-redo
    (kbd "x") #'majutsu-abandon
    (kbd "s") #'majutsu-squash-transient
    (kbd "L") #'majutsu-log-transient
    (kbd "b") #'majutsu-bookmark-transient
    (kbd "r") #'majutsu-rebase-transient
    (kbd "d") #'majutsu-diff-transient
    (kbd "D") #'majutsu-diff
    (kbd "E") #'majutsu-diffedit-emacs
    (kbd "M") #'majutsu-diffedit-smerge
    (kbd "?") #'majutsu-dispatch
    (kbd "RET") #'majutsu-enter-dwim)

  (majutsu-evil--define-keys 'normal 'majutsu-mode-map
    (kbd "y") #'majutsu-duplicate-transient
    (kbd "Y") #'majutsu-duplicate)

  (majutsu-evil--define-keys '(normal visual) 'majutsu-diff-mode-map
    (kbd "g d") #'majutsu-jump-to-diffstat-or-diff)

  (majutsu-evil--define-keys '(normal visual motion) 'majutsu-log-mode-map
    (kbd ".") #'majutsu-log-goto-@
    (kbd "O") #'majutsu-new
    (kbd "I") #'majutsu-new-with-before
    (kbd "A") #'majutsu-new-with-after))

;;;###autoload
(defun majutsu-evil-setup ()
  "Install Majutsu's native Evil integration.
Safe to call multiple times.  Set
`majutsu-evil-enable-integration' to nil to skip automatic setup."
  (interactive)
  (when (and (featurep 'evil) majutsu-evil-enable-integration)
    (majutsu-evil--set-initial-state)
    (majutsu-evil--define-mode-keys)))

(with-eval-after-load 'evil
  (majutsu-evil-setup))

(with-eval-after-load 'evil-snipe
  (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-override-mode))

(provide 'majutsu-evil)
;;; majutsu-evil.el ends here
