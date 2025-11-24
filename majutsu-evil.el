;;; majutsu-evil.el --- Evil bindings for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
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

(autoload 'evil-define-key "evil-core" nil nil 'macro)
(autoload 'evil-set-initial-state "evil-core")

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

(defmacro majutsu-evil--define-keys (states keymap &rest bindings)
  "Define Evil BINDINGS for each state in STATES on KEYMAP.
STATES must be a list of Evil state symbols, and BINDINGS follows
the usual `evil-define-key' format."
  (declare (indent 2))
  `(progn
     ,@(mapcar (lambda (state)
                 `(evil-define-key ',state ,keymap ,@bindings))
               states)))

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
  (majutsu-evil--define-keys (normal visual motion) majutsu-mode-map
    (kbd ".") #'majutsu-log-goto-@
    (kbd "R") #'majutsu-log-refresh
    (kbd "g r") #'majutsu-log-refresh
    (kbd "c") #'majutsu-commit
    (kbd "e") #'majutsu-edit-changeset
    (kbd "u") #'majutsu-undo
    (kbd "C-r") #'majutsu-redo
    (kbd "s") #'majutsu-squash-transient
    (kbd "l") #'majutsu-log-transient
    (kbd "d") #'majutsu-describe
    (kbd "x") #'majutsu-abandon
    (kbd "b") #'majutsu-bookmark-transient
    (kbd "r") #'majutsu-rebase-transient
    (kbd "D") #'majutsu-diff
    (kbd "E") #'majutsu-diffedit-emacs
    (kbd "M") #'majutsu-diffedit-smerge
    (kbd "?") #'majutsu-dispatch)
  ;; Keep enter consistent even if users remap `RET' in their Evil config.
  (majutsu-evil--define-keys (normal motion) majutsu-mode-map
    (kbd "RET") #'majutsu-enter-dwim)
  (majutsu-evil--define-keys (visual) majutsu-mode-map
    (kbd "RET") #'majutsu-enter-dwim)
  ;; per-buffer helpers
  (majutsu-evil--define-keys (normal visual motion) majutsu-diff-mode-map
    (kbd "RET") #'majutsu-enter-dwim))

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
