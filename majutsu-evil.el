;;; majutsu-evil.el --- Evil bindings for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library adds optional Evil keybindings for Majutsu without
;; depending on evil-collection.

;;; Code:

(require 'majutsu)

(declare-function turn-off-evil-snipe-mode "evil-snipe" ())
(declare-function turn-off-evil-snipe-override-mode "evil-snipe" ())
(declare-function evil-normalize-keymaps "evil-core" (&optional state))

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

(defvar majutsu-conflict-evil-before-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map (kbd (number-to-string n))
                    (lambda () (interactive)
                      (majutsu-conflict-keep-side n t)))))
    map)
  "Keymap for selecting the before state in JJ conflicts.")

(defvar majutsu-conflict-evil-resolve-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map (kbd (number-to-string n))
                    (lambda () (interactive)
                      (majutsu-conflict-keep-side n nil)))))
    map)
  "Keymap for JJ conflict actions under Evil.")

(defun majutsu-evil--define-keys (states keymap &rest bindings)
  "Define Evil BINDINGS for each state in STATES on KEYMAP.
STATES can be a symbol or list.  KEYMAP should be a symbol.
If KEYMAP is not yet bound, defer binding until it becomes available."
  (declare (indent 2))
  (when (and (featurep 'evil) (fboundp 'evil-define-key*))
    (let ((states (if (listp states) states (list states))))
      (cond
       ;; Keymap already available - bind immediately
       ((and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
        (dolist (state states)
          (apply #'evil-define-key* state (symbol-value keymap) bindings)))
       ;; Keymap not yet available - defer via after-load-functions
       ((symbolp keymap)
        (let* ((fname (format "majutsu-evil-define-key-in-%s" keymap))
               (fun (make-symbol fname)))
          (fset fun `(lambda (&rest _args)
                       (when (and (boundp ',keymap) (keymapp (symbol-value ',keymap)))
                         (remove-hook 'after-load-functions #',fun)
                         (dolist (state ',states)
                           (apply #'evil-define-key* state (symbol-value ',keymap) ',bindings)))))
          (add-hook 'after-load-functions fun t)))))))

(defun majutsu-evil--set-initial-state ()
  "Register initial Evil states for Majutsu modes."
  (when (and majutsu-evil-initial-state
             (fboundp 'evil-set-initial-state))
    (dolist (mode '(majutsu-mode
                    majutsu-log-mode
                    majutsu-op-log-mode
                    majutsu-diff-mode))
      (evil-set-initial-state mode majutsu-evil-initial-state))))

(defun majutsu-evil--adjust-section-bindings ()
  "Unbind C-j from section maps so Evil navigation takes precedence.
This mirrors `evil-collection-magit-adjust-section-bindings'."
  (when (boundp 'majutsu-diff-section-map)
    (define-key majutsu-diff-section-map "\C-j" nil))
  (when (boundp 'majutsu-file-section-map)
    (define-key majutsu-file-section-map "\C-j" nil))
  (when (boundp 'majutsu-hunk-section-map)
    (define-key majutsu-hunk-section-map "\C-j" nil)))

(defun majutsu-evil--define-mode-keys ()
  "Install Evil keybindings for Majutsu maps."
  ;; Unbind C-j from section maps first.
  (majutsu-evil--adjust-section-bindings)
  ;; Normal/visual/motion share the same bindings for navigation commands.
  (majutsu-evil--define-keys '(normal visual motion) 'majutsu-mode-map
    (kbd "C-j") #'magit-section-forward
    (kbd "C-k") #'magit-section-backward
    (kbd "g j") #'magit-section-forward-sibling
    (kbd "g k") #'magit-section-backward-sibling
    (kbd "]") #'magit-section-forward-sibling
    (kbd "[") #'magit-section-backward-sibling
    (kbd "R") #'majutsu-restore
    (kbd "g r") #'majutsu-refresh
    (kbd "`") #'majutsu-process-buffer
    (kbd "c") #'majutsu-describe
    (kbd "C") #'majutsu-commit
    (kbd "o") #'majutsu-new
    (kbd "e") #'majutsu-edit-changeset
    (kbd "u") #'majutsu-undo
    (kbd "C-r") #'majutsu-redo
    (kbd "a") #'majutsu-absorb
    (kbd "x") #'majutsu-abandon
    (kbd "s") #'majutsu-squash
    (kbd "S") #'majutsu-split
    (kbd "L") #'majutsu-log-transient
    (kbd "b") #'majutsu-bookmark
    (kbd "r") #'majutsu-rebase
    (kbd "d") #'majutsu-diff
    (kbd "D") #'majutsu-diff-dwim
    (kbd "*") #'majutsu-workspace
    (kbd "E") #'majutsu-ediff
    (kbd "?") #'majutsu-dispatch
    (kbd "RET") #'majutsu-visit-thing)

  (majutsu-evil--define-keys 'normal 'majutsu-mode-map
    (kbd "y") #'majutsu-duplicate
    (kbd "Y") #'majutsu-duplicate-dwim)

  (majutsu-evil--define-keys '(normal visual) 'majutsu-diff-mode-map
    (kbd "g d") #'majutsu-jump-to-diffstat-or-diff
    (kbd "C-<return>") #'majutsu-diff-visit-workspace-file)

  ;; majutsu-blob-mode is a minor mode, need hook + define-keys
  (add-hook 'majutsu-blob-mode-hook #'evil-normalize-keymaps)
  (majutsu-evil--define-keys '(normal visual motion) 'majutsu-blob-mode-map
    (kbd "p") #'majutsu-blob-previous
    (kbd "n") #'majutsu-blob-next
    (kbd "q") #'majutsu-blob-quit
    (kbd "b") #'majutsu-annotate-addition
    ;; RET visits the revision (edit)
    (kbd "RET") #'majutsu-edit-changeset)

  (majutsu-evil--define-keys '(normal visual motion) 'majutsu-log-mode-map
    (kbd ".") #'majutsu-log-goto-@
    (kbd "O") #'majutsu-new-dwim
    (kbd "I") #'majutsu-new-with-before
    (kbd "A") #'majutsu-new-with-after)

  ;; majutsu-conflict-mode is a minor mode
  (add-hook 'majutsu-conflict-mode-hook #'evil-normalize-keymaps)
  (majutsu-evil--define-keys 'normal 'majutsu-conflict-mode-map
    "gj" #'majutsu-conflict-next
    "]]" #'majutsu-conflict-next
    "gk" #'majutsu-conflict-prev
    "[[" #'majutsu-conflict-prev
    "gb" #'majutsu-conflict-keep-base
    "gr" majutsu-conflict-evil-resolve-map
    "gR" majutsu-conflict-evil-before-map
    "ge" #'majutsu-conflict-refine))

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

;;; _
(provide 'majutsu-evil)
;;; majutsu-evil.el ends here
