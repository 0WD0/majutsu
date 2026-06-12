;;; majutsu-core.el --- Core aggregation for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library gathers foundational Majutsu pieces so other modules
;; can depend on a single entry point, reducing circular dependencies.

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-base)
(require 'majutsu-process)

;;; Transient UX integration

(defcustom majutsu-prefix-use-buffer-arguments 'selected
  "Whether Majutsu transient prefixes reuse arguments from relevant buffers.

Valid values are:
`always'   Always use arguments from an existing relevant buffer.
`selected' Use arguments from a relevant buffer displayed in a window.
`current'  Use arguments only when the current buffer is relevant.
`never'    Never reuse arguments from buffers."
  :group 'majutsu
  :type '(choice (const :tag "Always" always)
          (const :tag "Selected" selected)
          (const :tag "Current" current)
          (const :tag "Never" never)))

(defcustom majutsu-direct-use-buffer-arguments 'selected
  "Whether direct commands reuse arguments from relevant buffers.

This affects commands that can be invoked from a transient prefix, but
are also commonly invoked directly."
  :group 'majutsu
  :type '(choice (const :tag "Always" always)
          (const :tag "Selected" selected)
          (const :tag "Current" current)
          (const :tag "Never" never)))

(defun majutsu--transient--majutsu-prefix-p ()
  "Return non-nil when the active transient prefix is a Majutsu command."
  (and (boundp 'transient--prefix)
       (eieio-object-p transient--prefix)
       (string-prefix-p "majutsu-"
                        (symbol-name (oref transient--prefix command)))))

(defun majutsu--transient--selection-active-p ()
  "Return non-nil if the user is in Evil visual state."
  (and (bound-and-true-p evil-local-mode)
       (fboundp 'evil-visual-state-p)
       (evil-visual-state-p)))

(defun majutsu--transient--quit-event ()
  "Return the final event of the current quit key sequence."
  (let ((keys (this-command-keys-vector)))
    (and (vectorp keys)
         (> (length keys) 0)
         (aref keys (1- (length keys))))))

(defun majutsu--transient--cancel-selection ()
  "Cancel the current visual selection/region in current buffer."
  (cond
   ((and (bound-and-true-p evil-local-mode)
         (fboundp 'evil-visual-state-p)
         (evil-visual-state-p)
         (fboundp 'evil-exit-visual-state))
    (evil-exit-visual-state))
   (t
    (deactivate-mark))))

(defun majutsu--transient--do-quit-one-dwim (orig-fn &rest args)
  "Advice around `transient--do-quit-one' for Majutsu transients.

If the user presses <escape> or C-g while in Evil visual state, then
exit visual state and keep the transient.  Otherwise quit as usual.

Note: <escape> does not affect the plain Emacs region."
  (let ((event (majutsu--transient--quit-event)))
    (cond
     ((and (majutsu--transient--majutsu-prefix-p)
           (memq event '(escape ?\C-g))
           (majutsu--transient--selection-active-p))
      (majutsu--transient--cancel-selection)
      t)
     ((and (majutsu--transient--majutsu-prefix-p)
           (eq event ?\C-g)
           (use-region-p))
      (deactivate-mark)
      t)
     (t
      (apply orig-fn args)))))

(with-eval-after-load 'transient
  (when (fboundp 'transient--do-quit-one)
    (unless (advice-member-p #'majutsu--transient--do-quit-one-dwim
                             'transient--do-quit-one)
      (advice-add 'transient--do-quit-one :around
                  #'majutsu--transient--do-quit-one-dwim))))

;;; Custom groups

(defgroup majutsu-essentials nil
  "Options that most Majutsu users should consider."
  :group 'majutsu)

;;; Shared Transients

(defun majutsu-transient-original-buffer ()
  "Return the transient's original buffer when it is live."
  (and (boundp 'transient--original-buffer)
       (buffer-live-p transient--original-buffer)
       transient--original-buffer))

(defun majutsu-transient-default-revset ()
  "Return the default revset for transient revset readers."
  (with-current-buffer (or (majutsu-transient-original-buffer)
                           (current-buffer))
    (or (magit-section-value-if 'jj-commit) "@")))

(defun majutsu-transient-prefix-command ()
  "Return the current transient prefix command."
  (oref (transient-prefix-object) command))

(defun majutsu-transient-jj-command-args ()
  "Return jj subcommand args for the active revset transient."
  (pcase (majutsu-transient-prefix-command)
    ('majutsu-absorb '("absorb"))
    ('majutsu-diff '("diff"))
    ('majutsu-ediff '("diff"))
    ('majutsu-duplicate '("duplicate"))
    ('majutsu-new '("new"))
    ('majutsu-rebase '("rebase"))
    ('majutsu-restore '("restore"))
    ('majutsu-revert '("revert"))
    ('majutsu-simplify-parents-transient '("simplify-parents"))
    ('majutsu-split '("split"))
    ('majutsu-squash '("squash"))))

(defun majutsu-transient-jj-option-arg ()
  "Return jj option arg for the active revset infix command."
  (string-remove-suffix "=" (oref (transient-suffix-object) argument)))

(defun majutsu-transient-revset-completion-args ()
  "Return jj native completion context for the active revset reader."
  (when-let* ((command (majutsu-transient-jj-command-args))
              (option (majutsu-transient-jj-option-arg)))
    (append command (list option))))

(defun majutsu-transient-expression-revset-p ()
  "Return non-nil if the active transient argument accepts a revset expression."
  (member (majutsu-transient-jj-option-arg)
          '("-r" "--revisions" "--revision" "--source" "--branch")))

(defun majutsu-transient-read-revset (prompt initial-input _history)
  "Read a revset value for transient infix options."
  (unless current-prefix-arg
    (let ((default (or initial-input (majutsu-transient-default-revset)))
          (completion-args (majutsu-transient-revset-completion-args)))
      (if (majutsu-transient-expression-revset-p)
          (majutsu-read-revset prompt default completion-args)
        (majutsu-read-single-revset prompt default completion-args)))))

(transient-define-argument majutsu-transient-arg-ignore-immutable ()
  :description "Ignore immutable"
  :class 'transient-switch
  :shortarg "-I"
  :argument "--ignore-immutable")

(defgroup majutsu-modes nil
  "Modes used or provided by Majutsu."
  :group 'majutsu)

(defgroup majutsu-buffers nil
  "Options concerning Majutsu buffers."
  :group 'majutsu
  :group 'majutsu-modes)

(defgroup majutsu-faces nil
  "Faces used by Majutsu."
  :group 'majutsu
  :group 'faces)

(defgroup majutsu-extensions nil
  "Extensions to Majutsu."
  :group 'majutsu)

(defgroup majutsu-related nil
  "Options relevant to Majutsu but defined elsewhere."
  :group 'majutsu
  :group 'majutsu-extensions
  :group 'majutsu-essentials)

;;; _
(provide 'majutsu-core)
;;; majutsu-core.el ends here
