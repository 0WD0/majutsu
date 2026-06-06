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
(require 'transient)

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

;;; Repository-local transient defaults

(defun majutsu-repository-config-id-file (&optional root)
  "Return jj's repo config-id file below workspace ROOT."
  (when-let* ((root (or root (majutsu--buffer-root) (majutsu-toplevel))))
    (expand-file-name ".jj/repo/config-id" root)))

(defun majutsu-repository-config-id (&optional create)
  "Return jj's secure repo config id for the current repository.

When CREATE is non-nil, ask jj to create the repo config id first if it
does not exist yet."
  (when-let* ((file (majutsu-repository-config-id-file)))
    (when (and create (not (file-readable-p file)))
      (majutsu-jj-string "config" "path" "--repo"))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string))))))

(defun majutsu-transient-global-default-key (namespace mode)
  "Return NAMESPACE/MODE's global transient defaults key."
  (intern (format "%s:%s" namespace mode)))

(defun majutsu-transient-repository-default-key (namespace mode &optional config-id)
  "Return NAMESPACE/MODE's repository-local transient defaults key."
  (when-let* ((config-id (or config-id (majutsu-repository-config-id))))
    (intern (format "%s:%s:repo:%s" namespace mode config-id))))

(defun majutsu-transient--repository-current-property (namespace)
  "Return the property used for NAMESPACE's repo-local session values."
  (intern (format "%s-current-repository-values" namespace)))

(defun majutsu-transient-repository-current-entry (namespace mode &optional config-id)
  "Return NAMESPACE/MODE's repo-local session entry for CONFIG-ID."
  (when-let* ((config-id (or config-id (majutsu-repository-config-id))))
    (assoc config-id
           (get mode (majutsu-transient--repository-current-property namespace)))))

(defun majutsu-transient-repository-current-value (namespace mode &optional config-id)
  "Return NAMESPACE/MODE's repo-local session value for CONFIG-ID."
  (cdr (majutsu-transient-repository-current-entry namespace mode config-id)))

(defun majutsu-transient-put-repository-current-value
    (namespace mode value &optional config-id)
  "Set NAMESPACE/MODE's repo-local session VALUE for CONFIG-ID."
  (let* ((config-id (or config-id
                        (majutsu-repository-config-id t)
                        (user-error "No jj repository config id available")))
         (prop (majutsu-transient--repository-current-property namespace))
         (values (get mode prop))
         (entry (assoc config-id values)))
    (if entry
        (setcdr entry value)
      (put mode prop (cons (cons config-id value) values)))))

(defun majutsu-transient-default-value (namespace mode current-property default-property)
  "Return NAMESPACE/MODE's default value.

Precedence is repository-local session value, repository-local saved value,
global session value, global saved value, and finally DEFAULT-PROPERTY on MODE."
  (let* ((repo-key (majutsu-transient-repository-default-key namespace mode))
         (repo-current (majutsu-transient-repository-current-entry namespace mode))
         (repo-saved (and repo-key (assq repo-key transient-values)))
         (global-saved (assq (majutsu-transient-global-default-key namespace mode)
                             transient-values)))
    (cond
     (repo-current (cdr repo-current))
     (repo-saved (cdr repo-saved))
     ((plist-member (symbol-plist mode) current-property)
      (get mode current-property))
     (global-saved (cdr global-saved))
     (t
      (get mode default-property)))))

(defun majutsu-transient-save-repository-value (namespace mode value)
  "Persist VALUE as NAMESPACE/MODE's repository-local transient default."
  (let* ((config-id (or (majutsu-repository-config-id t)
                        (user-error "No jj repository config id available")))
         (key (majutsu-transient-repository-default-key namespace mode config-id)))
    (majutsu-transient-put-repository-current-value namespace mode value config-id)
    (setf (alist-get key transient-values) value)
    (transient-save-values)))

(defun majutsu-filesets-split-transient-value (value)
  "Return (ARGS FILESETS) from transient VALUE.
ARGS contains ordinary option arguments.  FILESETS contains the values
from `transient-files' groups, without the -- separator."
  (let (args filesets)
    (dolist (arg value)
      (if (and (consp arg) (equal (car arg) "--"))
          (setq filesets (append filesets (cdr arg)))
        (push arg args)))
    (list (nreverse args) filesets)))

(defun majutsu-filesets-build-transient-value (args filesets)
  "Return transient value for ARGS and FILESETS."
  (if filesets
      `(("--" ,@filesets) ,@args)
    args))

(defun majutsu-jj-append-filesets (args filesets)
  "Return jj ARGS followed by -- and FILESETS."
  (if filesets
      (append args (cons "--" filesets))
    args))

(defclass majutsu-repository-transient-prefix (transient-prefix)
  ((repo-namespace :initarg :repo-namespace :initform nil)
   (repo-key :initarg :repo-key :initform nil)
   (repo-filter :initarg :repo-filter :initform nil))
  "Transient prefix whose plain argument list has repository-local defaults.")

(defun majutsu-repository-transient--namespace (obj)
  "Return OBJ's repository-default namespace."
  (or (oref obj repo-namespace) (oref obj command)))

(defun majutsu-repository-transient--key (obj)
  "Return OBJ's repository-default key."
  (or (oref obj repo-key) (oref obj command)))

(defun majutsu-repository-transient--args (obj)
  "Return OBJ's current arguments for default storage."
  (let ((args (transient-args (oref obj command))))
    (if-let* ((filter (oref obj repo-filter)))
        (funcall filter args)
      args)))

(cl-defmethod transient-init-value ((obj majutsu-repository-transient-prefix))
  (let ((key (majutsu-repository-transient--key obj)))
    (oset obj value
          (majutsu-transient-default-value
           (majutsu-repository-transient--namespace obj)
           key
           (intern (format "%s-current-arguments" key))
           (intern (format "%s-default-arguments" key))))))

(cl-defmethod transient-set-value ((obj majutsu-repository-transient-prefix))
  (let* ((obj (oref obj prototype))
         (namespace (majutsu-repository-transient--namespace obj))
         (key (majutsu-repository-transient--key obj))
         (args (majutsu-repository-transient--args obj)))
    (if-let* ((config-id (majutsu-repository-config-id)))
        (majutsu-transient-put-repository-current-value
         namespace key args config-id)
      (put key (intern (format "%s-current-arguments" key)) args))
    (transient--history-push obj)))

(cl-defmethod transient-save-value ((obj majutsu-repository-transient-prefix))
  (let* ((obj (oref obj prototype))
         (namespace (majutsu-repository-transient--namespace obj))
         (key (majutsu-repository-transient--key obj))
         (args (majutsu-repository-transient--args obj)))
    (put key (intern (format "%s-current-arguments" key)) args)
    (setf (alist-get (majutsu-transient-global-default-key namespace key)
                     transient-values)
          args)
    (transient-save-values)
    (transient--history-push obj)))

(cl-defgeneric majutsu-transient--save-repository-defaults (obj)
  "Save OBJ's current transient value as repository-local defaults.")

(cl-defmethod majutsu-transient--save-repository-defaults ((_obj transient-prefix))
  (user-error "This transient does not support repository-local defaults"))

(cl-defmethod majutsu-transient--save-repository-defaults
  ((obj majutsu-repository-transient-prefix))
  (let* ((obj (oref obj prototype))
         (namespace (majutsu-repository-transient--namespace obj))
         (key (majutsu-repository-transient--key obj))
         (args (majutsu-repository-transient--args obj)))
    (majutsu-transient-save-repository-value namespace key args)
    (transient--history-push obj)
    (message "Saved %s arguments as repository defaults" key)))

(defun majutsu-transient-save-repository-defaults ()
  "Save current transient arguments as defaults for this jj repository."
  (interactive)
  (if (and transient--prefix (eieio-object-p transient--prefix))
      (majutsu-transient--save-repository-defaults transient--prefix)
    (user-error "Not in a transient")))

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
