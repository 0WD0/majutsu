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

(defun majutsu-transient-repository-default-key (namespace mode &optional create)
  "Return NAMESPACE/MODE's repository-local transient defaults key."
  (when-let* ((config-id (majutsu-repository-config-id create)))
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
         (values (copy-tree (get mode prop)))
         (entry (assoc config-id values)))
    (if entry
        (setcdr entry value)
      (push (cons config-id value) values))
    (put mode prop values)))

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
  (let ((key (or (majutsu-transient-repository-default-key namespace mode t)
                 (user-error "No jj repository config id available"))))
    (majutsu-transient-put-repository-current-value namespace mode value)
    (setf (alist-get key transient-values) value)
    (transient-save-values)))

(defclass majutsu-repository-transient-prefix (transient-prefix)
  ((namespace :initarg :namespace :initform nil)
   (defaults-key :initarg :defaults-key :initform nil)
   (current-property :initarg :current-property :initform nil)
   (default-property :initarg :default-property :initform nil)
   (remember-args :initarg :remember-args :initform nil))
  "Transient prefix with repository-local argument defaults.

This class is for simple transients whose exported value is just their
argument list.  More structured transients such as log and diff keep their
specialized prefix classes and call the generic repository-default helpers
directly.")

(defun majutsu-repository-transient--namespace (obj)
  "Return the repository-default namespace for OBJ."
  (or (oref obj namespace) (oref obj command)))

(defun majutsu-repository-transient--defaults-key (obj)
  "Return the repository-default key for OBJ."
  (or (oref obj defaults-key) (oref obj command)))

(defun majutsu-repository-transient--current-property (obj)
  "Return the global session property for OBJ."
  (or (oref obj current-property)
      (intern (format "%s-current-arguments"
                      (majutsu-repository-transient--defaults-key obj)))))

(defun majutsu-repository-transient--default-property (obj)
  "Return the fallback default property for OBJ."
  (or (oref obj default-property)
      (intern (format "%s-default-arguments"
                      (majutsu-repository-transient--defaults-key obj)))))

(defun majutsu-repository-transient--remembered-args (obj args)
  "Return the subset of ARGS remembered for OBJ's defaults."
  (if-let* ((remember (oref obj remember-args)))
      (funcall remember args)
    args))

(defun majutsu-repository-transient--current-args (obj)
  "Return OBJ's current arguments suitable for saving as defaults."
  (majutsu-repository-transient--remembered-args
   obj (transient-args (oref obj command))))

(cl-defmethod transient-init-value ((obj majutsu-repository-transient-prefix))
  (oset obj value
        (majutsu-transient-default-value
         (majutsu-repository-transient--namespace obj)
         (majutsu-repository-transient--defaults-key obj)
         (majutsu-repository-transient--current-property obj)
         (majutsu-repository-transient--default-property obj))))

(cl-defmethod transient-set-value ((obj majutsu-repository-transient-prefix))
  (let* ((obj (oref obj prototype))
         (namespace (majutsu-repository-transient--namespace obj))
         (key (majutsu-repository-transient--defaults-key obj))
         (args (majutsu-repository-transient--current-args obj)))
    (if-let* ((config-id (majutsu-repository-config-id)))
        (majutsu-transient-put-repository-current-value
         namespace key args config-id)
      (put key (majutsu-repository-transient--current-property obj) args))
    (transient--history-push obj)))

(cl-defmethod transient-save-value ((obj majutsu-repository-transient-prefix))
  (let* ((obj (oref obj prototype))
         (namespace (majutsu-repository-transient--namespace obj))
         (key (majutsu-repository-transient--defaults-key obj))
         (args (majutsu-repository-transient--current-args obj)))
    (put key (majutsu-repository-transient--current-property obj) args)
    (setf (alist-get (majutsu-transient-global-default-key namespace key)
                     transient-values)
          args)
    (transient-save-values)
    (transient--history-push obj)))

(cl-defgeneric majutsu-transient-save-repository-default (obj)
  "Save OBJ's current transient value as a repository-local default.")

(cl-defmethod majutsu-transient-save-repository-default ((_obj transient-prefix))
  (user-error "This transient does not support repository-local defaults"))

(cl-defmethod majutsu-transient-save-repository-default
  ((obj majutsu-repository-transient-prefix))
  (let* ((obj (oref obj prototype))
         (namespace (majutsu-repository-transient--namespace obj))
         (key (majutsu-repository-transient--defaults-key obj))
         (args (majutsu-repository-transient--current-args obj)))
    (majutsu-transient-save-repository-value namespace key args)
    (transient--history-push obj)
    (message "Saved %s arguments as repository defaults" key)))

(defun majutsu-transient-save-repository-defaults ()
  "Save current transient arguments as defaults for this jj repository."
  (interactive)
  (unless (and transient--prefix (eieio-object-p transient--prefix))
    (user-error "Not in a transient"))
  (majutsu-transient-save-repository-default transient--prefix))

;;; Shared Transients

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
