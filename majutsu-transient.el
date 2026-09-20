;;; majutsu-transient.el --- Transient helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Shared Transient extensions used by Majutsu.

;;; Code:

(require 'cl-lib)
(require 'majutsu-jj)
(require 'majutsu-config)
(require 'subr-x)
(require 'transient)

(autoload 'majutsu--transient-with-selection-buffer "majutsu-selection")

(defclass majutsu-jj-transient-prefix (transient-prefix)
  ((jj-command :initarg :jj-command :initform nil))
  "Transient prefix backed by a jj command.

The `jj-command' slot is a string or list of strings used as native jj
completion context for revset readers.")

(defun majutsu-transient--jj-command-args (command)
  "Return COMMAND as a list of jj command arguments."
  (cond ((null command) nil)
        ((listp command) command)
        (t (list command))))

(defun majutsu-transient-revset-completion-args ()
  "Return jj native completion context for the active revset reader."
  (when-let* ((prefix (transient-prefix-object))
              ((cl-typep prefix 'majutsu-jj-transient-prefix))
              (command (majutsu-transient--jj-command-args
                        (oref prefix jj-command)))
              (suffix (transient-suffix-object))
              (argument (oref suffix argument)))
    (append command (list (string-remove-suffix "=" argument)))))

(defun majutsu-transient-read-revset (prompt initial-input history)
  "Read an optional revset expression for transient infix options."
  (unless current-prefix-arg
    (majutsu-read-revset
     prompt
     :allow-empty t
     :initial-input initial-input
     :history history
     :completion-args (majutsu-transient-revset-completion-args))))

(transient-define-argument majutsu-transient-arg-ignore-immutable ()
  :description "Ignore immutable"
  :class 'transient-switch
  :shortarg "-I"
  :argument "--ignore-immutable")

(defclass majutsu-transient-key-alias-suffix ()
  ((key-aliases :initarg :key-aliases :initform nil))
  "Mixin for suffixes that bind additional keys to the same item."
  :abstract t)

(cl-defgeneric majutsu-transient-key-aliases (obj)
  "Return additional key descriptions for OBJ.")

(cl-defmethod majutsu-transient-key-aliases ((obj majutsu-transient-key-alias-suffix))
  (ensure-list (oref obj key-aliases)))

(defun majutsu-transient--key-events (key)
  "Return the event list for key description or vector KEY."
  (listify-key-sequence (if (vectorp key) key (kbd key))))

(defun majutsu-transient-key= (a b)
  "Return non-nil when key descriptions or vectors A and B are equal."
  (equal (majutsu-transient--key-events a)
         (majutsu-transient--key-events b)))

(defun majutsu-transient-key-invoked-p (key)
  "Return non-nil when KEY invoked the current command."
  (majutsu-transient-key= key (this-command-keys-vector)))

(defun majutsu-transient--suffix-key-aliases (obj)
  "Return OBJ's effective, de-duplicated key aliases."
  (let (aliases)
    (dolist (key (ensure-list (majutsu-transient-key-aliases obj))
                 (nreverse aliases))
      (unless (or (null key)
                  (majutsu-transient-key= key (oref obj key))
                  (cl-find key aliases :test #'majutsu-transient-key=))
        (push key aliases)))))

(defun majutsu-transient--add-key-aliases (map)
  "Bind key aliases in MAP for the current transient."
  (dolist (obj transient--suffixes)
    (when (and (cl-typep obj 'majutsu-transient-key-alias-suffix)
               (not (oref obj inactive))
               (not (oref obj inapt)))
      (dolist (key (majutsu-transient--suffix-key-aliases obj))
        (let* ((kbd (kbd key))
               (cmd (oref obj command))
               (other (transient--lookup-key map kbd)))
          (cond
           ((not other)
            (define-key map kbd cmd))
           ((eq other cmd))
           (t
            (error "Cannot bind %S to %s; already bound to %s"
                   key cmd other)))))))
  map)

(cl-defmethod transient-format-key :around ((obj majutsu-transient-key-alias-suffix))
  (let ((transient-highlight-mismatched-keys
         (and transient-highlight-mismatched-keys
              (not (majutsu-transient--suffix-key-aliases obj)))))
    (cl-call-next-method)))

(cl-defmethod transient-format-key ((obj majutsu-transient-key-alias-suffix))
  (if-let* ((aliases (majutsu-transient--suffix-key-aliases obj)))
      (let* ((command (oref obj command))
             (key (mapconcat
                   (lambda (key)
                     (propertize key 'face (transient--key-face command key)))
                   (cons (oref obj key) aliases)
                   "/")))
        (when-let* ((width (oref transient--pending-group pad-keys)))
          (setq key (truncate-string-to-width key width nil ?\s)))
        key)
    (cl-call-next-method)))

(advice-add 'transient--make-transient-map
            :filter-return #'majutsu-transient--add-key-aliases)

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

(defun majutsu--transient-default-action-set (symbol value)
  "Set SYMBOL to VALUE after validating it as a key description."
  (unless (and (stringp value)
               (> (length value) 0)
               (condition-case nil
                   (let ((keys (kbd value)))
                     (or (stringp keys) (vectorp keys)))
                 (error nil)))
    (user-error "Invalid transient key: %S" value))
  (set-default symbol value))

(defcustom majutsu-transient-default-action "RET"
  "Key used for default action suffixes in Majutsu transients.

This controls Majutsu transient suffixes that perform the primary action
for their prefix, such as executing a rebase, squash, split, or upload.
When such a suffix also has a command-specific key, this key is bound as
an alias on the same menu row.  Set this to another key description, such
as \".\", to move the default action key."
  :group 'majutsu
  :type 'string
  :set #'majutsu--transient-default-action-set)

(defclass majutsu-transient-default-action-suffix
  (majutsu-transient-key-alias-suffix transient-suffix)
  ((advice* :initform #'majutsu--transient-with-selection-buffer))
  "Transient suffix with `majutsu-transient-default-action' as default key.")

(cl-defmethod majutsu-transient-key-aliases
  ((_obj majutsu-transient-default-action-suffix))
  (append (cl-call-next-method) (list majutsu-transient-default-action)))
;; Do not set `key' as a class initform: suffix prototypes are created at
;; load time, but this option should be read when each transient is shown.
(cl-defmethod transient--init-suffix-key ((obj majutsu-transient-default-action-suffix))
  (if (slot-boundp obj 'key)
      (cl-call-next-method)
    (oset obj key majutsu-transient-default-action)))

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

;;; Scoped defaults

(defun majutsu-transient--workspace-root ()
  "Return the current workspace root, if any."
  (or (majutsu--buffer-root) (majutsu-toplevel)))

(defun majutsu-transient--session-key (&optional root)
  "Return a workspace-local session key without creating a config identity."
  (file-name-as-directory
   (file-truename (or root (majutsu-transient--workspace-root) default-directory))))

(defun majutsu-transient-default-key (namespace key scope &optional create root)
  "Return NAMESPACE/KEY's saved key for SCOPE.
CREATE permits creating jj's config identity.  ROOT is the workspace root.
User and repo keys retain the existing on-disk representation."
  (pcase-exhaustive scope
    ('user (intern (format "%s:%s" namespace key)))
    ((or 'repo 'workspace)
     (when-let* ((id (majutsu-config-id scope create root)))
       (intern (format "%s:%s:%s:%s" namespace key scope id))))))

(defun majutsu-transient--session-property (namespace)
  "Return the property holding NAMESPACE's workspace session values."
  (intern (format "%s-current-workspace-values" namespace)))

(defun majutsu-transient-set-current-value (namespace key value)
  "Set NAMESPACE/KEY's VALUE for this workspace and Emacs session only."
  (let* ((property (majutsu-transient--session-property namespace))
         (values (get key property)))
    (setf (alist-get (majutsu-transient--session-key) values nil nil #'equal)
          value)
    (put key property values)))

(defun majutsu-transient--clear-current-value (namespace key)
  "Remove NAMESPACE/KEY's current workspace session override."
  (let ((property (majutsu-transient--session-property namespace)))
    (put key property
         (assoc-delete-all (majutsu-transient--session-key) (get key property)))))

(defun majutsu-transient--saved-entry (namespace key &optional root)
  "Return the most specific saved entry for NAMESPACE/KEY in ROOT.
An entry with a nil cdr is an explicit empty value, not an absent entry."
  (seq-some
   (lambda (scope)
     (when-let* ((saved-key (majutsu-transient-default-key
                             namespace key scope nil root)))
       (assq saved-key transient-values)))
   (if root '(workspace repo user) '(user))))

(defun majutsu-transient--default-entry (namespace key)
  "Return NAMESPACE/KEY's session or saved entry, including explicit nil."
  (let ((root (majutsu-transient--workspace-root)))
    (or (assoc (majutsu-transient--session-key (or root default-directory))
               (get key (majutsu-transient--session-property namespace)))
        (majutsu-transient--saved-entry namespace key root))))

(defun majutsu-transient-default-value (namespace key default-property)
  "Return NAMESPACE/KEY's defaults, falling back to DEFAULT-PROPERTY.
Precedence is workspace session, workspace saved, repo saved, user saved,
then DEFAULT-PROPERTY on KEY.  Each layer replaces the entire argument list."
  (let ((entry (majutsu-transient--default-entry namespace key)))
    (if entry (cdr entry) (get key default-property))))

(defun majutsu-transient--write-default-value (namespace key value scope remove)
  "Write or REMOVE NAMESPACE/KEY's VALUE at SCOPE."
  (let ((saved-key (majutsu-transient-default-key namespace key scope (not remove))))
    (unless (or saved-key remove)
      (user-error "No jj %s config identity available" scope))
    (when saved-key
      ;; Do not report an unsaved value as saved if writing the file fails.
      (let ((values (copy-alist transient-values)))
        (if remove
            (setq values (assq-delete-all saved-key values))
          (setf (alist-get saved-key values) value))
        (let ((transient-values values))
          (transient-save-values))
        (setq transient-values values)))
    (majutsu-transient--clear-current-value namespace key)))

(defun majutsu-transient-save-default-value (namespace key value scope)
  "Persist NAMESPACE/KEY's VALUE at SCOPE and clear this workspace's session value."
  (majutsu-transient--write-default-value namespace key value scope nil))

(defun majutsu-transient-clear-default-value (namespace key scope)
  "Remove NAMESPACE/KEY's SCOPE override and this workspace's session value.
Do not create a config identity or alter any other saved layer."
  (majutsu-transient--write-default-value namespace key nil scope t))

(defclass majutsu-defaults-prefix (majutsu-jj-transient-prefix)
  ((defaults-namespace :initarg :defaults-namespace :initform nil)
   (defaults-key :initarg :defaults-key :initform nil)
   (defaults-filter :initarg :defaults-filter :initform nil))
  "Prefix with workspace sessions and user/repo/workspace saved defaults.
Do not store session values on the prototype: that object is shared by
all workspaces.  The native init/set/save/reset methods are specialized
instead, while Transient still owns infix values, history and menu lifetime.")

(defun majutsu-transient--defaults-key (obj)
  "Return OBJ's (NAMESPACE . KEY) pair."
  (cons (or (oref obj defaults-namespace) (oref obj command))
        (or (and (slot-exists-p obj 'major-mode) (oref obj major-mode))
            (oref obj defaults-key)
            (oref obj command))))

(cl-defmethod clone ((obj majutsu-defaults-prefix) &rest params)
  ;; Transient resumes its stack by cloning with :value.  EIEIO maps that
  ;; initarg to default-value, but scoped initialization must treat it as
  ;; the live value rather than consulting workspace defaults again.
  (let ((copy (cl-call-next-method)))
    (when (plist-member params :value)
      (oset copy value (plist-get params :value)))
    copy))

(cl-defmethod transient-default-value ((obj majutsu-defaults-prefix))
  (pcase-let* ((`(,namespace . ,key) (majutsu-transient--defaults-key obj))
               (entry (majutsu-transient--default-entry namespace key))
               (property (intern (format "%s-default-arguments"
                                         (if (slot-exists-p obj 'major-mode)
                                             namespace key)))))
    (cond (entry (cdr entry))
          ((plist-member (symbol-plist key) property) (get key property))
          (t (cl-call-next-method)))))

(cl-defmethod transient-init-value ((obj majutsu-defaults-prefix))
  (oset obj value (transient-default-value obj)))

(defun majutsu-transient--savable-value (obj)
  "Collect OBJ's raw savable infix value, including native `unsavable' handling.
Use the same collector as Transient's set/save methods.  A prefix-specific
filter may further restrict persistence to safe, stable command options."
  (let ((value (transient--get-savable-value)))
    (if-let* ((filter (oref obj defaults-filter)))
        (funcall filter value)
      value)))

(cl-defmethod transient-init-value :around ((obj majutsu-defaults-prefix))
  ;; A stacked prefix is resumed with an explicit :value.  Preserve it;
  ;; re-reading defaults here would discard unsaved edits on C-g.
  (if (slot-boundp obj 'value)
      (oref obj value)
    (cl-call-next-method)))

(cl-defgeneric majutsu-transient-apply-value (obj value)
  "Apply OBJ's full command VALUE to the originating buffer only.
Never execute a command or change session or saved defaults.")

(cl-defmethod majutsu-transient-apply-value ((_obj majutsu-defaults-prefix) _value)
  nil)

(cl-defmethod transient-set-value ((obj majutsu-defaults-prefix))
  (majutsu--transient-with-selection-buffer
   (lambda ()
     (pcase-let ((`(,namespace . ,key) (majutsu-transient--defaults-key obj))
                 (value (majutsu-transient--savable-value obj)))
       (majutsu-transient-set-current-value namespace key value)
       (majutsu-transient-apply-value obj (transient-args (oref obj command)))
       (transient--history-push obj value)))))

(defun majutsu-transient--save-defaults (obj value saved-value scope)
  "Save SAVED-VALUE at SCOPE and apply full VALUE for OBJ."
  (pcase-let* ((`(,namespace . ,key) (majutsu-transient--defaults-key obj))
               (root (majutsu-transient--workspace-root))
               (overrides
                (cl-loop for level in '(workspace repo user)
                         until (eq level scope)
                         when (assq (majutsu-transient-default-key
                                     namespace key level nil root)
                                    transient-values)
                         collect (symbol-name level))))
    (majutsu-transient-save-default-value namespace key saved-value scope)
    (majutsu-transient-apply-value obj value)
    ;; Explicit VALUE is essential: the save submenu has no argument infixes.
    (transient--history-push obj saved-value)
    (message "Saved %s defaults%s" scope
             (if overrides
                 (format "; %s overrides still take precedence"
                         (string-join overrides ", "))
               ""))))

(cl-defmethod transient-save-value ((obj majutsu-defaults-prefix))
  (majutsu--transient-with-selection-buffer
   (lambda ()
     (majutsu-transient--save-defaults
      obj (transient-args (oref obj command))
      (majutsu-transient--savable-value obj) 'user))))

(cl-defmethod transient-reset-value ((obj majutsu-defaults-prefix))
  "Clear session and user defaults, the counterpart to `transient-save'.
More specific saved overrides remain intact.  Use the defaults menu to
remove those.  Buffer-only resets should use `transient-prefix-set'."
  (majutsu--transient-with-selection-buffer
   (lambda ()
     (pcase-let* ((`(,namespace . ,key) (majutsu-transient--defaults-key obj)))
       (majutsu-transient-clear-default-value namespace key 'user)
       (let ((value (transient-default-value obj)))
         (transient-prefix-set value)
         (transient--history-push obj value))
       (message "Cleared user and workspace session defaults; local saved overrides retained")))))

(defun majutsu-transient--defaults-action (scope &optional clear)
  "Save or CLEAR SCOPE using the parent captured by the defaults menu."
  (let* ((context (transient-scope 'majutsu-transient-defaults-menu))
         (buffer (plist-get context :buffer)))
    (unless (buffer-live-p buffer)
      (user-error "Defaults source buffer is no longer live"))
    (with-current-buffer buffer
      (let ((default-directory (plist-get context :directory))
            (obj (plist-get context :prefix)))
        (if clear
            (pcase-let ((`(,namespace . ,key) (majutsu-transient--defaults-key obj)))
              (majutsu-transient-clear-default-value namespace key scope)
              (message "Cleared %s defaults; current buffer options unchanged" scope))
          (majutsu-transient--save-defaults
           obj (plist-get context :value) (plist-get context :saved-value) scope))))))

(transient-define-suffix majutsu-transient-save-user-defaults ()
  "Save the parent prefix's arguments as user defaults."
  (interactive)
  (majutsu-transient--defaults-action 'user))

(transient-define-suffix majutsu-transient-save-repo-defaults ()
  "Save the parent prefix's arguments as repository defaults."
  (interactive)
  (majutsu-transient--defaults-action 'repo))

(transient-define-suffix majutsu-transient-save-workspace-defaults ()
  "Save the parent prefix's arguments as workspace defaults."
  (interactive)
  (majutsu-transient--defaults-action 'workspace))

(transient-define-suffix majutsu-transient-clear-user-defaults ()
  "Clear user defaults and the current workspace's session override."
  (interactive)
  (majutsu-transient--defaults-action 'user t))

(transient-define-suffix majutsu-transient-clear-repo-defaults ()
  "Clear repository defaults and the current workspace's session override."
  (interactive)
  (majutsu-transient--defaults-action 'repo t))

(transient-define-suffix majutsu-transient-clear-workspace-defaults ()
  "Clear workspace defaults and the current workspace's session override."
  (interactive)
  (majutsu-transient--defaults-action 'workspace t))

(transient-define-prefix majutsu-transient-defaults-menu ()
  "Save parent arguments or remove an override at an explicit scope.
Saving applies the arguments to the originating view but never executes
the parent's command.  Clearing leaves current buffer options unchanged."
  [["Save defaults"
    ("u" "User" majutsu-transient-save-user-defaults)
    ("r" "Repository" majutsu-transient-save-repo-defaults)
    ("w" "Workspace" majutsu-transient-save-workspace-defaults)]
   ["Clear override (inherit)"
    ("U" "User" majutsu-transient-clear-user-defaults)
    ("R" "Repository" majutsu-transient-clear-repo-defaults)
    ("W" "Workspace" majutsu-transient-clear-workspace-defaults)]]
  (interactive)
  ;; This command is a sub-prefix: use the exported parent, not the prefix
  ;; being initialized.  Native stacking preserves its infixes on C-g.
  (let ((parent transient-current-prefix))
    (unless (and parent (cl-typep parent 'majutsu-defaults-prefix))
      (user-error "Invoke this menu from a Majutsu defaults prefix"))
    (majutsu--transient-with-selection-buffer
     (lambda ()
       (let ((context (list :prefix parent
                            :value (transient-args (oref parent command))
                            :saved-value (majutsu-transient--savable-value parent)
                            :buffer (current-buffer)
                            :directory default-directory)))
         (transient-setup 'majutsu-transient-defaults-menu nil nil :scope context))))))

(provide 'majutsu-transient)
;;; majutsu-transient.el ends here
