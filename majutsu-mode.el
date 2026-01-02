;;; majutsu-mode.el --- Base major mode for Majutsu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library defines Majutsu's parent major mode, its keymap, and
;; shared buffer helpers.

;;; Code:

(require 'majutsu-base)
(require 'majutsu-jj)
(require 'cl-lib)
(require 'magit-section)
(require 'seq)
(require 'subr-x)

;;; Keymap

(defvar-keymap majutsu-mode-map
  :doc "Parent keymap for modes derived from `majutsu-mode'."
  :parent magit-section-mode-map
  "RET" 'majutsu-visit-thing
  "g"   'majutsu-refresh
  "q"   'quit-window
  "$"   'majutsu-process-buffer
  "l"   'majutsu-log-transient
  "?"   'majutsu-dispatch
  "c"   'majutsu-describe
  "C"   'majutsu-commit
  "N"   'majutsu-new
  "s"   'majutsu-squash-transient
  "d"   'majutsu-diff-transient
  "D"   'majutsu-diff-dwim
  "r"   'majutsu-rebase-transient
  "b"   'majutsu-bookmark-transient
  "y"   'majutsu-duplicate-transient
  "Y"   'majutsu-duplicate
  "G"   'majutsu-git-transient
  "a"   'majutsu-abandon
  "k"   'majutsu-abandon
  "C-/" 'majutsu-undo
  "C-?" 'majutsu-redo)

;;; Visit

(defun majutsu-visit-thing ()
  "Visit the thing at point.

This is a placeholder command.  Where applicable, section-specific
keymaps remap this command to another command that visits the thing at
  point."
  (declare (completion ignore))
  (interactive)
  (if-let* ((url (thing-at-point 'url t)))
      (browse-url url)
    (user-error "There is no thing at point that could be visited")))

;;; Helpers

(defvar majutsu-inhibit-refresh nil
  "When non-nil, inhibit refreshing Majutsu buffers.")

(defvar-local majutsu--default-directory nil
  "Value of `default-directory' when the buffer was generated.

This exists to prevent a let-bound `default-directory' from
tricking buffer reuse logic into thinking a buffer belongs to a
repository that it doesn't.")
(put 'majutsu--default-directory 'permanent-local t)

(defvar-local majutsu-buffer-locked-p nil
  "Whether this buffer is locked to its `majutsu-buffer-value'.")
(put 'majutsu-buffer-locked-p 'permanent-local t)

(defvar-local majutsu-previous-section nil
  "Section that was current before the buffer was refreshed/reused.")
(put 'majutsu-previous-section 'permanent-local t)

(cl-defgeneric majutsu-buffer-value ()
  "Return the value of the current buffer.

The \"value\" identifies what is being displayed in the buffer.
Buffers that are locked use this value to avoid being reused for
another value."
  nil)

(defcustom majutsu-create-buffer-hook nil
  "Normal hook run when a new Majutsu buffer is created by `majutsu-setup-buffer'."
  :type 'hook
  :group 'majutsu)

(defcustom majutsu-setup-buffer-hook nil
  "Normal hook run by `majutsu-setup-buffer' after displaying the buffer."
  :type 'hook
  :group 'majutsu)

(defcustom majutsu-post-create-buffer-hook nil
  "Normal hook run by `majutsu-setup-buffer' after the initial refresh."
  :type 'hook
  :group 'majutsu)

(defcustom majutsu-refresh-buffer-hook nil
  "Normal hook for `majutsu-refresh-buffer-internal' to run after refreshing."
  :type 'hook
  :group 'majutsu)

(defun majutsu--toplevel-safe (&optional directory)
  "Return repository root for DIRECTORY or `default-directory'."
  (let ((default-directory (or directory default-directory)))
    (or (ignore-errors (majutsu--root))
        (user-error "Not inside a jj repository"))))

(defun majutsu-get-mode-buffer (mode &optional value directory)
  "Return a buffer for DIRECTORY whose `major-mode' is MODE.

If VALUE is nil, return the first unlocked buffer.
If VALUE is non-nil, return the buffer locked to that value."
  (let ((topdir (majutsu--toplevel-safe directory)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (and (eq major-mode mode)
              (equal majutsu--default-directory topdir)
              (if value
                  (and majutsu-buffer-locked-p
                       (equal (majutsu-buffer-value) value))
                (not majutsu-buffer-locked-p))
              buffer)))
     (buffer-list))))

(defun majutsu--generate-buffer-name (mode &optional value directory)
  (let* ((default-directory (or directory default-directory))
         (topdir (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name topdir)))
         (base (string-remove-suffix "-mode" (symbol-name mode)))
         (val (and value (format "%s" (ensure-list value)))))
    (format "*%s: %s%s*"
            base repo
            (if val (concat " " val) ""))))

(defun majutsu-generate-new-buffer (mode &optional value directory)
  "Generate a new Majutsu buffer for MODE in DIRECTORY."
  (let* ((topdir (majutsu--toplevel-safe directory))
         (name (majutsu--generate-buffer-name mode value topdir))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq majutsu--default-directory topdir)
      (setq majutsu-buffer-locked-p (and value t))
      (setq-local majutsu--repo-root topdir)
      (setq default-directory topdir))
    buffer))

(defmacro majutsu-setup-buffer (mode &optional locked &rest args)
  "\n\n(fn MODE &optional LOCKED &key BUFFER DIRECTORY \
INITIAL-SECTION SELECT-SECTION &rest BINDINGS)"
  (declare (indent 2)
           (debug (form [&optional locked]
                        [&rest keywordp form]
                        [&rest (symbolp form)])))
  (let (kwargs)
    (while (keywordp (car args))
      (push (pop args) kwargs)
      (push (pop args) kwargs))
    `(majutsu-setup-buffer-internal
      ,mode ,locked
      ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                             `(list ',var ,form))
                           args))
      ,@(nreverse kwargs))))

(defun majutsu--mode-kind (mode)
  "Infer display kind from MODE symbol name.

For modes named like `majutsu-FOO-mode', return the symbol `FOO'."
  (when (symbolp mode)
    (let ((name (symbol-name mode)))
      (when (string-match "\\`majutsu-\\(.*\\)-mode\\'" name)
        (intern (match-string 1 name))))))

(cl-defun majutsu-setup-buffer-internal
    (mode locked bindings
          &key buffer directory initial-section select-section)
  (let* ((topdir (majutsu--toplevel-safe directory))
         (kind (majutsu--mode-kind mode))
         (value (and locked
                     (with-temp-buffer
                       (pcase-dolist (`(,var ,val) bindings)
                         (set (make-local-variable var) val))
                       (let ((major-mode mode)
                             (default-directory topdir))
                         (majutsu-buffer-value)))))
         (buffer (if buffer
                     (get-buffer-create buffer)
                   (majutsu-get-mode-buffer mode value topdir)))
         (section (and buffer (magit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (majutsu-generate-new-buffer mode value topdir)))
    (with-current-buffer buffer
      (setq majutsu-previous-section section)
      (setq majutsu--default-directory topdir)
      (setq-local majutsu--repo-root topdir)
      (when directory
        (setq default-directory directory))
      (funcall mode)
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        (run-hooks 'majutsu-create-buffer-hook)))
    (majutsu-display-buffer buffer kind)
    (with-current-buffer buffer
      (run-hooks 'majutsu-setup-buffer-hook)
      (majutsu-refresh-buffer-internal created
                                       :initial-section initial-section
                                       :select-section select-section)
      (when created
        (run-hooks 'majutsu-post-create-buffer-hook)))
    buffer))

(defun majutsu--refresh-buffer-function ()
  "Return the refresh function for the current Majutsu buffer, if any.
The function name is derived from `major-mode' by replacing the
\"-mode\" suffix with \"-refresh-buffer\", mirroring Magit's approach."
  (when (derived-mode-p 'majutsu-mode)
    (let* ((base (string-remove-suffix "-mode" (symbol-name major-mode)))
           (fn (intern (format "%s-refresh-buffer" base))))
      (and (fboundp fn)
           (not (memq fn '(majutsu-refresh
                           majutsu-refresh-buffer
                           majutsu-refresh-buffer-internal)))
           fn))))

(cl-defun majutsu-refresh-buffer-internal (&optional created &key initial-section select-section)
  "Refresh the current Majutsu buffer.

CREATED, INITIAL-SECTION, and SELECT-SECTION are for internal use."
  (when-let ((refresh (majutsu--refresh-buffer-function)))
    (let* ((action (if created "Creating" "Refreshing"))
           (pos (and (not created)
                     (when-let ((section (magit-section-at)))
                       (pcase-let ((`(,line ,char)
                                    (magit-section-get-relative-position section)))
                         (list section line char))))))
      (when (eq major-mode 'majutsu-mode)
        (majutsu--debug "%s buffer `%s'..." action (buffer-name)))
      (cond
       (created
        (funcall refresh)
        (cond (initial-section (funcall initial-section))
              (select-section (funcall select-section))))
       (t
        (deactivate-mark)
        (funcall refresh)
        (cond (select-section (funcall select-section))
              ((and pos
                    (let ((section (nth 0 pos))
                          (line (nth 1 pos))
                          (char (nth 2 pos)))
                      (let ((magit-section-movement-hook nil))
                        (magit-section-goto-successor section line char))))))))
      (let ((magit-section-cache-visibility nil))
        (when (bound-and-true-p magit-root-section)
          (magit-section-show magit-root-section)))
      (run-hooks 'majutsu-refresh-buffer-hook)
      (magit-section-update-highlight)
      (set-buffer-modified-p nil))))

(defun majutsu-refresh-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the current Majutsu buffer.

This is suitable for use as `revert-buffer-function'."
  (interactive)
  (majutsu--assert-mode 'majutsu-mode)
  (if (majutsu--refresh-buffer-function)
      (majutsu-refresh-buffer-internal)
    (user-error "No refresh function defined for %s" major-mode)))

(defun majutsu-refresh ()
  "Refresh Majutsu buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`majutsu-mode', and refresh the corresponding log buffer."
  (interactive)
  (unless majutsu-inhibit-refresh
    (let ((root (majutsu--buffer-root)))
      (when (derived-mode-p 'majutsu-mode)
        (if (called-interactively-p 'interactive)
            (majutsu-refresh-buffer)
          (ignore-errors (majutsu-refresh-buffer))))
      (when (and root
                 (not (derived-mode-p 'majutsu-log-mode))
                 (fboundp 'majutsu-log-refresh))
        (when-let ((buffer (majutsu--find-mode-buffer 'majutsu-log-mode root)))
          (with-current-buffer buffer
            (ignore-errors (majutsu-log-refresh))))))))

(defun majutsu-hack-dir-local-variables ()
  "Like `hack-dir-local-variables-non-file-buffer' but ignore some variables.
This prevents visual glitches (like red trailing whitespace) in Majutsu buffers
when the user has strict .dir-locals.el settings."
  (let ((ignored-local-variables
         (cons 'show-trailing-whitespace ignored-local-variables)))
    (hack-dir-local-variables-non-file-buffer)))

;;; Mode definition

(define-derived-mode majutsu-mode magit-section-mode "Majutsu"
  "Parent major mode from which Majutsu major modes inherit."
  :interactive nil
  :group 'majutsu
  (majutsu-hack-dir-local-variables))

;;; Local Variables

(defvar-local majutsu-buffer-diff-args nil
  "Remembered diff formatting arguments for the current diff buffer.")
(defvar-local majutsu-buffer-diff-revsets nil
  "Revision arguments for current diff buffer.")
(defvar-local majutsu-buffer-diff-filesets nil
  "Filesets filter for current diff buffer.")

;;; _
(provide 'majutsu-mode)

(with-eval-after-load 'evil
  (require 'majutsu-evil nil t))

;;; majutsu-mode.el ends here
