;;; majutsu-mode.el --- Base major mode for Majutsu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

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
  "N"   'majutsu-new-dwim
  "s"   'majutsu-squash
  "S"   'majutsu-split
  "d"   'majutsu-diff
  "D"   'majutsu-diff-dwim
  "r"   'majutsu-rebase
  "b"   'majutsu-bookmark
  "y"   'majutsu-duplicate
  "Y"   'majutsu-duplicate-dwim
  "G"   'majutsu-git-transient
  "Z"   'majutsu-workspace
  "%"   'majutsu-workspace
  "a"   'majutsu-abandon
  "k"   'majutsu-abandon
  "C-/" 'majutsu-undo
  "C-?" 'majutsu-redo
  "R"   'majutsu-restore)

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

(defun majutsu-get-mode-buffer (mode &optional value directory)
  "Return a buffer for DIRECTORY whose `major-mode' is MODE.

If VALUE is nil, return the first unlocked buffer.
If VALUE is non-nil, return the buffer locked to that value."
  (let ((topdir (or directory (majutsu--toplevel-safe))))
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
  (let* ((topdir (or directory (majutsu--toplevel-safe)))
         (repo (file-name-nondirectory (directory-file-name topdir)))
         (base (string-remove-suffix "-mode" (symbol-name mode)))
         (val (and value (format "%s" (ensure-list value)))))
    (format "*%s: %s%s*"
            base repo
            (if val (concat " " val) ""))))

(defun majutsu-generate-new-buffer (mode &optional value directory)
  "Generate a new Majutsu buffer for MODE in DIRECTORY."
  (let* ((topdir (or directory (majutsu--toplevel-safe)))
         (default-directory topdir)
         (name (majutsu--generate-buffer-name mode value topdir))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq majutsu--default-directory topdir)
      (setq majutsu-buffer-locked-p (and value t))
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
      (setq default-directory topdir)
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
           (lambda ()
             (let ((inhibit-read-only t))
               (erase-buffer)
               (save-excursion
                 (funcall fn))))))))

(defun majutsu--refresh-buffer-get-positions ()
  "Return positions for all windows displaying the current buffer.

The returned value is a list of entries of the form:
  (WINDOW SECTION LINE CHAR WS-SECTION WS-LINE WINDOW-START)."
  (let* ((buffer (current-buffer))
         (windows (nreverse (get-buffer-window-list buffer nil t)))
         positions)
    (dolist (window windows)
      (with-selected-window window
        (with-current-buffer buffer
          (when-let* ((section (magit-section-at)))
            (pcase-let* ((`(,line ,char)
                          (magit-section-get-relative-position section))
                         (ws (magit-section-at (window-start)))
                         (ws-line (and ws
                                       (car (magit-section-get-relative-position ws))))
                         (ws-start (and ws (window-start))))
              (push (list window section line char ws ws-line ws-start)
                    positions))))))
    (or (nreverse positions)
        (when-let* ((section (magit-section-at)))
          (pcase-let ((`(,line ,char)
                       (magit-section-get-relative-position section)))
            (list (list nil section line char nil nil nil)))))))

(defun majutsu--refresh-buffer-set-positions (positions)
  "Restore POSITIONS returned by `majutsu--refresh-buffer-get-positions'."
  (pcase-dolist
      (`(,window ,section ,line ,char ,ws-section ,ws-line ,window-start)
       positions)
    (if window
        (with-selected-window window
          (magit-section-goto-successor section line char)
          (unless (derived-mode-p 'majutsu-log-mode)
            (cond
             ((not window-start))
             ((> window-start (point)))
             ((and ws-section
                   (magit-section-equal ws-section
                                        (magit-section-at window-start)))
              (set-window-start window window-start t))
             (t
              (let ((pos (save-excursion
                           (and ws-section
                                (integerp ws-line)
                                (magit-section-goto-successor--same
                                 ws-section ws-line 0)
                                (point)))))
                (when pos
                  (set-window-start window pos t)))))))
      (let ((magit-section-movement-hook nil))
        (magit-section-goto-successor section line char)))))

(cl-defun majutsu-refresh-buffer-internal (&optional created &key initial-section select-section)
  "Refresh the current Majutsu buffer.

CREATED, INITIAL-SECTION, and SELECT-SECTION are for internal use."
  (when-let* ((refresh (majutsu--refresh-buffer-function)))
    (let ((action (if created "Creating" "Refreshing")))
      (when (eq major-mode 'majutsu-mode)
        (majutsu--debug "%s buffer `%s'..." action (buffer-name)))
      (cond
       (created
        (funcall refresh)
        (cond (initial-section (funcall initial-section))
              (select-section (funcall select-section))))
       (t
        (deactivate-mark)
        (setq magit-section-pre-command-section nil)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-selection-overlays nil)
        (setq magit-section-highlighted-sections nil)
        (setq magit-section-focused-sections nil)
        (let ((positions (majutsu--refresh-buffer-get-positions)))
          (funcall refresh)
          (cond (select-section (funcall select-section))
                ((majutsu--refresh-buffer-set-positions positions))))))
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
        (when-let* ((buffer (majutsu--find-mode-buffer 'majutsu-log-mode root)))
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

(defvar-local majutsu-buffer-log-args nil
  "Remembered log arguments for the current log buffer.")
(defvar-local majutsu-buffer-log-revsets nil
  "Revision set argument for the current log buffer.")
(defvar-local majutsu-buffer-log-filesets nil
  "Filesets filter for the current log buffer.")

(defvar-local majutsu-buffer-diff-args nil
  "Remembered diff formatting arguments for the current diff buffer.")
(defvar-local majutsu-buffer-diff-range nil
  "Remembered diff range arguments for the current diff buffer.

This is a list of `jj diff' range arguments, e.g.:

- (\"--revisions=@-\") or (\"--revisions=@-\" \"--revisions=main\")
- (\"--from=@-\" \"--to=@\")

When nil, jj's default range (\"-r @\") is used.")
(defvar-local majutsu-buffer-diff-filesets nil
  "Filesets filter for current diff buffer.")

;;; Display functions

(defcustom majutsu-default-display-function #'pop-to-buffer
  "Fallback function used to display Majutsu buffers.
The function must accept one argument: the buffer to display."
  :type '(choice
          (function-item switch-to-buffer)
          (function-item pop-to-buffer)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

(defcustom majutsu-display-functions
  '((log . pop-to-buffer)
    (op-log . pop-to-buffer)
    (process . pop-to-buffer)
    (diff . pop-to-buffer)
    (message . pop-to-buffer))
  "Alist mapping Majutsu buffer kinds to display functions.
Each function must accept one argument: the buffer to display.
Add new entries here to extend display behavior for additional buffers."
  :type '(alist :key-type (symbol :tag "Buffer kind")
          :value-type (choice
                       (function-item switch-to-buffer)
                       (function-item pop-to-buffer)
                       (function-item display-buffer)
                       (function :tag "Custom function")))
  :group 'majutsu)

(defun majutsu-display-function (kind)
  "Return display function for KIND or the default fallback."
  (or (alist-get kind majutsu-display-functions nil nil #'eq)
      majutsu-default-display-function))

;;; _
(provide 'majutsu-mode)

(with-eval-after-load 'evil
  (require 'majutsu-evil nil t))

;;; majutsu-mode.el ends here
