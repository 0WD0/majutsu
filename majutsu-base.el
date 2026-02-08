;;; majutsu-base.el --- Early utilities for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of early option/section plumbing are adapted from:
;; - Magit `lisp/magit-base.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors

;;; Commentary:

;; This library provides early utilities and section subclasses that
;; other Majutsu modules rely on while avoiding heavier dependencies.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eieio)
(require 'magit-section)

(declare-function majutsu-save-window-configuration "majutsu-mode" ())

;;; Options

(defgroup majutsu nil
  "Interface to jj version control system."
  :group 'tools)

(defcustom majutsu-debug nil
  "Enable debug logging for jj operations."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-display-buffer-function #'majutsu-display-buffer-traditional
  "The function used to display a Majutsu buffer.

All Majutsu buffers (buffers whose major-modes derive from
`majutsu-mode') are displayed using `majutsu-display-buffer',
which in turn uses the function specified here."
  :type '(radio (function-item majutsu-display-buffer-traditional)
          (function-item majutsu-display-buffer-same-window-except-diff-v1)
          (function-item majutsu-display-buffer-fullframe-log-v1)
          (function-item majutsu-display-buffer-fullframe-log-topleft-v1)
          (function-item majutsu-display-buffer-fullcolumn-most-v1)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

(defcustom majutsu-pre-display-buffer-hook (list #'majutsu-save-window-configuration)
  "Hook run by `majutsu-display-buffer' before displaying the buffer."
  :type 'hook
  :group 'majutsu)

(defcustom majutsu-post-display-buffer-hook (list #'majutsu-maybe-set-dedicated)
  "Hook run by `majutsu-display-buffer' after displaying the buffer."
  :type 'hook
  :group 'majutsu)

(defvar majutsu-display-buffer-noselect nil
  "If non-nil, `majutsu-display-buffer' doesn't call `select-window'.")

(defcustom majutsu-show-command-output t
  "Show jj command output in messages."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-confirm-critical-actions t
  "If non-nil, prompt for confirmation before critical operations."
  :type 'boolean
  :group 'majutsu)

(defconst majutsu--confirm-actions
  '((const undo)
    (const redo)
    (const abandon)
    (const rebase)
    (const workspace-forget))
  "Actions that may require confirmation.")

(defcustom majutsu-no-confirm nil
  "A list of symbols for actions Majutsu should not confirm, or t.

This is modeled after Magit's `magit-no-confirm`.  If this is t,
then no confirmation is required.  Otherwise, each symbol stands
for a class of actions that would normally ask for confirmation."
  :type `(choice (const :tag "Never require confirmation" t)
          (set   :tag "Require confirmation except for"
                 ,@majutsu--confirm-actions))
  :group 'majutsu)

(defcustom majutsu-slow-confirm nil
  "A list of actions that should use `yes-or-no-p' instead of `y-or-n-p'."
  :type `(choice (const :tag "Use yes-or-no for all" t)
          (set   :tag "Use yes-or-no for"
                 ,@majutsu--confirm-actions))
  :group 'majutsu)

;;; Section Classes

(defclass majutsu-commit-section (magit-section)
  ((overlay :initform nil
            :documentation "Selection overlay used by transient UIs.")
   (keymap :initform 'majutsu-commit-section-map)))

(defclass majutsu-diff-section (magit-section)
  ((keymap :initform 'majutsu-diff-section-map))
  :abstract t)

(defclass majutsu-file-section (majutsu-diff-section)
  ((keymap :initform 'majutsu-file-section-map)
   (header :initarg :header
           :initform nil
           :documentation "Raw file header text (diff --git + extended headers).")))

(defclass majutsu-hunk-section (majutsu-diff-section)
  ((keymap :initform 'majutsu-hunk-section-map)
   (fontified :initform nil)
   (combined :initarg :combined :initform nil)
   (from-range :initarg :from-range :initform nil)
   (from-ranges :initarg :from-ranges :initform nil)
   (to-range :initarg :to-range :initform nil)
   (about :initarg :about :initform nil)
   (painted :initform nil)
   (refined :initform nil)
   (heading-highlight-face :initform 'magit-diff-hunk-heading-highlight)
   (heading-selection-face :initform 'magit-diff-hunk-heading-selection)))

(setf (alist-get 'jj-commit magit--section-type-alist) 'majutsu-commit-section)
(setf (alist-get 'jj-file   magit--section-type-alist) 'majutsu-file-section)
(setf (alist-get 'jj-hunk   magit--section-type-alist) 'majutsu-hunk-section)

;; Workspace sections (`jj workspace list`)

(defclass majutsu-workspace-section (magit-section)
  ((keymap :initform 'majutsu-workspace-section-map)))

(setf (alist-get 'jj-workspace magit--section-type-alist) 'majutsu-workspace-section)

;;; Utilities

(defun majutsu--ensure-flag (args flag &optional position)
  "Return ARGS ensuring FLAG is present once.
POSITION may be `front' to insert FLAG at the beginning; otherwise FLAG
is appended."
  (if (member flag args)
      args
    (if (eq position 'front)
        (cons flag args)
      (append args (list flag)))))

(defun majutsu--debug (format-string &rest args)
  "Log debug message if `majutsu-debug' is enabled."
  (when majutsu-debug
    (message "[majutsu-mode] %s" (apply #'format format-string args))))

(defun majutsu--message-with-log (format-string &rest args)
  "Display message and log if debug is enabled."
  (let ((msg (apply #'format format-string args)))
    (majutsu--debug "User message: %s" msg)
    (message "%s" msg)))

(defun majutsu-y-or-n-p (prompt &optional action)
  "Ask PROMPT with y/n or yes/no depending on ACTION settings."
  (if (or (eq majutsu-slow-confirm t)
          (and action (member action majutsu-slow-confirm)))
      (yes-or-no-p prompt)
    (y-or-n-p prompt)))

(defun majutsu-confirm (action prompt)
  "Return non-nil if ACTION is confirmed.

ACTION is a symbol such as `rebase' or `abandon'.  PROMPT should
end with a question mark and space."
  (cond
   ((not majutsu-confirm-critical-actions) t)
   ((eq majutsu-no-confirm t) t)
   ((and action (memq action majutsu-no-confirm)) t)
   (t (majutsu-y-or-n-p prompt action))))

;;; Completing Read

(defun majutsu--make-completion-table (candidates &optional category)
  "Wrap CANDIDATES in a completion table.
When CATEGORY is non-nil, set it in metadata to control UI icons/styling."
  (let ((metadata `(metadata (display-sort-function . identity)
                    ,@(and category `((category . ,category))))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action candidates string pred)))))

(defun majutsu-completing-read (prompt collection &optional predicate require-match
                                       initial-input hist def category)
  "Read a choice with completion, preserving CATEGORY metadata.
Like `completing-read' but uses `format-prompt' and supports CATEGORY
for completion UI styling (icons, grouping)."
  (let ((table (if category
                   (majutsu--make-completion-table collection category)
                 collection)))
    (completing-read (format-prompt prompt def)
                     table predicate require-match
                     initial-input hist def)))

(defun majutsu-completing-read-multiple (prompt collection &optional predicate require-match
                                                initial-input hist def category)
  "Read multiple choices with completion, preserving CATEGORY metadata.
Like `completing-read-multiple' but uses `format-prompt' and supports CATEGORY."
  (let ((table (if category
                   (majutsu--make-completion-table collection category)
                 collection)))
    (completing-read-multiple (format-prompt prompt def)
                              table predicate require-match
                              initial-input hist def)))

(defun majutsu-read-string (prompt &optional initial-input history default-value)
  "Read a string from the minibuffer, prompting with PROMPT.
Uses `format-prompt' for consistent formatting.  Empty input returns
DEFAULT-VALUE if non-nil, otherwise signals an error."
  (let ((val (read-string (format-prompt prompt default-value)
                          initial-input history default-value)))
    (if (string-empty-p val)
        (if default-value
            default-value
          (user-error "Need non-empty input"))
      val)))

(defun majutsu-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'majutsu-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(majutsu-process-mode
                                majutsu-diff-mode
                                majutsu-log-mode))))
              '(display-buffer-same-window)
            nil)))

(defun majutsu-display-buffer-same-window-except-diff-v1 (buffer)
  "Display BUFFER in the selected window except for some modes.
If a buffer's `major-mode' derives from `majutsu-diff-mode' or
`majutsu-process-mode', display it in another window.  Display all
other buffers in the selected window."
  (display-buffer
   buffer (if (with-current-buffer buffer
                (derived-mode-p 'majutsu-diff-mode 'majutsu-process-mode))
              '(nil (inhibit-same-window . t))
            '(display-buffer-same-window))))

(defun majutsu--display-buffer-fullframe (buffer alist)
  (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

(defun majutsu-display-buffer-fullframe-log-v1 (buffer)
  "Display BUFFER, filling entire frame if BUFFER is a log buffer.
Otherwise, behave like `majutsu-display-buffer-traditional'."
  (if (eq (with-current-buffer buffer major-mode)
          'majutsu-log-mode)
      (display-buffer buffer '(majutsu--display-buffer-fullframe))
    (majutsu-display-buffer-traditional buffer)))

(defun majutsu--display-buffer-topleft (buffer alist)
  (or (display-buffer-reuse-window buffer alist)
      (when-let* ((window2 (display-buffer-pop-up-window buffer alist)))
        (let ((window1 (get-buffer-window))
              (buffer1 (current-buffer))
              (buffer2 (window-buffer window2))
              (w2-quit-restore (window-parameter window2 'quit-restore)))
          (set-window-buffer window1 buffer2)
          (set-window-buffer window2 buffer1)
          (select-window window2)
          ;; Swap some window state that `majutsu-mode-quit-window' and
          ;; `quit-restore-window' inspect.
          (set-window-prev-buffers window2 (cdr (window-prev-buffers window1)))
          (set-window-prev-buffers window1 nil)
          (set-window-parameter window2 'majutsu-dedicated
                                (window-parameter window1 'majutsu-dedicated))
          (set-window-parameter window1 'majutsu-dedicated t)
          (set-window-parameter window1 'quit-restore
                                (list 'window 'window
                                      (nth 2 w2-quit-restore)
                                      (nth 3 w2-quit-restore)))
          (set-window-parameter window2 'quit-restore nil)
          window1))))

(defun majutsu-display-buffer-fullframe-log-topleft-v1 (buffer)
  "Display BUFFER, filling entire frame if BUFFER is a log buffer.
When BUFFER derives from `majutsu-diff-mode' or `majutsu-process-mode',
try to display BUFFER to the top or left of the current buffer rather
than to the bottom or right, as `majutsu-display-buffer-fullframe-log-v1'
would.  Whether the split is made vertically or horizontally is determined
by `split-window-preferred-function'."
  (display-buffer
   buffer
   (cond ((eq (with-current-buffer buffer major-mode)
              'majutsu-log-mode)
          '(majutsu--display-buffer-fullframe))
         ((with-current-buffer buffer
            (derived-mode-p 'majutsu-diff-mode 'majutsu-process-mode))
          '(majutsu--display-buffer-topleft))
         ('(display-buffer-same-window)))))

(defun majutsu--display-buffer-fullcolumn (buffer alist)
  (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-below-selected buffer alist))))
    (delete-other-windows-vertically window)
    window))

(defun majutsu-display-buffer-fullcolumn-most-v1 (buffer)
  "Display BUFFER using the full column except in some cases.
For most cases where BUFFER's `major-mode' derives from
`majutsu-mode', display it in the selected window and grow that
window to the full height of the frame, deleting other windows in
that column as necessary.  However, display BUFFER in another
window if BUFFER's mode derives from `majutsu-process-mode', or if
BUFFER derives from `majutsu-diff-mode' while the current buffer
derives from `majutsu-log-mode'."
  (display-buffer
   buffer
   (cond ((and (or (bound-and-true-p majutsu-jjdescription-mode)
                   (derived-mode-p 'majutsu-log-mode
                                   'majutsu-op-log-mode))
               (with-current-buffer buffer
                 (derived-mode-p 'majutsu-diff-mode)))
          nil)
         ((with-current-buffer buffer
            (derived-mode-p 'majutsu-process-mode))
          nil)
         ('(majutsu--display-buffer-fullcolumn)))))

(defun majutsu-maybe-set-dedicated ()
  "Mark the selected window as dedicated if appropriate.

If a new window was created to display the buffer, then remember
that fact.  That information is used by `majutsu-mode-quit-window',
to determine whether the window should be deleted when its last
Majutsu buffer is buried."
  (let ((window (get-buffer-window (current-buffer))))
    (when (and (window-live-p window)
               (not (window-prev-buffers window)))
      (set-window-parameter window 'majutsu-dedicated t))))

(defun majutsu-display-buffer (buffer &optional display-function)
  "Display BUFFER in some window and maybe select it.

If optional DISPLAY-FUNCTION is non-nil, then use that to display
the buffer.  Otherwise use `majutsu-display-buffer-function', which
is the normal case.

Then, unless `majutsu-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `majutsu-pre-display-buffer-hook'
and `majutsu-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'majutsu-pre-display-buffer-hook))
  (let ((window (funcall (or display-function majutsu-display-buffer-function)
                         buffer)))
    (unless majutsu-display-buffer-noselect
      (let ((old-frame (selected-frame))
            (new-frame (window-frame window)))
        (select-window window)
        (unless (eq old-frame new-frame)
          (select-frame-set-input-focus new-frame)))))
  (with-current-buffer buffer
    (run-hooks 'majutsu-post-display-buffer-hook)))

(defun majutsu--buffer-root (&optional buffer)
  "Return the cached root for BUFFER (default `current-buffer').

This is used to match buffers to repositories when refreshing."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'majutsu--default-directory) majutsu--default-directory)))

(defun majutsu--get-mode-buffer (mode &optional selected)
  "Get a buffer in MODE.

If SELECTED is non-nil, then only consider buffers displayed in a window
of the selected frame."
  (let ((pred (if selected
                  (lambda (buf)
                    (and (eq (buffer-local-value 'major-mode buf) mode)
                         (get-buffer-window buf)))
                (lambda (buf)
                  (eq (buffer-local-value 'major-mode buf) mode)))))
    (or (and (not selected)
             (seq-find (lambda (buf)
                         (and (funcall pred buf)
                              (get-buffer-window buf)))
                       (buffer-list)))
        (seq-find pred (buffer-list)))))

(defun majutsu--find-mode-buffer (mode &optional root)
  "Return a live buffer in MODE for ROOT (or any repo when ROOT is nil)."
  (let ((root (or root (majutsu--buffer-root))))
    (seq-find (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p mode)
                       (or (null root)
                           (equal (majutsu--buffer-root buf) root)))))
              (buffer-list))))

(defun majutsu--resolve-mode-buffer (mode &optional root)
  "Prefer the current buffer if it is in MODE; otherwise find one for ROOT."
  (if (derived-mode-p mode)
      (current-buffer)
    (majutsu--find-mode-buffer mode root)))

(defun majutsu--assert-mode (mode)
  "Signal a user error unless the current buffer derives from MODE."
  (unless (derived-mode-p mode)
    (user-error "Command is only valid in %s buffers" mode)))

;;; Change at Point

(defvar majutsu-buffer-blob-revision)

(defun majutsu-revision-at-point ()
  "Return the change-id at point.
This checks multiple sources in order:
1. Section value (jj-commit section)
2. Blob buffer revision
3. Diff buffer revision"
  (or (magit-section-value-if 'jj-commit)
      (and (bound-and-true-p majutsu-buffer-blob-revision)
           majutsu-buffer-blob-revision)
      (and (derived-mode-p 'majutsu-diff-mode)
           (bound-and-true-p majutsu-buffer-diff-range)
           (let ((range majutsu-buffer-diff-range))
             (or (and (equal (car range) "-r") (cadr range))
                 (when-let* ((arg (seq-find (lambda (item) (string-prefix-p "--revisions=" item)) range)))
                   (substring arg (length "--revisions="))))))))

;;; _
(provide 'majutsu-base)
;;; majutsu-base.el ends here
