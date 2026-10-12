;;; majutsu-section-edit.el --- Edit section fields in place  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Majutsu and Magit buffers are read-only: their text is generated from
;; `jj' output and rebuilt wholesale on refresh.  This library lets a
;; section nominate regions of its generated text as editable fields.
;;
;; The buffer becomes writable during a session, but everything except
;; the fields carries the `read-only' text property (see Info node
;; `(elisp) Special Properties'); the fields are holes in that property.
;; Ordinary editing commands work in the fields while every other section
;; stays protected.  The protected character preceding a field is made
;; rear-nonsticky for `read-only' so that typing at the start of a
;; mid-line field is allowed.  Overlays are deliberately not used for
;; protection: Emacs does not enforce `read-only' on overlays.
;;
;; A session makes every field in the accessible buffer editable at once,
;; like Wdired or wgrep.  Finishing collects the changed fields and hands
;; them, as a list of (FIELD . NEW-TEXT), to the buffer's apply function
;; (`majutsu-section-edit-apply-function').  That lets a log buffer map
;; several edited rows onto a single bulk `jj describe' transaction.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'majutsu-mode)
(require 'seq)

(defvar majutsu-section-edit-mode)

(defvar-keymap majutsu-section-edit-mode-map
  :doc "Keymap for `majutsu-section-edit-mode'."
  "C-c C-c" #'majutsu-section-edit-finish
  "C-c C-k" #'majutsu-section-edit-abort
  "C-x C-q" #'majutsu-section-edit-exit)

;; The major mode's local map binds single letters to Majutsu commands, so a
;; session temporarily replaces it.  Unbound printable characters then
;; self-insert instead of running `majutsu-abandon' and friends.
(defvar-keymap majutsu-section-edit-local-map
  :doc "Local keymap installed during a section-edit session."
  "C-c C-c" #'majutsu-section-edit-finish
  "C-c C-k" #'majutsu-section-edit-abort
  "C-x C-q" #'majutsu-section-edit-exit)

(defvar majutsu-section-edit-property 'majutsu-section-edit
  "Text property holding the `majutsu-section-edit-field' for a region.")

(cl-defstruct (majutsu-section-edit-field
               (:constructor majutsu-section-edit-make-field))
  "An editable region inside a read-only Majutsu or Magit section.

KEY is an arbitrary identity used to recognise the field.  MULTILINE
controls whether newlines may be typed.  DATA is an arbitrary plist that
the buffer's `majutsu-section-edit-apply-function' interprets."
  (key nil)
  (multiline nil)
  (data nil))

;;; Session state

(defvar-local majutsu-section-edit--entries nil
  "Active session fields, as plists with :field, :beg, :end and :original.")

(defvar-local majutsu-section-edit--inhibit-change nil
  "Non-nil while the session itself modifies the buffer.")

(defvar-local majutsu-section-edit-apply-function nil
  "Function applying the changed fields of the current buffer.
It is called with a list of (FIELD . NEW-TEXT) for every field whose text
differs from the snapshot taken when the session started.  It should
return non-nil when the changes were applied.  Major modes set this when
they mark fields with `majutsu-section-edit-put'.")

;;; Field lookup and marking

(defun majutsu-section-edit-field-at (&optional pos)
  "Return the editable field at POS, or nil."
  (get-text-property (or pos (point)) majutsu-section-edit-property))

(defun majutsu-section-edit-put (beg end field)
  "Mark BEG..END as editable FIELD."
  (put-text-property beg end majutsu-section-edit-property field))

(defun majutsu-section-edit--first-field-position (beg end)
  "Return the first position in BEG..END carrying the field property."
  (let ((pos beg) found)
    (while (and (not found) (< pos end))
      (if (get-text-property pos majutsu-section-edit-property)
          (setq found pos)
        (setq pos (next-single-property-change
                   pos majutsu-section-edit-property nil end))))
    found))

(defun majutsu-section-edit--field-position ()
  "Return the start of the editable field relevant at point, or nil.
Point itself, the character before point, and finally the current section
are consulted, so a command bound on the section heading can still find a
field that lives in the heading body."
  (or (and (get-text-property (point) majutsu-section-edit-property)
           (point))
      (and (> (point) (point-min))
           (get-text-property (1- (point)) majutsu-section-edit-property)
           (1- (point)))
      (when-let* ((section (magit-current-section)))
        (majutsu-section-edit--first-field-position
         (oref section start) (oref section end)))))

(defun majutsu-section-edit--collect (&optional beg end)
  "Return session entries for every editable field in BEG..END.
BEG and END default to the accessible range."
  (let ((pos (or beg (point-min)))
        (end (or end (point-max)))
        entries)
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos majutsu-section-edit-property nil end)))
        (when-let* ((field (get-text-property pos majutsu-section-edit-property)))
          (push (list :field field
                      :beg (copy-marker pos nil)
                      :end (copy-marker next t)
                      :original (buffer-substring-no-properties pos next)
                      :sticky nil
                      :sticky-old nil)
                entries))
        (setq pos next)))
    (nreverse entries)))

;;; Session bookkeeping

(defvar-local majutsu-section-edit--saved-read-only nil
  "Value of `buffer-read-only' before the session made the buffer writable.")

(defvar-local majutsu-section-edit--saved-local-map nil
  "Local map replaced by `majutsu-section-edit-local-map' for the session.")

(defvar-local majutsu-section-edit--undo-was-disabled nil
  "Non-nil when the buffer had undo disabled before the session.
Magit section buffers call `buffer-disable-undo'; the session enables undo
so that in-place edits can be reverted, then restores the disabled state")

(defcustom majutsu-section-edit-cursor-type 'hollow
  "Cursor type used while `majutsu-section-edit-mode' is active.
A hollow cursor distinguishes an editable session from the read-only
buffer it was started in, mirroring `majutsu-blob-edit-cursor-type'."
  :group 'majutsu
  :type 'symbol)

(defvar-local majutsu-section-edit--saved-cursor-type nil
  "Cursor type before entering `majutsu-section-edit-mode'.")

(defvar-local majutsu-section-edit--saved-evil-normal-state-cursor nil
  "Saved `evil-normal-state-cursor' before the session.")

(defvar-local majutsu-section-edit--saved-evil-normal-state-cursor-valid nil
  "Whether `majutsu-section-edit--saved-evil-normal-state-cursor' is valid.")

(defun majutsu-section-edit--set-cursor-hint (cursor)
  "Set the session cursor hint to CURSOR.
When Evil is active, also update `evil-normal-state-cursor' so the hint is
visible while the session sits in Normal state."
  (setq-local cursor-type cursor)
  (when (and (bound-and-true-p evil-local-mode)
             (boundp 'evil-normal-state-cursor))
    (setq-local evil-normal-state-cursor cursor)))

(defun majutsu-section-edit--save-cursor-state ()
  "Save current cursor state before entering the session."
  (setq-local majutsu-section-edit--saved-cursor-type cursor-type)
  (if (and (bound-and-true-p evil-local-mode)
           (boundp 'evil-normal-state-cursor))
      (progn
        (setq-local majutsu-section-edit--saved-evil-normal-state-cursor
                    evil-normal-state-cursor)
        (setq-local majutsu-section-edit--saved-evil-normal-state-cursor-valid t))
    (setq-local majutsu-section-edit--saved-evil-normal-state-cursor-valid nil)))

(defun majutsu-section-edit--restore-cursor-state ()
  "Restore cursor state after leaving the session."
  (setq-local cursor-type majutsu-section-edit--saved-cursor-type)
  (when (and majutsu-section-edit--saved-evil-normal-state-cursor-valid
             (boundp 'evil-normal-state-cursor))
    (setq-local evil-normal-state-cursor
                majutsu-section-edit--saved-evil-normal-state-cursor)
    (setq-local majutsu-section-edit--saved-evil-normal-state-cursor-valid nil)))

(defun majutsu-section-edit--enter-editing-state ()
  "Replace the major mode's local map for the session.
The map is restored by `majutsu-section-edit--leave-editing-state'."
  (setq majutsu-section-edit--saved-local-map (current-local-map))
  (use-local-map majutsu-section-edit-local-map))

(defun majutsu-section-edit--leave-editing-state ()
  "Restore the local map saved by the session."
  (when majutsu-section-edit--saved-local-map
    (use-local-map majutsu-section-edit--saved-local-map)
    (setq majutsu-section-edit--saved-local-map nil)))

(defun majutsu-section-edit--expose-entry (entry)
  "Clear the `read-only' property over session ENTRY.
Insertion before a read-only character inherits that property by default,
so the protected character preceding the field is made rear-nonsticky for
`read-only'.  This is what lets point type at the very start of a field
in the middle of a line."
  (let ((beg (marker-position (plist-get entry :beg)))
        (end (marker-position (plist-get entry :end))))
    (when (and beg end (< beg end))
      (with-silent-modifications
        (remove-text-properties beg end '(read-only nil))))
    (when (and beg (> beg (point-min)) (not (plist-get entry :sticky)))
      (let ((old (get-text-property (1- beg) 'rear-nonsticky)))
        (plist-put entry :sticky (copy-marker (1- beg) nil))
        (plist-put entry :sticky-old old)
        (with-silent-modifications
          (put-text-property
           (1- beg) beg 'rear-nonsticky
           (cond ((eq old t) t)
                 ((listp old) (cons 'read-only old))
                 (t '(read-only)))))))))

(defun majutsu-section-edit--expose ()
  "Make the session's fields writable while protecting everything else.
The whole accessible buffer gets the `read-only' text property; the
fields are holes in it.  Unlike a buffer-wide `buffer-read-only', this
permits insertion exactly at a field's boundaries."
  (with-silent-modifications
    (put-text-property (point-min) (point-max) 'read-only t)
    ;; With no character on the left, insertion at the start of the buffer
    ;; would otherwise not inherit `read-only'; make the first character
    ;; front-sticky unless it is itself an editable field.
    (unless (get-text-property (point-min) majutsu-section-edit-property)
      (put-text-property (point-min) (1+ (point-min))
                         'front-sticky '(read-only))))
  (setq majutsu-section-edit--undo-was-disabled (eq buffer-undo-list t))
  (buffer-enable-undo)
  (setq buffer-undo-list nil)
  (setq majutsu-section-edit--saved-read-only buffer-read-only)
  (setq buffer-read-only nil)
  (majutsu-section-edit--enter-editing-state)
  (dolist (entry majutsu-section-edit--entries)
    (majutsu-section-edit--expose-entry entry)))

(defun majutsu-section-edit--flatten-entry (entry)
  "Replace newlines inside a single-line ENTRY with spaces."
  (let ((beg (marker-position (plist-get entry :beg)))
        (end (marker-position (plist-get entry :end))))
    (when (and beg end (< beg end))
      (with-silent-modifications
        (save-excursion
          (goto-char beg)
          (while (search-forward "\n" end t)
            (replace-match " " t t)))))))

(defun majutsu-section-edit--overlapping (beg end)
  "Return session entries overlapping the change BEG..END."
  (seq-filter (lambda (entry)
                (let ((ebeg (marker-position (plist-get entry :beg)))
                      (eend (marker-position (plist-get entry :end))))
                  (and ebeg eend (<= ebeg end) (>= eend beg))))
              majutsu-section-edit--entries))

(defun majutsu-section-edit--after-change (beg end _length)
  "Keep edited fields single-line after a change.
The read-only protection is self-maintaining: insertions outside a field
are refused (or inherit `read-only'), so only newlines inside fields need
to be flattened."
  (unless majutsu-section-edit--inhibit-change
    (let ((majutsu-section-edit--inhibit-change t))
      (dolist (entry (majutsu-section-edit--overlapping beg end))
        (unless (majutsu-section-edit-field-multiline (plist-get entry :field))
          (majutsu-section-edit--flatten-entry entry))))))

(defun majutsu-section-edit--changed ()
  "Return the changed fields of the session as (FIELD . NEW-TEXT) pairs."
  (cl-loop for entry in majutsu-section-edit--entries
           for beg = (marker-position (plist-get entry :beg))
           for end = (marker-position (plist-get entry :end))
           for new = (buffer-substring-no-properties beg end)
           unless (equal new (plist-get entry :original))
           collect (cons (plist-get entry :field) new)))

(defun majutsu-section-edit--stop ()
  "Leave the current edit session without touching repository state."
  (with-silent-modifications
    (dolist (entry majutsu-section-edit--entries)
      (when-let* ((sticky (plist-get entry :sticky)))
        (let ((pos (marker-position sticky))
              (old (plist-get entry :sticky-old)))
          (cond ((eq old t)
                 (put-text-property pos (1+ pos) 'rear-nonsticky t))
                (old
                 (put-text-property pos (1+ pos) 'rear-nonsticky old))
                (t
                 (remove-text-properties pos (1+ pos) '(rear-nonsticky nil)))))
        (set-marker sticky nil))
      (set-marker (plist-get entry :beg) nil)
      (set-marker (plist-get entry :end) nil))
    (remove-text-properties (point-min) (point-max) '(read-only nil))
    (remove-text-properties (point-min) (1+ (point-min)) '(front-sticky nil)))
  (majutsu-section-edit--leave-editing-state)
  (setq buffer-read-only majutsu-section-edit--saved-read-only)
  (when majutsu-section-edit--undo-was-disabled
    (buffer-disable-undo))
  (setq majutsu-section-edit--entries nil)
  (remove-hook 'after-change-functions #'majutsu-section-edit--after-change t)
  (majutsu-section-edit-mode -1))

;;; Commands

(defun majutsu-section-edit ()
  "Make every editable field in this buffer writable for in-place editing.

Change any number of fields, then finish with
\\[majutsu-section-edit-finish]; abandon the edit with
\\[majutsu-section-edit-abort]."
  (interactive)
  (unless majutsu-section-edit-apply-function
    (user-error "This buffer has no editable fields"))
  (when majutsu-section-edit-mode
    (majutsu-section-edit--stop))
  (setq majutsu-section-edit--entries (majutsu-section-edit--collect))
  (unless majutsu-section-edit--entries
    (user-error "No editable fields in this buffer"))
  (when-let* ((pos (majutsu-section-edit--field-position)))
    (goto-char pos))
  (majutsu-section-edit--expose)
  (add-hook 'after-change-functions #'majutsu-section-edit--after-change nil t)
  (majutsu-section-edit-mode 1)
  (message "%d editable field%s; C-c C-c to save, C-c C-k to abort"
           (length majutsu-section-edit--entries)
           (if (= (length majutsu-section-edit--entries) 1) "" "s")))

(defun majutsu-section-edit-finish ()
  "Apply the session's edits with one buffer-level apply function."
  (interactive)
  (unless majutsu-section-edit-mode
    (user-error "No active section edit"))
  (let ((changed (majutsu-section-edit--changed))
        (apply-fn majutsu-section-edit-apply-function))
    (if (null changed)
        (progn
          (majutsu-section-edit--stop)
          (message "No changes to apply"))
      (if (funcall apply-fn changed)
          (progn
            (majutsu-section-edit--stop)
            (majutsu-refresh))
        (majutsu-section-edit--expose)
        (user-error "Could not apply the edits")))))

(defun majutsu-section-edit-abort ()
  "Discard the active field edit and refresh the buffer."
  (interactive)
  (unless majutsu-section-edit-mode
    (user-error "No active section edit"))
  (majutsu-section-edit--stop)
  (majutsu-refresh)
  (message "Section edit aborted"))

(defun majutsu-section-edit-exit ()
  "Leave the session, asking whether to save pending edits."
  (interactive)
  (unless majutsu-section-edit-mode
    (user-error "No active section edit"))
  (if (majutsu-section-edit--changed)
      (if (y-or-n-p (format "Buffer %s modified; apply edits? "
                            (buffer-name)))
          (majutsu-section-edit-finish)
        (majutsu-section-edit-abort))
    (majutsu-section-edit--stop)
    (message "(No changes need to be applied)")))

(define-minor-mode majutsu-section-edit-mode
  "Minor mode for editing section fields in place.

While this mode is active the buffer becomes writable, but everything
except the marked fields carries the `read-only' text property, so only
those fields can be changed."
  :lighter " SectionEdit"
  :keymap majutsu-section-edit-mode-map
  (if majutsu-section-edit-mode
      (progn
        (majutsu-section-edit--save-cursor-state)
        (majutsu-section-edit--set-cursor-hint majutsu-section-edit-cursor-type))
    (majutsu-section-edit--restore-cursor-state)))

;;; _
(provide 'majutsu-section-edit)
;;; majutsu-section-edit.el ends here
