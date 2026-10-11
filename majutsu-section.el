;;; majutsu-section.el --- Section helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Majutsu owns shifted section visibility bounds throughout its buffers.
;; Magit's native commands use these bounds through show/hide advice, while
;; retaining its orchestration.  Rows can also supply a graph-only projection
;; to display in place of their hidden body.

;;; Code:

(require 'magit-section)
(require 'seq)

(declare-function magit-section--opportunistic-paint "magit-section" (section))
(declare-function magit-section--opportunistic-wash "magit-section" (section))
(declare-function magit-section-maybe-cache-visibility "magit-section" (section))
(declare-function magit-section-maybe-update-visibility-indicator "magit-section" (section))

(defun majutsu-section--hidden-bounds (section)
  "Return invisible overlay bounds for SECTION as (BEG . END).
If the section body ends with a newline, keep that newline visible by
shifting the invisible range one character earlier as well."
  (when-let* ((beg (oref section content))
              (end (oref section end))
              (_ (< beg end)))
    (if (and (> beg (oref section start))
             (eq (char-before end) ?\n))
        (cons (1- beg) (1- end))
      (cons beg end))))

(defun majutsu-section--collapsed-graph (section)
  "Return SECTION's graph-only projection, or nil for ordinary sections.
The heading property `majutsu-section-collapsed-graph' contains graph lines
joined by newlines, without a leading or trailing newline.  An empty string
represents one blank graph line and is distinct from no projection."
  (get-text-property (oref section start) 'majutsu-section-collapsed-graph))

(defun majutsu-section--isolate-graph-faces (string)
  "Give owned graph STRING complete display faces, modifying it in place.
Display strings otherwise inherit unspecified attributes from their buffer
anchor.  Retain explicit faces and ANSI font-lock faces, but use `default'
for remaining attributes rather than the adjacent revision's color or weight."
  (let ((pos 0)
        (end (length string)))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'font-lock-face string end)))
        (when-let* ((face (get-text-property pos 'font-lock-face string)))
          (add-face-text-property pos next face t string))
        (setq pos next)))
    (add-face-text-property 0 end 'default t string))
  string)

(defun majutsu-section--graph-display (section beg projection)
  "Return a replacement for SECTION's hidden body starting at BEG.
PROJECTION contains graph-only lines.  With shifted bounds the final body
newline remains visible; otherwise the replacement must supply its own."
  (let ((display (if (< beg (oref section content))
                     (concat "\n" projection)
                   (concat projection "\n"))))
    ;; Empty prefixes override properties inherited from the buffer, unlike
    ;; simply removing these properties from the graph string.
    (add-text-properties 0 (length display)
                         '(line-prefix "" wrap-prefix "") display)
    (majutsu-section--isolate-graph-faces display)))

(defun majutsu-section-show (section)
  "Show the body of SECTION."
  (interactive (list (magit-current-section)))
  (oset section hidden nil)
  (magit-section--opportunistic-wash section)
  (magit-section--opportunistic-paint section)
  (when-let* ((bounds (majutsu-section--hidden-bounds section)))
    ;; Clear up to the unshifted end as well: Magit's own show/hide
    ;; commands use [content, end) bounds and remove-overlays merely
    ;; truncates partially overlapping overlays, so clearing only the
    ;; shifted range would leave a one-character invisible remnant
    ;; behind whenever both implementations touch the same section.
    (remove-overlays (car bounds) (oref section end) 'invisible t))
  (magit-section-maybe-update-visibility-indicator section)
  (magit-section-maybe-cache-visibility section)
  (dolist (child (oref section children))
    (if (oref child hidden)
        (majutsu-section-hide child)
      (majutsu-section-show child))))

(defun majutsu-section-hide (section)
  "Hide the body of SECTION."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (oset section hidden t)
    (when-let* ((bounds (majutsu-section--hidden-bounds section)))
      (pcase-let ((`(,beg . ,end) bounds))
        (when (< beg (point) end)
          (goto-char (oref section start)))
        ;; Clear Magit-style [content, end) overlays too; see
        ;; `majutsu-section-show'.
        (remove-overlays beg (oref section end) 'invisible t)
        (let ((overlay (make-overlay beg end)))
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'invisible t)
          (overlay-put overlay 'cursor-intangible t)
          (when-let* ((projection (majutsu-section--collapsed-graph section)))
            (overlay-put overlay 'before-string
                         (majutsu-section--graph-display
                          section beg projection))))))
    (magit-section-maybe-update-visibility-indicator section)
    (magit-section-maybe-cache-visibility section)))

(define-advice magit-section-show (:around (show section) majutsu-visibility)
  "Use Majutsu's visibility bounds throughout Majutsu buffers."
  (if (derived-mode-p 'majutsu-mode)
      (majutsu-section-show section)
    (funcall show section)))

(define-advice magit-section-hide (:around (hide section) majutsu-visibility)
  "Use Majutsu's visibility bounds throughout Majutsu buffers."
  (if (derived-mode-p 'majutsu-mode)
      (majutsu-section-hide section)
    (funcall hide section)))

(define-advice magit-section-toggle-children
    (:around (toggle section) majutsu-visibility)
  "Correct child toggling in Majutsu without changing ordinary Magit buffers."
  (if (derived-mode-p 'majutsu-mode)
      (let* ((children (oref section children))
             (show (seq-some (lambda (child) (oref child hidden)) children)))
        ;; Magit's implementation assigns SHOW to `hidden', leaving uniformly
        ;; shown or hidden children unchanged.  Keep this correction local.
        (dolist (child children)
          (oset child hidden (not show)))
        (magit-section-show section))
    (funcall toggle section)))

;;;###autoload
(defun majutsu-copy-section-value ()
  "Copy the current section's stable value.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (if-let* ((section (magit-current-section))
              (value (oref section value)))
        (progn
          (kill-new (format "%s" value))
          (message "%s" value))
      (user-error "No section value at point"))))

(provide 'majutsu-section)
;;; majutsu-section.el ends here
