;;; majutsu-section.el --- Section helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library adapts Magit's section visibility commands for Majutsu.
;; Shifted invisible ranges keep the final body newline visible.  Rows can
;; also supply a graph-only projection to display in place of their hidden
;; body, including when Magit's native visibility commands are used.

;;; Code:

(require 'cl-lib)
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

(defun majutsu-section--graph-tree-p (section)
  "Return non-nil if SECTION or a descendant supplies a graph projection."
  (or (majutsu-section--collapsed-graph section)
      (seq-some #'majutsu-section--graph-tree-p (oref section children))))

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

(define-advice magit-section-show (:around (show section) majutsu-graph)
  "Restore graph sections and their parents using compatible overlay bounds.
An ordinary parent can itself have shifted bounds from a Majutsu hide;
native show would otherwise leave its heading newline invisible."
  (if (or (majutsu-section--collapsed-graph section)
          (and (derived-mode-p 'majutsu-mode)
               (seq-some #'majutsu-section--graph-tree-p
                         (oref section children))))
      (majutsu-section-show section)
    (funcall show section)))

(define-advice magit-section-hide (:around (hide section) majutsu-graph)
  "Preserve graph projections when native reveal/isearch changes visibility."
  (if (majutsu-section--collapsed-graph section)
      (majutsu-section-hide section)
    (funcall hide section)))

(defun majutsu-section-toggle (section)
  "Toggle visibility of SECTION."
  (interactive (list (magit-current-section)))
  (cond ((eq section magit-root-section)
         (user-error "Cannot hide root section"))
        ((oref section hidden)
         (majutsu-section-show section))
        (t
         (majutsu-section-hide section))))

(defun majutsu-section-toggle-children (section)
  "Toggle visibility of bodies of SECTION's direct children."
  (interactive (list (magit-current-section)))
  (let* ((children (oref section children))
         (show (seq-some (lambda (child) (oref child hidden)) children)))
    (dolist (child children)
      (oset child hidden (not show))))
  (majutsu-section-show section))

(defun majutsu-section-show-children (section &optional depth)
  "Recursively show bodies of SECTION's children.
With DEPTH, only show children that deep and hide deeper children."
  (interactive (list (magit-current-section)))
  (majutsu-section-show-children-1 section depth)
  (majutsu-section-show section))

(defun majutsu-section-show-children-1 (section &optional depth)
  "Implementation helper for `majutsu-section-show-children'."
  (dolist (child (oref section children))
    (oset child hidden nil)
    (if depth
        (if (> depth 0)
            (majutsu-section-show-children-1 child (1- depth))
          (majutsu-section-hide child))
      (majutsu-section-show-children-1 child))))

(defun majutsu-section-hide-children (section)
  "Recursively hide bodies of SECTION's children."
  (interactive (list (magit-current-section)))
  (mapc #'majutsu-section-hide (oref section children)))

(defun majutsu-section-show-headings (section)
  "Recursively show headings of SECTION's children."
  (interactive (list (magit-current-section)))
  (majutsu-section-show-headings-1 section)
  (majutsu-section-show section))

(defun majutsu-section-show-headings-1 (section)
  "Implementation helper for `majutsu-section-show-headings'."
  (dolist (child (oref section children))
    (oset child hidden nil)
    (when (or (oref child children)
              (not (oref child content)))
      (majutsu-section-show-headings-1 child))))

(defun majutsu-section-cycle (section)
  "Cycle visibility of SECTION and its children."
  (interactive (list (magit-current-section)))
  (cond
   ((and (equal (this-command-keys) [C-tab])
         (eq (global-key-binding [C-tab]) 'tab-next)
         (fboundp 'tab-bar-switch-to-next-tab))
    (tab-bar-switch-to-next-tab current-prefix-arg))
   ((eq section magit-root-section)
    (majutsu-section-cycle-global))
   ((oref section hidden)
    (majutsu-section-show section)
    (majutsu-section-hide-children section))
   ((let ((children (oref section children)))
      (and (seq-some (lambda (child) (oref child hidden)) children)
           (seq-some (lambda (child) (oref child children)) children)))
    (majutsu-section-show-headings section))
   ((seq-some #'magit-section-hidden-body (oref section children))
    (majutsu-section-show-children section))
   (t
    (majutsu-section-hide section))))

(defun majutsu-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (oref magit-root-section children)))
    (cond
     ((and (seq-some (lambda (child) (oref child hidden)) children)
           (seq-some (lambda (child) (oref child children)) children))
      (majutsu-section-show-headings magit-root-section))
     ((seq-some #'magit-section-hidden-body children)
      (majutsu-section-show-children magit-root-section))
     (t
      (mapc #'majutsu-section-hide children)))))

(defun majutsu-section-show-level (level)
  "Show surrounding sections up to LEVEL.
Likewise hide sections at higher levels.  If LEVEL is negative, show all
sections up to the absolute value of that, not just surrounding sections."
  (interactive "p")
  (if (< level 0)
      (let ((section (magit-current-section)))
        (setq level (- level))
        (while (> (1- (length (magit-section-ident section))) level)
          (setq section (oref section parent))
          (goto-char (oref section start)))
        (majutsu-section-show-children magit-root-section (1- level)))
    (dolist (section (or (magit-region-sections)
                         (list (magit-current-section))))
      (cl-do* ((current section
                        (oref current parent))
               (index (1- (length (magit-section-ident current)))
                      (cl-decf index)))
          ((cond ((< index level)
                  (majutsu-section-show-children current (- level index 1))
                  t)
                 ((= index level)
                  (majutsu-section-hide current)
                  t))
           (magit-section-goto current))))))

(defun majutsu-section-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (majutsu-section-show-level 1))

(defun majutsu-section-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (majutsu-section-show-level -1))

(defun majutsu-section-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (majutsu-section-show-level 2))

(defun majutsu-section-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (majutsu-section-show-level -2))

(defun majutsu-section-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (majutsu-section-show-level 3))

(defun majutsu-section-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (majutsu-section-show-level -3))

(defun majutsu-section-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (majutsu-section-show-level 4))

(defun majutsu-section-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (majutsu-section-show-level -4))

(defun majutsu-mouse-toggle-section (event)
  "Toggle visibility of the clicked section for EVENT."
  (interactive "e")
  (let* ((pos (event-start event))
         (section (magit-section-at (posn-point pos))))
    (if (eq (posn-area pos) 'left-fringe)
        (when section
          (while (not (magit-section-content-p section))
            (setq section (oref section parent)))
          (unless (eq section magit-root-section)
            (goto-char (oref section start))
            (majutsu-section-toggle section)))
      (majutsu-section-toggle section))))

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
