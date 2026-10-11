;;; majutsu-section-test.el --- Tests for section helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Majutsu section visibility wrappers.

;;; Code:

(require 'ert)
(require 'majutsu-log)
(require 'majutsu-section)

(ert-deftest majutsu-section-hide/preserves-final-newline-for-commit-sections ()
  "Hiding a commit section should keep the final newline visible."
  (let* ((majutsu-log-commit-columns
          '((:field change-id :module heading :face t
             :template majutsu-log-template-change-id)
            (:field description :module heading :face t
             :template majutsu-log-template-description)
            (:field long-desc :module body :face t
             :template majutsu-log-template-long-desc)
            (:field id :module metadata :face nil
             :template majutsu-log-template-id)))
         (compiled (majutsu-row-compile (majutsu-log--row-profile)))
         (entry-1 (list
                   :id "id-1"
                   :indent 2
                   :columns '((change-id . "chg1")
                              (description . "Title")
                              (long-desc . "Body line 1\nBody line 2"))
                   :heading-prefixes '("○ ")
                   :body-prefixes '("│ " "│ ")))
         (entry-2 (list
                   :id "id-2"
                   :indent 2
                   :columns '((change-id . "chg2")
                              (description . "Next"))
                   :heading-prefixes '("○ "))))
    (with-temp-buffer
      (majutsu-log-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (lograph)
        (majutsu-row-insert-entry entry-1 compiled)
        (majutsu-row-insert-entry entry-2 compiled))
      (goto-char (point-min))
      (search-forward "chg1")
      (let* ((section (magit-current-section))
             (content (oref section content)))
        (majutsu-section-show section)
        (should (eq (char-before (oref section end)) ?\n))
        (majutsu-section-hide section)
        (should (get-char-property (1- content) 'invisible))
        (should-not (get-char-property (1- (oref section end)) 'invisible))
        (search-forward "chg2")
        (beginning-of-line)
        (should (stringp (get-text-property (point) 'line-prefix)))))))

(defun majutsu-section-test--invisible-overlays ()
  "Return all invisible overlays in the current buffer as (BEG . END)."
  (delq nil
        (mapcar (lambda (ov)
                  (and (overlay-get ov 'invisible)
                       (cons (overlay-start ov) (overlay-end ov))))
                (overlays-in (point-min) (point-max)))))

(defmacro majutsu-section-test--with-two-sections (&rest body)
  "Run BODY in a buffer with two newline-terminated sibling sections."
  (declare (indent 0))
  `(with-temp-buffer
     (majutsu-log-mode)
     (let ((inhibit-read-only t)
           (magit-section-inhibit-markers t))
       (magit-insert-section (root)
         (magit-insert-section (demo "a")
           (magit-insert-heading "Section a")
           (insert "body a1\nbody a2\n"))
         (magit-insert-section (demo "b")
           (magit-insert-heading "Section b")
           (insert "body b1\n"))))
     ,@body))

(ert-deftest majutsu-section-show/clears-magit-style-overlays ()
  "Showing after `magit-section-hide' must not leave a remnant overlay.
Magit hides [content, end) while Majutsu shifts both bounds back by
one; the show cleanup has to cover both conventions."
  (majutsu-section-test--with-two-sections
    (let ((first (car (oref magit-root-section children))))
      (magit-section-hide first)
      (should (majutsu-section-test--invisible-overlays))
      (majutsu-section-show first)
      (should-not (majutsu-section-test--invisible-overlays)))))

(ert-deftest majutsu-section-show/clears-stacked-mixed-overlays ()
  "Mixed magit/majutsu hide calls must still converge to a clean show."
  (majutsu-section-test--with-two-sections
    (let ((first (car (oref magit-root-section children))))
      (majutsu-section-hide first)
      (should (majutsu-section-test--invisible-overlays))
      (magit-section-hide first)
      (majutsu-section-show first)
      (should-not (majutsu-section-test--invisible-overlays)))))

(ert-deftest majutsu-section-refresh-root-show-rehides-with-majutsu-bounds ()
  "Root-level show must re-hide collapsed children with shifted bounds."
  (majutsu-section-test--with-two-sections
    (let ((first (car (oref magit-root-section children))))
      (majutsu-section-hide first)
      (let ((before (majutsu-section-test--invisible-overlays)))
        (should before)
        (majutsu-section-show magit-root-section)
        ;; The hidden child keeps exactly one overlay with the same
        ;; shifted bounds; nothing is duplicated or truncated.
        (should (equal (majutsu-section-test--invisible-overlays) before))
        (majutsu-section-show first)
        (should-not (majutsu-section-test--invisible-overlays))))))

(ert-deftest majutsu-section-toggle-children/transitions-and-mixed-state ()
  "Toggling hides visible children, then shows hidden or mixed children."
  (majutsu-section-test--with-two-sections
    (let ((children (oref magit-root-section children)))
      (majutsu-section-show magit-root-section)
      (dolist (hidden '(t nil t nil))
        (majutsu-section-toggle-children magit-root-section)
        (dolist (child children)
          (should (eq (oref child hidden) hidden))
          (should (eq (not (null (get-char-property
                                  (oref child content) 'invisible)))
                      hidden))))
      (majutsu-section-hide (car children))
      (majutsu-section-toggle-children magit-root-section)
      (should-not (seq-some (lambda (child) (oref child hidden)) children))
      (should-not (majutsu-section-test--invisible-overlays)))))

(defun majutsu-section-test--visible-text ()
  "Return visible buffer text, including collapsed graph replacements.
Ignore heading decorations, faces and line prefixes."
  (let (pieces)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((pos (+ (point-min) offset)))
        (dolist (overlay (overlays-at pos))
          (when (and (= pos (overlay-start overlay))
                     (overlay-get overlay 'invisible)
                     (overlay-get overlay 'before-string))
            (push (substring-no-properties
                   (overlay-get overlay 'before-string))
                  pieces)))
        (unless (invisible-p pos)
          (push (buffer-substring-no-properties pos (1+ pos)) pieces))))
    (apply #'concat (nreverse pieces))))

(defun majutsu-section-test--insert-graph-section (projection &optional body)
  "Insert a section with PROJECTION and BODY using the row visibility contract."
  (let ((section
         (magit-insert-section (demo "graph")
           (magit-insert-heading "Revision")
           (insert (or body "body one\nbody two\nbody three\n")))))
    (put-text-property (oref section start) (1+ (oref section start))
                       'majutsu-section-collapsed-graph projection)
    ;; Rows install their projection after section insertion and then
    ;; reapply inherited visibility, including during refresh.
    (when (oref section hidden)
      (majutsu-section-hide section))
    section))

(defmacro majutsu-section-test--with-graph-section (projection &rest body)
  "Run BODY with a graph section followed by a plain elision and a sibling.
The graph replacement is PROJECTION; bind the section as `section'."
  (declare (indent 1))
  `(with-temp-buffer
     (majutsu-log-mode)
     (let ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           section)
       (magit-insert-section (root)
         (setq section
               (majutsu-section-test--insert-graph-section ,projection))
         (insert "~\n")
         (magit-insert-section (demo "next")
           (magit-insert-heading "Next")))
       ,@body)))

(ert-deftest majutsu-section-graph/mixed-native-visibility ()
  "All native/Majutsu hide-show combinations restore body and graph exactly."
  (majutsu-section-test--with-graph-section "├─╮\n│ │\n├─╯"
    (let ((expanded (buffer-substring-no-properties (point-min) (point-max))))
      (dolist (hide '(majutsu-section-hide magit-section-hide))
        (dolist (show '(majutsu-section-show magit-section-show))
          (dotimes (_ 2)
            (funcall hide section)
            (should (equal (majutsu-section-test--visible-text)
                           "Revision\n├─╮\n│ │\n├─╯\n~\nNext\n"))
            ;; The plain elision belongs to the container, not the commit.
            (should (= (char-after (oref section end)) ?~))
            (should-not (invisible-p (oref section end)))
            (funcall show section)
            (should (equal (majutsu-section-test--visible-text) expanded))
            (should-not (majutsu-section-test--invisible-overlays)))))
      (majutsu-section-hide section)
      (magit-section-hide section)
      (majutsu-section-hide section)
      (should (equal (majutsu-section-test--visible-text)
                     "Revision\n├─╮\n│ │\n├─╯\n~\nNext\n"))
      (magit-section-reveal section)
      (should (equal (majutsu-section-test--visible-text) expanded))
      (should-not (majutsu-section-test--invisible-overlays)))))

(ert-deftest majutsu-section-graph/empty-projection-is-one-line ()
  "An empty graph projection is a blank line, not an absent projection."
  (majutsu-section-test--with-graph-section ""
    (magit-section-hide section)
    (should (equal (majutsu-section-test--visible-text)
                   "Revision\n\n~\nNext\n"))
    (magit-section-show section)
    (should-not (majutsu-section-test--invisible-overlays))))

(ert-deftest majutsu-section-graph/does-not-repeat-row-prefixes ()
  "Graph replacements never inherit or repeat the surrounding row prefixes."
  (let ((projection (propertize "│" 'line-prefix "wrong" 'wrap-prefix "wrong")))
    (majutsu-section-test--with-graph-section projection
      (put-text-property (point-min) (point-max) 'line-prefix "inherited")
      (put-text-property (point-min) (point-max) 'wrap-prefix "inherited")
      (majutsu-section-hide section)
      (let* ((overlay (seq-find
                       (lambda (overlay) (overlay-get overlay 'before-string))
                       (overlays-at (1- (oref section content)))))
             (display (overlay-get overlay 'before-string)))
        (should (equal (substring-no-properties display) "\n│"))
        (dotimes (pos (length display))
          (should (equal (get-text-property pos 'line-prefix display) ""))
          (should (equal (get-text-property pos 'wrap-prefix display) "")))
        (should (equal (get-text-property 0 'line-prefix projection) "wrong"))))))

(ert-deftest majutsu-section-graph/unterminated-body ()
  "Unshifted body bounds preserve the heading newline and close the graph."
  (with-temp-buffer
    (majutsu-log-mode)
    (let ((inhibit-read-only t)
          section)
      (magit-insert-section (root)
        (setq section
              (majutsu-section-test--insert-graph-section "│" "body")))
      (majutsu-section-hide section)
      (should-not (invisible-p (1- (oref section content))))
      (should (equal (majutsu-section-test--visible-text) "Revision\n│\n"))
      (magit-section-show section)
      (should (equal (majutsu-section-test--visible-text) "Revision\nbody"))
      (should-not (majutsu-section-test--invisible-overlays)))))

(ert-deftest majutsu-section-graph/parent-and-global-visibility ()
  "Parent shows and both global cycles preserve or restore graph bodies."
  (majutsu-section-test--with-graph-section "│"
    (let ((expanded (majutsu-section-test--visible-text))
          (collapsed "Revision\n│\n~\nNext\n"))
      (magit-section-hide section)
      (dolist (show '(majutsu-section-show magit-section-show))
        (funcall show magit-root-section)
        (should (oref section hidden))
        (should (equal (majutsu-section-test--visible-text) collapsed)))
      (dolist (cycle '(majutsu-section-cycle-global magit-section-cycle-global))
        (funcall cycle)
        (should (equal (majutsu-section-test--visible-text) expanded))
        (funcall cycle)
        (should (equal (majutsu-section-test--visible-text) collapsed))))))

(ert-deftest majutsu-section-graph/hidden-parent-restores-child ()
  "Hiding an ordinary parent removes child projections until it is shown."
  (with-temp-buffer
    (majutsu-log-mode)
    (let ((inhibit-read-only t)
          parent section)
      (magit-insert-section (root)
        (setq parent
              (magit-insert-section (group "parent")
                (magit-insert-heading "Group")
                (setq section
                      (majutsu-section-test--insert-graph-section "│")))))
      (majutsu-section-hide section)
      (dolist (hide '(majutsu-section-hide magit-section-hide))
        (dolist (show '(majutsu-section-show magit-section-show))
          (funcall hide parent)
          (should (equal (majutsu-section-test--visible-text) "Group\n"))
          (funcall show parent)
          (should (equal (majutsu-section-test--visible-text)
                         "Group\nRevision\n│\n"))
          (should (oref section hidden)))))))

(ert-deftest majutsu-section-graph/refresh-restores-hidden-projection ()
  "Reinsertion inherits hidden state without retaining an old projection."
  (majutsu-section-test--with-graph-section "old"
    (majutsu-section-hide section)
    (erase-buffer)
    (magit-insert-section (root)
      (setq section (majutsu-section-test--insert-graph-section "new"))
      (insert "~\n"))
    (should (oref section hidden))
    (should (equal (majutsu-section-test--visible-text) "Revision\nnew\n~\n"))
    (magit-section-show magit-root-section)
    (should (equal (majutsu-section-test--visible-text) "Revision\nnew\n~\n"))
    (magit-section-show section)
    (should (equal (majutsu-section-test--visible-text)
                   "Revision\nbody one\nbody two\nbody three\n~\n"))
    (should-not (majutsu-section-test--invisible-overlays))))

(ert-deftest majutsu-section-graph/ordinary-sections-have-no-projection ()
  "Native and Majutsu folding of ordinary sections adds no graph lines."
  (majutsu-section-test--with-two-sections
    (let ((section (car (oref magit-root-section children)))
          (expanded (majutsu-section-test--visible-text)))
      (dolist (hide '(magit-section-hide majutsu-section-hide))
        (funcall hide section)
        (should (equal (majutsu-section-test--visible-text)
                       "Section a\nSection b\nbody b1\n"))
        (majutsu-section-show section)
        (should (equal (majutsu-section-test--visible-text) expanded))))))

(provide 'majutsu-section-test)
;;; majutsu-section-test.el ends here
