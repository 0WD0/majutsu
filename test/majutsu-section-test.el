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

(defun majutsu-section-test--log-entry (&rest kvs)
  "Build a log entry plist from KVS."
  kvs)

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
         (entry-1 (majutsu-section-test--log-entry
                   :id "id-1"
                   :indent 2
                   :columns '((change-id . "chg1")
                              (description . "Title")
                              (long-desc . "Body line 1\nBody line 2"))
                   :heading-prefixes '("○ ")))
         (entry-2 (majutsu-section-test--log-entry
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

(ert-deftest majutsu-section-command-remaps-magit-commands ()
  "Majutsu modes should remap Magit section commands to wrappers."
  (with-temp-buffer
    (majutsu-log-mode)
    (should (eq (command-remapping 'magit-section-toggle) 'majutsu-section-toggle))
    (should (eq (command-remapping 'magit-section-cycle) 'majutsu-section-cycle))
    (should (eq (command-remapping 'magit-section-show-level-1-all)
                'majutsu-section-show-level-1-all))
    (should (eq (command-remapping 'magit-mouse-toggle-section)
                'majutsu-mouse-toggle-section))))

(provide 'majutsu-section-test)
;;; majutsu-section-test.el ends here
