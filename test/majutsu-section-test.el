;;; majutsu-section-test.el --- Tests for section helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
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
  (let* ((compiled (majutsu-log--compile-columns
                    '((:field change-id :module heading :face t)
                      (:field description :module heading :face t)
                      (:field long-desc :module body :face t)
                      (:field id :module metadata :face nil))))
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
        (majutsu-log--insert-entry entry-1 compiled)
        (majutsu-log--insert-entry entry-2 compiled))
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
