;;; majutsu-jjdescription-test.el --- Tests for jjdescription font-lock  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for JJ description font-lock rules.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-jjdescription)

(defconst majutsu-test--jjdescription-sample
  (mapconcat
   #'identity
   '(
     "feat(conflict): add majutsu-conflict mode with font-lock"
     ""
     "- Add majutsu-conflict.el for parsing jj conflict markers"
     "- Support diff, snapshot, and git conflict styles"
     "- Add font-lock highlighting for conflict regions"
     "- Add smerge integration for conflict resolution"
     "- Add evil keybindings for conflict navigation"
     "- Add majutsu-conflict-test.el"
     ""
     "JJ: Change ID: rwkwxlpl"
     "JJ: This commit contains the following changes:"
     "JJ:     A majutsu-conflict.el"
     "JJ:     M majutsu-diff.el"
     "JJ:     M majutsu-evil.el"
     "JJ:     M majutsu.el"
     "JJ:     A test/majutsu-conflict-test.el"
     "JJ:"
     "JJ: Lines starting with \"JJ:\" (like this one) will be removed."
     "JJ: ignore-rest"
     "AFTER SHOULD BE COMMENT")
   "\n")
  "Sample JJ description text.")

(defun majutsu-test--faces-at (pos)
  "Return a list of font-lock faces at POS."
  (let ((font-lock-face (get-text-property pos 'font-lock-face))
        (face (get-text-property pos 'face)))
    (cl-remove-duplicates
     (append
      (cond
       ((listp font-lock-face) font-lock-face)
       ((symbolp font-lock-face) (list font-lock-face))
       (t nil))
      (cond
       ((listp face) face)
       ((symbolp face) (list face))
       (t nil)))
     :test #'eq)))

(ert-deftest majutsu-jjdescription-font-lock-summary ()
  "Summary line should use `git-commit-summary`."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'git-commit-summary (majutsu-test--faces-at (point))))))

(ert-deftest majutsu-jjdescription-summary-moves ()
  "Summary highlighting should move to the new first line."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert "Initial summary\n\nJJ: note\n")
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'git-commit-summary (majutsu-test--faces-at (point))))
    (goto-char (point-min))
    (insert "New summary\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'git-commit-summary (majutsu-test--faces-at (point))))
    (forward-line 1)
    (should-not (memq 'git-commit-summary (majutsu-test--faces-at (point))))))

(ert-deftest majutsu-jjdescription-font-lock-comments ()
  "JJ comment lines should be highlighted with comment faces."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "JJ: Change ID:")
    (beginning-of-line)
    (let ((prefix-pos (point))
          (heading-pos (progn (search-forward "JJ: ") (point)))
          (id-pos (progn (search-forward "Change ID: ") (point))))
      (should (memq 'font-lock-comment-face (majutsu-test--faces-at prefix-pos)))
      (should (memq 'git-commit-comment-heading (majutsu-test--faces-at heading-pos)))
      (should (memq majutsu-jjdescription-change-id-face
                    (majutsu-test--faces-at id-pos))))
    (search-forward "JJ:")
    (beginning-of-line)
    (should (memq 'font-lock-comment-face (majutsu-test--faces-at (point))))
    (search-forward "JJ:     A ")
    (let ((action-pos (- (point) 2))
          (file-pos (point)))
      (should (memq 'git-commit-comment-action (majutsu-test--faces-at action-pos)))
      (should (memq 'git-commit-comment-file (majutsu-test--faces-at file-pos))))))

(ert-deftest majutsu-jjdescription-ignore-rest-comments ()
  "Text after ignore-rest should be highlighted as comment."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "AFTER SHOULD BE COMMENT")
    (beginning-of-line)
    (should (memq 'font-lock-comment-face (majutsu-test--faces-at (point))))))

(ert-deftest majutsu-jjdescription-ignore-rest-overrides-summary ()
  "Summary highlighting should not appear after ignore-rest."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert "JJ: ignore-rest\nAfter should be comment\n")
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "After should be comment")
    (beginning-of-line)
    (let ((faces (majutsu-test--faces-at (point))))
      (should (memq 'font-lock-comment-face faces))
      (should-not (memq 'git-commit-summary faces)))))

(ert-deftest majutsu-jjdescription-ignore-rest-prefix ()
  "Ignore-rest prefix stays comment while directive is keyword."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "JJ: ignore-rest")
    (beginning-of-line)
    (let ((prefix-pos (point))
          (keyword-pos (progn (search-forward "ignore-rest") (match-beginning 0))))
      (should (memq 'font-lock-comment-face (majutsu-test--faces-at prefix-pos)))
      (should (memq 'git-commit-keyword (majutsu-test--faces-at keyword-pos))))))

(ert-deftest majutsu-jjdescription-major-mode ()
  "JJ description setup honors `majutsu-jjdescription-major-mode`."
  (let ((majutsu-jjdescription-major-mode #'fundamental-mode))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (majutsu-jjdescription-setup)
      (should (eq major-mode 'fundamental-mode)))))

(ert-deftest majutsu-jjdescription-comment-vars ()
  "JJ description setup configures comment variables."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (majutsu-jjdescription-setup)
    (should (equal comment-start majutsu-jjdescription-comment-prefix))
    (should (equal comment-start-skip
                   (format "^%s[ \t]*"
                           (regexp-quote majutsu-jjdescription-comment-prefix))))
    (should (equal comment-end ""))
    (should (equal comment-end-skip "\n"))
    (should (eq comment-use-syntax nil))
    (should (equal comment-padding " "))))

(ert-deftest majutsu-jjdescription-global-mode-hooks ()
  "Global mode toggles the setup hooks."
  (global-majutsu-jjdescription-mode -1)
  (should-not (memq #'majutsu-jjdescription-setup-check-buffer find-file-hook))
  (global-majutsu-jjdescription-mode 1)
  (should (memq #'majutsu-jjdescription-setup-check-buffer find-file-hook)))

(provide 'majutsu-jjdescription-test)

;;; majutsu-jjdescription-test.el ends here
