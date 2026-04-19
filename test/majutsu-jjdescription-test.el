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

(ert-deftest majutsu-jjdescription-buffer-message-strips-comments ()
  "Saved descriptions should exclude JJ comments and ignored text."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (should
     (equal (majutsu-jjdescription-buffer-message)
            (concat
             "feat(conflict): add majutsu-conflict mode with font-lock\n"
             "\n"
             "- Add majutsu-conflict.el for parsing jj conflict markers\n"
             "- Support diff, snapshot, and git conflict styles\n"
             "- Add font-lock highlighting for conflict regions\n"
             "- Add smerge integration for conflict resolution\n"
             "- Add evil keybindings for conflict navigation\n"
             "- Add majutsu-conflict-test.el\n")))))

(ert-deftest majutsu-jjdescription-save-message-ring ()
  "Saving a JJ description stores the cleaned message in history."
  (let ((log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size))
        (log-edit-comment-ring-index nil))
    (with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert majutsu-test--jjdescription-sample)
      (majutsu-jjdescription-setup)
      (setq log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size)
            log-edit-comment-ring-index nil)
      (majutsu-jjdescription-save-message)
      (should (= (ring-length log-edit-comment-ring) 1))
      (should (equal (ring-ref log-edit-comment-ring 0)
                     (majutsu-jjdescription-buffer-message))))))

(ert-deftest majutsu-jjdescription-prev-next-message ()
  "History cycling replaces only the editable description region."
  (let ((log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size))
        (log-edit-comment-ring-index nil))
    (with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "current summary\n\nJJ: Change ID: abcdefgh\nJJ: note\n")
      (majutsu-jjdescription-setup)
      (setq log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size)
            log-edit-comment-ring-index nil)
      (ring-insert log-edit-comment-ring "older summary\n")
      (majutsu-jjdescription-prev-message 1)
      (should (equal (buffer-string)
                     "older summary\n\nJJ: Change ID: abcdefgh\nJJ: note\n"))
      (majutsu-jjdescription-next-message 1)
      (should (equal (buffer-string)
                     "current summary\n\nJJ: Change ID: abcdefgh\nJJ: note\n")))))

(ert-deftest majutsu-jjdescription-search-message-backward ()
  "History search reuses JJ description region replacement logic."
  (let ((log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size))
        (log-edit-comment-ring-index nil)
        (log-edit-last-comment-match ""))
    (with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "current summary\n\nJJ: Change ID: abcdefgh\nJJ: note\n")
      (majutsu-jjdescription-setup)
      (setq log-edit-comment-ring (make-ring log-edit-maximum-comment-ring-size)
            log-edit-comment-ring-index nil)
      (ring-insert log-edit-comment-ring "feature work\n")
      (ring-insert log-edit-comment-ring "bugfix summary\n")
      (majutsu-jjdescription-search-message-backward "feature")
      (should (equal (buffer-string)
                     "feature work\n\nJJ: Change ID: abcdefgh\nJJ: note\n")))))

(ert-deftest majutsu-jjdescription-setup-uses-server-client-directory ()
  "Setup should bind the repository root from the emacsclient cwd."
  (with-temp-buffer
    (let ((client (make-process :name "majutsu-jjdescription-client"
                                :buffer (current-buffer)
                                :command (list "cat"))))
      (unwind-protect
          (progn
            (setq buffer-file-name "/tmp/editor-123.jjdescription")
            (process-put client 'server-client-directory "/tmp/test-repo")
            (setq-local server-buffer-clients (list client))
            (majutsu-jjdescription-setup)
            (should (equal default-directory "/tmp/test-repo/"))
            (should (equal majutsu--default-directory "/tmp/test-repo/")))
        (delete-process client)))))

(ert-deftest majutsu-jjdescription-show-diff-uses-change-id ()
  "Show-diff should prefer the first Change ID in the buffer."
  (with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert "summary\n\nJJ: Change ID: zzzabcde\nJJ: note\n")
    (cl-letf (((symbol-function #'majutsu-process-with-editor-file-root)
               (lambda (file)
                 (and (equal file buffer-file-name) "/tmp/test-repo/")))
              ((symbol-function #'majutsu-process-forget-with-editor-file-root)
               (lambda (&rest _) nil)))
      (majutsu-jjdescription-setup)
      (let (seen seen-default-directory)
        (cl-letf (((symbol-function #'majutsu-diff-revset)
                   (lambda (revset &rest _)
                     (setq seen revset
                           seen-default-directory default-directory))))
          (majutsu-jjdescription-show-diff))
        (should (equal seen "zzzabcde"))
        (should (equal seen-default-directory "/tmp/test-repo/"))))))

(provide 'majutsu-jjdescription-test)

;;; majutsu-jjdescription-test.el ends here
