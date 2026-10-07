;;; majutsu-jjdescription-test.el --- Tests for jjdescription font-lock  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for JJ description font-lock rules.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-jjdescription)

(defmacro majutsu-jjdescription-test--with-temp-buffer (&rest body)
  "Run BODY in a temporary buffer with private description history."
  (declare (indent 0) (debug t))
  `(let ((log-edit-comment-ring
          (make-ring log-edit-maximum-comment-ring-size))
         (with-editor-show-usage nil))
     (with-temp-buffer ,@body)))

(defmacro majutsu-jjdescription-test--with-global-mode (&rest body)
  "Run BODY with isolated global mode hooks, advice, and enabled state."
  (declare (indent 0) (debug t))
  `(let ((global-majutsu-jjdescription-mode nil)
         (global-minor-modes (copy-sequence global-minor-modes))
         (global-majutsu-jjdescription-mode-hook nil)
         (find-file-hook nil)
         (after-change-major-mode-hook nil))
     (cl-letf (((symbol-function 'server-visit-files) #'ignore))
       ,@body)))

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

(defun majutsu-jjdescription-test--faces-at (pos)
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
  (majutsu-jjdescription-test--with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'git-commit-summary (majutsu-jjdescription-test--faces-at (point))))))

(ert-deftest majutsu-jjdescription-summary-moves ()
  "Summary highlighting should move to the new first line."
  (majutsu-jjdescription-test--with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert "Initial summary\n\nJJ: note\n")
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'git-commit-summary (majutsu-jjdescription-test--faces-at (point))))
    (goto-char (point-min))
    (insert "New summary\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'git-commit-summary (majutsu-jjdescription-test--faces-at (point))))
    (forward-line 1)
    (should-not (memq 'git-commit-summary (majutsu-jjdescription-test--faces-at (point))))))

(ert-deftest majutsu-jjdescription-overlong-summary-boundaries ()
  "Highlight only summary characters strictly beyond the configured limit."
  (dolist (case '((nil "1234567" nil)
                  (0 "1234567" 0)
                  (5 "12345" nil)
                  (5 "1234567" 5)
                  (-1 "1234567" nil)))
    (pcase-let ((`(,limit ,summary ,overlong-offset) case))
      (let ((majutsu-jjdescription-summary-max-length limit))
        (majutsu-jjdescription-test--with-temp-buffer
          (text-mode)
          (setq buffer-file-name "/tmp/editor-123.jjdescription")
          (insert summary "\n\nJJ: note\n")
          (majutsu-jjdescription-setup)
          (font-lock-ensure)
          (goto-char (point-min))
          (when (and (integerp limit) (> limit 0))
            (should-not (memq 'git-commit-overlong-summary
                              (majutsu-jjdescription-test--faces-at (point)))))
          (if overlong-offset
              (progn
                (forward-char overlong-offset)
                (should (memq 'git-commit-overlong-summary
                              (majutsu-jjdescription-test--faces-at (point)))))
            (should-not
             (cl-loop for pos from (line-beginning-position)
                      below (line-end-position)
                      thereis (memq 'git-commit-overlong-summary
                                    (majutsu-jjdescription-test--faces-at pos))))))))))

(ert-deftest majutsu-jjdescription-overlong-summary-respects-ignore-rest ()
  "Text after JJ's ignore-rest marker is never treated as a summary."
  (let ((majutsu-jjdescription-summary-max-length 5))
    (majutsu-jjdescription-test--with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "JJ: ignore-rest\n1234567\n")
      (majutsu-jjdescription-setup)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "1234567")
      (let ((beg (- (point) 7))
            (end (point)))
        (should-not
         (cl-loop for pos from beg below end
                  thereis (memq 'git-commit-overlong-summary
                                (majutsu-jjdescription-test--faces-at pos))))))))

(ert-deftest majutsu-jjdescription-overlong-summary-shrink-clamps-old-range ()
  "Shrinking a summary never extends font-lock past the new buffer end."
  (let ((majutsu-jjdescription-summary-max-length 5))
    (majutsu-jjdescription-test--with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "1234567\n")
      (majutsu-jjdescription-setup)
      (font-lock-ensure)
      (goto-char (point-min))
      (delete-region (+ (point-min) 5) (line-end-position))
      (font-lock-ensure)
      (goto-char (point-min))
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     "12345"))
      (should-not
       (cl-loop for pos from (line-beginning-position)
                below (line-end-position)
                thereis (memq 'git-commit-overlong-summary
                              (majutsu-jjdescription-test--faces-at pos)))))))

(ert-deftest majutsu-jjdescription-fill-column-and-auto-fill ()
  "Keep the summary intact and fill body text at the configured column."
  (let ((majutsu-jjdescription-fill-column 12))
    (majutsu-jjdescription-test--with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "summary line that must remain intact\n\none two three four five")
      (majutsu-jjdescription-setup)
      (should (= fill-column 12))
      (should (eq auto-fill-function
                  #'majutsu-jjdescription--auto-fill-except-summary))
      (goto-char (point-min))
      (end-of-line)
      (funcall auto-fill-function)
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     "summary line that must remain intact"))
      (goto-char (point-max))
      (funcall auto-fill-function)
      (should (equal (buffer-substring-no-properties
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "one")
                        (match-beginning 0))
                      (point-max))
                     "one two\nthree four\nfive")))))

(ert-deftest majutsu-jjdescription-fill-column-accepts-only-positive-integers ()
  "The fill option accepts positive integers or nil as the disabled value."
  (require 'wid-edit)
  (let ((widget (widget-convert
                 (get 'majutsu-jjdescription-fill-column 'custom-type))))
    (should (widget-apply widget :match nil))
    (should (widget-apply widget :match 1))
    (should (widget-apply widget :match 72))
    (should-not (widget-apply widget :match 0))
    (should-not (widget-apply widget :match -1))))

(ert-deftest majutsu-jjdescription-nil-fill-column-disables-auto-fill ()
  "A nil option keeps the current fill column and disables Auto Fill."
  (let ((majutsu-jjdescription-major-mode nil)
        (majutsu-jjdescription-fill-column nil))
    (majutsu-jjdescription-test--with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (setq-local fill-column 37)
      (majutsu-jjdescription-setup)
      (should (= fill-column 37))
      (should-not auto-fill-function))))

(ert-deftest majutsu-jjdescription-auto-fill-delegates-to-major-mode ()
  "Body filling delegates to the major mode's normal Auto Fill function."
  (let ((majutsu-jjdescription-major-mode nil)
        (majutsu-jjdescription-fill-column 20))
    (majutsu-jjdescription-test--with-temp-buffer
      (text-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "summary\n\nbody text")
      (majutsu-jjdescription-setup)
      (let (called)
        (setq-local normal-auto-fill-function
                    (lambda () (setq called (point))))
        (goto-char (point-max))
        (funcall auto-fill-function)
        (should (= called (point)))))))

(ert-deftest majutsu-jjdescription-org-auto-fill-preserves-structures ()
  "Org headings, tables, and blocks retain Org's Auto Fill behavior."
  (let ((majutsu-jjdescription-major-mode nil)
        (majutsu-jjdescription-fill-column 12))
    (majutsu-jjdescription-test--with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert (concat "summary\n\n"
                      "* heading one two three four five\n\n"
                      "| cell one two three four | cell two |\n\n"
                      "#+begin_src emacs-lisp\n"
                      "(message \"one two three four five\")\n"
                      "#+end_src\n"))
      (majutsu-jjdescription-setup)
      (should (eq normal-auto-fill-function #'org-auto-fill-function))
      (let ((before (buffer-string)))
        (dolist (line '("* heading" "| cell" "(message"))
          (goto-char (point-min))
          (search-forward line)
          (end-of-line)
          (funcall auto-fill-function))
        (should (equal (buffer-string) before))))))

(ert-deftest majutsu-jjdescription-major-mode-refresh-reapplies-setup ()
  "Changing major mode reapplies JJ description buffer configuration."
  (majutsu-jjdescription-test--with-global-mode
    (let ((majutsu-jjdescription-fill-column 41))
      (global-majutsu-jjdescription-mode 1)
      (majutsu-jjdescription-test--with-temp-buffer
        (setq buffer-file-name "/tmp/editor-123.jjdescription")
        (text-mode)
        (setq-local fill-column 99)
        (auto-fill-mode -1)
        (fundamental-mode)
        (should (= fill-column 41))
        (should (equal comment-start majutsu-jjdescription-comment-prefix))
        (should (eq auto-fill-function
                    #'majutsu-jjdescription--auto-fill-except-summary))
        (should majutsu-jjdescription-mode)))))

(ert-deftest majutsu-jjdescription-complete-setup-enables-auto-fill-once ()
  "One complete description setup runs Auto Fill hooks exactly once."
  (let ((majutsu-jjdescription-major-mode #'text-mode)
        (majutsu-jjdescription-fill-column 41)
        (count 0)
        (auto-fill-mode-hook nil))
    (add-hook 'auto-fill-mode-hook (lambda () (cl-incf count)))
    (majutsu-jjdescription-test--with-temp-buffer
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (insert "summary\n\nbody\n")
      (majutsu-jjdescription-setup)
      (should (= count 1)))))

(ert-deftest majutsu-jjdescription-major-mode-refresh-does-not-duplicate-hook ()
  "Repeated mode refreshes do not duplicate the global refresh hook."
  (majutsu-jjdescription-test--with-global-mode
    (global-majutsu-jjdescription-mode 1)
    (global-majutsu-jjdescription-mode 1)
    (majutsu-jjdescription-test--with-temp-buffer
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (text-mode)
      (fundamental-mode)
      (text-mode))
    (should (= (cl-count #'majutsu-jjdescription-setup-font-lock-in-buffer
                         after-change-major-mode-hook :test #'eq)
               1))))

(ert-deftest majutsu-jjdescription-font-lock-comments ()
  "JJ comment lines should be highlighted with comment faces."
  (majutsu-jjdescription-test--with-temp-buffer
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
      (should (memq 'font-lock-comment-face (majutsu-jjdescription-test--faces-at prefix-pos)))
      (should (memq 'git-commit-comment-heading (majutsu-jjdescription-test--faces-at heading-pos)))
      (should (memq majutsu-jjdescription-change-id-face
                    (majutsu-jjdescription-test--faces-at id-pos))))
    (search-forward "JJ:")
    (beginning-of-line)
    (should (memq 'font-lock-comment-face (majutsu-jjdescription-test--faces-at (point))))
    (search-forward "JJ:     A ")
    (let ((action-pos (- (point) 2))
          (file-pos (point)))
      (should (memq 'git-commit-comment-action (majutsu-jjdescription-test--faces-at action-pos)))
      (should (memq 'git-commit-comment-file (majutsu-jjdescription-test--faces-at file-pos))))))

(ert-deftest majutsu-jjdescription-ignore-rest-comments ()
  "Text after ignore-rest should be highlighted as comment."
  (majutsu-jjdescription-test--with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert majutsu-test--jjdescription-sample)
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "AFTER SHOULD BE COMMENT")
    (beginning-of-line)
    (should (memq 'font-lock-comment-face (majutsu-jjdescription-test--faces-at (point))))))

(ert-deftest majutsu-jjdescription-ignore-rest-overrides-summary ()
  "Summary highlighting should not appear after ignore-rest."
  (majutsu-jjdescription-test--with-temp-buffer
    (text-mode)
    (setq buffer-file-name "/tmp/editor-123.jjdescription")
    (insert "JJ: ignore-rest\nAfter should be comment\n")
    (majutsu-jjdescription-setup)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "After should be comment")
    (beginning-of-line)
    (let ((faces (majutsu-jjdescription-test--faces-at (point))))
      (should (memq 'font-lock-comment-face faces))
      (should-not (memq 'git-commit-summary faces)))))

(ert-deftest majutsu-jjdescription-ignore-rest-prefix ()
  "Ignore-rest prefix stays comment while directive is keyword."
  (majutsu-jjdescription-test--with-temp-buffer
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
      (should (memq 'font-lock-comment-face (majutsu-jjdescription-test--faces-at prefix-pos)))
      (should (memq 'git-commit-keyword (majutsu-jjdescription-test--faces-at keyword-pos))))))

(ert-deftest majutsu-jjdescription-major-mode ()
  "JJ description setup honors `majutsu-jjdescription-major-mode`."
  (let ((majutsu-jjdescription-major-mode #'fundamental-mode))
    (majutsu-jjdescription-test--with-temp-buffer
      (setq buffer-file-name "/tmp/editor-123.jjdescription")
      (majutsu-jjdescription-setup)
      (should (eq major-mode 'fundamental-mode)))))

(ert-deftest majutsu-jjdescription-comment-vars ()
  "JJ description setup configures comment variables."
  (majutsu-jjdescription-test--with-temp-buffer
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
  (majutsu-jjdescription-test--with-global-mode
    (global-majutsu-jjdescription-mode -1)
    (should-not (memq #'majutsu-jjdescription-setup-check-buffer find-file-hook))
    (global-majutsu-jjdescription-mode 1)
    (should (memq #'majutsu-jjdescription-setup-check-buffer find-file-hook))))

(ert-deftest majutsu-jjdescription-buffer-message-strips-comments ()
  "Saved descriptions should exclude JJ comments and ignored text."
  (majutsu-jjdescription-test--with-temp-buffer
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
    (majutsu-jjdescription-test--with-temp-buffer
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
    (majutsu-jjdescription-test--with-temp-buffer
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
    (majutsu-jjdescription-test--with-temp-buffer
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
  (majutsu-jjdescription-test--with-temp-buffer
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
  (majutsu-jjdescription-test--with-temp-buffer
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

(ert-deftest majutsu-jjdescription-history-replaces-interleaved-drafts ()
  "History replaces all message text, including text after JJ comments."
  (dolist (draft '("JJ: Enter a description.\nold summary\n\nJJ: note\nold body\n\nJJ: ignore-rest\nuntouched diff\n"
                   "JJ: Description from destination commit:\nold destination\nJJ: Description from source commit:\nold source\n\nJJ: ignore-rest\nuntouched diff\n"))
    (majutsu-jjdescription-test--with-temp-buffer
      (let ((log-edit-comment-ring-index nil))
        (insert draft)
        (ring-insert log-edit-comment-ring "replacement\n")
        (majutsu-jjdescription-prev-message 1)
        (should (equal (majutsu-jjdescription-buffer-message) "replacement\n"))
        (should (string-suffix-p "JJ: ignore-rest\nuntouched diff\n" (buffer-string)))
        (should-not (string-match-p "old " (buffer-string)))
        (majutsu-jjdescription-next-message 1)
        (should (string-match-p "old " (majutsu-jjdescription-buffer-message)))))))

(ert-deftest majutsu-jjdescription-history-rejects-bulk-before-editing ()
  "Bulk drafts are rejected without changing either buffer or history."
  (majutsu-jjdescription-test--with-temp-buffer
    (insert "JJ: describe abcdefgh -------\nfirst\nJJ: describe zzzzzzzz -------\nsecond\n")
    (ring-insert log-edit-comment-ring "replacement\n")
    (let ((before (buffer-string))
          (log-edit-comment-ring-index nil))
      (should-error (majutsu-jjdescription-prev-message 1) :type 'user-error)
      (should (equal (buffer-string) before))
      (should (= (ring-length log-edit-comment-ring) 1))
      (should-not log-edit-comment-ring-index))))

(ert-deftest majutsu-jjdescription-server-visit-ignores-unrelated-files ()
  "Unrelated server visits must not inspect process or repository state."
  (cl-letf (((symbol-function 'process-get)
             (lambda (&rest _) (ert-fail "Unrelated visit inspected process")))
            ((symbol-function 'majutsu-toplevel)
             (lambda (&rest _) (ert-fail "Unrelated visit probed repository"))))
    (majutsu-jjdescription--remember-server-visit-roots
     '(("/tmp/ordinary.txt" (1 . 0))) nil)))

(ert-deftest majutsu-jjdescription-history-replaces-unterminated-message ()
  "A last message line without a newline is replaced as well."
  (majutsu-jjdescription-test--with-temp-buffer
    (let ((log-edit-comment-ring-index nil))
      (insert "JJ: source\nold text")
      (ring-insert log-edit-comment-ring "replacement\n")
      (majutsu-jjdescription-prev-message 1)
      (should (equal (majutsu-jjdescription-buffer-message) "replacement\n"))
      (should-not (string-match-p "old text" (buffer-string))))))

(provide 'majutsu-jjdescription-test)

;;; majutsu-jjdescription-test.el ends here
