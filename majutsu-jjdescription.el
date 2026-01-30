;;; majutsu-jjdescription.el --- Edit JJ descriptions -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-mode)
(require 'majutsu-process)

(require 'with-editor)
(require 'server)
(require 'git-commit nil t)
(require 'cl-lib)

(defvar recentf-exclude)
(defvar better-jumper-ignored-file-patterns)
(defvar font-lock-beg)
(defvar font-lock-end)

(defconst majutsu-jjdescription-regexp
  (rx (seq (or string-start
               (seq (* (not (any ?\n)))
                    (any ?/ ?\\)))
           "editor-" (+ (in "0-9A-Za-z"))
           ".jjdescription" string-end))
  "Regexp matching temporary jj description files created for editing.")

(defun majutsu-jjdescription--update-auto-mode-alist ()
  "Ensure `auto-mode-alist' reflects `majutsu-jjdescription-major-mode'."
  (setq auto-mode-alist
        (cl-remove-if (lambda (entry)
                        (and (stringp (car entry))
                             (string= (car entry) majutsu-jjdescription-regexp)))
                      auto-mode-alist))
  (when majutsu-jjdescription-major-mode
    (add-to-list 'auto-mode-alist
                 (cons majutsu-jjdescription-regexp
                       majutsu-jjdescription-major-mode))))

(defun majutsu-jjdescription--set-major-mode (symbol value)
  "Set SYMBOL to VALUE and update `auto-mode-alist'."
  (set-default symbol value)
  (majutsu-jjdescription--update-auto-mode-alist))

(defcustom majutsu-jjdescription-major-mode #'text-mode
  "Major mode used for editing JJ description buffers.

When nil, do not override whatever `auto-mode-alist' selects."
  :group 'majutsu
  :type '(choice (function-item text-mode)
          (function-item markdown-mode)
          (function-item org-mode)
          (function-item fundamental-mode)
          (function-item log-edit-mode)
          (function :tag "Another mode")
          (const :tag "No major mode" nil))
  :set #'majutsu-jjdescription--set-major-mode)

(defcustom majutsu-jjdescription-comment-prefix "JJ:"
  "Comment prefix used in JJ description buffers."
  :group 'majutsu
  :type 'string)

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude majutsu-jjdescription-regexp))

(with-eval-after-load 'better-jumper
  (add-to-list 'better-jumper-ignored-file-patterns majutsu-jjdescription-regexp))

(add-to-list 'with-editor-file-name-history-exclude majutsu-jjdescription-regexp)

(add-to-list 'with-editor-server-window-alist
             (cons majutsu-jjdescription-regexp #'switch-to-buffer))

(add-hook 'with-editor-filter-visit-hook #'majutsu--with-editor--apply-visit)
(add-hook 'server-visit-hook #'majutsu--with-editor--apply-visit)
(add-hook 'server-switch-hook #'majutsu--with-editor--apply-visit)

(defvar majutsu--with-editor-visit-queue nil
  "Queue of pending initializer functions for Majutsu with-editor buffers.")

(defun majutsu--with-editor--queue-visit (fn)
  "Enqueue FN so it runs when the next Majutsu editor buffer opens."
  (push fn majutsu--with-editor-visit-queue)
  fn)

(defun majutsu--with-editor--cancel-visit (fn)
  "Remove FN from the pending with-editor queue."
  (setq majutsu--with-editor-visit-queue
        (delq fn majutsu--with-editor-visit-queue)))

(defun majutsu--with-editor--apply-visit ()
  "Run the next pending Majutsu with-editor initializer if applicable."
  (let ((next '())
        handled)
    (dolist (fn majutsu--with-editor-visit-queue)
      (if (and (not handled)
               (with-demoted-errors "Majutsu with-editor init failed: %S"
                 (funcall fn)))
          (setq handled t)
        (push fn next)))
    (setq majutsu--with-editor-visit-queue (nreverse next))
    handled))

(defun majutsu--with-editor--target-buffer-p ()
  "Return non-nil when current buffer is a jj temporary editor file."
  (and buffer-file-name
       (string-match-p majutsu-jjdescription-regexp buffer-file-name)))

(defvar-local majutsu--with-editor-return-window nil
  "Window to restore after finishing a Majutsu with-editor session.")

(defvar-local majutsu--with-editor-return-buffer nil
  "Buffer to restore after finishing a Majutsu with-editor session.")

(defconst majutsu-jjdescription--heading-labels
  '("This commit contains the following changes:"
    "Enter a description for the combined commit."
    "Enter a description for the selected changes."
    "Enter a description for the remaining changes."
    "Enter or edit commit descriptions after the `JJ: describe` lines."
    "Warning:"
    "- The text you enter will be lost on a syntax error."
    "- The syntax of the separator lines may change in the future."
    "Description from the destination commit:"
    "Description from source commit:"
    "Trailers not found in the squashed commits:"
    "Lines starting with \"JJ:\" (like this one) will be removed."
    "Lines starting with \"JJ: \" (like this one) will be removed."
    "Author:"
    "Committer:")
  "Headings shown in JJ description comment blocks.")

(defconst majutsu-jjdescription--heading-labels-re
  (regexp-opt majutsu-jjdescription--heading-labels)
  "Regexp matching heading labels in JJ description comment blocks.")

(defun majutsu-jjdescription--comment-prefix ()
  "Return the current JJ comment prefix."
  (or comment-start majutsu-jjdescription-comment-prefix))

(defun majutsu-jjdescription--comment-prefix-re ()
  "Return a regexp matching the comment prefix and trailing whitespace."
  (concat "^" (regexp-quote (majutsu-jjdescription--comment-prefix)) "[ \t]*"))

(defun majutsu-jjdescription--comment-line-re ()
  "Return a regexp matching JJ comment lines."
  (concat "^" (regexp-quote (majutsu-jjdescription--comment-prefix)) ".*$"))

(defun majutsu-jjdescription--describe-line-re ()
  "Return a regexp matching JJ bulk describe header lines."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\(describe [^\n]+ -------\\)$"))

(defun majutsu-jjdescription--change-id-line-re ()
  "Return a regexp matching JJ Change ID lines."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\(Change ID:\\)\\s-+\\(.+\\)$"))

(defun majutsu-jjdescription--ignore-rest-line-re ()
  "Return a regexp matching JJ ignore-rest directive lines."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\(ignore-rest\\)\\b"))

(defface majutsu-hash
  '((t :inherit magit-hash))
  "Face used for hash identifiers."
  :group 'majutsu-faces)

(defcustom majutsu-jjdescription-change-id-face
  'majutsu-hash
  "Face used for JJ Change ID values."
  :group 'majutsu
  :type 'face)

(defun majutsu-jjdescription--change-id-face-spec ()
  "Return the face spec used for JJ Change ID values."
  (list 'face majutsu-jjdescription-change-id-face
        'font-lock-face majutsu-jjdescription-change-id-face))

(defun majutsu-jjdescription--heading-line-re ()
  "Return a regexp matching JJ description headings."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\(" majutsu-jjdescription--heading-labels-re "\\)"
          "\\(?:.*\\)$"))

(defun majutsu-jjdescription--file-line-re ()
  "Return a regexp matching JJ change summary lines."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\([A-Z]\\)\\s-+\\(.+\\)$"))

(defun majutsu-jjdescription--stat-line-re ()
  "Return a regexp matching JJ diffstat summary lines."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\([^|]+\\)\\s-*|\\s-*\\(.+\\)$"))

(defun majutsu-jjdescription--summary-count-line-re ()
  "Return a regexp matching JJ summary count lines."
  (concat (majutsu-jjdescription--comment-prefix-re)
          "\\([0-9]+ files? changed,.*\\)$"))

(defvar-local majutsu-jjdescription--summary-range nil
  "Cached summary line range for the current buffer.")

(defvar-local majutsu-jjdescription--summary-tick nil
  "Buffer modification tick for `majutsu-jjdescription--summary-range`.")

(defvar-local majutsu-jjdescription--summary-last-range nil
  "Previous summary line range used for refontification.")

(defvar-local majutsu-jjdescription--ignore-rest-pos nil
  "Position of the ignore-rest directive, if present.")

(defvar-local majutsu-jjdescription--ignore-rest-tick nil
  "Buffer modification tick for `majutsu-jjdescription--ignore-rest-pos`.")

(defun majutsu-jjdescription--refresh-summary-range ()
  "Refresh cached summary range for the current buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq majutsu-jjdescription--summary-tick tick)
      (setq majutsu-jjdescription--summary-tick tick
            majutsu-jjdescription--summary-last-range
            majutsu-jjdescription--summary-range
            majutsu-jjdescription--summary-range
            (majutsu-jjdescription--summary-range)))))

(defun majutsu-jjdescription--refresh-ignore-rest-pos ()
  "Refresh cached ignore-rest position for the current buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq majutsu-jjdescription--ignore-rest-tick tick)
      (setq majutsu-jjdescription--ignore-rest-tick tick
            majutsu-jjdescription--ignore-rest-pos nil)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (majutsu-jjdescription--ignore-rest-line-re) nil t)
          (setq majutsu-jjdescription--ignore-rest-pos (line-beginning-position)))))))

(defun majutsu-jjdescription--summary-range ()
  "Return the summary line range as (BEG . END), or nil if missing."
  (save-excursion
    (goto-char (point-min))
    (let* ((ignore-pos (progn
                         (majutsu-jjdescription--refresh-ignore-rest-pos)
                         majutsu-jjdescription--ignore-rest-pos))
           (search-end (or ignore-pos (point-max)))
           range)
      (while (and (not range) (< (point) search-end) (not (eobp)))
        (cond
         ((looking-at-p (rx bol (* (any " \t")) eol)))
         ((and comment-start-skip
               (looking-at-p comment-start-skip)))
         (t (setq range (cons (line-beginning-position)
                              (line-end-position)))))
        (forward-line 1))
      range)))

(defun majutsu-jjdescription--extend-region-summary-line ()
  "Extend region so the summary line gets refontified.
Added to `font-lock-extend-region-functions'."
  (save-excursion
    (save-match-data
      (majutsu-jjdescription--refresh-summary-range)
      (let ((ranges (delq nil (list majutsu-jjdescription--summary-range
                                    majutsu-jjdescription--summary-last-range))))
        (when ranges
          (let ((summary-beg (apply #'min (mapcar #'car ranges)))
                (summary-end (apply #'max (mapcar #'cdr ranges))))
            (when (or (< summary-beg font-lock-beg summary-end)
                      (< summary-beg font-lock-end summary-end))
              (setq font-lock-beg (min font-lock-beg summary-beg))
              (setq font-lock-end (max font-lock-end summary-end)))))))))

(defun majutsu-jjdescription--extend-region-ignore-rest ()
  "Extend region so ignore-rest refontifies following lines."
  (majutsu-jjdescription--refresh-ignore-rest-pos)
  (when (and majutsu-jjdescription--ignore-rest-pos
             (< majutsu-jjdescription--ignore-rest-pos font-lock-end))
    (setq font-lock-beg (min font-lock-beg majutsu-jjdescription--ignore-rest-pos))
    (setq font-lock-end (max font-lock-end (point-max)))))

(defun majutsu-jjdescription--summary-matcher (limit)
  "Match the first non-comment, non-empty line before LIMIT."
  (majutsu-jjdescription--refresh-summary-range)
  (when majutsu-jjdescription--summary-range
    (let ((beg (car majutsu-jjdescription--summary-range))
          (end (cdr majutsu-jjdescription--summary-range)))
      (when (and (< beg limit) (< (point) end))
        (goto-char end)
        (set-match-data (list beg end))
        t))))

(defun majutsu-jjdescription--ignore-rest-comment-matcher (limit)
  "Match text after ignore-rest as comments before LIMIT."
  (majutsu-jjdescription--refresh-ignore-rest-pos)
  (when majutsu-jjdescription--ignore-rest-pos
    (let ((start (save-excursion
                   (goto-char majutsu-jjdescription--ignore-rest-pos)
                   (end-of-line)
                   (if (eobp)
                       (point-max)
                     (forward-line 1)
                     (point)))))
      (when (< start limit)
        (let ((match-start (max start (point))))
          (when (< match-start limit)
            (set-match-data (list match-start limit))
            (goto-char limit)
            t))))))

(defvar-local majutsu-jjdescription--font-lock-keywords nil
  "Buffer-local font-lock keywords for JJ description buffers.")


(defun majutsu-jjdescription--build-font-lock-keywords ()
  "Return font-lock rules for JJ description buffers."
  (let ((comment-line-re (majutsu-jjdescription--comment-line-re))
        (change-id-re (majutsu-jjdescription--change-id-line-re))
        (describe-re (majutsu-jjdescription--describe-line-re))
        (heading-re (majutsu-jjdescription--heading-line-re))
        (file-re (majutsu-jjdescription--file-line-re))
        (stat-re (majutsu-jjdescription--stat-line-re))
        (summary-re (majutsu-jjdescription--summary-count-line-re))
        (ignore-rest-re (majutsu-jjdescription--ignore-rest-line-re)))
    `((majutsu-jjdescription--summary-matcher
       (0 '(face git-commit-summary font-lock-face git-commit-summary) t))
      (,comment-line-re
       (0 '(face font-lock-comment-face font-lock-face font-lock-comment-face) append))
      (,ignore-rest-re
       (1 '(face git-commit-keyword font-lock-face git-commit-keyword) t))
      (,change-id-re
       (1 '(face git-commit-comment-heading font-lock-face git-commit-comment-heading) t)
       (2 (majutsu-jjdescription--change-id-face-spec) t))
      (,describe-re
       (1 '(face git-commit-comment-heading font-lock-face git-commit-comment-heading) t))
      (,heading-re
       (1 '(face git-commit-comment-heading font-lock-face git-commit-comment-heading) t))
      (,file-re
       (1 '(face git-commit-comment-action font-lock-face git-commit-comment-action) t)
       (2 '(face git-commit-comment-file font-lock-face git-commit-comment-file) t))
      (,stat-re
       (1 '(face git-commit-comment-file font-lock-face git-commit-comment-file) t)
       (2 '(face git-commit-comment-action font-lock-face git-commit-comment-action) t))
      (,summary-re
       (1 '(face git-commit-comment-file font-lock-face git-commit-comment-file) t))
      (majutsu-jjdescription--ignore-rest-comment-matcher
       (0 '(face font-lock-comment-face font-lock-face font-lock-comment-face) prepend)))))

(defun majutsu-jjdescription--refresh-font-lock ()
  "Refresh font-lock keywords for JJ description buffers."
  (when majutsu-jjdescription--font-lock-keywords
    (font-lock-remove-keywords nil majutsu-jjdescription--font-lock-keywords))
  (setq-local majutsu-jjdescription--font-lock-keywords
              (majutsu-jjdescription--build-font-lock-keywords))
  (font-lock-add-keywords nil majutsu-jjdescription--font-lock-keywords 'append)
  (font-lock-flush))

(defun majutsu-jjdescription-setup-comments ()
  "Configure comment variables for JJ description buffers."
  (setq-local comment-start majutsu-jjdescription-comment-prefix)
  (setq-local comment-start-skip
              (format "^%s[ \t]*" (regexp-quote comment-start)))
  (setq-local comment-end "")
  (setq-local comment-end-skip "\n")
  (setq-local comment-use-syntax nil)
  (setq-local comment-padding " "))

(defun majutsu-jjdescription-setup-font-lock ()
  "Set up font-lock for JJ description buffers."
  (if majutsu-jjdescription-mode
      (majutsu-jjdescription--refresh-font-lock)
    (majutsu-jjdescription-mode 1)))

(define-minor-mode majutsu-jjdescription-mode
  "Minor mode for JJ description buffers."
  :lighter " JJDesc"
  (if majutsu-jjdescription-mode
      (progn
        (setq-local majutsu-jjdescription--summary-range nil
                    majutsu-jjdescription--summary-tick nil
                    majutsu-jjdescription--summary-last-range nil
                    majutsu-jjdescription--ignore-rest-pos nil
                    majutsu-jjdescription--ignore-rest-tick nil)
        (add-hook 'font-lock-extend-region-functions
                  #'majutsu-jjdescription--extend-region-summary-line nil t)
        (add-hook 'font-lock-extend-region-functions
                  #'majutsu-jjdescription--extend-region-ignore-rest nil t)
        (setq-local font-lock-extra-managed-props
                    (if (memq 'font-lock-face font-lock-extra-managed-props)
                        font-lock-extra-managed-props
                      (cons 'font-lock-face font-lock-extra-managed-props)))
        (setq-local font-lock-multiline t)
        (font-lock-mode 1)
        (majutsu-jjdescription--refresh-font-lock))
    (remove-hook 'font-lock-extend-region-functions
                 #'majutsu-jjdescription--extend-region-summary-line t)
    (remove-hook 'font-lock-extend-region-functions
                 #'majutsu-jjdescription--extend-region-ignore-rest t)
    (when majutsu-jjdescription--font-lock-keywords
      (font-lock-remove-keywords nil majutsu-jjdescription--font-lock-keywords))
    (setq-local majutsu-jjdescription--font-lock-keywords nil)
    (font-lock-flush)))

(defcustom majutsu-jjdescription-setup-hook nil
  "Hook run after setting up a JJ description buffer."
  :group 'majutsu
  :type 'hook)

(defun majutsu-jjdescription--apply-major-mode ()
  "Apply `majutsu-jjdescription-major-mode' if configured."
  (when (and majutsu-jjdescription-major-mode
             (not (eq major-mode majutsu-jjdescription-major-mode)))
    (if buffer-file-name
        (let ((auto-mode-alist
               (list (cons (concat "\\`"
                                   (regexp-quote
                                    (or (file-remote-p buffer-file-name 'localname)
                                        buffer-file-name))
                                   "\\'")
                           majutsu-jjdescription-major-mode))))
          (normal-mode t))
      (funcall majutsu-jjdescription-major-mode))))

(defun majutsu-jjdescription-setup ()
  "Set up the current buffer for editing a JJ description."
  (majutsu-jjdescription--apply-major-mode)
  (majutsu-jjdescription-setup-comments)
  (when (fboundp 'with-editor-mode)
    (with-editor-mode 1))
  (majutsu-jjdescription-setup-font-lock)
  (goto-char (point-min))
  (run-hooks 'majutsu-jjdescription-setup-hook)
  (set-buffer-modified-p nil))

(defun majutsu-jjdescription-setup-check-buffer ()
  "Run `majutsu-jjdescription-setup' for matching description buffers."
  (when (majutsu--with-editor--target-buffer-p)
    (majutsu-jjdescription-setup)))

(defun majutsu-jjdescription-setup-font-lock-in-buffer ()
  "Refresh JJ description font-lock after major-mode changes."
  (when (and buffer-file-name
             (string-match-p majutsu-jjdescription-regexp buffer-file-name))
    (majutsu-jjdescription-setup-comments)
    (majutsu-jjdescription-setup-font-lock)))

(define-minor-mode global-majutsu-jjdescription-mode
  "Edit JJ description buffers.

This global mode arranges for `majutsu-jjdescription-setup' to run
when a JJ description file is opened."
  :group 'majutsu
  :type 'boolean
  :global t
  :init-value t
  :initialize
  (lambda (symbol exp)
    (custom-initialize-default symbol exp)
    (when global-majutsu-jjdescription-mode
      (add-hook 'find-file-hook #'majutsu-jjdescription-setup-check-buffer)
      (add-hook 'after-change-major-mode-hook
                #'majutsu-jjdescription-setup-font-lock-in-buffer)))
  (cond
   (global-majutsu-jjdescription-mode
    (add-hook 'find-file-hook #'majutsu-jjdescription-setup-check-buffer)
    (add-hook 'after-change-major-mode-hook
              #'majutsu-jjdescription-setup-font-lock-in-buffer))
   (t
    (remove-hook 'find-file-hook #'majutsu-jjdescription-setup-check-buffer)
    (remove-hook 'after-change-major-mode-hook
                 #'majutsu-jjdescription-setup-font-lock-in-buffer))))

;; TODO: Add majutsu-jjdescription-show-diff similar to Magit.

(defun majutsu--with-editor--restore-context ()
  "Restore window focus and buffer after a Majutsu with-editor session."
  (let ((window majutsu--with-editor-return-window)
        (buffer majutsu--with-editor-return-buffer))
    (cond
     ((and (window-live-p window)
           (buffer-live-p buffer))
      (with-selected-window window
        (switch-to-buffer buffer)))
     ((buffer-live-p buffer)
      (majutsu-display-buffer buffer))))
  (remove-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context t)
  (remove-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context t))

(defun majutsu--with-editor--visit-hook (window buffer)
  "Return initializer enabling `with-editor-mode' and tracking WINDOW/BUFFER."
  (let ((done nil))
    (lambda ()
      (when (and (not done)
                 (majutsu--with-editor--target-buffer-p))
        (setq done t)
        (majutsu-jjdescription-setup)
        (majutsu--with-editor--setup-return window buffer)
        t))))

(defun majutsu--with-editor--setup-return (window buffer)
  "Remember WINDOW and BUFFER for restoring after the editor closes."
  (setq-local majutsu--with-editor-return-window window)
  (setq-local majutsu--with-editor-return-buffer buffer)
  (add-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context nil t)
  (add-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context nil t))

;;; _
(provide 'majutsu-jjdescription)
;;; majutsu-jjdescription.el ends here
