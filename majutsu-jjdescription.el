;;; majutsu-jjdescription.el --- Edit JJ descriptions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-mode)
(require 'majutsu-process)

(require 'log-edit)
(require 'ring)
(require 'subr-x)
(require 'with-editor)
(require 'server)
(require 'git-commit nil t)
(require 'cl-lib)

(autoload 'majutsu-diff-revset "majutsu-diff" nil t)

(defvar recentf-exclude)
(defvar better-jumper-ignored-file-patterns)
(defvar font-lock-beg)
(defvar font-lock-end)
(defvar server-buffer-clients)

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

;;; Font-Lock Support

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

;;; History Support

(defun majutsu-jjdescription--message-region-end ()
  "Return the end of the replaceable description region."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (majutsu-jjdescription--comment-line-re) nil t)
        (let ((beg (match-beginning 0)))
          (if (and (> beg (point-min))
                   (eq (char-before beg) ?\n))
              (1- beg)
            beg))
      (point-max))))

(defun majutsu-jjdescription-buffer-message ()
  "Return the current description without JJ comments.
Return nil when the remaining text is empty or whitespace only."
  (let ((comment-re (majutsu-jjdescription--comment-prefix-re))
        (ignore-rest-re (majutsu-jjdescription--ignore-rest-line-re))
        (str (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward ignore-rest-re nil t)
        (delete-region (line-beginning-position) (point-max)))
      (goto-char (point-min))
      (flush-lines comment-re)
      (goto-char (point-max))
      (when (and (> (point) (point-min))
                 (not (eq (char-before) ?\n)))
        (insert ?\n))
      (setq str (buffer-string)))
    (and (not (string-match-p "\\`[ \t\n\r]*\\'" str))
         (progn
           (when (string-match "\\`\n\\{2,\\}" str)
             (setq str (replace-match "\n" t t str)))
           (when (string-match "\n\\{2,\\}\\'" str)
             (setq str (replace-match "\n" t t str)))
           str))))

(defun majutsu-jjdescription--save-message (&optional quiet)
  "Save the current description to `log-edit-comment-ring'.
When QUIET is non-nil, do not report status in the echo area."
  (if-let* ((message (majutsu-jjdescription-buffer-message)))
      (progn
        (when-let* ((index (ring-member log-edit-comment-ring message)))
          (ring-remove log-edit-comment-ring index))
        (ring-insert log-edit-comment-ring message)
        (unless quiet
          (message "Description saved"))
        t)
    (unless quiet
      (message "Only whitespace and/or comments; description not saved"))
    nil))

(defun majutsu-jjdescription-save-message ()
  "Save the current description to `log-edit-comment-ring'."
  (interactive)
  (majutsu-jjdescription--save-message))

(defun majutsu-jjdescription-prepare-message-ring ()
  "Prepare buffer-local state for cycling description history."
  (make-local-variable 'log-edit-comment-ring-index))

(defun majutsu-jjdescription-prev-message (arg)
  "Cycle backward through description history, saving current text first.
With a numeric prefix ARG, go back ARG descriptions."
  (interactive "*p")
  (let ((len (ring-length log-edit-comment-ring)))
    (if (<= len 0)
        (progn (message "Empty description ring") (ding))
      (when-let* ((message (majutsu-jjdescription-buffer-message))
                  (_ (not (ring-member log-edit-comment-ring message))))
        (ring-insert log-edit-comment-ring message)
        (cl-incf arg)
        (setq len (ring-length log-edit-comment-ring)))
      (delete-region (point-min) (majutsu-jjdescription--message-region-end))
      (setq log-edit-comment-ring-index (log-edit-new-comment-index arg len))
      (message "Description %d" (1+ log-edit-comment-ring-index))
      (insert (ring-ref log-edit-comment-ring log-edit-comment-ring-index)))))

(defun majutsu-jjdescription-next-message (arg)
  "Cycle forward through description history, saving current text first.
With a numeric prefix ARG, go forward ARG descriptions."
  (interactive "*p")
  (majutsu-jjdescription-prev-message (- arg)))

(defun majutsu-jjdescription-search-message-backward (string)
  "Search backward through description history for STRING.
Save the current description before searching."
  (interactive
   (list (read-string (format-prompt "Description substring"
                                     log-edit-last-comment-match)
                      nil nil log-edit-last-comment-match)))
  (cl-letf (((symbol-function #'log-edit-previous-comment)
             (symbol-function #'majutsu-jjdescription-prev-message)))
    (log-edit-comment-search-backward string)))

(defun majutsu-jjdescription-search-message-forward (string)
  "Search forward through description history for STRING.
Save the current description before searching."
  (interactive
   (list (read-string (format-prompt "Description substring"
                                     log-edit-last-comment-match)
                      nil nil log-edit-last-comment-match)))
  (cl-letf (((symbol-function #'log-edit-previous-comment)
             (symbol-function #'majutsu-jjdescription-prev-message)))
    (log-edit-comment-search-forward string)))

(defun majutsu-jjdescription--change-id ()
  "Return the first JJ Change ID in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (majutsu-jjdescription--change-id-line-re) nil t)
      (string-trim (match-string-no-properties 2)))))

(defun majutsu-jjdescription--server-client-root ()
  "Return the associated emacsclient working directory, if any."
  (when-let* ((client (car-safe server-buffer-clients))
              (dir (process-get client 'server-client-directory)))
    (file-name-as-directory dir)))

(defun majutsu-jjdescription--repository-root ()
  "Return the repository root associated with this JJ description buffer."
  (or majutsu--default-directory
      (and buffer-file-name
           (majutsu-process-with-editor-file-root buffer-file-name))
      (majutsu-jjdescription--server-client-root)
      (ignore-errors (majutsu-toplevel default-directory))))

(defun majutsu-jjdescription-show-diff ()
  "Show the diff for the described JJ change."
  (interactive)
  (let ((default-directory
         (or (majutsu-jjdescription--repository-root)
             (user-error "No repository associated with this JJ description buffer"))))
    (majutsu-diff-revset (or (majutsu-jjdescription--change-id) "@"))))

(defvar-keymap majutsu-jjdescription-mode-map
  :doc "Keymap used by `majutsu-jjdescription-mode'."
  "M-p"     #'majutsu-jjdescription-prev-message
  "M-n"     #'majutsu-jjdescription-next-message
  "C-c M-p" #'majutsu-jjdescription-search-message-backward
  "C-c M-n" #'majutsu-jjdescription-search-message-forward
  "C-c M-s" #'majutsu-jjdescription-save-message
  "C-c C-d" #'majutsu-jjdescription-show-diff)

(define-minor-mode majutsu-jjdescription-mode
  "Minor mode for JJ description buffers."
  :lighter " JJDesc"
  :keymap majutsu-jjdescription-mode-map
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

(defun majutsu-jjdescription-setup ()
  "Set up the current buffer for editing a JJ description."
  (when-let* ((root (majutsu-jjdescription--repository-root)))
    (setq-local default-directory root)
    (setq-local majutsu--default-directory root)
    (when buffer-file-name
      (majutsu-process-forget-with-editor-file-root buffer-file-name)))

  ;; Apply major-mode with pre-enabled modes (like git-commit does)
  (when majutsu-jjdescription-major-mode
    (let ((auto-mode-alist
           (list (cons (concat "\\`"
                               (regexp-quote
                                (majutsu-convert-filename-for-jj buffer-file-name))
                               "\\'")
                       majutsu-jjdescription-major-mode)))
          ;; Pre-enable modes so hooks can see them
          (majutsu-jjdescription-mode t)
          (with-editor-mode t))
      (normal-mode t)))

  ;; Setup comments
  (majutsu-jjdescription-setup-comments)

  ;; Ensure with-editor-mode is enabled
  (unless with-editor-mode
    (with-editor-mode 1))

  ;; History support
  (add-hook 'with-editor-pre-finish-hook
            #'majutsu-jjdescription-save-message nil t)
  (add-hook 'with-editor-pre-cancel-hook
            #'majutsu-jjdescription-save-message nil t)
  (majutsu-jjdescription-prepare-message-ring)
  (majutsu-jjdescription--save-message t)

  ;; Font-lock and finalization
  (majutsu-jjdescription-setup-font-lock)
  (goto-char (point-min))
  (run-hooks 'majutsu-jjdescription-setup-hook)
  (set-buffer-modified-p nil))

(defun majutsu-jjdescription-setup-check-buffer ()
  "Run `majutsu-jjdescription-setup' for matching description buffers."
  (when (and buffer-file-name
             (string-match-p majutsu-jjdescription-regexp buffer-file-name))
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

;;; _
(provide 'majutsu-jjdescription)
;;; majutsu-jjdescription.el ends here
