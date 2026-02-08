;;; majutsu-sparse.el --- Sparse checkout support for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of sparse-checkout UI flow are adapted from:
;; - Magit `lisp/magit-sparse-checkout.el`
;;   (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors

;;; Commentary:

;; This library provides an interface to the `jj sparse' command.
;; Unlike Git's sparse-checkout which requires enable/init/disable,
;; jj sparse is always active and provides set/edit/list/reset commands.

;;; Code:

(require 'majutsu)
(require 'majutsu-mode)
(require 'majutsu-jj)

(defvar recentf-exclude)
(defvar better-jumper-ignored-file-patterns)

;;; .jjsparse file editing mode

(defconst majutsu-jjsparse-regexp
  (rx (seq (or string-start
               (seq (* (not (any ?\n)))
                    (any ?/ ?\\)))
           "editor-" (+ (in "0-9A-Za-z"))
           ".jjsparse" string-end))
  "Regexp matching temporary jj sparse files created for editing.")

;; Exclude .jjsparse files from various tracking lists
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude majutsu-jjsparse-regexp))

(with-eval-after-load 'better-jumper
  (add-to-list 'better-jumper-ignored-file-patterns majutsu-jjsparse-regexp))

(add-to-list 'with-editor-file-name-history-exclude majutsu-jjsparse-regexp)

(add-to-list 'with-editor-server-window-alist
             (cons majutsu-jjsparse-regexp #'switch-to-buffer))

(defvar-keymap majutsu-jjsparse-mode-map
  :doc "Keymap for `majutsu-jjsparse-mode'."
  :parent text-mode-map)

(define-derived-mode majutsu-jjsparse-mode text-mode "JJ-Sparse"
  "Major mode for editing .jjsparse files.

This mode provides syntax highlighting for sparse patterns and
key bindings for finishing (C-c C-c) or canceling (C-c C-k) the edit.

\\{majutsu-jjsparse-mode-map}"
  :group 'majutsu
  (setq-local comment-start "JJ:")
  (setq-local comment-start-skip "^JJ:[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax nil)

  ;; Simple syntax highlighting
  (font-lock-add-keywords nil
                          '(("^JJ:.*$" . font-lock-comment-face)
                            ("^\\." . font-lock-keyword-face)
                            ("^\\(?:[^J]\\|J[^J]\\|JJ[^:]\\).*/$" . font-lock-function-name-face)))

  ;; Enable with-editor-mode for C-c C-c / C-c C-k support
  (when (fboundp 'with-editor-mode)
    (with-editor-mode 1)))

;; Register the mode for .jjsparse files via auto-mode-alist
(add-to-list 'auto-mode-alist (cons majutsu-jjsparse-regexp #'majutsu-jjsparse-mode))

;;; Utilities

(defun majutsu-sparse-patterns ()
  "Return list of current sparse patterns."
  (majutsu-jj-lines "sparse" "list"))

(defun majutsu-sparse-enabled-p ()
  "Return non-nil if working tree is using sparse patterns.
This checks if patterns differ from the default '.' (all files)."
  (let ((patterns (majutsu-sparse-patterns)))
    (not (and (= (length patterns) 1)
              (string= (car patterns) ".")))))

(defun majutsu-sparse--available-paths ()
  "Return available paths in the working copy commit for completion."
  ;; Get all files from the working copy commit (@)
  (majutsu-jj-lines "file" "list" "-r" "@"))

(defun majutsu-sparse--directory-candidates ()
  "Return directory paths for sparse pattern completion."
  (let* ((files (majutsu-sparse--available-paths))
         (dirs (seq-uniq
                (seq-filter
                 (lambda (path)
                   (and path (not (string= path ""))))
                 (mapcar (lambda (file)
                           (let ((dir (file-name-directory file)))
                             (when dir
                               (directory-file-name dir))))
                         files)))))
    (append dirs '("."))))

;;; Commands

;;;###autoload
(defun majutsu-sparse-set (patterns &optional clear)
  "Set sparse patterns to PATTERNS.
With CLEAR non-nil, clear existing patterns first.
PATTERNS can be a single string or list of strings."
  (interactive
   (list (majutsu-completing-read-multiple
          "Sparse patterns (directories/files)"
          (majutsu-sparse--directory-candidates))
         current-prefix-arg))
  (when (stringp patterns)
    (setq patterns (list patterns)))
  (when patterns
    (let ((args (append '("sparse" "set")
                        (and clear '("--clear"))
                        (apply #'append
                               (mapcar (lambda (p) (list "--add" p))
                                       patterns)))))
      (apply #'majutsu-run-jj args))))

;;;###autoload
(defun majutsu-sparse-set-clear (patterns)
  "Clear all patterns and set to PATTERNS.
This is equivalent to `jj sparse set --clear --add <patterns>'."
  (interactive
   (list (majutsu-completing-read-multiple
          "New sparse patterns"
          (majutsu-sparse--directory-candidates))))
  (majutsu-sparse-set patterns t))

;;;###autoload
(defun majutsu-sparse-add (patterns)
  "Add PATTERNS to current sparse patterns.
PATTERNS can be a single string or list of strings."
  (interactive
   (list (majutsu-completing-read-multiple
          "Add patterns"
          (majutsu-sparse--directory-candidates))))
  (when (stringp patterns)
    (setq patterns (list patterns)))
  (when patterns
    (let ((args (append '("sparse" "set")
                        (apply #'append
                               (mapcar (lambda (p) (list "--add" p))
                                       patterns)))))
      (apply #'majutsu-run-jj args))))

;;;###autoload
(defun majutsu-sparse-remove (patterns)
  "Remove PATTERNS from current sparse patterns.
PATTERNS can be a single string or list of strings."
  (interactive
   (list (majutsu-completing-read-multiple
          "Remove patterns"
          (majutsu-sparse-patterns))))
  (when (stringp patterns)
    (setq patterns (list patterns)))
  (when patterns
    (let ((args (append '("sparse" "set")
                        (apply #'append
                               (mapcar (lambda (p) (list "--remove" p))
                                       patterns)))))
      (apply #'majutsu-run-jj args))))

;;;###autoload
(defun majutsu-sparse-reset ()
  "Reset sparse patterns to include all files.
This is equivalent to `jj sparse reset'."
  (interactive)
  (when (yes-or-no-p "Reset sparse patterns to include all files? ")
    (majutsu-run-jj "sparse" "reset")))

;;;###autoload
(defun majutsu-sparse-edit ()
  "Edit sparse patterns in an editor.
This opens your configured editor with the current patterns."
  (interactive)
  (majutsu-run-jj-with-editor "sparse" "edit"))

;;;###autoload
(defun majutsu-sparse-list ()
  "List current sparse patterns in a dedicated buffer."
  (interactive)
  (majutsu-setup-buffer #'majutsu-sparse-list-mode nil
    :buffer "*Majutsu Sparse*"))

(defun majutsu-sparse--wash-list (_args)
  "Wash `jj sparse list' output into sections."
  (let ((count 0))
    (magit-wash-sequence
     (lambda ()
       (let* ((line (buffer-substring (line-beginning-position)
                                      (line-end-position)))
              (trimmed (string-trim (substring-no-properties line))))
         (delete-region (line-beginning-position)
                        (min (point-max) (1+ (line-end-position))))
         (unless (string-empty-p trimmed)
           (setq count (1+ count))
           (magit-insert-section (jj-sparse-pattern trimmed t)
             (insert (propertize trimmed 'font-lock-face 'magit-filename))
             (insert "\n")))
         t)))
    (if (zerop count)
        (progn
          (magit-cancel-section)
          (insert (propertize "No patterns (should not happen)\n"
                              'font-lock-face 'magit-dimmed)))
      (insert "\n"))))

(defun majutsu-sparse-list-refresh-buffer ()
  "Refresh the sparse patterns list buffer."
  (majutsu--assert-mode 'majutsu-sparse-list-mode)
  (magit-insert-section (sparse-list)
    (magit-insert-heading "Sparse Patterns\n")
    (majutsu-jj-wash #'majutsu-sparse--wash-list nil
      '("sparse" "list"))))

(defvar-keymap majutsu-sparse-list-mode-map
  :doc "Keymap for `majutsu-sparse-list-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-sparse-list-mode majutsu-mode "Majutsu Sparse"
  "Major mode for viewing jj sparse patterns."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

;;; Miscellaneous

(defun majutsu-sparse-insert-header ()
  "Insert header line with sparse patterns information.
This header is not inserted by default. To enable it, add it to
`magit-status-headers-hook'."
  (when (majutsu-sparse-enabled-p)
    (insert (propertize (format "%-10s" "Sparse: ")
                        'font-lock-face 'magit-section-heading))
    (insert
     (let ((patterns (majutsu-sparse-patterns)))
       (pcase (length patterns)
         (0 "empty (error)")
         (1 (car patterns))
         (n (format "%d patterns" n)))))
    (insert ?\n)))

;;; Transient

;;;###autoload(autoload 'majutsu-sparse "majutsu-sparse" nil t)
(transient-define-prefix majutsu-sparse ()
  "Manage sparse working copy patterns."
  :man-page "jj-sparse"
  ["Sparse Working Copy"
   :description
   (lambda ()
     (let ((patterns (majutsu-sparse-patterns)))
       (if (and (= (length patterns) 1)
                (string= (car patterns) "."))
           "Sparse: all files (default)"
         (format "Sparse: %d pattern(s)" (length patterns)))))
   ["Query"
    ("l" "List patterns" majutsu-sparse-list
     :description "Show current patterns")]
   ["Modify"
    ("s" "Set patterns" majutsu-sparse-set
     :description "Add patterns to current set")
    ("S" "Set patterns (clear first)" majutsu-sparse-set-clear
     :description "Replace all patterns")
    ("a" "Add patterns" majutsu-sparse-add
     :description "Add patterns")
    ("r" "Remove patterns" majutsu-sparse-remove
     :description "Remove patterns")]
   ["Advanced"
    ("e" "Edit patterns" majutsu-sparse-edit
     :description "Edit patterns in editor")
    ("R" "Reset to all files" majutsu-sparse-reset
     :description "Include all files")]])

;;; _
(provide 'majutsu-sparse)
;;; majutsu-sparse.el ends here
