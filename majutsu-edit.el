;;; majutsu-edit.el --- Edit and diffedit helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements helpers for jj edit, using log context to
;; choose targets and with-editor when needed.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'majutsu-jj)
(require 'majutsu-file)
(require 'majutsu-process)
(require 'server)

(declare-function majutsu-goto-diff-line "majutsu-diff")
(declare-function majutsu-workspace-visit "majutsu-workspace")

;;; Edit

;; TODO: 我现在完全没有实现这些东西
(defun majutsu-enter-dwim ()
  "Context-sensitive Enter key behavior."
  (interactive)
  (magit-section-case
    (jj-commit
     (majutsu-edit-changeset))
    (jj-hunk
     (majutsu-goto-diff-line))
    (jj-file
     (majutsu-visit-file))
    (jj-workspace
     (majutsu-workspace-visit))))

;;;###autoload
(defun majutsu-edit-changeset (&optional arg)
  "Edit commit at point.

With prefix ARG, pass --ignore-immutable.
When called from a blob buffer, also visit the workspace file."
  (interactive "P")
  (let ((in-blob (and (bound-and-true-p majutsu-blob-mode)
                      majutsu-buffer-blob-root
                      majutsu-buffer-blob-path)))
    (if-let* ((revset (majutsu-revision-at-point))
              (args (append (list "edit" revset)
                            (when arg (list "--ignore-immutable")))))
        (when (zerop (apply #'majutsu-run-jj args))
          (message "Now editing commit %s" revset)
          ;; Visit workspace file when in blob buffer
          (when in-blob
            (let ((file (expand-file-name majutsu-buffer-blob-path
                                          majutsu-buffer-blob-root)))
              (find-file file))))
      (user-error "No revision at point"))))

(defcustom majutsu-edit-finish-on-save t
  "When non-nil, finish diffedit session on save."
  :group 'majutsu
  :type 'boolean)

(defun majutsu-edit--diffedit-root (file)
  "Return diffedit root directory for FILE, or nil if none.
Detects jj diffedit temp directories by locating JJ-INSTRUCTIONS."
  (when file
    (locate-dominating-file (file-name-directory file) "JJ-INSTRUCTIONS")))

(defun majutsu-edit--finish-on-save ()
  "Finish a with-editor session after saving a diffedit file."
  (when (and majutsu-edit-finish-on-save
             (bound-and-true-p with-editor-mode)
             (bound-and-true-p server-buffer-clients))
    (with-editor-finish nil)))

(define-minor-mode majutsu-diffedit-mode
  "Minor mode for jj diffedit buffers."
  :lighter " DiffEdit"
  (if majutsu-diffedit-mode
      (progn
        (with-editor-mode 1)
        (when majutsu-edit-finish-on-save
          (add-hook 'after-save-hook #'majutsu-edit--finish-on-save nil t)))
    (remove-hook 'after-save-hook #'majutsu-edit--finish-on-save t)))

(defun majutsu-edit--maybe-enable-diffedit-mode ()
  "Enable `majutsu-diffedit-mode' in jj diffedit temp buffers."
  (when (majutsu-edit--diffedit-root buffer-file-name)
    (majutsu-diffedit-mode 1)))

(add-hook 'find-file-hook #'majutsu-edit--maybe-enable-diffedit-mode)

(defun majutsu-edit--edit-range (args)
  "Return (FROM . TO) range for diffedit from ARGS or context."
  (or (majutsu-jj--parse-diff-range args)
      (and (derived-mode-p 'majutsu-diff-mode)
           (majutsu-jj--parse-diff-range majutsu-buffer-diff-range))
      (when-let* ((rev (majutsu-revision-at-point)))
        (cons (concat rev "-") rev))
      (cons "@-" "@")))

(defun majutsu-edit--build-diffedit-args (from to file)
  "Build `jj diffedit' arguments for FROM, TO and FILE."
  (append (cond
           ((and from to)
            (list "--from" from "--to" to))
           (to
            (list "-r" to))
           (from
            (list "-r" from))
           (t
            (list "-r" "@")))
          (when file
            (list "--" file))))

(defun majutsu-edit--diffedit-editor-target (file)
  "Return right-side target path expression for FILE in diffedit temp tree."
  (concat "$right/" file))

(defun majutsu-edit--replace-diffedit-file-arg (jj-args file)
  "Return JJ-ARGS with FILE as the diffedit path after `--'."
  (let ((args (copy-sequence jj-args)))
    (if-let* ((sep (member "--" args)))
        (if (cdr sep)
            (setcar (cdr sep) file)
          (setcdr sep (list file)))
      (setq args (append args (list "--" file))))
    args))

(defun majutsu-edit--file-at-point ()
  "Return file at point, including blob buffers."
  (or (and (bound-and-true-p majutsu-blob-mode)
           majutsu-buffer-blob-path)
      (majutsu-file-at-point)))

(defun majutsu-edit--read-diffedit-file (from to)
  "Return diffedit target file from context or prompt."
  (or (majutsu-edit--file-at-point)
      (majutsu-jj-read-diff-file from to)))

(defun majutsu-edit--run-diffedit (jj-args &optional file)
  "Run jj diffedit with JJ-ARGS editing FILE through with-editor."
  (setq file (or file (cadr (member "--" jj-args))))
  (unless file
    (user-error "Diffedit requires a file target"))
  (let* ((root (majutsu--toplevel-safe default-directory))
         (default-directory root)
         (file (if (file-name-absolute-p file)
                   (let* ((abs-root (file-name-as-directory (expand-file-name root)))
                          (abs-file (expand-file-name file)))
                     (if (string-prefix-p abs-root abs-file)
                         (file-relative-name abs-file abs-root)
                       (user-error "Diffedit target outside repository: %s" file)))
                 file))
         (jj-args (majutsu-edit--replace-diffedit-file-arg jj-args file)))
    (majutsu-with-editor
      (let ((diff-editor-cmd
             (majutsu-jj--editor-command-config
              "ui.diff-editor"
              (majutsu-edit--diffedit-editor-target file))))
        ;; Use async to avoid blocking Emacs while jj waits for emacsclient.
        (apply #'majutsu-run-jj-async "diffedit" "--config" diff-editor-cmd jj-args)))))

;;; _
(provide 'majutsu-edit)
;;; majutsu-edit.el ends here
