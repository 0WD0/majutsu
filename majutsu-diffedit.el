;;; majutsu-diffedit.el --- Diffedit helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared helpers for `jj diffedit' workflows.

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-core)
(require 'majutsu-process)
(require 'server)
(require 'with-editor)

(defvar majutsu-buffer-blob-path)
(defvar majutsu-buffer-diff-range)

(defcustom majutsu-diffedit-finish-on-save t
  "When non-nil, finish diffedit session on save."
  :group 'majutsu
  :type 'boolean)

(defun majutsu-diffedit--finish-on-save ()
  "Finish a with-editor session after saving a diffedit file."
  (when (and majutsu-diffedit-finish-on-save
             (bound-and-true-p with-editor-mode)
             (bound-and-true-p server-buffer-clients))
    (with-editor-finish nil)))

(define-minor-mode majutsu-diffedit-mode
  "Minor mode for jj diffedit buffers."
  :lighter " DiffEdit"
  (if majutsu-diffedit-mode
      (progn
        (with-editor-mode 1)
        (when majutsu-diffedit-finish-on-save
          (add-hook 'after-save-hook #'majutsu-diffedit--finish-on-save nil t)))
    (remove-hook 'after-save-hook #'majutsu-diffedit--finish-on-save t)))

(defun majutsu-diffedit--maybe-enable-mode ()
  "Enable `majutsu-diffedit-mode' in jj diffedit temp buffers."
  (when-let* ((file buffer-file-name))
    (when (locate-dominating-file (file-name-directory file) "JJ-INSTRUCTIONS")
      (majutsu-diffedit-mode 1))))

(add-hook 'find-file-hook #'majutsu-diffedit--maybe-enable-mode)

(defun majutsu-diffedit--range (args)
  "Return (FROM . TO) range for diffedit from ARGS or context."
  (or (majutsu-jj--parse-diff-range args)
      (and (derived-mode-p 'majutsu-diff-mode)
           (majutsu-jj--parse-diff-range majutsu-buffer-diff-range))
      (when-let* ((rev (majutsu-revision-at-point)))
        (cons (concat rev "-") rev))
      (cons "@-" "@")))

(defun majutsu-diffedit--read-file (from to)
  "Return diffedit target file from context or prompt."
  (or (and (bound-and-true-p majutsu-blob-mode)
           majutsu-buffer-blob-path)
      (majutsu-file-at-point)
      (majutsu-jj-read-diff-file from to)))

(defun majutsu-diffedit--normalize-file (file root)
  "Return FILE normalized to repository-relative path under ROOT."
  (if (file-name-absolute-p file)
      (let* ((abs-root (file-name-as-directory (expand-file-name root)))
             (abs-file (expand-file-name file)))
        (if (string-prefix-p abs-root abs-file)
            (file-relative-name abs-file abs-root)
          (user-error "Diffedit target outside repository: %s" file)))
    file))

(defun majutsu-diffedit-run (args file diff-editor)
  "Run jj diffedit with option ARGS, FILE and DIFF-EDITOR config.

DIFF-EDITOR may be a config string, or a function called with the normalized
repository-relative file path."
  (unless file
    (user-error "Diffedit requires a file target"))
  (let* ((root (majutsu--toplevel-safe default-directory))
         (default-directory root)
         (file (majutsu-diffedit--normalize-file file root))
         (diff-editor-cmd (if (functionp diff-editor)
                              (funcall diff-editor file)
                            diff-editor))
         (fileset (majutsu-jj-fileset-quote file))
         (jj-args (majutsu-jj-append-filesets args (list fileset))))
    (apply #'majutsu-run-jj-async "diffedit" "--config" diff-editor-cmd jj-args)))

(defun majutsu-diffedit-run-with-editor (args file)
  "Run jj diffedit with option ARGS and FILE through with-editor."
  (majutsu-with-editor
    (majutsu-diffedit-run
     args file
     (lambda (file)
       (majutsu-jj--editor-command-config "ui.diff-editor"
                                          (concat "$right/" file))))))

;;; _
(provide 'majutsu-diffedit)
;;; majutsu-diffedit.el ends here
