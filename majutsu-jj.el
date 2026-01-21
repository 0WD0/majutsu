;;; majutsu-jj.el --- JJ functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides early utilities and section subclasses that
;; other Majutsu modules rely on while avoiding heavier dependencies.

;;; Code:

(require 'ansi-color)
(require 'magit-section)
(require 'seq)
(require 'subr-x)

(require 'with-editor)
(require 'majutsu-base)

;;; Options

(defcustom majutsu-jj-executable "jj"
  "Path to jj executable."
  :group 'majutsu-process
  :type 'string)

(defcustom majutsu-jj-global-arguments
  `("--no-pager" "--color=always")
  "List of global arguments to pass to jj commands."
  :group 'majutsu-commands
  :group 'majutsu-process
  :type '(repeat string))

;;; with-editor

(defcustom majutsu-with-editor-envvar "JJ_EDITOR"
  "Environment variable used to tell jj which editor to invoke."
  :type 'string
  :group 'majutsu)

(defmacro majutsu-with-editor (&rest body)
  "Like `with-editor*' but let-bind some more variables.
Also respect the value of `majutsu-with-editor-envvar'."
  (declare (indent 0) (debug (body)))
  `(let ((majutsu-process-popup-time -1))
     (with-editor* majutsu-with-editor-envvar
       ,@body)))

;;; Reading

(defun majutsu-read-revset (prompt &optional default)
  "Prompt user with PROMPT to read a revision set string."
  (let ((default (or default (magit-section-value-if 'jj-commit) "@")))
    (majutsu-read-string prompt nil nil default)))

;;; Safe default-directory

(defun majutsu--safe-default-directory (&optional file)
  "Return a safe `default-directory' based on FILE or `default-directory'.

If the expanded directory is not accessible, walk up parent directories
until an accessible directory is found.  Return nil if none is found."
  (catch 'unsafe-default-dir
    (let ((dir (file-name-as-directory
                (expand-file-name (or file default-directory))))
          (previous nil))
      (while (not (file-accessible-directory-p dir))
        (setq dir (file-name-directory (directory-file-name dir)))
        (when (equal dir previous)
          (throw 'unsafe-default-dir nil))
        (setq previous dir))
      dir)))

(defmacro majutsu--with-safe-default-directory (file &rest body)
  (declare (indent 1) (debug (form body)))
  `(when-let* ((default-directory (majutsu--safe-default-directory ,file)))
     ,@body))

;;; Change at Point

(defvar majutsu-buffer-blob-revision)
(defvar majutsu-buffer-diff-range)

(defun majutsu-revision-at-point ()
  "Return the change-id at point.
This checks multiple sources in order:
1. Section value (jj-commit section)
2. Blob buffer revision
3. Diff/revision buffer revision"
  (or (magit-section-value-if 'jj-commit)
      (and (bound-and-true-p majutsu-buffer-blob-revision)
           majutsu-buffer-blob-revision)
      (and (derived-mode-p 'majutsu-diff-mode)
           (bound-and-true-p majutsu-buffer-diff-range)
           (let ((range majutsu-buffer-diff-range))
             (or (and (equal (car range) "-r") (cadr range))
                 (cdr (assoc "--revisions=" range)))))))

(defun majutsu-file-at-point ()
  "Return file at point for jj diff sections."
  (magit-section-case
    (jj-hunk (or (magit-section-parent-value it)
                 (oref it value)))
    (jj-file (oref it value))))

(defun majutsu-bookmarks-at-point (&optional bookmark-type)
  "Return a list of bookmark names at point."
  (let* ((rev (or (magit-section-value-if 'jj-commit) "@"))
         (args (append `("show" ,rev "--no-patch" "--ignore-working-copy"
                         "-T" ,(pcase bookmark-type
                                 ('remote "remote_bookmarks")
                                 ('local "local_bookmarks")
                                 (_ "bookmarks")))))
         (output (apply #'majutsu-jj-string args))
         (bookmarks (split-string output " " t)))
    (mapcar (lambda (s) (string-remove-suffix "*" s)) bookmarks)))

(defun majutsu-bookmark-at-point ()
  "Return a comma-separated string of bookmark names at point."
  (let ((bookmarks (majutsu-bookmarks-at-point)))
    (when bookmarks
      (string-join bookmarks ","))))

;;; Errors

(define-error 'majutsu-outside-jj-repo "Not inside jj repository")
(define-error 'majutsu-jj-executable-not-found "jj executable cannot be found")

(defun majutsu--assert-usable-jj ()
  (unless (executable-find majutsu-jj-executable)
    (signal 'majutsu-jj-executable-not-found (list majutsu-jj-executable)))
  nil)

(defun majutsu--not-inside-repository-error ()
  (majutsu--assert-usable-jj)
  (signal 'majutsu-outside-jj-repo (list default-directory)))

;;; JJ

(defun majutsu-toplevel (&optional directory)
  "Return the workspace root for DIRECTORY or `default-directory'.

This runs `jj workspace root' and returns a directory name (with a
trailing slash) or nil if not inside a JJ workspace."
  (majutsu--with-safe-default-directory directory
    (let* ((majutsu-jj-global-arguments
            (cons "--color=never"
                  (seq-remove (lambda (arg)
                                (string-prefix-p "--color" arg))
                              majutsu-jj-global-arguments)))
           (args (majutsu-process-jj-arguments '("workspace" "root"))))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix)
              (coding-system-for-write 'utf-8-unix)
              (exit (apply #'process-file majutsu-jj-executable nil t nil args)))
          ;; `process-file' may return nil on success for some Emacs builds.
          (when (null exit)
            (setq exit 0))
          (when (zerop exit)
            (let ((out (string-trim (buffer-string))))
              (unless (string-empty-p out)
                (file-name-as-directory (expand-file-name out))))))))))

(defun majutsu--toplevel-safe (&optional directory)
  "Return the workspace root for DIRECTORY or signal an error."
  (or (majutsu-toplevel directory)
      (let ((default-directory
             (or (majutsu--safe-default-directory directory)
                 (file-name-as-directory
                  (expand-file-name (or directory default-directory))))))
        (majutsu--not-inside-repository-error))))

(defmacro majutsu-with-toplevel (&rest body)
  (declare (indent defun) (debug (body)))
  `(let ((default-directory (majutsu--toplevel-safe)))
     ,@body))

(defun majutsu-process-jj-arguments (args)
  "Prepare ARGS for a function that invokes JJ.

Majutsu has many specialized functions for running JJ; they all
pass arguments through this function before handing them to JJ,
to do the following.

* Flatten ARGS, removing nil arguments.
* Prepend `majutsu-jj-global-arguments' to ARGS."
  (setq args (seq-remove #'null (flatten-tree args)))
  (append (seq-remove #'null majutsu-jj-global-arguments) args))

(defun majutsu-jj-string (&rest args)
  "Run jj command with ARGS and return its output as a string."
  (let* ((start-time (current-time))
         (safe-args (majutsu-process-jj-arguments args))
         result exit-code)
    (majutsu--debug "Running command: %s %s" majutsu-jj-executable (string-join safe-args " "))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix))
        (setq exit-code (apply #'process-file majutsu-jj-executable nil t nil safe-args)))
      (setq result (buffer-string))
      (majutsu--debug "Command completed in %.3f seconds, exit code: %d"
                      (float-time (time-subtract (current-time) start-time))
                      exit-code)
      (when (and majutsu-show-command-output (not (string-empty-p result)))
        (majutsu--debug "Command output: %s" (string-trim result)))
      result)))

(defun majutsu-jj-wash (washer keep-error &rest args)
  "Run jj with ARGS, insert output at point, then call WASHER.
KEEP-ERROR matches `magit--git-wash': nil drops stderr on error,
`wash-anyway' keeps output even on non-zero exit, anything else keeps the
error text.  Output is optionally colorized based on
`majutsu-process-apply-ansi-colors'."
  (declare (indent 2))
  (setq args (majutsu-process-jj-arguments args))
  (let ((beg (point))
        (exit (apply #'process-file majutsu-jj-executable nil t nil args)))
    (when (and (bound-and-true-p majutsu-process-apply-ansi-colors)
               (> (point) beg))
      ;; Use text-properties instead of overlays so that subsequent
      ;; washing/parsing that uses `buffer-substring' preserves faces.
      (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face))
        (ansi-color-apply-on-region beg (point))))
    ;; `process-file' may return nil on success for some Emacs builds.
    (when (null exit)
      (setq exit 0))
    (cond
     ;; Command produced no output.
     ((= (point) beg)
      (if (= exit 0)
          (magit-cancel-section)
        (insert (propertize (format "jj %s failed (exit %s)\n"
                                    (string-join args " ") exit)
                            'font-lock-face 'error))
        (unless (bolp)
          (insert "\n"))))
     ;; Failure path (unless we explicitly wash anyway).
     ((and (not (eq keep-error 'wash-anyway))
           (not (= exit 0)))
      (goto-char beg)
      (insert (propertize (format "jj %s failed (exit %s)\n"
                                  (string-join args " ") exit)
                          'font-lock-face 'error))
      (unless (bolp)
        (insert "\n")))
     ;; Success (or wash anyway).
     (t
      (unless (bolp)
        (insert "\n"))
      (when (or (= exit 0)
                (eq keep-error 'wash-anyway))
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char beg)
          (funcall washer args))
        (when (or (= (point) beg)
                  (= (point) (1+ beg)))
          (magit-cancel-section)))))
    exit))

;;; _
(provide 'majutsu-jj)
;;; majutsu-jj.el ends here
