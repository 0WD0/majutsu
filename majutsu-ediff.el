;;; majutsu-ediff.el --- Ediff extension for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides Ediff support for Majutsu.

;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'transient)
(require 'magit-section)
(require 'majutsu-base)
(require 'majutsu-jj)
(require 'majutsu-process)
(require 'majutsu-file)
(require 'majutsu-conflict)

(declare-function majutsu-diff-visit-file "majutsu-diff" (&optional force-workspace))

(defvar majutsu-buffer-diff-range)

;;; Options

(defgroup majutsu-ediff nil
  "Ediff support for Majutsu."
  :group 'majutsu)

(defcustom majutsu-ediff-quit-hook
  (list #'majutsu-ediff-cleanup-auxiliary-buffers
        #'majutsu-ediff-restore-previous-winconf)
  "Hooks to run after finishing Ediff, when that was invoked using Majutsu."
  :group 'majutsu-ediff
  :type 'hook)

;;; Variables

(defvar majutsu-ediff-previous-winconf nil
  "Window configuration before starting Ediff.")

;;; Buffer Management

(defmacro majutsu-ediff-buffers (a b &optional c setup quit file)
  "Run Ediff on two or three buffers.
A, B and C have the form (GET-BUFFER CREATE-BUFFER).  If GET-BUFFER
returns a non-nil value, that buffer is used and not killed when exiting.
Otherwise CREATE-BUFFER must return a buffer and that is killed on exit.

SETUP is called after Ediff setup.  QUIT is added to quit hook.
If FILE is non-nil, perform a merge with result written to FILE."
  (let (get make kill (char ?A))
    (dolist (spec (list a b c))
      (if (not spec)
          (push nil make)
        (pcase-let ((`(,g ,m) spec))
          (let ((b (intern (format "buf%c" char))))
            (push `(,b ,g) get)
            (push `(or ,b ,m) make)
            (push `(unless ,b
                     (let ((var ,(if (and file (= char ?C))
                                     'ediff-ancestor-buffer
                                   (intern (format "ediff-buffer-%c" char)))))
                       (ediff-kill-buffer-carefully var)))
                  kill))
          (cl-incf char))))
    (setq get  (nreverse get))
    (setq make (nreverse make))
    (setq kill (nreverse kill))
    (let ((mconf (gensym "conf"))
          (mfile (gensym "file")))
      `(majutsu-with-toplevel
         (let ((,mconf (current-window-configuration))
               (,mfile ,file)
               ,@get)
           (ediff-buffers-internal
            ,@make
            (list ,@(and setup (list setup))
                  (lambda ()
                    (setq-local ediff-quit-merge-hook nil)
                    (setq-local ediff-quit-hook
                                (list
                                 ,@(and quit (list quit))
                                 (lambda ()
                                   ,@kill
                                   (let ((majutsu-ediff-previous-winconf ,mconf))
                                     (run-hooks 'majutsu-ediff-quit-hook)))))))
            (pcase (list ,(and c t) (and ,mfile t))
              ('(nil nil) 'ediff-buffers)
              ('(nil t)   'ediff-merge-buffers)
              ('(t   nil) 'ediff-buffers3)
              ('(t   t)   'ediff-merge-buffers-with-ancestor))
            ,mfile))))))

(defun majutsu-ediff-cleanup-auxiliary-buffers ()
  "Kill Ediff control and auxiliary buffers."
  (let* ((ctl ediff-control-buffer))
    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
    (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
    (ediff-kill-buffer-carefully ediff-tmp-buffer)
    (ediff-kill-buffer-carefully ediff-error-buffer)
    (ediff-kill-buffer-carefully ctl)))

(defun majutsu-ediff-restore-previous-winconf ()
  "Restore window configuration saved before Ediff."
  (when (window-configuration-p majutsu-ediff-previous-winconf)
    (set-window-configuration majutsu-ediff-previous-winconf)))

;;; Helpers

(defun majutsu-ediff--get-revision-buffer (rev file)
  "Return existing buffer for REV:FILE or nil."
  (get-buffer (majutsu-file--buffer-name rev file)))

(defun majutsu-ediff--find-file-noselect (rev file)
  "Return buffer visiting FILE from REV."
  (majutsu-find-file-noselect rev file))

(defun majutsu-ediff--parse-diff-range (range)
  "Parse RANGE into (from . to) cons.
RANGE is a list like (\"--revisions=xxx\") or (\"--from=xxx\" \"--to=xxx\")."
  (when range
    (let ((from nil) (to nil) (revisions nil))
      (dolist (arg range)
        (cond
         ((string-prefix-p "--from=" arg)
          (setq from (substring arg 7)))
         ((string-prefix-p "--to=" arg)
          (setq to (substring arg 5)))
         ((string-prefix-p "--revisions=" arg)
          (setq revisions (substring arg 12)))
         ((string-prefix-p "-r" arg)
          (setq revisions (substring arg 2)))))
      (cond
       (revisions (cons (concat revisions "-") revisions))
       ((and from to) (cons from to))
       (from (cons from "@"))
       (to (cons "@-" to))
       (t (cons "@-" "@"))))))

(defun majutsu-ediff--read-file (from to)
  "Read file to compare between FROM and TO."
  (let* ((root (majutsu-file--root))
         (default-directory root)
         (changed (majutsu-jj-lines "diff" "--from" from "--to" to "--name-only")))
    (if (= (length changed) 1)
        (car changed)
      (completing-read
       (format "File to compare between %s and %s: " from to)
       changed nil t))))

(defun majutsu-ediff--list-conflicted-files (&optional rev)
  "Return list of conflicted files at REV (default @)."
  (let* ((default-directory (majutsu-file--root))
         (lines (majutsu-jj-lines "resolve" "--list" "-r" (or rev "@"))))
    ;; Parse "filename    N-sided conflict" format
    (mapcar (lambda (line)
              (if (string-match "^\\([^ \t]+\\)" line)
                  (match-string 1 line)
                line))
            lines)))

(defun majutsu-ediff--read-conflicted-file (&optional rev)
  "Prompt for a conflicted file at REV."
  (let ((files (majutsu-ediff--list-conflicted-files rev)))
    (cond
     ((null files)
      (user-error "No conflicts found at revision %s" (or rev "@")))
     ((= (length files) 1)
      (car files))
     (t
      (completing-read "Resolve conflicts in: " files nil t)))))

;;; Commands

;;;###autoload
(defun majutsu-ediff-compare (from to &optional file)
  "Compare FILE between FROM and TO revisions using Ediff.
If FILE is nil, prompt for one."
  (interactive
   (let* ((from (majutsu-read-revset "Compare from" "@-"))
          (to (majutsu-read-revset "Compare to" "@")))
     (list from to nil)))
  (let ((file (or file (majutsu-ediff--read-file from to))))
    (majutsu-ediff-buffers
     ((majutsu-ediff--get-revision-buffer from file)
      (majutsu-ediff--find-file-noselect from file))
     ((majutsu-ediff--get-revision-buffer to file)
      (majutsu-ediff--find-file-noselect to file)))))

;;;###autoload
(defun majutsu-ediff-show-revision (rev &optional file)
  "Show changes in REV using Ediff (parent vs rev).
If FILE is nil, prompt for one."
  (interactive
   (list (majutsu-read-revset "Show revision" "@")))
  (let* ((parent (concat rev "-"))
         (file (or file (majutsu-ediff--read-file parent rev))))
    (majutsu-ediff-compare parent rev file)))

(defun majutsu-ediff--current-range ()
  "Return the current diff range from transient or buffer."
  (majutsu-ediff--parse-diff-range
   (if (eq transient-current-command 'majutsu-ediff)
       (transient-args 'majutsu-ediff)
     majutsu-buffer-diff-range)))

;;;###autoload
(defun majutsu-ediff-dwim ()
  "Context-aware Ediff based on current section."
  (interactive)
  (magit-section-case
    (jj-hunk
     (save-excursion
       (goto-char (oref (oref it parent) start))
       (majutsu-ediff-dwim)))
    (jj-file
     (let* ((file (oref it value))
            (range (majutsu-ediff--current-range)))
       (majutsu-ediff-compare (car range) (cdr range) file)))
    (jj-commit
     (majutsu-ediff-show-revision (substring-no-properties (oref it value))))
    (t
     (let* ((range (majutsu-ediff--current-range))
            (file (majutsu-file-at-point)))
       (cond
        ((and (car range) (cdr range))
         (if file
             (majutsu-ediff-compare (car range) (cdr range) file)
           (majutsu-ediff-compare (car range) (cdr range))))
        ((car range)
         (majutsu-ediff-show-revision (car range)))
        (t
         (majutsu-ediff-show-revision "@")))))))

;;;###autoload
(defun majutsu-ediff-resolve (&optional file)
  "Resolve conflicts in FILE using Ediff.
For conflicts with more than 2 sides, fall back to `majutsu-conflict'."
  (interactive)
  (let* ((file (or file
                   (majutsu-file-at-point)
                   (majutsu-ediff--read-conflicted-file)))
         (full-path (expand-file-name file (majutsu-file--root))))
    (unless (file-exists-p full-path)
      (user-error "File does not exist: %s" file))
    (find-file full-path)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (cond
       ((null conflicts)
        (user-error "No conflicts in %s" file))
       ;; Check if any conflict has more than 2 sides
       ((cl-some (lambda (c)
                   (> (+ (length (majutsu-conflict-adds c))
                         (length (majutsu-conflict-removes c)))
                      3))  ; base + 2 sides = 3
                 conflicts)
        (message "Conflict has more than 2 sides, using majutsu-conflict")
        (majutsu-conflict-ensure-mode)
        (majutsu-conflict-goto-nearest))
       ;; Git-style conflicts: use smerge-ediff
       ((majutsu-conflict--git-style-only-p)
        (smerge-ediff))
       ;; JJ-style with 2 sides: use majutsu-conflict for now
       ;; (ediff 3-way merge requires extracting base/left/right content)
       (t
        (majutsu-conflict-ensure-mode)
        (majutsu-conflict-goto-nearest)
        (message "Use C-c ^ commands to resolve conflicts"))))))

;;;###autoload
(defun majutsu-ediff-resolve-with-conflict ()
  "Resolve conflicts using `majutsu-conflict-mode'."
  (interactive)
  (let* ((file (or (majutsu-file-at-point)
                   (majutsu-ediff--read-conflicted-file)))
         (full-path (expand-file-name file (majutsu-file--root))))
    (find-file full-path)
    (majutsu-conflict-ensure-mode)
    (majutsu-conflict-goto-nearest)))

;;;###autoload
(defun majutsu-ediff-edit (args)
  "Edit the right side of a diff using jj diffedit with Emacs as diff-editor.
ARGS are transient arguments."
  (interactive (list (transient-args 'majutsu-ediff)))
  (let* ((range (majutsu-ediff--parse-diff-range args))
         (from (car range))
         (to (cdr range))
         (jj-args (cond
                   ((and from to)
                    (list "--from" from "--to" to))
                   (to
                    (list "-r" to))
                   (from
                    (list "-r" from))
                   (t
                    (list "-r" "@")))))
    (majutsu-ediff--run-diffedit jj-args)))

(defun majutsu-ediff--run-diffedit (jj-args)
  "Run jj diffedit with JJ-ARGS using Emacs ediff as the diff editor."
  (let* ((emacsclient (or (executable-find "emacsclient")
                          (error "Cannot find emacsclient")))
         (server-name (or server-name "server"))
         ;; Build the diff-editor command - use TOML array syntax for proper quoting
         (diff-editor-cmd (format "ui.diff-editor=[\"%s\", \"-s\", \"%s\", \"--eval\", \"(majutsu-ediff-directories \\\"$left\\\" \\\"$right\\\")\"]"
                                  emacsclient server-name)))
    ;; Use async to avoid blocking Emacs while jj waits for emacsclient
    (apply #'majutsu-run-jj-async "diffedit" "--config" diff-editor-cmd jj-args)))

;;;###autoload
(defun majutsu-ediff-directories (left right)
  "Compare LEFT and RIGHT directories using ediff.
This is called by jj diffedit when using Emacs as the diff editor.
Blocks until user finishes editing and quits ediff."
  (interactive "DLeft directory: \nDRight directory: ")
  (let ((left (expand-file-name left))
        (right (expand-file-name right)))
    ;; Set up quit hook to exit recursive-edit
    (add-hook 'ediff-quit-hook #'majutsu-ediff--exit-recursive-edit)
    (unwind-protect
        (progn
          (ediff-directories left right nil)
          ;; Block until user quits ediff
          (recursive-edit))
      (remove-hook 'ediff-quit-hook #'majutsu-ediff--exit-recursive-edit))))

(defun majutsu-ediff--exit-recursive-edit ()
  "Exit recursive edit when ediff quits."
  (when (> (recursion-depth) 0)
    (exit-recursive-edit)))

;;; Transient

(defun majutsu-ediff--default-args ()
  "Return default args from diff buffer context."
  (when (derived-mode-p 'majutsu-diff-mode)
    majutsu-buffer-diff-range))

(defun majutsu-ediff--transient-read-revset (prompt _initial-input _history)
  "Read a revset for ediff transient with PROMPT."
  (majutsu-read-revset prompt))

;;;###autoload
(transient-define-prefix majutsu-ediff ()
  "Show differences using Ediff."
  :incompatible '(("--revisions=" "--from=")
                  ("--revisions=" "--to="))
  :transient-non-suffix t
  [:description "Ediff"
   :class transient-columns
   ["Selection"
    (majutsu-ediff:-r)
    (majutsu-ediff:--from)
    (majutsu-ediff:--to)
    (majutsu-ediff:revisions)
    (majutsu-ediff:from)
    (majutsu-ediff:to)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Actions"
    ("e" "Dwim" majutsu-ediff-dwim)
    ("E" "Edit (modify right)" majutsu-ediff-edit)]
   ["Resolve"
    ("m" "Resolve (ediff)" majutsu-ediff-resolve)
    ("M" "Resolve (conflict)" majutsu-ediff-resolve-with-conflict)]]
  (interactive)
  (transient-setup
   'majutsu-ediff nil nil
   :scope (majutsu-selection-session-begin)
   :value (majutsu-ediff--default-args)))

;;;; Infix Commands

(transient-define-argument majutsu-ediff:-r ()
  :description "Revisions"
  :class 'majutsu-selection-option
  :selection-label "[REVS]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :locate-fn (##majutsu-section-find % 'jj-commit)
  :key "-r"
  :argument "--revisions="
  :multi-value 'repeat
  :prompt "Revisions: ")

(transient-define-argument majutsu-ediff:revisions ()
  :description "Revisions (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :key "r"
  :argument "--revisions="
  :multi-value 'repeat)

(transient-define-argument majutsu-ediff:--from ()
  :description "From"
  :class 'majutsu-selection-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :locate-fn (##majutsu-section-find % 'jj-commit)
  :key "-f"
  :argument "--from="
  :reader #'majutsu-ediff--transient-read-revset)

(transient-define-argument majutsu-ediff:--to ()
  :description "To"
  :class 'majutsu-selection-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :locate-fn (##majutsu-section-find % 'jj-commit)
  :key "-t"
  :argument "--to="
  :reader #'majutsu-ediff--transient-read-revset)

(transient-define-argument majutsu-ediff:from ()
  :description "From (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-ediff:to ()
  :description "To (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :key "t"
  :argument "--to=")

;;; _
(provide 'majutsu-ediff)
;;; majutsu-ediff.el ends here
