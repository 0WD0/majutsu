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

(defun majutsu-ediff--resolve-revision-at-point ()
  "Return commit revision at point for resolve DWIM, or nil.
Only commit sections are considered for DWIM resolve flow."
  (when-let* ((rev (magit-section-value-if 'jj-commit)))
    (substring-no-properties rev)))

(defun majutsu-ediff--resolve-file-dwim (&optional file)
  "Return conflicted FILE for resolve workflow.
If FILE is nil and point is on a commit section, prompt conflicts in that
revision; otherwise prompt from working copy conflicts."
  (or file
      (majutsu-ediff--read-conflicted-file
       (majutsu-ediff--resolve-revision-at-point))))

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

(defun majutsu-ediff--edit-range (args)
  "Return (FROM . TO) range for diffedit from ARGS or context."
  (or (majutsu-ediff--parse-diff-range args)
      (and (derived-mode-p 'majutsu-diff-mode)
           (majutsu-ediff--parse-diff-range majutsu-buffer-diff-range))
      (cons "@-" "@")))

(defun majutsu-ediff--build-diffedit-args (from to file)
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

(defun majutsu-ediff--toml-escape (value)
  "Escape VALUE for use inside a TOML basic string."
  (let ((escaped (replace-regexp-in-string "\\\\" "\\\\\\\\" value t t)))
    (replace-regexp-in-string "\"" "\\\\\"" escaped t t)))

(defun majutsu-ediff--toml-array-config (key values)
  "Build KEY=[...] TOML config from string VALUES."
  (format "%s=[%s]"
          key
          (mapconcat (lambda (value)
                       (format "\"%s\"" (majutsu-ediff--toml-escape value)))
                     values
                     ", ")))

(defun majutsu-ediff--editor-command-from-env ()
  "Return with-editor command words from `majutsu-with-editor-envvar'."
  (let ((raw (getenv majutsu-with-editor-envvar)))
    (unless (and raw (not (equal raw "")))
      (user-error "Missing %s in process environment" majutsu-with-editor-envvar))
    (condition-case err
        (let ((words (split-string-shell-command raw)))
          (unless words
            (user-error "%s is empty" majutsu-with-editor-envvar))
          words)
      (error
       (user-error "Failed to parse %s: %s"
                   majutsu-with-editor-envvar
                   (error-message-string err))))))

(defun majutsu-ediff--editor-command-config (key target &optional editor-command)
  "Build KEY config command string editing TARGET.
EDITOR-COMMAND defaults to the command parsed from with-editor environment."
  (majutsu-ediff--toml-array-config
   key
   (append (or editor-command (majutsu-ediff--editor-command-from-env))
           (list target))))

(defun majutsu-ediff--diffedit-editor-target (file)
  "Return right-side target path expression for FILE in diffedit temp tree."
  (concat "$right/" file))

(defun majutsu-ediff--conflict-side-count (rev file)
  "Return the sidedness for FILE conflict at REV.
When sidedness cannot be parsed, return 0."
  (let* ((default-directory (majutsu-file--root))
         (line (car (majutsu-jj-lines "resolve" "--list" "-r" rev "--" file))))
    (if (and line
             (string-match "[ \t]+\\([0-9]+\\)-sided conflict\\'" line))
        (string-to-number (match-string 1 line))
      0)))

(defun majutsu-ediff--build-resolve-args (rev file merge-editor-config)
  "Build `jj resolve' arguments for REV and FILE.
MERGE-EDITOR-CONFIG is a `ui.merge-editor=[...]' TOML config string."
  (append
   (list
    "resolve"
    "--config"
    merge-editor-config
    "-r" (or rev "@"))
   (when file
     (list "--" file))))

(defun majutsu-ediff--run-resolve (rev file)
  "Run `jj resolve` for REV and FILE with with-editor merge-editor config."
  (majutsu-with-editor
    (let* ((merge-editor-cmd
            (majutsu-ediff--editor-command-config "ui.merge-editor" "$output"))
           (args (majutsu-ediff--build-resolve-args rev file merge-editor-cmd)))
      ;; Use async to avoid blocking Emacs while jj waits for emacsclient.
      (apply #'majutsu-run-jj-async args))))

(defun majutsu-ediff--directory-common-files (left right)
  "Return common relative file paths between LEFT and RIGHT directories."
  (let* ((left (file-name-as-directory (expand-file-name left)))
         (right (file-name-as-directory (expand-file-name right)))
         (files nil))
    (dolist (abs (directory-files-recursively right ".*" nil t))
      (when (file-regular-p abs)
        (let ((rel (file-relative-name abs right)))
          (when (and (not (string= rel "JJ-INSTRUCTIONS"))
                     (file-exists-p (expand-file-name rel left)))
            (push rel files)))))
    (sort files #'string<)))

(defun majutsu-ediff--read-directory-file (left right)
  "Read a single common file path between LEFT and RIGHT directories."
  (let ((files (majutsu-ediff--directory-common-files left right)))
    (cond
     ((null files)
      (user-error "No common editable files between %s and %s" left right))
     ((= (length files) 1)
      (car files))
     (t
      (completing-read "Diffedit file: " files nil t)))))

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
  "Resolve FILE conflicts using `jj resolve' with Emacs as merge editor.
If FILE is nil, DWIM selects from conflicted files at point revision (commit
section) or the working copy."
  (interactive)
  (let* ((rev (or (majutsu-ediff--resolve-revision-at-point) "@"))
         (file (majutsu-ediff--resolve-file-dwim file))
         (sides (majutsu-ediff--conflict-side-count rev file)))
    (if (> sides 2)
        (progn
          (message "%s has %d sides; using diffedit fallback" file sides)
          (majutsu-ediff--run-diffedit
           (majutsu-ediff--build-diffedit-args nil rev file)
           file))
      (majutsu-ediff--run-resolve rev file))))

;;;###autoload
(defun majutsu-ediff-resolve-with-conflict ()
  "Backward-compatible alias for `majutsu-ediff-resolve'."
  (interactive)
  (majutsu-ediff-resolve))

;;;###autoload
(defun majutsu-ediff-edit (args)
  "Edit the right side of a diff using jj diffedit with Emacs as diff-editor.
ARGS are transient arguments."
  (interactive
   (list (when (eq transient-current-command 'majutsu-ediff)
           (transient-args 'majutsu-ediff))))
  (let* ((range (majutsu-ediff--edit-range args))
         (from (car range))
         (to (cdr range))
         (file (or (majutsu-file-at-point)
                   (majutsu-ediff--read-file from to)))
         (jj-args (majutsu-ediff--build-diffedit-args from to file)))
    (majutsu-ediff--run-diffedit jj-args file)))

(defun majutsu-ediff--run-diffedit (jj-args &optional file)
  "Run jj diffedit with JJ-ARGS editing FILE through with-editor."
  (setq file (or file (cadr (member "--" jj-args))))
  (unless file
    (user-error "Diffedit requires a file target"))
  (majutsu-with-editor
    (let ((diff-editor-cmd
           (majutsu-ediff--editor-command-config
            "ui.diff-editor"
            (majutsu-ediff--diffedit-editor-target file))))
      ;; Use async to avoid blocking Emacs while jj waits for emacsclient.
      (apply #'majutsu-run-jj-async "diffedit" "--config" diff-editor-cmd jj-args))))

;;;###autoload
(defun majutsu-ediff-directories (left right)
  "Edit one file from LEFT/RIGHT diffedit directories.
This is called by jj diffedit when using Emacs as the diff editor and blocks
until the user completes manual editing." 
  (interactive "DLeft directory: \nDRight directory: ")
  (let* ((left (expand-file-name left))
         (right (expand-file-name right))
         (file (majutsu-ediff--read-directory-file left right))
         (right-file (expand-file-name file right)))
    (find-file right-file)
    (message "Edit %s and finish with with-editor (C-c C-c)" (abbreviate-file-name right-file))))

;;;###autoload
(defun majutsu-ediff-merge-files (left base right output)
  "Resolve a 3-way merge with Ediff and write result to OUTPUT.
Called by `jj resolve` merge editor command via emacsclient."
  (interactive "fLeft file: \nfBase file: \nfRight file: \nFOutput file: ")
  (let ((left (expand-file-name left))
        (base (expand-file-name base))
        (right (expand-file-name right))
        (output (expand-file-name output)))
    (ediff-merge-files-with-ancestor left right base nil output)))

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
