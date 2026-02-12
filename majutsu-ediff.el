;;; majutsu-ediff.el --- Ediff extension for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of Ediff orchestration are adapted from:
;; - Magit `lisp/magit-ediff.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors
;; - GNU Emacs `lisp/vc/ediff-util.el` (commit b37711a25f78a915a10245a6330c3b2b4434b2e5)
;;   Copyright (C) 1994-2026 Free Software Foundation, Inc.

;;; Commentary:

;; This library provides Ediff support for Majutsu.

;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'transient)
(require 'magit-section)
(require 'majutsu-base)
(require 'majutsu-jj)
(require 'majutsu-edit)
(require 'majutsu-process)
(require 'majutsu-file)
(require 'majutsu-conflict)
(require 'majutsu-selection)

(declare-function majutsu-diff-visit-file "majutsu-diff" (&optional force-workspace))

(defvar majutsu-buffer-diff-range)

(defvar ediff-after-quit-hook-internal)
(defvar ediff-buffer-C)
(defvar ediff-combination-pattern)
(defvar ediff-default-variant)
(defvar ediff-merge-store-file)

(defconst majutsu-ediff--merge-tool-name "majutsu_ediff_merge"
  "Jujutsu merge-tools entry used by `majutsu-ediff-resolve'.")

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

(defvar-local majutsu-ediff--merge-user-edited nil
  "Non-nil when the current Ediff merge session made user edits.")

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
    (ediff-kill-buffer-carefully ediff-msg-buffer)
    (ediff-kill-buffer-carefully ediff-debug-buffer)
    (when (boundp 'ediff-patch-diagnostics)
      (ediff-kill-buffer-carefully ediff-patch-diagnostics))
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

(defun majutsu-ediff--working-copy-revision-p (rev)
  "Return non-nil when REV resolves to the working copy change.
REV may be any revset accepted by jj."
  (let* ((working (majutsu-file--resolve-single-rev-info "@"))
         (target (majutsu-file--resolve-single-rev-info (or rev "@"))))
    (and working target
         (equal (plist-get working :change-id)
                (plist-get target :change-id)))))

(defun majutsu-ediff--resolve-target-buffer (rev file)
  "Return the conflict target buffer for REV and FILE.
When REV is the working copy, visit FILE from the filesystem. Otherwise
visit FILE as a blob buffer at REV."
  (if (majutsu-ediff--working-copy-revision-p rev)
      (find-file-noselect
       (expand-file-name file (majutsu-file--root)))
    (majutsu-find-file-noselect rev file)))

;;; Commands

;;;###autoload
(defun majutsu-ediff-compare (from to &optional file)
  "Compare FILE between FROM and TO revisions using Ediff.
If FILE is nil, prompt for one."
  (interactive
   (let* ((from (majutsu-read-revset "Compare from" "@-"))
          (to (majutsu-read-revset "Compare to" "@")))
     (list from to nil)))
  (let ((file (or file (majutsu-jj-read-diff-file from to))))
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
         (file (or file (majutsu-jj-read-diff-file parent rev))))
    (majutsu-ediff-compare parent rev file)))

(defun majutsu-ediff--current-range ()
  "Return the current diff range from transient or buffer."
  (majutsu-jj--parse-diff-range
   (if (eq transient-current-command 'majutsu-ediff)
       (transient-args 'majutsu-ediff)
     majutsu-buffer-diff-range)))

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
MERGE-EDITOR-CONFIG is a TOML config string or list of config strings."
  (let ((configs (if (listp merge-editor-config)
                     merge-editor-config
                   (list merge-editor-config))))
    (append
     (list "resolve")
     (apply #'append
            (mapcar (lambda (config)
                      (list "--config" config))
                    configs))
     (list "-r" (or rev "@"))
     (when file
       (list "--" file)))))

(defun majutsu-ediff--toml-string-config (key value)
  "Build KEY=VALUE TOML config where VALUE is a basic string."
  (format "%s=\"%s\"" key (majutsu-jj--toml-escape value)))

(defun majutsu-ediff--merge-editor-config ()
  "Return ui.merge-editor config that launches 3-way Ediff."
  (let* ((editor (majutsu-jj--editor-command-from-env))
         (program (car editor))
         (editor-args (cdr editor))
         (eval-form (format "(majutsu-ediff-merge-files %S %S %S %S %S)"
                            "$left" "$base" "$right" "$output" "$marker_length"))
         (merge-args (append editor-args (list "--eval" eval-form)))
         (tool-key (format "merge-tools.%s" majutsu-ediff--merge-tool-name)))
    (list
     (majutsu-ediff--toml-string-config "ui.merge-editor" majutsu-ediff--merge-tool-name)
     (majutsu-ediff--toml-string-config (format "%s.program" tool-key) program)
     (majutsu-jj--toml-array-config (format "%s.merge-args" tool-key) merge-args)
     (format "%s.merge-tool-edits-conflict-markers=true" tool-key)
     (majutsu-ediff--toml-string-config (format "%s.conflict-marker-style" tool-key) "git"))))

(defun majutsu-ediff--run-resolve (rev file)
  "Run `jj resolve` for REV and FILE with with-editor merge-editor config."
  (majutsu-with-editor
    (let* ((merge-editor-cmd
            (majutsu-ediff--merge-editor-config))
           (args (majutsu-ediff--build-resolve-args rev file merge-editor-cmd)))
      ;; Use async to avoid blocking Emacs while jj waits for emacsclient.
      (apply #'majutsu-run-jj-async args))))

(defun majutsu-ediff--diff-editor-config ()
  "Return ui.diff-editor config that launches single-file Ediff callback."
  (let* ((editor (majutsu-jj--editor-command-from-env))
         (eval-form "(majutsu-ediff-diffedit-file \"$left\" \"$right\")"))
    (majutsu-jj--toml-array-config
     "ui.diff-editor"
     (append editor (list "--eval" eval-form)))))

(defun majutsu-ediff--run-diffedit (jj-args &optional file)
  "Run jj diffedit with JJ-ARGS using `majutsu-ediff-diffedit-file'."
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
      (let ((diff-editor-cmd (majutsu-ediff--diff-editor-config)))
        ;; Use async to avoid blocking Emacs while jj waits for emacsclient.
        (apply #'majutsu-run-jj-async "diffedit" "--config" diff-editor-cmd jj-args)))))

(defun majutsu-ediff--cleanup-diffedit-variant-buffers
    (left-file right-file left-existing right-existing)
  "Persist and close diffedit variant buffers created for this session."
  (cl-labels ((force-kill (buffer)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    ;; with-editor installs a kill guard that raises a user-error.
                    ;; Diffedit cleanup runs after the edit session has already
                    ;; finished, so bypass query hooks to avoid aborting quit flow.
                    (let ((kill-buffer-query-functions nil))
                      (kill-buffer buffer))))))
    (let* ((left-buffer (get-file-buffer left-file))
           (right-buffer (get-file-buffer right-file)))
      (when (and (buffer-live-p right-buffer)
                 (not right-existing))
        (with-current-buffer right-buffer
          (when (and (buffer-file-name)
                     (buffer-modified-p)
                     (file-writable-p (buffer-file-name)))
            (save-buffer)))
        (force-kill right-buffer))
      (when (and (buffer-live-p left-buffer)
                 (not left-existing))
        (force-kill left-buffer)))))

(defun majutsu-ediff--setup-diffedit-quit-hooks
    (winconf left-file right-file left-existing right-existing)
  "Install local quit hooks for a diffedit Ediff session.
WINCONF is the window configuration saved before launching Ediff."
  (add-hook 'ediff-quit-hook
            (lambda ()
              (let ((majutsu-ediff-previous-winconf winconf))
                (run-hooks 'majutsu-ediff-quit-hook)))
            t t)
  (add-hook 'ediff-after-quit-hook-internal
            (lambda ()
              (majutsu-ediff--cleanup-diffedit-variant-buffers
               left-file right-file left-existing right-existing))
            t t)
  ;; Exit recursive edit only after Ediff has completed its quit cleanup.
  (add-hook 'ediff-after-quit-hook-internal
            #'majutsu-ediff--exit-recursive-edit t t))

(defun majutsu-ediff--setup-merge-quit-hooks (winconf)
  "Install local quit hooks for a merge Ediff session.
WINCONF is the window configuration saved before launching Ediff."
  (setq-local majutsu-ediff--merge-user-edited nil)
  (let ((control-buffer (current-buffer)))
    (when (ediff-buffer-live-p ediff-buffer-C)
      (with-current-buffer ediff-buffer-C
        (add-hook 'before-change-functions
                  (lambda (&rest _)
                    (when (buffer-live-p control-buffer)
                      (with-current-buffer control-buffer
                        (setq-local majutsu-ediff--merge-user-edited t))))
                  nil t))))
  ;; Replace Ediff's default merge quit hook so save semantics stay local to
  ;; Majutsu's `jj resolve` integration.
  (setq-local ediff-quit-merge-hook '(majutsu-ediff--quit-merge-session))
  (add-hook 'ediff-quit-hook
            (lambda ()
              (let ((majutsu-ediff-previous-winconf winconf))
                (run-hooks 'majutsu-ediff-quit-hook)))
            t t)
  ;; Exit recursive edit only after Ediff has completed its quit cleanup.
  (add-hook 'ediff-after-quit-hook-internal
            #'majutsu-ediff--exit-recursive-edit t t))

(defun majutsu-ediff--quit-merge-session ()
  "Persist merge output if edited, then close merge buffer.
This hook runs in the Ediff control buffer and is intended for `jj resolve'."
  (let ((store-file ediff-merge-store-file)
        (edited-p majutsu-ediff--merge-user-edited))
    (when (ediff-buffer-live-p ediff-buffer-C)
      (when (and edited-p
                 (stringp store-file)
                 (> (length store-file) 0))
        (ediff-with-current-buffer ediff-buffer-C
          ;; `ediff-merge-store-file' is local to the control buffer, so keep
          ;; a stable copy before switching to buffer C.
          (write-region nil nil store-file nil 'silent)))
      (ediff-with-current-buffer ediff-buffer-C
        ;; Avoid save prompts during cleanup. Output is already persisted above.
        (set-buffer-modified-p nil))
      (ediff-kill-buffer-carefully ediff-buffer-C))))

(defun majutsu-ediff--merge-marker-length (marker-length)
  "Return normalized conflict MARKER-LENGTH for Ediff merge sessions."
  (let* ((parsed (if (numberp marker-length)
                     marker-length
                   (string-to-number (or marker-length ""))))
         (len (if (> parsed 0) parsed 7)))
    (max len 7)))

(defun majutsu-ediff--git-combination-pattern (marker-length)
  "Return `ediff-combination-pattern' using git-style MARKER-LENGTH."
  (list (make-string marker-length ?<)
        'A
        (make-string marker-length ?|)
        'Ancestor
        (make-string marker-length ?=)
        'B
        (make-string marker-length ?>)))

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
(defun majutsu-ediff-edit (args)
  "Edit one changed file with two-sided Ediff via jj diffedit.
ARGS are transient arguments."
  (interactive
   (list (when (eq transient-current-command 'majutsu-ediff)
           (transient-args 'majutsu-ediff))))
  (let* ((range (majutsu-edit--edit-range args))
         (from (car range))
         (to (cdr range))
         (file (majutsu-edit--read-diffedit-file from to))
         (jj-args (majutsu-edit--build-diffedit-args from to file)))
    (majutsu-ediff--run-diffedit jj-args file)))

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
          (majutsu-edit--run-diffedit
           (majutsu-edit--build-diffedit-args nil rev file)
           file))
      (majutsu-ediff--run-resolve rev file))))

;;;###autoload
(defun majutsu-ediff-resolve-with-conflict ()
  "Resolve conflicts using `majutsu-conflict-mode'.
When resolving a non-working-copy revision, open the matching blob buffer
at that revision before enabling conflict mode."
  (interactive)
  (let* ((rev (or (majutsu-ediff--resolve-revision-at-point) "@"))
         (file (majutsu-ediff--resolve-file-dwim))
         (buffer (majutsu-ediff--resolve-target-buffer rev file)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (majutsu-conflict-ensure-mode)
      (majutsu-conflict-goto-nearest))))

;;;###autoload
(defun majutsu-ediff-diffedit-file (left right)
  "Compare one file from LEFT and RIGHT using Ediff.
This is called by jj diffedit when using Emacs as the diff editor.
Blocks until user finishes editing and quits Ediff."
  (interactive "DLeft directory: \nDRight directory: ")
  (let* ((left (expand-file-name left))
         (right (expand-file-name right))
         (file (majutsu-ediff--read-directory-file left right))
         (left-file (expand-file-name file left))
         (right-file (expand-file-name file right))
         (left-existing (get-file-buffer left-file))
         (right-existing (get-file-buffer right-file))
         (winconf (current-window-configuration)))
    (ediff-files left-file right-file
                 (list (lambda ()
                         (majutsu-ediff--setup-diffedit-quit-hooks
                          winconf left-file right-file left-existing right-existing))))
    (recursive-edit)))

(defun majutsu-ediff--exit-recursive-edit ()
  "Exit recursive edit when Ediff quits."
  (when (> (recursion-depth) 0)
    (exit-recursive-edit)))

;;;###autoload
(defun majutsu-ediff-merge-files (left base right output &optional marker-length)
  "Resolve a 3-way merge with Ediff and write result to OUTPUT.
Called by `jj resolve` merge editor command via emacsclient."
  (interactive "fLeft file: \nfBase file: \nfRight file: \nFOutput file: ")
  (let* ((left (expand-file-name left))
         (base (expand-file-name base))
         (right (expand-file-name right))
         (output (expand-file-name output))
         (marker-length (majutsu-ediff--merge-marker-length marker-length))
         (ediff-default-variant 'combined)
         (ediff-combination-pattern
          (majutsu-ediff--git-combination-pattern marker-length))
         (winconf (current-window-configuration)))
    (ediff-merge-files-with-ancestor
     left right base
     (list (lambda ()
             (majutsu-ediff--setup-merge-quit-hooks winconf)))
     output)
    (recursive-edit)))

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
    ("e" "Ediff (blob)" majutsu-ediff-dwim)
    ("E" "Ediff (diffedit)" majutsu-ediff-edit)]
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
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
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
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :key "-f"
  :argument "--from="
  :reader #'majutsu-ediff--transient-read-revset)

(transient-define-argument majutsu-ediff:--to ()
  :description "To"
  :class 'majutsu-selection-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
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
