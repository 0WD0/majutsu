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
(require 'majutsu-core)
(require 'majutsu-diffedit)
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

(defconst majutsu-ediff--control-packet-prefix "MAJUTSU-EDIFF"
  "Prefix used by Majutsu Ediff control packets in jj process output.")

(defconst majutsu-ediff--sleeping-input-ready-timeout 2.0
  "Seconds to wait for sleeping-editor temp inputs to become accessible.")

(defconst majutsu-ediff--sleeping-input-ready-interval 0.05
  "Polling interval while waiting for sleeping-editor temp inputs.")

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

(defun majutsu-ediff--conflicted-file-completion-entry (entry)
  "Return an annotated completion entry for conflicted-file ENTRY."
  (when-let* ((file (plist-get entry :path)))
    (cons file
          (when-let* ((sides (plist-get entry :sides)))
            (format "%d-sided conflict" sides)))))

(defun majutsu-ediff--read-conflicted-file (&optional rev)
  "Prompt for a conflicted file at REV."
  (let* ((default-directory (majutsu-file--root))
         (entries (majutsu-jj-conflicted-files (or rev "@")))
         (choices (delq nil
                        (mapcar #'majutsu-ediff--conflicted-file-completion-entry
                                entries))))
    (cond
     ((null choices)
      (user-error "No conflicts found at revision %s" (or rev "@")))
     ((= (length choices) 1)
      (caar choices))
     (t
      (majutsu-completing-read "Resolve conflicts in" choices nil t nil
                               'majutsu-file-path-history
                               nil 'majutsu-file)))))

(defun majutsu-ediff--resolve-file-dwim (&optional file)
  "Return conflicted FILE for resolve workflow.
If FILE is nil and point is on a commit section, prompt conflicts in that
revision; otherwise prompt from working copy conflicts."
  (or file
      (majutsu-ediff--read-conflicted-file
       (majutsu-revision-at-point))))

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
         (entries (majutsu-jj-conflicted-files
                   (or rev "@")
                   (majutsu-jj-fileset-quote file))))
    (or (plist-get (car entries) :sides) 0)))

(defun majutsu-ediff--build-resolve-args (rev file merge-editor-config)
  "Build `jj resolve' arguments for REV and FILE.
MERGE-EDITOR-CONFIG is a TOML config string or list of config strings."
  (let ((configs (if (listp merge-editor-config)
                     merge-editor-config
                   (list merge-editor-config))))
    (majutsu-jj-append-filesets
     (append
      (list "resolve")
      (apply #'append
             (mapcar (lambda (config)
                       (list "--config" config))
                     configs))
      (list "-r" (or rev "@")))
     (and file (list file)))))

(defun majutsu-ediff--toml-string-config (key value)
  "Build KEY=VALUE TOML config where VALUE is a basic string."
  (format "%s=\"%s\"" key (majutsu-jj--toml-escape value)))

(defun majutsu-ediff--merge-editor-config ()
  "Return ui.merge-editor config for Majutsu Ediff control callbacks."
  (let* ((merge-args (list "-c" (majutsu-ediff--control-merge-script)
                           "majutsu-ediff"
                           "$left" "$base" "$right" "$output" "$marker_length"))
         (tool-key (format "merge-tools.%s" majutsu-ediff--merge-tool-name)))
    (list
     (majutsu-ediff--toml-string-config "ui.merge-editor" majutsu-ediff--merge-tool-name)
     (majutsu-ediff--toml-string-config (format "%s.program" tool-key) "sh")
     (majutsu-jj--toml-array-config (format "%s.merge-args" tool-key) merge-args)
     (format "%s.merge-tool-edits-conflict-markers=true" tool-key)
     (majutsu-ediff--toml-string-config (format "%s.conflict-marker-style" tool-key) "git"))))

(defun majutsu-ediff--run-resolve (rev file)
  "Run `jj resolve` for REV and FILE with Majutsu Ediff config."
  (let* ((merge-editor-cmd
          (majutsu-ediff--merge-editor-config))
         (args (majutsu-ediff--build-resolve-args rev file merge-editor-cmd)))
    ;; Use async to avoid blocking Emacs while jj waits for merge completion.
    (apply #'majutsu-run-jj-async args)))

(defun majutsu-ediff--diff-editor-config (&optional file)
  "Return ui.diff-editor config for Majutsu Ediff control callbacks.
When FILE is non-nil, pass it as a file hint."
  (let* ((command (list "sh" "-c" (majutsu-ediff--control-diff-script)
                        "majutsu-ediff" "$left" "$right" (or file ""))))
    (majutsu-jj--toml-array-config
     "ui.diff-editor"
     command)))

(defun majutsu-ediff--control-diff-script ()
  "Return shell script used for diffedit control packets."
  (string-join
   (list
    ;; Keep jj blocked until Emacs signals completion.
    "sleep 604800 & sleep_pid=$!"
    "printf \"\\nMAJUTSU-EDIFF: $$ DIFF %s\\037%s\\037%s\\n\" \"$1\" \"$2\" \"$3\""
    "trap \"kill $sleep_pid; exit 0\" USR1"
    "trap \"kill $sleep_pid; exit 1\" USR2"
    "wait $sleep_pid")
   "; "))

(defun majutsu-ediff--control-merge-script ()
  "Return shell script used for resolve merge control packets."
  (string-join
   (list
    ;; Keep jj blocked until Emacs signals completion.
    "sleep 604800 & sleep_pid=$!"
    "printf \"\\nMAJUTSU-EDIFF: $$ MERGE %s\\037%s\\037%s\\037%s\\037%s\\n\" \"$1\" \"$2\" \"$3\" \"$4\" \"$5\""
    "trap \"kill $sleep_pid; exit 0\" USR1"
    "trap \"kill $sleep_pid; exit 1\" USR2"
    "wait $sleep_pid")
   "; "))

(defun majutsu-ediff--control-packet-spec (line)
  "Parse Majutsu Ediff control packet LINE into a plist spec.
Return nil when LINE is not a valid control packet."
  (when (string-match
         (format "^%s: \\([0-9]+\\) \\(DIFF\\|MERGE\\) \\(.*\\)\\(?:\\r\\)?$"
                 majutsu-ediff--control-packet-prefix)
         line)
    (let* ((pid (match-string 1 line))
           (kind (downcase (match-string 2 line)))
           (fields (split-string (match-string 3 line) "\x1f" nil)))
      (pcase kind
        ("diff"
         (pcase fields
           (`(,left ,right ,file)
            (let ((spec (list :pid pid
                              :type 'diff
                              :left left
                              :right right)))
              (if (string-empty-p file)
                  spec
                (append spec (list :file file)))))
           (`(,left ,right)
            (list :pid pid :type 'diff :left left :right right))
           (_ nil)))
        ("merge"
         (pcase fields
           (`(,left ,base ,right ,output ,marker)
            (list :pid pid
                  :type 'merge
                  :left left
                  :base base
                  :right right
                  :output output
                  :marker marker))
           (_ nil)))
        (_ nil)))))

(defun majutsu-ediff--control-required-inputs (spec)
  "Return temp input files that should exist before handling control SPEC."
  (pcase (plist-get spec :type)
    ('diff
     (when-let* ((file (plist-get spec :file))
                 (left-dir (majutsu-jj-expand-directory-from-jj
                            (plist-get spec :left)))
                 (right-dir (majutsu-jj-expand-directory-from-jj
                             (plist-get spec :right))))
       (list (expand-file-name file left-dir)
             (expand-file-name file right-dir))))
    ('merge
     (list (majutsu-jj-expand-filename-from-jj (plist-get spec :left))
           (majutsu-jj-expand-filename-from-jj (plist-get spec :base))
           (majutsu-jj-expand-filename-from-jj (plist-get spec :right))))
    (_ nil)))

(defun majutsu-ediff--wait-for-input-files (files)
  "Return non-nil when FILES become accessible before timeout."
  (let ((deadline (+ (float-time) majutsu-ediff--sleeping-input-ready-timeout))
        ready)
    (while (and (not (setq ready
                           (cl-every (lambda (file)
                                       (condition-case nil
                                           (file-exists-p file)
                                         (error nil)))
                                     files)))
                (< (float-time) deadline))
      (accept-process-output nil majutsu-ediff--sleeping-input-ready-interval))
    ready))

(defun majutsu-ediff--signal-control-session (pid ok)
  "Signal PID when control session completes.
When OK is non-nil send USR1, otherwise send USR2."
  (process-file "kill" nil nil nil
                "-s" (if ok "USR1" "USR2") pid))

(defun majutsu-ediff--run-control-packet (spec directory)
  "Handle parsed control SPEC in DIRECTORY and signal completion."
  (let ((default-directory (or directory default-directory))
        (inputs nil)
        (ok nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq inputs (majutsu-ediff--control-required-inputs spec))
              (unless (majutsu-ediff--wait-for-input-files inputs)
                (error "temporary inputs unavailable: %s"
                       (mapconcat #'identity inputs ", ")))
              (pcase (plist-get spec :type)
                ('diff
                 (majutsu-ediff-diffedit-file
                  (plist-get spec :left)
                  (plist-get spec :right)
                  (plist-get spec :file)))
                ('merge
                 (majutsu-ediff-merge-files
                  (plist-get spec :left)
                  (plist-get spec :base)
                  (plist-get spec :right)
                  (plist-get spec :output)
                  (plist-get spec :marker))))
              (setq ok t))
          ((error quit)
           (message "Majutsu Ediff control callback failed: %s"
                    (error-message-string err))))
      (condition-case err
          (majutsu-ediff--signal-control-session (plist-get spec :pid) ok)
        ((error quit)
         (message "Majutsu Ediff failed to signal editor process: %s"
                  (error-message-string err)))))))

(defun majutsu-ediff--handle-control-line (process line)
  "Parse and schedule Majutsu Ediff control LINE emitted by PROCESS.
Return non-nil when LINE is recognized as a Majutsu Ediff control packet."
  (when-let* ((spec (majutsu-ediff--control-packet-spec line)))
    (let ((directory (or (and process (process-get process 'default-dir))
                         default-directory)))
      (run-at-time 0 nil #'majutsu-ediff--run-control-packet spec directory))
    t))

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
  (let ((store-file ediff-merge-store-file))
    (when (ediff-buffer-live-p ediff-buffer-C)
      (when (and (buffer-modified-p ediff-buffer-C)
                 (stringp store-file)
                 (> (length store-file) 0)
                 (majutsu-ediff--confirm-save-merge store-file))
        (ediff-with-current-buffer ediff-buffer-C
          ;; `ediff-merge-store-file' is local to the control buffer, so keep
          ;; a stable copy before switching to buffer C.
          (write-region nil nil store-file nil 'silent)))
      (ediff-with-current-buffer ediff-buffer-C
        ;; Avoid save prompts during cleanup. Output is already persisted above.
        (set-buffer-modified-p nil))
      (ediff-kill-buffer-carefully ediff-buffer-C))))

(defun majutsu-ediff--confirm-save-merge (store-file)
  "Return non-nil when merge result should be saved to STORE-FILE."
  (yes-or-no-p (format "Conflict resolution finished; save %s? " store-file)))

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
      (majutsu-completing-read "Diffedit file" files nil t nil
                               'majutsu-file-path-history
                               nil 'majutsu-file)))))

;;;###autoload(autoload 'majutsu-ediff-dwim "majutsu-ediff" nil t)
(transient-define-suffix majutsu-ediff-dwim ()
  "Context-aware Ediff based on current section."
  :advice* #'majutsu--transient-with-selection-buffer
  (interactive)
  (cl-labels ((dwim ()
                (magit-section-case
                  (jj-hunk
                   (save-excursion
                     (goto-char (oref (oref it parent) start))
                     (dwim)))
                  (jj-file
                   (let* ((file (oref it value))
                          (range (majutsu-ediff--current-range)))
                     (majutsu-ediff-compare (car range) (cdr range) file)))
                  (jj-commit
                   (majutsu-ediff-show-revision (oref it value)))
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
                       (majutsu-ediff-show-revision "@"))))))))
    (dwim)))

;;;###autoload(autoload 'majutsu-ediff-edit "majutsu-ediff" nil t)
(transient-define-suffix majutsu-ediff-edit (args)
  "Edit one changed file with two-sided Ediff via jj diffedit.
ARGS are transient arguments."
  :advice* #'majutsu--transient-with-selection-buffer
  (interactive
   (list (when (eq transient-current-command 'majutsu-ediff)
           (transient-args 'majutsu-ediff))))
  (let* ((range (majutsu-diffedit--range args))
         (file (majutsu-diffedit--read-file (car range) (cdr range)))
         (jj-args (majutsu-diffedit--command-args args)))
    (majutsu-diffedit-run jj-args file #'majutsu-ediff--diff-editor-config)))

;;;###autoload(autoload 'majutsu-ediff-resolve "majutsu-ediff" nil t)
(transient-define-suffix majutsu-ediff-resolve (&optional file)
  "Resolve FILE conflicts using `jj resolve' with Emacs as merge editor.
If FILE is nil, DWIM selects from conflicted files at point revision (commit
section) or the working copy."
  :advice* #'majutsu--transient-with-selection-buffer
  (interactive)
  (let* ((rev (or (majutsu-revision-at-point) "@"))
         (file (majutsu-ediff--resolve-file-dwim file))
         (sides (majutsu-ediff--conflict-side-count rev file)))
    (if (> sides 2)
        (progn
          (message "%s has %d sides; using diffedit fallback" file sides)
          (majutsu-diffedit-run-with-editor (list "-r" rev) file))
      (majutsu-ediff--run-resolve rev file))))

;;;###autoload(autoload 'majutsu-ediff-resolve-with-conflict "majutsu-ediff" nil t)
(transient-define-suffix majutsu-ediff-resolve-with-conflict ()
  "Resolve conflicts using `majutsu-conflict-mode'.
When resolving a non-working-copy revision, open the matching blob buffer
at that revision before enabling conflict mode."
  :advice* #'majutsu--transient-with-selection-buffer
  (interactive)
  (let* ((rev (or (majutsu-revision-at-point) "@"))
         (file (majutsu-ediff--resolve-file-dwim))
         (buffer (majutsu-ediff--resolve-target-buffer rev file)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (majutsu-conflict-ensure-mode)
      (majutsu-conflict-goto-nearest))))

;;;###autoload
(defun majutsu-ediff-diffedit-file (left right &optional file-hint)
  "Compare one file from LEFT and RIGHT using Ediff.
This is called by jj diffedit when using Emacs as the diff editor.
Blocks until user finishes editing and quits Ediff."
  (interactive "DLeft directory: \nDRight directory: ")
  (let* ((left (majutsu-jj-expand-directory-from-jj left))
         (right (majutsu-jj-expand-directory-from-jj right))
         (file-hint (and file-hint
                         (not (string-empty-p file-hint))
                         file-hint))
         (file (or file-hint
                   (majutsu-ediff--read-directory-file left right)))
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
  (let* ((left (majutsu-jj-expand-filename-from-jj left))
         (base (majutsu-jj-expand-filename-from-jj base))
         (right (majutsu-jj-expand-filename-from-jj right))
         (output (majutsu-jj-expand-filename-from-jj output))
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

;;;###autoload(autoload 'majutsu-ediff "majutsu-ediff" nil t)
(transient-define-prefix majutsu-ediff ()
  "Show differences using Ediff."
  :class 'majutsu-jj-transient-prefix
  :jj-command "diff"
  :incompatible '(("--revisions=" "--from=")
                  ("--revisions=" "--to="))
  :transient-non-suffix t
  :description "Ediff"
  [["Selection"
    (majutsu-ediff:-r)
    (majutsu-ediff:--from)
    (majutsu-ediff:--to)
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
  :selection-toggle-key "r"
  :shortarg "-r"
  :argument "--revisions="
  :multi-value 'repeat
  :prompt "Revisions"
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-ediff:--from ()
  :description "From"
  :class 'majutsu-selection-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :selection-toggle-key "f"
  :shortarg "-f"
  :argument "--from="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-ediff:--to ()
  :description "To"
  :class 'majutsu-selection-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :selection-toggle-key "t"
  :shortarg "-t"
  :argument "--to="
  :reader #'majutsu-transient-read-revset)

;;; _
(provide 'majutsu-ediff)
;;; majutsu-ediff.el ends here
