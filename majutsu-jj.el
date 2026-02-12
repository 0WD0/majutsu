;;; majutsu-jj.el --- JJ functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of command output helpers are adapted from:
;; - Magit `lisp/magit-git.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors

;;; Commentary:

;; This library provides early utilities and section subclasses that
;; other Majutsu modules rely on while avoiding heavier dependencies.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
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

(defcustom majutsu-remote-jj-executable "jj"
  "Path to jj executable on remote hosts.
When `default-directory' is remote, this executable is used instead of
`majutsu-jj-executable'."
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

(defun majutsu-jj--toml-escape (value)
  "Escape VALUE for use inside a TOML basic string."
  (let ((escaped (replace-regexp-in-string "\\\\" "\\\\\\\\" value t t)))
    (replace-regexp-in-string "\"" "\\\"" escaped t t)))

(defun majutsu-jj--toml-array-config (key values)
  "Build KEY=[...] TOML config from string VALUES."
  (format "%s=[%s]"
          key
          (mapconcat (lambda (value)
                       (format "\"%s\"" (majutsu-jj--toml-escape value)))
                     values
                     ", ")))

(defun majutsu-jj--editor-command-from-env ()
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

(defun majutsu-jj--editor-command-config (key target &optional editor-command)
  "Build KEY config command string editing TARGET.
EDITOR-COMMAND defaults to the command parsed from with-editor environment.
If TARGET is nil, return config for EDITOR-COMMAND as-is."
  (let ((command (append (or editor-command (majutsu-jj--editor-command-from-env))
                         (when target (list target)))))
    (majutsu-jj--toml-array-config key command)))

(defsubst majutsu-jj--executable ()
  "Return local or remote jj executable for `default-directory'."
  (if (file-remote-p default-directory)
      majutsu-remote-jj-executable
    majutsu-jj-executable))

(defun majutsu-jj--expand-path (path &optional directory)
  "Expand PATH relative to DIRECTORY while preserving remote prefix.
If PATH is an absolute local path and DIRECTORY is remote, then prepend
the remote prefix from DIRECTORY so the result remains remote."
  (let* ((base (or directory default-directory))
         (remote (file-remote-p base)))
    (cond
     ((file-remote-p path)
      (expand-file-name path))
     ((and remote (file-name-absolute-p path))
      (concat remote path))
     (t
      (expand-file-name path base)))))

(defun majutsu-jj--expand-directory (path &optional directory)
  "Expand PATH as a directory relative to DIRECTORY.
See `majutsu-jj--expand-path' for remote handling details."
  (file-name-as-directory (majutsu-jj--expand-path path directory)))

(defun majutsu-jj--local-path (path)
  "Return PATH without TRAMP prefix when PATH is remote."
  (or (file-remote-p path 'localname) path))

;;; Reading

(defvar majutsu-read-revset-history nil
  "Minibuffer history for `majutsu-read-revset'.")

(defconst majutsu-jj--revset-source-order
  '(pseudo workspace bookmark tag)
  "Source display order for revset completion metadata.")

(defun majutsu-jj--safe-lines (&rest args)
  "Return `majutsu-jj-lines' for ARGS, or nil on command failure."
  (condition-case nil
      (apply #'majutsu-jj-lines args)
    (error nil)))

(defun majutsu-jj--add-revset-source (table candidate source)
  "Record SOURCE for CANDIDATE in hash TABLE."
  (when (and (stringp candidate) (not (string-empty-p candidate)))
    (let* ((current (gethash candidate table))
           (next (if (memq source current)
                     current
                   (append current (list source)))))
      (puthash candidate next table))))

(defun majutsu-jj--revset-source-label (source)
  "Return display label for completion SOURCE."
  (pcase source
    ('pseudo "pseudo")
    ('workspace "workspace")
    ('bookmark "bookmark")
    ('tag "tag")
    (_ (symbol-name source))))

(defun majutsu-jj--revset-annotation-function (sources)
  "Return annotation function from candidate SOURCES table."
  (lambda (candidate)
    (when-let* ((kinds (gethash candidate sources)))
      (let* ((ordered (seq-filter (lambda (k) (memq k kinds)) majutsu-jj--revset-source-order))
             (extra (seq-remove (lambda (k) (memq k majutsu-jj--revset-source-order)) kinds))
             (labels (mapcar #'majutsu-jj--revset-source-label (append ordered extra))))
        (format "  [%s]" (string-join labels ","))))))

(defun majutsu-jj-revset-candidate-data (&optional default)
  "Return revset completion data.
The return value is a plist with keys :candidates and :sources."
  (let* ((sources (make-hash-table :test #'equal))
         (ordered nil)
         (workspaces (majutsu-jj--safe-lines "workspace" "list" "-T" "name ++ \"\\n\""))
         (bookmarks (majutsu-jj--safe-lines "bookmark" "list" "--quiet" "-T" "name ++ \"\\n\""))
         (tags (majutsu-jj--safe-lines "tag" "list" "--quiet" "-T" "name ++ \"\\n\"")))
    (cl-labels ((add (candidate source)
                  (when (and (stringp candidate) (not (string-empty-p candidate)))
                    (unless (gethash candidate sources)
                      (setq ordered (append ordered (list candidate))))
                    (majutsu-jj--add-revset-source sources candidate source))))
      (dolist (pseudo '("@" "@-" "@+"))
        (add pseudo 'pseudo))
      (dolist (name workspaces)
        (add (concat name "@") 'workspace))
      (dolist (name bookmarks)
        (add name 'bookmark))
      (dolist (name tags)
        (add name 'tag)))
    (let ((candidates (if (and default (not (string-empty-p default)))
                          (cons default (delete default ordered))
                        ordered)))
      (list :candidates candidates
            :sources sources))))

(defun majutsu-jj-revset-candidates (&optional default)
  "Return completion candidates for revset prompts.
Candidates include pseudo revisions and repository references such as
workspace working-copy refs (`<workspace>@`), bookmarks, and tags.
DEFAULT, when non-nil, is inserted first so users can accept it quickly."
  (plist-get (majutsu-jj-revset-candidate-data default) :candidates))

(defun majutsu-read-revset (prompt &optional default)
  "Prompt user with PROMPT to read a revision set string.
Completion candidates include workspaces, bookmarks, and tags, while
still allowing free-form revset expressions."
  (let* ((default (or default (magit-section-value-if 'jj-commit) "@"))
         (data (majutsu-jj-revset-candidate-data default))
         (candidates (plist-get data :candidates))
         (sources (plist-get data :sources))
         (annotation (majutsu-jj--revset-annotation-function sources))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (display-sort-function . identity)
                        (category . majutsu-revision)
                        (annotation-function . ,annotation))
                    (complete-with-action action candidates string pred))))
         (value (completing-read (format-prompt prompt default)
                                 table nil nil nil
                                 'majutsu-read-revset-history
                                 default)))
    (if (string-empty-p value)
        (user-error "Need non-empty input")
      value)))

(defun majutsu-jj--parse-diff-range (range)
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

(defun majutsu-jj-read-diff-file (from to)
  "Read file to compare between FROM and TO revisions."
  (unless (and from to)
    (user-error "Expected both from/to, got %S and %S" from to))
  (let* ((root (majutsu--toplevel-safe))
         (default-directory root)
         (changed (majutsu-jj-lines "diff" "--from" from "--to" to "--name-only")))
    (cond
     ((null changed)
      (user-error "No files changed between %s and %s" from to))
     ((= (length changed) 1)
      (car changed))
     (t
      (completing-read
       (format "File to compare between %s and %s: " from to)
       changed nil t)))))

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

(defvar majutsu-revision-faces
  '(majutsu-log-revision-face
    majutsu-log-change-id-face
    majutsu-log-commit-id-face
    majutsu-log-bookmark-face
    majutsu-log-tag-face)
  "Faces used for JJ revision identifiers in Majutsu buffers.")

(defun majutsu-jj-revision-p (rev)
  "Return non-nil if REV names an existing JJ revision.
Uses `jj log -r REV -G -T self.contained_in(\"REV\")' to verify."
  (when (and rev (not (string-empty-p rev)))
    (let ((output (majutsu-jj-string "log" "-r" rev "-G" "-T" (majutsu-tpl `[:method 'self :contained_in ,rev]))))
      (string= output "true"))))

(put 'jj-revision 'thing-at-point #'majutsu-thingatpt--jj-revision)
(defun majutsu-thingatpt--jj-revision (&optional disallow)
  "Return the JJ revision at point, or nil if none is found.
Optional DISALLOW is a string of characters that should not appear
in the revision identifier (used for recursive refinement)."
  ;; Support change IDs, commit IDs, and references (bookmarks, tags, etc.)
  (and-let* ((bounds
              (let ((c (concat "\s\n\t~^:?*[\\|()<>@" disallow)))
                (cl-letf
                    (((get 'jj-revision 'beginning-op)
                      (lambda ()
                        (if (re-search-backward (format "[%s]" c) nil t)
                            (forward-char)
                          (goto-char (point-min)))))
                     ((get 'jj-revision 'end-op)
                      (lambda ()
                        (re-search-forward (format "\\=[^%s]*" c) nil t))))
                  (bounds-of-thing-at-point 'jj-revision))))
             (string (buffer-substring-no-properties (car bounds) (cdr bounds)))
             ;; References are allowed to contain most punctuation,
             ;; but if those appear at edges, they're likely delimiters.
             (string (thread-first string
                                   (string-trim-left  "[(</\"'")
                                   (string-trim-right "[])>\"'.,;:!]"))))
    (let (disallow)
      ;; Handle ranges (x..y) and special syntax
      (when (or (string-match-p "\\.\\." string)
                (string-match-p "/\\." string))
        (setq disallow (concat disallow ".")))
      ;; Handle change offset syntax (xyz/0, xyz/1)
      (when (and (string-match-p "/[0-9]" string)
                 (not (string-match-p "@" string)))
        ;; Keep the slash for change offset, but disallow further slashes
        (setq disallow (concat disallow "/")))
      ;; Handle remote references (name@remote)
      (when (and (string-match-p "@" string)
                 (not (string-equal string "@")))
        ;; Allow one @ for remote refs, but not more
        (unless (string-match-p "^@[a-zA-Z]" string)
          (setq disallow (concat disallow "@"))))
      (if disallow
          ;; Recurse with additional restrictions
          (majutsu-thingatpt--jj-revision disallow)
        ;; Final validation
        (and (not (string-match-p "^[\s\t\n]*$" string))
             (or
              ;; Check if it looks like a change ID (k-z range letters)
              ;; Change IDs use k-z instead of 0-9a-f; default display is lowercase
              (and (>= (length string) 1)
                   (string-match-p "^[k-z]+$" string)
                   ;; Shortest unique prefix can be as short as 1 char
                   (majutsu-jj-revision-p string))
              ;; Check if it looks like a commit ID (hex)
              (and (>= (length string) 4)
                   (string-match-p "^[0-9a-fA-F]+$" string)
                   (majutsu-jj-revision-p string))
              ;; Check if it's @ (working copy)
              (string-equal string "@")
              ;; Check if it's a bookmark/tag with proper face
              (and (member (get-text-property (point) 'face)
                           majutsu-revision-faces)
                   (majutsu-jj-revision-p string))
              ;; Fallback: try to validate any non-empty string
              (and (>= (length string) 1)
                   (majutsu-jj-revision-p string)))
             string)))))

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
         (lines (apply #'majutsu-jj-lines args))
         (bookmarks (split-string (string-join lines "\n") " " t)))
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
  (let ((jj (majutsu-jj--executable)))
    (unless
        (if (file-remote-p default-directory)
            (condition-case nil
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8-unix)
                        (coding-system-for-write 'utf-8-unix)
                        (exit (process-file jj nil t nil "--version")))
                    (and (integerp exit) (zerop exit))))
              (error nil))
          (executable-find jj))
      (signal 'majutsu-jj-executable-not-found (list jj))))
  nil)

(defun majutsu--not-inside-repository-error ()
  (majutsu--assert-usable-jj)
  (signal 'majutsu-outside-jj-repo (list default-directory)))

;;; JJ

(defmacro majutsu--with-no-color (&rest body)
  "Execute BODY with `--color=never' in `majutsu-jj-global-arguments'.

Replaces any existing `--color' flag so that jj produces plain text
output.  Use this around commands whose output is consumed
programmatically (paths, IDs, names, etc.)."
  (declare (indent 0) (debug (body)))
  `(let ((majutsu-jj-global-arguments
          (cons "--color=never"
                (seq-remove (lambda (arg)
                              (string-prefix-p "--color" arg))
                            majutsu-jj-global-arguments))))
     ,@body))

(defun majutsu-toplevel (&optional directory)
  "Return the workspace root for DIRECTORY or `default-directory'.

This runs `jj workspace root' and returns a directory name (with a
trailing slash) or nil if not inside a JJ workspace."
  (majutsu--with-safe-default-directory directory
    (majutsu--with-no-color
      (let* ((args (majutsu-process-jj-arguments '("workspace" "root"))))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (coding-system-for-write 'utf-8-unix)
                (exit (apply #'process-file (majutsu-jj--executable) nil t nil args)))
            (when (zerop exit)
              (let ((out (string-trim (buffer-string))))
                (unless (string-empty-p out)
                  (majutsu-jj--expand-directory out default-directory))))))))))

(defun majutsu--toplevel-safe (&optional directory)
  "Return the workspace root for DIRECTORY or signal an error."
  (or (majutsu-toplevel directory)
      (let ((default-directory
             (or (majutsu--safe-default-directory directory)
                 (majutsu-jj--expand-directory (or directory default-directory)))))
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

(defun majutsu--jj-insert (return-error &rest args)
  "Run jj with ARGS and insert output at point.

If RETURN-ERROR is nil, return the exit code.  If RETURN-ERROR is
non-nil, return the error message if the command fails, or the
exit code if there is no error output.  When RETURN-ERROR is
`full', return the full stderr output on error.

This is the low-level worker for `majutsu-jj-insert' and similar
functions."
  (setq args (majutsu-process-jj-arguments args))
  (let* ((start-time (current-time))
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (err-file (and return-error
                        (make-nearby-temp-file "majutsu-jj-err")))
         exit-code)
    (majutsu--debug "Running command: %s %s" (majutsu-jj--executable) (string-join args " "))
    (setq exit-code (apply #'process-file (majutsu-jj--executable) nil
                           (list t err-file) nil args))
    (majutsu--debug "Command completed in %.3f seconds, exit code: %d"
                    (float-time (time-subtract (current-time) start-time))
                    exit-code)
    (when (and majutsu-show-command-output (> (point-max) (point-min)))
      (majutsu--debug "Command output: %s"
                      (string-trim (buffer-substring (point-min) (point-max)))))
    (prog1 (if (and err-file (not (zerop exit-code)))
               (with-temp-buffer
                 (insert-file-contents err-file)
                 (if (eq return-error 'full)
                     (buffer-string)
                   (let ((line (car (split-string (buffer-string) "\n" t))))
                     (or line exit-code))))
             exit-code)
      (when err-file (ignore-errors (delete-file err-file))))))

(defun majutsu-jj-insert (&rest args)
  "Run jj with ARGS and insert output at point.

Return the exit code of the command."
  (apply #'majutsu--jj-insert nil args))

(defun majutsu-jj-lines (&rest args)
  "Run jj with ARGS and return output as a list of lines.

Color output is disabled so that return values are plain text.
Empty lines are omitted from the result.  If the command fails,
return nil or partial output depending on what was produced."
  (majutsu--with-no-color
    (with-temp-buffer
      (apply #'majutsu-jj-insert args)
      (split-string (buffer-string) "\n" t))))

(defun majutsu-jj-items (&rest args)
  "Run jj with ARGS and return output split by null bytes.

Color output is disabled so that return values are plain text.
This is useful for commands that use -z/--null flag.
Empty items are omitted from the result."
  (majutsu--with-no-color
    (with-temp-buffer
      (apply #'majutsu-jj-insert args)
      (split-string (buffer-string) "\0" t))))

(defun majutsu-jj-string (&rest args)
  "Run jj command with ARGS and return the first line of output.

Color output is disabled so that the return value is plain text.
If there is no output, return nil.  If the output begins with a
newline, return an empty string.  This function aligns with
`magit-git-string' behavior."
  (majutsu--with-no-color
    (with-temp-buffer
      (apply #'majutsu--jj-insert nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun majutsu-jj--escape-fileset-string (s)
  "Escape S for a jj fileset string literal."
  (unless (stringp s)
    (user-error "majutsu-jj: expected string, got %S" s))
  (apply #'concat
         (mapcar (lambda (ch)
                   (pcase ch
                     (?\" "\\\"")
                     (?\\ "\\\\")
                     (?\t "\\t")
                     (?\r "\\r")
                     (?\n "\\n")
                     (0 "\\0")
                     (27 "\\e")
                     (_
                      (if (or (< ch 32) (= ch 127))
                          (format "\\x%02X" ch)
                        (string ch)))))
                 (string-to-list s))))

(defun majutsu-jj-fileset-quote (s)
  "Return S as a jj fileset string literal."
  (format "file:\"%s\"" (majutsu-jj--escape-fileset-string s)))

(defun majutsu-jj-wash (washer keep-error &rest args)
  "Run jj with ARGS, insert output at point, then call WASHER.
KEEP-ERROR matches `magit--git-wash': nil drops stderr on error,
`wash-anyway' keeps output even on non-zero exit, anything else keeps the
error text.  Output is optionally colorized based on
`majutsu-process-apply-ansi-colors'."
  (declare (indent 2))
  (setq args (majutsu-process-jj-arguments args))
  (let ((beg (point))
        (exit (apply #'process-file (majutsu-jj--executable) nil t nil args)))
    (when (and (bound-and-true-p majutsu-process-apply-ansi-colors)
               (> (point) beg))
      ;; Use text-properties instead of overlays so that subsequent
      ;; washing/parsing that uses `buffer-substring' preserves faces.
      (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face))
        (ansi-color-apply-on-region beg (point))))
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
