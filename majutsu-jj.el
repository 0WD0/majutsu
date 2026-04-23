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
(require 'compat)
(require 'cl-lib)
(require 'json)
(require 'magit-section)
(require 'seq)
(require 'subr-x)

(require 'with-editor)
(require 'majutsu-base)
(require 'majutsu-completion)

(eval-when-compile (require 'majutsu-template))

(autoload 'majutsu-process-file "majutsu-process" nil nil)
(declare-function majutsu-marginalia-annotate-revision "majutsu-marginalia" (cand))

(defvar corfu-margin-formatters)

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

(defcustom majutsu-jj-diffstat-columns 80
  "Columns to expose when running `jj diff --stat'.
When non-nil, Majutsu sets `COLUMNS' to this value for diffstat commands.
This stabilizes output in very narrow terminal environments (common with
TRAMP) while keeping stat bars readable.  Increase the value to prioritize
full path visibility, or set to nil to inherit terminal width."
  :group 'majutsu-commands
  :group 'majutsu-process
  :type '(choice (const :tag "Inherit environment" nil)
          (integer :tag "Columns")))

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
        (let ((words
               ;; `split-string-shell-command' can mis-parse with-editor's
               ;; sleeping editor and yield only ("wait" "$sleep").
               ;; Detect `PROGRAM -c 'SCRIPT'' forms directly and preserve
               ;; the script as a single argument.
               (or (and (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+-c[[:space:]]+'" raw)
                        (let ((program (match-string 1 raw))
                              (start (match-end 0)))
                          (when (and (<= start (length raw))
                                     (string-suffix-p "'" raw))
                            (list program "-c" (substring raw start -1)))))
                   (split-string-shell-command raw))))
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

(defun majutsu-convert-filename-for-jj (filename)
  "Convert FILENAME so it can be passed to a jj subprocess.
This follows Magit's `magit-convert-filename-for-git' direction: absolute
remote file names are converted to host-local paths before being passed in
`process-file'/`start-file-process' ARGS. Relative names are left untouched."
  (if (not (file-name-absolute-p filename))
      filename
    (let ((expanded (expand-file-name filename)))
      (or (file-remote-p expanded 'localname)
          expanded))))

(defun majutsu-jj-expand-filename-from-jj (path &optional directory)
  "Expand PATH returned by jj into an Emacs-usable file name.
If PATH is an absolute local path and DIRECTORY is remote, prepend the
remote prefix from DIRECTORY so the result remains remote."
  (let* ((base (or directory default-directory))
         (remote (file-remote-p base)))
    (cond
     ((file-remote-p path)
      (expand-file-name path))
     ((and remote (file-name-absolute-p path))
      (concat remote path))
     (t
      (expand-file-name path base)))))

(defun majutsu-jj-expand-directory-from-jj (path &optional directory)
  "Like `majutsu-jj-expand-filename-from-jj', but always return a directory."
  (file-name-as-directory (majutsu-jj-expand-filename-from-jj path directory)))

;;; Reading

(defvar majutsu-read-revset-history nil
  "Minibuffer history for `majutsu-read-revset'.")

(defvar majutsu-jj--revset-completion-args nil
  "Dynamic jj command context for revset minibuffer completion.")

(defvar majutsu-jj--revset-completion-default nil
  "Dynamic default value for revset minibuffer completion.")

(defvar majutsu-read-revset-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'majutsu-jj-revset-complete)
    (define-key map (kbd "<tab>") #'majutsu-jj-revset-complete)
    (define-key map [remap completion-at-point]
                #'majutsu-jj-revset-complete)
    (make-composed-keymap map minibuffer-local-map))
  "Keymap used while reading revset expressions.")

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

(defun majutsu-jj--machine-completion-kind (item)
  "Return revision completion kind derived from machine-readable ITEM."
  (let ((value (gethash "value" item)))
    (pcase (gethash "display_order" item)
      (0 (if (and (stringp value)
                  (string-suffix-p "@" value))
             'workspace
           'bookmark))
      (1 'tag)
      (2 'change-id)
      (3 'remote-bookmark)
      (4 'revset-alias)
      (_ nil))))

(defun majutsu-jj--machine-completion-entry (item)
  "Return structured completion entry parsed from machine-readable ITEM."
  (when-let* ((value (gethash "value" item))
              ((stringp value))
              ((not (string-empty-p value))))
    (let ((help (gethash "help" item))
          (id (gethash "id" item))
          (tag (gethash "tag" item))
          (display-order (gethash "display_order" item))
          (hidden (gethash "hidden" item)))
      (list :value value
            :annotation (or (and (stringp help) (not (string-empty-p help)) help)
                            (and (stringp tag) (not (string-empty-p tag)) tag))
            :help (and (stringp help) (not (string-empty-p help)) help)
            :id (and (stringp id) (not (string-empty-p id)) id)
            :tag (and (stringp tag) (not (string-empty-p tag)) tag)
            :display-order display-order
            :hidden hidden
            :kind (majutsu-jj--machine-completion-kind item)))))

(defun majutsu-jj--completion-payload-from-entries (entries &optional category)
  "Return completion payload built from structured ENTRIES.
CATEGORY defaults to `majutsu-revision'."
  (let ((annotations (make-hash-table :test #'equal))
        (entry-table (make-hash-table :test #'equal))
        candidates)
    (dolist (entry entries)
      (when-let* ((candidate (plist-get entry :value)))
        (setq candidates (append candidates (list candidate)))
        (puthash candidate entry entry-table)
        (when-let* ((annotation (plist-get entry :annotation)))
          (puthash candidate annotation annotations))))
    (list :category (or category 'majutsu-revision)
          :candidates candidates
          :annotations annotations
          :entries entry-table)))

(defun majutsu-jj--machine-completion-payload (args &optional category)
  "Return jj machine-readable completion payload for ARGS.
ARGS are command-line arguments after the leading jj executable name.
Return a payload on success, or the symbol `:failed' if machine-readable
completion is not available."
  (condition-case nil
      (with-temp-buffer
        (let* ((argv (append '("util" "complete"
                               "--format" "json"
                               "--index")
                             (list (number-to-string (length args)))
                             '("--" "jj")
                             args))
               (exit (apply #'majutsu-process-file
                            (majutsu-jj--executable)
                            nil t nil
                            argv)))
          (if (zerop exit)
              (progn
                (goto-char (point-min))
                (majutsu-jj--completion-payload-from-entries
                 (delq nil
                       (mapcar #'majutsu-jj--machine-completion-entry
                               (json-parse-buffer :array-type 'list
                                                  :object-type 'hash-table
                                                  :null-object nil
                                                  :false-object nil)))
                 category))
            :failed)))
    (error :failed)))

(defun majutsu-jj--fish-completion-entry (item)
  "Return structured completion entry parsed from fish completion ITEM."
  (let ((candidate (if (consp item) (car item) item))
        (annotation (and (consp item) (cdr item))))
    (when (and (stringp candidate) (not (string-empty-p candidate)))
      (list :value candidate
            :annotation annotation
            :help annotation))))

(defun majutsu-jj--fish-completion-items (args)
  "Return jj shell completion items for ARGS using fish output format."
  (condition-case nil
      (with-temp-buffer
        (let* ((process-environment (cons "COMPLETE=fish" process-environment))
               (exit (apply #'majutsu-process-file
                            (majutsu-jj--executable)
                            nil t nil
                            (append '("--" "jj") args))))
          (when (zerop exit)
            (delq nil
                  (mapcar #'majutsu-completion-parse-annotated-line
                          (split-string (buffer-string) "\n" t))))))
    (error nil)))

(defun majutsu-jj--fish-completion-payload (args &optional category)
  "Return jj shell completion payload for ARGS using fish output format."
  (majutsu-jj--completion-payload-from-entries
   (mapcar #'majutsu-jj--fish-completion-entry
           (majutsu-jj--fish-completion-items args))
   category))

(defun majutsu-jj--completion-payload (args &optional category)
  "Return jj native completion payload for ARGS.
ARGS are command-line arguments after the leading jj executable name.
Prefer the machine-readable `jj util complete' interface when available, and
fall back to jj's shell completion protocol for older jj versions."
  (let ((payload (majutsu-jj--machine-completion-payload args category)))
    (if (eq payload :failed)
        (majutsu-jj--fish-completion-payload args category)
      payload)))

(defun majutsu-jj-completion-items (args)
  "Return jj native completion items for ARGS.
ARGS are command-line arguments after the leading jj executable name.
Each returned item is (CANDIDATE . HELP)."
  (majutsu-completion-payload-items
   (majutsu-jj--completion-payload args 'majutsu-revision)))

(defun majutsu-jj--completion-normalize-annotation (annotation)
  "Return ANNOTATION in Emacs completion metadata format."
  (when (and (stringp annotation) (not (string-empty-p annotation)))
    (if (string-prefix-p " " annotation)
        annotation
      (concat " " annotation))))

(defun majutsu-jj--revision-margin-label (entry)
  "Return a fixed-width short kind label for revision completion ENTRY."
  (pcase (plist-get entry :kind)
    ('bookmark "Bm ")
    ('remote-bookmark "Rb ")
    ('tag "Tag")
    ('workspace "Ws ")
    ('pseudo "@  ")
    ('change-id "Ch ")
    ('revset-alias "Al ")
    (_ "   ")))

(defun majutsu-jj--revision-company-kind (entry)
  "Return Company/Corfu kind symbol for revision completion ENTRY."
  (pcase (plist-get entry :kind)
    ((or 'bookmark 'remote-bookmark) 'reference)
    ('tag 'constant)
    ('workspace 'module)
    ('pseudo 'keyword)
    ('change-id 'value)
    ('revset-alias 'function)
    (_ 'text)))

(defun majutsu-jj--revision-margin-face (entry)
  "Return face used for revision completion ENTRY in Corfu margin."
  (pcase (plist-get entry :kind)
    ((or 'bookmark 'remote-bookmark) 'font-lock-keyword-face)
    ('tag 'font-lock-constant-face)
    ('workspace 'font-lock-variable-name-face)
    ('pseudo 'shadow)
    ('change-id 'font-lock-type-face)
    ('revset-alias 'font-lock-function-name-face)
    (_ 'shadow)))

(defun majutsu-jj--corfu-revision-margin-formatter (metadata)
  "Return Corfu margin formatter for revision completion METADATA."
  (when (eq (completion-metadata-get metadata 'category) 'majutsu-revision)
    (let ((entries (plist-get completion-extra-properties :majutsu-revision-entries)))
      (lambda (candidate)
        (let* ((entry (and (hash-table-p entries)
                           (gethash candidate entries)))
               (label (majutsu-jj--revision-margin-label entry)))
          (propertize label 'face (majutsu-jj--revision-margin-face entry)))))))

(defun majutsu-jj--revision-doc-buffer-function (entries)
  "Return popupinfo doc buffer function backed by revision ENTRIES."
  (lambda (candidate)
    (when-let* ((entry (and (hash-table-p entries)
                            (gethash candidate entries))))
      (with-current-buffer (get-buffer-create " *majutsu revision doc*")
        (erase-buffer)
        (insert candidate)
        (when-let* ((kind (plist-get entry :kind)))
          (insert "\nkind: " (symbol-name kind)))
        (when-let* ((sources (plist-get entry :sources)))
          (insert "\nsources: "
                  (mapconcat (lambda (source)
                               (format "%s" source))
                             sources ", ")))
        (when-let* ((tag (plist-get entry :tag)))
          (insert "\ntag: " tag))
        (when-let* ((id (plist-get entry :id)))
          (insert "\nid: " id))
        (when-let* ((help (plist-get entry :help)))
          (insert "\n\n" help))
        (current-buffer)))))

(with-eval-after-load 'corfu
  (add-hook 'corfu-margin-formatters #'majutsu-jj--corfu-revision-margin-formatter t))

(defun majutsu-jj--completion-candidates (payload default)
  "Return completion candidates from PAYLOAD, prepending DEFAULT if needed."
  (let ((candidates (copy-sequence (or (plist-get payload :candidates) nil))))
    (if (and default
             (stringp default)
             (not (string-empty-p default))
             (not (member default candidates)))
        (cons default candidates)
      candidates)))

(defun majutsu-jj--completion-table (args &optional category default)
  "Return a dynamic Emacs completion table using jj native completion.
ARGS are command-line arguments before the value being completed.  The
current input string is passed to jj as the value argument, which lets
expression arguments such as revsets continue completing after operators
like `|' or `..'.  CATEGORY, when non-nil, is exposed in completion
metadata.  DEFAULT, when non-empty and missing from jj's candidates, is
added first."
  (let ((payload-cache (make-hash-table :test #'equal))
        (annotations (make-hash-table :test #'equal))
        (entries (make-hash-table :test #'equal))
        (metadata-category (or category 'majutsu-revision)))
    (cl-labels
        ((payload-for
           (string)
           (or (gethash string payload-cache)
               (let ((payload (majutsu-jj--completion-payload
                               (append args (list string))
                               metadata-category)))
                 (puthash string payload payload-cache)
                 (when-let* ((payload-annotations (plist-get payload :annotations)))
                   (maphash (lambda (candidate annotation)
                              (when-let* ((normalized (majutsu-jj--completion-normalize-annotation annotation)))
                                (puthash candidate normalized annotations)))
                            payload-annotations))
                 (when-let* ((payload-entries (plist-get payload :entries)))
                   (maphash (lambda (candidate entry)
                              (puthash candidate entry entries))
                            payload-entries))
                 (majutsu-completion-prewarm-payload
                  payload metadata-category string default-directory)
                 payload))))
      (lambda (string predicate action)
        (if (eq action 'metadata)
            `(metadata
              (display-sort-function . identity)
              (category . ,metadata-category)
              (annotation-function
               . ,(lambda (candidate)
                    (gethash candidate annotations))))
          (complete-with-action
           action
           (majutsu-jj--completion-candidates (payload-for string) default)
           string predicate))))))

(defun majutsu-jj--revset-annotation-function (sources)
  "Return annotation function from candidate SOURCES table."
  (lambda (candidate)
    (when-let* ((kinds (gethash candidate sources)))
      (let* ((ordered (seq-filter (lambda (k) (memq k kinds)) majutsu-jj--revset-source-order))
             (extra (seq-remove (lambda (k) (memq k majutsu-jj--revset-source-order)) kinds))
             (labels (mapcar #'majutsu-jj--revset-source-label (append ordered extra))))
        (format "  [%s]" (string-join labels ","))))))

(defun majutsu-jj--revset-annotations (sources candidates)
  "Return annotation hash for revset CANDIDATES from SOURCES."
  (let ((annotations (make-hash-table :test #'equal))
        (annotation (majutsu-jj--revset-annotation-function sources)))
    (dolist (candidate candidates)
      (when-let* ((value (funcall annotation candidate)))
        (puthash candidate value annotations)))
    annotations))

(defun majutsu-jj--revset-primary-source (sources)
  "Return the primary revision source symbol from SOURCES."
  (or (seq-find (lambda (source)
                  (memq source sources))
                majutsu-jj--revset-source-order)
      (car sources)))

(defun majutsu-jj--revset-entries (sources candidates)
  "Return entry table for revset CANDIDATES backed by SOURCES."
  (let ((entries (make-hash-table :test #'equal)))
    (dolist (candidate candidates)
      (let ((candidate-sources (gethash candidate sources)))
        (puthash candidate
                 (list :value candidate
                       :kind (majutsu-jj--revset-primary-source candidate-sources)
                       :sources candidate-sources)
                 entries)))
    entries))

(defun majutsu-jj-revset-candidate-data (&optional default)
  "Return revset completion payload.
The return value is a plist with keys :category, :candidates,
:annotations, and :sources."
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
      (list :category 'majutsu-revision
            :candidates candidates
            :annotations (majutsu-jj--revset-annotations sources candidates)
            :entries (majutsu-jj--revset-entries sources candidates)
            :sources sources))))

(defun majutsu-jj-revset-candidates (&optional default)
  "Return completion candidates for revset prompts.
Candidates include pseudo revisions and repository references such as
workspace working-copy refs (`<workspace>@`), bookmarks, and tags.
DEFAULT, when non-nil, is inserted first so users can accept it quickly."
  (plist-get (majutsu-jj-revset-candidate-data default) :candidates))

(defun majutsu-jj--payload-table (payload &optional category default)
  "Return a completion table for PAYLOAD."
  (let* ((category (or category (plist-get payload :category)))
         (table (majutsu-completion-payload-table payload category default))
         (annotations (plist-get payload :annotations)))
    (if (eq category 'majutsu-revision)
        (completion-table-with-metadata
         table
         `((display-sort-function . identity)
           (category . ,category)
           (annotation-function
            . ,(lambda (candidate)
                 (and (hash-table-p annotations)
                      (gethash candidate annotations))))))
      table)))

(defun majutsu-jj--revset-completion-table (&optional default)
  "Return a completion table for revset input.
DEFAULT is inserted first in the candidate list when non-nil."
  (majutsu-jj--payload-table
   (majutsu-jj-revset-candidate-data default)
   'majutsu-revision default))

(defun majutsu-jj--revset-minibuffer-input ()
  "Return revset minibuffer contents before point."
  (buffer-substring-no-properties (minibuffer-prompt-end) (point)))

(defun majutsu-jj--revset-completion-payload (input)
  "Return completion payload for revset INPUT."
  (if majutsu-jj--revset-completion-args
      (majutsu-jj--completion-payload
       (append majutsu-jj--revset-completion-args (list input))
       'majutsu-revision)
    (majutsu-jj-revset-candidate-data majutsu-jj--revset-completion-default)))

(defun majutsu-jj-revset-completion-at-point ()
  "Return completion data for the revset expression at point."
  (let* ((input (majutsu-jj--revset-minibuffer-input))
         (payload (majutsu-jj--revset-completion-payload input))
         (entries (plist-get payload :entries))
         (annotations (plist-get payload :annotations))
         (table (majutsu-jj--payload-table
                 payload 'majutsu-revision
                 majutsu-jj--revset-completion-default)))
    (majutsu-completion-prewarm-payload
     payload 'majutsu-revision input default-directory)
    (when (plist-get payload :candidates)
      (list (minibuffer-prompt-end) (point) table
            :exclusive 'no
            :category 'majutsu-revision
            :annotation-function (lambda (candidate)
                                   (and (hash-table-p annotations)
                                        (gethash candidate annotations)))
            :company-kind (lambda (candidate)
                            (majutsu-jj--revision-company-kind
                             (and (hash-table-p entries)
                                  (gethash candidate entries))))
            :company-doc-buffer (majutsu-jj--revision-doc-buffer-function entries)
            :majutsu-revision-entries entries))))

(defun majutsu-jj-revset-complete ()
  "Complete the revset expression at point using jj's native completer."
  (interactive)
  (unless (completion-at-point)
    (minibuffer-message "No jj revset completions")))

(defun majutsu-jj--revset-minibuffer-setup ()
  "Install revset completion-at-point for the current minibuffer."
  (add-hook 'completion-at-point-functions
            #'majutsu-jj-revset-completion-at-point nil t))

(defun majutsu-read-single-revset (prompt &optional default completion-args history)
  "Prompt user with PROMPT to read a single revision selector.

Unlike `majutsu-read-revset', this reader is intended for arguments such as
`--from' and `--to' which accept one revision selector rather than a full
revset expression.  When COMPLETION-ARGS is non-nil, use jj's native completer
in that command context.  HISTORY defaults to `majutsu-read-revset-history'."
  (let* ((default (or default
                      (majutsu-thing-at-point 'jj-revision t)
                      (majutsu-revision-at-point)
                      "@"))
         (payload (if completion-args
                      (majutsu-jj--completion-payload
                       (append completion-args '(""))
                       'majutsu-revision)
                    (majutsu-jj-revset-candidate-data default)))
         (table (if completion-args
                    (majutsu-jj--completion-table completion-args
                                                  'majutsu-revision
                                                  default)
                  (majutsu-jj--payload-table payload
                                             'majutsu-revision default))))
    (when payload
      (majutsu-completion-prewarm-payload payload 'majutsu-revision nil default-directory))
    (let ((value (completing-read (format-prompt prompt default)
                                  table nil nil nil
                                  (or history 'majutsu-read-revset-history)
                                  default)))
      (cond
       ((not (string-empty-p value)) value)
       ((and default (not (string-empty-p default))) default)
       (t (user-error "Need non-empty input"))))))

(defun majutsu-read-revset (prompt &optional default completion-args)
  "Prompt user with PROMPT to read a revision set string.
Completion candidates include workspaces, bookmarks, and tags, while
still allowing free-form revset expressions.

When COMPLETION-ARGS is non-nil, use jj's native completer in that
command context.  COMPLETION-ARGS are command-line arguments before the
revset value being read."
  (let* ((default (or default
                      (majutsu-thing-at-point 'jj-revision t)
                      (majutsu-revision-at-point)
                      "@"))
         (majutsu-jj--revset-completion-args completion-args)
         (majutsu-jj--revset-completion-default default)
         (payload (unless completion-args
                    (majutsu-jj-revset-candidate-data default))))
    (when payload
      (majutsu-completion-prewarm-payload payload 'majutsu-revision nil default-directory))
    (let ((value (minibuffer-with-setup-hook
                     #'majutsu-jj--revset-minibuffer-setup
                   (read-from-minibuffer (format-prompt prompt default)
                                         nil
                                         majutsu-read-revset-map
                                         nil
                                         'majutsu-read-revset-history
                                         default))))
      (cond
       ((not (string-empty-p value)) value)
       ((and default (not (string-empty-p default))) default)
       (t (user-error "Need non-empty input"))))))

(defun majutsu-read-optional-revset (prompt &optional default initial-input history completion-args)
  "Prompt user with PROMPT to read an optional revset string.

Completion candidates include workspaces, bookmarks, and tags.  Empty
input returns nil instead of signaling an error.  DEFAULT is shown in
`format-prompt' when non-nil, and INITIAL-INPUT is inserted into the
minibuffer when non-nil.  HISTORY defaults to
`majutsu-read-revset-history'.  When COMPLETION-ARGS is non-nil, use
jj's native completer in that command context."
  (let* ((majutsu-jj--revset-completion-args completion-args)
         (majutsu-jj--revset-completion-default default)
         (payload (unless completion-args
                    (majutsu-jj-revset-candidate-data default))))
    (when payload
      (majutsu-completion-prewarm-payload payload 'majutsu-revision nil default-directory))
    (let ((value (minibuffer-with-setup-hook
                     #'majutsu-jj--revset-minibuffer-setup
                   (read-from-minibuffer (format-prompt prompt default)
                                         initial-input
                                         majutsu-read-revset-map
                                         nil
                                         (or history 'majutsu-read-revset-history)
                                         default))))
      (unless (string-empty-p value)
        value))))

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
      (majutsu-completing-read
       (format "File to compare between %s and %s: " from to)
       changed nil t nil 'majutsu-file-path-history nil 'majutsu-file)))))

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

(defvar majutsu-bookmark-faces
  '(majutsu-log-bookmark-face)
  "Faces used for JJ bookmark identifiers in Majutsu buffers.")

(defvar majutsu-tag-faces
  '(majutsu-log-tag-face)
  "Faces used for JJ tag identifiers in Majutsu buffers.")

(defvar majutsu-revision-faces
  '(majutsu-log-revision-face
    majutsu-log-change-id-face
    majutsu-log-commit-id-face
    majutsu-log-bookmark-face
    majutsu-log-tag-face)
  "Faces used for JJ revision identifiers in Majutsu buffers.")

(defun majutsu-thing-at-point (thing &optional no-properties)
  "Return THING at point.
This thin wrapper exists so Majutsu can later extend point semantics
without changing call sites."
  (thing-at-point thing no-properties))

(defun majutsu--faces-at-point (&optional pos)
  "Return all faces at POS as a list."
  (let ((faces (or (get-text-property (or pos (point)) 'font-lock-face)
                   (get-text-property (or pos (point)) 'face))))
    (cond
     ((null faces) nil)
     ((listp faces) faces)
     (t (list faces)))))

(defun majutsu--face-at-point-p (faces &optional pos)
  "Return non-nil when any of FACES appears at POS."
  (let ((faces (ensure-list faces)))
    (seq-some (lambda (face)
                (memq face (majutsu--faces-at-point pos)))
              faces)))

(defun majutsu--regexp-char-class (chars)
  "Return CHARS escaped for use inside a regexp character class."
  (mapconcat (lambda (ch)
               (if (memq ch '(?\[ ?\] ?\\ ?^ ?-))
                   (concat "\\" (char-to-string ch))
                 (char-to-string ch)))
             chars
             ""))

(defun majutsu--diff-revision-at-point ()
  "Return the revision implied by the current diff buffer, or nil."
  (and (derived-mode-p 'majutsu-diff-mode)
       (bound-and-true-p majutsu-buffer-diff-range)
       (let ((range majutsu-buffer-diff-range))
         (or (and (equal (car range) "-r") (cadr range))
             (when-let* ((arg (seq-find (lambda (item)
                                          (string-prefix-p "--revisions=" item))
                                        range)))
               (substring arg (length "--revisions=")))))))

(defun majutsu--section-revision-at-point ()
  "Return the section value at point when it identifies a JJ revision."
  (magit-section-case
    (jj-bookmark (oref it value))
    (jj-tag (oref it value))
    (jj-commit (oref it value))))

(defun majutsu--buffer-revision-at-point ()
  "Return the revision implied by the surrounding Majutsu buffer."
  (or (and (bound-and-true-p majutsu-buffer-blob-revision)
           majutsu-buffer-blob-revision)
      (majutsu--diff-revision-at-point)))

(defun majutsu-revision-at-point ()
  "Return the JJ revision at point.
Prefer semantic section values, then textual JJ revision syntax, then
buffer-implied revisions such as blob and diff contexts."
  (or (majutsu--section-revision-at-point)
      (majutsu-thing-at-point 'jj-revision t)
      (majutsu--buffer-revision-at-point)))

(defun majutsu-jj-revision-p (rev)
  "Return non-nil if REV names an existing JJ revision.
Uses `jj log -r REV -G -T self.contained_in(\"REV\")' to verify."
  (when (and rev (not (string-empty-p rev)))
    (let ((output (majutsu-jj-string "log" "-r" rev "-G" "-T"
                                     (majutsu-tpl `[:method [:self] :contained_in ,rev]))))
      (string= output "true"))))

(defun majutsu--explicit-jj-revision-syntax-p (string)
  "Return non-nil when STRING uses explicit JJ revision syntax."
  (or (and (not (string-equal string "@"))
           (= (cl-count ?@ string) 1))
      (string-match-p "\\`[^/]+/[0-9]+\\'" string)))

(put 'jj-revision 'thing-at-point #'majutsu-thingatpt--jj-revision)
(defun majutsu-thingatpt--jj-revision (&optional disallow)
  "Return the JJ revision at point, or nil if none is found.
Optional DISALLOW is a string of characters that should not appear
in the revision identifier (used for recursive refinement)."
  (and-let* ((bounds
              (let ((c (majutsu--regexp-char-class
                        (concat " \n\t~^:?*[\\|()<>" disallow))))
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
             (string (thread-first string
                                   (string-trim-left  "[][()</\"']+")
                                   (string-trim-right "[])>\"'.,;:!]+"))))
    (let* ((revision-face (majutsu--face-at-point-p majutsu-revision-faces))
           (explicit-syntax (majutsu--explicit-jj-revision-syntax-p string)))
      (when (or (string-match-p "\\.\\." string)
                (string-match-p "/\\." string))
        (setq disallow (concat disallow ".")))
      (when (> (cl-count ?@ string) 1)
        (setq disallow (concat disallow "@")))
      (when (and (string-match-p "/[0-9]" string)
                 (> (cl-count ?/ string) 1))
        (setq disallow (concat disallow "/")))
      (if disallow
          (majutsu-thingatpt--jj-revision disallow)
        (and (not (string-match-p "\`[[:space:]]*\'" string))
             (or
              (and (string-match-p "^[k-z]+$" string)
                   (>= (length string) (if revision-face 1 4))
                   (majutsu-jj-revision-p string))
              (and (>= (length string) 4)
                   (string-match-p "^[0-9a-fA-F]+$" string)
                   (majutsu-jj-revision-p string))
              (string-equal string "@")
              (and explicit-syntax
                   (majutsu-jj-revision-p string))
              (and revision-face
                   (majutsu-jj-revision-p string)))
             string)))))

(defun majutsu-file-at-point ()
  "Return file at point for jj diff sections."
  (magit-section-case
    (jj-hunk (or (magit-section-parent-value it)
                 (oref it value)))
    (jj-file (oref it value))))

(defun majutsu-bookmark-at-point (&optional _bookmark-type)
  "Return the bookmark name at point, or nil when none is found."
  (or (magit-section-value-if 'jj-bookmark)
      (and (majutsu--face-at-point-p majutsu-bookmark-faces)
           (majutsu-thing-at-point 'jj-revision t))))

(defun majutsu-tag-at-point ()
  "Return the tag name at point, or nil when none is found."
  (or (magit-section-value-if 'jj-tag)
      (and (majutsu--face-at-point-p majutsu-tag-faces)
           (majutsu-thing-at-point 'jj-revision t))))

;;; Errors

(define-error 'majutsu-outside-jj-repo "Not inside jj repository")
(define-error 'majutsu-jj-executable-not-found "jj executable cannot be found")

(defun majutsu--assert-usable-jj ()
  (let ((jj (majutsu-jj--executable)))
    (unless (executable-find jj t)
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
                (exit (apply #'majutsu-process-file (majutsu-jj--executable) nil t nil args)))
            (when (zerop exit)
              (let ((out (string-trim (buffer-string))))
                (unless (string-empty-p out)
                  (majutsu-jj-expand-directory-from-jj out default-directory))))))))))

(defun majutsu--toplevel-safe (&optional directory)
  "Return the workspace root for DIRECTORY or signal an error."
  (or (majutsu-toplevel directory)
      (let ((default-directory
             (or (majutsu--safe-default-directory directory)
                 (majutsu-jj-expand-directory-from-jj (or directory default-directory)))))
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
         (err-file (and return-error
                        (make-nearby-temp-file "majutsu-jj-err")))
         exit-code)
    (majutsu--debug "Running command: %s %s" (majutsu-jj--executable) (string-join args " "))
    (setq exit-code (apply #'majutsu-process-file (majutsu-jj--executable) nil
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
  (let* ((beg (point))
         (exit (apply #'majutsu-process-file (majutsu-jj--executable) nil t nil args)))
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
