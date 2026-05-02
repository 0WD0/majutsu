;;; majutsu-gerrit.el --- Gerrit integration commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library wraps jj's Gerrit commands and exposes them through
;; Majutsu transients.

;;; Code:

(require 'majutsu)
(require 'majutsu-remote)
(require 'majutsu-selection)

(require 'seq)
(require 'subr-x)

(declare-function majutsu-jj-lines "majutsu-jj")
(declare-function majutsu-transient-read-remote-name "majutsu-remote")

;;; majutsu-gerrit

(defun majutsu-gerrit--start (args &optional success-msg finish-callback)
  "Start `jj gerrit ARGS' asynchronously, for side-effects."
  (majutsu-start-jj (append '("gerrit") args) success-msg finish-callback))

(defvar majutsu-gerrit-remote-branch-history nil
  "Minibuffer history for Gerrit remote branch names.")

(defun majutsu-gerrit--remote-branch-names (&optional remote)
  "Return a list of remote branch names for REMOTE.
If REMOTE is nil, return branches from all remotes.
Uses `jj bookmark list' with a template to enumerate remote bookmarks."
  (let ((default-directory (ignore-errors (majutsu--toplevel-safe)))
        (template "if(remote, name ++ \"\\n\", \"\")"))
    (when default-directory
      (delete-dups
       (sort
        (delq nil
              (mapcar
               (lambda (line)
                 (let ((s (string-trim line)))
                   (unless (string-empty-p s) s)))
               (condition-case _
                   (apply #'majutsu-jj-lines
                          (append (list "bookmark" "list" "--quiet" "-T" template)
                                  (and remote (list "--remote" remote))))
                 (error nil))))
        #'string<)))))

(defun majutsu-gerrit-upload--read-revset (prompt initial-input _history)
  "Read revset for `jj gerrit upload --revision='."
  (majutsu-read-revset prompt initial-input '("gerrit" "upload" "-r")))

(defun majutsu-gerrit-upload--read-remote-branch (prompt initial-input _history)
  "Read target remote branch for `jj gerrit upload' with completion."
  (let* ((args (transient-get-value))
         (remote (cl-loop for arg in args
                          when (string-prefix-p "--remote=" arg)
                          return (substring arg 9)))
         (branches (majutsu-gerrit--remote-branch-names remote)))
    (completing-read (format-prompt prompt nil)
                     branches nil nil initial-input
                     'majutsu-gerrit-remote-branch-history)))

(defun majutsu-gerrit-upload-arguments ()
  "Return current Gerrit upload arguments.

When called from `majutsu-gerrit-upload-transient', use the transient's
arguments.  Otherwise leave the revision unset so `jj gerrit upload' can use
its own @/@- default."
  (if (eq transient-current-command 'majutsu-gerrit-upload-transient)
      (transient-args 'majutsu-gerrit-upload-transient)
    '()))

;;;###autoload
(defun majutsu-gerrit-upload (args)
  "Upload changes to Gerrit with ARGS."
  (interactive (list (majutsu-gerrit-upload-arguments)))
  (majutsu--message-with-log "Uploading to Gerrit...")
  (majutsu-gerrit--start
   (append '("upload") args)
   (if (member "--dry-run" args)
       "Gerrit upload dry run completed"
     "Uploaded to Gerrit")))

(defun majutsu-gerrit-upload--repo-args (args)
  "Keep only stable `jj gerrit upload' ARGS for repository defaults."
  (seq-filter (lambda (arg)
                (or (transient-arg-value "--remote=" (list arg))
                    (transient-arg-value "--remote-branch=" (list arg))
                    (transient-arg-value "--notify=" (list arg))
                    (member arg '("--ignore-attention-set"))))
              args))

(defclass majutsu-gerrit-upload-option (majutsu-selection-option) ())

(transient-define-argument majutsu-gerrit-upload:--revision ()
  :description "Revision"
  :class 'majutsu-gerrit-upload-option
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-toggle-key "r"
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :shortarg "-r"
  :argument "--revision="
  :multi-value 'repeat
  :prompt "Revision"
  :reader #'majutsu-gerrit-upload--read-revset)

(transient-define-argument majutsu-gerrit-upload:--remote-branch ()
  :description "Remote branch"
  :class 'transient-option
  :shortarg "-b"
  :argument "--remote-branch="
  :prompt "Remote branch"
  :reader #'majutsu-gerrit-upload--read-remote-branch)

(transient-define-argument majutsu-gerrit-upload:--remote ()
  :description "Remote"
  :class 'transient-option
  :key "-R"
  :argument "--remote="
  :prompt "Remote"
  :reader #'majutsu-transient-read-remote-name)

(transient-define-argument majutsu-gerrit-upload:--reviewer ()
  :description "Reviewer"
  :class 'transient-option
  :key "-v"
  :argument "--reviewer="
  :multi-value 'repeat
  :prompt "Reviewer email: ")

(transient-define-argument majutsu-gerrit-upload:--cc ()
  :description "CC"
  :class 'transient-option
  :key "-C"
  :argument "--cc="
  :multi-value 'repeat
  :prompt "CC email: ")

(transient-define-argument majutsu-gerrit-upload:--label ()
  :description "Label"
  :class 'transient-option
  :shortarg "-l"
  :argument "--label="
  :multi-value 'repeat
  :prompt "Label: ")

(transient-define-argument majutsu-gerrit-upload:--topic ()
  :description "Topic"
  :class 'transient-option
  :key "-T"
  :argument "--topic="
  :prompt "Topic: ")

(transient-define-argument majutsu-gerrit-upload:--hashtag ()
  :description "Hashtag"
  :class 'transient-option
  :key "-H"
  :argument "--hashtag="
  :multi-value 'repeat
  :prompt "Hashtag: ")

(transient-define-argument majutsu-gerrit-upload:--message ()
  :description "Patch set message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message="
  :prompt "Patch set message: ")

(transient-define-argument majutsu-gerrit-upload:--notify ()
  :description "Notify"
  :class 'transient-option
  :key "-N"
  :argument "--notify="
  :choices '("none" "owner" "owner-reviewers" "all")
  :prompt "Notify: ")

(transient-define-argument majutsu-gerrit-upload:--deadline ()
  :description "Deadline"
  :class 'transient-option
  :key "-D"
  :argument "--deadline="
  :prompt "Deadline: ")

(transient-define-argument majutsu-gerrit-upload:--custom ()
  :description "Custom key=value"
  :class 'transient-option
  :key "-K"
  :argument "--custom="
  :multi-value 'repeat
  :prompt "Custom key=value: ")

(transient-define-argument majutsu-gerrit-upload:--trace ()
  :description "Trace"
  :class 'transient-option
  :key "-X"
  :argument "--trace="
  :prompt "Trace: ")

;;;###autoload(autoload 'majutsu-gerrit-upload-transient "majutsu-gerrit" nil t)
(transient-define-prefix majutsu-gerrit-upload-transient ()
  "Transient for jj gerrit upload."
  :man-page "jj-gerrit-upload"
  :class 'majutsu-repository-transient-prefix
  :repo-namespace 'majutsu-gerrit
  :repo-key 'majutsu-gerrit-upload
  :repo-filter #'majutsu-gerrit-upload--repo-args
  :incompatible '(("--wip" "--ready")
                  ("--private" "--remove-private")
                  ("--publish-comments" "--no-publish-comments"))
  :transient-non-suffix t
  [:description
   "JJ Gerrit Upload"
   ["Selection"
    (majutsu-gerrit-upload:--revision)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Target"
    (majutsu-gerrit-upload:--remote-branch)
    (majutsu-gerrit-upload:--remote)
    ("-n" "Dry run" ("-n" "--dry-run"))]
   ["Review"
    (majutsu-gerrit-upload:--reviewer)
    (majutsu-gerrit-upload:--cc)
    (majutsu-gerrit-upload:--label)
    (majutsu-gerrit-upload:--topic)
    (majutsu-gerrit-upload:--hashtag)
    (majutsu-gerrit-upload:--message)]
   ["State"
    ("-e" "Change edit" "--edit")
    ("-w" "WIP" "--wip")
    ("-a" "Ready" "--ready")
    ("-p" "Private" "--private")
    ("-P" "Remove private" "--remove-private")]
   ["Notify"
    (majutsu-gerrit-upload:--notify)
    ("-u" "Publish comments" "--publish-comments")
    ("-U" "No publish comments" "--no-publish-comments")
    ("-A" "Ignore attention" "--ignore-attention-set")]
   ["Advanced"
    ("-s" "Submit" "--submit")
    ("-S" "Skip validation" "--skip-validation")
    ("-M" "Merged" "--merged")
    (majutsu-gerrit-upload:--deadline)
    (majutsu-gerrit-upload:--custom)
    (majutsu-gerrit-upload:--trace)]
   ["Actions"
    ("u" "Upload" majutsu-gerrit-upload)
    ("RET" "Upload" majutsu-gerrit-upload)
    ("W" "Save repo defaults" majutsu-transient-save-repository-defaults
     :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-gerrit-upload-transient nil nil
   :scope (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-gerrit)
;;; majutsu-gerrit.el ends here
