;;; majutsu-gerrit-upload.el --- jj gerrit upload transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides the `jj gerrit upload' transient and its
;; option readers.  Shared native Gerrit discovery and completion
;; helpers live in `majutsu-gerrit.el'.

;;; Code:

(require 'majutsu)
(require 'majutsu-completion)
(require 'majutsu-config)
(require 'majutsu-gerrit)
(require 'majutsu-selection)

(require 'consult)

(defvar majutsu-gerrit-remote-branch-history nil
  "Minibuffer history for Gerrit remote branch names.")

(defvar majutsu-gerrit-reviewer-history nil
  "Minibuffer history for Gerrit reviewer values.")

(defvar majutsu-gerrit-cc-history nil
  "Minibuffer history for Gerrit CC values.")

(defvar majutsu-gerrit-topic-history nil
  "Minibuffer history for Gerrit topics.")

(defvar majutsu-gerrit-hashtag-history nil
  "Minibuffer history for Gerrit hashtags.")

(defvar majutsu-gerrit-label-history nil
  "Minibuffer history for Gerrit labels.")

(declare-function majutsu-completing-read-multiple "majutsu-completion")
(declare-function majutsu-completing-read-payload "majutsu-completion")
(declare-function majutsu-gerrit-read-accounts "majutsu-gerrit")
(declare-function majutsu-gerrit-remote-branch-candidate-data "majutsu-gerrit")
(declare-function majutsu-gerrit-topic-candidate-data "majutsu-gerrit")
(declare-function majutsu-read-revset "majutsu-completion")
(declare-function majutsu-read-string "majutsu-completion")
(declare-function majutsu-selection-clear "majutsu-selection")
(declare-function majutsu-selection-find-section "majutsu-selection")
(declare-function majutsu-transient-read-remote-name "majutsu-remote")
(declare-function majutsu-transient-save-repository-defaults "majutsu-core")

(defun majutsu-gerrit-upload--read-revset (prompt initial-input _history)
  "Read revset for `jj gerrit upload --revision='."
  (majutsu-read-revset prompt initial-input '("gerrit" "upload" "-r")))

(defun majutsu-gerrit-upload--read-remote-branch (prompt initial-input _history)
  "Read target remote branch for `jj gerrit upload' with completion."
  (majutsu-with-toplevel
    (let ((remote (majutsu-gerrit-upload--transient-remote)))
      (majutsu-completing-read-payload
       prompt (majutsu-gerrit-remote-branch-candidate-data remote)
       nil nil initial-input 'majutsu-gerrit-remote-branch-history
       nil 'majutsu-gerrit-remote-branch))))

(defun majutsu-gerrit-upload--transient-remote ()
  "Return the Gerrit upload transient's selected remote, if any."
  (when-let* ((args (ignore-errors (transient-get-value))))
    (transient-arg-value "--remote=" args)))

(defun majutsu-gerrit-upload--read-reviewer (prompt initial-input history)
  "Read one or more Gerrit reviewers."
  (majutsu-with-toplevel
    (majutsu-gerrit-read-accounts
     prompt initial-input (or history 'majutsu-gerrit-reviewer-history)
     (majutsu-gerrit-upload--transient-remote))))

(defun majutsu-gerrit-upload--read-cc (prompt initial-input history)
  "Read one or more Gerrit CC values."
  (majutsu-with-toplevel
    (majutsu-gerrit-read-accounts
     prompt initial-input (or history 'majutsu-gerrit-cc-history)
     (majutsu-gerrit-upload--transient-remote))))

(defun majutsu-gerrit-upload--read-topic (prompt initial-input history)
  "Read a Gerrit topic."
  (majutsu-with-toplevel
    (let* ((remote (majutsu-gerrit-upload--transient-remote))
           (payload (majutsu-gerrit-topic-candidate-data remote)))
      (if payload
          (majutsu-completing-read-payload
           prompt payload nil nil initial-input
           (or history 'majutsu-gerrit-topic-history)
           nil 'majutsu-gerrit-topic nil default-directory)
        (majutsu-read-string prompt initial-input
                             (or history 'majutsu-gerrit-topic-history))))))

(defun majutsu-gerrit-upload--read-hashtag (prompt initial-input history)
  "Read one or more Gerrit hashtags."
  (majutsu-completing-read-multiple
   prompt '() nil nil initial-input
   (or history 'majutsu-gerrit-hashtag-history)
   nil 'majutsu-gerrit-hashtag))

(defun majutsu-gerrit-upload--read-label (prompt initial-input history)
  "Read one or more Gerrit labels."
  (majutsu-completing-read-multiple
   prompt '() nil nil initial-input
   (or history 'majutsu-gerrit-label-history)
   nil 'majutsu-gerrit-label))

(defun majutsu-gerrit-upload-arguments ()
  "Return current Gerrit upload arguments.

When called from `majutsu-gerrit-upload-transient', use the transient's
arguments.  Otherwise leave the revision unset so `jj gerrit upload' can use
its own @/@- default."
  (if (eq transient-current-command 'majutsu-gerrit-upload-transient)
      (transient-args 'majutsu-gerrit-upload-transient)
    '()))

;;;###autoload(autoload 'majutsu-gerrit-upload "majutsu-gerrit-upload" nil t)
(transient-define-suffix majutsu-gerrit-upload (args)
  "Upload changes to Gerrit with ARGS."
  :class 'majutsu-transient-default-action-suffix
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
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :selection-toggle-key "r"
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
  :prompt "Reviewer"
  :reader #'majutsu-gerrit-upload--read-reviewer)

(transient-define-argument majutsu-gerrit-upload:--cc ()
  :description "CC"
  :class 'transient-option
  :key "-C"
  :argument "--cc="
  :multi-value 'repeat
  :prompt "CC"
  :reader #'majutsu-gerrit-upload--read-cc)

(transient-define-argument majutsu-gerrit-upload:--label ()
  :description "Label"
  :class 'transient-option
  :shortarg "-l"
  :argument "--label="
  :multi-value 'repeat
  :prompt "Label"
  :reader #'majutsu-gerrit-upload--read-label)

(transient-define-argument majutsu-gerrit-upload:--topic ()
  :description "Topic"
  :class 'transient-option
  :key "-T"
  :argument "--topic="
  :prompt "Topic"
  :reader #'majutsu-gerrit-upload--read-topic)

(transient-define-argument majutsu-gerrit-upload:--hashtag ()
  :description "Hashtag"
  :class 'transient-option
  :key "-H"
  :argument "--hashtag="
  :multi-value 'repeat
  :prompt "Hashtag"
  :reader #'majutsu-gerrit-upload--read-hashtag)

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

;;;###autoload(autoload 'majutsu-gerrit-upload-transient "majutsu-gerrit-upload" nil t)
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
    ("W" "Save repo defaults" majutsu-transient-save-repository-defaults)]]
  (interactive)
  (transient-setup
   'majutsu-gerrit-upload-transient nil nil
   :scope (majutsu-selection-session-begin)))

(provide 'majutsu-gerrit-upload)

;;; majutsu-gerrit-upload.el ends here
