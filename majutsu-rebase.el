;;; majutsu-rebase.el --- Rebase transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj rebase transients and execution, managing
;; source and destination selections and flags.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-rebase-option (majutsu-selection-option)
  ())

;;; majutsu-rebase

;;;###autoload(autoload 'majutsu-rebase-execute "majutsu-rebase" nil t)
(transient-define-suffix majutsu-rebase-execute (args)
  "Execute rebase with selected source and destinations.
ARGS are passed from the transient."
  :description "Execute rebase"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-rebase)))
  (let ((has-dest (or (transient-arg-value "--onto=" args)
                      (transient-arg-value "--insert-after=" args)
                      (transient-arg-value "--insert-before=" args))))
    (if has-dest
        (when (majutsu-confirm 'rebase "Rebase with current selections? ")
          (let ((all-args (cons "rebase" args)))
            (majutsu--message-with-log "Rebasing...")
            (majutsu--debug "Running jj rebase with args: %s" (string-join all-args " "))
            (when (zerop (apply #'majutsu-run-jj all-args))
              (message "Rebase completed"))))
      (majutsu--message-with-log "Please select destination (-o/-A/-B) first"))))

;;; Rebase Transient

(transient-define-argument majutsu-rebase:--source ()
  :description "Source"
  :class 'majutsu-rebase-option
  :selection-label "[SRC]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-toggle-key "s"
  :shortarg "-s"
  :argument "--source="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-rebase:--branch ()
  :description "Branch"
  :class 'majutsu-rebase-option
  :selection-label "[BRANCH]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-toggle-key "b"
  :shortarg "-b"
  :argument "--branch="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-rebase:--revision ()
  :description "Revisions"
  :class 'majutsu-rebase-option
  :selection-label "[REVS]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-toggle-key "r"
  :shortarg "-r"
  :argument "--revision="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-rebase:--onto ()
  :description "Onto"
  :class 'majutsu-rebase-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-toggle-key "o"
  :shortarg "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-rebase:--after ()
  :description "After"
  :class 'majutsu-rebase-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-toggle-key "a"
  :shortarg "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-rebase:--before ()
  :description "Before"
  :class 'majutsu-rebase-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-toggle-key "B"
  :shortarg "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

;;;###autoload(autoload 'majutsu-rebase "majutsu-rebase" nil t)
(transient-define-prefix majutsu-rebase ()
  "Internal transient for jj rebase operations."
  :man-page "jj-rebase"
  :incompatible '(("--source=" "--branch=")
                  ("--source=" "--revision=")
                  ("--branch=" "--revision=")
                  ("--onto=" "--insert-after=")
                  ("--onto=" "--insert-before="))
  :transient-non-suffix t
  :description "JJ Rebase"
  [["Source"
    (majutsu-rebase:--source)
    (majutsu-rebase:--branch)
    (majutsu-rebase:--revision)]
   ["Destination"
    (majutsu-rebase:--onto)
    (majutsu-rebase:--after)
    (majutsu-rebase:--before)
    ("c" "Clear selections" majutsu-selection-clear
     :transient t)]
   ["Options"
    ("-ke" "Skip emptied" "--skip-emptied")
    ("-kd" "Keep divergent" "--keep-divergent")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    (majutsu-rebase-execute)]]
  (interactive)
  (transient-setup
   'majutsu-rebase nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-rebase)
;;; majutsu-rebase.el ends here
