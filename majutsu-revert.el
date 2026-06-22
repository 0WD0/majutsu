;;; majutsu-revert.el --- Revert transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj revert transients, managing source
;; revision selection and destination placement.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-revert-option (majutsu-selection-option)
  ())

(defclass majutsu-revert--toggle-option (majutsu-selection-toggle-option)
  ())

(defun majutsu-revert-arguments ()
  "Return the current revert arguments.
If inside the transient, return transient args unchanged.
Otherwise, if point is on a revision and args are missing source or
destination, fill with --revision and --insert-after defaults."
  (let* ((inside-transient (eq transient-current-command 'majutsu-revert))
         (args (if inside-transient
                   (transient-args 'majutsu-revert)
                 '())))
    (unless inside-transient
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (unless (transient-arg-value "--revision=" args)
          (setq args (append args (list (concat "--revision=" rev)))))
        (unless (or (transient-arg-value "--onto=" args)
                    (transient-arg-value "--insert-after=" args)
                    (transient-arg-value "--insert-before=" args))
          (setq args (append args (list (concat "--insert-after=" rev)))))))
    args))

;;;###autoload(autoload 'majutsu-revert-execute "majutsu-revert" nil t)
(transient-define-suffix majutsu-revert-execute (args)
  "Execute jj revert with ARGS from the transient."
  :description "Revert"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (majutsu-revert-arguments)))
  (let ((has-revision (transient-arg-value "--revision=" args))
        (has-destination (or (transient-arg-value "--onto=" args)
                             (transient-arg-value "--insert-after=" args)
                             (transient-arg-value "--insert-before=" args))))
    (cond
     ((not has-revision)
      (majutsu--message-with-log "Please select source revisions (-r) first"))
     ((not has-destination)
      (majutsu--message-with-log "Please select destination (-o/-A/-B) first"))
     ((zerop (apply #'majutsu-run-jj "revert" args))
      (message "Revert completed")))))

;;; Revert transient

(transient-define-argument majutsu-revert:--revision ()
  :description "Revisions"
  :class 'majutsu-revert-option
  :selection-label "[REVS]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :shortarg "-r"
  :argument "--revision="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-revert:--onto ()
  :description "Onto"
  :class 'majutsu-revert-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :shortarg "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-revert:--insert-after ()
  :description "Insert after"
  :class 'majutsu-revert-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :shortarg "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-revert:--insert-before ()
  :description "Insert before"
  :class 'majutsu-revert-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :shortarg "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-revert:revision ()
  :description "Revisions (toggle at point)"
  :class 'majutsu-revert--toggle-option
  :key "r"
  :argument "--revision="
  :multi-value 'repeat)

(transient-define-argument majutsu-revert:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-revert--toggle-option
  :key "o"
  :argument "--onto="
  :multi-value 'repeat)

(transient-define-argument majutsu-revert:insert-after ()
  :description "Insert after (toggle at point)"
  :class 'majutsu-revert--toggle-option
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-revert:insert-before ()
  :description "Insert before (toggle at point)"
  :class 'majutsu-revert--toggle-option
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

;;;###autoload(autoload 'majutsu-revert "majutsu-revert" nil t)
(transient-define-prefix majutsu-revert ()
  "Transient for jj revert operations."
  :man-page "jj-revert"
  :incompatible '(("--onto=" "--insert-after=")
                  ("--onto=" "--insert-before=")
                  ("--insert-after=" "--insert-before="))
  :transient-non-suffix t
  :description "JJ Revert"
  [["Selection"
    (majutsu-revert:--revision)
    (majutsu-revert:revision)
    ("c" "Clear selections" majutsu-selection-clear
     :transient t)]
   ["Destination"
    (majutsu-revert:--onto)
    (majutsu-revert:--insert-after)
    (majutsu-revert:--insert-before)
    (majutsu-revert:onto)
    (majutsu-revert:insert-after)
    (majutsu-revert:insert-before)]
   ["Options"
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    (majutsu-revert-execute :key "_")
    (majutsu-revert-execute :key "V")
    (majutsu-revert-execute)]]
  (interactive)
  (transient-setup
   'majutsu-revert nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-revert)
;;; majutsu-revert.el ends here
