;;; majutsu-rebase.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides jj rebase transients and execution, managing
;; source and destination selections and flags.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-rebase-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-rebase--toggle-option (majutsu-selection-toggle-option)
  ())

;;; majutsu-rebase

;;;###autoload
(defun majutsu-rebase-toggle-source-type ()
  "Toggle rebase source type between -s, -b, and -r."
  (interactive)
  (setq majutsu-rebase-source-type
        (cond
         ((string= majutsu-rebase-source-type "-s") "-b")
         ((string= majutsu-rebase-source-type "-b") "-r")
         (t "-s"))))

;;;###autoload
(defun majutsu-rebase-toggle-dest-type ()
  "Toggle rebase destination type between -o, -A, and -B."
  (interactive)
  (setq majutsu-rebase-dest-type
        (cond
         ((string= majutsu-rebase-dest-type "-o") "-A")
         ((string= majutsu-rebase-dest-type "-A") "-B")
         (t "-o"))))

;;;###autoload
(defun majutsu-rebase-execute (args)
  "Execute rebase with selected source and destinations.
ARGS are passed from the transient."
  (interactive (list (transient-args 'majutsu-rebase)))
  (let ((sources (mapcar (lambda (s) (substring s 9))
                         (seq-filter (lambda (s) (string-prefix-p "--source=" s)) args)))
        (dests (mapcar (lambda (s) (substring s 14))
                       (seq-filter (lambda (s) (string-prefix-p "--destination=" s)) args)))
        (skip-emptied? (member "--skip-emptied" args))
        (keep-divergent? (member "--keep-divergent" args))
        (ignore-immutable? (member "--ignore-immutable" args)))
    (if (and sources dests)
        (when (yes-or-no-p (format "Rebase %s -> %s? "
                                   (string-join sources ", ")
                                   (string-join dests ", ")))
          (let* ((dest-args
                  (apply #'append
                         (mapcar (lambda (dest)
                                   (list majutsu-rebase-dest-type dest))
                                 dests)))
                 (source-args
                  (apply #'append
                         (mapcar (lambda (source)
                                   (list majutsu-rebase-source-type source))
                                 sources)))
                 (all-args (append '("rebase") source-args dest-args
                                   (when skip-emptied? '("--skip-emptied"))
                                   (when keep-divergent? '("--keep-divergent"))
                                   (when ignore-immutable? '("--ignore-immutable"))))
                 (success-msg (format "Rebase completed")))
            (majutsu--message-with-log "Rebasing...")
            (majutsu--debug "Running jj rebase with args: %s" (string-join all-args " "))
            (when (zerop (apply #'majutsu-run-jj all-args))
              (message "%s" success-msg))))
      (majutsu--message-with-log "Please select source (s) and at least one destination (d) first"))))

;;; Rebase Transient

(defvar-local majutsu-rebase-source-type "-s"
  "Flag to use for rebase source (-s, -b, or -r).")

(defvar-local majutsu-rebase-dest-type "-o"
  "Flag to use for rebase destination (-o, -A, or -B).")

(transient-define-argument majutsu-rebase:--source ()
  :description "Source"
  :class 'majutsu-rebase-option
  :selection-key 'source
  :selection-label "[SOURCE]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-type 'multi
  :key "-S"
  :argument "--source="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-rebase:--destination ()
  :description "Destination"
  :class 'majutsu-rebase-option
  :selection-key 'destination
  :selection-label "[DEST]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-type 'multi
  :key "-D"
  :argument "--destination="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-rebase:source ()
  :description "Source (toggle at point)"
  :class 'majutsu-rebase--toggle-option
  :selection-key 'source
  :selection-type 'multi
  :key "s"
  :argument "--source="
  :multi-value 'repeat)

(transient-define-argument majutsu-rebase:destination ()
  :description "Destination (toggle at point)"
  :class 'majutsu-rebase--toggle-option
  :selection-key 'destination
  :selection-type 'multi
  :key "d"
  :argument "--destination="
  :multi-value 'repeat)

(defun majutsu-rebase-clear-selections ()
  "Clear all rebase selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-rebase-option)
                 (memq (oref obj selection-key) '(source destination)))
        (transient-infix-set obj nil))))
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all rebase selections"))

(transient-define-prefix majutsu-rebase ()
  "Internal transient for jj rebase operations."
  :man-page "jj-rebase"
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Rebase"
             (format " | Source Type: %s" majutsu-rebase-source-type)
             (format " | Dest Type: %s" majutsu-rebase-dest-type)))
   :class transient-columns
   ["Selection"
    (majutsu-rebase:--source)
    (majutsu-rebase:--destination)
    (majutsu-rebase:source)
    ("S" "Toggle source type" majutsu-rebase-toggle-source-type
     :description (lambda () (format "Source type (%s)" majutsu-rebase-source-type))
     :transient t)
    (majutsu-rebase:destination)
    ("D" "Toggle dest type" majutsu-rebase-toggle-dest-type
     :description (lambda () (format "Dest type (%s)" majutsu-rebase-dest-type))
     :transient t)
    ("c" "Clear selections" majutsu-rebase-clear-selections
     :transient t)]
   ["Options"
    ("-se" "Skip emptied" "--skip-emptied")
    ("-kd" "Keep divergent" "--keep-divergent")
   (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("r" "Execute rebase" majutsu-rebase-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-rebase nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-rebase)
;;; majutsu-rebase.el ends here
