;;; majutsu-duplicate.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj duplicate commands and transients,
;; managing source and destination selections.

;;; Code:

(require 'majutsu)

;;; Duplicate

(defun majutsu-duplicate-clear-selections ()
  "Clear duplicate selections and overlays."
  (interactive)
  (majutsu--entry-clear-overlays majutsu-duplicate-sources)
  (majutsu--entry-clear-overlays majutsu-duplicate-destinations)
  (majutsu--entry-clear-overlays majutsu-duplicate-after)
  (majutsu--entry-clear-overlays majutsu-duplicate-before)
  (setq majutsu-duplicate-sources nil
        majutsu-duplicate-destinations nil
        majutsu-duplicate-after nil
        majutsu-duplicate-before nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared duplicate selections")))

(defun majutsu-duplicate-toggle-source ()
  "Toggle the commit at point as a duplicate source."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "source"
   :label "[SRC]"
   :face '(:background "goldenrod" :foreground "black")
   :collection-var 'majutsu-duplicate-sources))

(defun majutsu-duplicate-toggle-destination ()
  "Toggle the commit at point as a duplicate destination."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "--destination"
   :label "[DEST]"
   :face '(:background "dark green" :foreground "white")
   :collection-var 'majutsu-duplicate-destinations))

(defun majutsu-duplicate-toggle-after ()
  "Toggle the commit at point as a duplicate --after entry."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "--after"
   :label "[AFTER]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-duplicate-after))

(defun majutsu-duplicate-toggle-before ()
  "Toggle the commit at point as a duplicate --before entry."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "--before"
   :label "[BEFORE]"
   :face '(:background "dark magenta" :foreground "white")
   :collection-var 'majutsu-duplicate-before))

(defun majutsu-duplicate--run-command (args)
  "Execute jj duplicate with ARGS and refresh log."
  (let ((result (apply #'majutsu-run-jj args)))
    (when (majutsu--handle-command-result
           args result
           "Duplicated changeset(s)"
           "Failed to duplicate changeset(s)")
      (majutsu-log-refresh)
      t)))

(defun majutsu-duplicate-execute ()
  "Execute jj duplicate using transient selections."
  (interactive)
  (when (majutsu-duplicate--run-command (majutsu-duplicate--build-args))
    (majutsu-duplicate-clear-selections)))

(defun majutsu-duplicate-cleanup-on-exit ()
  "Cleanup duplicate selections after transient exit."
  (majutsu-duplicate-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-duplicate-cleanup-on-exit t))

;;;###autoload
(defun majutsu-duplicate-transient ()
  "Open the jj duplicate transient."
  (interactive)
  (add-hook 'transient-exit-hook 'majutsu-duplicate-cleanup-on-exit nil t)
  (majutsu-duplicate-transient--internal))

;;;###autoload
(defun majutsu-duplicate (arg)
  "Duplicate the changeset at point.
With prefix ARG, open the duplicate transient."
  (interactive "P")
  (if arg
      (majutsu-duplicate-transient)
    (let* ((rev (majutsu-log--revset-at-point))
           (args (majutsu-duplicate--build-args
                  :sources (list (or rev "@")))))
      (majutsu-duplicate--run-command args))))


;;; Duplicate Transient

(defvar-local majutsu-duplicate-sources nil
  "Entry structs representing revisions to duplicate.")

(defvar-local majutsu-duplicate-destinations nil
  "Entry structs representing `--destination' parents.")

(defvar-local majutsu-duplicate-after nil
  "Entry structs representing `--after' parents.")

(defvar-local majutsu-duplicate-before nil
  "Entry structs representing `--before' parents.")

(defun majutsu-duplicate--sources ()
  "Return normalized duplicate source revsets."
  (let ((sources (majutsu--selection-normalize-revsets majutsu-duplicate-sources)))
    (if (seq-empty-p sources)
        (list (or (majutsu-log--revset-at-point) "@"))
      sources)))

(defun majutsu-duplicate--sources-display ()
  "Return human-readable description of duplicate sources."
  (if majutsu-duplicate-sources
      (string-join (mapcar #'majutsu--entry-display majutsu-duplicate-sources) ", ")
    (or (majutsu-log--revset-at-point) "@")))

(defun majutsu-duplicate--summary ()
  "Return a vector of descriptive fragments for duplicate state."
  (let (parts)
    (push (format "Sources: %s" (majutsu-duplicate--sources-display)) parts)
    (when majutsu-duplicate-destinations
      (push (format "Destinations: %d" (length majutsu-duplicate-destinations)) parts))
    (when majutsu-duplicate-after
      (push (format "--after: %d" (length majutsu-duplicate-after)) parts))
    (when majutsu-duplicate-before
      (push (format "--before: %d" (length majutsu-duplicate-before)) parts))
    (nreverse parts)))

(defun majutsu-duplicate--description ()
  "Build duplicate transient description."
  (let ((parts (majutsu-duplicate--summary)))
    (if parts
        (concat "JJ Duplicate | " (string-join parts " | "))
      "JJ Duplicate")))

(cl-defun majutsu-duplicate--build-args (&key sources destinations after before)
  "Construct jj duplicate argument list."
  (let* ((sources (majutsu--selection-normalize-revsets
                   (or sources majutsu-duplicate-sources)))
         (sources (if (seq-empty-p sources)
                      (majutsu-duplicate--sources)
                    sources))
         (destinations (majutsu--selection-normalize-revsets
                        (or destinations majutsu-duplicate-destinations)))
         (after (majutsu--selection-normalize-revsets
                 (or after majutsu-duplicate-after)))
         (before (majutsu--selection-normalize-revsets
                  (or before majutsu-duplicate-before)))
         (args '("duplicate")))
    (dolist (rev destinations)
      (setq args (append args (list "--destination" rev))))
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (setq args (append args sources))
    args))

(transient-define-prefix majutsu-duplicate-transient--internal ()
  "Internal transient for jj duplicate."
  :man-page "jj-duplicate"
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-duplicate--description
   :class transient-columns
   ["Sources"
    ("y" "Toggle source" majutsu-duplicate-toggle-source
     :description (lambda ()
                    (format "Toggle source (%d selected)"
                            (length majutsu-duplicate-sources)))
     :transient t)
    ("c" "Clear selections" majutsu-duplicate-clear-selections
     :transient t)]
   ["Placement"
    ("d" "Toggle --destination" majutsu-duplicate-toggle-destination
     :description (lambda ()
                    (format "--destination (%d selected)"
                            (length majutsu-duplicate-destinations)))
     :transient t)
    ("a" "Toggle --after" majutsu-duplicate-toggle-after
     :description (lambda ()
                    (format "--after (%d selected)"
                            (length majutsu-duplicate-after)))
     :transient t)
    ("b" "Toggle --before" majutsu-duplicate-toggle-before
     :description (lambda ()
                    (format "--before (%d selected)"
                            (length majutsu-duplicate-before)))
     :transient t)]
   ["Actions"
    ("RET" "Duplicate changes" majutsu-duplicate-execute)
    ("p" "Duplicate changes" majutsu-duplicate-execute)
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-duplicate)
;;; majutsu-duplicate.el ends here
