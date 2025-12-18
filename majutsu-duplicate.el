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
  "Clear duplicate selections."
  (interactive)
  (majutsu-selection-clear)
  (when (called-interactively-p 'interactive)
    (message "Cleared duplicate selections")))

(defun majutsu-duplicate-toggle-source ()
  "Toggle the commit at point as a duplicate source."
  (interactive)
  (majutsu-selection-toggle 'source))

(defun majutsu-duplicate-toggle-destination ()
  "Toggle the commit at point as a duplicate destination."
  (interactive)
  (majutsu-selection-toggle 'onto))

(defun majutsu-duplicate-toggle-after ()
  "Toggle the commit at point as a duplicate --after entry."
  (interactive)
  (majutsu-selection-toggle 'after))

(defun majutsu-duplicate-toggle-before ()
  "Toggle the commit at point as a duplicate --before entry."
  (interactive)
  (majutsu-selection-toggle 'before))

(defun majutsu-duplicate--run-command (args)
  "Execute jj duplicate with ARGS and refresh log."
  (when (zerop (apply #'majutsu-call-jj args))
    (message "Duplicated changeset(s)")
    (majutsu-selection-session-end)
    t))

(defun majutsu-duplicate-execute ()
  "Execute jj duplicate using transient selections."
  (interactive)
  (majutsu-duplicate--run-command (majutsu-duplicate--build-args)))

;;;###autoload
(defun majutsu-duplicate-transient ()
  "Open the jj duplicate transient."
  (interactive)
  (majutsu-selection-session-begin
   '((:key source
      :label "[SRC]"
      :face (:background "goldenrod" :foreground "black")
      :type multi)
     (:key onto
      :label "[ONTO]"
      :face (:background "dark green" :foreground "white")
      :type multi)
     (:key after
      :label "[AFTER]"
      :face (:background "dark blue" :foreground "white")
      :type multi)
     (:key before
      :label "[BEFORE]"
      :face (:background "dark magenta" :foreground "white")
      :type multi)))
  (add-hook 'transient-exit-hook #'majutsu-selection-session-end nil t)
  (majutsu-duplicate-transient--internal))

;;;###autoload
(defun majutsu-duplicate (arg)
  "Duplicate the changeset at point.
With prefix ARG, open the duplicate transient."
  (interactive "P")
  (if arg
      (majutsu-duplicate-transient)
    (let* ((rev (magit-section-value-if 'jj-commit))
           (args (majutsu-duplicate--build-args
                  :sources (list (or rev "@")))))
      (majutsu-duplicate--run-command args))))

;;; Duplicate Transient

(defun majutsu-duplicate--sources ()
  "Return normalized duplicate source revsets."
  (or (majutsu-selection-values 'source)
      (list (or (magit-section-value-if 'jj-commit) "@"))))

(defun majutsu-duplicate--sources-display ()
  "Return human-readable description of duplicate sources."
  (if-let* ((values (majutsu-selection-values 'source)))
      (string-join values ", ")
    (or (magit-section-value-if 'jj-commit) "@")))

(defun majutsu-duplicate--summary ()
  "Return a vector of descriptive fragments for duplicate state."
  (let (parts)
    (push (format "Sources: %s" (majutsu-duplicate--sources-display)) parts)
    (when (> (majutsu-selection-count 'onto) 0)
      (push (format "Destinations: %d" (majutsu-selection-count 'onto)) parts))
    (when (> (majutsu-selection-count 'after) 0)
      (push (format "--after: %d" (majutsu-selection-count 'after)) parts))
    (when (> (majutsu-selection-count 'before) 0)
      (push (format "--before: %d" (majutsu-selection-count 'before)) parts))
    (nreverse parts)))

(defun majutsu-duplicate--description ()
  "Build duplicate transient description."
  (let ((parts (majutsu-duplicate--summary)))
    (if parts
        (concat "JJ Duplicate | " (string-join parts " | "))
      "JJ Duplicate")))

(cl-defun majutsu-duplicate--build-args (&key sources destinations after before)
  "Construct jj duplicate argument list."
  (let* ((sources (or sources (majutsu-duplicate--sources)))
         (destinations (or destinations (majutsu-selection-values 'onto)))
         (after (or after (majutsu-selection-values 'after)))
         (before (or before (majutsu-selection-values 'before)))
         (args '("duplicate")))
    (dolist (rev destinations)
      (setq args (append args (list "--onto" rev))))
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
                            (majutsu-selection-count 'source)))
     :transient t)
    ("c" "Clear selections" majutsu-duplicate-clear-selections
     :transient t)]
   ["Placement"
    ("o" "Toggle --onto" majutsu-duplicate-toggle-destination
     :description (lambda ()
                    (format "--onto (%d selected)"
                            (majutsu-selection-count 'onto)))
     :transient t)
    ("a" "Toggle --after" majutsu-duplicate-toggle-after
     :description (lambda ()
                    (format "--after (%d selected)"
                            (majutsu-selection-count 'after)))
     :transient t)
    ("b" "Toggle --before" majutsu-duplicate-toggle-before
     :description (lambda ()
                    (format "--before (%d selected)"
                            (majutsu-selection-count 'before)))
     :transient t)]
   ["Actions"
    ("RET" "Duplicate changes" majutsu-duplicate-execute)
    ("p" "Duplicate changes" majutsu-duplicate-execute)
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-duplicate)
;;; majutsu-duplicate.el ends here
