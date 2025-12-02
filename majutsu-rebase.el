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

;;; majutsu-rebase

;;;###autoload
(defun majutsu-rebase-clear-selections ()
  "Clear all rebase selections and overlays."
  (interactive)
  (majutsu--entry-clear-overlays majutsu-rebase-source)
  (majutsu--entry-clear-overlays majutsu-rebase-destinations)
  (setq majutsu-rebase-source nil
        majutsu-rebase-destinations nil)
  (message "Cleared all rebase selections"))

;;;###autoload
(defun majutsu-rebase-set-source ()
  "Set the commit at point as rebase source."
  (interactive)
  (majutsu--selection-select-revset
   :kind "source"
   :label "[SOURCE]"
   :face '(:background "dark green" :foreground "white")
   :collection-var 'majutsu-rebase-source))

;;;###autoload
(defun majutsu-rebase-toggle-destination ()
  "Toggle the commit at point as a rebase destination."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "destination"
   :label "[DEST]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-rebase-destinations))

(defun majutsu-rebase-cleanup-on-exit ()
  "Clean up rebase selections when transient exits."
  (majutsu-rebase-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-rebase-cleanup-on-exit t))

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
         ((string= majutsu-rebase-dest-type "-d") "-A")
         ((string= majutsu-rebase-dest-type "-A") "-B")
         (t "-d"))))

;;;###autoload
(defun majutsu-rebase-execute (args)
  "Execute rebase with selected source and destinations.
ARGS are passed from the transient."
  (interactive (list (transient-args 'majutsu-rebase-transient--internal)))
  (if (and (majutsu-rebase--source-entry) majutsu-rebase-destinations)
      (let* ((source-entry (majutsu-rebase--source-entry))
             (source-rev (majutsu--entry-revset source-entry))
             (source-display (majutsu--entry-display source-entry))
             (dest-revs (seq-filter (lambda (rev) (and rev (not (string-empty-p rev))))
                                    (majutsu-rebase--destination-revsets)))
             (dest-display (majutsu-rebase--destination-display))
             (skip-emptied? (member "--skip-emptied" args))
             (keep-divergent? (member "--keep-divergent" args)))
        (when (and source-rev dest-revs
                   (yes-or-no-p (format "Rebase %s -> %s? " source-display dest-display)))
          (let* ((dest-args (apply #'append (mapcar (lambda (dest) (list majutsu-rebase-dest-type dest)) dest-revs)))
                 (all-args (append (list "rebase" majutsu-rebase-source-type source-rev)
                                   dest-args
                                   (when skip-emptied? '("--skip-emptied"))
                                   (when keep-divergent? '("--keep-divergent"))))
                 (progress-msg (format "Rebasing %s onto %s" source-display dest-display))
                 (success-msg (format "Rebase completed: %s -> %s" source-display dest-display)))
            (majutsu--message-with-log "%s..." progress-msg)
            (let ((result (apply #'majutsu-run-jj all-args)))
              (if (majutsu--handle-command-result all-args result success-msg "Rebase failed")
                  (progn
                    (majutsu-rebase-clear-selections)
                    (majutsu-log-refresh)))))))
    (majutsu--message-with-log "Please select source (s) and at least one destination (d) first")))

;;;###autoload
(defun majutsu-rebase-transient ()
  "Transient for jj rebase operations."
  (interactive)
  ;; Add cleanup hook for when transient exits
  (add-hook 'transient-exit-hook 'majutsu-rebase-cleanup-on-exit nil t)
  (majutsu-rebase-transient--internal))

;;; Rebase Transient

(defvar-local majutsu-rebase-source nil
  "List containing at most one selected log section for the rebase source.")

(defvar-local majutsu-rebase-destinations nil
  "List of selected log sections to rebase onto.")

(defvar-local majutsu-rebase-source-type "-s"
  "Flag to use for rebase source (-s, -b, or -r).")

(defvar-local majutsu-rebase-dest-type "-d"
  "Flag to use for rebase destination (-d, -A, or -B).")

(defun majutsu-rebase--source-entry ()
  "Return the entry selected as rebase source, if any."
  (car majutsu-rebase-source))

(defun majutsu-rebase--destination-revsets ()
  "Return the list of destination revsets."
  (mapcar #'majutsu--entry-revset majutsu-rebase-destinations))

(defun majutsu-rebase--destination-display ()
  "Return a comma-separated string for destination display."
  (string-join (mapcar #'majutsu--entry-display majutsu-rebase-destinations) ", "))

(defun majutsu-rebase--source-display ()
  "Return a display string for the current source."
  (when-let* ((entry (majutsu-rebase--source-entry)))
    (majutsu--entry-display entry)))



(transient-define-prefix majutsu-rebase-transient--internal ()
  "Internal transient for jj rebase operations."
  :man-page "jj-rebase"
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Rebase"
             (when-let* ((source (majutsu-rebase--source-display)))
               (format " | Source (%s): %s" majutsu-rebase-source-type source))
             (when majutsu-rebase-destinations
               (format " | Destinations (%s): %s"
                       majutsu-rebase-dest-type
                       (majutsu-rebase--destination-display)))))
   :class transient-columns
   ["Selection"
    ("s" "Set source" majutsu-rebase-set-source
     :description (lambda ()
                    (if (majutsu-rebase--source-entry)
                        (format "Set source (current: %s)" (majutsu-rebase--source-display))
                      "Set source"))
     :transient t)
    ("S" "Toggle source type" majutsu-rebase-toggle-source-type
     :description (lambda () (format "Source type (%s)" majutsu-rebase-source-type))
     :transient t)
    ("d" "Toggle destination" majutsu-rebase-toggle-destination
     :description (lambda ()
                    (format "Toggle destination (%d selected)"
                            (length majutsu-rebase-destinations)))
     :transient t)
    ("D" "Toggle dest type" majutsu-rebase-toggle-dest-type
     :description (lambda () (format "Dest type (%s)" majutsu-rebase-dest-type))
     :transient t)
    ("c" "Clear selections" majutsu-rebase-clear-selections
     :transient t)]
   ["Options"
    ("-se" "Skip emptied" "--skip-emptied")
    ("-kd" "Keep divergent" "--keep-divergent")]
   ["Actions"
    ("r" "Execute rebase" majutsu-rebase-execute
     :description (lambda ()
                    (if (and (majutsu-rebase--source-entry) majutsu-rebase-destinations)
                        (format "Rebase %s -> %s"
                                (majutsu-rebase--source-display)
                                (majutsu-rebase--destination-display))
                      "Execute rebase (select source & destinations first)")))
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-rebase)
;;; majutsu-rebase.el ends here
