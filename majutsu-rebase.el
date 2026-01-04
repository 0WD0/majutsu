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
  "Clear all rebase selections."
  (interactive)
  (majutsu-selection-clear)
  (message "Cleared all rebase selections"))

;;;###autoload
(defun majutsu-rebase-set-source ()
  "Set the commit at point as rebase source."
  (interactive)
  (majutsu-selection-toggle 'source))

;;;###autoload
(defun majutsu-rebase-toggle-destination ()
  "Toggle the commit at point as a rebase destination."
  (interactive)
  (majutsu-selection-toggle 'destination))

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
  (if (and (> (majutsu-selection-count 'source) 0)
           (> (majutsu-selection-count 'destination) 0))
      (let* ((source-revs (majutsu-selection-values 'source))
             (source-display (majutsu-rebase--source-display))
             (dest-revs (majutsu-selection-values 'destination))
             (dest-display (majutsu-rebase--destination-display))
             (skip-emptied? (member "--skip-emptied" args))
             (keep-divergent? (member "--keep-divergent" args))
             (ignore-immutable? (member "--ignore-immutable" args)))
        (when (and source-revs dest-revs
                   (yes-or-no-p (format "Rebase %s -> %s? " source-display dest-display)))
          (let* ((dest-args
                  (apply #'append
                         (mapcar (lambda (dest)
                                   (list majutsu-rebase-dest-type dest))
                                 dest-revs)))
                 (source-args
                  (apply #'append
                         (mapcar (lambda (source)
                                   (list majutsu-rebase-source-type source))
                                 source-revs)))
                 (all-args (append '("rebase") source-args dest-args
                                   (when skip-emptied? '("--skip-emptied"))
                                   (when keep-divergent? '("--keep-divergent"))
                                   (when ignore-immutable? '("--ignore-immutable"))))
                 (progress-msg (format "Rebasing %s onto %s" source-display dest-display))
                 (success-msg (format "Rebase completed: %s -> %s" source-display dest-display)))
            (majutsu--message-with-log "%s..." progress-msg)
            (majutsu--debug "Running jj rebase with args: %s" (string-join all-args " "))
            (when (zerop (apply #'majutsu-run-jj all-args))
              (message "%s" success-msg)
              (majutsu-selection-session-end)))))
    (majutsu--message-with-log "Please select source (s) and at least one destination (d) first")))

;;; Rebase Transient

(defvar-local majutsu-rebase-source-type "-s"
  "Flag to use for rebase source (-s, -b, or -r).")

(defvar-local majutsu-rebase-dest-type "-o"
  "Flag to use for rebase destination (-o, -A, or -B).")

(defun majutsu-rebase--destination-display ()
  "Return a comma-separated string for destination display."
  (when-let* ((values (majutsu-selection-values 'destination)))
    (string-join values ", ")))

(defun majutsu-rebase--source-display ()
  "Return a comma-separated string for source display."
  (when-let* ((values (majutsu-selection-values 'source)))
    (string-join values ", ")))

(transient-define-prefix majutsu-rebase ()
  "Internal transient for jj rebase operations."
  :man-page "jj-rebase"
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Rebase"
             (when-let* ((source (majutsu-rebase--source-display)))
               (format " | Source (%s): %s" majutsu-rebase-source-type source))
             (when (> (majutsu-selection-count 'destination) 0)
               (format " | Destinations (%s): %s"
                       majutsu-rebase-dest-type
                       (majutsu-rebase--destination-display)))))
   :class transient-columns
   ["Selection"
    ("s" "Set source" majutsu-rebase-set-source
     :description (lambda ()
                    (if (> (majutsu-selection-count 'source) 0)
                        (format "Set source (current: %s)" (majutsu-rebase--source-display))
                      "Set source"))
     :transient t)
    ("S" "Toggle source type" majutsu-rebase-toggle-source-type
     :description (lambda () (format "Source type (%s)" majutsu-rebase-source-type))
     :transient t)
    ("d" "Toggle destination" majutsu-rebase-toggle-destination
     :description (lambda ()
                    (format "Toggle destination (%d selected)"
                            (majutsu-selection-count 'destination)))
     :transient t)
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
    ("r" "Execute rebase" majutsu-rebase-execute
     :description (lambda ()
                    (if (and (> (majutsu-selection-count 'source) 0)
                             (> (majutsu-selection-count 'destination) 0))
                        (format "Rebase %s -> %s"
                                (majutsu-rebase--source-display)
                                (majutsu-rebase--destination-display))
                      "Execute rebase (select source & destinations first)")))
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (majutsu-selection-session-begin
   '((:key source
      :label "[SOURCE]"
      :face (:background "dark green" :foreground "white")
      :type multi)
     (:key destination
      :label "[DEST]"
      :face (:background "dark blue" :foreground "white")
      :type multi)))
  (add-hook 'transient-exit-hook #'majutsu-selection-session-end nil t)
  (transient-setup 'majutsu-rebase))

;;; _
(provide 'majutsu-rebase)
;;; majutsu-rebase.el ends here
