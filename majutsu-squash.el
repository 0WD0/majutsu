;;; majutsu-squash.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides jj squash transients, managing from and into
;; selections and assembling flags.

;;; Code:

(require 'majutsu)

;;; majutsu-squash

;;;###autoload
(defun majutsu-squash-clear-selections ()
  "Clear all squash selections and overlays."
  (interactive)
  (majutsu--entry-clear-overlays majutsu-squash-from)
  (majutsu--entry-clear-overlays majutsu-squash-into)
  (setq majutsu-squash-from nil
        majutsu-squash-into nil)
  (message "Cleared all squash selections"))

;;;###autoload
(defun majutsu-squash-set-from ()
  "Set the commit at point as squash `from' source."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "from"
   :label "[FROM]"
   :face '(:background "dark orange" :foreground "white")
   :collection-var 'majutsu-squash-from))

;;;###autoload
(defun majutsu-squash-set-into ()
  "Set the commit at point as squash `into' destination."
  (interactive)
  (majutsu--selection-select-revset
   :kind "into"
   :label "[INTO]"
   :face '(:background "dark cyan" :foreground "white")
   :collection-var 'majutsu-squash-into))

;;;###autoload
(defun majutsu-squash-execute (&optional args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-squash-transient--internal)))
  (let* ((keep (member "--keep" args))
         (from-entries majutsu-squash-from)
         (into-entry (majutsu-squash--into-entry))
         (from-revsets (majutsu--selection-normalize-revsets from-entries))
         (into (when into-entry (majutsu--entry-revset into-entry))))
    (cond
     ((and from-revsets into)
      (majutsu--squash-run from-revsets into keep))
     (from-revsets
      (majutsu--squash-run from-revsets nil keep))
     ((majutsu-log--revset-at-point)
      (majutsu--squash-run (list (majutsu-log--revset-at-point)) nil keep))
     (t
      (majutsu--message-with-log "No commit selected for squash")))))

(defun majutsu--squash-run (from-list into keep)
  "Run jj squash using with-editor."
  (let* ((normalized (majutsu--selection-normalize-revsets from-list))
         (froms (or normalized '("@")))
         (args (append '("squash")
                       (apply #'append (mapcar (lambda (rev) (list "--from" rev)) froms))
                       (when into (list "--into" into))
                       (when keep '("--keep-emptied"))))
         (from-display (string-join froms ", "))
         (success-msg (if into
                          (format "Squashed %s into %s" from-display into)
                        (format "Squashed %s into parent" from-display))))
    (majutsu--with-editor-run args success-msg "Squash failed"
                              #'majutsu-squash-clear-selections)))

(defun majutsu-squash-cleanup-on-exit ()
  "Clean up squash selections when transient exits."
  (unless (eq this-command 'majutsu-mode-bury-squash)
    (majutsu-squash-clear-selections)
    (remove-hook 'transient-exit-hook 'majutsu-squash-cleanup-on-exit t)))

;;;###autoload
(defun majutsu-squash-transient ()
  "Transient for jj squash operations."
  (interactive)
  ;; Add cleanup hook for when transient exits
  (add-hook 'transient-exit-hook 'majutsu-squash-cleanup-on-exit nil t)
  (majutsu-squash-transient--internal))

(defun majutsu-mode-bury-squash ()
  (interactive)
  (transient-quit-one))

;;; Squash Transient

(defvar-local majutsu-squash-from nil
  "List of selected log sections used as squash sources (for --from).")

(defvar-local majutsu-squash-into nil
  "List containing at most one selected log section for squash destination.")

(defun majutsu-squash--into-entry ()
  "Return the entry selected as squash destination, if any."
  (car majutsu-squash-into))

(defun majutsu-squash--from-display ()
  "Return a display string for the squash source."
  (when majutsu-squash-from
    (string-join (mapcar #'majutsu--entry-display majutsu-squash-from) ", ")))

(defun majutsu-squash--into-display ()
  "Return a display string for the squash destination."
  (when-let* ((entry (majutsu-squash--into-entry)))
    (majutsu--entry-display entry)))

(transient-define-prefix majutsu-squash-transient--internal ()
  "Internal transient for jj squash operations."
  :man-page "jj-squash"
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Squash"
             (when-let* ((from (majutsu-squash--from-display)))
               (format " | From: %s" from))
             (when-let* ((into (majutsu-squash--into-display)))
               (format " | Into: %s" into))))
   ["Selection"
    ("f" "Set from" majutsu-squash-set-from
     :description (lambda ()
                    (if majutsu-squash-from
                        (format "Set from (current: %s)" (majutsu-squash--from-display))
                      "Set from"))
     :transient t)
    ("t" "Set into" majutsu-squash-set-into
     :description (lambda ()
                    (if (majutsu-squash--into-entry)
                        (format "Set into (current: %s)" (majutsu-squash--into-display))
                      "Set into"))
     :transient t)
    ("c" "Clear selections" majutsu-squash-clear-selections
     :transient t)]
   ["Options"
    ("-k" "Keep emptied commit" "--keep")]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute
     :description (lambda ()
                    (cond
                     ((and majutsu-squash-from (majutsu-squash--into-entry))
                      (format "Squash %s into %s"
                              (majutsu-squash--from-display)
                              (majutsu-squash--into-display)))
                     (majutsu-squash-from
                      (format "Squash %s into parent" (majutsu-squash--from-display)))
                     (t "Execute squash (select commits first)")))
     :transient nil)
    ("q" "Quit" transient-quit-one)
    ("b" "Bury" majutsu-mode-bury-squash)]])

;;; _
(provide 'majutsu-squash)
;;; majutsu-squash.el ends here
