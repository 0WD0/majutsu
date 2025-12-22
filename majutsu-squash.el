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
  "Clear all squash selections."
  (interactive)
  (majutsu-selection-clear)
  (message "Cleared all squash selections"))

;;;###autoload
(defun majutsu-squash-set-from ()
  "Set the commit at point as squash `from' source."
  (interactive)
  (majutsu-selection-toggle 'from))

;;;###autoload
(defun majutsu-squash-set-into ()
  "Set the commit at point as squash `into' destination."
  (interactive)
  (majutsu-selection-select 'into))

;;;###autoload
(defun majutsu-squash-execute (&optional args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-squash-transient--internal)))
  (let* ((keep (member "--keep" args))
         (ignore-immutable (member "--ignore-immutable" args))
         (from-revsets (or (majutsu-selection-values 'from)
                           (and-let* ((rev (magit-section-value-if 'jj-commit)))
                             (list rev))))
         (into (car (majutsu-selection-values 'into))))
    (cond
     ((and from-revsets into)
      (majutsu--squash-run from-revsets into keep ignore-immutable))
     (from-revsets
      (majutsu--squash-run from-revsets nil keep ignore-immutable))
     ((magit-section-value-if 'jj-commit)
      (majutsu--squash-run (list (magit-section-value-if 'jj-commit)) nil keep ignore-immutable))
     (t
      (majutsu--message-with-log "No commit selected for squash")))))

(defun majutsu--squash-run (from-list into keep ignore-immutable)
  "Run jj squash using with-editor."
  (let* ((froms (seq-filter (lambda (rev)
                              (and rev (not (string-empty-p (string-trim rev)))))
                            (mapcar (lambda (rev) (and rev (string-trim rev)))
                                    from-list)))
         (froms (or froms '("@")))
         (args (append '("squash")
                       (apply #'append (mapcar (lambda (rev) (list "--from" rev)) froms))
                       (when into (list "--into" into))
                       (when keep '("--keep-emptied"))
                       (when ignore-immutable '("--ignore-immutable"))))
         (from-display (string-join froms ", "))
         (success-msg (if into
                          (format "Squashed %s into %s" from-display into)
                        (format "Squashed %s into parent" from-display))))
    (majutsu--with-editor-run args success-msg "Squash failed"
                              #'majutsu-selection-session-end)))

;;;###autoload
(defun majutsu-squash-transient ()
  "Transient for jj squash operations."
  (interactive)
  (majutsu-selection-session-begin
   '((:key from
      :label "[FROM]"
      :face (:background "dark orange" :foreground "white")
      :type multi)
     (:key into
      :label "[INTO]"
      :face (:background "dark cyan" :foreground "white")
      :type single)))
  (add-hook 'transient-exit-hook #'majutsu-selection-session-end nil t)
  (majutsu-squash-transient--internal))

(defun majutsu-mode-bury-squash ()
  (interactive)
  (transient-quit-one))

;;; Squash Transient

(defun majutsu-squash--from-display ()
  "Return a display string for the squash source."
  (when-let* ((values (majutsu-selection-values 'from)))
    (string-join values ", ")))

(defun majutsu-squash--into-display ()
  "Return a display string for the squash destination."
  (car (majutsu-selection-values 'into)))

(transient-define-prefix majutsu-squash-transient--internal ()
  "Internal transient for jj squash operations."
  :man-page "jj-squash"
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
                    (if (> (majutsu-selection-count 'from) 0)
                        (format "Set from (current: %s)" (majutsu-squash--from-display))
                      "Set from"))
     :transient t)
    ("t" "Set into" majutsu-squash-set-into
     :description (lambda ()
                    (if (> (majutsu-selection-count 'into) 0)
                        (format "Set into (current: %s)" (majutsu-squash--into-display))
                      "Set into"))
     :transient t)
    ("c" "Clear selections" majutsu-squash-clear-selections
     :transient t)]
   ["Options"
    ("-k" "Keep emptied commit" "--keep")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute
     :description (lambda ()
                    (cond
                     ((and (> (majutsu-selection-count 'from) 0)
                           (> (majutsu-selection-count 'into) 0))
                      (format "Squash %s into %s"
                              (majutsu-squash--from-display)
                              (majutsu-squash--into-display)))
                     ((> (majutsu-selection-count 'from) 0)
                      (format "Squash %s into parent" (majutsu-squash--from-display)))
                     (t "Execute squash (select commits first)"))))
    ("q" "Quit" transient-quit-one)
    ("b" "Bury" majutsu-mode-bury-squash)]])

;;; _
(provide 'majutsu-squash)
;;; majutsu-squash.el ends here
