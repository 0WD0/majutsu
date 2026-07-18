;;; majutsu-metaedit.el --- Metaedit transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides a transient frontend for `jj metaedit'.

;;; Code:

(require 'majutsu)

(defclass majutsu-metaedit-option (majutsu-selection-option)
  ())

(defun majutsu-metaedit--selection-targets ()
  "Return revisions selected by the region or point."
  (or (magit-region-values 'jj-commit t)
      (when-let* ((revision (magit-section-value-if 'jj-commit)))
        (list revision))))

(defun majutsu-metaedit--default-revisions ()
  "Return default revisions for `jj metaedit'."
  (or (majutsu-metaedit--selection-targets)
      '("@")))

(defun majutsu-metaedit--default-args ()
  "Return default arguments for `jj metaedit'."
  (mapcar (lambda (revision) (concat "-r=" revision))
          (majutsu-metaedit--default-revisions)))

(defun majutsu-metaedit-arguments ()
  "Return the current metaedit arguments.
If inside the transient, return transient args, ensuring revision
arguments are present.  Outside the transient, return defaults."
  (let* ((inside-transient (eq transient-current-command 'majutsu-metaedit-transient))
         (args (if inside-transient
                   (transient-args 'majutsu-metaedit-transient)
                 '()))
         (has-revision (transient-arg-value "-r=" args)))
    (if has-revision
        args
      (append args (majutsu-metaedit--default-args)))))

;;;###autoload(autoload 'majutsu-metaedit-execute "majutsu-metaedit" nil t)
(transient-define-suffix majutsu-metaedit-execute (args)
  "Execute jj metaedit with ARGS from the transient."
  :description "Metaedit"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (majutsu-metaedit-arguments)))
  (let ((exit (apply #'majutsu-run-jj "metaedit" args)))
    (when (zerop exit)
      (message "Metaedit completed"))))

(transient-define-argument majutsu-metaedit:-r ()
  :description "Revisions"
  :class 'majutsu-metaedit-option
  :selection-label "[REVS]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :targets-fn #'majutsu-metaedit--selection-targets
  :selection-toggle-key "r"
  :shortarg "-r"
  :argument "-r="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-metaedit:--message ()
  :description "Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(transient-define-argument majutsu-metaedit:--author ()
  :description "Author"
  :class 'transient-option
  :key "-a"
  :argument "--author=")

(transient-define-argument majutsu-metaedit:--author-timestamp ()
  :description "Author timestamp"
  :class 'transient-option
  :key "-t"
  :argument "--author-timestamp=")

(transient-define-argument majutsu-metaedit:--update-change-id ()
  :description "Update change-id"
  :class 'transient-switch
  :key "-c"
  :argument "--update-change-id")

(transient-define-argument majutsu-metaedit:--update-author ()
  :description "Update author"
  :class 'transient-switch
  :key "-u"
  :argument "--update-author")

(transient-define-argument majutsu-metaedit:--update-author-timestamp ()
  :description "Update author timestamp"
  :class 'transient-switch
  :key "-U"
  :argument "--update-author-timestamp")

(transient-define-argument majutsu-metaedit:--force-rewrite ()
  :description "Force rewrite"
  :class 'transient-switch
  :key "-f"
  :argument "--force-rewrite")

;;;###autoload
(defun majutsu-metaedit (&optional arg)
  "Open the metaedit transient.
With prefix ARG, pre-enable --ignore-immutable."
  (interactive "P")
  (transient-setup
   'majutsu-metaedit-transient nil nil
   :scope (majutsu-selection-session-begin)
   :value (append (majutsu-metaedit-arguments)
                  (when arg '("--ignore-immutable")))))

(transient-define-prefix majutsu-metaedit-transient ()
  "Transient for jj metaedit operations."
  :man-page "jj-metaedit"
  :class 'majutsu-jj-transient-prefix
  :jj-command "metaedit"
  :incompatible '(("--update-author" "--author=")
                  ("--update-author-timestamp" "--author-timestamp="))
  :transient-non-suffix t
  :description "JJ Metaedit"
  [["Selection"
    (majutsu-metaedit:-r)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Metadata"
    (majutsu-metaedit:--message)
    (majutsu-metaedit:--author)
    (majutsu-metaedit:--author-timestamp)]
   ["Options"
    (majutsu-metaedit:--update-change-id)
    (majutsu-metaedit:--update-author)
    (majutsu-metaedit:--update-author-timestamp)
    (majutsu-metaedit:--force-rewrite)
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("m" "Metaedit" majutsu-metaedit-execute)]])

;;; _
(provide 'majutsu-metaedit)
;;; majutsu-metaedit.el ends here
