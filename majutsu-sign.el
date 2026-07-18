;;; majutsu-sign.el --- Commit signing transients for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides selection-aware frontends for `jj sign' and
;; `jj unsign'.

;;; Code:

(require 'epa)
(require 'majutsu)
(require 'majutsu-config)

(declare-function magit-read-gpg-secret-key "magit"
                  (prompt &optional initial-input history predicate default))

(defclass majutsu-sign-option (majutsu-selection-option)
  ())

(defun majutsu-sign--selection-targets ()
  "Return revisions selected by the region or point."
  (or (magit-region-values 'jj-commit t)
      (when-let* ((revision (magit-section-value-if 'jj-commit)))
        (list revision))))

(defun majutsu-sign--default-args ()
  "Return default revision arguments for signing commands."
  (mapcar (lambda (revision) (concat "--revision=" revision))
          (or (majutsu-sign--selection-targets) '("@"))))

(defun majutsu-sign--gpg-signing-key-p (certificate)
  "Return non-nil when CERTIFICATE contains a signing-capable key."
  (seq-some (lambda (key)
              (memq 'sign (epg-sub-key-capability key)))
            (epg-key-sub-key-list certificate)))

(defun majutsu-sign--read-gpg-key
    (prompt initial-input history protocol default)
  "Read a secret signing key using EPG.
PROMPT, INITIAL-INPUT, and HISTORY follow a transient reader.
PROTOCOL is the EPG protocol and DEFAULT is jj's configured key."
  (unless (fboundp 'magit-read-gpg-secret-key)
    (require 'magit))
  (let ((epa-protocol protocol))
    (magit-read-gpg-secret-key
     prompt initial-input history
     #'majutsu-sign--gpg-signing-key-p default)))

(defun majutsu-sign--ssh-key-files ()
  "Return conventional SSH public-key files for completion."
  (let ((directory (expand-file-name "~/.ssh")))
    (when (file-directory-p directory)
      (mapcar #'abbreviate-file-name
              (seq-filter #'file-regular-p
                          (directory-files directory t "\\.pub\\'" t))))))

(defun majutsu-sign--read-key (prompt &optional initial-input history)
  "Read a backend-specific signing key with PROMPT.
INITIAL-INPUT and HISTORY follow a transient option reader."
  (let ((backend (majutsu-get "signing.backend"))
        (default (majutsu-get "signing.key")))
    (pcase backend
      ("gpg"
       (let ((epg-gpg-program
              (or (majutsu-get "signing.backends.gpg.program")
                  epg-gpg-program)))
         (majutsu-sign--read-gpg-key
          prompt initial-input history 'OpenPGP default)))
      ("gpgsm"
       (let ((epg-gpgsm-program
              (or (majutsu-get "signing.backends.gpgsm.program")
                  epg-gpgsm-program)))
         (majutsu-sign--read-gpg-key
          prompt initial-input history 'CMS default)))
      ("ssh"
       (majutsu-completing-read
        prompt
        (delq nil (delete-dups
                   (cons default (majutsu-sign--ssh-key-files))))
        nil 'any initial-input history default))
      (_
       (read-string prompt initial-input history default)))))

(transient-define-suffix majutsu-sign-execute (args)
  "Execute jj sign with ARGS from the transient."
  :description "Sign"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-sign)))
  (when (zerop (apply #'majutsu-run-jj "sign" args))
    (message "Signed revisions")))

(transient-define-suffix majutsu-unsign-execute (args)
  "Execute jj unsign with ARGS from the transient."
  :description "Unsign"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-unsign)))
  (when (zerop (apply #'majutsu-run-jj "unsign" args))
    (message "Removed revision signatures")))

(transient-define-argument majutsu-sign:--revision ()
  :description "Revisions"
  :class 'majutsu-sign-option
  :selection-label "[REVS]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :targets-fn #'majutsu-sign--selection-targets
  :selection-toggle-key "r"
  :shortarg "-r"
  :argument "--revision="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-sign:--key ()
  :description "Signing key"
  :class 'transient-option
  :key "-k"
  :argument "--key="
  :reader #'majutsu-sign--read-key)

(defun majutsu-sign--setup (command)
  "Open signing transient COMMAND for the current source buffer."
  (transient-setup
   command nil nil
   :scope (majutsu-selection-session-begin)
   :value (majutsu-sign--default-args)))

;;;###autoload(autoload 'majutsu-sign "majutsu-sign" nil t)
(transient-define-prefix majutsu-sign ()
  "Transient for jj sign."
  :man-page "jj-sign"
  :class 'majutsu-jj-transient-prefix
  :jj-command "sign"
  :transient-non-suffix t
  :description "JJ Sign"
  [["Selection"
    (majutsu-sign:--revision)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Options"
    (majutsu-sign:--key)
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("S" "Sign" majutsu-sign-execute)]]
  (interactive)
  (majutsu-sign--setup 'majutsu-sign))

;;;###autoload(autoload 'majutsu-unsign "majutsu-sign" nil t)
(transient-define-prefix majutsu-unsign ()
  "Transient for jj unsign."
  :man-page "jj-unsign"
  :class 'majutsu-jj-transient-prefix
  :jj-command "unsign"
  :transient-non-suffix t
  :description "JJ Unsign"
  [["Selection"
    (majutsu-sign:--revision)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Options"
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("U" "Unsign" majutsu-unsign-execute)]]
  (interactive)
  (majutsu-sign--setup 'majutsu-unsign))

;;; _
(provide 'majutsu-sign)
;;; majutsu-sign.el ends here
