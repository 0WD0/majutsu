;;; majutsu-gerrit-test.el --- Tests for majutsu-gerrit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for `majutsu-gerrit.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit)
(require 'majutsu-git)

(defun majutsu-gerrit-test--suffix-prototype (suffix)
  "Return the prototype object configured for transient SUFFIX."
  (when-let* ((command (plist-get (cdr suffix) :command)))
    (get command 'transient--suffix)))

(defun majutsu-gerrit-test--suffix-reader (suffix)
  "Return the reader configured for transient SUFFIX."
  (or (plist-get (cdr suffix) :reader)
      (when-let* ((prototype (majutsu-gerrit-test--suffix-prototype suffix)))
        (oref prototype reader))))

(ert-deftest majutsu-gerrit-upload/starts-jj-gerrit-upload-async ()
  "Upload should dispatch to `jj gerrit upload' asynchronously."
  (let (seen-args seen-success seen-message)
    (cl-letf (((symbol-function 'majutsu--message-with-log)
               (lambda (format-string &rest args)
                 (setq seen-message (apply #'format format-string args))))
              ((symbol-function 'majutsu-gerrit--start)
               (lambda (args &optional success-msg _finish-callback)
                 (setq seen-args args)
                 (setq seen-success success-msg)
                 'process)))
      (should (eq (majutsu-gerrit-upload '("--revision=@-" "--dry-run"))
                  'process))
      (should (equal seen-message "Uploading to Gerrit..."))
      (should (equal seen-args '("upload" "--revision=@-" "--dry-run")))
      (should (equal seen-success "Gerrit upload dry run completed")))))

(ert-deftest majutsu-gerrit-upload/uses-success-message-for-real-upload ()
  "Non-dry-run upload should use a normal success message."
  (let (seen-success)
    (cl-letf (((symbol-function 'majutsu--message-with-log)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-gerrit--start)
               (lambda (_args &optional success-msg _finish-callback)
                 (setq seen-success success-msg))))
      (majutsu-gerrit-upload '("--revision=@-"))
      (should (equal seen-success "Uploaded to Gerrit")))))

(ert-deftest majutsu-gerrit-upload-arguments/direct-uses-jj-default ()
  "Outside the transient, leave revisions unset so jj can default @/@-."
  (let ((transient-current-command nil))
    (should-not (majutsu-gerrit-upload-arguments))))

(ert-deftest majutsu-gerrit-upload-arguments/uses-transient-args ()
  "Inside the transient, use the transient's explicit selections only."
  (let ((transient-current-command 'majutsu-gerrit-upload-transient))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--dry-run" "--revision=abc"))))
      (should (equal (majutsu-gerrit-upload-arguments)
                     '("--dry-run" "--revision=abc"))))))

(ert-deftest majutsu-gerrit-upload-read-revset/uses-native-completion-context ()
  "The upload revision reader should complete in jj gerrit upload context."
  (let (seen-prompt seen-default seen-context)
    (cl-letf (((symbol-function 'majutsu-read-revset)
               (lambda (prompt default completion-args)
                 (setq seen-prompt prompt)
                 (setq seen-default default)
                 (setq seen-context completion-args)
                 "@-")))
      (should (equal (majutsu-gerrit-upload--read-revset
                      "Revision: " "@" nil)
                     "@-"))
      (should (equal seen-prompt "Revision: "))
      (should (equal seen-default "@"))
      (should (equal seen-context '("gerrit" "upload" "-r"))))))

(ert-deftest majutsu-gerrit-upload-transient/uses-toggle-at-point-revision-selection ()
  "Upload revisions should use Majutsu's selection/toggle UI."
  (let* ((option (transient-get-suffix 'majutsu-gerrit-upload-transient "-r"))
         (option-prototype (majutsu-gerrit-test--suffix-prototype option)))
    (should (cl-typep option-prototype 'majutsu-gerrit-upload-option))
    (should (equal (oref option-prototype argument) "--revision="))
    (should (eq (oref option-prototype multi-value) 'repeat))
    (should (equal (oref option-prototype selection-label) "[REV]"))
    (should (equal (oref option-prototype selection-toggle-key) "r"))))

(ert-deftest majutsu-gerrit-upload-transient/uses-shared-remote-reader ()
  "Upload remote option should use the exact remote reader."
  (let* ((remote (transient-get-suffix 'majutsu-gerrit-upload-transient "-R"))
         (reader (majutsu-gerrit-test--suffix-reader remote)))
    (should (or (eq reader 'majutsu-transient-read-remote-name)
                (equal reader '(function majutsu-transient-read-remote-name))))))

(ert-deftest majutsu-gerrit-upload-repo-args/keeps-stable-policy-only ()
  "Repository defaults should omit change-specific and dangerous options."
  (should (equal (majutsu-gerrit-upload--repo-args
                  '("--remote=gerrit"
                    "--remote-branch=main"
                    "--notify=owner"
                    "--ignore-attention-set"
                    "--revision=@-"
                    "--dry-run"
                    "--message=patch set"
                    "--submit"
                    "--skip-validation"
                    "--wip"
                    "--ready"
                    "--private"
                    "--remove-private"
                    "--reviewer=a@example.invalid"
                    "--cc=b@example.invalid"
                    "--topic=topic"
                    "--hashtag=hash"
                    "--label=Commit-Queue+1"))
                 '("--remote=gerrit"
                   "--remote-branch=main"
                   "--notify=owner"
                   "--ignore-attention-set"))))

(provide 'majutsu-gerrit-test)
;;; majutsu-gerrit-test.el ends here
