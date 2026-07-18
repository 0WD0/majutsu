;;; majutsu-sign-test.el --- Tests for signing transients  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for jj sign/unsign selection and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-config)
(require 'majutsu-sign)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-sign-default-args/use-region-revisions ()
  "Initialize signing commands with every revision in the active region."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) '("a" "b")))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _)
               (ert-fail "point fallback should not be read"))))
    (should (equal (majutsu-sign--default-args)
                   '("--revision=a" "--revision=b")))))

(ert-deftest majutsu-sign-default-args/use-point-or-at ()
  "Fall back from region to the revision at point, then to @."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) "point")))
    (should (equal (majutsu-sign--default-args)
                   '("--revision=point"))))
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) nil)))
    (should (equal (majutsu-sign--default-args)
                   '("--revision=@")))))

(ert-deftest majutsu-sign-execute/runs-jj-with-selections-and-key ()
  "Pass selected revisions and the signing key to jj sign."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) #'ignore))
      (majutsu-sign-execute
       '("--revision=a" "--revision=b" "--key=test-key"))
      (should (equal called
                     '("sign" "--revision=a" "--revision=b"
                       "--key=test-key"))))))

(ert-deftest majutsu-unsign-execute/runs-jj-with-selections ()
  "Pass selected revisions to jj unsign."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) #'ignore))
      (majutsu-unsign-execute '("--revision=a" "--revision=b"))
      (should (equal called
                     '("unsign" "--revision=a" "--revision=b"))))))

(ert-deftest majutsu-sign-transients/use-selection-sessions ()
  "Open Sign and Unsign with source-buffer selection sessions."
  (dolist (command '(majutsu-sign majutsu-unsign))
    (with-temp-buffer
      (let (setup-prefix setup-args)
        (cl-letf (((symbol-function 'magit-region-values)
                   (lambda (&rest _) '("a" "b")))
                  ((symbol-function 'transient-setup)
                   (lambda (prefix &rest args)
                     (setq setup-prefix prefix
                           setup-args args))))
          (funcall command)
          (should (eq setup-prefix command))
          (should (equal (plist-get setup-args :value)
                         '("--revision=a" "--revision=b")))
          (let ((session (plist-get setup-args :scope)))
            (should (majutsu-selection-session-p session))
            (should (eq (majutsu-selection-session-buffer session)
                        (current-buffer)))))))))

(ert-deftest majutsu-sign-revision-option/supports-visual-selection ()
  "Expose repeatable visual revision selection to both commands."
  (let ((obj (get 'majutsu-sign:--revision 'transient--suffix)))
    (should (cl-typep obj 'majutsu-sign-option))
    (should (equal (oref obj argument) "--revision="))
    (should (eq (oref obj multi-value) 'repeat))
    (should (equal (oref obj selection-label) "[REVS]"))
    (should (equal (oref obj selection-toggle-key) "r")))
  (should (transient-get-suffix 'majutsu-sign "-r"))
  (should (transient-get-suffix 'majutsu-unsign "-r"))
  (should (transient-get-suffix 'majutsu-sign "-k"))
  (should-not (ignore-errors
                (transient-get-suffix 'majutsu-unsign "-k"))))

(ert-deftest majutsu-sign-key-option/uses-backend-aware-reader ()
  "Read signing keys with the backend-aware completion reader."
  (let ((obj (get 'majutsu-sign:--key 'transient--suffix)))
    (should (eq (oref obj reader) #'majutsu-sign--read-key))))

(ert-deftest majutsu-sign-read-key/completes-gpg-secret-keys ()
  "Complete secret signing keys with jj's configured GPG program."
  (let (read-args protocol program)
    (cl-letf (((symbol-function 'majutsu-get)
               (lambda (key)
                 (pcase key
                   ("signing.backend" "gpg")
                   ("signing.key" "configured-key")
                   ("signing.backends.gpg.program" "gpg2"))))
              ((symbol-function 'magit-read-gpg-secret-key)
               (lambda (&rest args)
                 (setq read-args args
                       protocol epa-protocol
                       program epg-gpg-program)
                 "fingerprint")))
      (should (equal (majutsu-sign--read-key
                      "Signing key" "initial" 'test-history)
                     "fingerprint"))
      (should (equal (seq-take read-args 3)
                     '("Signing key" "initial" test-history)))
      (should (functionp (nth 3 read-args)))
      (should (equal (nth 4 read-args) "configured-key"))
      (should (eq protocol 'OpenPGP))
      (should (equal program "gpg2")))))

(ert-deftest majutsu-sign-read-key/completes-gpgsm-secret-keys ()
  "Use the CMS protocol and configured program for GPGSM keys."
  (let (protocol program)
    (cl-letf (((symbol-function 'majutsu-get)
               (lambda (key)
                 (pcase key
                   ("signing.backend" "gpgsm")
                   ("signing.key" nil)
                   ("signing.backends.gpgsm.program" "/usr/bin/gpgsm"))))
              ((symbol-function 'magit-read-gpg-secret-key)
               (lambda (&rest _)
                 (setq protocol epa-protocol
                       program epg-gpgsm-program)
                 "certificate")))
      (should (equal (majutsu-sign--read-key "Signing key")
                     "certificate"))
      (should (eq protocol 'CMS))
      (should (equal program "/usr/bin/gpgsm")))))

(ert-deftest majutsu-sign-read-key/completes-ssh-public-key-files ()
  "Offer configured and conventional SSH public keys without requiring one."
  (let (read-args)
    (cl-letf (((symbol-function 'majutsu-get)
               (lambda (key)
                 (pcase key
                   ("signing.backend" "ssh")
                   ("signing.key" "ssh-ed25519 configured"))))
              ((symbol-function 'majutsu-sign--ssh-key-files)
               (lambda () '("~/.ssh/id_ed25519.pub"
                            "~/.ssh/id_rsa.pub")))
              ((symbol-function 'majutsu-completing-read)
               (lambda (&rest args)
                 (setq read-args args)
                 "~/.ssh/id_ed25519.pub")))
      (should (equal (majutsu-sign--read-key
                      "Signing key" "initial" 'test-history)
                     "~/.ssh/id_ed25519.pub"))
      (should (equal (nth 1 read-args)
                     '("ssh-ed25519 configured"
                       "~/.ssh/id_ed25519.pub"
                       "~/.ssh/id_rsa.pub")))
      (should (eq (nth 3 read-args) 'any))
      (should (equal (nth 4 read-args) "initial"))
      (should (eq (nth 5 read-args) 'test-history))
      (should (equal (nth 6 read-args) "ssh-ed25519 configured")))))

(ert-deftest majutsu-sign-read-key/keeps-free-input-for-other-backends ()
  "Fall back to free input when jj cannot enumerate backend keys."
  (let (read-args)
    (cl-letf (((symbol-function 'majutsu-get)
               (lambda (key)
                 (pcase key
                   ("signing.backend" "test")
                   ("signing.key" "configured-key"))))
              ((symbol-function 'read-string)
               (lambda (&rest args)
                 (setq read-args args)
                 "custom-key")))
      (should (equal (majutsu-sign--read-key
                      "Signing key" "initial" 'test-history)
                     "custom-key"))
      (should (equal read-args
                     '("Signing key" "initial" test-history
                       "configured-key"))))))

(ert-deftest majutsu-dispatch/exposes-signing-transients ()
  "Expose Sign and Unsign from the top-level dispatcher."
  (should (transient-get-suffix 'majutsu-dispatch "j"))
  (should (transient-get-suffix 'majutsu-dispatch "J")))

(ert-deftest majutsu-sign-integration/signs-and-unsigns-working-copy ()
  "Exercise jj's test signing backend through Sign and Unsign arguments."
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-call
     repo "sign" "--revision=@"
     "--config=signing.backend=test"
     "--config=signing.behavior=keep")
    (should (string-match-p
             "good test-display"
             (majutsu-jj-integration-output
              repo "show" "-r=@" "--no-patch"
              "-T=if(signature, signature.status() ++ \" \" ++ signature.display())"
              "--config=signing.backend=test"
              "--config=signing.behavior=keep")))
    (majutsu-jj-integration-call
     repo "unsign" "--revision=@" "--config=signing.behavior=keep")
    (should-not
     (string-match-p
      "good test-display"
      (majutsu-jj-integration-output
       repo "show" "-r=@" "--no-patch"
       "-T=if(signature, signature.status() ++ \" \" ++ signature.display())"
       "--config=signing.backend=test"
       "--config=signing.behavior=keep")))))

(provide 'majutsu-sign-test)
;;; majutsu-sign-test.el ends here
