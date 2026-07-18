;;; majutsu-metaedit-test.el --- Tests for Metaedit transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Metaedit revision selection and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-metaedit)

(ert-deftest majutsu-metaedit-default-args/use-region-revisions ()
  "Initialize Metaedit with every revision in the active region."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) '("a" "b")))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _)
               (ert-fail "point fallback should not be read"))))
    (should (equal (majutsu-metaedit--default-args)
                   '("-r=a" "-r=b")))))

(ert-deftest majutsu-metaedit-default-args/use-point-or-at ()
  "Fall back from region to the revision at point, then to @."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) "point")))
    (should (equal (majutsu-metaedit--default-args) '("-r=point"))))
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) nil)))
    (should (equal (majutsu-metaedit--default-args) '("-r=@")))))

(ert-deftest majutsu-metaedit-arguments/accept-multiple-revisions ()
  "Preserve every revision selected in the Metaedit transient."
  (let ((transient-current-command 'majutsu-metaedit-transient))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _)
                 '("-r=a" "-r=b" "--update-author"))))
      (should (equal (majutsu-metaedit-arguments)
                     '("-r=a" "-r=b" "--update-author"))))))

(ert-deftest majutsu-metaedit/open-with-selection-session ()
  "Open Metaedit with a source-buffer session and region selections."
  (with-temp-buffer
    (let (setup-prefix setup-args)
      (cl-letf (((symbol-function 'magit-region-values)
                 (lambda (&rest _) '("a" "b")))
                ((symbol-function 'transient-setup)
                 (lambda (prefix &rest args)
                   (setq setup-prefix prefix
                         setup-args args))))
        (majutsu-metaedit)
        (should (eq setup-prefix 'majutsu-metaedit-transient))
        (should (equal (plist-get setup-args :value)
                       '("-r=a" "-r=b")))
        (let ((session (plist-get setup-args :scope)))
          (should (majutsu-selection-session-p session))
          (should (eq (majutsu-selection-session-buffer session)
                      (current-buffer))))))))

(ert-deftest majutsu-metaedit-revision-option/supports-selection ()
  "Expose revisions as a repeatable visual selection category."
  (let ((obj (get 'majutsu-metaedit:-r 'transient--suffix)))
    (should (cl-typep obj 'majutsu-metaedit-option))
    (should (equal (oref obj argument) "-r="))
    (should (eq (oref obj multi-value) 'repeat))
    (should (equal (oref obj selection-label) "[REVS]"))
    (should (equal (oref obj selection-toggle-key) "r"))
    (should (eq (oref obj targets-fn)
                #'majutsu-metaedit--selection-targets)))
  (let ((obj (make-instance 'majutsu-metaedit-option
                            :command 'ignore
                            :key "-r"
                            :argument "-r="
                            :multi-value 'repeat)))
    (oset obj value '("a" "b"))
    (should (equal (transient-infix-value obj)
                   '("-r=a" "-r=b")))))

(ert-deftest majutsu-metaedit-execute/runs-jj-with-selections ()
  "Pass all selected revisions and metadata options to jj."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) #'ignore))
      (majutsu-metaedit-execute
       '("-r=a" "-r=b" "--author=Test User <test@example.com>"))
      (should (equal called
                     '("metaedit" "-r=a" "-r=b"
                       "--author=Test User <test@example.com>"))))))

(provide 'majutsu-metaedit-test)
;;; majutsu-metaedit-test.el ends here
