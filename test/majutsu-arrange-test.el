;;; majutsu-arrange-test.el --- Tests for jj's native Arrange TUI  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the thin Arrange-to-terminal-session adapter.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-arrange)

(ert-deftest majutsu-arrange-tui/no-revsets-defers-to-jj-configuration ()
  "Omitting revsets passes only `arrange', preserving jj's configured default."
  (let ((origin (current-buffer)) called)
    (cl-letf (((symbol-function 'majutsu-terminal-session-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-terminal-session-start-jj)
               (lambda (&rest args)
                 (setq called args)
                 'session)))
      (should (eq (majutsu-arrange-tui) 'session)))
    (should
     (equal called
            (list '("arrange")
                  :origin-buffer origin
                  :title "arrange"
                  :unchanged-message
                  "jj arrange ended without a repository operation")))))

(ert-deftest majutsu-arrange-tui/preserves-distinct-revset-arguments ()
  "Multiple revsets remain separate argv tokens, including spaces within one."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-terminal-session-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-terminal-session-start-jj)
               (lambda (argv &rest _keys)
                 (setq called argv))))
      (majutsu-arrange-tui "roots(foo)" "bar | baz"))
    (should (equal called '("arrange" "roots(foo)" "bar | baz")))))

(ert-deftest majutsu-arrange-tui/rejects-missing-ghostel-before-start ()
  "Arrange never falls back to a non-PTY process host."
  (cl-letf (((symbol-function 'majutsu-terminal-session-available-p)
             (lambda () nil)))
    (should-error (majutsu-arrange-tui) :type 'user-error)))

(ert-deftest majutsu-arrange-tui/interactive-default-does-not-prompt ()
  "Ordinary interactive invocation leaves `revsets.arrange' to jj."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-read-revset)
               (lambda (&rest _)
                 (ert-fail "Arrange unexpectedly prompted for a revset")))
              ((symbol-function 'majutsu-terminal-session-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-terminal-session-start-jj)
               (lambda (argv &rest _keys)
                 (setq called argv))))
      (let ((current-prefix-arg nil))
        (call-interactively #'majutsu-arrange-tui)))
    (should (equal called '("arrange")))))

(ert-deftest majutsu-arrange-tui/interactive-prefix-reads-one-revset ()
  "A prefix argument opts into one native revset expression."
  (let (reader-args called)
    (cl-letf (((symbol-function 'majutsu-read-revset)
               (lambda (&rest args)
                 (setq reader-args args)
                 "mine() | @"))
              ((symbol-function 'majutsu-terminal-session-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-terminal-session-start-jj)
               (lambda (argv &rest _keys)
                 (setq called argv))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'majutsu-arrange-tui)))
    (should (equal reader-args
                   '("Arrange revisions" nil ("arrange"))))
    (should (equal called '("arrange" "mine() | @")))))

(provide 'majutsu-arrange-test)
;;; majutsu-arrange-test.el ends here
