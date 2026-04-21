;;; majutsu-command-test.el --- Tests for ad-hoc command runners  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Magit-style command runner compatibility.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-command)
(require 'majutsu-mode)

(ert-deftest majutsu-command-test-remaps-magit-run-commands ()
  "Majutsu modes should remap Magit's ad-hoc runner commands."
  (with-temp-buffer
    (majutsu-mode)
    (should (eq (command-remapping 'magit-run) 'majutsu-command))
    (should (eq (command-remapping 'magit-git-command) 'majutsu-jj-command))
    (should (eq (command-remapping 'magit-git-command-topdir)
                'majutsu-jj-command-topdir))
    (should (eq (command-remapping 'magit-shell-command)
                'majutsu-shell-command))
    (should (eq (command-remapping 'magit-shell-command-topdir)
                'majutsu-shell-command-topdir))))

(ert-deftest majutsu-command-test-shell-command-jj-args-parses-plain-jj ()
  "Plain jj commands should be parsed into argv for direct execution."
  (let ((majutsu-jj-executable "/usr/bin/jj"))
    (should (equal (majutsu--shell-command-jj-args "jj status -r @")
                   '("status" "-r" "@")))
    (should (equal (majutsu--shell-command-jj-args "JJ.exe log -r 'all()'")
                   '("log" "-r" "all()")))))

(ert-deftest majutsu-command-test-shell-command-jj-args-rejects-shell-syntax ()
  "Commands using shell operators should stay on the shell path."
  (let ((majutsu-jj-executable "jj"))
    (should-not (majutsu--shell-command-jj-args "jj status | sed -n 1p"))
    (should-not (majutsu--shell-command-jj-args "echo hi && jj status"))))

(ert-deftest majutsu-command-test-start-shell-command-runs-jj-directly ()
  "Plain jj commands should use Majutsu's process helpers directly."
  (let ((majutsu-jj-executable "/usr/bin/jj")
        call)
    (cl-letf (((symbol-function 'majutsu-process-jj-arguments)
               (lambda (args)
                 (append '("--no-pager" "--color=always") args)))
              ((symbol-function 'majutsu-start-process)
               (lambda (&rest args)
                 (setq call args)
                 'process)))
      (should (eq (majutsu--start-shell-command "jj st") 'process))
      (should (equal call
                     '("/usr/bin/jj" nil "--no-pager" "--color=always" "st"))))))

(ert-deftest majutsu-command-test-start-shell-command-runs-other-commands-via-shell ()
  "Non-jj commands should be executed through the shell."
  (let ((shell-file-name "/bin/sh")
        (shell-command-switch "-c")
        call)
    (cl-letf (((symbol-function 'majutsu-start-process)
               (lambda (&rest args)
                 (setq call args)
                 'process)))
      (should (eq (majutsu--start-shell-command "printf test") 'process))
      (should (equal call
                     '("/bin/sh" nil "-c" "printf test"))))))

(provide 'majutsu-command-test)
;;; majutsu-command-test.el ends here
