;;; majutsu-git-test.el --- Tests for majutsu-git -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for path handling in `majutsu-git.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-git)

(ert-deftest majutsu-git--expand-option-arg/strips-tramp-prefix ()
  "--git-repo option paths should be converted to host-local paths."
  (cl-letf (((symbol-function 'majutsu-convert-filename-for-jj)
             (lambda (_path)
               "/srv/git/repo")))
    (should (equal (majutsu-git--expand-option-arg
                    "--git-repo=/ssh:demo:/srv/git/repo"
                    "--git-repo=")
                   "--git-repo=/srv/git/repo"))))

(ert-deftest majutsu-git-clone/uses-local-destination-for-jj ()
  "Clone should pass local destination path when default-directory is remote."
  (let ((default-directory "/ssh:demo:/tmp/current/")
        seen-start-args)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 "https://example.invalid/repo.git"))
              ((symbol-function 'read-directory-name)
               (lambda (&rest _args)
                 "/ssh:demo:/tmp/clone-target/"))
              ((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (_path)
                 "/tmp/clone-target/"))
              ((symbol-function 'majutsu--message-with-log)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-git--start)
               (lambda (args &optional _success _cb)
                 (setq seen-start-args args)
                 nil)))
      (majutsu-git-clone '("--colocate"))
      (should (equal seen-start-args
                     '("clone" "--colocate"
                       "https://example.invalid/repo.git"
                       "/tmp/clone-target/"))))))

(ert-deftest majutsu-git-init/uses-local-paths-for-jj-args ()
  "Init should localize both DEST and --git-repo path arguments."
  (let ((default-directory "/ssh:demo:/tmp/current/")
        seen-start-args)
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _args)
                 "/ssh:demo:/tmp/newrepo/"))
              ((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (path)
                 (pcase path
                   ("/ssh:demo:/tmp/existing/.git" "/tmp/existing/.git")
                   ("/ssh:demo:/tmp/newrepo/" "/tmp/newrepo/")
                   (_ path))))
              ((symbol-function 'majutsu--message-with-log)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-git--start)
               (lambda (args &optional _success _cb)
                 (setq seen-start-args args)
                 nil)))
      (majutsu-git-init '("--git-repo=/ssh:demo:/tmp/existing/.git" "--colocate"))
      (should (equal seen-start-args
                     '("init" "--git-repo=/tmp/existing/.git" "--colocate" "/tmp/newrepo/"))))))

(ert-deftest majutsu-git-root/expands-remote-path-before-copy ()
  "Git root output should be expanded to an Emacs-usable remote path."
  (let ((default-directory "/ssh:demo:/tmp/repo/")
        copied)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest _args)
                 '("/home/demo/repo/.git")))
              ((symbol-function 'majutsu-jj-expand-filename-from-jj)
               (lambda (path _directory)
                 (concat "/ssh:demo:" path)))
              ((symbol-function 'kill-new)
               (lambda (text)
                 (setq copied text)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-root)
      (should (equal copied "/ssh:demo:/home/demo/repo/.git")))))

(provide 'majutsu-git-test)
;;; majutsu-git-test.el ends here
