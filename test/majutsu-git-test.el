;;; majutsu-git-test.el --- Tests for majutsu-git -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for path handling in `majutsu-git.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-git)

(ert-deftest majutsu-git-remote-candidate-data/parses-fetch-and-push-urls ()
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args)
               '("origin git@github.com:0WD0/majutsu.git"
                 "rad rad://z4fugm4aenykjpk8tpvvqjvwtzvwj (push: rad://z4fugm4aenykjpk8tpvvqjvwtzvwj/z6Mk...)"))))
    (let* ((payload (majutsu-git-remote-candidate-data))
           (entries (plist-get payload :entries))
           (origin (gethash "origin" entries))
           (rad (gethash "rad" entries)))
      (should (equal (plist-get payload :candidates) '("origin" "rad")))
      (should (equal (plist-get origin :fetch-url) "git@github.com:0WD0/majutsu.git"))
      (should (equal (plist-get rad :fetch-url)
                     "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj"))
      (should (equal (plist-get rad :push-url)
                     "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj/z6Mk...")))))

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

(ert-deftest majutsu-git-read-remote/uses-history-and-category ()
  (let (seen-history seen-category prewarm)
    (cl-letf (((symbol-function 'majutsu-git-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin" "upstream")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-marginalia-prewarm-candidate-data)
               (lambda (&rest args)
                 (setq prewarm args)))
              ((symbol-function 'completing-read)
               (lambda (_prompt table _predicate _require-match _initial history _default)
                 (setq seen-history history)
                 (let ((metadata (funcall table "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 "origin")))
      (should (equal (majutsu-git--read-remote "Remote") "origin"))
      (should (eq seen-history 'majutsu-remote-name-history))
      (should (eq seen-category 'majutsu-remote))
      (should (eq (car prewarm) 'majutsu-remote)))))

(ert-deftest majutsu-git-push-repo-args/keeps-stable-sync-policy ()
  "Push repo defaults should omit target-specific arguments."
  (should (equal (majutsu-git-push--repo-args
                  '("--remote=upstream" "--bookmark=main" "--all"
                    "--tracked" "--deleted" "--allow-private"
                    "--allow-empty-description" "--revisions=mine()"
                    "--change=abc" "--named=foo=@" "--dry-run"))
                 '("--remote=upstream" "--all" "--tracked" "--deleted"
                   "--allow-private" "--allow-empty-description"))))

(ert-deftest majutsu-git-fetch-repo-args/keeps-stable-sync-policy ()
  "Fetch repo defaults should omit branch-specific arguments."
  (should (equal (majutsu-git-fetch--repo-args
                  '("--remote=upstream" "--branch=main"
                    "--tracked" "--all-remotes"))
                 '("--remote=upstream" "--tracked" "--all-remotes"))))

(ert-deftest majutsu-git-sync-transients/expose-repo-default-action ()
  "Git push/fetch transients should expose repository-local defaults."
  (dolist (prefix '(majutsu-git-push-transient majutsu-git-fetch-transient))
    (let ((suffix (transient-get-suffix prefix "W")))
      (should suffix)
      (should (eq (plist-get (cdr suffix) :command)
                  'majutsu-transient-save-repository-defaults)))))

(ert-deftest majutsu-git-remote-transients/split-command-specific-options ()
  "Remote add/set-url options should live on command-specific transients."
  (should (transient-get-suffix 'majutsu-git-remote-transient "a"))
  (should-not (ignore-errors
                (transient-get-suffix 'majutsu-git-remote-transient "-T")))
  (should (transient-get-suffix 'majutsu-git-remote-add-transient "-T"))
  (should (transient-get-suffix 'majutsu-git-remote-add-transient "-P"))
  (should (transient-get-suffix 'majutsu-git-remote-set-url-transient "-f"))
  (should (transient-get-suffix 'majutsu-git-remote-set-url-transient "-p")))

(ert-deftest majutsu-git-remote-add/passes-add-arguments ()
  "Remote add should pass add-specific transient arguments."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-git--read-remote)
               (lambda (&rest _args) "origin"))
              ((symbol-function 'read-string)
               (lambda (&rest _args) "https://example.invalid/fetch.git"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-add '("--fetch-tags=all"
                                "--push-url=https://example.invalid/push.git"))
      (should (equal seen-args
                     '("git" ("remote" "add"
                              "--fetch-tags=all"
                              "--push-url=https://example.invalid/push.git"
                              "origin"
                              "https://example.invalid/fetch.git")))))))

(ert-deftest majutsu-git-remote-set-url/passes-fetch-and-push-arguments ()
  "Remote set-url should pass fetch/push-specific transient arguments."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-git--read-remote)
               (lambda (&rest _args) "origin"))
              ((symbol-function 'read-string)
               (lambda (&rest _args)
                 (ert-fail "Should not prompt when transient args are present")))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-set-url '("--fetch=https://example.invalid/fetch.git"
                                    "--push=https://example.invalid/push.git"))
      (should (equal seen-args
                     '("git" ("remote" "set-url" "origin"
                              "--fetch=https://example.invalid/fetch.git"
                              "--push=https://example.invalid/push.git")))))))

(ert-deftest majutsu-git-push-transient/uses-repository-defaults ()
  "Git sync transients should read defaults via the generic repo layer."
  (let ((transient-values nil)
        (config-id "0123456789abcdefabcd"))
    (cl-letf (((symbol-function 'majutsu-repository-config-id)
               (lambda (&optional _create) config-id)))
      (setf (alist-get (majutsu-transient-repository-default-key
                        'majutsu-git 'majutsu-git-push)
                       transient-values)
            '("--remote=upstream" "--tracked"))
      (let ((obj (make-instance 'majutsu-repository-transient-prefix
                                :command 'majutsu-git-push-transient
                                :repo-namespace 'majutsu-git
                                :repo-key 'majutsu-git-push)))
        (transient-init-value obj)
        (should (equal (oref obj value)
                       '("--remote=upstream" "--tracked")))))))

(ert-deftest majutsu-transient-save-repository-defaults/saves-filtered-args ()
  "The generic repo-default action should save remembered arguments only."
  (let* ((transient-values nil)
         (config-id "0123456789abcdefabcd")
         (prototype (make-instance
                     'majutsu-repository-transient-prefix
                     :command 'majutsu-git-push-transient
                     :repo-namespace 'majutsu-git
                     :repo-key 'majutsu-git-push
                     :repo-filter #'majutsu-git-push--repo-args))
         (transient--prefix (make-instance
                             'majutsu-repository-transient-prefix
                             :command 'majutsu-git-push-transient
                             :prototype prototype))
         saved)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-repository-config-id)
                   (lambda (&optional _create) config-id))
                  ((symbol-function 'transient-args)
                   (lambda (_command)
                     '("--remote=upstream" "--change=abc" "--tracked")))
                  ((symbol-function 'transient-save-values)
                   (lambda () (setq saved t)))
                  ((symbol-function 'transient--history-push)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'message)
                   (lambda (&rest _args) nil)))
          (majutsu-transient-save-repository-defaults)
          (let ((key (majutsu-transient-repository-default-key
                      'majutsu-git 'majutsu-git-push)))
            (should (equal (cdr (assq key transient-values))
                           '("--remote=upstream" "--tracked")))
            (should (equal (majutsu-transient-repository-current-value
                            'majutsu-git 'majutsu-git-push config-id)
                           '("--remote=upstream" "--tracked")))))
      (put 'majutsu-git-push 'majutsu-git-current-repository-values nil))
    (should saved)))

(provide 'majutsu-git-test)
;;; majutsu-git-test.el ends here
