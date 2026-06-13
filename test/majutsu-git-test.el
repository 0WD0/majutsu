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
        seen-start-args seen-mustmatch)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 "https://example.invalid/repo.git"))
              ((symbol-function 'read-directory-name)
               (lambda (&rest args)
                 (setq seen-mustmatch (nth 3 args))
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
      (should-not seen-mustmatch)
      (should (equal seen-start-args
                     '("clone" "--colocate"
                       "https://example.invalid/repo.git"
                       "/tmp/clone-target/"))))))

(ert-deftest majutsu-git-clone/localizes-source-path-for-jj ()
  "Clone should pass host-local source paths when default-directory is remote."
  (let ((default-directory "/ssh:demo:/tmp/current/")
        seen-start-args)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args)
                 "/ssh:demo:/srv/git/source.git"))
              ((symbol-function 'read-directory-name)
               (lambda (&rest _args)
                 "/ssh:demo:/tmp/current/"))
              ((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (path)
                 (pcase path
                   ("/ssh:demo:/srv/git/source.git" "/srv/git/source.git")
                   (_ path))))
              ((symbol-function 'majutsu--message-with-log)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-git--start)
               (lambda (args &optional _success _cb)
                 (setq seen-start-args args)
                 nil)))
      (majutsu-git-clone nil)
      (should (equal seen-start-args
                     '("clone" "/srv/git/source.git"))))))

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

(ert-deftest majutsu-git-url-or-path-arg/converts-only-local-paths ()
  "Git remote URL/path values should preserve URLs but localize Emacs paths."
  (cl-letf (((symbol-function 'majutsu-convert-filename-for-jj)
             (lambda (_path) "/srv/git/repo.git")))
    (should (equal (majutsu-git--url-or-path-arg
                    "/ssh:demo:/srv/git/repo.git")
                   "/srv/git/repo.git"))
    (should (equal (majutsu-git--url-or-path-arg
                    "https://example.invalid/repo.git")
                   "https://example.invalid/repo.git"))
    (should (equal (majutsu-git--url-or-path-arg
                    "git@example.invalid:repo.git")
                   "git@example.invalid:repo.git"))))

(ert-deftest majutsu-git-push-read-revset/splits-comma-values ()
  "Git push repeat revset reader should preserve CRM comma splitting."
  (should (equal (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                            (lambda (&rest _args) "a, b")))
                   (majutsu-git-push--read-revset "Revisions: " nil nil))
                 '("a" "b"))))

(ert-deftest majutsu-git-push-read-revset/empty-input-clears ()
  "Git push repeat revset reader should keep empty input unset."
  (should-not (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                         #'ignore))
                (majutsu-git-push--read-revset "Revisions: " nil nil))))

(ert-deftest majutsu-git-push-repo-args/keeps-stable-sync-policy ()
  "Push repo defaults should omit target-specific arguments."
  (should (equal (majutsu-git-push--repo-args
                  '("--remote=upstream" "--bookmark=main" "--all"
                    "--tracked" "--deleted" "--allow-private"
                    "--allow-empty-description" "--revisions=mine()"
                    "--change=abc" "--named=foo=@" "--option=ci.skip"
                    "--dry-run"))
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

(ert-deftest majutsu-git-transients/expose-simple-upstream-options ()
  "Git transients should expose simple upstream jj options."
  (should (transient-get-suffix 'majutsu-git-push-transient "-o"))
  (should (transient-get-suffix 'majutsu-git-fetch-transient "-R"))
  (should (transient-get-suffix 'majutsu-git-fetch-transient "-b"))
  (should (transient-get-suffix 'majutsu-git-clone-transient "-b")))

(defun majutsu-git-test--suffix-reader (suffix)
  "Return the reader configured for transient SUFFIX."
  (or (plist-get (cdr suffix) :reader)
      (when-let* ((command (plist-get (cdr suffix) :command))
                  (prototype (get command 'transient--suffix)))
        (oref prototype reader))))

(ert-deftest majutsu-git-transients/use-shared-remote-readers ()
  "Push/fetch/clone remote options should use the correct remote readers."
  (let* ((push-remote (transient-get-suffix 'majutsu-git-push-transient "-R"))
         (fetch-remote (transient-get-suffix 'majutsu-git-fetch-transient "-R"))
         (clone-remote (transient-get-suffix 'majutsu-git-clone-transient "-R"))
         (push-reader (majutsu-git-test--suffix-reader push-remote))
         (fetch-reader (majutsu-git-test--suffix-reader fetch-remote))
         (clone-reader (majutsu-git-test--suffix-reader clone-remote)))
    (should (or (eq push-reader 'majutsu-transient-read-remote-name)
                (equal push-reader '(function majutsu-transient-read-remote-name))))
    (should (or (eq fetch-reader 'majutsu-transient-read-remote-patterns)
                (equal fetch-reader '(function majutsu-transient-read-remote-patterns))))
    (should (or (eq clone-reader 'majutsu-transient-read-remote-name)
                (equal clone-reader '(function majutsu-transient-read-remote-name))))))

(ert-deftest majutsu-git-fetch-remote-reader/returns-repeat-values ()
  "Fetch remote reader should return a list for Transient's repeat option."
  (let* ((suffix (transient-get-suffix 'majutsu-git-fetch-transient "-R"))
         (reader (majutsu-git-test--suffix-reader suffix)))
    (cl-letf (((symbol-function 'majutsu-read-remote-patterns)
               (lambda (prompt candidates default initial-input history)
                 (should (equal prompt "Remote: "))
                 (should-not candidates)
                 (should-not default)
                 (should (equal initial-input "gerri"))
                 (should (eq history 'test-history))
                 '("gerrit"))))
      (let* ((value (funcall reader "Remote: " "gerri" 'test-history))
             (obj (make-instance 'transient-option
                                  :argument "--remote="
                                  :multi-value 'repeat)))
        (oset obj value value)
        (should (equal value '("gerrit")))
        (should (equal (transient-infix-value obj)
                       '("--remote=gerrit")))))))

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
  "Remote add should pass options followed by REMOTE and URL positionals."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-read-new-remote-name)
               (lambda (&rest _args) "origin"))
              ((symbol-function 'majutsu-git--read-url-or-path)
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

(ert-deftest majutsu-git-remote-add/localizes-path-positionals-and-options ()
  "Remote add should pass host-local paths for URL and --push-url."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-read-new-remote-name)
               (lambda (&rest _args) "local"))
              ((symbol-function 'majutsu-git--read-url-or-path)
               (lambda (&rest _args) "/ssh:demo:/srv/git/fetch.git"))
              ((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (path)
                 (pcase path
                   ("/ssh:demo:/srv/git/fetch.git" "/srv/git/fetch.git")
                   ("/ssh:demo:/srv/git/push.git" "/srv/git/push.git")
                   (_ path))))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-add '("--push-url=/ssh:demo:/srv/git/push.git"))
      (should (equal seen-args
                     '("git" ("remote" "add"
                              "--push-url=/srv/git/push.git"
                              "local"
                              "/srv/git/fetch.git")))))))

(ert-deftest majutsu-git-remote-set-url/reads-remote-from-transient-args ()
  "Remote set-url should extract --remote= from transient and pass as positional."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-set-url '("--remote=origin"
                                    "--fetch=https://example.invalid/fetch.git"))
      (should (equal seen-args
                     '("git" ("remote" "set-url" "origin"
                              "--fetch=https://example.invalid/fetch.git")))))))

(ert-deftest majutsu-git-remote-set-url/signals-when-remote-missing ()
  "Remote set-url should signal an error when --remote= is absent."
  (should-error
   (cl-letf (((symbol-function 'user-error)
              (lambda (&rest _args)
                (signal 'user-error nil))))
     (majutsu-git-remote-set-url '("--fetch=https://example.invalid/fetch.git")))))

(ert-deftest majutsu-git-remote-set-url/applies-no-url-args-as-no-op ()
  "Remote set-url should call jj without URL args when none given."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-set-url '("--remote=upstream"))
      (should (equal seen-args
                     '("git" ("remote" "set-url" "upstream")))))))

(ert-deftest majutsu-git-remote-set-url/passes-fetch-and-push-arguments ()
  "Remote set-url should pass --fetch and --push through to jj."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-set-url '("--remote=mirror"
                                    "--fetch=https://example.invalid/fetch.git"
                                    "--push=https://example.invalid/push.git"))
      (should (equal seen-args
                     '("git" ("remote" "set-url" "mirror"
                              "--fetch=https://example.invalid/fetch.git"
                              "--push=https://example.invalid/push.git")))))))

(ert-deftest majutsu-git-remote-set-url/converts-local-path-url-args ()
  "Remote set-url should convert Emacs paths in URL args."
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (_path) "/srv/git/fetch.git"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-set-url '("--remote=origin"
                                    "--fetch=/ssh:demo:/srv/git/fetch.git"))
      (should (equal seen-args
                     '("git" ("remote" "set-url" "origin"
                              "--fetch=/srv/git/fetch.git")))))))

(ert-deftest majutsu-git-remote-rename/requires-old-and-new-remote-readers ()
  "Remote rename should read an existing OLD and a new NEW positional."
  (let (seen-args seen-old-require-match seen-new-prompt)
    (cl-letf (((symbol-function 'majutsu-read-remote-name)
               (lambda (_prompt &optional require-match _default)
                 (setq seen-old-require-match require-match)
                 "origin"))
              ((symbol-function 'majutsu-read-new-remote-name)
               (lambda (prompt)
                 (setq seen-new-prompt prompt)
                 "upstream"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-git-remote-rename)
      (should seen-old-require-match)
      (should (string-match-p "origin" seen-new-prompt))
      (should (equal seen-args
                     '("git" ("remote" "rename" "origin" "upstream")))))))

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
