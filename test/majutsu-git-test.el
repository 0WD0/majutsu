;;; majutsu-git-test.el --- Tests for majutsu-git -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for path handling in `majutsu-git.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-git)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-git-remote-list/reuses-bookmark-buffer ()
  (require 'majutsu-bookmark)
  (save-window-excursion
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((buffer (current-buffer))
            (inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (magit-insert-section (bookmark-group 'local)
            (magit-insert-heading "Local bookmarks"))
          (magit-insert-section (bookmark-group 'remote)
            (magit-insert-heading "Remote bookmarks")
            (insert "body\n")))
        (magit-section-hide (cadr (oref magit-root-section children)))
        (cl-letf (((symbol-function 'majutsu-bookmark-list) (lambda () buffer)))
          (majutsu-git-remote-list))
        (should (eq (current-buffer) buffer))
        (should (eq (oref (magit-current-section) value) 'remote))
        (should-not (oref (magit-current-section) hidden))))))

(ert-deftest majutsu-git-fetch-remote/selects-only-current-remote ()
  (let (args)
    (cl-letf (((symbol-function 'majutsu-remote-at-point) (lambda () "fork*"))
              ((symbol-function 'majutsu-git-fetch) (lambda (value) (setq args value))))
      (majutsu-git-fetch-remote)
      (should (equal args '("--remote" "exact:fork*")))))
  (cl-letf (((symbol-function 'majutsu-remote-at-point) (lambda () nil)))
    (should-error (majutsu-git-fetch-remote) :type 'user-error)))

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

(ert-deftest majutsu-git-push-read-revset/empty-input-clears ()
  "Git push repeat revset reader should keep empty input unset."
  (should-not (cl-letf (((symbol-function 'majutsu-read-revset)
                         #'ignore))
                (majutsu-git-push--read-revset "Revisions: " nil nil))))

(ert-deftest majutsu-git-push-remembered-args/keeps-stable-sync-policy ()
  "Push defaults should omit target-specific arguments."
  (should (equal (majutsu-git-push--remembered-args
                  '("--remote=upstream" "--bookmark=main" "--tag=v1" "--all"
                    "--tracked" "--deleted" "--allow-private"
                    "--allow-empty-description" "--revisions=mine()"
                    "--change=abc" "--named=foo=@" "--option=ci.skip"
                    "--dry-run"))
                 '("--remote=upstream" "--all" "--tracked" "--deleted"
                   "--allow-private" "--allow-empty-description"))))

(ert-deftest majutsu-git-fetch-remembered-args/keeps-stable-sync-policy ()
  "Fetch defaults should omit branch/tag-specific arguments."
  (should (equal (majutsu-git-fetch--remembered-args
                  '("--remote=upstream" "--branch=main" "--tag=v1"
                    "--tracked" "--all-remotes"))
                 '("--remote=upstream" "--tracked" "--all-remotes"))))

(defun majutsu-git-test--suffix-reader (suffix)
  "Return the reader configured for transient SUFFIX."
  (or (plist-get (cdr suffix) :reader)
      (when-let* ((command (plist-get (cdr suffix) :command))
                  (prototype (get command 'transient--suffix)))
        (oref prototype reader))))

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

(ert-deftest majutsu-git-push-revset/retains-function-arguments ()
  "The push option consumer receives one whole free-form revset."
  (cl-letf (((symbol-function 'majutsu-read-revset)
             (lambda (&rest _) "ancestors(@, 2)")))
    (let ((obj (make-instance 'transient-option
                              :argument "--revisions=" :multi-value 'repeat)))
      (oset obj value (majutsu-git-push--read-revset "Revisions: " nil nil))
      (should (equal (transient-infix-value obj)
                     '("--revisions=ancestors(@, 2)"))))))

(ert-deftest majutsu-git-clone-remote/accepts-new-name-outside-repository ()
  "Clone outside a repository creates the remote chosen in its transient."
  (majutsu-jj-integration-with-sandbox sandbox
    (let* ((source (majutsu-jj-integration-init sandbox "source"))
           (destination (expand-file-name "clone" sandbox))
           (default-directory (file-name-as-directory sandbox))
           (reader (majutsu-git-test--suffix-reader
                    (transient-get-suffix 'majutsu-git-clone-transient "-R")))
           (obj (make-instance 'transient-option :argument "--remote=")))
      (majutsu-jj-integration-commit source "source commit")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "fresh-origin")))
        (oset obj value (funcall reader "Remote: " nil nil)))
      (majutsu-jj-integration-call
       sandbox "git" "clone" (transient-infix-value obj) source destination)
      (should (equal (majutsu-remote-names destination)
                     '("fresh-origin"))))))

(ert-deftest majutsu-git-clone-remote/rejects-invalid-new-names ()
  (dolist (name '("git" "bad/name"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) name)))
      (should-error (majutsu-git-clone--read-remote "Remote: " nil nil)
                    :type 'user-error))))


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
  (cl-letf (((symbol-function 'majutsu-run-jj)
             (lambda (&rest _args)
               (ert-fail "Missing remote must be rejected before running jj"))))
    (should-error
     (majutsu-git-remote-set-url '("--fetch=https://example.invalid/fetch.git"))
     :type 'user-error)))

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

(provide 'majutsu-git-test)
;;; majutsu-git-test.el ends here
