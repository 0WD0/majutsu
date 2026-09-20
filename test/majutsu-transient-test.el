;;; majutsu-transient-test.el --- Scoped defaults regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'majutsu-core)
(require 'majutsu-log)
(require 'majutsu-git)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-defaults/shared-repo-and-workspace-inheritance ()
  "Real additional workspaces share repo defaults but not workspace overrides."
  (majutsu-jj-integration-with-sandbox sandbox
    (setenv "XDG_CONFIG_HOME" (expand-file-name "config" sandbox))
    (let* ((a (majutsu-jj-integration-init sandbox "a"))
           (b (expand-file-name "b" sandbox))
           (majutsu-jj-executable majutsu-jj-integration--jj)
           (transient-values nil)
           (transient-values-file (expand-file-name "values.el" sandbox))
           (mode (make-symbol "majutsu-test-mode")))
      (majutsu-jj-integration-call a "workspace" "add" "--name" "b" b)
      (put mode 'builtin '("builtin"))
      (cl-labels ((value (root)
                    (with-temp-buffer
                      (setq-local majutsu--default-directory root)
                      (majutsu-transient-default-value 'test mode 'builtin)))
                  (save (root scope args)
                    (with-temp-buffer
                      (setq-local majutsu--default-directory root)
                      (majutsu-transient-save-default-value 'test mode args scope)))
                  (clear (root scope)
                    (with-temp-buffer
                      (setq-local majutsu--default-directory root)
                      (majutsu-transient-clear-default-value 'test mode scope))))
        ;; Neither reading nor clearing absent defaults creates jj identities.
        (should (equal (value b) '("builtin")))
        (clear b 'workspace)
        (should-not (file-exists-p (expand-file-name ".jj/workspace-config-id" b)))
        (should-not (file-exists-p (expand-file-name ".jj/repo/config-id" a)))
        (save a 'user '("user"))
        (save b 'repo '("repo"))
        (should (equal (value a) '("repo")))
        (should (equal (value b) '("repo")))
        (save a 'workspace '("workspace-a"))
        (save b 'workspace nil)
        (should (equal (value a) '("workspace-a")))
        (should-not (value b))
        ;; Re-read the serialized file, not the in-memory alist.
        (setq transient-values
              (with-temp-buffer
                (insert-file-contents transient-values-file)
                (read (current-buffer))))
        (should (equal (value a) '("workspace-a")))
        (should-not (value b))
        (save b 'user '("new-user"))
        (should-not (value b))
        (clear b 'workspace)
        (should (equal (value b) '("repo")))
        (clear a 'repo)
        (should (equal (value b) '("new-user")))
        (should (equal (value a) '("workspace-a")))
        ;; Identity follows a workspace rename/move, not its path/name.
        (majutsu-jj-integration-call a "workspace" "rename" "renamed")
        (let ((moved (expand-file-name "moved" sandbox)))
          (rename-file a moved)
          (should (equal (value moved) '("workspace-a"))))))))

(ert-deftest majutsu-defaults/session-does-not-depend-on-config-id ()
  "A session override stays workspace-local before and after identity creation."
  (let* ((root (make-temp-file "majutsu-defaults-" t))
         (a (expand-file-name "a" root))
         (b (expand-file-name "b" root))
         (mode (make-symbol "majutsu-test-mode"))
         (transient-values nil))
    (unwind-protect
        (progn
          (make-directory a)
          (make-directory b)
          (put mode 'builtin '("builtin"))
          (with-temp-buffer
            (setq-local majutsu--default-directory a)
            (majutsu-transient-set-current-value 'test mode nil)
            (should-not (majutsu-transient-default-value 'test mode 'builtin))
            (make-directory (expand-file-name ".jj" a))
            (with-temp-file (expand-file-name ".jj/workspace-config-id" a)
              (insert "0123456789abcdefabcd"))
            (should-not (majutsu-transient-default-value 'test mode 'builtin)))
          (with-temp-buffer
            (setq-local majutsu--default-directory b)
            (should (equal (majutsu-transient-default-value 'test mode 'builtin)
                           '("builtin")))))
      (delete-directory root t))))

(ert-deftest majutsu-defaults/legacy-saved-keys-remain-readable ()
  "Existing user/repo records work without migration, including empty values."
  (let* ((root (make-temp-file "majutsu-defaults-" t))
         (mode (make-symbol "majutsu-test-mode"))
         (transient-values
          '((test:majutsu-test-mode "user")
            (test:majutsu-test-mode:repo:0123456789abcdefabcd))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".jj/repo" root) t)
          (with-temp-file (expand-file-name ".jj/repo/config-id" root)
            (insert "0123456789abcdefabcd"))
          (with-temp-buffer
            (setq-local majutsu--default-directory root)
            (should-not (majutsu-transient-default-value 'test mode nil))
            (setq transient-values (cdr (reverse transient-values)))
            (should (equal (majutsu-transient-default-value 'test mode nil)
                           '("user")))))
      (delete-directory root t))))

(ert-deftest majutsu-defaults/stacked-prefix-keeps-unsaved-edits ()
  "Resuming from a sub-prefix must honor Transient's explicit :value."
  (let ((transient-values nil))
    (with-temp-buffer
      (majutsu-log-mode)
      (setq-local majutsu-buffer-log-args '("--limit=12"))
      (let* ((raw '(("--" "src") "--no-graph"))
             (transient--prefix
              (transient--init-prefix 'majutsu-log-transient (list :value raw)))
             (transient--suffixes
              (transient--flatten-suffixes
               (transient--init-suffixes 'majutsu-log-transient))))
        (should (equal (majutsu-filesets-split-transient-value (transient-get-value))
                       '(("--no-graph") ("src"))))
        (should (equal (majutsu-transient--savable-value transient--prefix)
                       '("--no-graph")))))))

(ert-deftest majutsu-defaults/native-save-honors-unsavable ()
  "Native save keys respect unsavable infixes and do not execute a Git command."
  (let* ((root (make-temp-file "majutsu-defaults-" t))
         (transient-values nil)
         (transient-history nil)
         (transient-values-file (expand-file-name "values.el" root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu--default-directory root)
          (let* ((transient--prefix
                  (transient--init-prefix
                   'majutsu-git-push-transient
                   '(:value ("--remote=origin" "--tracked" "--bookmark=topic"))))
                 (transient--suffixes
                  (transient--flatten-suffixes
                   (transient--init-suffixes 'majutsu-git-push-transient)))
                 (transient-current-command 'majutsu-git-push-transient)
                 (transient-current-suffixes transient--suffixes))
            ;; A user can mark even a normally remembered option unsavable.
            (oset (seq-find (lambda (obj)
                              (and (slot-exists-p obj 'argument)
                                   (equal (oref obj argument) "--remote=")))
                            transient--suffixes)
                  unsavable t)
            (cl-letf (((symbol-function 'majutsu-run-jj-async)
                       (lambda (&rest _) (ert-fail "Saving executed Git"))))
              (transient-save-value transient--prefix))
            (should (equal (majutsu-transient-default-value
                            'majutsu-git 'majutsu-git-push nil)
                           '("--tracked")))))
      (delete-directory root t))))

(ert-deftest majutsu-defaults/buffer-reuse-is-workspace-local ()
  "A view in another workspace must not mask this workspace's defaults."
  (let ((source (generate-new-buffer " *majutsu-source*"))
        (transient-values '((majutsu-log:majutsu-log-mode "--no-graph"))))
    (unwind-protect
        (progn
          (with-current-buffer source
            (majutsu-log-mode)
            (setq-local majutsu--default-directory "/tmp/majutsu-workspace-a/")
            (setq-local majutsu-buffer-log-args '("--reversed")))
          (with-temp-buffer
            (setq-local majutsu--default-directory "/tmp/majutsu-workspace-b/")
            (should (equal (majutsu-log--get-value 'majutsu-log-mode 'always)
                           '(("--no-graph") nil)))
            (setq-local majutsu--default-directory "/tmp/majutsu-workspace-a/")
            (should (equal (majutsu-log--get-value 'majutsu-log-mode 'always)
                           '(("--reversed") nil)))))
      (kill-buffer source))))

(provide 'majutsu-transient-test)
;;; majutsu-transient-test.el ends here
