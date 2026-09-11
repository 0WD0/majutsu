;;; majutsu-forge-test.el --- Tests for Forge integration glue  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for optional Majutsu/Forge integration helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-forge)

(ert-deftest majutsu-forge-section-hooks/adds-and-removes-default-hooks ()
  (let ((original (default-value 'majutsu-log-sections-hook))
        (majutsu-forge--sections-installed nil)
        (majutsu-forge--installed-section-hooks nil))
    (unwind-protect
        (progn
          (set-default 'majutsu-log-sections-hook '(majutsu-log-insert-logs))
          (majutsu-forge--add-section-hooks)
          (should (equal (default-value 'majutsu-log-sections-hook)
                         '(majutsu-log-insert-logs
                           majutsu-forge--clear-section-errors
                           majutsu-forge-insert-pullreqs
                           majutsu-forge-insert-issues
                           majutsu-forge-insert-discussions)))
          (majutsu-forge--add-section-hooks)
          (should (equal (default-value 'majutsu-log-sections-hook)
                         '(majutsu-log-insert-logs
                           majutsu-forge--clear-section-errors
                           majutsu-forge-insert-pullreqs
                           majutsu-forge-insert-issues
                           majutsu-forge-insert-discussions)))
          (majutsu-forge--remove-section-hooks)
          (should (equal (default-value 'majutsu-log-sections-hook)
                         '(majutsu-log-insert-logs))))
      (set-default 'majutsu-log-sections-hook original))))

(ert-deftest majutsu-forge-section-hooks/rolls-back-partial-installation ()
  "An error midway through section setup should leave no installed prefix."
  (let ((original (default-value 'majutsu-log-sections-hook))
        (real-add (symbol-function 'magit-add-section-hook))
        (majutsu-forge--sections-installed nil)
        (majutsu-forge--installed-section-hooks nil))
    (unwind-protect
        (progn
          (set-default 'majutsu-log-sections-hook '(majutsu-log-insert-logs))
          (cl-letf (((symbol-function 'magit-add-section-hook)
                     (lambda (hook function &rest args)
                       (if (eq function 'majutsu-forge-insert-issues)
                           (error "section setup failed")
                         (apply real-add hook function args)))))
            (should-error (majutsu-forge--add-section-hooks)))
          (should-not majutsu-forge--sections-installed)
          (should (equal (default-value 'majutsu-log-sections-hook)
                         '(majutsu-log-insert-logs
                           majutsu-forge--clear-section-errors
                           majutsu-forge-insert-pullreqs)))
          (majutsu-forge--cleanup-installation)
          (should (equal (default-value 'majutsu-log-sections-hook)
                         '(majutsu-log-insert-logs)))
          (should-not majutsu-forge--installed-section-hooks))
      (majutsu-forge--cleanup-installation)
      (set-default 'majutsu-log-sections-hook original))))

(ert-deftest majutsu-forge-git-worktree/uses-jj-git-root-parent ()
  (let ((root (make-temp-file "majutsu-forge-git-" t)))
    (unwind-protect
        (let ((default-directory root))
          (make-directory (expand-file-name ".git" root))
          (cl-letf (((symbol-function 'majutsu-jj-lines)
                     (lambda (&rest _) (list (expand-file-name ".git" root))))
                    ((symbol-function 'majutsu-jj-expand-directory-from-jj)
                     (lambda (path &optional _) (file-name-as-directory path))))
            (should (equal (majutsu-forge--git-worktree)
                           (file-name-as-directory root)))))
      (delete-directory root t))))

(ert-deftest majutsu-forge-buffer-setup/uses-git-worktree ()
  (let ((real-featurep (symbol-function 'featurep))
        (git-root "/git-worktree/")
        directories)
    (with-temp-buffer
      (let ((default-directory "/jj-workspace/"))
        (cl-letf (((symbol-function 'featurep)
                   (lambda (feature)
                     (or (eq feature 'forge) (funcall real-featurep feature))))
                  ((symbol-function 'majutsu-forge--git-worktree)
                   (lambda () git-root))
                  ((symbol-function 'forge--init-buffer-topics-spec)
                   (lambda () (push default-directory directories)))
                  ((symbol-function 'forge-set-buffer-repository)
                   (lambda () (push default-directory directories))))
          (majutsu-forge--init-buffer)))
    (should (equal directories (list git-root git-root))))))

(ert-deftest majutsu-forge-dispatch/uses-git-worktree ()
  (let ((git-root "/git-worktree/")
        directory)
    (with-temp-buffer
      (let ((default-directory "/jj-workspace/"))
        (cl-letf (((symbol-function 'majutsu-forge--require) (lambda () t))
                  ((symbol-function 'majutsu-forge--git-worktree)
                   (lambda () git-root))
                  ((symbol-function 'forge-dispatch)
                   (lambda () (interactive) (setq directory default-directory))))
          (majutsu-forge-dispatch)))
    (should (equal directory git-root)))))

(ert-deftest majutsu-forge-insert-pullreq-commits/uses-placeholder ()
  (with-temp-buffer
    (majutsu-forge--insert-pullreq-commits nil)
    (should (string-match-p "Commit list is not shown"
                            (buffer-string)))))

(ert-deftest majutsu-forge-pullreq-commit-advice/suppresses-in-majutsu-log ()
  (cl-letf (((symbol-function 'forge--insert-pullreq-commits)
             (lambda (&rest _)
               (insert "git log body\n"))))
    (unwind-protect
        (progn
          (majutsu-forge--add-advices)
          (with-temp-buffer
            (majutsu-log-mode)
            (let ((majutsu-forge-suppress-pullreq-commits t))
              (forge--insert-pullreq-commits nil))
            (should (string-match-p "Commit list is not shown"
                                    (buffer-string)))
            (should-not (string-match-p "git log body"
                                        (buffer-string)))))
      (majutsu-forge--remove-advices))))

(ert-deftest majutsu-forge-pullreq-commit-advice/keeps-forge-outside-majutsu ()
  (cl-letf (((symbol-function 'forge--insert-pullreq-commits)
             (lambda (&rest _)
               (insert "git log body\n"))))
    (unwind-protect
        (progn
          (majutsu-forge--add-advices)
          (with-temp-buffer
            (let ((majutsu-forge-suppress-pullreq-commits t))
              (forge--insert-pullreq-commits nil))
            (should (equal (buffer-string) "git log body\n"))))
      (majutsu-forge--remove-advices))))

(ert-deftest majutsu-forge-refresh-advice/explicit-buffer-refreshes-once ()
  "Forge's recursive explicit-BUFFER dispatch should trigger one refresh."
  (let ((majutsu-forge--installed-advices nil)
        (majutsu-forge--refreshing nil)
        (majutsu-forge--pending-refresh-roots nil)
        (refreshes 0)
        roots
        (target (generate-new-buffer " *majutsu-forge-target*")))
    (unwind-protect
        (cl-letf (((symbol-function 'forge-refresh-buffer)
                   (lambda (&optional buffer)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (forge-refresh-buffer))))))
          (unwind-protect
              (progn
                (majutsu-forge--add-advices)
                (with-temp-buffer
                  (cl-letf (((symbol-function 'majutsu-forge--buffer-root)
                             (lambda (&optional buffer)
                               (should-not buffer)
                               (should (eq (current-buffer) target))
                               "/repo/"))
                            ((symbol-function
                              'majutsu-forge--refresh-majutsu-buffers)
                             (lambda (&optional root)
                               (push root roots)
                               (cl-incf refreshes))))
                    (forge-refresh-buffer target)))
                (should (= refreshes 1))
                (should (equal roots '("/repo/"))))
            (majutsu-forge--remove-advices)))
      (when (buffer-live-p target)
        (kill-buffer target)))))

(ert-deftest majutsu-forge-refresh-advice/coalesces-reentrant-refreshes ()
  "Nested Forge refreshes should cause at most one supplemental pass."
  (let ((majutsu-forge--installed-advices nil)
        (majutsu-forge--refreshing nil)
        (majutsu-forge--pending-refresh-roots nil)
        (refreshes 0))
    (cl-letf (((symbol-function 'forge-refresh-buffer) #'ignore)
              ((symbol-function 'majutsu-forge--buffer-root)
               (lambda (&optional _buffer) "/repo/"))
              ((symbol-function 'majutsu-forge--refresh-majutsu-buffers)
               (lambda (&optional _root)
                 (cl-incf refreshes)
                 ;; Queue multiple requests in both the initial and
                 ;; supplemental passes.  Only the initial queue is drained.
                 (forge-refresh-buffer)
                 (forge-refresh-buffer))))
      (unwind-protect
          (progn
            (majutsu-forge--add-advices)
            (forge-refresh-buffer)
            (should (= refreshes 2))
            (should-not majutsu-forge--refreshing)
            (should-not majutsu-forge--pending-refresh-roots))
        (majutsu-forge--remove-advices)))))

(ert-deftest majutsu-forge-refresh/pending-nil-means-refresh-all ()
  "A queued all-buffer request should get one complete supplemental pass."
  (let ((majutsu-forge--refreshing nil)
        (majutsu-forge--pending-refresh-roots nil)
        (refreshes 0))
    (with-temp-buffer
      (let ((target (current-buffer)))
        (cl-letf (((symbol-function 'buffer-list) (lambda () (list target)))
                  ((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'majutsu-refresh-buffer)
                   (lambda ()
                     (cl-incf refreshes)
                     (majutsu-forge--request-majutsu-refresh nil))))
          (majutsu-forge--request-majutsu-refresh nil))))
    (should (= refreshes 2))
    (should-not majutsu-forge--refreshing)
    (should-not majutsu-forge--pending-refresh-roots)))

(ert-deftest majutsu-forge-cleanup/preserves-preexisting-hooks-and-advices ()
  "Cleanup should only remove entries installed by the current mode run."
  (let ((original-sections (default-value 'majutsu-log-sections-hook))
        (original-log-mode (default-value 'majutsu-log-mode-hook))
        (original-refresh (default-value 'majutsu-refresh-buffer-hook))
        (majutsu-forge--sections-installed nil)
        (majutsu-forge--installed-section-hooks nil)
        (majutsu-forge--installed-hooks nil)
        (majutsu-forge--installed-advices nil)
        (majutsu-forge--bindings-installed nil)
        (majutsu-forge--saved-bindings nil))
    (cl-letf (((symbol-function 'forge-refresh-buffer) #'ignore)
              ((symbol-function 'forge--insert-pullreq-commits) #'ignore))
      (unwind-protect
          (progn
            (set-default 'majutsu-log-sections-hook
                         '(majutsu-log-insert-logs
                           majutsu-forge-insert-pullreqs))
            (set-default 'majutsu-log-mode-hook
                         '(majutsu-forge--init-buffer))
            (set-default 'majutsu-refresh-buffer-hook
                         '(majutsu-forge--expand-sections))
            ;; Model identical advice installed independently before the mode.
            (advice-add 'forge-refresh-buffer :after
                        #'majutsu-forge--after-forge-refresh)
            (advice-add 'forge--insert-pullreq-commits :around
                        #'majutsu-forge--insert-pullreq-commits-around)
            (majutsu-forge--add-section-hooks)
            (majutsu-forge--add-owned-hook
             'majutsu-log-mode-hook #'majutsu-forge--connect-database-once)
            (majutsu-forge--add-owned-hook
             'majutsu-log-mode-hook #'majutsu-forge--init-buffer)
            (majutsu-forge--add-owned-hook
             'majutsu-refresh-buffer-hook #'majutsu-forge--expand-sections)
            (majutsu-forge--add-advices)
            (majutsu-forge--cleanup-installation)
            (should (equal (default-value 'majutsu-log-sections-hook)
                           '(majutsu-log-insert-logs
                             majutsu-forge-insert-pullreqs)))
            (should (equal (default-value 'majutsu-log-mode-hook)
                           '(majutsu-forge--init-buffer)))
            (should (equal (default-value 'majutsu-refresh-buffer-hook)
                           '(majutsu-forge--expand-sections)))
            (should (advice-member-p #'majutsu-forge--after-forge-refresh
                                     'forge-refresh-buffer))
            (should (advice-member-p
                     #'majutsu-forge--insert-pullreq-commits-around
                     'forge--insert-pullreq-commits)))
        (advice-remove 'forge-refresh-buffer
                       #'majutsu-forge--after-forge-refresh)
        (advice-remove 'forge--insert-pullreq-commits
                       #'majutsu-forge--insert-pullreq-commits-around)
        (majutsu-forge--cleanup-installation)
        (set-default 'majutsu-log-sections-hook original-sections)
        (set-default 'majutsu-log-mode-hook original-log-mode)
        (set-default 'majutsu-refresh-buffer-hook original-refresh)))))

(ert-deftest majutsu-forge-mode-bindings/restore-previous-bindings ()
  (let ((original-n (keymap-lookup majutsu-mode-map "N"))
        (original-quote (keymap-lookup majutsu-mode-map "'")))
    (unwind-protect
        (progn
          (define-key majutsu-mode-map (key-parse "N") #'ignore)
          (define-key majutsu-mode-map (key-parse "'") nil)
          (majutsu-forge--add-mode-bindings)
          (should (eq (keymap-lookup majutsu-mode-map "N")
                      #'majutsu-forge-dispatch))
          (should (eq (keymap-lookup majutsu-mode-map "'")
                      #'majutsu-forge-dispatch))
          (majutsu-forge--remove-mode-bindings)
          (should (eq (keymap-lookup majutsu-mode-map "N") #'ignore))
          (should-not (keymap-lookup majutsu-mode-map "'")))
      (majutsu-forge--restore-bindings)
      (define-key majutsu-mode-map (key-parse "N") original-n)
      (define-key majutsu-mode-map (key-parse "'") original-quote)
      (setq majutsu-forge--saved-bindings nil
            majutsu-forge--bindings-installed nil))))

(ert-deftest majutsu-forge-mode-bindings/preserve-later-changes ()
  (let ((original-n (keymap-lookup majutsu-mode-map "N"))
        (original-quote (keymap-lookup majutsu-mode-map "'")))
    (unwind-protect
        (progn
          (define-key majutsu-mode-map (key-parse "N") #'ignore)
          (define-key majutsu-mode-map (key-parse "'") nil)
          (majutsu-forge--add-mode-bindings)
          (keymap-set majutsu-mode-map "N" #'next-line)
          (majutsu-forge--remove-mode-bindings)
          (should (eq (keymap-lookup majutsu-mode-map "N") #'next-line))
          (should-not (keymap-lookup majutsu-mode-map "'")))
      (majutsu-forge--restore-bindings)
      (define-key majutsu-mode-map (key-parse "N") original-n)
      (define-key majutsu-mode-map (key-parse "'") original-quote)
      (setq majutsu-forge--saved-bindings nil
            majutsu-forge--bindings-installed nil))))

(ert-deftest majutsu-forge-section-remaps/adds-and-restores ()
  (let* ((maps '(forge-pullreqs-section-map
                 forge-pullreq-section-map
                 forge-issues-section-map
                 forge-issue-section-map
                 forge-discussions-section-map
                 forge-discussion-section-map
                 forge-repository-section-map))
         (originals (mapcar (lambda (map)
                              (list map (boundp map)
                                    (and (boundp map) (symbol-value map))))
                            maps))
         (majutsu-forge--saved-bindings nil))
    (cl-letf (((symbol-function 'forge-list-pullreqs) #'ignore)
              ((symbol-function 'forge-visit-this-topic) #'ignore)
              ((symbol-function 'forge-list-issues) #'ignore)
              ((symbol-function 'forge-list-discussions) #'ignore)
              ((symbol-function 'forge-visit-this-repository) #'ignore))
      (unwind-protect
          (progn
            (dolist (map maps)
              (set map (make-sparse-keymap)))
            (keymap-set forge-pullreqs-section-map
                        "<remap> <majutsu-visit-thing>" #'ignore)
            (majutsu-forge--add-section-remaps)
            (should (eq (keymap-lookup forge-pullreqs-section-map
                                       "<remap> <majutsu-visit-thing>")
                        #'forge-list-pullreqs))
            (should (eq (keymap-lookup forge-pullreq-section-map
                                       "<remap> <majutsu-visit-thing>")
                        #'forge-visit-this-topic))
            (should (eq (keymap-lookup forge-issues-section-map
                                       "<remap> <majutsu-visit-thing>")
                        #'forge-list-issues))
            (should (eq (keymap-lookup forge-discussions-section-map
                                       "<remap> <majutsu-visit-thing>")
                        #'forge-list-discussions))
            (should (eq (keymap-lookup forge-repository-section-map
                                       "<remap> <majutsu-visit-thing>")
                        #'forge-visit-this-repository))
            (majutsu-forge--restore-bindings)
            (should (eq (keymap-lookup forge-pullreqs-section-map
                                       "<remap> <majutsu-visit-thing>")
                        #'ignore))
            (should-not (keymap-lookup forge-pullreq-section-map
                                       "<remap> <majutsu-visit-thing>")))
        (majutsu-forge--restore-bindings)
        (pcase-dolist (`(,map ,bound ,value) originals)
          (if bound
              (set map value)
            (makunbound map)))))))

(ert-deftest majutsu-forge-with-section-errors/suppresses-by-default ()
  (let ((majutsu-forge-show-section-errors nil)
        (majutsu-forge-section-errors nil))
    (with-temp-buffer
      (should-not
       (majutsu-forge--with-section-errors "test"
         (error "boom")))
      (should (equal majutsu-forge-section-errors
                     '(("test" . "boom")))))))

(ert-deftest majutsu-forge-with-section-errors/removes-partial-insert ()
  (let ((majutsu-forge-show-section-errors nil)
        (majutsu-forge-section-errors nil))
    (with-temp-buffer
      (insert "before\n")
      (should-not
       (majutsu-forge--with-section-errors "test"
         (insert "partial\n")
         (error "boom")))
      (should (equal (buffer-string) "before\n"))
      (should (equal majutsu-forge-section-errors
                     '(("test" . "boom")))))))

(ert-deftest majutsu-forge-section-errors/clears-stale-errors-on-render ()
  (let ((original (default-value 'majutsu-log-sections-hook))
        (original-sections-installed majutsu-forge--sections-installed))
    (unwind-protect
        (progn
          (set-default 'majutsu-log-sections-hook nil)
          (majutsu-forge--add-section-hooks)
          (with-temp-buffer
            (setq majutsu-forge-section-errors '(("old" . "stale")))
            (cl-letf (((symbol-function 'forge-insert-pullreqs)
                       (lambda () (error "boom")))
                      ((symbol-function 'forge-insert-issues) #'ignore)
                      ((symbol-function 'forge-insert-discussions) #'ignore)
                      ((symbol-function 'majutsu-forge--ensure-buffer) #'ignore))
              (run-hooks 'majutsu-log-sections-hook))
            (should (equal majutsu-forge-section-errors
                           '(("pull requests" . "boom"))))))
      (majutsu-forge--remove-section-hooks)
      (setq majutsu-forge--sections-installed original-sections-installed)
      (set-default 'majutsu-log-sections-hook original))))

(ert-deftest majutsu-forge-same-root-p/ignores-file-errors ()
  (cl-letf (((symbol-function 'file-truename)
             (lambda (_file) (error "missing"))))
    (should-not (majutsu-forge--same-root-p "/missing-a" "/missing-b"))))

(ert-deftest majutsu-forge-mode/cleans-up-after-enable-error ()
  (let ((original-mode majutsu-forge-mode)
        (original-sections-installed majutsu-forge--sections-installed)
        (original-bindings-installed majutsu-forge--bindings-installed)
        (original-log-sections (default-value 'majutsu-log-sections-hook))
        (original-log-mode-hook majutsu-log-mode-hook)
        (original-refresh-hook majutsu-refresh-buffer-hook)
        (majutsu-forge-add-default-sections t)
        (majutsu-forge-add-default-bindings nil)
        (advices-installed nil)
        (advices-removed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-forge--require) (lambda () t))
                  ((symbol-function 'majutsu-forge--add-advices)
                   (lambda () (setq advices-installed t)))
                  ((symbol-function 'majutsu-forge--remove-advices)
                   (lambda () (setq advices-removed t)))
                  ((symbol-function 'majutsu-forge--refresh-majutsu-buffers)
                   (lambda (&optional _root) (error "refresh failed"))))
          (setq majutsu-forge-mode nil
                majutsu-forge--sections-installed nil
                majutsu-forge--bindings-installed nil)
          (set-default 'majutsu-log-sections-hook '(majutsu-log-insert-logs))
          (setq majutsu-log-mode-hook nil
                majutsu-refresh-buffer-hook nil)
          (should-error (majutsu-forge-mode 1))
          (should advices-installed)
          (should advices-removed)
          (should-not majutsu-forge-mode)
          (should-not majutsu-forge--sections-installed)
          (should-not majutsu-forge--bindings-installed)
          (should-not (memq #'majutsu-forge--clear-section-errors
                            (default-value 'majutsu-log-sections-hook)))
          (should-not (memq #'majutsu-forge--connect-database-once
                            majutsu-log-mode-hook))
          (should-not (memq #'majutsu-forge--expand-sections
                            majutsu-refresh-buffer-hook)))
      (setq majutsu-forge-mode original-mode
            majutsu-forge--sections-installed original-sections-installed
            majutsu-forge--bindings-installed original-bindings-installed
            majutsu-log-mode-hook original-log-mode-hook
            majutsu-refresh-buffer-hook original-refresh-hook)
      (set-default 'majutsu-log-sections-hook original-log-sections))))

(ert-deftest majutsu-forge-mode/refreshes-on-enable-and-disable ()
  (let ((original-mode majutsu-forge-mode)
        (original-sections-installed majutsu-forge--sections-installed)
        (original-bindings-installed majutsu-forge--bindings-installed)
        (original-log-sections (default-value 'majutsu-log-sections-hook))
        (original-log-mode-hook majutsu-log-mode-hook)
        (original-refresh-hook majutsu-refresh-buffer-hook)
        (majutsu-forge-add-default-sections t)
        (majutsu-forge-add-default-bindings nil)
        (refreshes 0))
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-forge--require) (lambda () t))
                  ((symbol-function 'majutsu-forge--add-advices) #'ignore)
                  ((symbol-function 'majutsu-forge--remove-advices) #'ignore)
                  ((symbol-function 'majutsu-forge--refresh-majutsu-buffers)
                   (lambda (&optional _root) (cl-incf refreshes))))
          (setq majutsu-forge-mode nil
                majutsu-forge--sections-installed nil
                majutsu-forge--bindings-installed nil)
          (set-default 'majutsu-log-sections-hook '(majutsu-log-insert-logs))
          (setq majutsu-log-mode-hook nil
                majutsu-refresh-buffer-hook nil)
          (majutsu-forge-mode 1)
          (should (= refreshes 1))
          (should majutsu-forge--sections-installed)
          (should (memq #'majutsu-forge--clear-section-errors
                        (default-value 'majutsu-log-sections-hook)))
          (should (memq #'majutsu-forge--expand-sections
                        majutsu-refresh-buffer-hook))
          (majutsu-forge-mode -1)
          (should (= refreshes 2))
          (should-not majutsu-forge--sections-installed)
          (should-not (memq #'majutsu-forge--clear-section-errors
                            (default-value 'majutsu-log-sections-hook)))
          (should-not (memq #'majutsu-forge--expand-sections
                            majutsu-refresh-buffer-hook)))
      (setq majutsu-forge-mode original-mode
            majutsu-forge--sections-installed original-sections-installed
            majutsu-forge--bindings-installed original-bindings-installed
            majutsu-log-mode-hook original-log-mode-hook
            majutsu-refresh-buffer-hook original-refresh-hook)
      (set-default 'majutsu-log-sections-hook original-log-sections))))

(provide 'majutsu-forge-test)
;;; majutsu-forge-test.el ends here
