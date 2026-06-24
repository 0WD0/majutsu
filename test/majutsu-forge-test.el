;;; majutsu-forge-test.el --- Tests for Forge integration glue  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for optional Majutsu/Forge integration helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-forge)

(ert-deftest majutsu-forge-section-hooks/adds-and-removes-default-hooks ()
  (let ((original (default-value 'majutsu-log-sections-hook)))
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
