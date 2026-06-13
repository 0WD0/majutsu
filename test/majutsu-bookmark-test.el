;;; majutsu-bookmark-test.el --- Tests for bookmark helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for bookmark parsing and transient argument behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'majutsu-bookmark)

(ert-deftest majutsu-bookmark-split-remote-ref/basic ()
  (should (equal (majutsu--bookmark-split-remote-ref "main@origin")
                 '("main" . "origin"))))

(ert-deftest majutsu-bookmark-split-remote-ref/last-at ()
  (should (equal (majutsu--bookmark-split-remote-ref "foo@bar@origin")
                 '("foo@bar" . "origin"))))

(ert-deftest majutsu-bookmark-split-remote-ref/no-remote ()
  (should (equal (majutsu--bookmark-split-remote-ref "main")
                 '("main" . nil))))

(ert-deftest majutsu-bookmark-get-bookmark-names/local-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main" "feature"))))
      (should (equal (majutsu--get-bookmark-names 'local) '("main" "feature")))
      (should (equal (seq-take seen-args 3) '("bookmark" "list" "--quiet")))
      (should-not (member "--all-remotes" seen-args))
      (should-not (member "--tracked" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "!remote") template))
        (should (string-match-p (regexp-quote "present") template))
        (should (string-match-p (regexp-quote "\\n") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main@origin" "dev@upstream"))))
      (should (equal (majutsu--get-bookmark-names 'remote) '("main@origin" "dev@upstream")))
      (should (member "--all-remotes" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "remote && present") template))
        (should (string-match-p (regexp-quote "\"@\"") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-tracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-tracked) '("main@origin")))
      (should (member "--tracked" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "tracked") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-untracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("topic@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-untracked) '("topic@origin")))
      (should (member "--all-remotes" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "!tracked") template))))))

(ert-deftest majutsu-bookmark-list-args/uses-structured-template ()
  (let ((majutsu-bookmark--list-all nil))
    (let ((args (majutsu-bookmark--list-args)))
      (should (equal (seq-take args 3) '("bookmark" "list" "--quiet")))
      (should (member "-T" args))
      (should (equal (cadr (member "-T" args)) majutsu-bookmark--list-template))
      (should-not (member "--all-remotes" args)))))

(ert-deftest majutsu-bookmark-list-args/all-remotes ()
  (let ((majutsu-bookmark--list-all t))
    (should (member "--all-remotes" (majutsu-bookmark--list-args)))))

(ert-deftest majutsu-bookmark-list-mode-map/actions-bound ()
  (let ((bindings '(("RET" . majutsu-bookmark-visit-target)
                    ("n" . majutsu-bookmark-new)
                    ("c" . majutsu-bookmark-create)
                    ("f" . majutsu-bookmark-forget)
                    ("d" . majutsu-bookmark-delete)
                    ("r" . majutsu-bookmark-rename)
                    ("m" . majutsu-bookmark-move)
                    ("M" . majutsu-bookmark-move-allow-backwards)
                    ("a" . majutsu-bookmark-advance-patterns)
                    ("p" . majutsu-bookmark-advance-patterns)
                    ("t" . majutsu-bookmark-track)
                    ("u" . majutsu-bookmark-untrack)
                    ("A" . majutsu-bookmark-list-toggle-all-remotes))))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (dolist (binding bindings)
        (should (eq (key-binding (kbd (car binding))) (cdr binding)))))))

(ert-deftest majutsu-bookmark-define-list-mode-keys/updates-existing-map ()
  (let ((majutsu-bookmark-list-mode-map (make-sparse-keymap)))
    (majutsu-bookmark--define-list-mode-keys)
    (should (eq (lookup-key majutsu-bookmark-list-mode-map (kbd "RET"))
                'majutsu-bookmark-visit-target))
    (should (eq (lookup-key majutsu-bookmark-list-mode-map (kbd "n"))
                'majutsu-bookmark-new))
    (should (eq (lookup-key majutsu-bookmark-list-mode-map (kbd "c"))
                'majutsu-bookmark-create))
    (should (eq (lookup-key majutsu-bookmark-list-mode-map (kbd "f"))
                'majutsu-bookmark-forget))
    (should (eq (lookup-key majutsu-bookmark-list-mode-map (kbd "s"))
                'undefined))))

(ert-deftest majutsu-bookmark-parse-list-output/basic ()
  (let* ((sep majutsu-bookmark--list-field-separator)
         (line (string-join '("main" "" "1" "0" "0" "0" "1"
                              "kzsquqlr" "32730a15" "Arthur Heymans"
                              "2 weeks ago" "Add libgfxinit support" "" "")
                            sep)))
    (should (equal (majutsu-bookmark-parse-list-output (concat line "\n"))
                   '((:name "main"
                      :remote nil
                      :present t
                      :conflict nil
                      :tracked nil
                      :tracking-present nil
                      :synced t
                      :change-id "kzsquqlr"
                      :commit-id "32730a15"
                      :author "Arthur Heymans"
                      :age "2 weeks ago"
                      :description "Add libgfxinit support"
                      :ahead nil
                      :behind nil))))))

(ert-deftest majutsu-bookmark-parse-list-output/remote-tracked ()
  (let* ((sep majutsu-bookmark--list-field-separator)
         (line (string-join '("main" "origin" "1" "0" "1" "1" "0"
                              "kzsquqlr" "32730a15" "Arthur Heymans"
                              "2 weeks ago" "Add libgfxinit support" "2" "1")
                            sep))
         (entry (car (majutsu-bookmark-parse-list-output line))))
    (should (equal (plist-get entry :remote) "origin"))
    (should (plist-get entry :tracked))
    (should (equal (majutsu-bookmark--entry-ref entry) "main@origin"))
    (should (equal (majutsu-bookmark--entry-state entry) "+2/-1"))))

(ert-deftest majutsu-bookmark-default-target-revset/selected-entry-without-target-errors ()
  (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
             (lambda () '(:name "gone" :present nil))))
    (should-error (majutsu-bookmark--default-target-revset)
                  :type 'user-error)))

(ert-deftest majutsu-bookmark-new/opens-log-after-creating-child ()
  (let (called log-target)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :present t :change-id "kzsquqlr")))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'majutsu-log-setup-buffer)
               (lambda (&optional commit _locked)
                 (setq log-target commit)))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-new)
      (should (equal called '("new" "kzsquqlr")))
      (should (equal log-target "@")))))

(ert-deftest majutsu-bookmark-delete/at-local-bookmark-deletes-point ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :present t)))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-delete)
      (should (equal called '("bookmark" "delete" "main"))))))

(ert-deftest majutsu-bookmark-rename/at-local-bookmark-prompts-only-new-name ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :present t)))
              ((symbol-function 'majutsu-read-string)
               (lambda (&rest _args) "trunk"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (call-interactively #'majutsu-bookmark-rename)
      (should (equal called '("bookmark" "rename" "main" "trunk"))))))

(ert-deftest majutsu-bookmark-move/at-local-bookmark-prompts-only-target ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :present t)))
              ((symbol-function 'majutsu-read-revset)
               (lambda (&rest _args) "@"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (call-interactively #'majutsu-bookmark-move)
      (should (equal called '("bookmark" "move" "-t" "@" "main"))))))

(ert-deftest majutsu-bookmark-read-advance-patterns/at-local-bookmark-uses-point ()
  (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
             (lambda () '(:name "main" :present t))))
    (should (equal (majutsu--bookmark-read-advance-patterns) '("main")))))

(ert-deftest majutsu-bookmark-track/at-remote-bookmark-tracks-point ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :remote "origin" :present t)))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-track)
      (should (equal called '("bookmark" "track" "main" "--remote" "origin"))))))

(ert-deftest majutsu-bookmark-track/reads-patterns-and-calls-jj ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu--bookmark-remote-name-candidates)
               (lambda () '("main" "dev")))
              ((symbol-function 'majutsu--bookmark-git-remote-candidates)
               (lambda () '("origin" "upstream")))
              ((symbol-function 'completing-read-multiple)
               (lambda (prompt _collection &rest _args)
                 (cond
                  ((string-match-p "Track bookmark name" prompt)
                   '("main" "glob:\"feat*\""))
                  ((string-match-p "Remote.*pattern" prompt)
                   '("origin" "upstream"))
                  (t nil))))
              ((symbol-function 'majutsu-call-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-track)
      (should (equal called
                     '("bookmark" "track"
                       "main" "glob:\"feat*\""
                       "--remote" "origin"
                       "--remote" "upstream"))))))

(ert-deftest majutsu-bookmark-untrack/at-remote-bookmark-untracks-point ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :remote "origin" :present t :tracked t)))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (call-interactively #'majutsu-bookmark-untrack)
      (should (equal called '("bookmark" "untrack" "main" "--remote" "origin"))))))

(ert-deftest majutsu-bookmark-untrack/at-untracked-remote-bookmark-prompts ()
  (let (called prompts)
    (cl-letf (((symbol-function 'majutsu-bookmark--entry-at-point)
               (lambda () '(:name "main" :remote "origin" :present t :tracked nil)))
              ((symbol-function 'majutsu--bookmark-remote-name-candidates)
               (lambda () '("main" "dev")))
              ((symbol-function 'majutsu--bookmark-git-remote-candidates)
               (lambda () '("origin" "upstream")))
              ((symbol-function 'majutsu-completing-read-multiple)
               (lambda (prompt _collection &rest _args)
                 (push prompt prompts)
                 (cond
                  ((string-match-p "Untrack bookmark" prompt) '("dev"))
                  ((string-match-p "Remote" prompt) '("upstream"))
                  (t nil))))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (call-interactively #'majutsu-bookmark-untrack)
      (should (equal called '("bookmark" "untrack" "dev" "--remote" "upstream")))
      (should (equal (length prompts) 2)))))

(ert-deftest majutsu-bookmark-untrack/builds-remote-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-call-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-untrack '("main" "glob:\"ci/*\"") '("origin" "upstream"))
      (should (equal called
                     '("bookmark" "untrack"
                       "main" "glob:\"ci/*\""
                       "--remote" "origin"
                       "--remote" "upstream"))))))

;;; majutsu-bookmark-test.el ends here
