;;; majutsu-bookmark-test.el -*- lexical-binding: t; -*-

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
