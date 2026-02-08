;;; majutsu-annotate-test.el --- Tests for annotate helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc

;;; Commentary:

;; Tests for annotate navigation helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-annotate)

(ert-deftest majutsu-annotate-file-exists-p/uses-exact-path-match ()
  "Return non-nil only when jj output exactly matches FILE."
  (let (captured-path)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq captured-path (nth 4 args))
                 '("src/file.el"))))
      (should (majutsu-annotate--file-exists-p "rev" "src/file.el"))
      (should (equal captured-path (majutsu-jj-fileset-quote "src/file.el"))))))

(ert-deftest majutsu-annotate-file-exists-p/rejects-prefix-or-warning-output ()
  "Return nil when jj output does not contain an exact path match."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args) '("src/file.el.bak"))))
    (should-not (majutsu-annotate--file-exists-p "rev" "src/file.el")))
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args) '("Warning: No matching entries"))))
    (should-not (majutsu-annotate--file-exists-p "rev" "src/file.el"))))

(ert-deftest majutsu-annotate-visit-other-file/stops-when-target-file-missing ()
  "Do not open a blob buffer if parent revision doesn't contain FILE."
  (with-temp-buffer
    (let ((chunk (make-majutsu-annotate-chunk :prev-rev "abc123" :orig-line 1))
          find-file-called)
      (setq-local majutsu-buffer-blob-path "src/file.el")
      (cl-letf (((symbol-function 'majutsu-annotate-current-chunk)
                 (lambda () chunk))
                ((symbol-function 'majutsu-annotate--file-exists-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'majutsu-find-file)
                 (lambda (&rest _args)
                   (setq find-file-called t))))
        (should-error (majutsu-annotate-visit-other-file) :type 'user-error)
        (should-not find-file-called)))))

(provide 'majutsu-annotate-test)
;;; majutsu-annotate-test.el ends here
