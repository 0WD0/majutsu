;;; majutsu-ediff-test.el --- Tests for majutsu-ediff -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for ediff integration.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst majutsu-ediff-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name
         (locate-library "majutsu-ediff.el"))))))

(when majutsu-ediff-test--root
  (add-to-list 'load-path majutsu-ediff-test--root)
  (load (expand-file-name "majutsu-ediff.el" majutsu-ediff-test--root) nil t))

(require 'majutsu-ediff)

;;; Tests

(ert-deftest majutsu-ediff-test-parse-diff-range-revisions ()
  "Test parsing --revisions= format."
  (let ((result (majutsu-ediff--parse-diff-range '("--revisions=abc"))))
    (should (equal (car result) "abc-"))
    (should (equal (cdr result) "abc"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-from-to ()
  "Test parsing --from/--to format."
  (let ((result (majutsu-ediff--parse-diff-range '("--from=foo" "--to=bar"))))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-from-only ()
  "Test parsing --from only."
  (let ((result (majutsu-ediff--parse-diff-range '("--from=foo"))))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "@"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-to-only ()
  "Test parsing --to only."
  (let ((result (majutsu-ediff--parse-diff-range '("--to=bar"))))
    (should (equal (car result) "@-"))
    (should (equal (cdr result) "bar"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-nil ()
  "Test parsing nil range."
  (let ((result (majutsu-ediff--parse-diff-range nil)))
    (should (null result))))

(ert-deftest majutsu-ediff-test-parse-diff-range-empty ()
  "Test parsing empty range."
  (let ((result (majutsu-ediff--parse-diff-range '())))
    (should (null result))))

(ert-deftest majutsu-ediff-test-parse-diff-range-short-r ()
  "Test parsing -r format."
  (let ((result (majutsu-ediff--parse-diff-range '("-rxyz"))))
    (should (equal (car result) "xyz-"))
    (should (equal (cdr result) "xyz"))))

(ert-deftest majutsu-ediff-test-edit-range-default-fallback ()
  "When no args/context are available, diffedit range defaults to @-..@."
  (with-temp-buffer
    (let ((range (majutsu-ediff--edit-range nil)))
      (should (equal (car range) "@-"))
      (should (equal (cdr range) "@")))))

(ert-deftest majutsu-ediff-test-build-diffedit-args-with-file ()
  "Diffedit args should include a fileset path when file is selected."
  (should (equal (majutsu-ediff--build-diffedit-args "foo" "bar" "src/x.el")
                 '("--from" "foo" "--to" "bar" "--" "src/x.el"))))

(ert-deftest majutsu-ediff-test-editor-command-config ()
  "Editor command config should use explicit command words."
  (let ((config (majutsu-ediff--editor-command-config
                 "ui.diff-editor"
                 "$right/docs/majutsu.org"
                 '("emacsclient" "--socket-name=/tmp/editor.sock"))))
    (should (string-prefix-p "ui.diff-editor=[" config))
    (should (string-match-p "emacsclient" config))
    (should (string-match-p "--socket-name=/tmp/editor.sock" config))
    (should-not (string-match-p "\\bsh\\b" config))
    (should-not (string-match-p "\\beval\\b" config))
    (should (string-match-p "\\$right/docs/majutsu.org" config))))

(ert-deftest majutsu-ediff-test-editor-command-from-env ()
  "Editor command should be parsed from with-editor env var value."
  (let* ((majutsu-with-editor-envvar "JJ_EDITOR")
         (process-environment
          (cons "JJ_EDITOR='emacsclient' --socket-name='/tmp/editor.sock'"
                process-environment)))
    (should (equal (majutsu-ediff--editor-command-from-env)
                   '("emacsclient" "--socket-name=/tmp/editor.sock")))))

(ert-deftest majutsu-ediff-test-diffedit-editor-target ()
  "Diffedit target should point into right side temp tree."
  (should (equal (majutsu-ediff--diffedit-editor-target "src/one.el")
                 "$right/src/one.el")))

(ert-deftest majutsu-ediff-test-build-resolve-args ()
  "Resolve args should include rev, file and merge editor config."
  (should (equal (majutsu-ediff--build-resolve-args
                  "abc123"
                  "f.txt"
                  "ui.merge-editor=[\"emacsclient\",\"...\"]")
                 '("resolve"
                   "--config"
                   "ui.merge-editor=[\"emacsclient\",\"...\"]"
                   "-r" "abc123"
                   "--" "f.txt"))))

(ert-deftest majutsu-ediff-test-conflict-side-count ()
  "Conflict side count should be parsed from `jj resolve --list` output."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _)
               '("f.txt    4-sided conflict")))
            ((symbol-function 'majutsu-file--root)
             (lambda () default-directory)))
    (should (= 4 (majutsu-ediff--conflict-side-count "@" "f.txt")))))

(ert-deftest majutsu-ediff-test-directory-common-files ()
  "Directory common file discovery should ignore JJ-INSTRUCTIONS."
  (let ((left (make-temp-file "majutsu-ediff-left" t))
        (right (make-temp-file "majutsu-ediff-right" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" left) t)
          (make-directory (expand-file-name "sub" right) t)
          (write-region "a" nil (expand-file-name "a.txt" left) nil 'silent)
          (write-region "a" nil (expand-file-name "a.txt" right) nil 'silent)
          (write-region "b" nil (expand-file-name "sub/b.txt" left) nil 'silent)
          (write-region "b" nil (expand-file-name "sub/b.txt" right) nil 'silent)
          (write-region "ignored" nil (expand-file-name "JJ-INSTRUCTIONS" right) nil 'silent)
          (write-region "only-right" nil (expand-file-name "c.txt" right) nil 'silent)
          (should (equal (majutsu-ediff--directory-common-files left right)
                         '("a.txt" "sub/b.txt"))))
      (delete-directory left t)
      (delete-directory right t))))

(ert-deftest majutsu-ediff-test-directories-runs-manual-right-file-edit ()
  "Directory editor entry should open only right-side file for editing."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-ediff--read-directory-file)
               (lambda (_left _right) "foo.txt"))
              ((symbol-function 'find-file)
               (lambda (right-file)
                 (setq captured right-file))))
      (majutsu-ediff-directories "/tmp/left" "/tmp/right")
      (should (equal captured
                     (expand-file-name "foo.txt" "/tmp/right"))))))

(ert-deftest majutsu-ediff-test-resolve-file-dwim-uses-commit-revision ()
  "Resolve DWIM should query conflicted files at commit section revision."
  (let (asked-rev)
    (cl-letf (((symbol-function 'magit-section-value-if)
               (lambda (type)
                 (when (eq type 'jj-commit) "abc123")))
              ((symbol-function 'majutsu-ediff--read-conflicted-file)
               (lambda (&optional rev)
                 (setq asked-rev rev)
                 "picked-file.txt")))
      (should (equal (majutsu-ediff--resolve-file-dwim nil) "picked-file.txt"))
      (should (equal asked-rev "abc123")))))

(ert-deftest majutsu-ediff-test-resolve-file-dwim-prefers-explicit-file ()
  "Resolve DWIM should respect explicitly provided file."
  (cl-letf (((symbol-function 'majutsu-ediff--read-conflicted-file)
             (lambda (&optional _rev)
               (ert-fail "should not prompt when file arg is provided"))))
    (should (equal (majutsu-ediff--resolve-file-dwim "given.txt") "given.txt"))))

(ert-deftest majutsu-ediff-test-resolve-with-conflict-is-alias ()
  "Legacy resolve-with-conflict should delegate to resolve."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-ediff-resolve)
               (lambda (&optional _file)
                 (setq called t))))
      (majutsu-ediff-resolve-with-conflict)
      (should called))))

(ert-deftest majutsu-ediff-test-resolve-runs-jj-resolve ()
  "Resolve command should invoke jj resolve with built args."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-ediff--resolve-revision-at-point)
               (lambda () "rev-at-point"))
              ((symbol-function 'majutsu-ediff--resolve-file-dwim)
               (lambda (&optional _file) "conflicted.txt"))
              ((symbol-function 'majutsu-ediff--conflict-side-count)
               (lambda (_rev _file) 2))
              ((symbol-function 'majutsu-ediff--run-resolve)
               (lambda (rev file)
                 (setq captured (list rev file)))))
      (majutsu-ediff-resolve)
      (should (equal captured '("rev-at-point" "conflicted.txt"))))))

(ert-deftest majutsu-ediff-test-resolve-falls-back-to-diffedit-for-multi-side ()
  "Resolve should use diffedit fallback for conflicts with more than 2 sides."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-ediff--resolve-revision-at-point)
               (lambda () "rev-at-point"))
              ((symbol-function 'majutsu-ediff--resolve-file-dwim)
               (lambda (&optional _file) "conflicted.txt"))
              ((symbol-function 'majutsu-ediff--conflict-side-count)
               (lambda (_rev _file) 4))
              ((symbol-function 'majutsu-ediff--run-diffedit)
               (lambda (args file)
                 (setq captured (list args file)))))
      (majutsu-ediff-resolve)
      (should (equal captured
                     '(("-r" "rev-at-point" "--" "conflicted.txt") "conflicted.txt"))))))

(ert-deftest majutsu-ediff-test-edit-prompts-file-and-runs-single-file ()
  "Diffedit should prompt for file when none at point."
  (let (captured)
    (with-temp-buffer
      (cl-letf (((symbol-function 'majutsu-file-at-point)
                 (lambda () nil))
                ((symbol-function 'majutsu-ediff--read-file)
                 (lambda (from to)
                   (should (equal from "@-"))
                   (should (equal to "@"))
                   "docs/majutsu.org"))
                ((symbol-function 'majutsu-ediff--run-diffedit)
                 (lambda (args file)
                   (setq captured (list args file)))))
        (majutsu-ediff-edit nil)
        (should (equal captured
                       '(("--from" "@-" "--to" "@" "--" "docs/majutsu.org")
                         "docs/majutsu.org")))))))

(ert-deftest majutsu-ediff-test-edit-uses-file-at-point ()
  "Diffedit should use file at point and skip file prompt."
  (let (captured)
    (with-temp-buffer
      (cl-letf (((symbol-function 'majutsu-file-at-point)
                 (lambda () "src/one.el"))
                ((symbol-function 'majutsu-ediff--read-file)
                 (lambda (&rest _)
                   (ert-fail "should not prompt when file at point exists")))
                ((symbol-function 'majutsu-ediff--run-diffedit)
                 (lambda (args file)
                   (setq captured (list args file)))))
        (majutsu-ediff-edit '("--from=main" "--to=@"))
        (should (equal captured
                       '(("--from" "main" "--to" "@" "--" "src/one.el")
                         "src/one.el")))))))

(ert-deftest majutsu-ediff-test-transient-no-resolve-actions ()
  "Ediff transient should no longer expose resolve actions."
  (should-error (transient-get-suffix 'majutsu-ediff "m"))
  (should-error (transient-get-suffix 'majutsu-ediff "M")))

(provide 'majutsu-ediff-test)
;;; majutsu-ediff-test.el ends here
