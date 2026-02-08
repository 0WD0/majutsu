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
  (let ((result (majutsu-jj--parse-diff-range '("--revisions=abc"))))
    (should (equal (car result) "abc-"))
    (should (equal (cdr result) "abc"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-from-to ()
  "Test parsing --from/--to format."
  (let ((result (majutsu-jj--parse-diff-range '("--from=foo" "--to=bar"))))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-from-only ()
  "Test parsing --from only."
  (let ((result (majutsu-jj--parse-diff-range '("--from=foo"))))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "@"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-to-only ()
  "Test parsing --to only."
  (let ((result (majutsu-jj--parse-diff-range '("--to=bar"))))
    (should (equal (car result) "@-"))
    (should (equal (cdr result) "bar"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-nil ()
  "Test parsing nil range."
  (let ((result (majutsu-jj--parse-diff-range nil)))
    (should (null result))))

(ert-deftest majutsu-ediff-test-parse-diff-range-empty ()
  "Test parsing empty range."
  (let ((result (majutsu-jj--parse-diff-range '())))
    (should (null result))))

(ert-deftest majutsu-ediff-test-parse-diff-range-short-r ()
  "Test parsing -r format."
  (let ((result (majutsu-jj--parse-diff-range '("-rxyz"))))
    (should (equal (car result) "xyz-"))
    (should (equal (cdr result) "xyz"))))

(ert-deftest majutsu-ediff-test-edit-range-default-fallback ()
  "When no args/context are available, diffedit range defaults to @-..@."
  (with-temp-buffer
    (let ((range (majutsu-edit--edit-range nil)))
      (should (equal (car range) "@-"))
      (should (equal (cdr range) "@")))))

(ert-deftest majutsu-ediff-test-build-diffedit-args-with-file ()
  "Diffedit args should include a fileset path when file is selected."
  (should (equal (majutsu-edit--build-diffedit-args "foo" "bar" "src/x.el")
                 '("--from" "foo" "--to" "bar" "--" "src/x.el"))))

(ert-deftest majutsu-ediff-test-editor-command-config ()
  "Editor command config should use explicit command words."
  (let ((config (majutsu-jj--editor-command-config
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
    (should (equal (majutsu-jj--editor-command-from-env)
                   '("emacsclient" "--socket-name=/tmp/editor.sock")))))

(ert-deftest majutsu-ediff-test-diffedit-editor-target ()
  "Diffedit target should point into right side temp tree."
  (should (equal (majutsu-edit--diffedit-editor-target "src/one.el")
                 "$right/src/one.el")))

(ert-deftest majutsu-ediff-test-merge-editor-config ()
  "Merge editor config should evaluate 3-way Ediff."
  (let* ((majutsu-with-editor-envvar "JJ_EDITOR")
         (process-environment
          (cons "JJ_EDITOR=emacsclient --socket-name=/tmp/editor.sock"
                process-environment))
         (config (majutsu-ediff--merge-editor-config)))
    (should (string-prefix-p "ui.merge-editor=[" config))
    (should (string-match-p "emacsclient" config))
    (should (string-match-p "--eval" config))
    (should (string-match-p "majutsu-ediff-merge-files" config))
    (should (string-match-p "\\$left" config))
    (should (string-match-p "\\$base" config))
    (should (string-match-p "\\$right" config))
    (should (string-match-p "\\$output" config))))

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

(ert-deftest majutsu-ediff-test-diffedit-file-launches-ediff-session ()
  "Directory editor entry should start an Ediff file session."
  (let (called-left called-right entered-recursive)
    (cl-letf (((symbol-function 'majutsu-ediff--read-directory-file)
               (lambda (_left _right) "foo.txt"))
              ((symbol-function 'ediff-files)
               (lambda (left right)
                 (setq called-left left)
                 (setq called-right right)))
              ((symbol-function 'recursive-edit)
               (lambda ()
                 (setq entered-recursive t))))
      (majutsu-ediff-diffedit-file "/tmp/left" "/tmp/right")
      (should (equal called-left (expand-file-name "foo.txt" "/tmp/left")))
      (should (equal called-right (expand-file-name "foo.txt" "/tmp/right")))
      (should entered-recursive))))

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

(ert-deftest majutsu-ediff-test-resolve-with-conflict-uses-working-copy-file ()
  "Resolve-with-conflict should open filesystem file for working copy revs."
  (let (opened pop-buffer ensured gotoed)
    (cl-letf (((symbol-function 'majutsu-ediff--resolve-revision-at-point)
               (lambda () "rev-at-point"))
              ((symbol-function 'majutsu-ediff--resolve-file-dwim)
               (lambda (&optional _file) "conflicted.txt"))
              ((symbol-function 'majutsu-ediff--working-copy-revision-p)
               (lambda (_rev) t))
              ((symbol-function 'majutsu-file--root)
               (lambda () "/repo"))
              ((symbol-function 'find-file-noselect)
               (lambda (path)
                 (setq opened path)
                 (current-buffer)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _)
                 (setq pop-buffer buffer-or-name)
                 buffer-or-name))
              ((symbol-function 'majutsu-conflict-ensure-mode)
               (lambda ()
                 (setq ensured t)))
              ((symbol-function 'majutsu-conflict-goto-nearest)
               (lambda ()
                 (setq gotoed t))))
      (with-temp-buffer
        (majutsu-ediff-resolve-with-conflict)
        (should (equal opened "/repo/conflicted.txt"))
        (should (bufferp pop-buffer))
        (should ensured)
        (should gotoed)))))

(ert-deftest majutsu-ediff-test-resolve-with-conflict-uses-blob-buffer-for-non-wc ()
  "Resolve-with-conflict should open blob buffer when rev is not working copy."
  (let (seen-rev seen-file)
    (cl-letf (((symbol-function 'majutsu-ediff--resolve-revision-at-point)
               (lambda () "abc123"))
              ((symbol-function 'majutsu-ediff--resolve-file-dwim)
               (lambda (&optional _file) "conflicted.txt"))
              ((symbol-function 'majutsu-ediff--working-copy-revision-p)
               (lambda (_rev) nil))
              ((symbol-function 'find-file-noselect)
               (lambda (_path)
                 (ert-fail "should not open filesystem file for non-working-copy rev")))
              ((symbol-function 'majutsu-find-file-noselect)
               (lambda (rev file &optional _revert)
                 (setq seen-rev rev)
                 (setq seen-file file)
                 (current-buffer)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _)
                 buffer-or-name))
              ((symbol-function 'majutsu-conflict-ensure-mode)
               (lambda ()))
              ((symbol-function 'majutsu-conflict-goto-nearest)
               (lambda ())))
      (with-temp-buffer
        (majutsu-ediff-resolve-with-conflict)
        (should (equal seen-rev "abc123"))
        (should (equal seen-file "conflicted.txt"))))))

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
              ((symbol-function 'majutsu-edit--run-diffedit)
               (lambda (args file)
                 (setq captured (list args file)))))
      (majutsu-ediff-resolve)
      (should (equal captured
                     '(("-r" "rev-at-point" "--" "conflicted.txt") "conflicted.txt"))))))

(ert-deftest majutsu-ediff-test-edit-prompts-file-and-runs-single-file ()
  "Diffedit should prompt for file when none at point."
  (let (captured)
    (with-temp-buffer
      (cl-letf (((symbol-function 'majutsu-edit--file-at-point)
                 (lambda () nil))
                ((symbol-function 'majutsu-jj-read-diff-file)
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
      (cl-letf (((symbol-function 'majutsu-edit--file-at-point)
                 (lambda () "src/one.el"))
                ((symbol-function 'majutsu-jj-read-diff-file)
                 (lambda (&rest _)
                   (ert-fail "should not prompt when file at point exists")))
                ((symbol-function 'majutsu-ediff--run-diffedit)
                 (lambda (args file)
                   (setq captured (list args file)))))
        (majutsu-ediff-edit '("--from=main" "--to=@"))
        (should (equal captured
                       '(("--from" "main" "--to" "@" "--" "src/one.el")
                         "src/one.el")))))))

(ert-deftest majutsu-ediff-test-transient-has-resolve-actions ()
  "Ediff transient should expose both resolve actions."
  (should (transient-get-suffix 'majutsu-ediff "m"))
  (should (transient-get-suffix 'majutsu-ediff "M")))

(provide 'majutsu-ediff-test)
;;; majutsu-ediff-test.el ends here
