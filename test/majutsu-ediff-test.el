;;; majutsu-ediff-test.el --- Tests for majutsu-ediff  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

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
         (configs (majutsu-ediff--merge-editor-config))
         (all (mapconcat #'identity configs "\n")))
    (should (= (length configs) 5))
    (should (string-match-p "ui.merge-editor=\"majutsu_ediff_merge\"" all))
    (should (string-match-p "merge-tools.majutsu_ediff_merge.program=\"emacsclient\"" all))
    (should (string-match-p "merge-tools.majutsu_ediff_merge.merge-args=\\[" all))
    (should (string-match-p "--socket-name=/tmp/editor.sock" all))
    (should (string-match-p "--eval" all))
    (should (string-match-p "majutsu-ediff-merge-files" all))
    (should (string-match-p "\\$left" all))
    (should (string-match-p "\\$base" all))
    (should (string-match-p "\\$right" all))
    (should (string-match-p "\\$output" all))
    (should (string-match-p "\\$marker_length" all))
    (should (string-match-p "merge-tools.majutsu_ediff_merge.merge-tool-edits-conflict-markers=true" all))
    (should (string-match-p "merge-tools.majutsu_ediff_merge.conflict-marker-style=\"git\"" all))))

(ert-deftest majutsu-ediff-test-build-resolve-args ()
  "Resolve args should include rev, file and all merge editor configs."
  (should (equal (majutsu-ediff--build-resolve-args
                  "abc123"
                  "f.txt"
                  '("cfg-a" "cfg-b"))
                 '("resolve"
                   "--config"
                   "cfg-a"
                   "--config"
                   "cfg-b"
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
  (let (called-left called-right startup-hooks entered-recursive)
    (cl-letf (((symbol-function 'majutsu-ediff--read-directory-file)
               (lambda (_left _right) "foo.txt"))
              ((symbol-function 'ediff-files)
               (lambda (left right &optional hooks)
                 (setq called-left left)
                 (setq called-right right)
                 (setq startup-hooks hooks)))
              ((symbol-function 'recursive-edit)
               (lambda ()
                 (setq entered-recursive t))))
      (majutsu-ediff-diffedit-file "/tmp/left" "/tmp/right")
      (should (equal called-left (expand-file-name "foo.txt" "/tmp/left")))
      (should (equal called-right (expand-file-name "foo.txt" "/tmp/right")))
      (should (= (length startup-hooks) 1))
      (should entered-recursive))))

(ert-deftest majutsu-ediff-test-diffedit-file-installs-local-quit-hooks ()
  "Diffedit startup hook should install local quit hooks.
The recursive-edit exit hook must run from `ediff-after-quit-hook-internal'
instead of `ediff-quit-hook' to avoid interrupting Ediff cleanup."
  (let (startup-hooks)
    (cl-letf (((symbol-function 'majutsu-ediff--read-directory-file)
               (lambda (_left _right) "foo.txt"))
              ((symbol-function 'ediff-files)
               (lambda (_left _right &optional hooks)
                 (setq startup-hooks hooks)))
              ((symbol-function 'recursive-edit)
               (lambda () nil)))
      (majutsu-ediff-diffedit-file "/tmp/left" "/tmp/right")
      (should (= (length startup-hooks) 1))
      (with-temp-buffer
        (let ((ediff-quit-hook nil)
              (ediff-after-quit-hook-internal nil)
              (default-quit-count (length (default-value 'ediff-quit-hook))))
          (funcall (car startup-hooks))
          (should (consp ediff-quit-hook))
          (should (> (length ediff-quit-hook) default-quit-count))
          (should-not (memq #'majutsu-ediff--exit-recursive-edit
                            ediff-quit-hook))
          (should (memq #'majutsu-ediff--exit-recursive-edit
                        ediff-after-quit-hook-internal)))))))

(ert-deftest majutsu-ediff-test-merge-files-launches-ediff-session ()
  "Merge callback should launch Ediff merge session and enter recursive edit."
  (let (called-left called-right called-base startup-hooks called-output entered-recursive)
    (cl-letf (((symbol-function 'ediff-merge-files-with-ancestor)
               (lambda (left right base &optional hooks output)
                 (setq called-left left)
                 (setq called-right right)
                 (setq called-base base)
                 (setq startup-hooks hooks)
                 (setq called-output output)))
              ((symbol-function 'recursive-edit)
               (lambda ()
                 (setq entered-recursive t))))
      (majutsu-ediff-merge-files "/tmp/left" "/tmp/base" "/tmp/right" "/tmp/output")
      (should (equal called-left (expand-file-name "/tmp/left")))
      (should (equal called-right (expand-file-name "/tmp/right")))
      (should (equal called-base (expand-file-name "/tmp/base")))
      (should (equal called-output (expand-file-name "/tmp/output")))
      (should (= (length startup-hooks) 1))
      (should (functionp (car startup-hooks)))
      (should entered-recursive))))

(ert-deftest majutsu-ediff-test-merge-files-uses-git-markers-with-marker-length ()
  "Merge callback should configure git-style marker pattern from jj marker length."
  (let (captured-default captured-pattern)
    (cl-letf (((symbol-function 'ediff-merge-files-with-ancestor)
               (lambda (_left _right _base &optional _hooks _output)
                 (setq captured-default ediff-default-variant)
                 (setq captured-pattern ediff-combination-pattern)))
              ((symbol-function 'recursive-edit)
               (lambda () nil)))
      (majutsu-ediff-merge-files "/tmp/left" "/tmp/base" "/tmp/right" "/tmp/output" "11")
      (should (eq captured-default 'combined))
      (should (equal captured-pattern
                     (list (make-string 11 ?<)
                           'A
                           (make-string 11 ?|)
                           'Ancestor
                           (make-string 11 ?=)
                           'B
                           (make-string 11 ?>)))))))

(ert-deftest majutsu-ediff-test-merge-marker-length-normalization ()
  "Marker length helper should normalize invalid values to at least 7."
  (should (= 7 (majutsu-ediff--merge-marker-length nil)))
  (should (= 7 (majutsu-ediff--merge-marker-length "")))
  (should (= 7 (majutsu-ediff--merge-marker-length "0")))
  (should (= 7 (majutsu-ediff--merge-marker-length "-3")))
  (should (= 7 (majutsu-ediff--merge-marker-length "6")))
  (should (= 9 (majutsu-ediff--merge-marker-length "9")))
  (should (= 12 (majutsu-ediff--merge-marker-length 12))))

(ert-deftest majutsu-ediff-test-merge-files-installs-local-quit-hooks ()
  "Merge callback startup hook should install local quit hooks."
  (let (startup-hooks)
    (let ((quit-ran nil)
          (exit-ran nil)
          (captured-winconf nil))
      (cl-letf (((symbol-function 'ediff-merge-files-with-ancestor)
                 (lambda (_left _right _base &optional hooks _output)
                   (setq startup-hooks hooks)))
                ((symbol-function 'recursive-edit)
                 (lambda () nil))
                ((symbol-function 'run-hooks)
                 (lambda (&rest hooks)
                   (if (memq 'majutsu-ediff-quit-hook hooks)
                       (progn
                         (setq quit-ran t)
                         (setq captured-winconf majutsu-ediff-previous-winconf)))))
                ((symbol-function 'majutsu-ediff--exit-recursive-edit)
                 (lambda ()
                   (setq exit-ran t))))
        (majutsu-ediff-merge-files "/tmp/left" "/tmp/base" "/tmp/right" "/tmp/output")
        (with-temp-buffer
          (let ((default-quit-count (length (default-value 'ediff-quit-hook))))
            (setq-local ediff-quit-hook
                        (copy-sequence (default-value 'ediff-quit-hook)))
            (setq-local ediff-after-quit-hook-internal nil)
            (funcall (car startup-hooks))
            (should (consp ediff-quit-hook))
            (should (> (length ediff-quit-hook) default-quit-count))
            (should (equal ediff-quit-merge-hook
                           '(majutsu-ediff--quit-merge-session)))
            (should (memq #'majutsu-ediff--exit-recursive-edit
                          ediff-after-quit-hook-internal))
            (funcall (car (last ediff-quit-hook)))
            (funcall (car (last ediff-after-quit-hook-internal)))
            (should quit-ran)
            (should exit-ran)
            (should (window-configuration-p captured-winconf))))))))

(ert-deftest majutsu-ediff-test-quit-merge-session-saves-output-when-edited ()
  "Merge quit hook should persist output file when merge was edited."
  (let ((merge-buffer (generate-new-buffer " *majutsu-merge*"))
        (asked nil)
        (written nil)
        (killed nil))
    (unwind-protect
        (with-temp-buffer
          (let ((ediff-buffer-C merge-buffer))
            (setq-local majutsu-ediff--merge-user-edited t)
            (setq-local ediff-merge-store-file "/tmp/output_test.txt")
            (with-current-buffer merge-buffer
              (setq-local ediff-merge-store-file nil)
              (setq buffer-file-name "/tmp/output_test.txt")
              (set-buffer-modified-p t))
            (cl-letf (((symbol-function 'majutsu-ediff--confirm-save-merge)
                       (lambda (file)
                         (setq asked file)
                         t))
                      ((symbol-function 'write-region)
                       (lambda (_start _end filename &optional _append _visit _lockname _mustbenew)
                         (setq written filename)))
                      ((symbol-function 'ediff-kill-buffer-carefully)
                       (lambda (_)
                         (setq killed t))))
              (majutsu-ediff--quit-merge-session)
              (should (equal asked "/tmp/output_test.txt"))
              (should (equal written "/tmp/output_test.txt"))
              (should killed)
              (should-not (with-current-buffer merge-buffer
                            (buffer-modified-p))))))
      (kill-buffer merge-buffer))))

(ert-deftest majutsu-ediff-test-quit-merge-session-discards-when-save-declined ()
  "Merge quit hook should discard edited output when save is declined."
  (let ((merge-buffer (generate-new-buffer " *majutsu-merge*"))
        (asked nil)
        (written nil)
        (killed nil))
    (unwind-protect
        (with-temp-buffer
          (let ((ediff-buffer-C merge-buffer))
            (setq-local majutsu-ediff--merge-user-edited t)
            (setq-local ediff-merge-store-file "/tmp/output_test.txt")
            (with-current-buffer merge-buffer
              (setq-local ediff-merge-store-file nil)
              (setq buffer-file-name "/tmp/output_test.txt")
              (set-buffer-modified-p t))
            (cl-letf (((symbol-function 'majutsu-ediff--confirm-save-merge)
                       (lambda (file)
                         (setq asked file)
                         nil))
                      ((symbol-function 'write-region)
                       (lambda (&rest _)
                         (setq written t)))
                      ((symbol-function 'ediff-kill-buffer-carefully)
                       (lambda (_)
                         (setq killed t))))
              (majutsu-ediff--quit-merge-session)
              (should (equal asked "/tmp/output_test.txt"))
              (should-not written)
              (should killed)
              (should-not (with-current-buffer merge-buffer
                            (buffer-modified-p))))))
      (kill-buffer merge-buffer))))

(ert-deftest majutsu-ediff-test-quit-merge-session-skips-save-without-edits ()
  "Merge quit hook should not persist output file if merge was untouched."
  (let ((merge-buffer (generate-new-buffer " *majutsu-merge*"))
        (written nil)
        (killed nil))
    (unwind-protect
        (with-temp-buffer
          (let ((ediff-buffer-C merge-buffer))
            (setq-local majutsu-ediff--merge-user-edited nil)
            (setq-local ediff-merge-store-file "/tmp/output_test.txt")
            (with-current-buffer merge-buffer
              (setq buffer-file-name "/tmp/output_test.txt")
              (set-buffer-modified-p t))
            (cl-letf (((symbol-function 'write-region)
                       (lambda (&rest _)
                         (setq written t)))
                      ((symbol-function 'ediff-kill-buffer-carefully)
                       (lambda (_)
                         (setq killed t))))
              (majutsu-ediff--quit-merge-session)
              (should-not written)
              (should killed)
              (should-not (with-current-buffer merge-buffer
                            (buffer-modified-p))))))
      (kill-buffer merge-buffer))))

(ert-deftest majutsu-ediff-test-quit-merge-session-skips-save-without-output-file ()
  "Merge quit hook should skip save when no output file is configured."
  (let ((merge-buffer (generate-new-buffer " *majutsu-merge*"))
        (written nil)
        (killed nil))
    (unwind-protect
        (with-temp-buffer
          (let ((ediff-buffer-C merge-buffer))
            (setq-local majutsu-ediff--merge-user-edited t)
            (setq-local ediff-merge-store-file nil)
            (with-current-buffer merge-buffer
              (setq buffer-file-name "/tmp/output_test.txt")
              (set-buffer-modified-p t))
            (cl-letf (((symbol-function 'write-region)
                       (lambda (&rest _)
                         (setq written t)))
                      ((symbol-function 'ediff-kill-buffer-carefully)
                       (lambda (_)
                         (setq killed t))))
              (majutsu-ediff--quit-merge-session)
              (should-not written)
              (should killed)
              (should-not (with-current-buffer merge-buffer
                            (buffer-modified-p))))))
      (kill-buffer merge-buffer))))

(ert-deftest majutsu-ediff-test-cleanup-diffedit-variant-buffers ()
  "Diffedit helper should save and kill session-created variant buffers."
  (let ((left (make-temp-file "majutsu-ediff-left"))
        (right (make-temp-file "majutsu-ediff-right"))
        left-buf right-buf)
    (unwind-protect
        (progn
          (setq left-buf (find-file-noselect left))
          (setq right-buf (find-file-noselect right))
          (with-current-buffer right-buf
            (erase-buffer)
            (insert "edited"))
          (majutsu-ediff--cleanup-diffedit-variant-buffers
           left right nil nil)
          (should-not (buffer-live-p left-buf))
          (should-not (buffer-live-p right-buf))
          (with-temp-buffer
            (insert-file-contents right)
            (should (equal (buffer-string) "edited"))))
      (when (buffer-live-p left-buf)
        (kill-buffer left-buf))
      (when (buffer-live-p right-buf)
        (kill-buffer right-buf))
      (delete-file left)
      (delete-file right))))

(ert-deftest majutsu-ediff-test-cleanup-diffedit-variant-buffers-bypasses-kill-guard ()
  "Diffedit cleanup should ignore kill-buffer query guards.
This mirrors with-editor's kill guard so cleanup cannot abort quit hooks."
  (let ((left (make-temp-file "majutsu-ediff-left"))
        (right (make-temp-file "majutsu-ediff-right"))
        left-buf right-buf)
    (unwind-protect
        (progn
          (setq left-buf (find-file-noselect left))
          (setq right-buf (find-file-noselect right))
          (with-current-buffer left-buf
            (add-hook 'kill-buffer-query-functions
                      (lambda ()
                        (user-error "kill guard"))
                      nil t))
          (with-current-buffer right-buf
            (add-hook 'kill-buffer-query-functions
                      (lambda ()
                        (user-error "kill guard"))
                      nil t))
          (majutsu-ediff--cleanup-diffedit-variant-buffers
           left right nil nil)
          (should-not (buffer-live-p left-buf))
          (should-not (buffer-live-p right-buf)))
      (when (buffer-live-p left-buf)
        (kill-buffer left-buf))
      (when (buffer-live-p right-buf)
        (kill-buffer right-buf))
      (delete-file left)
      (delete-file right))))

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
