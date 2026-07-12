;;; majutsu-split-test.el --- Tests for split transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for split command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-split)

(ert-deftest majutsu-split-transient/exposes-tool-infix-under-equals-prefix ()
  "The jj tool chooser uses Magit's `=t' convention, not jj's `-t'."
  (should (transient-get-suffix 'majutsu-split "=t"))
  (should-not
   (condition-case nil
       (transient-get-suffix 'majutsu-split "-t")
     (error nil))))

(ert-deftest majutsu-split-execute/routes-jj-editor-flags-to-diff-editor ()
  "Keep each explicit jj editor flag when routing split to its session."
  (let ((origin (current-buffer)))
    (dolist (editor-args '(("-i")
                           ("--interactive")
                           ("--tool" "meld")
                           ("--tool=meld")))
      (let (called)
        (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
                   (lambda (&rest _) nil))
                  ((symbol-function 'majutsu-diff-editor-start)
                   (lambda (&rest args)
                     (setq called args))))
          (majutsu-split-execute
           (append '(("--" "src/a.el") "--revision=@") editor-args))
          (should (equal (cl-subseq called 0 4)
                         (list "split"
                               (append '("--revision=@") editor-args)
                               '("src/a.el")
                               :origin-buffer)))
          (should (eq (nth 4 called) origin)))))))

(ert-deftest majutsu-split-execute/routes-no-fileset-to-diff-editor ()
  "Let jj perform its default interactive split without a fileset."
  (let ((origin (current-buffer)) called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-diff-editor-start)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-split-execute '("--revision=@"))
      (should (equal (cl-subseq called 0 4)
                     '("split" ("--revision=@") nil :origin-buffer)))
      (should (eq (nth 4 called) origin)))))

(ert-deftest majutsu-split-execute/explicit-jj-editor-wins-over-patch-selection ()
  "An explicit jj editor request must not consume a Majutsu patch selection."
  (let ((origin (current-buffer)) called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-diff-editor-start)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _)
                 (ert-fail "Should not replace an explicit jj editor")))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute
       '(("--" "src/a.el") "--revision=@" "--tool" "meld"))
      (should (equal (cl-subseq called 0 4)
                     '("split" ("--revision=@" "--tool" "meld")
                       ("src/a.el") :origin-buffer)))
      (should (eq (nth 4 called) origin))
      (should-not cleared))))

(ert-deftest majutsu-split-execute/places-structured-filesets-after-options ()
  "Run a noninteractive fileset split through the ordinary runner."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@"))
      (should (equal called
                     '(("split" "--revision=@" "--" "src/a.el")))))))

(ert-deftest majutsu-split-default-args/resolves-single-diff-revision ()
  "A single diff revision becomes split's resolved --revision target."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=mine()"))
    (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (revset)
                 (should (equal revset "mine()"))
                 "abc123")))
      (should (equal (majutsu-split--default-args)
                     '("--revision=abc123"))))))

(ert-deftest majutsu-split-default-args/accepts-supported-revision-spellings ()
  "Resolve each supported spelling, including short attached equals."
  (dolist (range '(("--revisions" "mine()")
                   ("-r" "mine()")
                   ("-rmine()")
                   ("-r=mine()")))
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-range range)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (revset)
                   (should (equal revset "mine()"))
                   "abc123")))
        (should (equal (majutsu-split--default-args)
                       '("--revision=abc123")))))))

(ert-deftest majutsu-split-default-args/drops-arbitrary-diff-ranges ()
  "Never pass a diff --from/--to range to jj split."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=A" "--to=B"))
    (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (&rest _)
                 (ert-fail "Should not resolve an arbitrary diff range"))))
      (should-not (majutsu-split--default-args)))))

(ert-deftest majutsu-split-default-args/rejects-ambiguous-or-missing-revisions ()
  "Only a resolvable singleton --revisions context can seed split."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range
                '("--revisions=A" "--revisions=B"))
    (should-not (majutsu-split--default-args))
    (setq-local majutsu-buffer-diff-range '("--revisions=A"))
    (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (&rest _) nil)))
      (should-not (majutsu-split--default-args)))))

(ert-deftest majutsu-split-patch-source/default-range-is-canonical-and-cached ()
  "The default diff renders @, whose canonical ID is cached per rendering."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range nil)
    (let (calls)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (revset)
                   (push revset calls)
                   "commit-id")))
        (should (equal (majutsu-split--patch-source-commit) "commit-id"))
        (should (equal (majutsu-split--patch-source-commit) "commit-id")))
      (should (equal calls '("@"))))))

(ert-deftest majutsu-split-patch-source/rejects-unsafe-diff-ranges ()
  "Reject from/to, repeated revisions, and a revset resolving to a range."
  (dolist (range '(("--from=A" "--to=B")
                   ("-f=@" "-t=@-")
                   ("--revisions=A" "-r=B")))
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-range range)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (&rest _)
                   (ert-fail "Unsafe option shape must not be resolved"))))
        (should-not (majutsu-split--patch-source-commit)))))
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=A::B"))
    (let (resolved)
      (cl-letf (((symbol-function 'majutsu-jj-resolve-single-commit)
                 (lambda (revset)
                   (setq resolved revset)
                   nil)))
        (should-not (majutsu-split--patch-source-commit)))
      (should (equal resolved "A::B")))))

(ert-deftest majutsu-split-interactive-selection-available/requires-safe-source ()
  "The transient predicate combines general diff support with source safety."
  (cl-letf (((symbol-function 'majutsu-interactive-selection-available-p)
             (lambda () t))
            ((symbol-function 'majutsu-split--patch-source-commit)
             (lambda (&rest _) "commit-id")))
    (should (equal (majutsu-split-interactive-selection-available-p)
                   "commit-id")))
  (cl-letf (((symbol-function 'majutsu-interactive-selection-available-p)
             (lambda () t))
            ((symbol-function 'majutsu-split--patch-source-commit)
             (lambda (&rest _) nil)))
    (should-not (majutsu-split-interactive-selection-available-p))))

(ert-deftest majutsu-split-execute/patch-rejects-different-or-unsafe-source ()
  "The execution guard preserves a selection when its source is unsafe."
  (let (ran)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-split--patch-source-commit)
               (lambda (&rest _) "rendered-id"))
              ((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (&rest _) "different-id"))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _) (setq ran t))))
      (should-error (majutsu-split-execute '("--revision=other"))
                    :type 'user-error)
      (should-not ran)))
  (let (ran)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-split--patch-source-commit)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _) (setq ran t))))
      (should-error (majutsu-split-execute '("--revision=@"))
                    :type 'user-error)
      (should-not ran)))
  (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
             (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
            ((symbol-function 'majutsu-split--patch-source-commit)
             (lambda (&rest _) "rendered-id")))
    (should-error
     (majutsu-split-execute '("--revision=A" "--revision=B"))
     :type 'user-error)))

(ert-deftest majutsu-split-execute/patch-keeps-filesets-after-options ()
  "Patch split should still pass transient filesets after option arguments."
  (let (called cleared stripped)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-diff-editor-strip-interactive-arguments)
               (lambda (args)
                 (setq stripped args)
                 args))
              ((symbol-function 'majutsu-split--patch-source-commit)
               (lambda (&rest _) "commit-id"))
              ((symbol-function 'majutsu-jj-resolve-single-commit)
               (lambda (&rest _) "commit-id"))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute '(("--" "src/a.el") "--revision=@"))
      (should (equal called
                     '("split" ("--revision=commit-id")
                       ("src/a.el") (:base left :payload-root right :patch "PATCH" :file-ops nil))))
      (should (equal stripped '("--revision=commit-id")))
      (should-not cleared))))

(defun majutsu-split-test--jj-call (program directory &rest args)
  "Run PROGRAM with ARGS in DIRECTORY and return standard output."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory directory))
           (exit (apply #'call-process
                        program nil t nil
                        (append '("--no-pager" "--color=never"
                                  "--config" "user.name=Majutsu Test"
                                  "--config" "user.email=majutsu@example.invalid")
                                args))))
      (unless (zerop exit)
        (ert-fail (format "jj failed (%d): %s\n%s"
                          exit (string-join args " ") (buffer-string))))
      (buffer-string))))

(ert-deftest majutsu-split/integration-splits-selected-added-region ()
  "Split one selected added line from a non-working-copy source."
  (let ((jj (or (let ((configured (getenv "MAJUTSU_TEST_JJ")))
                  (and configured (file-executable-p configured) configured))
                (executable-find "jj"))))
    (skip-unless jj)
    (let* ((parent (make-temp-file "majutsu-split-region-" t))
           (repo (expand-file-name "repo" parent))
           source process)
      (unwind-protect
          (progn
            (majutsu-split-test--jj-call jj parent "git" "init" repo)
            (let ((default-directory (file-name-as-directory repo)))
              (with-temp-file (expand-file-name "notes.txt" repo)
                (insert "base\n"))
              (majutsu-split-test--jj-call jj repo "describe" "-m" "A")
              (majutsu-split-test--jj-call jj repo "new" "-m" "B")
              (with-temp-file (expand-file-name "notes.txt" repo)
                (insert "base\nselected addition\nunselected addition\n"))
              (setq source
                    (string-trim
                     (majutsu-split-test--jj-call
                      jj repo "log" "-r" "@" "--no-graph" "-T" "change_id")))
              ;; Render B while the working copy is its child C.  This proves
              ;; both guards use the rendered source instead of silently
              ;; falling back to @.
              (majutsu-split-test--jj-call jj repo "new" "-m" "C")
              (with-temp-buffer
                (setq-local default-directory (file-name-as-directory repo))
                (majutsu-diff-mode)
                (setq-local majutsu-buffer-diff-range
                            (list "--revisions" source))
                (let ((inhibit-read-only t)
                      (magit-section-inhibit-markers t))
                  (magit-insert-section (root)
                    (insert (majutsu-split-test--jj-call
                             jj repo "diff" "--git" "-r" source))
                    (save-restriction
                      (narrow-to-region (point-min) (point-max))
                      (majutsu-diff-wash-diffs '("--git")))))
                (let* ((file (majutsu-interactive--file-section-for-file
                              "notes.txt"))
                       (hunk (car (majutsu-interactive--file-section-hunks file)))
                       (beg (save-excursion
                              (goto-char (oref hunk content))
                              (forward-line 1)
                              (point)))
                       (end (save-excursion
                              (goto-char beg)
                              (forward-line 1)
                              (point))))
                  (should file)
                  (should hunk)
                  (let ((transient-current-command 'majutsu-split)
                        (transient-mark-mode t))
                    (goto-char beg)
                    (set-mark end)
                    (setq mark-active t)
                    (majutsu-interactive-toggle-region))
                  (should (eq majutsu-interactive--selection-operation
                              'majutsu-split))
                  (let ((origin (current-buffer))
                        (majutsu-jj-executable jj)
                        (majutsu-jj-global-arguments
                         '("--no-pager" "--color=never"
                           "--config" "user.name=Majutsu Test"
                           "--config" "user.email=majutsu@example.invalid"))
                        (majutsu-process-popup-time -1)
                        (deadline (+ (float-time) 10)))
                    (cl-letf (((symbol-function 'majutsu--process-display-buffer)
                               (lambda (&rest _) nil))
                              ((symbol-function 'majutsu-refresh)
                               (lambda () nil))
                              ((symbol-function 'majutsu-mode-get-buffers)
                               (lambda (&rest _) (list origin))))
                      (setq process
                            (majutsu-split-execute
                             (list (concat "--revision=" source)
                                   "--message=selected")))
                      (should (processp process))
                      (while (and (process-live-p process)
                                  (< (float-time) deadline))
                        (accept-process-output process 0.05))
                      (when (process-live-p process)
                        (ignore-errors (delete-process process))
                        (ert-fail "Timed out waiting for region split"))
                      (unless (zerop (process-exit-status process))
                        (ert-fail
                         (with-current-buffer (process-buffer process)
                           (buffer-string))))
                      (while (and (majutsu-interactive-has-selections-p)
                                  (< (float-time) deadline))
                        (sit-for 0.01))
                      (should-not
                       (majutsu-interactive-has-selections-p)))))))
            (should (equal (majutsu-split-test--jj-call
                            jj repo "file" "show" "-r" source "notes.txt")
                           "base\nselected addition\n"))
            (should (equal (majutsu-split-test--jj-call
                            jj repo "file" "show" "-r"
                            (format "children(%s)" source) "notes.txt")
                           "base\nselected addition\nunselected addition\n"))
            (should (equal
                     (string-trim
                      (majutsu-split-test--jj-call
                       jj repo "log" "-r" source "--no-graph" "-T" "description"))
                     "selected")))
        (when (and (processp process) (process-live-p process))
          (ignore-errors (delete-process process)))
        (when (file-directory-p parent)
          (delete-directory parent t))))))

(provide 'majutsu-split-test)
;;; majutsu-split-test.el ends here
