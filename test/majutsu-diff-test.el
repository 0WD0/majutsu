;;; majutsu-diff-test.el --- Tests for diff section rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for diff section rendering.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu)

(defmacro majutsu-diff-test--with-transient-context (prefix suffix &rest body)
  "Run BODY as if transient PREFIX were reading infix SUFFIX."
  `(cl-letf (((symbol-function 'transient-prefix-object)
              (lambda () (get ,prefix 'transient--prefix)))
             ((symbol-function 'transient-suffix-object)
              (lambda (&optional _command)
                (get ,suffix 'transient--suffix))))
     ,@body))

(ert-deftest majutsu-diff-inserts-toggleable-sections ()
  "Diff sections should create headings so users can toggle them."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (diff (string-join
                  '("diff --git a/foo b/foo"
                    "index 1234567..89abcde 100644"
                    "--- a/foo"
                    "+++ b/foo"
                    "@@ -1 +1 @@"
                    "-foo"
                    "+bar")
                  "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-heading "Diff")
        (majutsu--insert-diff-hunks diff))
      (let* ((root magit-root-section)
             (file-section (car (oref root children)))
             (hunk-section (car (oref file-section children))))
        (should (eieio-object-p file-section))
        (should (oref file-section content))
        (should (eieio-object-p hunk-section))
        (should (oref hunk-section content))))))

(ert-deftest majutsu-diff-wash-diffs-parses-diffstat-and-diff ()
  "Diff washer should parse output containing both `--stat' and `--git'."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (propertize
                    (string-join
                     '("foo | 1 +"
                       "1 file changed, 1 insertion(+), 0 deletions(-)"
                       "diff --git a/foo b/foo"
                       "index 1234567..89abcde 100644"
                       "--- a/foo"
                       "+++ b/foo"
                       "@@ -1 +1 @@"
                       "-foo"
                       "+bar")
                     "\n")
                    'fontified nil)))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-diff-wash-diffs '("--stat")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (children (oref diff-root children))
             (diffstat (seq-find (lambda (sec) (eq (oref sec type) 'diffstat))
                                 children))
             (diffstat-file (seq-find (lambda (sec)
                                        (eq (oref sec type) 'jj-file))
                                      (oref diffstat children)))
             (diff-file (seq-find (lambda (sec)
                                    (and (eq (oref sec type) 'jj-file)
                                         (equal (oref sec value) "foo")))
                                  children)))
        (should (eieio-object-p diffstat))
        (should (eieio-object-p diffstat-file))
        (should (seq-find (lambda (sec)
                            (and (eq (oref sec type) 'jj-file)
                                 (equal (oref sec value) "foo")))
                          (oref diffstat children)))
        (should (eieio-object-p diff-file))
        (should (oref diff-file content))
        (should-not (text-properties-at 0 (oref diffstat-file value)))
        (should-not (text-properties-at 0 (oref diff-file value)))))))

(ert-deftest majutsu-insert-diff/puts-filesets-after-separator ()
  "Diff command construction should pass filesets after --."
  (let (called heading)
    (with-temp-buffer
      (magit-section-mode)
      (majutsu-diff-mode)
      (setq buffer-read-only nil
            majutsu-buffer-diff-args '("--git")
            majutsu-buffer-diff-range '("--from=A" "--to=B")
            majutsu-buffer-diff-filesets '("src/a.el"))
      (cl-letf (((symbol-function 'majutsu-diff--backend-washer)
                 (lambda (&rest _) #'ignore))
                ((symbol-function 'magit-insert-heading)
                 (lambda (text) (setq heading text)))
                ((symbol-function 'majutsu-jj-wash)
                 (lambda (_washer _keep-error args)
                   (setq called args))))
        (majutsu-insert-diff)
        (should (equal called
                       '("diff" "--git" "--from=A" "--to=B"
                         "--" "src/a.el")))
        (should (equal heading
                       "jj diff --git --from=A --to=B -- src/a.el"))))))

(ert-deftest majutsu-diff-remembered-args-filters-only-formatting-options ()
  "Only diff formatting options should be remembered per buffer."
  (should (equal (majutsu-diff--remembered-args
                  '("--stat"
                    "-r" "@-"
                    "--from" "main"
                    "--context=5"
                    "--ignore-all-space"))
                 '("--stat" "--context=5" "--ignore-all-space"))))

(ert-deftest majutsu-diff-refresh-buffer-does-not-ansi-wash-git-diff ()
  "Git diff buffers must preserve literal ANSI escapes in file content."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git"))
    (setq-local majutsu-buffer-diff-range '("--revisions=@"))
    (let (seen-ansi seen-global-args)
      (cl-letf (((symbol-function 'magit-run-section-hook)
                 (lambda (&rest _)
                   (setq seen-ansi majutsu-process-apply-ansi-colors
                         seen-global-args majutsu-jj-global-arguments))))
        (majutsu-diff-refresh-buffer))
      (should-not seen-ansi)
      (should (member "--color=never" seen-global-args)))))

(ert-deftest majutsu-diff-set-buffer-args-does-not-clear-filesets ()
  "Updating diff args must not clear existing filesets unless requested."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-filesets '("a" "b"))
    (cl-letf (((symbol-function 'majutsu-diff-refresh-buffer) #'ignore))
      (majutsu-diff--set-buffer-args '("--summary")))
    (should (equal majutsu-buffer-diff-filesets '("a" "b")))
    (should (equal majutsu-buffer-diff-args '("--summary")))))

(ert-deftest majutsu-diff-refresh-keeps-transient-filesets ()
  "Refreshing from diff transient should update args, range and filesets."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let (refreshed)
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (&rest _)
                   '(("--stat") ("--from=A" "--to=B") ("src/a.el"))))
                ((symbol-function 'majutsu-diff-refresh-buffer)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'majutsu-repository-config-id) #'ignore))
        (majutsu-diff-refresh))
      (should refreshed)
      (should (equal majutsu-buffer-diff-args '("--stat")))
      (should (equal majutsu-buffer-diff-range '("--from=A" "--to=B")))
      (should (equal majutsu-buffer-diff-filesets '("src/a.el"))))))

(ert-deftest majutsu-diff-transient-revset-completion-args/uses-transient-objects ()
  "Transient revset readers should complete in the matching jj context."
  (majutsu-diff-test--with-transient-context
      'majutsu-diff 'majutsu-diff:--from
    (should (equal (majutsu-transient-revset-completion-args)
                   '("diff" "--from"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-ediff 'majutsu-ediff:--from
    (should (equal (majutsu-transient-revset-completion-args)
                   '("diff" "--from"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-rebase 'majutsu-rebase:--after
    (should (equal (majutsu-transient-revset-completion-args)
                   '("rebase" "--insert-after"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-new 'majutsu-new:-r
    (should (equal (majutsu-transient-revset-completion-args)
                   '("new" "-r")))))

(ert-deftest majutsu-diff-transient-revset-completion-args/uses-prefix-jj-command ()
  "Transient revset completion should use the prefix jj command slot."
  (let ((prefix (make-instance 'majutsu-jj-transient-prefix
                               :command 'test-prefix
                               :jj-command '("custom" "command"))))
    (cl-letf (((symbol-function 'transient-prefix-object)
               (lambda () prefix))
              ((symbol-function 'transient-suffix-object)
               (lambda (&optional _command)
                 (get 'majutsu-diff:--from 'transient--suffix))))
      (should (equal (majutsu-transient-revset-completion-args)
                     '("custom" "command" "--from"))))))

(ert-deftest majutsu-diff-transient-read-revset/uses-native-completion-context ()
  "Transient single-revision readers should pass native jj completion context."
  (let (current-prefix-arg seen-default seen-completion-args seen-initial-input)
    (majutsu-diff-test--with-transient-context
        'majutsu-restore 'majutsu-restore:--changes-in
      (cl-letf (((symbol-function 'majutsu-read-optional-single-revset)
                 (lambda (_prompt default initial-input _history completion-args)
                   (setq seen-default default
                         seen-completion-args completion-args
                         seen-initial-input initial-input)
                   "main")))
        (should (equal (majutsu-transient-read-revset "Changes in: " "old" nil)
                       "main"))
        (should (null seen-default))
        (should (equal seen-completion-args '("restore" "--changes-in")))
        (should (equal seen-initial-input "old"))))))

(ert-deftest majutsu-diff-transient-read-revset/uses-expression-reader-for-revsets ()
  "Revset expression infixes should keep using the expression reader."
  (dolist (case '((majutsu-diff majutsu-diff:-r ("diff" "--revisions"))
                  (majutsu-simplify-parents-transient
                   majutsu-simplify-parents:--source
                   ("simplify-parents" "--source"))
                  (majutsu-rebase
                   majutsu-rebase:--branch
                   ("rebase" "--branch"))))
    (pcase-let ((`(,prefix ,suffix ,expected-completion-args) case)
                (current-prefix-arg nil)
                (seen nil))
      (majutsu-diff-test--with-transient-context prefix suffix
        (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                   (lambda (_prompt default initial-input _history completion-args)
                     (setq seen (list default initial-input completion-args))
                     "main | dev"))
                  ((symbol-function 'majutsu-read-single-revset)
                   (lambda (&rest _args)
                     (ert-fail "Should not use single reader for revset expressions"))))
          (should (equal (majutsu-transient-read-revset "Revset: " "old" nil)
                         "main | dev"))
          (should (equal seen (list nil "old" expected-completion-args))))))))

(ert-deftest majutsu-diff-transient-read-revset/empty-input-clears ()
  "Empty transient revset input should not fall back to context revision."
  (let (current-prefix-arg)
    (majutsu-diff-test--with-transient-context
        'majutsu-diff 'majutsu-diff:-r
      (cl-letf (((symbol-function 'majutsu-read-optional-revset) #'ignore)
                ((symbol-function 'majutsu-transient-default-revset)
                 (lambda () (ert-fail "Should not read context default"))))
        (should-not (majutsu-transient-read-revset "Revset: " nil nil))))))

(ert-deftest majutsu-diff-repo-default-action/is-available ()
  "The diff transient should expose generic repository-local defaults."
  (let ((suffix (transient-get-suffix 'majutsu-diff "W")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'majutsu-transient-save-repository-defaults))))

(ert-deftest majutsu-diff-selection-actions/use-session-buffer-advice ()
  "Point- or repository-sensitive diff actions should use the source buffer."
  (dolist (key '("d" "W" "g"))
    (let* ((suffix (transient-get-suffix 'majutsu-diff key))
           (command (plist-get (cdr suffix) :command))
           (prototype (get command 'transient--suffix)))
      (should suffix)
      (should (eq (oref prototype advice*)
                  #'majutsu--transient-with-selection-buffer)))))

(ert-deftest majutsu-diff--r-argument/uses-native-revset-reader ()
  "The `-r' diff infix should use the native revset reader."
  (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
    (dolist (prefix '(majutsu-diff majutsu-ediff))
      (let ((obj (seq-find (lambda (suffix)
                             (equal (oref suffix key) "-r"))
                           (transient-suffixes prefix))))
        (should obj)
        (should (eq (oref obj reader) #'majutsu-transient-read-revset))))))

(ert-deftest majutsu-diff-prefix-init/does-not-double-revisions-prefix ()
  "Initializing diff from buffer range should keep one --revisions= prefix."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git" "--stat"))
    (setq-local majutsu-buffer-diff-range '("--revisions=abc123"))
    (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
      (let* ((transient--prefix (transient--init-prefix 'majutsu-diff))
             (transient--suffixes (transient--flatten-suffixes
                                   (transient--init-suffixes 'majutsu-diff))))
        (let ((value (transient-get-value)))
          (should (member "--revisions=abc123" value))
          (should-not (member "--revisions=--revisions=abc123" value)))))))

(ert-deftest majutsu-diff-repeat-revset-reader/splits-comma-values ()
  "Custom repeat revset readers should preserve CRM comma splitting."
  (let ((obj (seq-find (lambda (suffix)
                         (equal (oref suffix key) "-r"))
                       (transient-suffixes 'majutsu-diff))))
    (should (equal (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                              (lambda (&rest _args) "a, b"))
                             ((symbol-function 'transient--show) #'ignore))
                     (majutsu-diff-test--with-transient-context
                         'majutsu-diff 'majutsu-diff:-r
                       (transient-infix-read obj)))
                   '("a" "b")))))

(ert-deftest majutsu-diff-revset-falls-back-to-default-args-when-nil ()
  "`majutsu-diff-revset' should use default formatting args when ARGS is nil."
  (let (captured-args
        captured-range)
    (cl-letf (((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (_mode _locked bindings &rest _kwargs)
                 (setq captured-args (cadr (assq 'majutsu-buffer-diff-args bindings))
                       captured-range (cadr (assq 'majutsu-buffer-diff-range bindings)))
                 (let ((buffer (generate-new-buffer " *majutsu diff revset*")))
                   (with-current-buffer buffer
                     (setq-local majutsu-buffer-diff-args captured-args))
                   buffer))))
      (let ((buffer (majutsu-diff-revset "abc123")))
        (unwind-protect
            (progn
              (should (equal captured-args '("--git" "--stat")))
              (should (equal captured-range '("--revisions=abc123"))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-diff-dwim-uses-transient-args-when-active ()
  "When called from the transient, DWIM should use current transient args."
  (let ((transient-current-command 'majutsu-diff)
        (majutsu-direct-use-buffer-arguments 'never)
        called-args
        called-files
        called-range)
    (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
               (lambda (args range filesets &rest _)
                 (setq called-args args
                       called-files filesets
                       called-range range)))
              ((symbol-function 'majutsu-diff--dwim)
               (lambda () '(commit . "abc123")))
              ((symbol-function 'transient-args)
               (lambda (&rest _) (list '("--context=9" "--stat") nil nil))))
      (call-interactively #'majutsu-diff-dwim)
      (should (equal called-args '("--context=9" "--stat")))
      (should (equal called-files nil))
      (should (equal called-range '("--revisions=abc123"))))))

(ert-deftest majutsu-diff-dwim-inherits-current-buffer-range ()
  "DWIM should reuse range and filesets from the current diff buffer."
  (let ((majutsu-direct-use-buffer-arguments 'current)
        called-args
        called-files
        called-range)
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-args '("--stat"))
      (setq-local majutsu-buffer-diff-range '("--from=main" "--to=dev"))
      (setq-local majutsu-buffer-diff-filesets '("foo" "bar"))
      (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
                 (lambda (args range filesets &rest _)
                   (setq called-args args
                         called-files filesets
                         called-range range)))
                ((symbol-function 'majutsu-diff--dwim)
                 (lambda () '(commit . "abc123"))))
        (call-interactively #'majutsu-diff-dwim)
        (should (equal called-args '("--stat")))
        (should (equal called-range '("--from=main" "--to=dev")))
        (should (equal called-files '("foo" "bar")))))))

(ert-deftest majutsu-diff-dwim-does-not-inherit-range-from-other-buffer ()
  "DWIM should not reuse range or filesets from other diff buffers."
  (let ((majutsu-direct-use-buffer-arguments 'always)
        called-files
        called-range)
    (let ((diff-buf (generate-new-buffer " *majutsu diff test*")))
      (unwind-protect
          (progn
            (with-current-buffer diff-buf
              (majutsu-diff-mode)
              (setq-local majutsu-buffer-diff-args '("--stat"))
              (setq-local majutsu-buffer-diff-range '("--from=main" "--to=dev"))
              (setq-local majutsu-buffer-diff-filesets '("foo" "bar")))
            (with-temp-buffer
              (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
                         (lambda (_args range filesets &rest _)
                           (setq called-range range
                                 called-files filesets)))
                        ((symbol-function 'majutsu-diff--dwim)
                         (lambda () '(commit . "abc123"))))
                (call-interactively #'majutsu-diff-dwim)
                (should (equal called-range '("--revisions=abc123")))
                (should (equal called-files nil)))))
        (when (buffer-live-p diff-buf)
          (kill-buffer diff-buf))))))

(ert-deftest majutsu-diff-dwim/prefers-literal-revision-at-point ()
  (cl-letf (((symbol-function 'majutsu-thing-at-point)
             (lambda (_thing &optional _no-properties)
               "main@origin"))
            ((symbol-function 'majutsu-revision-at-point)
             (lambda () "context")))
    (should (equal (majutsu-diff--dwim)
                   '(revision . "main@origin")))))

(ert-deftest majutsu-diff-refine-hunk-default-disabled ()
  "Diff refinement should be disabled by default."
  (with-temp-buffer
    (majutsu-diff-mode)
    (should-not majutsu-diff-refine-hunk)))

(ert-deftest majutsu-diff-toggle-refine-hunk-updates ()
  "Toggling refinement should update local state and trigger refresh."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let ((calls 0))
      (cl-letf (((symbol-function 'majutsu-diff--update-hunk-refinement)
                 (lambda (&rest _)
                   (setq calls (1+ calls)))))
        (should-not majutsu-diff-refine-hunk)
        (majutsu-diff-toggle-refine-hunk nil)
        (should (eq majutsu-diff-refine-hunk t))
        (should (= calls 1))
        (majutsu-diff-toggle-refine-hunk t)
        (should (eq majutsu-diff-refine-hunk 'all))
        (should (= calls 2))
        (majutsu-diff-toggle-refine-hunk t)
        (should (eq majutsu-diff-refine-hunk t))
        (should (= calls 3))
        (majutsu-diff-toggle-refine-hunk nil)
        (should-not majutsu-diff-refine-hunk)
        (should (= calls 4))))))

(ert-deftest majutsu-diff-color-words-goto-from-uses-removed-block ()
  "For shared color-words lines, removed block should target old side."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) 'removed)))
    (should (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-goto-from-uses-added-block ()
  "For shared color-words lines, added block should target new side."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) 'added)))
    (should-not (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-goto-from-falls-back-to-line-shape ()
  "When side cannot be inferred, keep line-shape fallback behavior."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) nil)))
    (should (majutsu-diff--color-words-goto-from '(:from-line 10)))
    (should-not (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-column-uses-side-aware-helper ()
  "Color-words column helper should receive side selection."
  (let ((info '(:content-column 4))
        called)
    (cl-letf (((symbol-function 'majutsu-color-words-column-at-point)
               (lambda (goto-from &optional _pos _info)
                 (setq called goto-from)
                 17)))
      (should (= (majutsu-diff--color-words-column info t) 17))
      (should called)
      (should (= (majutsu-diff--color-words-column info nil) 17))
      (should-not called))))

(provide 'majutsu-diff-test)

;;; majutsu-diff-test.el ends here
