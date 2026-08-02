;;; majutsu-squash-test.el --- Tests for squash transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for squash argument defaults and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-squash)

(ert-deftest majutsu-squash-arguments/use-transient-args-without-defaults ()
  "Opening the transient should not prefill point/diff defaults into args."
  (let ((transient-current-command 'majutsu-squash))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () '("--from=point"))))
      (should-not (majutsu-squash-arguments)))))

(ert-deftest majutsu-squash-source-values/accepts-separate-arguments ()
  "Parse repeated --from options without consuming fileset values."
  (should (equal (majutsu-squash--source-values
                  '("--from" "B" "--from=C" "--" "--from=D"))
                 '("B" "C"))))

(ert-deftest majutsu-squash-default-args/from-diff-revisions ()
  "Use diff --revisions context as default squash source."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::D"))
    (should (equal (majutsu-squash--default-args)
                   '("--from=B::D")))))

(ert-deftest majutsu-squash-default-args/from-legacy-diff-revisions ()
  "Keep all accepted diff revision spellings when seeding squash."
  (dolist (range '(("-r" "B::D")
                   ("--revisions" "B::D")
                   ("-rB::D")))
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-range range)
      (should (equal (majutsu-squash--default-args)
                     '("--from=B::D"))))))

(ert-deftest majutsu-squash-default-args/does-not-inherit-diff-from-to ()
  "Do not inherit arbitrary diff --from/--to range for squash."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=A" "--to=D"))
    (should-not (majutsu-squash--default-args))))

(ert-deftest majutsu-squash-default-args/from-log-region ()
  "Use selected log commits as default sources."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) '("B" "C")))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) "ignored")))
    (should (equal (majutsu-squash--default-args)
                   '("--from=B" "--from=C")))))

(ert-deftest majutsu-squash-default-args/from-log-point ()
  "Use commit at point as default source."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (&rest _) "B")))
    (should (equal (majutsu-squash--default-args)
                   '("--from=B")))))

(ert-deftest majutsu-squash-patch-source/rejects-arbitrary-from-to-diff ()
  "Squash patch selection is unavailable for arbitrary from/to diff buffers."
  (dolist (range '(("--from=A" "--to=B")
                   ("--from" "A" "--to" "B")))
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-range range)
      (should-not (majutsu-squash--patch-source-revset (current-buffer))))))

(ert-deftest majutsu-squash-patch-source/requires-exactly-one-commit ()
  "A range revset must not be mistaken for one patch-selection source."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let (calls)
      (cl-letf (((symbol-function 'majutsu-jj-string)
                 (lambda (&rest args)
                   (push args calls)
                   nil)))
        (should-not (majutsu-squash--patch-source-revset (current-buffer)))
        ;; Rendering the transient asks this predicate more than once; it must
        ;; not synchronously query jj for each entry.
        (should-not (majutsu-squash--patch-source-revset (current-buffer))))
      (should (equal calls
                     '(("--ignore-working-copy" "log" "-r"
                        "exactly((B::C), 1)"
                        "--no-graph" "--limit" "1" "-T" "commit_id")))))))

(ert-deftest majutsu-squash-patch-source/accepts-one-commit-revset ()
  "A one-commit revset remains valid in every supported diff spelling."
  (dolist (range '(("--revisions=B")
                   ("--revisions" "B")
                   ("-r" "B")
                   ("-rB")))
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-range range)
      (cl-letf (((symbol-function 'majutsu-jj-string)
                 (lambda (&rest _) "commit-id")))
        (should (equal (majutsu-squash--patch-source-revset (current-buffer))
                       "B"))))))

(ert-deftest majutsu-squash-patch-source/invalidates-after-diff-refresh ()
  "A dynamic revset is checked again after the rendered diff changes."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=mine()"))
    (let ((result "commit-id") calls)
      (cl-letf (((symbol-function 'majutsu-jj-string)
                 (lambda (&rest _)
                   (setq calls (1+ (or calls 0)))
                   result)))
        (should (equal (majutsu-squash--patch-source-revset (current-buffer))
                       "mine()"))
        (let ((inhibit-read-only t))
          (insert "refreshed"))
        (setq result nil)
        (should-not (majutsu-squash--patch-source-revset (current-buffer)))
        (should (= calls 2))))))

(ert-deftest majutsu-squash-execute/rejects-range-patch-selection ()
  "Never replay one aggregate patch for each commit in a squash range."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let (applied)
      (cl-letf (((symbol-function 'majutsu-jj-string)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-interactive-build-patch-if-selected)
                 (lambda (&rest _) "PATCH"))
                ((symbol-function 'majutsu-interactive-run-with-patch)
                 (lambda (&rest _)
                   (setq applied t))))
        (should-error (majutsu-squash-execute nil) :type 'user-error)
        (should-not applied)))))

(defun majutsu-squash-test--jj-call (program directory &rest args)
  "Run PROGRAM with ARGS in DIRECTORY, returning its standard output.
Signal an ERT failure if the command exits unsuccessfully."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory directory))
           (exit (apply #'call-process
                        program nil t nil
                        (append '("--no-pager" "--color=never"
                                  "--config" "user.name=\"Majutsu Test\""
                                  "--config" "user.email=\"majutsu@example.invalid\"")
                                args))))
      (unless (zerop exit)
        (ert-fail (format "jj failed (%d): %s\n%s"
                          exit (string-join args " ") (buffer-string))))
      (buffer-string))))

(ert-deftest majutsu-squash/integration-configured-editor-runs-on-each-range-source ()
  "A configured native jj editor must run once for every range source."
  (let ((jj (or (let ((configured (getenv "MAJUTSU_TEST_JJ")))
                  (and configured (file-executable-p configured) configured))
                (executable-find "jj"))))
    (skip-unless jj)
    (let* ((parent (make-temp-file "majutsu-squash-integration-" t))
           (repo (expand-file-name "repo" parent))
           (tool (expand-file-name "count-tool" parent))
           (calls (expand-file-name "tool-calls" parent))
           (destination nil)
           (session nil)
           (process nil)
           (majutsu-diff-editor--live-sessions (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (majutsu-squash-test--jj-call jj parent "git" "init" repo)
            (with-temp-file tool
              (insert "#!/bin/sh\n"
                      "if test -e \"$2/c.txt\"; then\n"
                      "  printf 'C\\n'\n"
                      "else\n"
                      "  printf 'B\\n'\n"
                      "fi >> "
                      (shell-quote-argument calls)
                      "\nexit 0\n"))
            (set-file-modes tool #o755)
            (let ((default-directory (file-name-as-directory repo)))
              (with-temp-file (expand-file-name "base.txt" repo)
                (insert "base\n"))
              (majutsu-squash-test--jj-call jj repo "describe" "-m" "A")
              (setq destination
                    (string-trim
                     (majutsu-squash-test--jj-call
                      jj repo "log" "-r" "@" "--no-graph" "-T" "change_id")))
              (majutsu-squash-test--jj-call jj repo "new" "-m" "B")
              (with-temp-file (expand-file-name "b.txt" repo)
                (insert "B\n"))
              (majutsu-squash-test--jj-call jj repo "new" "-m" "C")
              (let ((source-b (string-trim
                               (majutsu-squash-test--jj-call
                                jj repo "log" "-r" "@-" "--no-graph"
                                "-T" "change_id"))))
                (with-temp-file (expand-file-name "c.txt" repo)
                  (insert "C\n"))
                (majutsu-squash-test--jj-call jj repo "new" "-m" "D")
                (let ((source-c (string-trim
                                 (majutsu-squash-test--jj-call
                                  jj repo "log" "-r" "@-" "--no-graph"
                                  "-T" "change_id"))))
                  (with-temp-buffer
                    (setq-local default-directory (file-name-as-directory repo))
                    (majutsu-diff-mode)
                    (setq-local majutsu-buffer-diff-range
                                (list "-r" (format "%s::%s"
                                                   source-b source-c)))
                    (let* ((majutsu-jj-executable jj)
                           (majutsu-jj-global-arguments
                            (list "--no-pager" "--color=never"
                                  "--config" "user.name=\"Majutsu Test\""
                                  "--config" "user.email=\"majutsu@example.invalid\""
                                  "--config"
                                  (format "ui.diff-editor=%S" "count")
                                  "--config"
                                  (format "merge-tools.count.program=%S" tool)
                                  "--config"
                                  "merge-tools.count.edit-args=[\"$left\", \"$right\"]"
                                  "--config"
                                  "merge-tools.count.edit-invocation-mode=\"dir\""))
                           (majutsu-diff-editor-host 'process)
                           (majutsu-process-popup-time -1))
                      (should-not
                       (majutsu-squash-interactive-selection-available-p))
                      (cl-letf
                          (((symbol-function
                             'majutsu-interactive-build-patch-if-selected)
                            (lambda (&rest _) nil))
                           ((symbol-function 'majutsu--process-display-buffer)
                            (lambda (&rest _) nil))
                           ((symbol-function 'majutsu-diff-editor--refresh-origin)
                            (lambda (&rest _) nil)))
                        (let ((deadline (+ (float-time) 10)))
                          (setq session
                                (majutsu-squash-execute
                                 (list "-i" "--message" "range"
                                       (concat "--into=" destination))))
                          (setq process
                                (majutsu-diff-editor-session-process session))
                          (let ((root
                                 (majutsu-diff-editor-session-repository-root
                                  session)))
                            (should (equal root (file-name-as-directory repo)))
                            (should (processp process))
                            (while (and (process-live-p process)
                                        (< (float-time) deadline))
                              (accept-process-output process 0.05))
                            (when (process-live-p process)
                              (ignore-errors (delete-process process))
                              (ert-fail "Timed out waiting for range squash"))
                            (should (zerop (process-exit-status process)))
                            ;; Session completion is deferred out of the process
                            ;; sentinel; keep the UI stubs active until it frees
                            ;; the per-repository slot.
                            (while (and (gethash root majutsu-diff-editor--live-sessions)
                                        (< (float-time) deadline))
                              (sit-for 0.01))
                            (should-not (gethash root
                                                 majutsu-diff-editor--live-sessions))))))))
            (should (file-exists-p calls))
            (with-temp-buffer
              (insert-file-contents calls)
              (should (equal (sort (split-string (buffer-string) "\n" t)
                                   #'string<)
                             '("B" "C"))))
            (should (equal (majutsu-squash-test--jj-call
                            jj repo "file" "show" "-r" "@" "b.txt")
                           "B\n"))
            (should (equal (majutsu-squash-test--jj-call
                            jj repo "file" "show" "-r" "@" "c.txt")
                           "C\n"))
            (should (equal
                     (string-trim
                      (majutsu-squash-test--jj-call
                       jj repo "log" "-r" destination "--no-graph"
                       "-T" "description"))
                     "range")))
        (when (and (processp process) (process-live-p process))
          (ignore-errors (delete-process process)))
        (when session
          (ignore-errors (majutsu-diff-editor--unregister-session session)))
        (when (file-directory-p parent)
          (delete-directory parent t))))))))

(ert-deftest majutsu-squash-execute/runs-jj-squash-with-inferred-destination ()
  "Execute non-patch squash through `majutsu-run-jj-with-editor'."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute '("--from=B"))
      (should (equal called
                     '(("squash" "--from=B" "--into=parents(roots((B)))")))))))

(ert-deftest majutsu-squash-execute/defaults-to-working-copy-parent ()
  "No source defaults to @ and its external parent."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () nil))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute nil)
      (should (equal called
                     '(("squash" "--from=@" "--into=parents(roots((@)))")))))))

(ert-deftest majutsu-squash-execute/uses-context-default-source ()
  "Execution-time context defaults become --from when user selected no source."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--default-args)
               (lambda () '("--from=B")))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute nil)
      (should (equal called
                     '(("squash" "--from=B" "--into=parents(roots((B)))")))))))

(ert-deftest majutsu-squash-execute/keeps-diff-range-with-explicit-destination ()
  "An explicit destination must not replace a diff range source with @."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=B::C"))
    (let ((origin (current-buffer)) called)
      (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
                 (lambda (&rest _) nil))
                ((symbol-function 'majutsu-diff-editor-start)
                 (lambda (&rest args)
                   (setq called args))))
        (majutsu-squash-execute '("--into=A" "--tool=count"))
        (should (equal called
                       (list "squash"
                             '("--into=A" "--tool=count" "--from=B::C")
                             nil :origin-buffer origin)))))))

(ert-deftest majutsu-squash-execute/keeps-explicit-destination ()
  "Do not infer destination when user selected one."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () "B")))
      (majutsu-squash-execute '("--from=C" "--into=A"))
      (should (equal called
                     '(("squash" "--from=C" "--into=A")))))))

(ert-deftest majutsu-squash-execute/keeps-literal-none-source-for-jj-noop ()
  "Do not add --into for literal --from=none(); let jj keep no-op behavior."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () "B")))
      (majutsu-squash-execute '("--from=none()"))
      (should (equal called
                     '(("squash" "--from=none()")))))))

(ert-deftest majutsu-squash-execute/places-structured-filesets-after-options ()
  "Transient fileset groups should be emitted after completed options."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute '(("--" "majutsu-squash.el") "--from=B"))
      (should (equal called
                     '(("squash" "--from=B" "--into=parents(roots((B)))"
                        "--" "majutsu-squash.el")))))))

(ert-deftest majutsu-squash-execute/routes-jj-editor-flags-to-diff-editor ()
  "Preserve jj editor flags and canonical squash arguments in the session."
  (let ((origin (current-buffer)))
    (dolist (editor-args '(("-i")
                           ("--interactive")
                           ("--tool" "meld")
                           ("--tool=meld")))
      (let (called)
        (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
                   (lambda (&rest _) nil))
                  ((symbol-function 'majutsu-squash--point-revision)
                   (lambda () nil))
                  ((symbol-function 'majutsu-diff-editor-start)
                   (lambda (&rest args)
                     (setq called args))))
          (majutsu-squash-execute
           (append '(("--" "src/a.el") "--from=B") editor-args))
          (should (equal (cl-subseq called 0 4)
                         (list "squash"
                               (append '("--from=B")
                                       editor-args
                                       '("--into=parents(roots((B)))"))
                               '("src/a.el")
                               :origin-buffer)))
          (should (eq (nth 4 called) origin)))))))
(ert-deftest majutsu-squash-execute/explicit-jj-editor-wins-over-patch-selection ()
  "A requested jj editor bypasses patch-source validation and keeps selection."
  (let ((origin (current-buffer)) called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-squash--patch-source-revset)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil))
              ((symbol-function 'majutsu-diff-editor-start)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest _)
                 (ert-fail "Should not replace an explicit jj editor")))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-squash-execute '("--from=B" "--tool" "meld"))
      (should (equal (cl-subseq called 0 4)
                     '("squash"
                       ("--from=B" "--tool" "meld"
                        "--into=parents(roots((B)))")
                       nil :origin-buffer)))
      (should (eq (nth 4 called) origin))
      (should-not cleared))))
(ert-deftest majutsu-squash-transient/exposes-tool-infix ()
  "Squash should expose the jj --tool option without shadowing --into."
  (should (transient-get-suffix 'majutsu-squash "=t")))

(ert-deftest majutsu-squash-execute/patch-removes-native-interactive-tool-args ()
  "Patch mode calls the shared jj-editor argument stripper."
  (let (called cleared stripped)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _)
                 (list :base 'left :payload-root 'right
                       :patch "PATCH"
                       :file-ops
                       '((:action modify :path "image.bin")))))
              ((symbol-function 'majutsu-squash--patch-source-revset)
               (lambda (&rest _) "B"))
              ((symbol-function 'majutsu-diff-editor-strip-interactive-arguments)
               (lambda (args)
                 (setq stripped args)
                 args))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t)))
              ((symbol-function 'majutsu-squash--point-revision)
               (lambda () nil)))
      (majutsu-squash-execute '("--from=B"))
      (should (equal called
                     '("squash"
                       ("--from=B" "--into=parents(roots((B)))")
                       nil
                       (:base left :payload-root right
                        :patch "PATCH"
                        :file-ops
                        ((:action modify :path "image.bin"))))))
      (should (equal stripped
                     '("--from=B" "--into=parents(roots((B)))")))
      (should-not cleared))))


(ert-deftest majutsu-squash-execute/patch-rejects-different-source ()
  "Patch mode should not apply the current diff patch to another source."
  (let (cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _)
                 '(:base left :payload-root right :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-squash--patch-source-revset)
               (lambda (&rest _) "B"))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (should-error (majutsu-squash-execute '("--from=C")) :type 'user-error)
      (should-not cleared))))

(provide 'majutsu-squash-test)
;;; majutsu-squash-test.el ends here
