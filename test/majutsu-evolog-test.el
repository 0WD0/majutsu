;;; majutsu-evolog-test.el --- Tests for evolog buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for jj evolog integration.

;;; Code:

(require 'ert)
(require 'transient)
(require 'majutsu-evolog)

(defun majutsu-evolog-test--raw-entry (&optional heading)
  "Return one row encoded evolog test entry with HEADING."
  (concat "○  "
          majutsu-row-start-token
          (or heading
              (concat "qsustnur wd@example.com 2026-05-02 06:23:59 f6af8071\n"
                      "│  feat(op): phase1\n"
                      "│  -- operation 86978b4db954 describe commit 8794efe3"))
          "\n│  "
          majutsu-row-tail-token
          majutsu-row-body-token
          majutsu-row-meta-token
          "change-full" majutsu-row-field-separator
          "commit-full" majutsu-row-field-separator
          "op-full"
          majutsu-row-end-token
          "\n"))

(ert-deftest majutsu-evolog-entry-template/uses-majutsu-owned-row-template ()
  "Evolog should keep visible formatting under Majutsu's control."
  (let ((template (prin1-to-string majutsu-evolog--entry-template)))
    (should (string-match-p (regexp-quote "\\x1dS") template))
    (should (string-match-p (regexp-quote "\\x1dT") template))
    (should (string-match-p (regexp-quote "\\x1dB") template))
    (should (string-match-p (regexp-quote "\\x1dM") template))
    (should (string-match-p (regexp-quote "\\x1dE") template))
    (should (string-match-p "self.commit().change_id().shortest(8)"
                            majutsu-evolog--entry-template))
    (should (string-match-p "self.commit().author().email()"
                            majutsu-evolog--entry-template))
    (should (string-match-p "self.commit().committer().timestamp().local().format"
                            majutsu-evolog--entry-template))
    (should (string-match-p "self.operation().id().short()"
                            majutsu-evolog--entry-template))
    (should (string-match-p
             "config(\"ui.show-cryptographic-signatures\").as_boolean()"
             majutsu-evolog--entry-template))
    (should (string-match-p
             "format_short_cryptographic_signature(self.commit().signature())"
             majutsu-evolog--entry-template))
    (should (string-match-p
             "label(\"description placeholder\", \"(no description set)\")"
             majutsu-evolog--entry-template))
    (should-not (string-match-p "builtin_evolog_compact"
                                majutsu-evolog--entry-template))
    (should-not (string-match-p "GR:evolog"
                                majutsu-evolog--entry-template))))

(ert-deftest majutsu-evolog-command-args/use-read-only-top-level-args-and-template ()
  "Evolog list commands should avoid snapshots and use Majutsu's template."
  (let ((args (majutsu-evolog--command-args "@" '("--limit=2" "--reversed"))))
    (should (member "--at-op=@" args))
    (should (member "--ignore-working-copy" args))
    (should (< (cl-position "--at-op=@" args :test #'equal)
               (cl-position "evolog" args :test #'equal)))
    (should (< (cl-position "--config=ui.log-word-wrap=false"
                            args :test #'equal)
               (cl-position "evolog" args :test #'equal)))
    (should (member "--limit=2" args))
    (should (member "--reversed" args))
    (should-not (member "--no-graph" args))
    (should (equal (last args 4)
                   (list "-r" "@" "-T" majutsu-evolog--entry-template)))))

(ert-deftest majutsu-evolog-validate-args/accepts-and-normalizes-native-list-options ()
  (should (equal (majutsu-evolog--validate-args
                  '("--limit=0" "--reversed" "-G"))
                 '("--limit=0" "--reversed" "--no-graph")))
  (should (equal (majutsu-evolog--validate-args '("-n" "02"))
                 '("--limit=02")))
  (should (equal (majutsu-evolog--validate-args '("--limit" "3"))
                 '("--limit=3"))))

(ert-deftest majutsu-evolog-validate-args/rejects-protocol-and-format-options ()
  (dolist (args '(("--limit=-1")
                  ("--limit=x")
                  ("--limit")
                  ("-n")
                  ("-n" "-1")
                  ("--revision=@")
                  ("-r" "@")
                  ("-T" "builtin_evolog")
                  ("--template=x")
                  ("--patch")
                  ("--git")
                  ("--color=never")
                  ("--limit=2" "--limit=3")
                  ("--reversed" "--reversed")
                  ("-G" "--no-graph")))
    (should-error (majutsu-evolog--validate-args args) :type 'user-error)))

(ert-deftest majutsu-evolog/rejects-args-before-opening-buffer ()
  (let (opened)
    (cl-letf (((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (&rest _args) (setq opened t))))
      (should-error (majutsu-evolog "@" '("--template=bad"))
                    :type 'user-error)
      (should-not opened))))

(ert-deftest majutsu-evolog-refresh/does-not-wash-jj-errors ()
  (let (keep-error)
    (cl-letf (((symbol-function 'majutsu-jj-wash)
               (lambda (_washer keep &rest _args)
                 (setq keep-error keep)
                 1)))
      (with-temp-buffer
        (majutsu-evolog-mode)
        (setq majutsu-evolog--revset "@")
        (let ((inhibit-read-only t))
          (majutsu-evolog-refresh-buffer))))
    (should-not keep-error)))

(ert-deftest majutsu-evolog-wash-output/inserts-row-sections ()
  "Evolog washer should render compact graph output as Magit sections."
  (with-temp-buffer
    (magit-section-mode)
    (setq buffer-read-only nil)
    (insert (majutsu-evolog-test--raw-entry))
    (goto-char (point-min))
    (majutsu-evolog--wash-output nil)
    (goto-char (point-min))
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "qsustnur wd@example.com" content))
      (should (string-match-p "feat(op): phase1" content))
      (should (string-match-p "-- operation 86978b4db954" content))
      (should-not (string-match-p "change-full" content))
      (should-not (string-match-p "commit-full" content)))
    (should (equal (substring-no-properties
                    (get-text-property (point) 'line-prefix))
                   "○  "))
    (should (equal (magit-section-value-if 'jj-evolog-entry)
                   "commit-full"))
    (search-forward "feat(op): phase1")
    (beginning-of-line)
    (should (equal (substring-no-properties
                    (get-text-property (point) 'line-prefix))
                   "│  "))
    (should (= (length majutsu-row-cached-entries) 1))
    (let ((entry (car majutsu-row-cached-entries)))
      (should (equal (majutsu-row-column entry 'change-id) "change-full"))
      (should (equal (majutsu-row-column entry 'commit-id) "commit-full"))
      (should (equal (majutsu-row-column entry 'operation-id) "op-full")))))

(ert-deftest majutsu-evolog-refresh-buffer/inserts-compact-entry-sections ()
  "Evolog refresh should render compact row sections, not details."
  (cl-letf (((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep-error &rest _args)
               (insert (majutsu-evolog-test--raw-entry))
               (funcall washer nil)
               0)))
    (with-temp-buffer
      (majutsu-evolog-mode)
      (setq majutsu-evolog--revset "@")
      (let ((inhibit-read-only t))
        (majutsu-evolog-refresh-buffer))
      (goto-char (point-min))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "qsustnur wd@example.com" content))
        (should (string-match-p "-- operation 86978b4db954" content))
        (should-not (string-match-p "Change: change-full" content))
        (should-not (string-match-p "Operation id: op-full" content)))
      (search-forward "qsustnur")
      (should (equal (magit-section-value-if 'jj-evolog-entry)
                     "commit-full")))))

(ert-deftest majutsu-evolog-copy-transient-has-copy-actions ()
  "Evolog copy transient should expose shared row copy actions."
  (should (transient-get-suffix 'majutsu-evolog-copy-transient "s"))
  (should (transient-get-suffix 'majutsu-evolog-copy-transient "f"))
  (should (transient-get-suffix 'majutsu-evolog-copy-transient "F"))
  (should (transient-get-suffix 'majutsu-evolog-copy-transient "h"))
  (should (transient-get-suffix 'majutsu-evolog-copy-transient "m")))

(ert-deftest majutsu-evolog-transient/has-safe-list-options ()
  (let ((limit (get 'majutsu-evolog:--limit 'transient--suffix)))
    (should limit)
    (should (eq (oref limit reader) #'transient-read-number-N0)))
  (should (transient-get-suffix 'majutsu-evolog-transient "-v"))
  (should (transient-get-suffix 'majutsu-evolog-transient "-G"))
  (should (transient-get-suffix 'majutsu-evolog-transient "g"))
  (should (transient-get-suffix 'majutsu-evolog-transient "0")))

(ert-deftest majutsu-evolog-inter-diff-command-args/uses-upstream-git-patch ()
  (should
   (equal (majutsu-evolog--inter-diff-command-args "0123abcdef")
          (append majutsu-evolog--read-only-global-args
                  '("evolog" "--no-graph" "--limit=1"
                    "--patch" "--git"
                    "-r" "commit_id(0123abcdef)" "-T" "")))))

(ert-deftest majutsu-evolog-inter-diff-command-args/rejects-invalid-id ()
  (dolist (id '(nil "" "change-id" "abc|def" "abc def"))
    (should-error (majutsu-evolog--inter-diff-command-args id)
                  :type 'user-error)))

(ert-deftest majutsu-evolog-diff-at-point/uses-canonical-commit-id ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-row-current-entry)
               (lambda (&optional _message)
                 '(:columns ((commit-id . "0123abcdef")))))
              ((symbol-function 'majutsu-evolog-inter-diff)
               (lambda (commit-id) (setq seen commit-id))))
      (majutsu-evolog-diff-at-point))
    (should (equal seen "0123abcdef"))))

(ert-deftest majutsu-evolog-inter-diff/opens-locked-buffer-with-full-id ()
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-evolog-diff--buffer-name)
               (lambda (_commit-id) "*evolog-diff*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-evolog-inter-diff "0123abcdef") 'buffer))
      (should (eq (nth 0 captured) #'majutsu-evolog-diff-mode))
      (should (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-evolog-diff--commit-id "0123abcdef"))))
      (should (equal (nth 3 captured)
                     '(:buffer "*evolog-diff*" :directory "/repo/"))))))

(ert-deftest majutsu-evolog-inter-diff-refresh/sectionizes-complete-git-patch ()
  (let (seen-keep seen-args seen-global-args seen-ansi)
    (cl-letf (((symbol-function 'majutsu-jj-wash)
               (lambda (washer keep &rest args)
                 (setq seen-keep keep
                       seen-args args
                       seen-global-args majutsu-jj-global-arguments
                       seen-ansi majutsu-process-apply-ansi-colors)
                 (insert (string-join
                          '("diff --git a/JJ-COMMIT-DESCRIPTION b/JJ-COMMIT-DESCRIPTION"
                            "--- JJ-COMMIT-DESCRIPTION"
                            "+++ JJ-COMMIT-DESCRIPTION"
                            "@@ -1 +1 @@"
                            "-old description"
                            "+new description"
                            "diff --git a/foo b/foo"
                            "index 1234567..89abcde 100644"
                            "--- a/foo"
                            "+++ b/foo"
                            "@@ -1 +1 @@"
                            "-foo"
                            "+bar")
                          "\n"))
                 (funcall washer args)
                 0)))
      (with-temp-buffer
        (majutsu-evolog-diff-mode)
        (setq majutsu-evolog-diff--commit-id "0123abcdef")
        (let ((inhibit-read-only t))
          (majutsu-evolog-diff-refresh-buffer))
        (should (string-match-p "foo" (buffer-string)))
        (should (magit-get-section
                 '((jj-file . "JJ-COMMIT-DESCRIPTION") (diff-root)
                   (jj-evolog-diff . "0123abcdef"))))
        (let* ((file (magit-get-section
                      '((jj-file . "foo") (diff-root)
                        (jj-evolog-diff . "0123abcdef"))))
               (hunk (seq-find (lambda (section)
                                 (magit-section-match 'jj-hunk section))
                               (oref file children))))
          (should (cl-typep file 'majutsu-evolog-file-section))
          (should (cl-typep hunk 'majutsu-evolog-hunk-section))))
    (should-not seen-keep)
    (should-not seen-ansi)
    (should (member "--color=never" seen-global-args))
    (should (= 1 (seq-count (lambda (arg) (string-prefix-p "--color" arg))
                            seen-global-args)))
    (should (equal seen-args
                   (majutsu-evolog--inter-diff-command-args "0123abcdef"))))))

(ert-deftest majutsu-evolog-inter-diff-refresh/shows-empty-patch ()
  (cl-letf (((symbol-function 'majutsu-jj-wash)
             (lambda (_washer _keep &rest _args)
               (magit-cancel-section))))
    (with-temp-buffer
      (majutsu-evolog-diff-mode)
      (setq majutsu-evolog-diff--commit-id "0123abcdef")
      (let ((inhibit-read-only t))
        (majutsu-evolog-diff-refresh-buffer))
      (should (string-match-p "No patch" (buffer-string))))))

(ert-deftest majutsu-evolog-copy-entry-field-copies-operation-id ()
  "Shared row copy should work in evolog buffers."
  (let (copied)
    (with-temp-buffer
      (majutsu-evolog-mode)
      (setq buffer-read-only nil)
      (insert (majutsu-evolog-test--raw-entry))
      (goto-char (point-min))
      (majutsu-evolog--wash-output nil)
      (goto-char (point-min))
      (search-forward "feat(op): phase1")
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (string) (setq copied string)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (apply #'format format-string args)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _collection &rest _)
                   "operation-id")))
        (majutsu-row-copy-entry-field))
      (should (equal copied "op-full")))))

(ert-deftest majutsu-evolog-filter-buffer-substring/cleans-graph-properties ()
  "Evolog copy filter should strip row display properties."
  (with-temp-buffer
    (majutsu-evolog-mode)
    (setq buffer-read-only nil)
    (insert (majutsu-evolog-test--raw-entry))
    (goto-char (point-min))
    (majutsu-evolog--wash-output nil)
    (let ((copied (filter-buffer-substring (point-min) (point-max))))
      (should (string-match-p "feat(op): phase1" copied))
      (should-not (text-property-not-all
                   0 (length copied) 'majutsu-row-module nil copied))
      (should-not (text-property-not-all
                   0 (length copied) 'line-prefix nil copied)))))

(ert-deftest majutsu-evolog/passes-revset-and-args-to-buffer ()
  "majutsu-evolog should remember revset and args in the buffer."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-evolog--buffer-name)
               (lambda (_revset) "*evolog*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-evolog "@" '("--limit=2")) 'buffer))
      (should (eq (nth 0 captured) #'majutsu-evolog-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-evolog--revset "@")
                       (majutsu-evolog--args ("--limit=2")))))
      (should (equal (nth 3 captured)
                     '(:buffer "*evolog*" :directory "/repo/"))))))

(provide 'majutsu-evolog-test)
;;; majutsu-evolog-test.el ends here
