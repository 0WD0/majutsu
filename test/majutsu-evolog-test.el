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
          (replace-regexp-in-string
           "\n" "\n│  "
           (concat
            (or heading
                (concat "qsustnur wd@example.com 2026-05-02 06:23:59 f6af8071\n"
                        "feat(op): phase1\n"
                        "-- operation 86978b4db954 describe commit 8794efe3"))
            "\n")
           t t)
          majutsu-row-tail-token
          majutsu-row-body-token
          majutsu-row-meta-token
          "\"change-full\"" majutsu-row-field-separator
          "\"commit-full\"" majutsu-row-field-separator
          "\"op-full\""
          majutsu-row-end-token
          "\n"))

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

(ert-deftest majutsu-evolog-refresh/discards-partial-output-on-error ()
  "Valid-looking rows from a failed backend must not become actionable."
  (cl-letf (((symbol-function 'majutsu-process-file)
             (lambda (&rest _args)
               (insert (majutsu-evolog-test--raw-entry))
               1)))
    (with-temp-buffer
      (majutsu-evolog-mode)
      (setq-local majutsu-evolog--revset "@")
      (let ((inhibit-read-only t))
        (majutsu-evolog-refresh-buffer))
      (should-not majutsu-row-cached-entries)
      (magit-map-sections
       (lambda (section)
         (should-not (magit-section-match 'jj-evolog-entry section)))
       magit-root-section))))

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

(ert-deftest majutsu-evolog-inter-diff-command-args/rejects-invalid-id ()
  (dolist (id '(nil "" "change-id" "abc|def" "abc def"))
    (should-error (majutsu-evolog--inter-diff-command-args id)
                  :type 'user-error)))

(ert-deftest majutsu-evolog-inter-diff-refresh/sectionizes-complete-git-patch ()
  "Description and file diffs both retain their read-only section types."
  (cl-letf (((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep &rest args)
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
      (setq-local majutsu-evolog-diff--commit-id "0123abcdef")
      (let ((inhibit-read-only t))
        (majutsu-evolog-diff-refresh-buffer))
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
        (should (cl-typep hunk 'majutsu-evolog-hunk-section))))))

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

(provide 'majutsu-evolog-test)
;;; majutsu-evolog-test.el ends here
