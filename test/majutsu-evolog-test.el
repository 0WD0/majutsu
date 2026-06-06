;;; majutsu-evolog-test.el --- Tests for evolog buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for jj evolog integration.

;;; Code:

(require 'ert)
(require 'transient)
(require 'majutsu-evolog)

(defun majutsu-evolog-test--raw-entry (&optional heading operation-id)
  "Return one row encoded evolog test entry with HEADING and OPERATION-ID."
  (concat "○  "
          majutsu-row-start-token
          (or heading
              (concat "qsustnur wd@example.com 2026-05-02 06:23:59 f6af8071\n"
                      "│  feat(op): phase1\n"
                      "│  -- operation 86978b4db954 describe commit 8794efe3"))
          majutsu-row-tail-token
          majutsu-row-body-token
          majutsu-row-meta-token
          "change-full" majutsu-row-field-separator
          "commit-full" majutsu-row-field-separator
          (if (null operation-id) "op-full" operation-id)
          majutsu-row-end-token
          "\n"))

(ert-deftest majutsu-evolog-entry-template/rebuilds-default-compact-with-row ()
  "Evolog template should rebuild builtin_evolog_compact via row."
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
    (should (string-match-p (regexp-quote
                             "if(self.operation(), self.operation().id(), \"\")")
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
    (should (member "--limit=2" args))
    (should (member "--reversed" args))
    (should-not (member "--no-graph" args))
    (should (equal (last args 4)
                   (list "-r" "@" "-T" majutsu-evolog--entry-template)))))

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
      (should (equal (plist-get entry :change-id) "change-full"))
      (should (equal (plist-get entry :commit-id) "commit-full"))
      (should (equal (plist-get entry :operation-id) "op-full")))))

(ert-deftest majutsu-evolog-wash-output/handles-entry-without-operation ()
  "Evolog entries without an operation should still render and hide empty ids."
  (with-temp-buffer
    (magit-section-mode)
    (setq buffer-read-only nil)
    (insert (majutsu-evolog-test--raw-entry
             (concat "qsustnur wd@example.com 2026-05-02 06:23:59 f6af8071\n"
                     "│  feat(op): phase1")
             ""))
    (goto-char (point-min))
    (majutsu-evolog--wash-output nil)
    (goto-char (point-min))
    (should (equal (magit-section-value-if 'jj-evolog-entry)
                   "commit-full"))
    (let ((entry (car majutsu-row-cached-entries)))
      (should (equal (plist-get entry :operation-id) ""))
      (should-not (memq 'operation-id
                        (majutsu-row-entry-copyable-fields
                         entry majutsu-evolog--entry-compiled))))))

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
                 (lambda (_prompt collection &rest _)
                   (let ((annotation-function
                          (plist-get completion-extra-properties :annotation-function)))
                     (should (eq (plist-get completion-extra-properties :category)
                                 'majutsu-row-field))
                     (should (assoc "operation-id" collection))
                     (should (equal (funcall annotation-function "operation-id")
                                    " op-full")))
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
