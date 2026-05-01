;;; majutsu-evolog-test.el --- Tests for evolog buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for jj evolog integration.

;;; Code:

(require 'ert)
(require 'majutsu-evolog)

(defun majutsu-evolog-test--entry-line (&optional change-id change-short commit-id commit-short op-id op-short graph-prefix)
  "Return one graph-prefixed structured evolog entry line for tests."
  (concat (or graph-prefix "@  ")
          (majutsu-graph-record--start-token majutsu-evolog--record-name)
          (string-join (list (or change-id "change-full")
                             (or change-short "change-short")
                             (or commit-id "commit-full")
                             (or commit-short "commit-short")
                             "author@example.com"
                             "2 minutes ago"
                             ""
                             "conflict"
                             "empty"
                             ""
                             "description"
                             (or op-id "op-full")
                             (or op-short "op-short")
                             "operation description")
                       majutsu-evolog--field-separator)
          (majutsu-graph-record--end-token majutsu-evolog--record-name)))

(ert-deftest majutsu-evolog-entry-template/carries-full-ids-and-option-operation ()
  "Evolog template should carry entry ids and optional operation fields."
  (should (string-match-p "self.commit().change_id()"
                          majutsu-evolog--entry-template))
  (should (string-match-p "self.commit().commit_id()"
                          majutsu-evolog--entry-template))
  (should (string-match-p "self.operation()"
                          majutsu-evolog--entry-template))
  (should (string-match-p (regexp-quote "\\x1DGR:evolog:S")
                          majutsu-evolog--entry-template))
  (should-not (string-match-p "separate("
                              majutsu-evolog--entry-template)))

(ert-deftest majutsu-evolog-parse-entry-line/keeps-full-and-display-ids ()
  "Evolog parser should keep full ids and display fields separately."
  (let* ((line (concat "○  "
                       (majutsu-graph-record--start-token majutsu-evolog--record-name)
                       (string-join
                        (list "change-full"
                              (propertize "change-short" 'font-lock-face 'success)
                              "commit-full"
                              (propertize "commit-short" 'font-lock-face 'warning)
                              "author@example.com"
                              "2 minutes ago"
                              "hidden"
                              ""
                              "empty"
                              ""
                              "description"
                              ""
                              ""
                              "")
                        majutsu-evolog--field-separator)
                       (majutsu-graph-record--end-token majutsu-evolog--record-name)))
         (entry (majutsu-evolog--parse-entry-line line)))
    (should (equal (plist-get entry :graph-prefix) "○  "))
    (should (equal (plist-get entry :change-id) "change-full"))
    (should (equal (plist-get entry :commit-id) "commit-full"))
    (should (equal (plist-get entry :change-id-short) "change-short"))
    (should (get-text-property 0 'font-lock-face
                               (plist-get entry :change-id-short-display)))
    (should (plist-get entry :hidden))
    (should-not (plist-get entry :conflict))
    (should (plist-get entry :empty))
    (should (equal (plist-get entry :operation-id) ""))))

(ert-deftest majutsu-evolog-command-args/use-read-only-top-level-args ()
  "Evolog list commands should avoid working-copy snapshots."
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

(ert-deftest majutsu-evolog-show-command-args/use-inter-diff-template ()
  "Evolog show commands should use the entry commit id and inter_diff()."
  (let ((args (majutsu-evolog--show-command-args "commit-full")))
    (should (member "--at-op=@" args))
    (should (member "--ignore-working-copy" args))
    (should (member "--limit=1" args))
    (should (member "commit_id(commit-full)" args))
    (should (member majutsu-evolog--inter-diff-template args))))

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

(ert-deftest majutsu-evolog-refresh-buffer/renders-entry-sections ()
  "Evolog refresh should wash entries into sections."
  (cl-letf (((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep-error &rest _args)
               (insert (majutsu-evolog-test--entry-line))
               (insert "\n")
               (funcall washer nil)
               0)))
    (with-temp-buffer
      (majutsu-evolog-mode)
      (setq majutsu-evolog--revset "@")
      (let ((inhibit-read-only t))
        (majutsu-evolog-refresh-buffer))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "@  change-short commit-short" content))
        (should (string-match-p "description" content))
        (should-not (string-match-p "Operation id: op-full" content))
        (should-not (string-match-p (regexp-quote majutsu-evolog--field-separator)
                                    content)))
      (goto-char (point-min))
      (search-forward "commit-short")
      (let ((section (magit-current-section))
            (entry (magit-section-value-if 'jj-evolog-entry)))
        (should (oref section hidden))
        (should (equal (plist-get entry :commit-id) "commit-full"))))))

(ert-deftest majutsu-evolog-show-at-point/uses-entry-commit-id ()
  "Evolog entry action should show the selected entry patch."
  (let ((entry (majutsu-evolog--parse-entry-line (majutsu-evolog-test--entry-line)))
        captured)
    (with-temp-buffer
      (majutsu-evolog-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-evolog-entry entry)
          (magit-insert-heading "entry")))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'majutsu-evolog-show)
                 (lambda (commit-id)
                   (setq captured commit-id))))
        (majutsu-evolog-show-at-point))
      (should (equal captured "commit-full")))))

(ert-deftest majutsu-evolog-show/passes-commit-binding-to-buffer ()
  "majutsu-evolog-show should remember the entry commit id."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-evolog-show--buffer-name)
               (lambda (_commit-id) "*evolog-show*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-evolog-show "commit-full") 'buffer))
      (should (eq (nth 0 captured) #'majutsu-evolog-show-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-evolog-show--commit-id "commit-full"))))
      (should (equal (nth 3 captured)
                     '(:buffer "*evolog-show*" :directory "/repo/"))))))

(provide 'majutsu-evolog-test)
;;; majutsu-evolog-test.el ends here
