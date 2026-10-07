;;; majutsu-file-test.el --- Tests for majutsu-file helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for majutsu file helpers.

;;; Code:

(require 'ert)
(require 'majutsu-file)
(require 'majutsu-annotate)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-file-revset-for-files-quotes-path ()
  "Paths with single quotes should be fileset-quoted."
  (should (equal (majutsu-file--revset-for-files "rev" "test'file" 'prev)
                 "::rev-&files(file:\"test'file\")")))

(ert-deftest majutsu-file-prev-change/handles-nil-output ()
  "Return nil when jj output is nil."
  (cl-letf (((symbol-function 'majutsu-jj-string) (lambda (&rest _args) nil)))
    (should-not (majutsu-file-prev-change "rev" "path"))))

(ert-deftest majutsu-file-next-change/handles-nil-output ()
  "Return nil when jj output is nil."
  (cl-letf (((symbol-function 'majutsu-jj-string) (lambda (&rest _args) nil)))
    (should-not (majutsu-file-next-change "rev" "path"))))

(ert-deftest majutsu-find-file-noselect/filesystem-relative-to-root ()
  "Relative FILE is resolved from the repository root for filesystem visits."
  (let* ((root (make-temp-file "majutsu-file-root-" t))
         (sub (expand-file-name "sub" root))
         (root-target (expand-file-name "note.txt" root))
         (sub-target (expand-file-name "note.txt" sub))
         (buf nil))
    (unwind-protect
        (progn
          (make-directory sub t)
          (with-temp-file root-target
            (insert "root"))
          (with-temp-file sub-target
            (insert "hello"))
          (let ((default-directory sub))
            (cl-letf (((symbol-function 'majutsu-file--root)
                       (lambda () root)))
              (setq buf (majutsu-find-file-noselect nil "note.txt"))))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (equal buffer-file-name root-target))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory root t))))

(ert-deftest majutsu-find-file-noselect/rejects-outside-repo-for-blob ()
  "Blob views reject paths outside the repository root."
  (let* ((root (make-temp-file "majutsu-file-root-" t))
         (outside (make-temp-file "majutsu-file-outside-" nil ".txt")))
    (unwind-protect
        (let ((default-directory root))
          (cl-letf (((symbol-function 'majutsu-file--root)
                     (lambda () root)))
            (should-error (majutsu-find-file-noselect "@" outside t)
                          :type 'user-error)))
      (delete-directory root t)
      (delete-file outside))))

(ert-deftest majutsu-find-file-read-args/always-prompts-for-path ()
  "Find-file args should always call path reader with context default."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-file--root)
               (lambda () "/tmp/repo"))
              ((symbol-function 'majutsu-file--default-revset)
               (lambda () "@"))
              ((symbol-function 'majutsu-read-revset)
               (lambda (_prompt &rest keys)
                 (should (equal (plist-get keys :default) "@"))
                 "chosen-rev"))
              ((symbol-function 'majutsu-file--path-at-point)
               (lambda (_root) "src/at-point.el"))
              ((symbol-function 'majutsu-file--read-path)
               (lambda (revset root default)
                 (setq seen (list revset root default))
                 "picked.el")))
      (should (equal (majutsu-find-file-read-args "Find file")
                     '("chosen-rev" "picked.el")))
      (should (equal seen '("chosen-rev" "/tmp/repo" "src/at-point.el"))))))

(ert-deftest majutsu-file-candidate-data/combines-type-and-status ()
  "File candidate payload should combine file type and diff summary data."
  (let ((sep majutsu-file--completion-field-separator))
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (pcase (car args)
                   ("diff" '("M src/a.el" "A src/new.el"))
                   ("file" (list (concat "src/a.el" sep "file" sep "t" sep "")
                                 (concat "src/new.el" sep "symlink" sep "" sep "")))
                   (_ nil)))))
      (let* ((payload (majutsu-file-candidate-data "@" "/tmp/repo/"
                                                   '("src/a.el" "src/new.el")))
             (entries (plist-get payload :entries))
             (suffix-function (plist-get payload :annotation-suffix-function))
             (a (gethash "src/a.el" entries))
             (new (gethash "src/new.el" entries)))
        (should (eq (plist-get payload :category) 'majutsu-file))
        (should (equal (plist-get payload :candidates) '("src/a.el" "src/new.el")))
        (should (equal (plist-get a :file-type) "file"))
        (should (plist-get a :executable))
        (should (equal (plist-get a :status) "modified"))
        (should (equal (plist-get new :file-type) "symlink"))
        (should (equal (plist-get new :status) "added"))
        (should (functionp suffix-function))
        (should (string-match-p "modified" (funcall suffix-function "src/a.el")))
        (should (string-match-p "executable" (funcall suffix-function "src/a.el")))
        (should (string-match-p "symlink" (funcall suffix-function "src/new.el")))))))

(ert-deftest majutsu-file-read-path/uses-history-and-category ()
  "File path reader should expose dedicated history and file category."
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'majutsu-file--list)
               (lambda (_revset _root)
                 '("src/a.el" "src/b.el")))
              ((symbol-function 'majutsu-file-candidate-data)
               (lambda (_revset _root candidates)
                 (list :category 'majutsu-file
                       :candidates candidates
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-file--path-at-point)
               (lambda (_root)
                 "src/a.el"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate _require-match _initial history _default)
                 (setq seen-history history
                       seen-category (plist-get completion-extra-properties :category))
                 (should (equal collection '("src/a.el" "src/b.el")))
                 "src/b.el")))
      (should (equal (majutsu-file--read-path "@" "/tmp/repo") "src/b.el"))
      (should (eq seen-history 'majutsu-file-path-history))
      (should (eq seen-category 'majutsu-file)))))

(ert-deftest majutsu-read-files/uses-default-history-and-category ()
  "File multi-reader should default to file-path history and category."
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'majutsu-file--root)
               (lambda ()
                 "/tmp/repo"))
              ((symbol-function 'majutsu-file-candidate-data)
               (lambda (_revset _root candidates)
                 (list :category 'majutsu-file
                       :candidates candidates
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-file--path-at-point)
               (lambda (_root)
                 "src/a.el"))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match _initial history _default)
                 (setq seen-history history
                       seen-category (plist-get completion-extra-properties :category))
                 (should (equal collection '("src/a.el" "src/b.el")))
                 '("src/b.el"))))
      (should (equal (majutsu-read-files "Files" nil nil
                                         (lambda () '("src/a.el" "src/b.el")))
                     '("src/b.el")))
      (should (eq seen-history 'majutsu-file-path-history))
      (should (eq seen-category 'majutsu-file)))))

(ert-deftest majutsu-read-file-items/uses-file-category-and-defaults ()
  "File item readers should preserve prompt defaults and metadata."
  (let (seen-items seen-category seen-history seen-initial seen-directory)
    (cl-letf (((symbol-function 'majutsu-file--root)
               (lambda () "/tmp/repo/"))
              ((symbol-function 'majutsu-file--path-at-point)
               (lambda (_root) "src/current.el"))
              ((symbol-function 'majutsu-completion-table)
               (lambda (items category)
                 (setq seen-items items
                       seen-category category)
                 'file-table))
              ((symbol-function 'majutsu-completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match
                        initial-input history _default category)
                 (should (eq collection 'file-table))
                 (setq seen-initial initial-input
                       seen-history history
                       seen-directory default-directory)
                 (should (eq category 'majutsu-file))
                 '("src/changed.el"))))
      (should
       (equal
        (majutsu-read-file-items
         "Files" nil nil '(("src/changed.el" . "Modified")))
        '("src/changed.el")))
      (should (equal seen-items '(("src/changed.el" . "Modified"))))
      (should (eq seen-category 'majutsu-file))
      (should (eq seen-history 'majutsu-file-path-history))
      (should (equal seen-initial "src/current.el"))
      (should (equal seen-directory "/tmp/repo/")))))

(ert-deftest majutsu-file-revert-buffer/detects-hidden-local-drift ()
  "Revert should prompt when content changed but modified flag is cleared."
  (let (asked)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-change-id "chg1")
      (setq-local majutsu-buffer-blob-commit-id "c1")
      (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
      (erase-buffer)
      (insert "drifted")
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _args)
                   (setq asked t)
                   nil))
                ((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (&rest _args)
                   (ert-fail "Should not resolve revision when user declines"))))
        (should-not (majutsu-file-revert-buffer nil nil))
        (should asked)
        (should (equal (buffer-string) "drifted"))))))

(ert-deftest majutsu-blob-edit-start/enables-editable-mode ()
  "Starting blob edit mode should make blob buffer writable."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (majutsu-blob-mode 1)
    (setq buffer-read-only t)
    (majutsu-blob-edit-start)
    (should majutsu-blob-edit-mode)
    (should-not majutsu-blob-mode)
    (should-not buffer-read-only)
    (should (equal majutsu-blob-edit--original-content "old"))))

(ert-deftest majutsu-blob-edit-start/does-not-force-evil-insert-state ()
  "Starting blob edit mode from Evil binding should stay in normal state."
  (let (insert-called)
    (with-temp-buffer
      (insert "old")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-commit-id "commit")
      (majutsu-blob-mode 1)
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq insert-called t))))
        (majutsu-blob-edit-start)
        (should majutsu-blob-edit-mode)
        (should-not insert-called)))))

(ert-deftest majutsu-blob-edit-mode/toggles-cursor-type ()
  "Editable mode should switch cursor type and restore it on exit."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (setq-local cursor-type 'box)
    (let ((majutsu-blob-edit-cursor-type 'bar))
      (majutsu-blob-edit-mode 1)
      (should (eq cursor-type 'bar))
      (majutsu-blob-edit-mode -1)
      (should (eq cursor-type 'box)))))

(ert-deftest majutsu-blob-edit-mode/toggles-evil-normal-cursor ()
  "Editable mode should mirror Dirvish-style Evil cursor changes."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (setq-local cursor-type '(box . 4))
    (setq-local evil-local-mode t)
    (setq-local evil-normal-state-cursor '(box . 4))
    (let ((majutsu-blob-edit-cursor-type 'hollow))
      (majutsu-blob-edit-mode 1)
      (should (eq cursor-type 'hollow))
      (should (eq evil-normal-state-cursor 'hollow))
      (majutsu-blob-edit-mode -1)
      (should (equal cursor-type '(box . 4)))
      (should (equal evil-normal-state-cursor '(box . 4))))))

(ert-deftest majutsu-blob-edit-mode/disables-annotate-read-only-mode ()
  "Editable mode should disable annotate's read-only keymap while editing."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (majutsu-blob-mode 1)
    (majutsu-annotate-mode 1)
    (should majutsu-annotate-read-only-mode)
    (majutsu-blob-edit-mode 1)
    (should-not majutsu-annotate-read-only-mode)
    (majutsu-blob-edit-mode -1)
    (should majutsu-annotate-read-only-mode)
    (majutsu-annotate-mode -1)))

(ert-deftest majutsu-blob-edit-exit/no-changes-disables-mode ()
  "Exit should just leave editable mode when there are no changes."
  (with-temp-buffer
    (insert "stable")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (majutsu-blob-mode 1)
    (majutsu-blob-edit-mode 1)
    (set-buffer-modified-p nil)
    (majutsu-blob-edit-exit)
    (should-not majutsu-blob-edit-mode)
    (should majutsu-blob-mode)))

(ert-deftest majutsu-blob-edit-finish/no-changes-disables-mode ()
  "Finish should leave editable mode when no changes exist."
  (with-temp-buffer
    (insert "stable")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (majutsu-blob-mode 1)
    (majutsu-blob-edit-mode 1)
    (majutsu-blob-edit-finish)
    (should-not majutsu-blob-edit-mode)
    (should majutsu-blob-mode)))

(ert-deftest majutsu-blob-edit-exit/decline-save-restores-entry-point ()
  "Declining save on exit should restore original content and point."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-commit-id "commit")
    (majutsu-blob-mode 1)
    (goto-char (point-min))
    (forward-line 1)
    (move-to-column 1)
    (let ((entry-point (point))
          (original (buffer-string)))
      (majutsu-blob-edit-mode 1)
      (goto-char (point-max))
      (insert "changed")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (majutsu-blob-edit-exit))
      (should-not majutsu-blob-edit-mode)
      (should majutsu-blob-mode)
      (should (equal (buffer-string) original))
      (should (= (point) entry-point)))))

(ert-deftest majutsu-blob-edit/integration-divergent-revision-editable ()
  "A blob on a divergent change opens, edits, and tracks its successor."
  (majutsu-jj-integration-with-repo repo
    (let ((file (majutsu-jj-integration-file repo "f.txt")))
      (majutsu-jj-integration-write-text file "one\n")
      (majutsu-jj-integration-call repo "describe" "-m" "v1")
      (let ((commit-a (string-trim
                       (majutsu-jj-integration-output
                        repo "log" "-r" "@" "--no-graph" "-T" "commit_id"))))
        ;; Rewrite the change, then rewrite its hidden predecessor to
        ;; make the change divergent with two visible versions.
        (majutsu-jj-integration-call repo "describe" "-m" "v2")
        (majutsu-jj-integration-call repo "describe" "-r" commit-a "-m" "v3")
        (let* ((change (string-trim
                        (majutsu-jj-integration-output
                         repo "log" "-r" "@" "--no-graph" "-T" "change_id")))
               (versions
                (lambda ()
                  (length
                   (split-string
                    (majutsu-jj-integration-output
                     repo "log" "-r" (format "change_id(%s)" change)
                     "--no-graph" "-T" "commit_id ++ \"\\n\"")
                    "\n" t)))))
          (should (= 2 (funcall versions)))
          (let* ((default-directory (file-name-as-directory repo))
                 ;; Opening by the divergent change id falls back to the
                 ;; newest visible version instead of erroring.
                 (buffer (majutsu-find-file-noselect change "f.txt")))
            (unwind-protect
                (with-current-buffer buffer
                  (should majutsu-buffer-blob-commit-id)
                  (let ((edited-commit majutsu-buffer-blob-commit-id))
                    (majutsu-blob-edit-start)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert "two\n"))
                    ;; Saving edits a divergent revision successfully...
                    (should (majutsu-blob-edit--write-contents))
                    ;; ...rewrites content at the tracked successor...
                    (should (equal (majutsu-jj-integration-output
                                    repo "file" "show" "-r"
                                    majutsu-buffer-blob-commit-id
                                    "f.txt")
                                   "two\n"))
                    ;; ...moves the pin off the now-hidden commit...
                    (should-not (equal majutsu-buffer-blob-commit-id
                                       edited-commit))
                    ;; ...and does not diverge the change any further.
                    (should (= 2 (funcall versions)))))
              (kill-buffer buffer))))))))

(ert-deftest majutsu-blob-edit/integration-external-working-copy-move ()
  "Successive saves rewrite the buffer's branch, not an external @ successor."
  (majutsu-jj-integration-with-repo repo
    (let* ((path "docs/wiki/Configuration:-Layout.md")
           (file (majutsu-jj-integration-file repo path)))
      (make-directory (file-name-directory file) t)
      (majutsu-jj-integration-write-text file "A\n")
      (majutsu-jj-integration-call repo "describe" "-m" "original")
      (let ((buffer (majutsu-find-file-noselect "@" path)))
        (unwind-protect
            (with-current-buffer buffer
              (let ((pin majutsu-buffer-blob-commit-id)
                    (change majutsu-buffer-blob-change-id))
                (majutsu-jj-integration-write-text file "B\n")
                (majutsu-jj-integration-call repo "describe" "-m" "external")
                (let ((external (majutsu-jj-integration-output
                                 repo "log" "-r" "@" "--no-graph" "-T" "commit_id")))
                  (dolist (content '("C\n" "D\n"))
                    (majutsu-blob-edit-start)
                    (erase-buffer)
                    (insert content)
                    ;; Finish must detect content changes even when Emacs's
                    ;; modified flag has been cleared by another command.
                    (set-buffer-modified-p nil)
                    (majutsu-blob-edit-finish)
                    (should-not majutsu-blob-edit-mode)
                    (should-not (buffer-modified-p))
                    (should (equal (buffer-string) content))
                    (should (equal majutsu-buffer-blob-content-hash
                                   (secure-hash 'sha1 content)))
                    (should (equal majutsu-buffer-blob-change-id change))
                    (should-not (equal majutsu-buffer-blob-commit-id pin))
                    (setq pin majutsu-buffer-blob-commit-id)
                    (should (equal (majutsu-jj-integration-output
                                    repo "file" "show" "-r" pin "--"
                                    (majutsu-jj-fileset-quote path))
                                   content))
                    (should (equal (majutsu-jj-integration-output
                                    repo "log" "-r" "@" "--no-graph" "-T" "commit_id")
                                   external))
                    (should (equal (majutsu-jj-integration-read-bytes file) "B\n"))
                    (should
                     (= 2 (length
                           (split-string
                            (majutsu-jj-integration-output
                             repo "log" "--no-graph" "-r"
                             (format "change_id(%s)" change)
                             "-T" "commit_id ++ \"\\n\"")
                            "\n" t))))))))
          (kill-buffer buffer))))))

(defun majutsu-file-test--historical-conflict (repo)
  "Create a two-parent conflict in REPO and return its commit id.
Leave the working copy on an unrelated revision."
  (let ((file (majutsu-jj-integration-file repo "f.txt")))
    (majutsu-jj-integration-write-text file "base\n")
    (majutsu-jj-integration-commit repo "base")
    (let ((base (majutsu-jj-integration-output
                 repo "log" "-r" "@-" "--no-graph" "-T" "commit_id")))
      (majutsu-jj-integration-write-text file "left\n")
      (majutsu-jj-integration-call repo "describe" "-m" "left")
      (let ((left (majutsu-jj-integration-output
                   repo "log" "-r" "@" "--no-graph" "-T" "commit_id")))
        (majutsu-jj-integration-call repo "new" base)
        (majutsu-jj-integration-write-text file "right\n")
        (majutsu-jj-integration-call repo "describe" "-m" "right")
        (majutsu-jj-integration-call repo "new" left "@" "-m" "merge")
        (prog1 (majutsu-jj-integration-output
                repo "log" "-r" "@" "--no-graph" "-T" "commit_id")
          (majutsu-jj-integration-call repo "new" "root()" "-m" "unrelated"))))))

(ert-deftest majutsu-blob-edit/integration-historical-merge-save ()
  "Resolving a historical two-parent merge rewrites that merge, not @."
  (majutsu-jj-integration-with-repo repo
    (let* ((merge (majutsu-file-test--historical-conflict repo))
           (working (majutsu-jj-integration-output
                     repo "log" "-r" "@" "--no-graph" "-T" "commit_id"))
           (buffer (majutsu-find-file-noselect merge "f.txt")))
      (unwind-protect
          (with-current-buffer buffer
            (should (string-match-p "<<<<<<<" (buffer-string)))
            (majutsu-blob-edit-start)
            (erase-buffer)
            (insert "resolved\n")
            (majutsu-blob-edit-finish)
            (should-not (equal majutsu-buffer-blob-commit-id merge))
            (should (equal
                     (majutsu-jj-integration-output
                      repo "file" "show" "-r" majutsu-buffer-blob-commit-id "f.txt")
                     "resolved\n"))
            (should (equal
                     (majutsu-jj-integration-output
                      repo "log" "-r" majutsu-buffer-blob-commit-id "--no-graph"
                      "-T" "parents.len() ++ \" \" ++ conflict")
                     "2 false"))
            (should (equal
                     (majutsu-jj-integration-output
                      repo "log" "-r"
                      (format "change_id(%s)" majutsu-buffer-blob-change-id)
                      "--no-graph" "-T" "commit_id")
                     majutsu-buffer-blob-commit-id))
            (should (equal
                     (majutsu-jj-integration-output
                      repo "log" "-r" "@" "--no-graph" "-T" "commit_id")
                     working)))
        (kill-buffer buffer)))))

(ert-deftest majutsu-blob-edit/integration-semantic-noop-keeps-hidden-pin ()
  "Restyling conflict labels must not select one of the divergent siblings."
  (majutsu-jj-integration-with-repo repo
    (let* ((merge (majutsu-file-test--historical-conflict repo))
           (buffer (majutsu-find-file-noselect merge "f.txt")))
      (unwind-protect
          (with-current-buffer buffer
            (majutsu-jj-integration-call repo "describe" "-r" merge "-m" "sibling one")
            (majutsu-jj-integration-call repo "describe" "-r" merge "-m" "sibling two")
            (let* ((revset (format "change_id(%s)" majutsu-buffer-blob-change-id))
                   (versions (majutsu-jj-integration-output
                              repo "log" "-r" revset "--no-graph"
                              "-T" "commit_id ++ \"\\n\""))
                   (operation (majutsu-jj-operation-id))
                   (original (buffer-string))
                   (restyled (replace-regexp-in-string
                              "Conflict [^\n]*" "Restyled label" original)))
              (should (= 2 (length (split-string versions "\n" t))))
              (should-not (member merge (split-string versions "\n" t)))
              (should-not (equal original restyled))
              (majutsu-blob-edit-start)
              (erase-buffer)
              (insert restyled)
              (majutsu-blob-edit-finish)
              (should (equal majutsu-buffer-blob-commit-id merge))
              (should (equal (buffer-string) restyled))
              (should-not (buffer-modified-p))
              (should-not majutsu-blob-edit--save-unverified)
              (should (equal (majutsu-jj-operation-id) operation))
              (should (equal
                       (majutsu-jj-integration-output
                        repo "log" "-r" revset "--no-graph" "-T" "commit_id ++ \"\\n\"")
                       versions))
              (should (equal
                       (majutsu-jj-integration-output
                        repo "file" "show" "-r" merge "f.txt")
                       original))))
        (kill-buffer buffer)))))

(ert-deftest majutsu-blob-edit/integration-unverified-save-requires-revert ()
  "An intervening operation must not leave a silently reusable stale pin."
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-write-text
     (majutsu-jj-integration-file repo "f.txt") "before\n")
    (majutsu-jj-integration-call repo "describe" "-m" "original")
    (let ((buffer (majutsu-find-file-noselect "@" "f.txt")))
      (unwind-protect
          (with-current-buffer buffer
            (let ((pin majutsu-buffer-blob-commit-id)
                  (call-jj (symbol-function 'majutsu-call-jj)))
              (majutsu-blob-edit-start)
              (erase-buffer)
              (insert "saved\n")
              ;; Perform the real save, then race its provenance query with
              ;; another real operation, rather than fabricating CLI output.
              (cl-letf (((symbol-function 'majutsu-call-jj)
                         (lambda (&rest args)
                           (prog1 (apply call-jj args)
                             (majutsu-jj-integration-call
                              repo "describe" "-m" "concurrent")))))
                (should-error (majutsu-blob-edit--write-contents) :type 'user-error))
              (should majutsu-blob-edit--save-unverified)
              (should (equal majutsu-buffer-blob-commit-id pin))
              (should (buffer-modified-p))
              (should (equal (buffer-string) "saved\n"))
              (let ((operation (majutsu-jj-operation-id)))
                (should-error (majutsu-blob-edit--write-contents) :type 'user-error)
                (should (equal (majutsu-jj-operation-id) operation)))
              (majutsu-blob-edit-abort)
              (majutsu-file-revert-buffer nil t)
              (should-not majutsu-blob-edit--save-unverified)
              (should-not (equal majutsu-buffer-blob-commit-id pin))
              (should (equal (buffer-string) "saved\n"))
              (majutsu-blob-edit-start)
              (erase-buffer)
              (insert "next\n")
              (majutsu-blob-edit-finish)
              (should (equal
                       (majutsu-jj-integration-output
                        repo "file" "show" "-r" majutsu-buffer-blob-commit-id "f.txt")
                       "next\n"))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest majutsu-file-apply-contents/integration-preserves-executable-bit ()
  "A private non-executable staging file must not remove the target's mode."
  (majutsu-jj-integration-with-repo repo
    (let ((path (majutsu-jj-integration-file repo "tool.sh")))
      (majutsu-jj-integration-write-text path "#!/bin/sh\nexit 1\n")
      (set-file-modes path #o755)
      (let* ((info (majutsu-file--resolve-single-rev-info "@"))
             (result (majutsu-file-apply-contents
                      (plist-get info :commit-id)
                      (plist-get info :change-id)
                      '(("tool.sh" . "#!/bin/sh\nexit 0\n")))))
        (should (equal (majutsu-jj-integration-output
                        repo "file" "show" "-r" result "tool.sh")
                       "#!/bin/sh\nexit 0\n"))
        (should (= (logand (file-modes path) #o111) #o111))))))

(ert-deftest majutsu-find-file-noselect/integration-snapshot-identity ()
  "Blob reuse and Ediff distinguish full commits and workspace roots."
  (require 'majutsu-ediff)
  (majutsu-jj-integration-with-repo repo
    (let ((path (majutsu-jj-integration-file repo "f.txt"))
          buffers)
      (unwind-protect
          (progn
            (majutsu-jj-integration-write-text path "old\n")
            (let* ((old-info (majutsu-file--resolve-single-rev-info "@"))
                   (old (plist-get old-info :commit-id))
                   (first (majutsu-find-file-noselect old "f.txt")))
              (push first buffers)
              (majutsu-jj-integration-write-text path "new\n")
              (let* ((new-info (majutsu-file--resolve-single-rev-info "@"))
                     (new (plist-get new-info :commit-id))
                     (second (majutsu-find-file-noselect new "f.txt"))
                     (other (expand-file-name
                             "other" (file-name-directory
                                      (directory-file-name repo)))))
                (push second buffers)
                (should (equal (plist-get old-info :change-id)
                               (plist-get new-info :change-id)))
                (should-not (equal old new))
                (should-not (eq first second))
                (should (eq first (majutsu-find-file-noselect old "f.txt")))
                (should (eq first (majutsu-ediff--get-revision-buffer
                                   old "f.txt")))
                (should-not (majutsu-ediff--working-copy-revision-p old))
                (should (majutsu-ediff--working-copy-revision-p new))
                (with-current-buffer first
                  (should (equal (buffer-string) "old\n"))
                  (goto-char (point-max))
                  (should (equal (majutsu-revision-at-point) old)))
                (with-current-buffer second
                  (should (equal (buffer-string) "new\n")))
                (majutsu-jj-integration-call
                 repo "workspace" "add" "--name" "other" "-r" old other)
                (let* ((default-directory (file-name-as-directory other))
                       (third (majutsu-find-file-noselect old "f.txt")))
                  (push third buffers)
                  (should-not (eq first third))
                  (with-current-buffer third
                    (should (equal (buffer-string) "old\n"))
                    (should (equal majutsu-buffer-blob-root
                                   default-directory)))))))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-blob-context/integration-restore-uses-displayed-commit ()
  "Restoring an old blob must not rewrite the latest version of its change."
  (require 'majutsu-restore)
  (majutsu-jj-integration-with-repo repo
    (let ((path (majutsu-jj-integration-file repo "f.txt")))
      (majutsu-jj-integration-write-text path "displayed\n")
      (let* ((old (plist-get (majutsu-file--resolve-single-rev-info "@")
                            :commit-id))
             (buffer (majutsu-find-file-noselect old "f.txt")))
        (unwind-protect
            (progn
              (majutsu-jj-integration-write-text path "latest\n")
              (majutsu-jj-integration-call repo "status")
              (with-current-buffer buffer
                (goto-char (point-max))
                (apply #'majutsu-jj-integration-call
                       repo "restore" (majutsu-restore--default-args)))
              (should (equal (majutsu-jj-integration-output
                              repo "file" "show" "-r" "@" "f.txt")
                             "latest\n")))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-file/integration-refuses-historical-symlink ()
  "Check the pinned tree's type, not the current workspace's regular file."
  (skip-unless (not (eq system-type 'windows-nt)))
  (majutsu-jj-integration-with-repo repo
    (let* ((target (majutsu-jj-integration-file repo "target.txt"))
           (link (majutsu-jj-integration-file repo "link.txt")))
      (majutsu-jj-integration-write-text target "must survive\n")
      (make-symbolic-link "target.txt" link)
      (let* ((info (majutsu-file--resolve-single-rev-info "@"))
             (commit (plist-get info :commit-id))
             (change (plist-get info :change-id)))
        (delete-file link)
        (majutsu-jj-integration-write-text link "now regular\n")
        (majutsu-jj-integration-call repo "status")
        (let ((operation (majutsu-jj-integration-output
                          repo "op" "log" "-n" "1" "--no-graph"
                          "-T" "id")))
          (should-error (majutsu-find-file-noselect commit "link.txt")
                        :type 'user-error)
          (should-error (majutsu-file-apply-contents
                         commit change '(("link.txt" . "overwrite\n")))
                        :type 'user-error)
          (should (equal (majutsu-jj-integration-read-bytes target)
                         "must survive\n"))
          (should (equal (majutsu-jj-integration-read-bytes link)
                         "now regular\n"))
          (should (equal (majutsu-jj-integration-output
                          repo "op" "log" "-n" "1" "--no-graph" "-T" "id")
                         operation)))))))

(ert-deftest majutsu-file-revert-buffer/integration-read-failure-is-atomic ()
  "Missing paths preserve the old view and pin; empty files remain valid."
  (majutsu-jj-integration-with-repo repo
    (let ((path (majutsu-jj-integration-file repo "f.txt")))
      (majutsu-jj-integration-write-text path "keep this\n")
      (let ((buffer (majutsu-find-file-noselect "@" "f.txt")))
        (unwind-protect
            (with-current-buffer buffer
              (let ((commit majutsu-buffer-blob-commit-id)
                    (hash majutsu-buffer-blob-content-hash))
                (delete-file path)
                (majutsu-jj-integration-call repo "status")
                (should-error (majutsu-file-revert-buffer nil t)
                              :type 'user-error)
                (should (equal (buffer-string) "keep this\n"))
                (should (equal majutsu-buffer-blob-commit-id commit))
                (should (equal majutsu-buffer-blob-content-hash hash))
                (should-not (buffer-modified-p))
                (majutsu-jj-integration-write-text path "")
                (majutsu-file-revert-buffer nil t)
                (should (equal (buffer-string) ""))
                (should-not (equal majutsu-buffer-blob-commit-id commit))
                (should-not (buffer-modified-p))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-file-revert-buffer/integration-denied-read-is-atomic ()
  "A native jj warning with exit zero must not replace a blob with empty text."
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-write-text
     (majutsu-jj-integration-file repo "secret.txt") "preserve me\n")
    (let ((buffer (majutsu-find-file-noselect "@" "secret.txt")))
      (unwind-protect
          (with-current-buffer buffer
            (let ((pin majutsu-buffer-blob-commit-id)
                  (hash majutsu-buffer-blob-content-hash))
              (majutsu-jj-integration-call repo "describe" "-m" "new version")
              ;; Same fixture as jj's cli/tests/test_acls.rs.  The backend
              ;; deliberately denies reads while keeping tree metadata valid.
              (majutsu-jj-integration-write-text
               (expand-file-name ".jj/repo/store/type" repo) "secret")
              (with-temp-buffer
                (let ((status
                       (majutsu--with-no-color
                         (majutsu-process-jj
                          '(t t) '("--ignore-working-copy" "log" "--no-graph"
                                   "-r" "@" "-T" "commit_id")))))
                  (when (and (not (zerop status))
                             (string-match-p
                              "Unsupported commit backend type 'secret'"
                              (buffer-string)))
                    (ert-skip "jj was built without its testing backend"))
                  (should (zerop status))))
              (should-error (majutsu-file-revert-buffer nil t) :type 'user-error)
              (should (equal (buffer-string) "preserve me\n"))
              (should (equal majutsu-buffer-blob-commit-id pin))
              (should (equal majutsu-buffer-blob-content-hash hash))
              (should-not (buffer-modified-p))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest majutsu-file-revert-buffer/integration-preserves-logical-position ()
  "A rewritten version of the same change keeps the logical line and column."
  (majutsu-jj-integration-with-repo repo
    (let ((path (majutsu-jj-integration-file repo "f.txt")))
      (majutsu-jj-integration-write-text path "one\ntwo\nthree\n")
      (let ((buffer (majutsu-find-file-noselect "@" "f.txt")))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line 1)
              (forward-char 1)
              (majutsu-jj-integration-write-text path "zero\none\ntwo\nthree\n")
              (majutsu-file-revert-buffer nil t)
              (should (= (line-number-at-pos) 3))
              (should (= (current-column) 1))
              (should (equal (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))
                             "two"))
              (majutsu-file-revert-buffer nil t)
              (should (= (line-number-at-pos) 3))
              (should (= (current-column) 1))
              (goto-char (point-min))
              (majutsu-jj-integration-write-text
               path "start\nzero\none\ntwo\nthree\n")
              (majutsu-file-revert-buffer nil t)
              (should (= (line-number-at-pos) 2))
              (should (= (current-column) 0)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-blob-edit/integration-respects-workspace-owner ()
  "A busy workspace rejects blob saves without poisoning retry state."
  (majutsu-jj-integration-with-repo repo
    (let ((path (majutsu-jj-integration-file repo "f.txt"))
          (root (file-name-as-directory repo))
          (owner (list 'another-editor)))
      (majutsu-jj-integration-write-text path "before\n")
      (let ((buffer (majutsu-find-file-noselect "@" "f.txt")))
        (unwind-protect
            (with-current-buffer buffer
              (majutsu-blob-edit-start)
              (erase-buffer)
              (insert "after\n")
              (majutsu-process-register-interactive-session root owner)
              (should-error (majutsu-blob-edit-finish) :type 'user-error)
              (should-not majutsu-blob-edit--save-unverified)
              (should (buffer-modified-p))
              (should (equal (majutsu-jj-integration-output
                              repo "file" "show" "-r" "@" "f.txt")
                             "before\n"))
              (majutsu-process-unregister-interactive-session root owner)
              (majutsu-blob-edit-finish)
              (should-not (buffer-modified-p))
              (should (equal (majutsu-jj-integration-output
                              repo "file" "show" "-r" "@" "f.txt")
                             "after\n")))
          (majutsu-process-unregister-interactive-session root owner)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(provide 'majutsu-file-test)
