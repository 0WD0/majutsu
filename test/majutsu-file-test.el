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

(ert-deftest majutsu-find-file-noselect/blob-relative-to-root ()
  "Relative FILE becomes a root-relative blob path."
  (let* ((root (make-temp-file "majutsu-file-root-" t))
         (sub (expand-file-name "sub" root))
         (target (expand-file-name "note.txt" sub))
         (buf nil))
    (unwind-protect
        (progn
          (make-directory sub t)
          (with-temp-file target
            (insert "hello"))
          (let ((default-directory sub))
            (cl-letf (((symbol-function 'majutsu-file--root)
                       (lambda () root))
                      ((symbol-function 'majutsu-file--resolve-single-rev-info)
                       (lambda (_rev) '(:change-id "abcdef123456"
                                        :commit-id "0123456789ab")))
                      ((symbol-function 'majutsu-file--short-id)
                       (lambda (_id) "abcdef12"))
                      ((symbol-function 'majutsu-file-revert-buffer)
                       (lambda (&rest _args) nil)))
              (setq buf (majutsu-find-file-noselect "@" "sub/note.txt" t))))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (equal majutsu-buffer-blob-root root))
            (should (equal majutsu-buffer-blob-path "sub/note.txt"))
            (should (equal (buffer-name) "sub/note.txt@~abcdef12~"))))
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

(ert-deftest majutsu-file-revert-buffer/noop-when-revision-unchanged ()
  "Revert should be a no-op when change-id and commit-id are unchanged."
  (let (called)
    (with-temp-buffer
      (insert "same\n")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "chg1")
      (setq-local majutsu-buffer-blob-change-id "chg1")
      (setq-local majutsu-buffer-blob-commit-id "c1")
      (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
      (set-buffer-modified-p nil)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (_revset)
                   '(:change-id "chg1" :commit-id "c1")))
                ((symbol-function 'majutsu-jj-insert)
                 (lambda (&rest _args)
                   (setq called t))))
        (should-not (majutsu-file-revert-buffer nil t))
        (should-not called)
        (should (equal (buffer-string) "same\n"))))))

(ert-deftest majutsu-file-revert-buffer/same-change-uses-offset ()
  "When commit changes within same change-id, revert should use diff offset."
  (let (offset-args)
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\nfive\nsix\n")
      (goto-char (point-min))
      (forward-line 2)
      (move-to-column 2)
      (setq-local majutsu-buffer-blob-root "/tmp/repo")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "chg1")
      (setq-local majutsu-buffer-blob-change-id "chg1")
      (setq-local majutsu-buffer-blob-commit-id "c1")
      (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
      (setq-local majutsu-blob-mode nil)
      (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                 (lambda (_revset)
                   '(:change-id "chg1" :commit-id "c2")))
                ((symbol-function 'majutsu-diff-visit--offset)
                 (lambda (&rest args)
                   (setq offset-args args)
                   5))
                ((symbol-function 'majutsu-jj-insert)
                 (lambda (&rest _args)
                   (insert "aaaa\nbbbb\ncccc\ndddd\neeee\nffff\n")))
                ((symbol-function 'normal-mode)
                 (lambda (&optional _find-file) nil)))
        (majutsu-file-revert-buffer nil t)
        (should (equal offset-args
                       '("/tmp/repo" "src/a.el" "c1" "c2" 3)))
        (should (equal majutsu-buffer-blob-commit-id "c2"))
        (should majutsu-blob-mode)
        (should (= (line-number-at-pos) 5))
        (should (= (current-column) 2))))))

(ert-deftest majutsu-file-revert-buffer/change-change-jumps-top-and-renames ()
  "When change-id changes, revert jumps to top and updates buffer name."
  (with-temp-buffer
    (rename-buffer "temp-blob-buffer" t)
    (insert "one\ntwo\n")
    (goto-char (point-max))
    (setq-local majutsu-buffer-blob-root "/tmp/repo")
    (setq-local majutsu-buffer-blob-revset "@")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "oldchange")
    (setq-local majutsu-buffer-blob-change-id "oldchange")
    (setq-local majutsu-buffer-blob-commit-id "cold")
    (setq-local majutsu-buffer-blob-content-hash (secure-hash 'sha1 (current-buffer)))
    (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
               (lambda (_revset)
                 '(:change-id "newchangeid" :commit-id "cnew")))
              ((symbol-function 'majutsu-jj-insert)
               (lambda (&rest _args)
                 (insert "n1\nn2\n")))
              ((symbol-function 'normal-mode)
               (lambda (&optional _find-file) nil)))
      (majutsu-file-revert-buffer nil t)
      (should (equal majutsu-buffer-blob-revision "newchangeid"))
      (should (equal (buffer-name) "src/a.el@~newchang~"))
      (should (= (line-number-at-pos) 1))
      (should (= (current-column) 0)))))

(ert-deftest majutsu-file-revert-buffer/detects-hidden-local-drift ()
  "Revert should prompt when content changed but modified flag is cleared."
  (let (asked)
    (with-temp-buffer
      (insert "before")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-revset "@")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "chg1")
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
    (setq-local majutsu-buffer-blob-revision "rev")
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
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (cl-letf (((symbol-function 'evil-insert-state)
                 (lambda () (setq insert-called t))))
        (majutsu-blob-edit-start)
        (should majutsu-blob-edit-mode)
        (should-not insert-called)))))

(ert-deftest majutsu-blob-visit-magit/uses-commit-id ()
  "Open magit blob view using resolved commit id and path."
  (let* ((root (file-name-as-directory (make-temp-file "majutsu-blob-magit-" t)))
         (other (file-name-as-directory (make-temp-file "majutsu-blob-default-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu-buffer-blob-root root)
          (setq-local majutsu-buffer-blob-path "src/a.el")
          (setq-local majutsu-buffer-blob-revset "@")
          (setq-local majutsu-buffer-blob-revision "change-id")
          (setq-local majutsu-buffer-blob-change-id "change-id")
          (setq-local majutsu-buffer-blob-commit-id nil)
          (setq default-directory other)
          (insert "zero\nfirst\nsecond\n")
          (goto-char (point-min))
          (forward-line 1)
          (move-to-column 2)
          (majutsu-blob-mode 1)
          (let ((line (line-number-at-pos))
                (col (current-column))
                seen-rev seen-file seen-dir opened-buf)
            (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                       (lambda (_revset)
                         '(:change-id "change-id" :commit-id "deadbeef")))
                      ((symbol-function 'magit-find-file)
                       (lambda (rev file)
                         (setq opened-buf (generate-new-buffer " *magit-blob*"))
                         (with-current-buffer opened-buf
                           (insert "zero\nfirst\nsecond\n"))
                         (setq seen-rev rev
                               seen-file file
                               seen-dir default-directory)
                         opened-buf)))
              (majutsu-blob-visit-magit)
              (should (equal seen-rev "deadbeef"))
              (should (equal seen-file "src/a.el"))
              (should (equal seen-dir root))
              (should (equal majutsu-buffer-blob-commit-id "deadbeef"))
              (with-current-buffer opened-buf
                (should (= (line-number-at-pos) line))
                (should (= (current-column) col)))
              (when (buffer-live-p opened-buf)
                (kill-buffer opened-buf)))))
      (delete-directory root t)
      (delete-directory other t))))

(ert-deftest majutsu-blob-visit-magit/prefers-cached-commit-id ()
  "Use cached blob commit-id without re-resolving revset."
  (let ((root (file-name-as-directory (make-temp-file "majutsu-blob-magit-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu-buffer-blob-root root)
          (setq-local majutsu-buffer-blob-path "src/a.el")
          (setq-local majutsu-buffer-blob-revset "@")
          (setq-local majutsu-buffer-blob-revision "change-id")
          (setq-local majutsu-buffer-blob-change-id "change-id")
          (setq-local majutsu-buffer-blob-commit-id "cachedc0ffee")
          (majutsu-blob-mode 1)
          (let (seen-rev opened-buf)
            (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                       (lambda (&rest _args)
                         (ert-fail "Should not resolve when commit-id is cached")))
                      ((symbol-function 'magit-find-file)
                       (lambda (rev _file)
                         (setq seen-rev rev)
                         (setq opened-buf (generate-new-buffer " *magit-blob*"))
                         opened-buf)))
              (majutsu-blob-visit-magit)
              (should (equal seen-rev "cachedc0ffee"))
              (when (buffer-live-p opened-buf)
                (kill-buffer opened-buf)))))
      (delete-directory root t))))

(ert-deftest majutsu-blob-visit-magit/errors-when-unresolved ()
  "Signal an error when revset does not resolve to a commit id."
  (let ((root (file-name-as-directory (make-temp-file "majutsu-blob-magit-" t))))
    (unwind-protect
        (with-temp-buffer
          (setq-local majutsu-buffer-blob-root root)
          (setq-local majutsu-buffer-blob-path "src/a.el")
          (setq-local majutsu-buffer-blob-revset "@")
          (setq-local majutsu-buffer-blob-revision "change-id")
          (setq-local majutsu-buffer-blob-change-id "change-id")
          (setq-local majutsu-buffer-blob-commit-id nil)
          (majutsu-blob-mode 1)
          (cl-letf (((symbol-function 'majutsu-file--resolve-single-rev-info)
                     (lambda (_revset) nil))
                    ((symbol-function 'magit-find-file)
                     (lambda (&rest _args) (error "unexpected"))))
            (should-error (majutsu-blob-visit-magit) :type 'user-error)))
      (delete-directory root t))))

(ert-deftest majutsu-blob-edit-mode/toggles-cursor-type ()
  "Editable mode should switch cursor type and restore it on exit."
  (with-temp-buffer
    (insert "old")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
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
    (setq-local majutsu-buffer-blob-revision "rev")
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
    (setq-local majutsu-buffer-blob-revision "rev")
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
    (setq-local majutsu-buffer-blob-revision "rev")
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
    (setq-local majutsu-buffer-blob-revision "rev")
    (majutsu-blob-mode 1)
    (majutsu-blob-edit-mode 1)
    (majutsu-blob-edit-finish)
    (should-not majutsu-blob-edit-mode)
    (should majutsu-blob-mode)))

(ert-deftest majutsu-blob-edit-exit/uses-content-delta-not-modified-flag ()
  "Exit should still treat content deltas as changes when modified flag is nil."
  (let (finished aborted)
    (with-temp-buffer
      (insert "stable")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (insert "!")
      ;; Simulate a stale/cleared modified flag.
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'majutsu-blob-edit-finish)
                 (lambda () (setq finished t)))
                ((symbol-function 'majutsu-blob-edit-abort)
                 (lambda () (setq aborted t))))
        (majutsu-blob-edit-exit)
        (should finished)
        (should-not aborted)))))

(ert-deftest majutsu-blob-edit-exit/modified-can-finish ()
  "Exit should finish when user confirms save."
  (let (finished aborted)
    (with-temp-buffer
      (insert "stable")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (insert "!")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'majutsu-blob-edit-finish)
                 (lambda () (setq finished t)))
                ((symbol-function 'majutsu-blob-edit-abort)
                 (lambda () (setq aborted t))))
        (majutsu-blob-edit-exit)
        (should finished)
        (should-not aborted)))))

(ert-deftest majutsu-blob-edit-exit/modified-can-abort ()
  "Exit should abort when user declines save."
  (let (finished aborted)
    (with-temp-buffer
      (insert "stable")
      (setq-local majutsu-buffer-blob-root "/tmp")
      (setq-local majutsu-buffer-blob-path "src/a.el")
      (setq-local majutsu-buffer-blob-revision "rev")
      (majutsu-blob-mode 1)
      (majutsu-blob-edit-mode 1)
      (insert "!")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'majutsu-blob-edit-finish)
                 (lambda () (setq finished t)))
                ((symbol-function 'majutsu-blob-edit-abort)
                 (lambda () (setq aborted t))))
        (majutsu-blob-edit-exit)
        (should aborted)
        (should-not finished)))))

(ert-deftest majutsu-blob-edit-exit/decline-save-restores-entry-point ()
  "Declining save on exit should restore original content and point."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (setq-local majutsu-buffer-blob-root "/tmp")
    (setq-local majutsu-buffer-blob-path "src/a.el")
    (setq-local majutsu-buffer-blob-revision "rev")
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

(provide 'majutsu-file-test)
