;;; majutsu-embark-test.el --- Tests for Majutsu Embark actions  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for optional Embark category action registration.

;;; Code:

(require 'ert)
(require 'embark)
(require 'majutsu-embark)

(ert-deftest majutsu-embark-registers-bookmark-actions ()
  "Bookmark candidates should expose domain actions without hiding general ones."
  (should (eq (alist-get 'majutsu-bookmark embark-keymap-alist)
              'majutsu-embark-bookmark-map))
  (dolist (binding '(("RET" . majutsu-diff-revset)
                     ("e" . majutsu-edit-revision)
                     ("D" . majutsu-diff-revset)
                     ("v" . majutsu-evolog)
                     ("p" . majutsu-bookmark-advance-patterns)
                     ("s" . majutsu-bookmark-set)
                     ("m" . majutsu-bookmark-move)
                     ("M" . majutsu-bookmark-move-allow-backwards)
                     ("r" . majutsu-bookmark-rename)
                     ("d" . majutsu-bookmark-delete)
                     ("f" . majutsu-bookmark-forget)))
    (should (eq (keymap-lookup majutsu-embark-bookmark-map (car binding))
                (cdr binding))))
  (should (eq (keymap-lookup majutsu-embark-bookmark-map "w")
              #'embark-copy-as-kill)))

(ert-deftest majutsu-embark-registers-workspace-actions ()
  "Workspace completion candidates should expose their domain actions."
  (should (eq (alist-get 'majutsu-workspace embark-keymap-alist)
              'majutsu-embark-workspace-map))
  (should (eq (keymap-lookup majutsu-embark-workspace-map "RET")
              #'majutsu-workspace-visit-name))
  (should (eq (keymap-lookup majutsu-embark-workspace-map "v")
              #'majutsu-workspace-visit-name))
  (should (eq (keymap-lookup majutsu-embark-workspace-map "d")
              #'majutsu-workspace-dired))
  (should (eq (keymap-lookup majutsu-embark-workspace-map "W")
              #'majutsu-workspace-copy-root)))

(require 'majutsu-bookmark-test-utils)

(ert-deftest majutsu-embark-list-targets-use-section-metadata ()
  (dolist (case '((nil nil majutsu-bookmark)
                  ("origin" nil majutsu-untracked-bookmark)
                  ("origin" t majutsu-tracked-bookmark)
                  ("git" t majutsu-git-bookmark)))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert (majutsu-bookmark-test--ref "name" (car case) (cadr case)
                                              "CUSTOM HEADING" nil nil '("abc")))
          (majutsu-bookmark--wash-list nil)))
      (goto-char (point-min))
      (should-not (majutsu-embark-target-section))
      (search-forward "CUSTOM HEADING")
      (let* ((target (majutsu-embark-target-section))
             (type (car target))
             (map (symbol-value (alist-get type embark-keymap-alist))))
        (should (eq type (nth 2 case)))
        (should (equal (cdr target) (if (car case) (concat "name@" (car case)) "name")))
        (when (car case)
          (should-not (eq (keymap-lookup map "d") #'majutsu-bookmark-delete))
          (should-not (eq (keymap-lookup map "r") #'majutsu-bookmark-rename)))
        (should (eq (alist-get type embark-default-action-overrides) #'majutsu-diff-revset))))))

(ert-deftest majutsu-embark-section-target-does-not-require-list-or-row-cache ()
  (with-temp-buffer
    (majutsu-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (magit-insert-section (jj-bookmark "topic@origin" nil :remote "origin" :tracked t)
          (magit-insert-heading "Custom remote label"))))
    (goto-char (point-min))
    (should (equal (majutsu-embark-target-section)
                   '(majutsu-tracked-bookmark . "topic@origin")))))

(ert-deftest majutsu-embark-conflict-target-is-a-commit ()
  (with-temp-buffer
    (majutsu-bookmark-list-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (bookmark-list)
        (insert (majutsu-bookmark-test--ref "topic" nil nil "topic conflicted"
                                            "  + first\n  + second" nil '("first-id" "second-id")))
        (majutsu-bookmark--wash-list nil)))
    (goto-char (point-min))
    (search-forward "topic conflicted")
    (magit-section-show (magit-current-section))
    (search-forward "+ second")
    (should (equal (majutsu-embark-target-section)
                   '(majutsu-revision . "second-id")))))

(ert-deftest majutsu-embark-empty-remote-has-remote-actions ()
  (with-temp-buffer
    (majutsu-bookmark-list-mode)
    (let ((inhibit-read-only t)
          (majutsu-bookmark--list-remotes (make-hash-table :test #'equal)))
      (puthash "origin" '(:fetch-url "https://example.org/repo") majutsu-bookmark--list-remotes)
      (magit-insert-section (bookmark-list) (majutsu-bookmark--wash-list nil)))
    (goto-char (point-min))
    (search-forward "https://")
    (should (equal (majutsu-embark-target-section)
                   '(majutsu-remote . "origin")))))

(ert-deftest majutsu-embark-action-passes-target-and-repository ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((default-directory temporary-file-directory)
            seen)
        (embark--act (lambda (target) (setq seen (list target default-directory)))
                     '(:type majutsu-tracked-bookmark :target "topic@fork"))
        (should (equal seen (list "topic@fork" temporary-file-directory)))))))

(ert-deftest majutsu-embark-track-actions-use-explicit-target ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-start-jj) (lambda (args) (setq seen args))))
      (majutsu-bookmark-track-ref "topic*@fork*")
      (should (equal seen '("bookmark" "track" "exact:topic*" "--remote" "exact:fork*")))
      (majutsu-bookmark-untrack-ref "topic@fork")
      (should (equal seen '("bookmark" "untrack" "exact:topic" "--remote" "exact:fork")))
      (should-error (majutsu-bookmark-track-ref "topic@git") :type 'user-error)
      (should-error (majutsu-bookmark-untrack-ref "topic") :type 'user-error))))

(provide 'majutsu-embark-test)
;;; majutsu-embark-test.el ends here
