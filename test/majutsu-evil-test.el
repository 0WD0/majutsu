;;; majutsu-evil-test.el --- Tests for majutsu-evil  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for Evil-specific keymaps.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-conflict)
(require 'majutsu-evil)
(require 'majutsu-file)
(require 'majutsu-jj-integration)
(require 'majutsu-section-edit)

(defvar evil-local-mode)
(defvar evil-state)

(ert-deftest majutsu-evil-test-conflict-side-bindings ()
  "Evil digit bindings resolve the selected side or base of a conflict."
  (pcase-dolist (`(,map ,key ,expected)
                 '((majutsu-conflict-evil-before-map "1" "base\n")
                   (majutsu-conflict-evil-resolve-map "2" "second\n")))
    (with-temp-buffer
      (insert "<<<<<<< conflict 1 of 1\n"
              "+++++++ side-1\nfirst\n"
              "------- base\nbase\n"
              "+++++++ side-2\nsecond\n"
              ">>>>>>> conflict 1 of 1 ends\n")
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (call-interactively (lookup-key (symbol-value map) (kbd key)))
      (should (equal (buffer-string) expected)))))

(ert-deftest majutsu-evil/integration-write-saves-blob ()
  "Actual Evil :w saves the pinned blob instead of visiting a filesystem path."
  (skip-unless (require 'evil nil t))
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-write-text
     (majutsu-jj-integration-file repo "f.txt") "before\n")
    (let ((buffer (majutsu-find-file-noselect "@" "f.txt")))
      (unwind-protect
          (with-current-buffer buffer
            (evil-local-mode 1)
            (majutsu-blob-edit-start)
            (goto-char (point-max))
            (insert "saved through :w\n")
            (evil-ex-execute "w")
            (should-not (buffer-modified-p))
            (should-not majutsu-blob-edit-mode)
            (should buffer-read-only)
            (should (equal (majutsu-jj-integration-output
                            repo "file" "show" "-r"
                            majutsu-buffer-blob-commit-id "f.txt")
                           "before\nsaved through :w\n")))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (evil-local-mode -1)
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest majutsu-evil/section-edit-i-dwim ()
  "First `i' starts a section-edit session; the next `i' inserts."
  (skip-unless (require 'evil nil t))
  (with-temp-buffer
    (let (started inserted)
      (cl-letf (((symbol-function 'majutsu-section-edit)
                 (lambda () (setq started t)))
                ((symbol-function 'evil-insert-state)
                 (lambda () (setq inserted t))))
        (majutsu-section-edit-mode 1)
        (call-interactively #'majutsu-evil-section-edit)
        (should inserted)
        (should-not started)
        (setq started nil inserted nil)
        (majutsu-section-edit-mode -1)
        (call-interactively #'majutsu-evil-section-edit)
        (should started)
        (should-not inserted)))))

(ert-deftest majutsu-evil/section-edit-i-is-bound ()
  "`i' resolves to the section-edit command in every relevant Normal map."
  (skip-unless (require 'evil nil t))
  (require 'majutsu-section-edit)
  (majutsu-evil-setup)
  (dolist (map '(majutsu-log-mode-map majutsu-diff-mode-map
                 majutsu-section-edit-mode-map))
    (let ((aux (evil-get-auxiliary-keymap (symbol-value map) 'normal)))
      (should (eq (lookup-key aux (kbd "i")) #'majutsu-evil-section-edit)))))

(provide 'majutsu-evil-test)
;;; majutsu-evil-test.el ends here
