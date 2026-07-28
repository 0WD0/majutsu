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
  (dolist (binding '(("RET" . majutsu-edit-revision)
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

(provide 'majutsu-embark-test)
;;; majutsu-embark-test.el ends here
