;;; majutsu.el --- Interface to jj version control system  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (transient "0.5.0") (magit "3.3.0"))

;;; Commentary:

;; Majutsu provides an interface to the Jujutsu (jj) version control system,
;; inspired by Magit.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'majutsu-log)
(require 'majutsu-status)
(require 'majutsu-transient)
(require 'majutsu-commands)

;;; Keymap

(defvar majutsu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "n") 'majutsu-goto-next-changeset)
    (define-key map (kbd "p") 'majutsu-goto-prev-changeset)
    (define-key map (kbd "RET") 'majutsu-enter-dwim)
    (define-key map (kbd "g") 'majutsu-log-refresh)
    (define-key map (kbd "l") 'majutsu-log-transient)
    (define-key map (kbd "?") 'majutsu-log-transient)
    (define-key map (kbd "c") 'majutsu-commit)
    (define-key map (kbd "d") 'majutsu-describe)
    (define-key map (kbd "N") 'majutsu-new)
    (define-key map (kbd "S") 'majutsu-squash-transient)
    (define-key map (kbd "D") 'majutsu-diff-transient)
    (define-key map (kbd "R") 'majutsu-rebase-transient)
    (define-key map (kbd "B") 'majutsu-bookmark-transient)
    (define-key map (kbd "Y") 'majutsu-duplicate-transient)
    (define-key map (kbd "G") 'majutsu-git-transient)
    (define-key map (kbd "u") 'majutsu-undo)
    (define-key map (kbd "U") 'majutsu-redo)
    (define-key map (kbd "X") 'majutsu-abandon)
    map)
  "Keymap for `majutsu-mode'.")

;;; Mode Configuration

;; Configure majutsu-log-mode to use our keymap
(with-eval-after-load 'majutsu-log
  (set-keymap-parent majutsu-log-mode-map majutsu-mode-map))

;;; Aliases

(defalias 'majutsu-mode 'majutsu-log-mode
  "Alias for `majutsu-log-mode'.")

(defalias 'majutsu 'majutsu-log
  "Begin using Majutsu.

This alias for `majutsu-log' exists for better discoverability.

Instead of invoking this alias for `majutsu-log' using
\"M-x majutsu RET\", you should bind a key to `majutsu-log'.")

(provide 'majutsu)
;;; majutsu.el ends here
