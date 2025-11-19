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

;;; Aliases

(defalias 'majutsu 'majutsu-log
  "Begin using Majutsu.

This alias for `majutsu-log' exists for better discoverability.

Instead of invoking this alias for `majutsu-log' using
\"M-x majutsu RET\", you should bind a key to `majutsu-log'.")

(provide 'majutsu)
;;; majutsu.el ends here
