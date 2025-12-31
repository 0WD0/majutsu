;;; majutsu-file.el --- Finding files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; Support jj file commands.

;;; Code:

(defvar majutsu-find-file-hook nil)
(add-hook 'majutsu-find-file-hook #'majutsu-blob-mode)

;; `magit-find-file-read-args' depends on `majutsu-list-refnames'
;; depends on `majutsu-workspace-list' `majutsu-tag-list' `majutsu-bookmark-list'
;; blocked by 'bk:todo-b-t-reading' 重构 majutsu-bookmark.el，
;; 其他两个函数也都没有实现。

;;; _
(provide 'majutsu-files)
;;; majutsu-file.el ends here
