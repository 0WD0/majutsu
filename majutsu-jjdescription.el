;;; majutsu-jjdescription.el --- Edit JJ descriptions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-mode)
(require 'majutsu-process)

(require 'with-editor)
(require 'server)

(defconst majutsu-jjdescription-regexp
  (rx (seq (or string-start
               (seq (* (not (any ?\n)))
                    (any ?/ ?\\)))
           "editor-" (+ (in "0-9A-Za-z"))
           ".jjdescription" string-end))
  "Regexp matching temporary jj description files created for editing.")

(add-to-list 'with-editor-file-name-history-exclude majutsu-jjdescription-regexp)

(add-to-list 'with-editor-server-window-alist
             (cons majutsu-jjdescription-regexp #'pop-to-buffer-same-window))

;;; _
(provide 'majutsu-jjdescription)
;;; majutsu-jjdescription.el ends here
