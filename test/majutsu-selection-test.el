;;; majutsu-selection-test.el --- Tests for selection helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for `majutsu-selection'.

;;; Code:

(require 'ert)
(require 'majutsu)

(ert-deftest majutsu-selection-locate-default-returns-section ()
  "Default locate-fn should return the matching section object."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t))
      (magit-insert-section (logbuf)
        (magit-insert-section (jj-commit "a")
          (magit-insert-heading "a")
          (insert "A\n"))
        (magit-insert-section (jj-commit "b")
          (magit-insert-heading "b")
          (insert "B\n")))
      (let* ((root magit-root-section)
             (a (car (oref root children)))
             (b (cadr (oref root children))))
        (goto-char (oref b start))
        (should (eq (magit-current-section) b))
        (let ((found (majutsu-selection--locate-default "a")))
          (should (eieio-object-p found))
          (should (eq found a)))))))

(provide 'majutsu-selection-test)
;;; majutsu-selection-test.el ends here

