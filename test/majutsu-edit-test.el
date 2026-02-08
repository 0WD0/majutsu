;;; majutsu-edit-test.el --- Tests for majutsu-edit -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for edit and diffedit helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-edit)

(ert-deftest majutsu-edit-test-diffedit-root-detects-instructions ()
  "Diffedit root should be detected by JJ-INSTRUCTIONS file."
  (let ((root (make-temp-file "majutsu-diffedit" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "JJ-INSTRUCTIONS" root) nil 'silent)
          (let* ((nested (expand-file-name "right/file.txt" root))
                 (dir (file-name-directory nested)))
            (make-directory dir t)
            (write-region "" nil nested nil 'silent)
            (should (equal (file-name-as-directory root)
                           (file-name-as-directory
                            (majutsu-edit--diffedit-root nested))))))
      (delete-directory root t))))

(ert-deftest majutsu-edit-test-finish-on-save-calls-with-editor ()
  "Finish-on-save should call with-editor-finish when active."
  (let ((called nil))
    (with-temp-buffer
      (setq-local server-buffer-clients '(dummy))
      (with-editor-mode 1)
      (let ((majutsu-edit-finish-on-save t))
        (cl-letf (((symbol-function 'with-editor-finish)
                   (lambda (&optional _force)
                     (setq called t))))
          (majutsu-edit--finish-on-save))))
    (should called)))

(provide 'majutsu-edit-test)
;;; majutsu-edit-test.el ends here
