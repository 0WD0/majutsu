;;; majutsu-core-test.el --- Tests for Majutsu core helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for core transient integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-core)
(require 'majutsu-selection)

(ert-deftest majutsu-transient-default-action-suffix/uses-selection-buffer-advice ()
  "Default action suffixes should run through selection-buffer advice."
  (let ((obj (make-instance 'majutsu-transient-default-action-suffix
                            :command 'ignore
                            :description "Ignore")))
    (should (eq (oref obj advice*) #'majutsu--transient-with-selection-buffer))
    (transient--init-suffix-key obj)
    (should (equal (oref obj key) majutsu-transient-default-action))))

(ert-deftest majutsu-transient-default-action-suffix/binds-default-key-as-alias ()
  "Default action suffixes with explicit keys should also bind the default key."
  (let* ((majutsu-transient-default-action "RET")
         (obj (make-instance 'majutsu-transient-default-action-suffix
                             :command 'ignore
                             :key "x"
                             :description "Ignore"))
         (transient--suffixes (list obj))
         (map (make-sparse-keymap)))
    (define-key map (kbd "x") #'ignore)
    (majutsu-transient--add-key-aliases map)
    (should (eq (lookup-key map (kbd "RET")) #'ignore))))

(ert-deftest majutsu-transient-default-action-suffix/formats-key-alias ()
  "Default action aliases should appear on the same menu row."
  (let* ((majutsu-transient-default-action "RET")
         (transient-semantic-coloring nil)
         (transient--pending-group (make-instance 'transient-column))
         (obj (make-instance 'majutsu-transient-default-action-suffix
                             :command 'ignore
                             :key "x"
                             :description "Ignore")))
    (should (equal (substring-no-properties (transient-format-key obj))
                   "x/RET"))))

(ert-deftest majutsu-transient-with-selection-buffer/uses-session-buffer ()
  "Selection sessions should make action suffixes run in their source buffer."
  (let* ((selection-dir (file-name-as-directory
                         (make-temp-file "majutsu-selection-" t)))
         (caller-dir (file-name-as-directory
                      (make-temp-file "majutsu-caller-" t)))
         (selection-buf (generate-new-buffer " *majutsu-selection-test*"))
         (caller-buf (generate-new-buffer " *majutsu-caller-test*"))
         (session (majutsu-selection-session-create
                   :buffer selection-buf
                   :overlays (make-hash-table :test 'equal)))
         seen-dir)
    (unwind-protect
        (progn
          (with-current-buffer selection-buf
            (setq default-directory selection-dir))
          (with-current-buffer caller-buf
            (setq default-directory caller-dir))
          (cl-letf (((symbol-function 'transient-scope)
                     (lambda (&rest _args) session)))
            (with-current-buffer caller-buf
              (majutsu--transient-with-selection-buffer
               (lambda ()
                 (setq seen-dir default-directory)))))
          (should (equal seen-dir selection-dir))
          (should (not (equal seen-dir caller-dir))))
      (when (buffer-live-p selection-buf)
        (kill-buffer selection-buf))
      (when (buffer-live-p caller-buf)
        (kill-buffer caller-buf))
      (delete-directory selection-dir t)
      (delete-directory caller-dir t))))

(ert-deftest majutsu-transient-with-selection-buffer/falls-back-without-session ()
  "Without a selection session, action advice should preserve current buffer."
  (let ((default-directory "/tmp/")
        seen-dir)
    (cl-letf (((symbol-function 'transient-scope)
               (lambda (&rest _args) nil)))
      (majutsu--transient-with-selection-buffer
       (lambda ()
         (setq seen-dir default-directory))))
    (should (equal seen-dir "/tmp/"))))

(provide 'majutsu-core-test)
;;; majutsu-core-test.el ends here
