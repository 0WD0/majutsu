;;; majutsu-section-edit-test.el --- Tests for in-place section editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the generic editable-field facility used by log and diff
;; buffers.  The text is generated and read-only, so these tests focus on
;; the read-only boundary, batching, and the jj describe payload.

;;; Code:

(require 'ert)
(require 'majutsu-jjdescription)
(require 'majutsu-log)
(require 'majutsu-section-edit)

(defmacro majutsu-section-edit-test--with-buffer (&rest body)
  "Run BODY in a read-only buffer with the fields below.
Both `FIELD1' and `FIELD2' are editable single-line fields."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "aa FIELD1\nbb FIELD2\n")
     (majutsu-section-edit-put
      4 10 (majutsu-section-edit-make-field
            :key 'one :multiline nil :data (list :id "one")))
     (majutsu-section-edit-put
      14 20 (majutsu-section-edit-make-field
             :key 'two :multiline nil :data (list :id "two")))
     (setq buffer-read-only t)
     (goto-char 4)
     ,@body))

(defmacro majutsu-section-edit-test--capture (variable)
  "Return an apply function storing its argument in VARIABLE."
  (declare (indent 0))
  `(lambda (changes) (setq ,variable changes) t))

(ert-deftest majutsu-section-edit/finish-passed-to-apply ()
  "Typed text should be extracted and handed to the buffer's applier."
  (let (captured)
    (majutsu-section-edit-test--with-buffer
     (setq-local majutsu-section-edit-apply-function
                 (majutsu-section-edit-test--capture captured))
     (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
               ((symbol-function 'message) #'ignore))
       (majutsu-section-edit)
       (should majutsu-section-edit-mode)
       (goto-char 10)
       (insert-and-inherit "X")
       (majutsu-section-edit-finish)
       (should-not majutsu-section-edit-mode)
       (should (= (length captured) 1))
       (should (equal (cdr (car captured)) "FIELD1X"))))))

(ert-deftest majutsu-section-edit/batches-multiple-fields ()
  "Editing several fields should collect them into one apply call."
  (let (captured)
    (majutsu-section-edit-test--with-buffer
     (setq-local majutsu-section-edit-apply-function
                 (majutsu-section-edit-test--capture captured))
     (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
               ((symbol-function 'message) #'ignore))
       (majutsu-section-edit)
       (goto-char 20)
       (insert-and-inherit "Y")
       (goto-char 10)
       (insert-and-inherit "X")
       (majutsu-section-edit-finish)
       (should (= (length captured) 2))
       (should (equal (mapcar #'cdr captured) '("FIELD1X" "FIELD2Y")))))))

(ert-deftest majutsu-section-edit/protects-surrounding-text ()
  "Text outside the fields must stay read-only during a session."
  (majutsu-section-edit-test--with-buffer
   (setq-local majutsu-section-edit-apply-function (lambda (_) t))
   (cl-letf (((symbol-function 'message) #'ignore))
     (majutsu-section-edit)
     (should-error (progn (goto-char 1) (insert "X"))
                   :type 'text-read-only)
     (goto-char (point-max))
     (should-error (insert "X") :type 'text-read-only))))

(ert-deftest majutsu-section-edit/single-line-field-flattens-newline ()
  "A newline typed into a single-line field becomes a space."
  (let (captured)
    (majutsu-section-edit-test--with-buffer
     (setq-local majutsu-section-edit-apply-function
                 (majutsu-section-edit-test--capture captured))
     (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
               ((symbol-function 'message) #'ignore))
       (majutsu-section-edit)
       (goto-char 10)
       (insert-and-inherit "\n")
       (should (equal (buffer-substring-no-properties 4 11) "FIELD1 "))
       (majutsu-section-edit-finish)
       (should (equal (cdr (car captured)) "FIELD1 "))))))

(ert-deftest majutsu-section-edit/no-change-skips-apply ()
  "Finishing without edits should not run the applier."
  (majutsu-section-edit-test--with-buffer
   (let (called)
     (setq-local majutsu-section-edit-apply-function
                 (lambda (_) (setq called t) t))
     (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
               ((symbol-function 'message) #'ignore))
       (majutsu-section-edit)
       (majutsu-section-edit-finish)
       (should-not called)))))

(ert-deftest majutsu-section-edit/abort-refreshes ()
  "Aborting should delegate to a plain refresh."
  (let (refreshed)
    (majutsu-section-edit-test--with-buffer
     (setq-local majutsu-section-edit-apply-function (lambda (_) t))
     (cl-letf (((symbol-function 'majutsu-refresh)
                (lambda (&rest _) (setq refreshed t)))
               ((symbol-function 'message) #'ignore))
       (majutsu-section-edit)
       (majutsu-section-edit-abort)
       (should refreshed)
       (should-not majutsu-section-edit-mode)))))

(ert-deftest majutsu-section-edit/refresh-is-guarded-while-editing ()
  "Majutsu refresh must refuse to rebuild an active edit session."
  (majutsu-section-edit-test--with-buffer
   (setq-local majutsu-section-edit-apply-function (lambda (_) t))
   (cl-letf (((symbol-function 'message) #'ignore))
     (majutsu-section-edit)
     (should-error (majutsu-refresh-buffer-internal)
                   :type 'user-error))))

(ert-deftest majutsu-section-edit/mid-line-field-end-and-protected-tail ()
  "A field in the middle of a line must accept text at its end.
The protected text after it must stay untouchable."
  (let (captured)
    (with-temp-buffer
      (insert "aa FIELD1 TAIL\n")
      (majutsu-section-edit-put
       4 10 (majutsu-section-edit-make-field
             :key 'one :multiline nil :data '(:id "one")))
      (setq buffer-read-only t)
      (goto-char 4)
      (setq-local majutsu-section-edit-apply-function
                  (majutsu-section-edit-test--capture captured))
      (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (majutsu-section-edit)
        (goto-char 10)
        (insert-and-inherit "X")
        (should (equal (buffer-substring-no-properties 4 11) "FIELD1X"))
        (should-error (progn (goto-char 12) (delete-char 1))
                      :type 'text-read-only)
        (majutsu-section-edit-finish)
        (should (equal (cdr (car captured)) "FIELD1X"))))))

(ert-deftest majutsu-section-edit/stop-restores-read-only ()
  "Leaving a session must restore the buffer's read-only state."
  (majutsu-section-edit-test--with-buffer
   (setq-local majutsu-section-edit-apply-function (lambda (_) t))
   (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
             ((symbol-function 'message) #'ignore))
     (majutsu-section-edit)
     (should-not buffer-read-only)
     (goto-char 10)
     (insert-and-inherit "X")
     (majutsu-section-edit-abort)
     (should buffer-read-only)
     (should-not (text-property-any (point-min) (point-max) 'read-only t)))))

(ert-deftest majutsu-section-edit/smoke-blocks-majutsu-command-keys ()
  "A session must shadow Majutsu's single-letter command bindings.
Otherwise typing a description that contains `k' or `e' would run
`majutsu-abandon', `majutsu-edit', and friends."
  (with-temp-buffer
    (majutsu-log-mode)
    (setq buffer-read-only nil)
    (insert "before FIELD after\n")
    (majutsu-section-edit-put
     8 13 (majutsu-section-edit-make-field
           :key 'one :multiline nil :data '(:id "one")))
    (setq buffer-read-only t)
    (goto-char 8)
    (let (captured)
      (setq-local majutsu-section-edit-apply-function
                  (majutsu-section-edit-test--capture captured))
      (cl-letf (((symbol-function 'majutsu-refresh) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (majutsu-section-edit)
        (dolist (key '("k" "e" "c" "x"))
          (should (eq (key-binding key) #'self-insert-command)))
        (goto-char 13)
        (dolist (char (string-to-list "key"))
          (let ((last-command-event char))
            (call-interactively (key-binding (char-to-string char)))))
        (should (equal (buffer-substring-no-properties 8 16) "FIELDkey"))
        (majutsu-section-edit-finish)
        (should (equal (cdr (car captured)) "FIELDkey"))))))

;;; jj describe payload

(ert-deftest majutsu-jjdescription-payload/single-description ()
  "A single description uses the plain editor format."
  (should (equal (majutsu-jjdescription--describe-payload
                  '(("abcdef012345" . "only one")))
                 "only one\n")))

(ert-deftest majutsu-jjdescription-payload/bulk-descriptions ()
  "Several descriptions use jj's `JJ: describe' headers."
  (let ((payload (majutsu-jjdescription--describe-payload
                  '(("abcdef012345" . "first")
                    ("123456abcdef" . "second")))))
    (should (string-match-p
             (concat "^JJ: describe abcdef012345 -------\nfirst\n\n"
                     "JJ: describe 123456abcdef -------\nsecond\n\n")
             payload))
    (should (string-match-p "JJ: Lines starting" payload))))

;;; Log integration

(ert-deftest majutsu-log-apply-descriptions/preserves-body ()
  "Editing the summary should preserve each description's body."
  (let (captured
        (field-a (majutsu-section-edit-make-field
                  :data '(:commit-id "aaaaaaaaaaaa" :body "\nbody-a")))
        (field-b (majutsu-section-edit-make-field
                  :data '(:commit-id "bbbbbbbbbbbb" :body ""))))
    (cl-letf (((symbol-function 'majutsu-jjdescription-apply-descriptions)
               (lambda (descriptions)
                 (setq captured descriptions)
                 t)))
      (should (majutsu-log--apply-description-changes
               (list (cons field-a "new-a")
                     (cons field-b "new-b"))))
      (should (equal captured
                     '(("aaaaaaaaaaaa" . "new-a\n\nbody-a")
                       ("bbbbbbbbbbbb" . "new-b")))))))

(provide 'majutsu-section-edit-test)
;;; majutsu-section-edit-test.el ends here
