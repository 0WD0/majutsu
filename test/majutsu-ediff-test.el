;;; majutsu-ediff-test.el --- Tests for majutsu-ediff -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for ediff integration.

;;; Code:

(require 'ert)

(defconst majutsu-ediff-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name
         (locate-library "majutsu-ediff.el"))))))

(when majutsu-ediff-test--root
  (add-to-list 'load-path majutsu-ediff-test--root)
  (load (expand-file-name "majutsu-ediff.el" majutsu-ediff-test--root) nil t))

(require 'majutsu-ediff)

;;; Tests

(ert-deftest majutsu-ediff-test-parse-diff-range-revisions ()
  "Test parsing --revisions= format."
  (let ((result (majutsu-ediff--parse-diff-range '("--revisions=abc"))))
    (should (equal (car result) "abc-"))
    (should (equal (cdr result) "abc"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-from-to ()
  "Test parsing --from/--to format."
  (let ((result (majutsu-ediff--parse-diff-range '("--from=foo" "--to=bar"))))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-from-only ()
  "Test parsing --from only."
  (let ((result (majutsu-ediff--parse-diff-range '("--from=foo"))))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "@"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-to-only ()
  "Test parsing --to only."
  (let ((result (majutsu-ediff--parse-diff-range '("--to=bar"))))
    (should (equal (car result) "@-"))
    (should (equal (cdr result) "bar"))))

(ert-deftest majutsu-ediff-test-parse-diff-range-nil ()
  "Test parsing nil range."
  (let ((result (majutsu-ediff--parse-diff-range nil)))
    (should (null result))))

(ert-deftest majutsu-ediff-test-parse-diff-range-empty ()
  "Test parsing empty range."
  (let ((result (majutsu-ediff--parse-diff-range '())))
    (should (null result))))

(ert-deftest majutsu-ediff-test-parse-diff-range-short-r ()
  "Test parsing -r format."
  (let ((result (majutsu-ediff--parse-diff-range '("-rxyz"))))
    (should (equal (car result) "xyz-"))
    (should (equal (cdr result) "xyz"))))

(provide 'majutsu-ediff-test)
;;; majutsu-ediff-test.el ends here
