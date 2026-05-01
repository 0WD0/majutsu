;;; majutsu-graph-record-test.el --- Tests for graph record protocol  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for graph-aware structured jj record helpers.

;;; Code:

(require 'ert)
(require 'majutsu-graph-record)

(ert-deftest majutsu-graph-record-template/wraps-fields-with-record-tokens ()
  "Graph record templates should separate graph prefix from payload."
  (let ((template (majutsu-graph-record-template
                   "test"
                   '([:change_id] [:commit_id])
                   'Commit)))
    (should (string-match-p (regexp-quote "\\x1DGR:test:S")
                            template))
    (should (string-match-p (regexp-quote "\\x1E")
                            template))
    (should (string-match-p (regexp-quote "\\x1DGR:test:E")
                            template))))

(ert-deftest majutsu-graph-record-template/can-wrap-visible-payload ()
  "Graph record templates should support hidden fields plus visible payload."
  (let ((template (majutsu-graph-record-template
                   "test"
                   '([:change_id])
                   'Commit
                   '[:concat [:change_id :short] "\n" [:description :first_line]])))
    (should (string-match-p (regexp-quote "\\x1DGR:test:S") template))
    (should (string-match-p (regexp-quote "\\x1DGR:test:P") template))
    (should (string-match-p (regexp-quote "self.description().first_line()")
                            template))))

(ert-deftest majutsu-graph-record-parse-line/splits-graph-prefix-and-payload ()
  "Graph record line parsing should preserve graph prefix separately."
  (let* ((line (concat "@  "
                       (majutsu-graph-record--start-token "test")
                       "a" majutsu-graph-record-field-separator "b"
                       (majutsu-graph-record--end-token "test")))
         (record (majutsu-graph-record-parse-line "test" line)))
    (should (equal (plist-get record :graph-prefix) "@  "))
    (should (equal (majutsu-graph-record-split-fields
                    (plist-get record :payload) 2)
                   '("a" "b")))))

(ert-deftest majutsu-graph-record-parse-at-point/parses-hidden-fields ()
  "Graph record parsing should consume one hidden-field record."
  (with-temp-buffer
    (insert "○  " (majutsu-graph-record--start-token "test")
            "a" (majutsu-graph-record--end-token "test") "\n"
            "next\n")
    (goto-char (point-min))
    (let ((record (majutsu-graph-record-parse-at-point "test")))
      (should (equal (plist-get record :graph-prefix) "○  "))
      (should (equal (plist-get record :payload) "a"))
      (should (equal (plist-get record :display) ""))
      (should (looking-at "next")))))

(ert-deftest majutsu-graph-record-parse-at-point/keeps-multiline-display ()
  "Graph record parsing should keep multiline visible text with graph prefixes."
  (with-temp-buffer
    (insert "○  " (majutsu-graph-record--start-token "test")
            "a" (majutsu-graph-record--payload-token "test") "heading\n"
            "│  body\n"
            "│  tail" (majutsu-graph-record--end-token "test") "\n"
            "@  next\n")
    (goto-char (point-min))
    (let ((record (majutsu-graph-record-parse-at-point "test")))
      (should (equal (plist-get record :payload) "a"))
      (should (equal (plist-get record :display)
                     "○  heading\n│  body\n│  tail"))
      (should (looking-at "@  next")))))

(provide 'majutsu-graph-record-test)
;;; majutsu-graph-record-test.el ends here
