;;; majutsu-graph-record-test.el --- Tests for graph record protocol  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for graph-aware structured jj record helpers.

;;; Code:

(require 'ert)
(require 'majutsu-graph-record)

(ert-deftest majutsu-graph-record-template/wraps-display-and-metadata ()
  "Graph record templates should wrap visible display and hidden metadata."
  (let ((template (majutsu-graph-record-template
                   "test"
                   '([:commit_id] [:change_id])
                   'Commit
                   '[:concat [:change_id :short] "\n" [:description :first_line]])))
    (should (string-match-p (regexp-quote "\\x1DGR:test:S") template))
    (should (string-match-p (regexp-quote "self.description().first_line()")
                            template))
    (should (string-match-p (regexp-quote "\\x1DGR:test:M") template))
    (should (string-match-p (regexp-quote "\\x1E") template))
    (should (string-match-p (regexp-quote "\\x1DGR:test:E") template))))

(ert-deftest majutsu-graph-record-parse-at-point/parses-single-line-display ()
  "Graph record parsing should keep graph prefix and hidden payload separate."
  (with-temp-buffer
    (insert "@  "
            (majutsu-graph-record--start-token "test")
            "visible"
            (majutsu-graph-record--metadata-token "test")
            "a" majutsu-graph-record-field-separator "b"
            (majutsu-graph-record--end-token "test")
            "\n")
    (goto-char (point-min))
    (let ((record (majutsu-graph-record-parse-at-point "test")))
      (should (equal (plist-get record :graph-prefix) "@  "))
      (should (equal (plist-get record :payload)
                     (concat "a" majutsu-graph-record-field-separator "b")))
      (should (equal (plist-get record :display) "@  visible"))
      (should (equal (majutsu-graph-record-split-fields
                      (plist-get record :payload) 2)
                     '("a" "b"))))))

(ert-deftest majutsu-graph-record-parse-at-point/keeps-multiline-display ()
  "Graph record parsing should keep multiline visible text with graph prefixes."
  (with-temp-buffer
    (insert "○  " (majutsu-graph-record--start-token "test") "heading\n"
            "│  body\n"
            "│  tail" (majutsu-graph-record--metadata-token "test") "meta"
            (majutsu-graph-record--end-token "test") "\n"
            "@  " (majutsu-graph-record--start-token "test") "next"
            (majutsu-graph-record--metadata-token "test") "meta2"
            (majutsu-graph-record--end-token "test") "\n")
    (goto-char (point-min))
    (let ((record (majutsu-graph-record-parse-at-point "test")))
      (should (equal (plist-get record :payload) "meta"))
      (should (equal (plist-get record :display)
                     "○  heading\n│  body\n│  tail"))
      (should (looking-at "@  ")))))

(ert-deftest majutsu-graph-record-parse-at-point/keeps-suffix-lines ()
  "Graph record parsing should attach graph continuation lines before next record."
  (with-temp-buffer
    (insert "○  " (majutsu-graph-record--start-token "test") "heading"
            (majutsu-graph-record--metadata-token "test") "meta"
            (majutsu-graph-record--end-token "test") "\n"
            "│\n"
            "@  " (majutsu-graph-record--start-token "test") "next"
            (majutsu-graph-record--metadata-token "test") "meta2"
            (majutsu-graph-record--end-token "test") "\n")
    (goto-char (point-min))
    (let ((record (majutsu-graph-record-parse-at-point "test")))
      (should (equal (plist-get record :display)
                     "○  heading\n│"))
      (should (looking-at "@  ")))))

(provide 'majutsu-graph-record-test)
;;; majutsu-graph-record-test.el ends here
