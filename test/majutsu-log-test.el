;;; majutsu-log-test.el --- Tests for log parsing  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for log parsing logic.

;;; Code:

(require 'ert)
(require 'majutsu)

(ert-deftest majutsu-log-parse-entries-basic ()
  "Parse a simple log entry."
  (let* ((sample-output (concat
                         "o" "\x1e"
                         "change-id" "\x1e"
                         "Author Name <author@example.com>" "\x1e"
                         "bookmark1 bookmark2" "\x1e"
                         "" "\x1e" ; git_head
                         "" "\x1e" ; conflict
                         "" "\x1e" ; signature
                         "" "\x1e" ; empty
                         "Short description" "\x1e"
                         "commit-id-full" "\x1e"
                         "2023-01-01 12:00:00" "\x1e"
                         "\"Long description\"" ; json encoded
                         "\n"))
         (majutsu-executable "jj") ; Mock executable
         (entries (cl-letf (((symbol-function 'majutsu-run-jj)
                             (lambda (&rest _) sample-output)))
                    (majutsu-parse-log-entries))))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (equal (plist-get entry :change-id) "change-id"))
      (should (equal (plist-get entry :commit_id) "commit-id-full"))
      (should (equal (plist-get entry :author) "Author Name <author@example.com>"))
      (should (equal (plist-get entry :short-desc) "Short description"))
      (should (equal (plist-get entry :bookmarks) "bookmark1 bookmark2"))
      (should (equal (plist-get entry :timestamp) "2023-01-01 12:00:00"))
      (should (equal (plist-get entry :long-desc) "Long description")))))

(ert-deftest majutsu-log-parse-entries-multiple ()
  "Parse multiple log entries."
  (let* ((sample-output (concat
                         "@" "\x1e" "c1" "\x1e" "A1" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "Desc1" "\x1e" "cid1" "\x1e" "ts1" "\x1e" "\"Long1\"" "\n"
                         "o" "\x1e" "c2" "\x1e" "A2" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "Desc2" "\x1e" "cid2" "\x1e" "ts2" "\x1e" "\"Long2\"" "\n"))
         (entries (cl-letf (((symbol-function 'majutsu-run-jj)
                             (lambda (&rest _) sample-output)))
                    (majutsu-parse-log-entries))))
    (should (= (length entries) 2))
    (should (equal (plist-get (nth 0 entries) :change-id) "c1"))
    (should (equal (plist-get (nth 1 entries) :change-id) "c2"))))

(ert-deftest majutsu-log-parse-entries-with-graph-lines ()
  "Parse entries interleaved with graph lines."
  (let* ((sample-output (concat
                         "@" "\x1e" "c1" "\x1e" "A1" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "Desc1" "\x1e" "cid1" "\x1e" "ts1" "\x1e" "\"Long1\"" "\n"
                         "|" "\n"
                         "o" "\x1e" "c2" "\x1e" "A2" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "" "\x1e" "Desc2" "\x1e" "cid2" "\x1e" "ts2" "\x1e" "\"Long2\"" "\n"))
         (entries (cl-letf (((symbol-function 'majutsu-run-jj)
                             (lambda (&rest _) sample-output)))
                    (majutsu-parse-log-entries))))
    (should (= (length entries) 2))
    (let ((entry1 (nth 0 entries)))
      (should (equal (plist-get entry1 :change-id) "c1"))
      (should (equal (plist-get entry1 :suffix-lines) '("|"))))))

(ert-deftest majutsu-log-refresh-uses-explicit-target ()
  "Respect explicit target ids when refreshing the log."
  (let (goto-args fallback-called)
    (cl-letf (((symbol-function 'majutsu--root)
               (lambda () default-directory))
              ((symbol-function 'majutsu-run-jj-async)
               (lambda (_args callback &optional _err _color)
                 (funcall callback "output")
                 nil))
              ((symbol-function 'majutsu-parse-log-entries)
               (lambda (&rest _) '()))
              ((symbol-function 'majutsu-log-render)
               (lambda ()))
              ((symbol-function 'majutsu--goto-log-entry)
               (lambda (change commit)
                 (setq goto-args (list change commit))
                 t))
              ((symbol-function 'majutsu-log-goto-@)
               (lambda ()
                 (setq fallback-called t))))
      (with-temp-buffer
        (majutsu-log-refresh "target-change" "target-commit")))
    (should (equal goto-args '("target-change" "target-commit")))
    (should-not fallback-called)))

(provide 'majutsu-log-test)

;;; majutsu-log-test.el ends here
