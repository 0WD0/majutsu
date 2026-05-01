;;; majutsu-op-test.el --- Tests for operation buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for jj operation integration.

;;; Code:

(require 'ert)
(require 'majutsu-op)
(require 'transient)

(defun majutsu-op-test--summary (&optional change-id change-short commit-id commit-short flags desc)
  "Return a structured op diff commit summary for tests."
  (let ((flags (or flags '("" "" ""))))
    (string-join (list (or change-id "change-full")
                       (or change-short "change-short")
                       (or commit-id "commit-full")
                       (or commit-short "commit-short")
                       (nth 0 flags)
                       (nth 1 flags)
                       (nth 2 flags)
                       (or desc "description"))
                 majutsu-op--field-separator)))

(defun majutsu-op-test--colored-summary ()
  "Return a colored structured op diff commit summary for tests."
  (string-join (list "\e[35mchange-full\e[39m"
                     "\e[35mchange-short\e[39m"
                     "\e[34mcommit-full\e[39m"
                     "\e[34mcommit-short\e[39m"
                     ""
                     ""
                     "empty"
                     "\e[32mdescription\e[39m")
               majutsu-op--field-separator))

(ert-deftest majutsu-op-log-template/carries-full-and-display-id ()
  "Operation log template should carry full and display operation ids."
  (should (string-match-p "self.id()" majutsu-op--log-template))
  (should (string-match-p "self.id().short()" majutsu-op--log-template))
  (should (string-match-p "self.description().first_line()"
                          majutsu-op--log-template)))

(ert-deftest majutsu-op-commit-summary-template/carries-full-ids ()
  "Operation diff commit summary should carry full ids for actions."
  (should (string-match-p "self.change_id()"
                          majutsu-op--commit-summary-template))
  (should (string-match-p "self.change_id().short()"
                          majutsu-op--commit-summary-template))
  (should (string-match-p "self.commit_id()"
                          majutsu-op--commit-summary-template))
  (should (string-match-p "self.commit_id().short()"
                          majutsu-op--commit-summary-template))
  (should-not (string-match-p "separate(" majutsu-op--commit-summary-template)))

(ert-deftest majutsu-op-parse-log-entries/strips-machine-id-and-keeps-display-face ()
  "Parser should strip ANSI from machine ids and keep display field faces."
  (let* ((raw (concat "\e[31mfull-operation-id\e[0m"
                      majutsu-op--field-separator
                      "\e[32mshort-id\e[0m"
                      majutsu-op--field-separator
                      "user"
                      majutsu-op--field-separator
                      "workspace"
                      majutsu-op--field-separator
                      "2 minutes ago"
                      majutsu-op--field-separator
                      "op"
                      majutsu-op--field-separator
                      "description\n"))
         (entry (car (majutsu-parse-op-log-entries nil raw)))
         (short (plist-get entry :op-id-short)))
    (should (equal (plist-get entry :op-id) "full-operation-id"))
    (should (equal short "short-id"))
    (should (get-text-property 0 'font-lock-face short))
    (should (equal (plist-get entry :kind) "op"))
    (should (equal (plist-get entry :desc) "description"))))

(ert-deftest majutsu-op-parse-log-entries/rejects-malformed-records ()
  "Malformed records should be ignored instead of creating bad sections."
  (should-not (majutsu-parse-op-log-entries nil "only-one-field\n")))

(ert-deftest majutsu-op-parse-show-line/parses-metadata-record ()
  "Operation show metadata parser should keep full operation ids."
  (let* ((line (string-join '("full-id" "short-id" "user" "workspace"
                              "start" "end" "duration" "op" "desc")
                            majutsu-op--field-separator))
         (entry (majutsu-op--parse-show-line line)))
    (should (equal (plist-get entry :op-id) "full-id"))
    (should (equal (plist-get entry :op-id-short) "short-id"))
    (should (equal (plist-get entry :start-time) "start"))
    (should (equal (plist-get entry :desc) "desc"))))

(ert-deftest majutsu-op-parse-diff-output/parses-colored-commit-line ()
  "Operation diff parser should use plain marker detection and colored fields."
  (let* ((line (concat "\e[32m+\e[39m " (majutsu-op-test--colored-summary)))
         (nodes (majutsu-op--parse-diff-output
                 (concat "From operation: from\n  To operation: to\n\n"
                         "Changed commits:\n" line "\n")))
         (group (car nodes))
         (entry (car (plist-get group :children)))
         (value (plist-get entry :value)))
    (should (equal (plist-get group :kind) 'commits))
    (should (equal (plist-get entry :type) 'commit-line))
    (should (equal (plist-get value :marker) "+"))
    (should (equal (plist-get value :change-id) "change-full"))
    (should (equal (plist-get value :commit-id) "commit-full"))
    (should (plist-get value :empty))
    (should (get-text-property 0 'font-lock-face
                               (plist-get value :change-id-short-display)))))

(ert-deftest majutsu-op-parse-diff-output/parses-ref-prefixes-and-absent ()
  "Operation diff parser should parse ref target prefixes and absent values."
  (let* ((nodes (majutsu-op--parse-diff-output
                 (concat "Changed remote bookmarks:\n"
                         "main@origin:\n"
                         "+ tracked " (majutsu-op-test--summary) "\n"
                         "- untracked (absent)\n")))
         (group (car nodes))
         (ref (car (plist-get group :children)))
         (entries (plist-get ref :children))
         (added (plist-get (nth 0 entries) :value))
         (removed (plist-get (nth 1 entries) :value)))
    (should (equal (plist-get group :kind) 'remote-bookmarks))
    (should (equal (plist-get ref :name) "main@origin"))
    (should (equal (plist-get added :prefix) "tracked"))
    (should (equal (plist-get added :commit-id) "commit-full"))
    (should (equal (plist-get removed :prefix) "untracked"))
    (should (plist-get removed :absent))))

(ert-deftest majutsu-op-format-diff-line/hides-machine-separators ()
  "Formatted operation diff lines should not expose machine separators."
  (let* ((value (append '(:marker "+")
                        (majutsu-op--parse-commit-summary
                         (majutsu-op-test--summary))))
         (line (majutsu-op--format-diff-line value)))
    (should (string-match-p "change-short commit-short" line))
    (should-not (string-match-p (regexp-quote majutsu-op--field-separator)
                                line))))

(ert-deftest majutsu-op-show/passes-operation-binding-to-buffer-setup ()
  "majutsu-op-show should pass operation as a buffer-local binding."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-op-show--buffer-name)
               (lambda (_operation) "*op*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-op-show "abc") 'buffer))
      (should (eq (nth 0 captured) #'majutsu-op-show-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-op-show--operation "abc"))))
      (should (equal (nth 3 captured)
                     '(:buffer "*op*" :directory "/repo/"))))))

(ert-deftest majutsu-op-show-refresh-buffer/inserts-metadata-and-diff-body ()
  "Operation show refresh should insert metadata and body content."
  (cl-letf (((symbol-function 'majutsu-op--show-metadata)
             (lambda (_operation)
               (list :op-id "full-id"
                     :op-id-short "short-id"
                     :user "user"
                     :workspace "workspace"
                     :start-time "start"
                     :end-time "end"
                     :duration "duration"
                     :kind "op")))
            ((symbol-function 'majutsu-op--operation-body)
             (lambda (_operation template)
               (if (string-match-p "tags" template)
                   "args: jj op\n"
                 "description body\n")))
            ((symbol-function 'majutsu-op--operation-diff-output)
             (lambda (_operation)
               (concat "Changed commits:\n+ "
                       (majutsu-op-test--summary)
                       "\n"))))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (setq majutsu-op-show--operation "@")
      (let ((inhibit-read-only t))
        (majutsu-op-show-refresh-buffer))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "User: user" content))
        (should (string-match-p "description body" content))
        (should (string-match-p "args: jj op" content))
        (should (string-match-p "Changed commits" content))
        (should (string-match-p "change-short commit-short" content))
        (should-not (string-match-p (regexp-quote majutsu-op--field-separator)
                                    content))))))

(ert-deftest majutsu-op-show-refresh-buffer/inserts-line-level-sections ()
  "Operation show refresh should attach section values to parsed diff lines."
  (cl-letf (((symbol-function 'majutsu-op--show-metadata)
             (lambda (_operation)
               (list :op-id "full-id"
                     :op-id-short "short-id"
                     :user "user"
                     :workspace "workspace"
                     :start-time "start"
                     :end-time "end"
                     :duration "duration"
                     :kind "op")))
            ((symbol-function 'majutsu-op--operation-body)
             (lambda (&rest _) ""))
            ((symbol-function 'majutsu-op--operation-diff-output)
             (lambda (_operation)
               (concat "From operation: old\n  To operation: new\n\n"
                       "Changed commits:\n"
                       "+ " (majutsu-op-test--summary
                              "change-full" "change-short"
                              "commit-full" "commit-short") "\n\n"
                       "Changed local bookmarks:\n"
                       "main:\n"
                       "+ (added) " (majutsu-op-test--summary
                                      "ref-change-full" "ref-change-short"
                                      "ref-commit-full" "ref-commit-short") "\n"
                       "- (absent)\n"))))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (setq majutsu-op-show--operation "@")
      (let ((inhibit-read-only t))
        (majutsu-op-show-refresh-buffer))
      (goto-char (point-min))
      (search-forward "commit-short")
      (let ((value (magit-section-value-if 'jj-op-commit-line)))
        (should (equal (plist-get value :commit-id) "commit-full"))
        (should-not (plist-get value :absent)))
      (search-forward "ref-commit-short")
      (let ((value (magit-section-value-if 'jj-op-ref-line)))
        (should (equal (plist-get value :prefix) "(added)"))
        (should (equal (plist-get value :commit-id) "ref-commit-full")))
      (search-forward "(absent)")
      (let ((value (magit-section-value-if 'jj-op-ref-line)))
        (should (plist-get value :absent))))))

(ert-deftest majutsu-op-show-diff-at-point/uses-full-commit-id ()
  "Diff action should pass full commit-id revsets to ordinary diff."
  (let ((value '(:marker "+" :commit-id "full-commit")))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-op-commit-line value t)
          (magit-insert-heading "line")))
      (goto-char (point-min))
      (let (captured)
        (cl-letf (((symbol-function 'majutsu-diff-revset)
                   (lambda (revset &rest _)
                     (setq captured revset))))
          (majutsu-op-show-diff-at-point))
        (should (equal captured "commit_id(full-commit)"))))))

(ert-deftest majutsu-op-show-mode-map/line-actions ()
  "Operation show mode should expose line-level actions."
  (should (eq (lookup-key majutsu-op-show-mode-map (kbd "RET"))
              'majutsu-op-show-default-action))
  (should (eq (lookup-key majutsu-op-show-mode-map (kbd "d"))
              'majutsu-op-show-diff-at-point))
  (should (eq (lookup-key majutsu-op-show-mode-map (kbd "v"))
              'majutsu-op-show-evolog-at-point)))

(ert-deftest majutsu-op-transient/exposes-log-show-undo-redo ()
  "Operation transient should expose initial operation actions."
  (should (transient-get-suffix 'majutsu-op-transient "l"))
  (should (transient-get-suffix 'majutsu-op-transient "s"))
  (should (transient-get-suffix 'majutsu-op-transient "u"))
  (should (transient-get-suffix 'majutsu-op-transient "r")))

(ert-deftest majutsu-op-log-mode-map/show-at-point ()
  "Operation log mode should make RET open operation details."
  (should (eq (lookup-key majutsu-op-log-mode-map (kbd "RET"))
              'majutsu-op-log-show-at-point))
  (should (eq (lookup-key majutsu-mode-map (kbd "X"))
              'majutsu-op-transient)))

;;; _
(provide 'majutsu-op-test)
;;; majutsu-op-test.el ends here
