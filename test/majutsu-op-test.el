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
                          majutsu-op--commit-summary-template)))

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
               "Changed commits:\n+ change\n")))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (setq majutsu-op-show--operation "@")
      (let ((inhibit-read-only t))
        (majutsu-op-show-refresh-buffer))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "User: user" content))
        (should (string-match-p "description body" content))
        (should (string-match-p "args: jj op" content))
        (should (string-match-p "Changed commits:" content))))))

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
