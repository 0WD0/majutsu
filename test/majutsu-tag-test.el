;;; majutsu-tag-test.el --- Tests for tag helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for tag completion, parsing, and command dispatch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-tag)

(ert-deftest majutsu-tag-get-tag-names/local-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("v1.0" "v1.1"))))
      (should (equal (majutsu--get-tag-names 'local) '("v1.0" "v1.1")))
      (should (equal (seq-take seen-args 3) '("tag" "list" "--quiet")))
      (should-not (member "--all-remotes" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "!remote") template))))))

(ert-deftest majutsu-tag-get-tag-names/remote-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("v1.0@git"))))
      (should (equal (majutsu--get-tag-names 'remote) '("v1.0@git")))
      (should (member "--all-remotes" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "remote && present") template))
        (should (string-match-p (regexp-quote "\"@\"") template))))))

(ert-deftest majutsu-tag-candidate-data/builds-annotations ()
  (cl-letf (((symbol-function 'majutsu--get-tag-names)
             (lambda (&optional _scope) '("v1.0" "v1.1"))))
    (let* ((payload (majutsu-tag-candidate-data))
           (candidates (plist-get payload :candidates))
           (annotations (plist-get payload :annotations)))
      (should (equal candidates '("v1.0" "v1.1")))
      (should (hash-table-p annotations))
      (should (equal (gethash "v1.0" annotations) "local tag")))))

(ert-deftest majutsu-tag-read-names/uses-completion-and-prewarm ()
  (let* ((payload (list :candidates '("v1.0" "v1.1")
                        :annotations (make-hash-table :test #'equal)))
         seen-prewarm
         seen-read)
    (cl-letf (((symbol-function 'majutsu-tag-candidate-data)
               (lambda (&optional _directory) payload))
              ((symbol-function 'majutsu-marginalia-prewarm-candidate-data)
               (lambda (category prewarm-payload &optional revset directory)
                 (setq seen-prewarm (list category prewarm-payload revset directory))))
              ((symbol-function 'majutsu-completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match
                        _initial _history _default category)
                 (setq seen-read (list collection category))
                 '("v1.0" ""))))
      (should (equal (majutsu-tag--read-names "Set tag(s)") '("v1.0")))
      (should (equal seen-read
                     (list '("v1.0" "v1.1") 'majutsu-tag)))
      (should (equal (car seen-prewarm) 'majutsu-tag))
      (should (equal (cadr seen-prewarm) payload)))))

(ert-deftest majutsu-tag-parse-list-output/conflicted-block ()
  (let* ((output (concat "conflicted_tag (conflicted):\n"
                         "  - old target\n"
                         "  + new target\n"
                         "v1.0: abcdef\n"
                         "  @git: abcdef\n"))
         (entries (majutsu-tag-parse-list-output output)))
    (should (equal (length entries) 2))
    (should (equal (plist-get (nth 0 entries) :name) "conflicted_tag"))
    (should (equal (length (plist-get (nth 0 entries) :lines)) 3))
    (should (equal (plist-get (nth 1 entries) :name) "v1.0"))
    (should (equal (length (plist-get (nth 1 entries) :lines)) 2))))

(ert-deftest majutsu-tag-set/allow-move-dispatches-correctly ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-tag-set '("v1.0" "release") "@-" t)
      (should (equal called
                     '("tag" "set" "--allow-move" "-r" "@-" "v1.0" "release"))))))

(ert-deftest majutsu-tag-delete/dispatches-correctly ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-tag-delete '("v1.*" "release"))
      (should (equal called
                     '("tag" "delete" "v1.*" "release"))))))

(provide 'majutsu-tag-test)
;;; majutsu-tag-test.el ends here
