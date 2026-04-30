;;; majutsu-remote-test.el --- Tests for majutsu-remote -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for shared Git remote readers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-remote)

(defmacro majutsu-remote-test--with-root (&rest body)
  "Run BODY with repository discovery stubbed for reader tests."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'majutsu--toplevel-safe)
              (lambda (&optional _directory) "/repo/")))
     ,@body))

(ert-deftest majutsu-remote-candidate-data/parses-fetch-and-push-urls ()
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args)
               '("origin git@github.com:0WD0/majutsu.git"
                 "rad rad://z4fugm4aenykjpk8tpvvqjvwtzvwj (push: rad://z4fugm4aenykjpk8tpvvqjvwtzvwj/z6Mk...)"))))
    (let* ((payload (majutsu-remote-candidate-data))
           (entries (plist-get payload :entries))
           (origin (gethash "origin" entries))
           (rad (gethash "rad" entries)))
      (should (equal (plist-get payload :candidates) '("origin" "rad")))
      (should (equal (plist-get origin :fetch-url) "git@github.com:0WD0/majutsu.git"))
      (should (equal (plist-get rad :fetch-url)
                     "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj"))
      (should (equal (plist-get rad :push-url)
                     "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj/z6Mk...")))))

(ert-deftest majutsu-read-remote-name/uses-history-and-category ()
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin" "upstream")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table _predicate _require-match _initial history _default)
                 (setq seen-history history)
                 (let ((metadata (funcall table "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 "origin")))
      (should (equal (majutsu-read-remote-name "Remote") "origin"))
      (should (eq seen-history 'majutsu-remote-name-history))
      (should (eq seen-category 'majutsu-remote)))))

(ert-deftest majutsu-read-remote-name/defaults-to-remote-at-point ()
  "Existing-remote readers should default to the remote section at point."
  (let (seen-default)
    (majutsu-remote-test--with-root
      (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
                 (lambda (&optional _directory)
                   (list :candidates '("origin" "upstream")
                         :entries (make-hash-table :test #'equal))))
                ((symbol-function 'magit-section-value-if)
                 (lambda (_type) "upstream"))
                ((symbol-function 'majutsu-completing-read-payload)
                 (lambda (_prompt _payload _predicate _require-match _initial
                                  _history default _category _context _directory)
                   (setq seen-default default)
                   default)))
        (should (equal (majutsu-read-remote-name "Remote" t) "upstream"))
        (should (equal seen-default "upstream"))))))

(ert-deftest majutsu-read-remote-name/requires-existing-remote-when-requested ()
  "Commands that need an existing remote should fail before prompting if none exist."
  (majutsu-remote-test--with-root
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates nil :entries (make-hash-table :test #'equal)))))
      (should-error (majutsu-read-remote-name "Remote" t)
                    :type 'user-error))))

(ert-deftest majutsu-read-new-remote-name/defaults-origin-when-empty ()
  "Adding the first remote should offer origin as the default name."
  (let (seen-default)
    (majutsu-remote-test--with-root
      (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
                 (lambda (&optional _directory)
                   (list :candidates nil :entries (make-hash-table :test #'equal))))
                ((symbol-function 'majutsu-completing-read-payload)
                 (lambda (_prompt _payload _predicate _require-match _initial
                                  _history default _category _context _directory)
                   (setq seen-default default)
                   "origin")))
        (should (equal (majutsu-read-new-remote-name "Remote name") "origin"))
        (should (equal seen-default "origin"))))))

(ert-deftest majutsu-read-new-remote-name/rejects-existing-name ()
  "New remote readers should reject existing remote names."
  (majutsu-remote-test--with-root
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-completing-read-payload)
               (lambda (&rest _args) "origin")))
      (should-error (majutsu-read-new-remote-name "Remote name")
                    :type 'user-error))))

(ert-deftest majutsu-read-new-remote-name/rejects-jj-incompatible-names ()
  "New remote readers should mirror jj's simple remote-name restrictions."
  (dolist (remote '("git" "team/origin"))
    (majutsu-remote-test--with-root
      (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
                 (lambda (&optional _directory)
                   (list :candidates nil :entries (make-hash-table :test #'equal))))
                ((symbol-function 'majutsu-completing-read-payload)
                 (lambda (&rest _args) remote)))
        (should-error (majutsu-read-new-remote-name "Remote name")
                      :type 'user-error)))))

(ert-deftest majutsu-read-remote-pattern/uses-remote-history-and-category ()
  (let (seen-history seen-category seen-require-match seen-initial)
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin" "upstream")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate require-match initial hist _default)
                 (setq seen-history hist
                       seen-require-match require-match
                       seen-initial initial)
                 (let ((metadata (funcall collection "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 "glob:*")))
      (should (equal (majutsu-read-remote-pattern "Remote" "orig") "glob:*"))
      (should-not seen-require-match)
      (should (equal seen-initial "orig"))
      (should (eq seen-history 'majutsu-remote-pattern-history))
      (should (eq seen-category 'majutsu-remote)))))

(ert-deftest majutsu-read-remote-patterns/uses-remote-history ()
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin" "upstream")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match _initial hist _default)
                 (setq seen-history hist)
                 (let ((metadata (funcall collection "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 '("origin"))))
      (should (equal (majutsu-read-remote-patterns "Remote(s)" '("origin" "upstream"))
                     '("origin")))
      (should (eq seen-history 'majutsu-remote-pattern-history))
      (should (eq seen-category 'majutsu-remote)))))

(ert-deftest majutsu-transient-read-remote-patterns/forwards-initial-and-history ()
  "Transient repeat remote reader should preserve Transient's edit state."
  (let (seen-initial seen-history)
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin" "gerrit")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt _collection _predicate _require-match initial hist _default)
                 (setq seen-initial initial
                       seen-history hist)
                 '("gerrit"))))
      (should (equal (majutsu-transient-read-remote-patterns
                      "Remote" "gerri" 'transient-history)
                     '("gerrit")))
      (should (equal seen-initial "gerri"))
      (should (eq seen-history 'transient-history)))))

;;; majutsu-remote-test.el ends here
