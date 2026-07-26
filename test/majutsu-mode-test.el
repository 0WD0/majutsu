;;; majutsu-mode-test.el --- Tests for majutsu-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the buffer refresh machinery in `majutsu-mode.el'.

;;; Code:

(require 'ert)
(require 'majutsu-mode)

(defvar majutsu-mode-test--refresh-calls 0
  "Number of times the dummy refresh function ran.")

(defvar majutsu-mode-test--refresh-depth 0
  "Current nesting depth of the dummy refresh function.")

(defvar majutsu-mode-test--max-depth 0
  "Deepest observed nesting of the dummy refresh function.")

(defvar majutsu-mode-test--reenter nil
  "Function simulating a sentinel firing inside the dummy refresh, or nil.")

(define-derived-mode majutsu-mode-test-dummy-mode majutsu-mode "MjDummy"
  "Throwaway Majutsu mode for refresh tests."
  :interactive nil)

(defun majutsu-mode-test-dummy-refresh-buffer ()
  "Render one section and optionally re-enter the refresh machinery."
  (cl-incf majutsu-mode-test--refresh-calls)
  (let ((majutsu-mode-test--refresh-depth
         (1+ majutsu-mode-test--refresh-depth)))
    (setq majutsu-mode-test--max-depth
          (max majutsu-mode-test--max-depth
               majutsu-mode-test--refresh-depth))
    (magit-insert-section (dummybuf)
      (insert (format "render %d\n" majutsu-mode-test--refresh-calls)))
    (when majutsu-mode-test--reenter
      (let ((reenter majutsu-mode-test--reenter))
        (setq majutsu-mode-test--reenter nil)
        (funcall reenter)))))

(defmacro majutsu-mode-test--with-dummy-buffer (&rest body)
  "Run BODY in a dummy Majutsu buffer with refresh counters reset."
  (declare (indent 0))
  `(with-temp-buffer
     (setq majutsu-mode-test--refresh-calls 0
           majutsu-mode-test--refresh-depth 0
           majutsu-mode-test--max-depth 0
           majutsu-mode-test--reenter nil)
     (delay-mode-hooks (majutsu-mode-test-dummy-mode))
     ,@body))

(ert-deftest majutsu-mode-refresh/plain-refresh-runs-once ()
  "A refresh without interference renders exactly once."
  (majutsu-mode-test--with-dummy-buffer
    (majutsu-refresh-buffer-internal)
    (should (= majutsu-mode-test--refresh-calls 1))
    (should (equal (buffer-string) "render 1\n"))))

(ert-deftest majutsu-mode-refresh/reentrant-refresh-is-deferred ()
  "A refresh arriving mid-rebuild must not nest; it reruns afterwards."
  (majutsu-mode-test--with-dummy-buffer
    (setq majutsu-mode-test--reenter #'majutsu-refresh-buffer-internal)
    (majutsu-refresh-buffer-internal)
    ;; The deferred request reran the refresh sequentially, never nested.
    (should (= majutsu-mode-test--refresh-calls 2))
    (should (= majutsu-mode-test--max-depth 1))
    (should (equal (buffer-string) "render 2\n"))))

(ert-deftest majutsu-mode-refresh/multiple-nested-requests-coalesce ()
  "Several refresh requests during one rebuild collapse into one rerun."
  (majutsu-mode-test--with-dummy-buffer
    (setq majutsu-mode-test--reenter
          (lambda ()
            (majutsu-refresh-buffer-internal)
            (majutsu-refresh-buffer-internal)
            (majutsu-refresh-buffer-internal)))
    (majutsu-refresh-buffer-internal)
    (should (= majutsu-mode-test--refresh-calls 2))
    (should (= majutsu-mode-test--max-depth 1))))

(ert-deftest majutsu-mode-refresh/guard-is-released-on-error ()
  "A refresh that signals must release the guard for later refreshes."
  (majutsu-mode-test--with-dummy-buffer
    (setq majutsu-mode-test--reenter (lambda () (error "boom")))
    (should-error (majutsu-refresh-buffer-internal))
    (should-not majutsu--refreshing)
    (setq majutsu-mode-test--reenter nil)
    (majutsu-refresh-buffer-internal)
    (should (= majutsu-mode-test--refresh-calls 2))))

(ert-deftest majutsu-mode-refresh/error-drops-pending-request ()
  "A failing rebuild must not leak its deferred request into later ones."
  (majutsu-mode-test--with-dummy-buffer
    (setq majutsu-mode-test--reenter
          (lambda ()
            (majutsu-refresh-buffer-internal)
            (error "boom")))
    (should-error (majutsu-refresh-buffer-internal))
    (should-not majutsu--refreshing)
    (should-not majutsu--refresh-pending)
    (setq majutsu-mode-test--reenter nil)
    (let ((calls majutsu-mode-test--refresh-calls))
      (majutsu-refresh-buffer-internal)
      ;; Exactly one rebuild: the stale pending flag must not double it.
      (should (= majutsu-mode-test--refresh-calls (1+ calls))))))

(provide 'majutsu-mode-test)
;;; majutsu-mode-test.el ends here
