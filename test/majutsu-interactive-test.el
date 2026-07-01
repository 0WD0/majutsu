;;; majutsu-interactive-test.el --- Tests for majutsu-interactive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for helper logic in `majutsu-interactive.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-interactive)
(require 'majutsu-restore)
(require 'majutsu-split)
(require 'majutsu-squash)

(ert-deftest majutsu-interactive--build-tool-config/strips-tramp-prefix ()
  "Tool config should pass local remote paths to jj merge-tool args."
  (let ((config
         (cl-letf (((symbol-function 'majutsu-interactive--write-applypatch-script)
                    (lambda (_reverse) "/ssh:demo:/tmp/applypatch.sh"))
                   ((symbol-function 'majutsu-convert-filename-for-jj)
                    (lambda (path)
                      (pcase path
                        ("/ssh:demo:/tmp/applypatch.sh" "/tmp/applypatch.sh")
                        ("/ssh:demo:/tmp/patch.diff" "/tmp/patch.diff")
                        (_ path)))))
           (majutsu-interactive--build-tool-config "/ssh:demo:/tmp/patch.diff" nil))))
    (should (equal (length config) 4))
    (should (string-match-p "merge-tools\\.majutsu-applypatch\\.program=/tmp/applypatch\\.sh"
                            (nth 1 config)))
    (should (string-match-p "\\\"/tmp/patch\\.diff\\\"" (nth 3 config)))
    (should-not (string-match-p "/ssh:demo:" (nth 1 config)))
    (should-not (string-match-p "/ssh:demo:" (nth 3 config)))))

(ert-deftest majutsu-interactive--temp-dir/uses-nearby-temp-file ()
  "Temp dir helper should allocate directories near current workspace."
  (let ((majutsu-interactive--temp-dir nil)
        (majutsu-interactive--temp-dir-remote nil)
        seen)
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (_path) nil))
              ((symbol-function 'make-nearby-temp-file)
               (lambda (prefix dir-flag &optional suffix)
                 (setq seen (list prefix dir-flag suffix))
                 "/tmp/majutsu-interactive-dir")))
      (should (equal (majutsu-interactive--temp-dir)
                     "/tmp/majutsu-interactive-dir"))
      (should (equal seen '("majutsu-interactive-" t nil))))))

(ert-deftest majutsu-interactive--temp-dir/recreates-when-remote-prefix-changes ()
  "Temp dir cache should be invalidated when host context changes."
  (let ((default-directory "/tmp/")
        (majutsu-interactive--temp-dir nil)
        (majutsu-interactive--temp-dir-remote nil)
        seen)
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (_path) t))
              ((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   (if (string-prefix-p "/ssh:demo:" default-directory)
                       "/ssh:demo:"
                     nil))))
              ((symbol-function 'make-nearby-temp-file)
               (lambda (_prefix _dir-flag &optional _suffix)
                 (let ((value (format "/tmp/majutsu-interactive-%d" (length seen))))
                   (push value seen)
                   value))))
      (let ((local (majutsu-interactive--temp-dir)))
        (setq default-directory "/ssh:demo:/tmp/")
        (let ((remote (majutsu-interactive--temp-dir)))
          (should (not (equal local remote)))
          (should (equal (length seen) 2)))))))

(defun majutsu-interactive-test--with-selection-cwd (fn)
  "Call FN with a selection buffer and another current buffer.
FN is called as (FN SELECTION-DIR CALLER-DIR)."
  (let* ((selection-dir (file-name-as-directory
                         (make-temp-file "majutsu-selection-" t)))
         (caller-dir (file-name-as-directory
                      (make-temp-file "majutsu-caller-" t)))
         (selection-buf (generate-new-buffer " *majutsu-selection-test*"))
         (caller-buf (generate-new-buffer " *majutsu-caller-test*")))
    (unwind-protect
        (progn
          (with-current-buffer selection-buf
            (setq default-directory selection-dir))
          (with-current-buffer caller-buf
            (setq default-directory caller-dir))
          (cl-letf (((symbol-function 'majutsu-interactive--selection-buffer)
                     (lambda () selection-buf)))
            (with-current-buffer caller-buf
              (funcall fn selection-dir caller-dir))))
      (when (buffer-live-p selection-buf)
        (kill-buffer selection-buf))
      (when (buffer-live-p caller-buf)
        (kill-buffer caller-buf))
      (delete-directory selection-dir t)
      (delete-directory caller-dir t))))

(ert-deftest majutsu-restore-execute/patch-runs-in-selection-buffer-directory ()
  "Partial restore should run jj where the selected patch was built."
  (majutsu-interactive-test--with-selection-cwd
   (lambda (selection-dir caller-dir)
     (let (seen-run-dir seen-args seen-clear-dir)
       (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
                  (lambda (&rest _args) "patch"))
                 ((symbol-function 'majutsu-interactive-run-with-patch)
                  (lambda (&rest args)
                    (setq seen-run-dir default-directory
                          seen-args args)))
                 ((symbol-function 'majutsu-interactive-clear)
                  (lambda ()
                    (setq seen-clear-dir default-directory))))
         (majutsu-restore-execute '("--changes-in=@" "--interactive")))
       (should (equal seen-run-dir selection-dir))
       (should (not (equal seen-run-dir caller-dir)))
       (should (equal seen-clear-dir selection-dir))
       (should (equal seen-args
                      '("restore" ("--changes-in=@") "patch")))))))

(ert-deftest majutsu-split-execute/patch-runs-in-selection-buffer-directory ()
  "Partial split should run jj where the selected patch was built."
  (majutsu-interactive-test--with-selection-cwd
   (lambda (selection-dir caller-dir)
     (let (seen-run-dir seen-args seen-clear-dir)
       (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
                  (lambda (&rest _args) "patch"))
                 ((symbol-function 'majutsu-interactive-run-with-patch)
                  (lambda (&rest args)
                    (setq seen-run-dir default-directory
                          seen-args args)))
                 ((symbol-function 'majutsu-interactive-clear)
                  (lambda ()
                    (setq seen-clear-dir default-directory))))
         (majutsu-split-execute '("--revision=@" "--interactive")))
       (should (equal seen-run-dir selection-dir))
       (should (not (equal seen-run-dir caller-dir)))
       (should (equal seen-clear-dir selection-dir))
       (should (equal seen-args
                      '("split" ("--revision=@") "patch" t)))))))

(ert-deftest majutsu-squash-execute/patch-runs-in-selection-buffer-directory ()
  "Partial squash should run jj where the selected patch was built."
  (majutsu-interactive-test--with-selection-cwd
   (lambda (selection-dir caller-dir)
     (let (seen-run-dir seen-args seen-clear-dir)
       (cl-letf (((symbol-function 'majutsu-interactive-build-patch-if-selected)
                  (lambda (&rest _args) "patch"))
                 ((symbol-function 'majutsu-interactive-run-with-patch)
                  (lambda (&rest args)
                    (setq seen-run-dir default-directory
                          seen-args args)))
                 ((symbol-function 'majutsu-interactive-clear)
                  (lambda ()
                    (setq seen-clear-dir default-directory))))
         (majutsu-squash-execute '("--revision=@")))
       (should (equal seen-run-dir selection-dir))
       (should (not (equal seen-run-dir caller-dir)))
       (should (equal seen-clear-dir selection-dir))
       (should (equal seen-args
                      '("squash" ("--revision=@") "patch" t)))))))

(provide 'majutsu-interactive-test)
;;; majutsu-interactive-test.el ends here
