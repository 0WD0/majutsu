;;; majutsu-interactive-test.el --- Tests for majutsu-interactive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for helper logic in `majutsu-interactive.el'.

;;; Code:

(require 'ert)
(require 'majutsu-interactive)

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

(provide 'majutsu-interactive-test)
;;; majutsu-interactive-test.el ends here
