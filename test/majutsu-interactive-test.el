;;; majutsu-interactive-test.el --- Tests for majutsu-interactive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for helper logic in `majutsu-interactive.el'.

;;; Code:

(require 'ert)
(require 'majutsu-interactive)
(require 'majutsu-split)

(defun majutsu-interactive-test--find-section (type value)
  "Find a section by TYPE and VALUE in the current buffer."
  (let (result)
    (magit-map-sections
     (lambda (section)
       (when (and (not result)
                  (eq (oref section type) type)
                  (equal (oref section value) value)
                  ;; Skip diffstat file sections when a real diff file section
                  ;; is available.
                  (or (not (eq type 'jj-file)) (oref section header)))
         (setq result section)))
     magit-root-section)
    result))

(defun majutsu-interactive-test--insert-binary-diff ()
  "Insert a binary-only diff and return its file section."
  (magit-section-mode)
  (let* ((inhibit-read-only t)
         (magit-section-inhibit-markers t)
         (output (string-join
                  '("bin.dat | (binary) +3 bytes"
                    "1 file changed, 0 insertions(+), 0 deletions(-)"
                    "diff --git a/bin.dat b/bin.dat"
                    "new file mode 100644"
                    "index 0000000000..8352675d67"
                    "Binary files /dev/null and b/bin.dat differ")
                  "\n")))
    (magit-insert-section (diff-root)
      (insert output)
      (save-restriction
        (narrow-to-region (point-min) (point-max))
        (majutsu-diff-wash-diffs '("--stat")))))
  (majutsu-interactive-test--find-section 'jj-file "bin.dat"))

(ert-deftest majutsu-interactive--build-tool-config/strips-tramp-prefix ()
  "Tool config should pass local remote paths to jj merge-tool args."
  (let ((config
         (cl-letf (((symbol-function 'majutsu-interactive--write-applypatch-script)
                    (lambda (_reverse &optional _file-ops)
                      "/ssh:demo:/tmp/applypatch.sh"))
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

(ert-deftest majutsu-interactive-toggle-file/selects-binary-file-change ()
  "File selection should handle binary diffs with no text hunks."
  (with-temp-buffer
    (let ((file-section (majutsu-interactive-test--insert-binary-diff)))
      (should (eieio-object-p file-section))
      (should-not (oref file-section children))
      (goto-char (oref file-section start))
      (majutsu-interactive-toggle-file)
      (let ((operation (majutsu-interactive-build-operation-if-selected)))
        (should-not (plist-get operation :patch))
        (should (equal (plist-get operation :file-ops)
                       '((:path "bin.dat" :delete-paths nil))))))))

(ert-deftest majutsu-interactive-toggle-file/rejects-binary-file-in-non-split-transient ()
  "Non-split transients should not record unsupported whole-file selections."
  (with-temp-buffer
    (let ((file-section (majutsu-interactive-test--insert-binary-diff))
          (transient-current-command 'majutsu-restore))
      (goto-char (oref file-section start))
      (should-error (majutsu-interactive-toggle-file) :type 'user-error)
      (should-not (majutsu-interactive-build-operation-if-selected)))))

(ert-deftest majutsu-interactive--write-applypatch-script/replays-file-ops ()
  "Applypatch script should replay selected whole-file paths after reset."
  (let ((majutsu-interactive--temp-dir nil)
        (majutsu-interactive--temp-dir-remote nil)
        (temporary-file-directory temporary-file-directory))
    (let* ((dir (make-temp-file "majutsu-interactive-test-" t))
           (left (expand-file-name "left" dir))
           (right (expand-file-name "right" dir))
           (patch (expand-file-name "patch.diff" dir))
           (script nil))
      (unwind-protect
          (progn
            (make-directory left)
            (make-directory right)
            (with-temp-file (expand-file-name "base.txt" left)
              (insert "base"))
            (with-temp-file (expand-file-name "old.bin" left)
              (insert "old-base"))
            (with-temp-file (expand-file-name "bin.dat" right)
              (set-buffer-multibyte nil)
              (insert (string 0 1 2)))
            (with-temp-file (expand-file-name "unselected.dat" right)
              (insert "do not preserve"))
            (with-temp-file patch)
            (cl-letf (((symbol-function 'majutsu-interactive--temp-dir)
                       (lambda () dir)))
              (setq script
                    (majutsu-interactive--write-applypatch-script
                     t '((:path "bin.dat" :delete-paths ("old.bin"))))))
            (let ((contents (with-temp-buffer
                              (insert-file-contents script)
                              (buffer-string))))
              (should-not (string-match-p
                           "cp -a \\\"\\$RIGHT\\\"/\\. \\\"\\$ORIGINAL_RIGHT\\\"/"
                           contents))
              (should (string-match-p
                       "majutsu_preserve_path \\\"\\$RIGHT\\\"/bin\\.dat"
                       contents)))
            (should (= 0 (call-process script nil nil nil left right patch)))
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally
                              (expand-file-name "bin.dat" right))
                             (buffer-string))
                           (string 0 1 2)))
            (should (equal (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name "base.txt" right))
                             (buffer-string))
                           "base"))
            (should-not (file-exists-p (expand-file-name "old.bin" right)))
            (should-not (file-exists-p (expand-file-name "unselected.dat" right))))
        (delete-directory dir t)))))

(ert-deftest majutsu-split-execute/forwards-file-ops-operation ()
  "Split execution should forward file-op-only operations to the patch runner."
  (let* ((file-ops '((:path "bin.dat" :delete-paths nil)))
         (operation (list :patch nil :file-ops file-ops))
         captured
         cleared)
    (cl-letf (((symbol-function 'majutsu-interactive--selection-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'majutsu-interactive-build-operation-if-selected)
               (lambda (_buffer _invert _include-all-files &optional _context-on-added)
                 operation))
              ((symbol-function 'majutsu-interactive-run-with-patch)
               (lambda (command args patch &optional reverse passed-file-ops)
                 (setq captured (list command args patch reverse passed-file-ops))))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-split-execute '("--interactive" "--tool=custom" "--revision=@"))
      (should (equal captured
                     (list "split" '("--revision=@") nil t file-ops)))
      (should cleared))))

(provide 'majutsu-interactive-test)
;;; majutsu-interactive-test.el ends here
