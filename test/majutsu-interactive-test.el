;;; majutsu-interactive-test.el --- Tests for majutsu-interactive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for helper logic in `majutsu-interactive.el'.

;;; Code:

(require 'ert)
(require 'majutsu-interactive)

(defun majutsu-interactive-test--insert-diff (diff &optional file)
  "Wash DIFF and return its real jj-file section, optionally matching FILE."
  (majutsu-diff-mode)
  (let ((inhibit-read-only t)
        (magit-section-inhibit-markers t))
    (magit-insert-section (root)
      (insert diff)
      (save-restriction
        (narrow-to-region (point-min) (point-max))
        (majutsu-diff-wash-diffs '("--git")))))
  (let (result)
    (magit-map-sections
     (lambda (section)
       (when (and (not result)
                  (magit-section-match 'jj-file section)
                  (oref section header)
                  (or (null file) (equal (oref section value) file)))
         (setq result section)))
     magit-root-section)
    result))

(defun majutsu-interactive-test--file-contents (path &optional literal)
  "Return PATH contents, reading literally when LITERAL is non-nil."
  (with-temp-buffer
    (if literal
        (insert-file-contents-literally path)
      (insert-file-contents path))
    (buffer-string)))

(ert-deftest majutsu-interactive-run-with-patch/inserts-tool-before-filesets ()
  "Patch runner should keep jj options before filesets."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch) "/tmp/patch.diff"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch-file _reverse &optional _file-ops)
                 '("--config" "merge-tools.majutsu-applypatch.program=/tmp/applypatch")))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called (flatten-tree args)))))
      (majutsu-interactive-run-with-patch
       "restore" '("--from=A" "--to=B") '("src/a.el") "PATCH")
      (should (equal called
                     '("restore" "--from=A" "--to=B"
                       "-i" "--tool" "majutsu-applypatch"
                       "--config" "merge-tools.majutsu-applypatch.program=/tmp/applypatch"
                       "--" "src/a.el"))))))

(ert-deftest majutsu-interactive-run-with-patch/normalizes-structured-filesets ()
  "Patch runner should also normalize transient-files groups."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch) "/tmp/patch.diff"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch-file _reverse &optional _file-ops)
                 '("--config" "tool=config")))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called (flatten-tree args)))))
      (majutsu-interactive-run-with-patch
       "split" '("--revision=@") '("src/a.el") "PATCH" t)
      (should (equal called
                     '("split" "--revision=@"
                       "-i" "--tool" "majutsu-applypatch"
                       "--config" "tool=config"
                       "--" "src/a.el"))))))

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

(ert-deftest majutsu-interactive-file-operation/parses-explicit-actions ()
  "Whole-file actions must come from diff metadata, including rename/copy."
  (dolist
      (case
       '(("diff --git a/add.bin b/add.bin\nnew file mode 100644\nBinary files /dev/null and b/add.bin differ\n"
          "add.bin" (:action add :path "add.bin"))
         ("diff --git a/mod.bin b/mod.bin\nindex 123..456 100644\nBinary files a/mod.bin and b/mod.bin differ\n"
          "mod.bin" (:action modify :path "mod.bin"))
         ("diff --git a/del.bin b/del.bin\ndeleted file mode 100644\nBinary files a/del.bin and /dev/null differ\n"
          "del.bin" (:action delete :path "del.bin"))
         ("diff --git a/old.bin b/new.bin\nsimilarity index 100%\nrename from old.bin\nrename to new.bin\n"
          "new.bin" (:action rename :source "old.bin" :path "new.bin"))
         ("diff --git a/src.bin b/copy.bin\nsimilarity index 100%\ncopy from src.bin\ncopy to copy.bin\n"
          "copy.bin" (:action copy :source "src.bin" :path "copy.bin"))
         ("diff --git a/x b/x\nsimilarity index 100%\nrename from \"old\\040name.bin\"\nrename to \"new\\040name.bin\"\n"
          "x" (:action rename :source "old name.bin" :path "new name.bin"))
         ("diff --git a/x b/x\nsimilarity index 100%\ncopy from \"src\\042quote\\134path.bin\"\ncopy to \"dst\\040name.bin\"\n"
          "x" (:action copy :source "src\"quote\\path.bin"
               :path "dst name.bin"))))
    (with-temp-buffer
      (pcase-let ((`(,diff ,file ,expected) case))
        (let ((section (majutsu-interactive-test--insert-diff diff file)))
          (should section)
          (should (equal (majutsu-interactive--file-operation section)
                         expected)))))))

(ert-deftest majutsu-interactive-header-path/rejects-malformed-git-quoting ()
  "Malformed Git C-style paths must not silently become filesystem paths."
  (should-error
   (majutsu-interactive--header-path
    "rename from \"unterminated\\040path\n" "rename from ")
   :type 'user-error)
  (should-error
   (majutsu-interactive--header-path
    "copy from \"valid\"trailing\n" "copy from ")
   :type 'user-error))

(ert-deftest majutsu-interactive-toggle-file/selects-hunkless-file-for-split ()
  "A binary file section is selectable as a whole-file split operation."
  (with-temp-buffer
    (let* ((diff "diff --git a/bin.dat b/bin.dat\nnew file mode 100644\nBinary files /dev/null and b/bin.dat differ\n")
           (section (majutsu-interactive-test--insert-diff diff "bin.dat"))
           (transient-current-command 'majutsu-split))
      (goto-char (oref section start))
      (majutsu-interactive-toggle-file)
      (should
       (equal (majutsu-interactive-build-operation-if-selected nil t t nil)
              '(:patch nil :file-ops ((:action add :path "bin.dat"))))))))

(ert-deftest majutsu-interactive-toggle-file/rejects-hunkless-non-split ()
  "Restore/squash must reject and not record a whole-file selection."
  (dolist (command '(majutsu-restore majutsu-squash nil))
    (with-temp-buffer
      (let* ((diff "diff --git a/bin.dat b/bin.dat\nindex 1..2 100644\nBinary files a/bin.dat and b/bin.dat differ\n")
             (section (majutsu-interactive-test--insert-diff diff "bin.dat"))
             (transient-current-command command))
        (goto-char (oref section start))
        (should-error (majutsu-interactive-toggle-file) :type 'user-error)
        (should-not (majutsu-interactive-has-selections-p))))))

(ert-deftest majutsu-interactive-operation/mixed-does-not-include-unselected-hunks ()
  "A mixed text/binary operation must not carry an unselected text hunk."
  (with-temp-buffer
    (let* ((diff (concat
                  "diff --git a/text.txt b/text.txt\n--- a/text.txt\n+++ b/text.txt\n"
                  "@@ -1 +1 @@\n-old one\n+selected one\n"
                  "@@ -10 +10 @@\n-old two\n+unselected two\n"
                  "diff --git a/bin.dat b/bin.dat\nnew file mode 100644\n"
                  "Binary files /dev/null and b/bin.dat differ\n"))
           (text (majutsu-interactive-test--insert-diff diff "text.txt"))
           (binary (majutsu-interactive--file-section-for-file "bin.dat"))
           (hunk (car (majutsu-interactive--file-section-hunks text))))
      (majutsu-interactive--set-selection
       (majutsu-interactive--hunk-id hunk) :all)
      (majutsu-interactive--set-selection
       (majutsu-interactive--file-id (oref binary value)) :all)
      (let* ((operation (majutsu-interactive-build-operation-if-selected))
             (patch (plist-get operation :patch)))
        (should (string-match-p "selected one" patch))
        (should-not (string-match-p "unselected two" patch))
        (should (equal (plist-get operation :file-ops)
                       '((:action add :path "bin.dat"))))))))

(ert-deftest majutsu-interactive--write-applypatch-script/replays-explicit-ops ()
  "The real helper replays add/modify/delete/rename/copy after text patch."
  (let* ((dir (make-temp-file "majutsu-interactive-script-" t))
         (left (expand-file-name "left" dir))
         (right (expand-file-name "right" dir))
         (patch-file (expand-file-name "patch.diff" dir))
         (ops '((:action add :path "added.bin")
                (:action modify :path "modified.bin")
                (:action delete :path "deleted.bin")
                (:action delete :path "collision")
                (:action rename :source "old.bin" :path "renamed.bin")
                (:action copy :source "source.bin" :path "copied.bin"))))
    (unwind-protect
        (progn
          (make-directory left)
          (make-directory right)
          (dolist (entry '(("text.txt" . "old\n")
                           ("modified.bin" . "old-mod")
                           ("deleted.bin" . "delete-me")
                           ("collision" . "left-file")
                           ("old.bin" . "rename-me")
                           ("source.bin" . "copy-me")))
            (with-temp-file (expand-file-name (car entry) left)
              (insert (cdr entry))))
          (dolist (entry '(("text.txt" . "unselected-right\n")
                           ("added.bin" . "added")
                           ("modified.bin" . "new-mod")
                           ("renamed.bin" . "rename-me")
                           ("source.bin" . "copy-me")
                           ("copied.bin" . "copy-me")
                           ("unselected.bin" . "must-disappear")))
            (with-temp-file (expand-file-name (car entry) right)
              (insert (cdr entry))))
          ;; Exercise the Major regression: the original right path is an
          ;; unrelated directory, but explicit delete must still delete it.
          (make-directory (expand-file-name "collision" right))
          (with-temp-file (expand-file-name "collision/child" right)
            (insert "unrelated"))
          (with-temp-file patch-file
            (insert "diff --git a/text.txt b/text.txt\n--- a/text.txt\n+++ b/text.txt\n@@ -1 +1 @@\n-old\n+selected\n"))
          (let (script)
            (cl-letf (((symbol-function 'majutsu-interactive--temp-dir)
                       (lambda () dir)))
              (setq script
                    (majutsu-interactive--write-applypatch-script t ops)))
            (should (zerop (call-process script nil nil nil
                                         left right patch-file))))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "text.txt" right))
                         "selected\n"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "added.bin" right)) "added"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "modified.bin" right)) "new-mod"))
          (should-not (file-exists-p (expand-file-name "deleted.bin" right)))
          (should-not (file-exists-p (expand-file-name "collision" right)))
          (should-not (file-exists-p (expand-file-name "old.bin" right)))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "renamed.bin" right)) "rename-me"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "source.bin" right)) "copy-me"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "copied.bin" right)) "copy-me"))
          (should-not (file-exists-p (expand-file-name "unselected.bin" right))))
      (delete-directory dir t))))

(provide 'majutsu-interactive-test)
;;; majutsu-interactive-test.el ends here
