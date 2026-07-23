;;; majutsu-interactive-test.el --- Tests for majutsu-interactive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for helper logic in `majutsu-interactive.el'.

;;; Code:

(require 'ert)
(require 'majutsu-interactive)
(require 'majutsu-jj-integration)

(defun majutsu-interactive-test--insert-diff (diff &optional file metadata)
  "Wash DIFF, attach METADATA, and return the real section matching FILE."
  (majutsu-diff-mode)
  (let ((inhibit-read-only t)
        (magit-section-inhibit-markers t))
    (magit-insert-section (root)
      (insert diff)
      (save-restriction
        (narrow-to-region (point-min) (point-max))
        (majutsu-diff-wash-diffs '("--git"))))
    (when metadata
      (majutsu-diff--attach-file-metadata metadata)))
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

(defun majutsu-interactive-test--select-replacement (file removed added)
  "Select the REMOVED/ADDED replacement in FILE's current washed diff."
  (let* ((file-section
          (majutsu-interactive--file-section-for-file file))
         (hunk (and file-section
                    (car (majutsu-interactive--file-section-hunks
                          file-section))))
         (lines (and hunk (majutsu-interactive--hunk-lines hunk)))
         (removed-line
          (seq-find (lambda (line)
                      (equal (car line) (concat "-" removed "\n")))
                    lines))
         (added-line
          (seq-find (lambda (line)
                      (equal (car line) (concat "+" added "\n")))
                    lines)))
    (should file-section)
    (should hunk)
    (should removed-line)
    (should added-line)
    (majutsu-interactive--set-selection
     (majutsu-interactive--hunk-id hunk)
     (list (cons (nth 2 removed-line) (nth 3 added-line))))))

(defun majutsu-interactive-test--toggle-whole-file (file command)
  "Toggle hunkless FILE for transient COMMAND in the current washed diff."
  (let ((section (majutsu-interactive--file-section-for-file file))
        (transient-current-command command))
    (should section)
    (should-not (majutsu-interactive--file-section-hunks section))
    (goto-char (oref section start))
    (majutsu-interactive-toggle-file)))

(defun majutsu-interactive-test--replacement-plan
    (repo file removed added &optional mode)
  "Select the REMOVED/ADDED replacement in FILE's diff and build MODE's plan."
  (majutsu-jj-integration-with-washed-diff
   repo
   (lambda ()
     (majutsu-interactive-test--select-replacement file removed added)
     (majutsu-interactive-build-replay-plan-if-selected nil mode))))

(defun majutsu-interactive-test--prepare-mixed-changes (repo)
  "Create text, binary, and mode-only changes in REPO and return their paths."
  (let ((text-file (majutsu-jj-integration-file repo "text.txt"))
        (other-file (majutsu-jj-integration-file repo "other.txt"))
        (mode-file (majutsu-jj-integration-file repo "executable.sh"))
        (selected-bin (majutsu-jj-integration-file repo "selected.bin"))
        (remaining-bin (majutsu-jj-integration-file repo "remaining.bin")))
    (majutsu-jj-integration-write-text
     text-file "old one\nunchanged\nold two\n")
    (majutsu-jj-integration-write-text other-file "other old\n")
    (majutsu-jj-integration-write-text mode-file "#!/bin/sh\n")
    (majutsu-jj-integration-write-bytes selected-bin (unibyte-string 0 1))
    (majutsu-jj-integration-write-bytes remaining-bin (unibyte-string 0 3))
    (majutsu-jj-integration-commit repo "base")
    (majutsu-jj-integration-write-text
     text-file "new one\nunchanged\nnew two\n")
    (majutsu-jj-integration-write-text other-file "other new\n")
    (set-file-modes mode-file #o755)
    (majutsu-jj-integration-write-bytes selected-bin (unibyte-string 0 4))
    (majutsu-jj-integration-write-bytes remaining-bin (unibyte-string 0 5))
    (list :text text-file :other other-file :mode mode-file
          :selected-bin selected-bin :remaining-bin remaining-bin)))

(defun majutsu-interactive-test--mixed-plan (repo command &optional mode)
  "Build MODE's plan in REPO for a text region and two whole files.
COMMAND identifies the transient whose whole-file selections are toggled."
  (majutsu-jj-integration-with-washed-diff
   repo
   (lambda ()
     (majutsu-interactive-test--select-replacement
      "text.txt" "old one" "new one")
     (majutsu-interactive-test--toggle-whole-file "selected.bin" command)
     (majutsu-interactive-test--toggle-whole-file "executable.sh" command)
     (majutsu-interactive-build-replay-plan-if-selected nil mode))))

(ert-deftest majutsu-interactive-run-replay-plan/inserts-tool-before-filesets ()
  "Replay plan should keep jj options before filesets."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch &optional _directory) "/tmp/patch.diff"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch-file _plan _directory)
                 '("--config" "merge-tools.majutsu-applypatch.program=/tmp/applypatch")))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called (flatten-tree args)))))
      (majutsu-interactive-run-replay-plan
       "restore" '("--from=A" "--to=B") '("src/a.el")
       '(:base left :payload-root right :patch "PATCH" :file-ops nil))
      (should (equal called
                     '("restore" "--from=A" "--to=B"
                       "-i" "--tool" "majutsu-applypatch"
                       "--config" "merge-tools.majutsu-applypatch.program=/tmp/applypatch"
                       "--" "src/a.el"))))))

(ert-deftest majutsu-interactive-run-replay-plan/normalizes-structured-filesets ()
  "Replay plan should also normalize structured filesets."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch &optional _directory) "/tmp/patch.diff"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch-file _plan _directory)
                 '("--config" "tool=config")))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest args)
                 (setq called (flatten-tree args)))))
      (majutsu-interactive-run-replay-plan
       "split" '("--revision=@") '("src/a.el")
       '(:base left :payload-root right :patch "PATCH" :file-ops nil))
      (should (equal called
                     '("split" "--revision=@"
                       "-i" "--tool" "majutsu-applypatch"
                       "--config" "tool=config"
                       "--" "src/a.el"))))))

(ert-deftest majutsu-interactive--build-tool-config/strips-tramp-prefix ()
  "Tool config should pass local remote paths to jj merge-tool args."
  (let ((config
         (cl-letf (((symbol-function 'majutsu-interactive--write-applypatch-script)
                    (lambda (_plan _directory)
                      "/ssh:demo:/tmp/applypatch.sh"))
                   ((symbol-function 'majutsu-convert-filename-for-jj)
                    (lambda (path)
                      (pcase path
                        ("/ssh:demo:/tmp/applypatch.sh" "/tmp/applypatch.sh")
                        ("/ssh:demo:/tmp/patch.diff" "/tmp/patch.diff")
                        (_ path)))))
           (majutsu-interactive--build-tool-config
            "/ssh:demo:/tmp/patch.diff"
            '(:base left :payload-root right :patch "" :file-ops nil)
            "/ssh:demo:/tmp"))))
    (should (equal (length config) 4))
    (should (string-match-p "merge-tools\\.majutsu-applypatch\\.program=/tmp/applypatch\\.sh"
                            (nth 1 config)))
    (should (string-match-p "\\\"/tmp/patch\\.diff\\\"" (nth 3 config)))
    (should-not (string-match-p "/ssh:demo:" (nth 1 config)))
    (should-not (string-match-p "/ssh:demo:" (nth 3 config)))))

(ert-deftest majutsu-interactive--make-operation-temp-dir/uses-nearby-temp-file ()
  "Each operation should allocate a nearby private directory."
  (let (seen)
    (cl-letf (((symbol-function 'make-nearby-temp-file)
               (lambda (prefix dir-flag &optional suffix)
                 (setq seen (list prefix dir-flag suffix))
                 "/tmp/majutsu-interactive-dir")))
      (should (equal (majutsu-interactive--make-operation-temp-dir)
                     "/tmp/majutsu-interactive-dir"))
      (should (equal seen '("majutsu-interactive-" t nil))))))

(ert-deftest majutsu-interactive-run-replay-plan/uses-private-directory-per-run ()
  "Overlapping async operations must never share patch or helper paths."
  (let (created writes configs cleaned)
    (cl-letf (((symbol-function 'majutsu-interactive--make-operation-temp-dir)
               (lambda ()
                 (let ((dir (format "/tmp/majutsu-operation-%d" (length created))))
                   (push dir created)
                   dir)))
              ((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch &optional directory)
                 (push directory writes)
                 (expand-file-name "patch.diff" directory)))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch _plan _directory)
                 (push _directory configs)
                 nil))
              ((symbol-function 'majutsu-run-jj-with-editor)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-interactive--delete-operation-temp-dir)
               (lambda (directory) (push directory cleaned))))
      (majutsu-interactive-run-replay-plan
       "split" nil nil
       '(:base left :payload-root right :patch "one" :file-ops nil))
      (majutsu-interactive-run-replay-plan
       "split" nil nil
       '(:base left :payload-root right :patch "two" :file-ops nil))
      (should (= (length (delete-dups (copy-sequence created))) 2))
      (should (equal writes configs))
      (should (= (length cleaned) 2)))))

(ert-deftest majutsu-interactive-retain-temp-dir/closes-installation-race ()
  "An exit between liveness check and sentinel install cleans immediately."
  (let ((checks 0)
        installed
        cleaned
        properties
        (original (lambda (&rest _))))
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p)
               (lambda (_process) (= (cl-incf checks) 1)))
              ((symbol-function 'process-sentinel)
               (lambda (_process) original))
              ((symbol-function 'process-put)
               (lambda (_process key value)
                 (setf (alist-get key properties) value)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_process sentinel) (push sentinel installed)))
              ((symbol-function 'majutsu-interactive--delete-operation-temp-dir)
               (lambda (directory) (push directory cleaned))))
      (should (majutsu-interactive--retain-temp-dir-for-process
               'fake "/tmp/private-operation"))
      (should (= checks 2))
      (should (equal installed
                     (list original
                           #'majutsu-interactive--temp-dir-process-sentinel)))
      (should (equal cleaned '("/tmp/private-operation")))
      (should-not (alist-get 'majutsu-interactive-temp-dir properties))
      (should-not
       (alist-get 'majutsu-interactive-original-sentinel properties)))))

(ert-deftest majutsu-interactive-temp-dir-sentinel/cleans-on-exit-and-signal ()
  "The wrapper runs the original sentinel and cleans for both terminal states."
  (dolist (status '(exit signal))
    (let (original-called cleaned)
      (cl-letf (((symbol-function 'process-get)
                 (lambda (_process key)
                   (pcase key
                     ('majutsu-interactive-original-sentinel
                      (lambda (&rest _) (setq original-called t)))
                     ('majutsu-interactive-temp-dir "/tmp/private-operation"))))
                ((symbol-function 'process-status) (lambda (_process) status))
                ((symbol-function 'majutsu-interactive--delete-operation-temp-dir)
                 (lambda (directory) (setq cleaned directory))))
        (majutsu-interactive--temp-dir-process-sentinel 'fake "done")
        (should original-called)
        (should (equal cleaned "/tmp/private-operation"))))))

(ert-deftest majutsu-interactive-file-operation/parses-explicit-actions ()
  "Whole-file actions must come only from structured jj metadata."
  (dolist
      (case
       '(("diff --git a/add.bin b/add.bin\nnew file mode 100644\nBinary files /dev/null and b/add.bin differ\n"
          "add.bin" (:status "added" :source "add.bin" :target "add.bin")
          (:action add :path "add.bin"))
         ("diff --git a/mod.bin b/mod.bin\nindex 123..456 100644\nBinary files a/mod.bin and b/mod.bin differ\n"
          "mod.bin" (:status "modified" :source "mod.bin" :target "mod.bin")
          (:action modify :path "mod.bin"))
         ("diff --git a/del.bin b/del.bin\ndeleted file mode 100644\nBinary files a/del.bin and /dev/null differ\n"
          "del.bin" (:status "removed" :source "del.bin" :target "del.bin")
          (:action delete :path "del.bin"))
         ("diff --git a/old.bin b/new.bin\nsimilarity index 100%\nrename from old.bin\nrename to new.bin\n"
          "new.bin" (:status "renamed" :source "old.bin" :target "new.bin")
          (:action rename :source "old.bin" :path "new.bin"))
         ("diff --git a/src.bin b/copy.bin\nsimilarity index 100%\ncopy from src.bin\ncopy to copy.bin\n"
          "copy.bin" (:status "copied" :source "src.bin" :target "copy.bin")
          (:action copy :source "src.bin" :path "copy.bin"))
         ("diff --git a/foo b/bar.bin b/foo b/bar.bin\nindex 123..456 100644\nBinary files a/foo b/bar.bin and b/foo b/bar.bin differ\n"
          "foo b/bar.bin"
          (:status "modified" :source "foo b/bar.bin"
           :target "foo b/bar.bin")
          (:action modify :path "foo b/bar.bin"))
         ("diff --git \"a/literal\\\"old.bin\" \"b/literal\\\"new.bin\"\nsimilarity index 100%\nrename from \"literal\\\"old.bin\"\nrename to \"literal\\\"new.bin\"\n"
          "literal\"new.bin"
          (:status "renamed" :source "literal\"old.bin"
           :target "literal\"new.bin")
          (:action rename :source "literal\"old.bin"
           :path "literal\"new.bin"))))
    (with-temp-buffer
      (pcase-let ((`(,diff ,file ,metadata ,expected) case))
        (let ((section (majutsu-interactive-test--insert-diff
                        diff file (list metadata))))
          (should section)
          (should (equal (majutsu-interactive--file-operation
                          (majutsu-interactive--file-change section))
                         expected)))))))

(ert-deftest majutsu-interactive-build-replay-plan/complement-uses-unselected-file-ops ()
  "Restore complement keeps selected whole-file ops by replaying the rest."
  (with-temp-buffer
    (let* ((diff (concat
                  "diff --git a/keep.bin b/keep.bin\nnew file mode 100644\n"
                  "Binary files /dev/null and b/keep.bin differ\n"
                  "diff --git a/drop.bin b/drop.bin\nnew file mode 100644\n"
                  "Binary files /dev/null and b/drop.bin differ\n"))
           (keep (majutsu-interactive-test--insert-diff
                  diff "keep.bin"
                  '((:status "added" :source "keep.bin" :target "keep.bin")
                    (:status "added" :source "drop.bin" :target "drop.bin"))))
           (drop (majutsu-interactive--file-section-for-file "drop.bin")))
      (should keep)
      (should drop)
      (majutsu-interactive--set-selection
       (majutsu-interactive--file-id (oref keep value)) :all)
      (should (equal (majutsu-interactive-build-replay-plan-if-selected
                      nil 'complement)
                     '(:base right :payload-root left
                       :patch nil
                       :file-ops ((:action add :path "drop.bin"))))))))

(ert-deftest majutsu-interactive-toggle-file/fails-closed-without-machine-metadata ()
  "Rendered Git headers alone must never authorize a whole-file operation."
  (with-temp-buffer
    (let* ((diff "diff --git a/bin.dat b/bin.dat\nnew file mode 100644\nBinary files /dev/null and b/bin.dat differ\n")
           (section (majutsu-interactive-test--insert-diff diff "bin.dat"))
           (transient-current-command 'majutsu-split))
      (goto-char (oref section start))
      (should-error (majutsu-interactive-toggle-file) :type 'user-error)
      (should-not (majutsu-interactive-has-selections-p)))))

(ert-deftest majutsu-interactive-toggle-file/selects-hunkless-file-for-supported-operations ()
  "Split, squash, and restore can select a binary file as one operation."
  (dolist (command '(majutsu-split majutsu-squash majutsu-restore))
    (with-temp-buffer
      (let* ((diff "diff --git a/bin.dat b/bin.dat\nnew file mode 100644\nBinary files /dev/null and b/bin.dat differ\n")
             (section (majutsu-interactive-test--insert-diff
                       diff "bin.dat"
                       '((:status "added" :source "bin.dat"
                          :target "bin.dat"))))
             (transient-current-command command))
        (goto-char (oref section start))
        (majutsu-interactive-toggle-file)
        (should (equal (majutsu-interactive-build-replay-plan-if-selected)
                       '(:base left :payload-root right
                         :patch nil
                         :file-ops ((:action add :path "bin.dat")))))))))

(ert-deftest majutsu-interactive-operation/mixed-does-not-include-unselected-hunks ()
  "A mixed text/binary operation must not carry an unselected text hunk."
  (with-temp-buffer
    (let* ((diff (concat
                  "diff --git a/text.txt b/text.txt\n--- a/text.txt\n+++ b/text.txt\n"
                  "@@ -1 +1 @@\n-old one\n+selected one\n"
                  "@@ -10 +10 @@\n-old two\n+unselected two\n"
                  "diff --git a/bin.dat b/bin.dat\nnew file mode 100644\n"
                  "Binary files /dev/null and b/bin.dat differ\n"))
           (text (majutsu-interactive-test--insert-diff
                  diff "text.txt"
                  '((:status "modified" :source "text.txt" :target "text.txt")
                    (:status "added" :source "bin.dat" :target "bin.dat"))))
           (binary (majutsu-interactive--file-section-for-file "bin.dat"))
           (hunk (car (majutsu-interactive--file-section-hunks text))))
      (majutsu-interactive--set-selection
       (majutsu-interactive--hunk-id hunk) :all)
      (majutsu-interactive--set-selection
       (majutsu-interactive--file-id (oref binary value)) :all)
      (let* ((operation (majutsu-interactive-build-replay-plan-if-selected))
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
                (:action add :path "line\nbreak.bin")
                (:action modify :path "modified.bin")
                (:action delete :path "deleted.bin")
                (:action delete :path "collision")
                (:action rename :source "old.bin" :path "renamed.bin")
                (:action rename :source "literal\"old.bin"
                 :path "literal\"new.bin")
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
                           ("literal\"old.bin" . "quoted-rename")
                           ("source.bin" . "copy-me")))
            (with-temp-file (expand-file-name (car entry) left)
              (insert (cdr entry))))
          (dolist (entry '(("text.txt" . "unselected-right\n")
                           ("added.bin" . "added")
                           ("line\nbreak.bin" . "newline")
                           ("modified.bin" . "new-mod")
                           ("renamed.bin" . "rename-me")
                           ("literal\"new.bin" . "quoted-rename")
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
          (let ((script (majutsu-interactive--write-applypatch-script
                         (list :base 'left :payload-root 'right
                               :patch nil :file-ops ops)
                         dir)))
            (should (zerop (call-process script nil nil nil
                                         left right patch-file))))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "text.txt" right))
                         "selected\n"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "added.bin" right)) "added"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "line\nbreak.bin" right)) "newline"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "modified.bin" right)) "new-mod"))
          (should-not (file-exists-p (expand-file-name "deleted.bin" right)))
          (should-not (file-exists-p (expand-file-name "collision" right)))
          (should-not (file-exists-p (expand-file-name "old.bin" right)))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "renamed.bin" right)) "rename-me"))
          (should-not (file-exists-p
                       (expand-file-name "literal\"old.bin" right)))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "literal\"new.bin" right))
                         "quoted-rename"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "source.bin" right)) "copy-me"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "copied.bin" right)) "copy-me"))
          (should-not (file-exists-p (expand-file-name "unselected.bin" right))))
      (delete-directory dir t))))

(ert-deftest majutsu-interactive--write-applypatch-script/complement-keeps-selected ()
  "A restore complement plan keeps the source tree and applies unselected ops."
  (let* ((dir (make-temp-file "majutsu-interactive-restore-" t))
         (left (expand-file-name "left" dir))
         (right (expand-file-name "right" dir))
         (patch-file (expand-file-name "patch.diff" dir))
         ;; Unselected whole-file ops only: keep selected modified.bin from right.
         (ops '((:action add :path "added.bin")
                (:action delete :path "removed.bin")
                (:action rename :source "old.bin" :path "renamed.bin")
                (:action copy :source "source.bin" :path "copied.bin"))))
    (unwind-protect
        (progn
          (make-directory left)
          (make-directory right)
          ;; Destination (tool left): has the unselected destination-side state.
          (dolist (entry '(("text.txt" . "dest\n")
                           ("added.bin" . "added")
                           ("modified.bin" . "dest-mod")
                           ("renamed.bin" . "rename-me")
                           ("source.bin" . "copy-me")
                           ("copied.bin" . "copy-me")))
            (with-temp-file (expand-file-name (car entry) left)
              (insert (cdr entry))))
          ;; Source (tool right): start here; selected modified.bin stays.
          (dolist (entry '(("text.txt" . "source\n")
                           ("modified.bin" . "source-mod")
                           ("removed.bin" . "remove-me")
                           ("old.bin" . "rename-me")
                           ("source.bin" . "copy-me")))
            (with-temp-file (expand-file-name (car entry) right)
              (insert (cdr entry))))
          ;; Complement text: apply the unselected text change from left.
          (with-temp-file patch-file
            (insert "diff --git a/text.txt b/text.txt\n--- a/text.txt\n+++ b/text.txt\n@@ -1 +1 @@\n-source\n+dest\n"))
          (let ((script (majutsu-interactive--write-applypatch-script
                         (list :base 'right :payload-root 'left
                               :patch nil :file-ops ops)
                         dir)))
            (should (zerop (call-process script nil nil nil
                                         left right patch-file))))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "text.txt" right))
                         "dest\n"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "added.bin" right))
                         "added"))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "modified.bin" right))
                         "source-mod"))
          (should-not (file-exists-p (expand-file-name "removed.bin" right)))
          (should-not (file-exists-p (expand-file-name "old.bin" right)))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "renamed.bin" right))
                         "rename-me"))
          (should (file-exists-p (expand-file-name "source.bin" right)))
          (should (equal (majutsu-interactive-test--file-contents
                          (expand-file-name "copied.bin" right))
                         "copy-me")))
      (delete-directory dir t))))

(ert-deftest majutsu-interactive/integration-non-split-binary-selections ()
  "Run real jj restore/squash with selected whole-file binary changes.
Covers restore modify/add/delete/rename and squash modify."
  (majutsu-jj-integration-with-sandbox sandbox
    ;; Restore: partial modify.  Complement keeps selected.bin from the source
    ;; tree and replays unselected kept.bin from dest.
    (let* ((repo (majutsu-jj-integration-init sandbox "restore-modify"))
           (selected (majutsu-jj-integration-file repo "selected.bin"))
           (kept (majutsu-jj-integration-file repo "kept.bin")))
      (majutsu-jj-integration-write-bytes selected (unibyte-string 0 1))
      (majutsu-jj-integration-write-bytes kept (unibyte-string 2 3))
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-bytes selected (unibyte-string 0 4))
      (majutsu-jj-integration-write-bytes kept (unibyte-string 2 5))
      (majutsu-jj-integration-run-replay
       repo "restore" '("--changes-in=@")
       '(:base right :payload-root left :patch nil
         :file-ops ((:action modify :path "kept.bin"))))
      (should (equal (majutsu-jj-integration-read-bytes selected)
                     (unibyte-string 0 1)))
      (should (equal (majutsu-jj-integration-read-bytes kept)
                     (unibyte-string 2 5))))
    ;; Restore: undo a selected add (file disappears), keep another add.
    (let* ((repo (majutsu-jj-integration-init sandbox "restore-add"))
           (selected (majutsu-jj-integration-file repo "selected.bin"))
           (kept (majutsu-jj-integration-file repo "kept.bin")))
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-bytes selected (unibyte-string 0 1))
      (majutsu-jj-integration-write-bytes kept (unibyte-string 2 3))
      (majutsu-jj-integration-run-replay
       repo "restore" '("--changes-in=@")
       '(:base right :payload-root left :patch nil
         :file-ops ((:action add :path "kept.bin"))))
      (should-not (file-exists-p selected))
      (should (equal (majutsu-jj-integration-read-bytes kept)
                     (unibyte-string 2 3))))
    ;; Restore: undo a selected delete (file returns), leave another deletion
    ;; unrestored.
    (let* ((repo (majutsu-jj-integration-init sandbox "restore-delete"))
           (selected (majutsu-jj-integration-file repo "selected.bin"))
           (kept-gone (majutsu-jj-integration-file repo "kept-gone.bin")))
      (majutsu-jj-integration-write-bytes selected (unibyte-string 0 1))
      (majutsu-jj-integration-write-bytes kept-gone (unibyte-string 2 3))
      (majutsu-jj-integration-commit repo "base")
      (delete-file selected)
      (delete-file kept-gone)
      (majutsu-jj-integration-run-replay
       repo "restore" '("--changes-in=@")
       '(:base right :payload-root left :patch nil
         :file-ops ((:action delete :path "kept-gone.bin"))))
      (should (equal (majutsu-jj-integration-read-bytes selected)
                     (unibyte-string 0 1)))
      (should-not (file-exists-p kept-gone)))
    ;; Restore: undo a selected rename.
    (let* ((repo (majutsu-jj-integration-init sandbox "restore-rename"))
           (old (majutsu-jj-integration-file repo "old.bin"))
           (new (majutsu-jj-integration-file repo "new.bin")))
      (majutsu-jj-integration-write-bytes old (unibyte-string 0 1 2 3))
      (majutsu-jj-integration-commit repo "base")
      (rename-file old new)
      ;; Selected rename: complement has no unselected whole-file ops, so the
      ;; source tree (old name) is kept as-is.
      (majutsu-jj-integration-run-replay
       repo "restore" '("--changes-in=@")
       '(:base right :payload-root left :patch nil :file-ops nil))
      (should (file-exists-p old))
      (should-not (file-exists-p new))
      (should (equal (majutsu-jj-integration-read-bytes old)
                     (unibyte-string 0 1 2 3))))
    ;; Squash: move only selected.bin into the parent.
    (let* ((repo (majutsu-jj-integration-init sandbox "squash-modify"))
           (selected (majutsu-jj-integration-file repo "selected.bin"))
           (kept (majutsu-jj-integration-file repo "kept.bin")))
      (majutsu-jj-integration-write-bytes selected (unibyte-string 0 1))
      (majutsu-jj-integration-write-bytes kept (unibyte-string 2 3))
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-bytes selected (unibyte-string 0 4))
      (majutsu-jj-integration-write-bytes kept (unibyte-string 2 5))
      (majutsu-jj-integration-run-replay
       repo "squash" '("--from=@" "--into=@-")
       '(:base left :payload-root right :patch nil
         :file-ops ((:action modify :path "selected.bin"))))
      (should (equal (majutsu-jj-integration-read-bytes selected)
                     (unibyte-string 0 4)))
      (should (equal (majutsu-jj-integration-read-bytes kept)
                     (unibyte-string 2 5)))
      (let ((diff (majutsu-jj-integration-output
                   repo "diff" "--git" "--from=@-" "--to=@")))
        (should-not (string-match-p "selected\\.bin" diff))
        (should (string-match-p "kept\\.bin" diff))))))

(ert-deftest majutsu-interactive/integration-restore-partial-new-file-text ()
  "Partial restore of a new text file must not reverse-apply the new-file patch.
Selecting only the first added line leaves the unselected line in place."
  (majutsu-jj-integration-with-repo repo
    (let ((added (majutsu-jj-integration-file repo "added.txt"))
          plan)
      (majutsu-jj-integration-write-text
       (majutsu-jj-integration-file repo "note.txt") "base\n")
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-text added "one\ntwo\n")
      (setq plan
            (majutsu-jj-integration-with-washed-diff
             repo
             (lambda ()
               (let* ((file (majutsu-interactive--file-section-for-file
                             "added.txt"))
                      (hunk (car (majutsu-interactive--file-section-hunks file)))
                      (lines (majutsu-interactive--hunk-lines hunk))
                      (one (seq-find (lambda (line)
                                       (equal (car line) "+one\n"))
                                     lines)))
                 (should file)
                 (should hunk)
                 (should one)
                 ;; Select only the "+one" line for restore.
                 (majutsu-interactive--set-selection
                  (majutsu-interactive--hunk-id hunk)
                  (list (cons (nth 2 one) (nth 3 one))))
                 (majutsu-interactive-build-replay-plan-if-selected
                  nil 'complement)))))
      (should plan)
      (should (eq (plist-get plan :base) 'right))
      (should (eq (plist-get plan :payload-root) 'left))
      (should (string-match-p "\\+two" (or (plist-get plan :patch) "")))
      (should-not (string-match-p "\\+one" (or (plist-get plan :patch) "")))
      (majutsu-jj-integration-run-replay
       repo "restore" '("--changes-in=@") plan)
      (should (equal (majutsu-interactive-test--file-contents added)
                     "two\n")))))

(ert-deftest majutsu-interactive/integration-restore-region-modified-text ()
  "Restore a selected replacement while retaining an adjacent replacement."
  (majutsu-jj-integration-with-repo repo
    (let ((file (majutsu-jj-integration-file repo "modified.txt"))
          plan)
      (majutsu-jj-integration-write-text file
                                         "old one\nunchanged\nold two\n")
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-text file
                                         "new one\nunchanged\nnew two\n")
      (setq plan
            (majutsu-interactive-test--replacement-plan
             repo "modified.txt" "old one" "new one" 'complement))
      (let ((patch (or (plist-get plan :patch) "")))
        (should (string-match-p "^-old two$" patch))
        (should (string-match-p "^+new two$" patch))
        (should (string-match-p "^ old one$" patch))
        (should-not (string-match-p "^ new one$" patch)))
      (majutsu-jj-integration-run-replay
       repo "restore" '("--from=@-" "--to=@") plan)
      (should (equal (majutsu-interactive-test--file-contents file)
                     "old one\nunchanged\nnew two\n")))))

(ert-deftest majutsu-interactive/integration-split-region-modified-text ()
  "Split a selected replacement while leaving an adjacent one in the child."
  (majutsu-jj-integration-with-repo repo
    (let ((file (majutsu-jj-integration-file repo "modified.txt"))
          plan)
      (majutsu-jj-integration-write-text file
                                         "old one\nunchanged\nold two\n")
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-text file
                                         "new one\nunchanged\nnew two\n")
      (setq plan
            (majutsu-interactive-test--replacement-plan
             repo "modified.txt" "old one" "new one"))
      (should (eq (plist-get plan :base) 'left))
      (let ((patch (or (plist-get plan :patch) "")))
        (should (string-match-p "^-old one$" patch))
        (should (string-match-p "^+new one$" patch))
        (should-not (string-match-p "^[+-].*two$" patch)))
      (majutsu-jj-integration-run-replay
       repo "split" '("--revision=@" "--message=selected") plan)
      (should (equal (majutsu-jj-integration-output
                      repo "file" "show" "-r" "@-" "modified.txt")
                     "new one\nunchanged\nold two\n"))
      (let ((diff (majutsu-jj-integration-output
                   repo "diff" "--git" "--from=@-" "--to=@")))
        (should-not (string-match-p "^[+-].*one$" diff))
        (should (string-match-p "^-old two$" diff))
        (should (string-match-p "^+new two$" diff))))))

(ert-deftest majutsu-interactive/integration-mixed-text-and-whole-file-plans ()
  "Build mixed plans from real diffs and run them through Restore and Split."
  (majutsu-jj-integration-with-sandbox sandbox
    ;; Restore one text replacement and one binary file while retaining every
    ;; unselected text and binary change from the destination tree.
    (let* ((repo (majutsu-jj-integration-init sandbox "mixed-restore"))
           (files (majutsu-interactive-test--prepare-mixed-changes repo))
           (text-file (plist-get files :text))
           (other-file (plist-get files :other))
           (mode-file (plist-get files :mode))
           (selected-bin (plist-get files :selected-bin))
           (remaining-bin (plist-get files :remaining-bin))
           (plan (majutsu-interactive-test--mixed-plan
                  repo 'majutsu-restore 'complement)))
      (should (eq (plist-get plan :base) 'right))
      (should (equal (plist-get plan :file-ops)
                     '((:action modify :path "remaining.bin"))))
      (let ((patch (or (plist-get plan :patch) "")))
        (should-not (string-match-p "^+new one$" patch))
        (should (string-match-p "^+new two$" patch))
        (should (string-match-p "^+other new$" patch)))
      (majutsu-jj-integration-run-replay
       repo "restore" '("--changes-in=@") plan)
      (should (equal (majutsu-interactive-test--file-contents text-file)
                     "old one\nunchanged\nnew two\n"))
      (should (equal (majutsu-interactive-test--file-contents other-file)
                     "other new\n"))
      (should (= (logand (file-modes mode-file) #o777) #o644))
      (should (equal (majutsu-jj-integration-read-bytes selected-bin)
                     (unibyte-string 0 1)))
      (should (equal (majutsu-jj-integration-read-bytes remaining-bin)
                     (unibyte-string 0 5))))
    ;; Split the same mixed selection into the parent and leave both kinds of
    ;; unselected changes in the child revision.
    (let* ((repo (majutsu-jj-integration-init sandbox "mixed-split"))
           (files (majutsu-interactive-test--prepare-mixed-changes repo))
           (text-file (plist-get files :text))
           (mode-file (plist-get files :mode))
           (plan (majutsu-interactive-test--mixed-plan
                  repo 'majutsu-split)))
      (should (eq (plist-get plan :base) 'left))
      (should (= (length (plist-get plan :file-ops)) 2))
      (should (member '(:action modify :path "executable.sh")
                      (plist-get plan :file-ops)))
      (should (member '(:action modify :path "selected.bin")
                      (plist-get plan :file-ops)))
      (majutsu-jj-integration-run-replay
       repo "split" '("--revision=@" "--message=selected") plan)
      (should (equal (majutsu-jj-integration-output
                      repo "file" "show" "-r" "@-" "text.txt")
                     "new one\nunchanged\nold two\n"))
      (should (= (logand (file-modes mode-file) #o777) #o755))
      (let ((selected-diff
             (majutsu-jj-integration-output repo "diff" "--summary"
                                            "-r" "@-"))
            (remaining-diff
             (majutsu-jj-integration-output repo "diff" "--summary"
                                            "--from=@-" "--to=@")))
        (should (string-match-p "selected\\.bin" selected-diff))
        (should (string-match-p "executable\\.sh" selected-diff))
        (should-not (string-match-p "remaining\\.bin" selected-diff))
        (should-not (string-match-p "other\\.txt" selected-diff))
        (should-not (string-match-p "selected\\.bin" remaining-diff))
        (should-not (string-match-p "executable\\.sh" remaining-diff))
        (should (string-match-p "remaining\\.bin" remaining-diff))
        (should (string-match-p "other\\.txt" remaining-diff))))))

(ert-deftest majutsu-interactive/integration-machine-paths-with-minimum-jj ()
  "Exercise sidecar binding against the jj binary named by MAJUTSU_TEST_JJ."
  (majutsu-jj-integration-with-repo repo
    (let* ((odd "foo b/bar.bin")
           (newline "line\nbreak.bin")
           (old "literal\"old.bin")
           (new "literal\"new.bin")
           (odd-file (majutsu-jj-integration-file repo odd))
           (newline-file (majutsu-jj-integration-file repo newline))
           (old-file (majutsu-jj-integration-file repo old))
           (new-file (majutsu-jj-integration-file repo new)))
      (make-directory (file-name-directory odd-file) t)
      (majutsu-jj-integration-write-bytes odd-file (unibyte-string 0 1 2))
      (majutsu-jj-integration-write-bytes newline-file (unibyte-string 0 1 2))
      (majutsu-jj-integration-write-text old-file "rename me")
      (majutsu-jj-integration-commit repo "base")
      (majutsu-jj-integration-write-bytes odd-file (unibyte-string 0 3 4))
      (majutsu-jj-integration-write-bytes newline-file (unibyte-string 0 3 4))
      (rename-file old-file new-file)
      ;; The rendered diff snapshots first; the sidecar intentionally reads
      ;; that snapshot without doing a second one.
      (majutsu-jj-integration-with-washed-diff
       repo
       (lambda ()
         (let ((odd-section (majutsu-interactive--file-section-for-file odd))
               (newline-section
                (majutsu-interactive--file-section-for-file newline))
               (rename-section
                (majutsu-interactive--file-section-for-file new)))
           (should odd-section)
           (should newline-section)
           (should rename-section)
           (should (equal (majutsu-interactive--file-operation
                           (majutsu-interactive--file-change odd-section))
                          `(:action modify :path ,odd)))
           (should (equal (majutsu-interactive--file-operation
                           (majutsu-interactive--file-change newline-section))
                          `(:action modify :path ,newline)))
           (should (equal (majutsu-interactive--file-operation
                           (majutsu-interactive--file-change rename-section))
                          `(:action rename :source ,old :path ,new)))))))))

(provide 'majutsu-interactive-test)
;;; majutsu-interactive-test.el ends here
