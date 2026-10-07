;;; majutsu-interactive-test.el --- Tests for majutsu-interactive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for helper logic in `majutsu-interactive.el'.

;;; Code:

(require 'ert)
(require 'majutsu-interactive)

(defun majutsu-interactive-test--overlay-face-p (face)
  "Return non-nil when a selection overlay uses FACE."
  (seq-some (lambda (overlay)
              (eq (overlay-get overlay 'face) face))
            majutsu-interactive--overlays))

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

(defun majutsu-interactive-test--select-replacement
    (file removed added command)
  "Select REMOVED/ADDED in FILE for transient COMMAND."
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
    (let ((transient-current-command command)
          (transient-mark-mode t))
      (goto-char (nth 2 removed-line))
      (set-mark (nth 3 added-line))
      (setq mark-active t)
      (majutsu-interactive-toggle-region))))

(defun majutsu-interactive-test--toggle-whole-file (file command)
  "Toggle all changes to FILE for transient COMMAND in the current diff."
  (let ((section (majutsu-interactive--file-section-for-file file))
        (transient-current-command command))
    (should section)
    (goto-char (oref section start))
    (majutsu-interactive-toggle-file)))

(defun majutsu-interactive-test--replacement-plan
    (repo file removed added &optional mode)
  "Select the REMOVED/ADDED replacement in FILE's diff and build MODE's plan."
  (majutsu-jj-integration-with-washed-diff
   repo
   (lambda ()
     (majutsu-interactive-test--select-replacement
      file removed added
      (if (eq mode 'complement) 'majutsu-restore 'majutsu-split))
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
      "text.txt" "old one" "new one" command)
     (majutsu-interactive-test--toggle-whole-file "selected.bin" command)
     (majutsu-interactive-test--toggle-whole-file "executable.sh" command)
     (majutsu-interactive-build-replay-plan-if-selected nil mode))))

(ert-deftest majutsu-interactive-invalidate/clears-ranges-and-overlays ()
  "A diff rebuild must not leave position-based selections behind."
  (with-temp-buffer
    (insert "old diff\n")
    (let ((overlay (make-overlay (point-min) (point-max))))
      (setq-local majutsu-interactive--selections
                  (let ((table (make-hash-table :test 'equal)))
                    (puthash 'hunk :all table)
                    table))
      (setq-local majutsu-interactive--overlays (list overlay))
      (setq-local majutsu-interactive--selection-operation 'majutsu-split)
      (majutsu-interactive-invalidate)
      (should-not majutsu-interactive--selections)
      (should-not majutsu-interactive--selection-operation)
      (should-not majutsu-interactive--overlays)
      (should-not (overlay-buffer overlay)))))

(ert-deftest majutsu-interactive-selection-context/rejects-a-different-command ()
  "A patch selection cannot silently change Split/Squash/Restore meaning."
  (with-temp-buffer
    (let ((table (make-hash-table :test 'equal)))
      (puthash 'hunk :all table)
      (setq-local majutsu-interactive--selections table)
      (setq-local majutsu-interactive--selection-operation 'majutsu-split)
      (should-error
       (majutsu-interactive-build-patch-if-selected
        nil nil nil 'majutsu-restore)
       :type 'user-error))))

(ert-deftest majutsu-interactive-complete-patch-operation/retains-cancelled-selection ()
  "Only a changed or unknown repository operation invalidates a patch selection."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let ((table (make-hash-table :test 'equal))
          ids refreshed)
      (puthash 'hunk :all table)
      (setq-local majutsu-interactive--selections table)
      (cl-letf (((symbol-function 'majutsu-jj-operation-id)
                 (lambda (&rest _)
                   (pop ids)))
                ((symbol-function 'majutsu--toplevel-safe)
                 (lambda (&optional _directory) default-directory))
                ((symbol-function 'majutsu-mode-get-buffers)
                 (lambda (_root) (list (current-buffer))))
                ((symbol-function 'majutsu-refresh)
                 (lambda () (setq refreshed (1+ (or refreshed 0)))))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (setq ids '("before"))
        (majutsu-interactive--complete-patch-operation
         (current-buffer) default-directory "before" 0)
        (should (eq (gethash 'hunk table) :all))
        (should-not refreshed)
        (setq ids '("after"))
        (majutsu-interactive--complete-patch-operation
         (current-buffer) default-directory "before" 0)
        (should-not majutsu-interactive--selections)
        (should (= refreshed 1))))))



(ert-deftest majutsu-interactive-run-replay-plan/inserts-tool-before-filesets ()
  "Replay plan should keep jj options before filesets."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch &optional _directory) "/tmp/patch.diff"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch-file _plan _directory)
                 '("--config" "merge-tools.majutsu-applypatch.program=/tmp/applypatch")))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "op-before"))
              ((symbol-function 'majutsu-process-completion-owned-p)
               (lambda (&rest _) t))
              ((symbol-function 'majutsu-start-jj-with-editor)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-interactive-run-replay-plan
       "restore" '("--from=A" "--to=B") '("src/a.el")
       '(:base left :payload-root right :patch "PATCH" :file-ops nil))
      (should (equal (car called)
                     '("restore" "--from=A" "--to=B"
                       "-i" "--tool" "majutsu-applypatch"
                       "--config" "merge-tools.majutsu-applypatch.program=/tmp/applypatch"
                       "--" "src/a.el")))
      (should-not (nth 1 called))
      (should (functionp (nth 2 called)))
      (should (eq (nth 3 called) t)))))

(ert-deftest majutsu-interactive-run-replay-plan/normalizes-structured-filesets ()
  "Replay plan should also normalize structured filesets."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive--write-patch)
               (lambda (_patch &optional _directory) "/tmp/patch.diff"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (_patch-file _plan _directory)
                 '("--config" "tool=config")))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "op-before"))
              ((symbol-function 'majutsu-process-completion-owned-p)
               (lambda (&rest _) t))
              ((symbol-function 'majutsu-start-jj-with-editor)
               (lambda (&rest args)
                 (setq called args))))
      (majutsu-interactive-run-replay-plan
       "split" '("--revision=@") '("src/a.el")
       '(:base left :payload-root right :patch "PATCH" :file-ops nil))
      (should (equal (car called)
                     '("split" "--revision=@"
                       "-i" "--tool" "majutsu-applypatch"
                       "--config" "tool=config"
                       "--" "src/a.el")))
      (should (functionp (nth 2 called)))
      (should (eq (nth 3 called) t)))))

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
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "op-before"))
              ((symbol-function 'majutsu-process-completion-owned-p)
               (lambda (&rest _) t))
              ((symbol-function 'majutsu-start-jj-with-editor)
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

(defun majutsu-interactive-test--insert-stat-tree (&optional without-metadata)
  "Insert a diffstat with text, rename, and unrelated file changes."
  (majutsu-diff-mode)
  (let* ((inhibit-read-only t)
         (magit-section-inhibit-markers t)
         (metadata
          (unless without-metadata
            '((:status "modified" :source "src/a.rs" :target "src/a.rs")
              (:status "renamed" :source "old/source.bin" :target "src/new.bin")
              (:status "modified" :source "outside.txt" :target "outside.txt"))))
         (majutsu-diff--active-file-metadata metadata)
         (majutsu-diff--file-metadata-queue (copy-sequence metadata)))
    (magit-insert-section (diffbuf)
      (magit-insert-section (diff-root)
        (insert
         "src/a.rs | 4 ++--\n"
         "{old/source.bin => src/new.bin} | 0\n"
         "outside.txt | 2 +-\n"
         "3 files changed, 3 insertions(+), 3 deletions(-)\n"
         "diff --git a/src/a.rs b/src/a.rs\n"
         "index 1111111..2222222 100644\n--- a/src/a.rs\n+++ b/src/a.rs\n"
         "@@ -1 +1 @@\n-old one\n+new one\n"
         "@@ -10 +10 @@\n-old two\n+new two\n"
         "diff --git a/old/source.bin b/src/new.bin\n"
         "rename from old/source.bin\nrename to src/new.bin\n"
         "diff --git a/outside.txt b/outside.txt\n"
         "index 1111111..2222222 100644\n--- a/outside.txt\n+++ b/outside.txt\n"
         "@@ -1 +1 @@\n-old\n+new\n")
        (majutsu-diff-wash-diffs '("--stat"))
        (majutsu-diff--attach-file-metadata metadata)))
    (majutsu-section-show magit-root-section)))

(ert-deftest majutsu-interactive-directory-selection/mirrors-partial-and-full-state ()
  "Directory selection includes hidden files and renames, but not siblings."
  (with-temp-buffer
    (majutsu-interactive-test--insert-stat-tree)
    (let* ((transient-current-command 'majutsu-split)
           (stat (majutsu-diff--stat-file-section "src/a.rs"))
           (directory (oref stat parent))
           (rename (majutsu-diff--stat-file-section "src/new.bin"))
           (outside (majutsu-diff--stat-file-section "outside.txt"))
           (file (majutsu-interactive--file-section-for-file "src/a.rs"))
           (hunks (majutsu-interactive--file-section-hunks file)))
      (cl-labels ((face-p
                   (section face)
                   (seq-some (lambda (ov) (eq (overlay-get ov 'face) face))
                             (overlays-at (oref section start)))))
        (should (oref rename hidden))
        (goto-char (oref (car hunks) start))
        (majutsu-interactive-toggle-hunk)
        (should (face-p stat 'majutsu-interactive-partially-selected-file))
        (should (face-p directory 'majutsu-interactive-partially-selected-file))
        (should-not (face-p rename 'majutsu-interactive-selected-file))
        (goto-char (oref directory start))
        (majutsu-section-hide directory)
        (majutsu-interactive-toggle-file)
        (should (oref directory hidden))
        (should (face-p directory 'majutsu-interactive-selected-file))
        (should (face-p stat 'majutsu-interactive-selected-file))
        (should (face-p rename 'majutsu-interactive-selected-file))
        (should-not (face-p outside 'majutsu-interactive-selected-file))
        (let ((plan (majutsu-interactive-build-replay-plan-if-selected)))
          (should (equal (plist-get plan :file-ops)
                         '((:action rename :source "old/source.bin"
                            :path "src/new.bin"))))
          (should (string-match-p "new one" (plist-get plan :patch)))
          (should (string-match-p "new two" (plist-get plan :patch)))
          (should-not (string-match-p "outside.txt" (plist-get plan :patch))))
        (majutsu-section-show directory)
        (should (oref rename hidden))
        (majutsu-section-show rename)
        (should-not
         (seq-some (lambda (ov)
                     (eq (overlay-get ov 'face)
                         'majutsu-interactive-selected-file))
                   (overlays-at (oref rename content))))
        (goto-char (oref rename content))
        (majutsu-interactive-toggle-file)
        (should-not (face-p rename 'majutsu-interactive-selected-file))
        (should (face-p directory 'majutsu-interactive-partially-selected-file))
        (goto-char (oref directory start))
        (majutsu-interactive-toggle-file)
        (majutsu-interactive-toggle-file)
        (should-not (majutsu-interactive-has-selections-p))
        (should-not majutsu-interactive--overlays)))))

(ert-deftest majutsu-interactive-directory-selection/validates-before-selecting ()
  "An unverifiable rename must not leave other files in its directory selected."
  (with-temp-buffer
    (majutsu-interactive-test--insert-stat-tree t)
    (let ((transient-current-command 'majutsu-split)
          (directory (oref (majutsu-diff--stat-file-section "src/a.rs") parent)))
      (goto-char (oref directory start))
      (should-error (majutsu-interactive-toggle-file) :type 'user-error)
      (should-not (majutsu-interactive-has-selections-p))
      (should-not majutsu-interactive--overlays))))

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
  "Patch-selection commands can select a binary file as one operation."
  (dolist (command '(majutsu-split majutsu-squash majutsu-absorb majutsu-restore))
    (with-temp-buffer
      (let* ((diff "diff --git a/bin.dat b/bin.dat\nnew file mode 100644\nBinary files /dev/null and b/bin.dat differ\n")
             (section (majutsu-interactive-test--insert-diff
                       diff "bin.dat"
                       '((:status "added" :source "bin.dat"
                          :target "bin.dat"))))
             (transient-current-command command))
        (goto-char (oref section start))
        (majutsu-interactive-toggle-file)
        (should (eq majutsu-interactive--selection-operation command))
        (should (equal (majutsu-interactive-build-replay-plan-if-selected)
                       '(:base left :payload-root right
                         :patch nil
                         :file-ops ((:action add :path "bin.dat")))))))))

(ert-deftest majutsu-interactive-mode-change/selects-independently-from-text ()
  "Mode and content changes should be independently selectable patch units."
  (with-temp-buffer
    (let* ((diff (concat
                  "diff --git a/script b/script\n"
                  "old mode 100644\n"
                  "new mode 100755\n"
                  "index 1111111..2222222\n"
                  "--- a/script\n"
                  "+++ b/script\n"
                  "@@ -1 +1 @@\n"
                  "-old\n"
                  "+new\n"))
           (file (majutsu-interactive-test--insert-diff diff "script"))
           (mode-change (seq-find
                         (lambda (section)
                           (magit-section-match 'jj-mode-change section))
                         (oref file children)))
           (text-hunk (seq-find
                       (lambda (section)
                         (magit-section-match 'jj-hunk section))
                       (oref file children)))
           (transient-current-command 'majutsu-split))
      (goto-char (oref mode-change start))
      (majutsu-interactive-toggle-hunk)
      (let ((patch (majutsu-interactive--build-patch)))
        (should (string-match-p "old mode 100644\nnew mode 100755" patch))
        (should-not (string-match-p "^@@" patch)))
      (majutsu-interactive-clear)
      (goto-char (oref text-hunk start))
      (majutsu-interactive-toggle-hunk)
      (let ((patch (majutsu-interactive--build-patch)))
        (should-not (string-match-p "^old mode " patch))
        (should (string-match-p "^@@ " patch)))
      (majutsu-interactive-clear)
      (goto-char (oref file start))
      (majutsu-interactive-toggle-file)
      (let ((patch (majutsu-interactive--build-patch)))
        (should (string-match-p "old mode 100644\nnew mode 100755" patch))
        (should (string-match-p "^@@ " patch))))))

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

(ert-deftest majutsu-interactive/integration-binary-and-mode-selections ()
  "Replay binary bytes and chmod independently, together, and by directory."
  (majutsu-jj-integration-with-sandbox sandbox
    (dolist (command '(majutsu-restore majutsu-split majutsu-squash))
      (dolist (selection '(mode content file directory neither))
        (dolist (old-mode '(#o644 #o755))
          (let* ((repo (majutsu-jj-integration-init
                        sandbox (format "%s-%s-%o" command selection old-mode)))
                 (path "dir/change.bin")
                 (file (majutsu-jj-integration-file repo path))
                 (other (majutsu-jj-integration-file repo "outside.txt"))
                 (old-bytes (unibyte-string 0 1 2 3))
                 (new-bytes (unibyte-string 0 4 5 6))
                 (new-mode (if (= old-mode #o644) #o755 #o644))
                 (restore (eq command 'majutsu-restore))
                 (content-selected (memq selection '(content file directory)))
                 (mode-selected (memq selection '(mode file directory)))
                 plan)
            (make-directory (file-name-directory file) t)
            (majutsu-jj-integration-write-bytes file old-bytes)
            (set-file-modes file old-mode)
            (majutsu-jj-integration-write-text other "old\n")
            (majutsu-jj-integration-commit repo "base")
            (majutsu-jj-integration-write-bytes file new-bytes)
            (set-file-modes file new-mode)
            ;; Keep the child nonempty even when moving this entire file.
            (majutsu-jj-integration-write-text other "new\n")
            (setq plan
                  (majutsu-jj-integration-with-washed-diff
                   repo
                   (lambda ()
                     (when (eq selection 'directory)
                       ;; Render the real diffstat too, so F is exercised on
                       ;; its directory rather than on the patch file.
                       (let ((inhibit-read-only t)
                             (magit-section-inhibit-markers t)
                             (metadata (majutsu-diff--query-file-metadata nil nil))
                             (output (majutsu-jj-buffer-string
                                      "diff" "--git" "--stat")))
                         (erase-buffer)
                         (let ((majutsu-diff--active-file-metadata metadata)
                               (majutsu-diff--file-metadata-queue
                                (copy-sequence metadata)))
                           (magit-insert-section (diffbuf)
                             (magit-insert-section (diff-root)
                               (insert output)
                               (majutsu-diff-wash-diffs '("--git" "--stat")))))
                         (majutsu-diff--attach-file-metadata metadata)))
                     (let* ((transient-current-command command)
                            (section (majutsu-interactive--file-section-for-file
                                      path))
                            (mode (seq-find
                                   (lambda (child)
                                     (magit-section-match 'jj-mode-change child))
                                   (oref section children))))
                       (should mode)
                       (pcase selection
                         ('directory
                          (goto-char
                           (oref (oref (majutsu-diff--stat-file-section path)
                                       parent)
                                 start))
                          (majutsu-interactive-toggle-file))
                         ((or 'file 'content)
                          (majutsu-interactive-test--toggle-whole-file path command)
                          (when (eq selection 'content)
                            (goto-char (oref mode start))
                            (majutsu-interactive-toggle-hunk)))
                         ('neither
                          (majutsu-interactive-test--toggle-whole-file
                           "outside.txt" command))
                         ('mode
                          (goto-char (oref mode start))
                          (majutsu-interactive-toggle-hunk)))
                       (should (eq (majutsu-interactive--file-selection-state section)
                                   (pcase selection
                                     ((or 'file 'directory) 'all)
                                     ('neither nil)
                                     (_ 'partial))))
                       (majutsu-interactive-build-replay-plan-if-selected
                        nil (if restore 'complement 'selected))))))
            (majutsu-jj-integration-run-replay
             repo (pcase command
                    ('majutsu-restore "restore")
                    ('majutsu-split "split")
                    ('majutsu-squash "squash"))
             (pcase command
               ('majutsu-restore '("--changes-in=@"))
               ('majutsu-split '("--revision=@" "--message=selected"))
               ('majutsu-squash '("--from=@" "--into=@-")))
             plan)
            (should (equal (majutsu-jj-integration-read-bytes file)
                           (if (and restore content-selected) old-bytes new-bytes)))
            (should (= (logand (file-modes file) #o777)
                       (if (and restore mode-selected) old-mode new-mode)))
            (should (equal (majutsu-interactive-test--file-contents other)
                           (if (and restore (eq selection 'neither))
                               "old\n" "new\n")))
            (unless restore
              ;; Materialize the revision receiving the selection to inspect
              ;; its actual bytes and executable bit, not generated patches.
              (majutsu-jj-integration-call repo "new" "@-")
              (should (equal (majutsu-jj-integration-read-bytes file)
                             (if content-selected new-bytes old-bytes)))
              (should (= (logand (file-modes file) #o777)
                         (if mode-selected new-mode old-mode)))
              (should (equal (majutsu-interactive-test--file-contents other)
                             (if (eq selection 'neither)
                                 "new\n" "old\n"))))))))))

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
      (should (equal (plist-get plan :file-ops)
                     '((:action modify :path "selected.bin"))))
      (should (string-match-p
               "old mode 100644\nnew mode 100755"
               (or (plist-get plan :patch) "")))
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

(ert-deftest majutsu-interactive-complete-operation/quit-invalidates-conservatively ()
  "A quit in the after-operation probe must invalidate before propagating."
  (with-temp-buffer
    (let ((table (make-hash-table :test 'equal)) scheduled)
      (puthash 'hunk :all table)
      (setq-local majutsu-interactive--selections table)
      (cl-letf (((symbol-function 'majutsu-jj-operation-id)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'majutsu-interactive-invalidate-repository)
                 (lambda (&rest _) (majutsu-interactive-invalidate t)))
                ((symbol-function 'run-at-time)
                 (lambda (&rest args) (setq scheduled args))))
        (should
         (eq (condition-case condition
                 (progn
                   (majutsu-interactive-complete-repository-operation "/repo/" (current-buffer) "before")
                   nil)
               (quit (car condition)))
             'quit))
        (should-not majutsu-interactive--selections)
        (should scheduled)))))
(ert-deftest majutsu-interactive-complete-operation/retains-cancelled-selection ()
  "Only a changed or unknown repository operation invalidates a patch selection."
  (with-temp-buffer
    (let ((table (make-hash-table :test 'equal))
          ids refreshed)
      (puthash 'hunk :all table)
      (setq-local majutsu-interactive--selections table)
      (cl-letf (((symbol-function 'majutsu-jj-operation-id)
                 (lambda (&rest _)
                   (pop ids)))
                ((symbol-function 'majutsu-interactive-invalidate-repository)
                 (lambda (&rest _) (majutsu-interactive-invalidate t)))
                ((symbol-function 'majutsu-interactive--refresh-operation-origin)
                 (lambda (&rest _)
                   (setq refreshed (1+ (or refreshed 0)))))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (setq ids '("before"))
        (majutsu-interactive-complete-repository-operation default-directory (current-buffer) "before")
        (should (eq (gethash 'hunk table) :all))
        (should-not refreshed)
        (setq ids '("after"))
        (majutsu-interactive-complete-repository-operation default-directory (current-buffer) "before")
        (should-not majutsu-interactive--selections)
        (should (= refreshed 1))))))
(ert-deftest majutsu-interactive-invalidate-repository/clears-all-diff-buffers ()
  "A repository mutation invalidates selections outside the origin buffer."
  (let ((one (generate-new-buffer " *majutsu-selection-one*"))
        (two (generate-new-buffer " *majutsu-selection-two*"))
        (other (generate-new-buffer " *majutsu-selection-other*")))
    (unwind-protect
        (progn
          (dolist (entry `((,one . "/repo/") (,two . "/repo/")
                           (,other . "/other/")))
            (with-current-buffer (car entry)
              (majutsu-diff-mode)
              (setq-local majutsu--default-directory (cdr entry))
              (setq-local majutsu-interactive--selections
                          (let ((table (make-hash-table :test 'equal)))
                            (puthash 'hunk :all table)
                            table))))
          (majutsu-interactive-invalidate-repository "/repo/")
          (should-not (buffer-local-value
                       'majutsu-interactive--selections one))
          (should-not (buffer-local-value
                       'majutsu-interactive--selections two))
          (should (buffer-local-value
                   'majutsu-interactive--selections other)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list one two other)))))
(ert-deftest majutsu-interactive-render-context/precedes-refresh-hooks ()
  "A refresh hook operation must make the just-rendered context stale."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu--default-directory "/repo/")
    (let ((operation "rendered")
          snapshots
          (transient-current-command 'majutsu-split))
      (setq-local majutsu-refresh-buffer-hook
                  (list (lambda () (setq operation "after-hook"))))
      (cl-letf (((symbol-function 'majutsu--refresh-buffer-function)
                 (lambda ()
                   (lambda ()
                     (let ((inhibit-read-only t))
                       (insert "rendered diff\n")))))
                ((symbol-function 'majutsu-interactive--repository-root)
                 (lambda () "/repo/"))
                ((symbol-function 'majutsu-jj-operation-id)
                 (lambda (_root &optional snapshot)
                   (push snapshot snapshots)
                   operation))
                ((symbol-function 'magit-section-update-highlight) #'ignore))
        (majutsu-refresh-buffer-internal)
        (should
         (equal (majutsu-interactive-context-operation-id
                 majutsu-interactive--render-context)
                "rendered"))
        (should (equal operation "after-hook"))
        (should-error (majutsu-interactive--ensure-selection-context)
                      :type 'user-error)
        (should (equal (nreverse snapshots) '(nil t)))
        (should (eq majutsu-interactive--render-context 'stale))))))

(ert-deftest majutsu-interactive-selection-context/stale-diff-requires-refresh ()
  "Clearing stale selections must not let old text acquire a fresh context."
  (dolist (invalidate '(repository selection))
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu--default-directory "/repo/")
      (let ((operation "before")
            (transient-current-command 'majutsu-split))
        (cl-letf (((symbol-function 'majutsu-interactive--repository-root)
                   (lambda () "/repo/"))
                  ((symbol-function 'majutsu-jj-operation-id)
                   (lambda (&rest _) operation)))
          (majutsu-interactive-record-render-context)
          (setq operation "after")
          (pcase invalidate
            ('repository (majutsu-interactive-invalidate-repository "/repo/"))
            ('selection
             (should-error (majutsu-interactive--ensure-selection-context)
                           :type 'user-error)))
          ;; Clearing selections or retrying must not bypass the stale state.
          (majutsu-interactive-invalidate)
          (dotimes (_ 2)
            (should-error (majutsu-interactive--ensure-selection-context)
                          :type 'user-error)
            (should-not majutsu-interactive--selection-context))
          ;; The refresh lifecycle records a new context after rendering.
          (majutsu-interactive-record-render-context)
          (majutsu-interactive--ensure-selection-context)
          (should (equal (majutsu-interactive-context-operation-id
                          majutsu-interactive--selection-context)
                         "after")))))))
(ert-deftest majutsu-interactive-selection-context/rejects-operation-before-apply ()
  "An external jj operation after selection must make the patch unusable."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu--default-directory "/repo/")
    (let ((table (make-hash-table :test 'equal)))
      (puthash 'hunk :all table)
      (setq-local majutsu-interactive--selections table)
      (setq-local majutsu-interactive--selection-operation 'majutsu-split)
      (setq-local majutsu-interactive--selection-context
                  (majutsu-interactive-context-create
                   :operation 'majutsu-split :root "/repo/"
                   :operation-id "before"
                   :buffer-tick (buffer-chars-modified-tick)
                   :diff-range nil :diff-filesets nil))
      (cl-letf (((symbol-function 'majutsu-interactive--repository-root)
                 (lambda () "/repo/"))
                ((symbol-function 'majutsu-jj-operation-id)
                 (lambda (&rest _) "after")))
        (should-error
         (majutsu-interactive-build-patch-if-selected nil nil nil 'majutsu-split)
         :type 'user-error)
        (should-not majutsu-interactive--selections)
        (should-not majutsu-interactive--selection-context)))))
(ert-deftest majutsu-interactive-selection-context/rejects-post-render-operation ()
  "An external jj operation after render must block the first selection."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu--default-directory "/repo/")
    (setq-local majutsu-interactive--render-context
                (majutsu-interactive-context-create
                 :root "/repo/" :operation-id "before"
                 :buffer-tick (buffer-chars-modified-tick)
                 :diff-range nil :diff-filesets nil))
    (let ((transient-current-command 'majutsu-split))
      (cl-letf (((symbol-function 'majutsu-interactive--repository-root)
                 (lambda () "/repo/"))
                ((symbol-function 'majutsu-jj-operation-id)
                 (lambda (&rest _) "after")))
        (should-error (majutsu-interactive--ensure-selection-context)
                      :type 'user-error)
        (should (eq majutsu-interactive--render-context 'stale))
        (should-not majutsu-interactive--selection-context)))))
(ert-deftest majutsu-interactive-toggle-hunk/applies-selected-hunk-face ()
  "A whole-hunk selection is visibly rendered with its documented face."
  (with-temp-buffer
    (let* ((diff (concat "diff --git a/a.txt b/a.txt\n"
                         "index 1111111..2222222 100644\n"
                         "--- a/a.txt\n+++ b/a.txt\n"
                         "@@ -1 +1,2 @@\n base\n+added\n"))
           (file (majutsu-interactive-test--insert-diff diff "a.txt"))
           (hunk (car (majutsu-interactive--file-section-hunks file)))
           (transient-current-command 'majutsu-split))
      (goto-char (oref hunk content))
      (cl-letf (((symbol-function 'majutsu-interactive--repository-root)
                 #'ignore))
        (majutsu-interactive-toggle-hunk))
      (should
       (majutsu-interactive-test--overlay-face-p
        'majutsu-interactive-selected-hunk)))))
(ert-deftest majutsu-interactive-toggle-region/applies-selected-region-face ()
  "A sub-hunk region is visibly rendered with its documented face."
  (with-temp-buffer
    (let* ((diff (concat "diff --git a/a.txt b/a.txt\n"
                         "index 1111111..2222222 100644\n"
                         "--- a/a.txt\n+++ b/a.txt\n"
                         "@@ -1 +1,2 @@\n base\n+added\n"))
           (file (majutsu-interactive-test--insert-diff diff "a.txt"))
           (hunk (car (majutsu-interactive--file-section-hunks file)))
           (beg (save-excursion
                  (goto-char (oref hunk content))
                  (forward-line 1)
                  (point)))
           (end (save-excursion
                  (goto-char beg)
                  (forward-line 1)
                  (point)))
           (transient-current-command 'majutsu-restore)
           (transient-mark-mode t))
      (goto-char beg)
      (set-mark end)
      (setq mark-active t)
      (cl-letf (((symbol-function 'majutsu-interactive--repository-root)
                 #'ignore))
        (majutsu-interactive-toggle-region))
      (should
       (majutsu-interactive-test--overlay-face-p
        'majutsu-interactive-selected-region)))))

(defun majutsu-interactive-test--hunk-patch (diff select &optional invert)
  "Wash DIFF, then build a patch from its first hunk.
SELECT is `:all' or a list of hunk body line texts to select."
  (let* ((file (majutsu-interactive-test--insert-diff diff))
         (hunk (car (majutsu-interactive--file-section-hunks file)))
         (spec (if (eq select :all)
                   :all
                 (mapcar (lambda (wanted)
                           (let ((line (seq-find
                                        (lambda (line) (equal (car line) wanted))
                                        (majutsu-interactive--hunk-lines hunk))))
                             (should line)
                             (cons (nth 2 line) (nth 3 line))))
                         select))))
    (should hunk)
    (majutsu-interactive--build-hunk-patch hunk spec invert)))

(defconst majutsu-interactive-test--no-newline-diff
  (concat "diff --git a/t.txt b/t.txt\n"
          "index 1111111..2222222 100644\n"
          "--- a/t.txt\n+++ b/t.txt\n"
          "@@ -1 +1 @@\n"
          "-old\n"
          "\\ No newline at end of file\n"
          "+new\n"
          "\\ No newline at end of file\n")
  "Replacement of a final line that lacks a trailing newline on both sides.")

(ert-deftest majutsu-interactive-build-hunk-patch/readds-no-newline-line-before-kept-addition ()
  "Keeping only the addition must not glue it onto the unremoved final line."
  (with-temp-buffer
    (should (equal (majutsu-interactive-test--hunk-patch
                    majutsu-interactive-test--no-newline-diff
                    '("+new\n"))
                   (concat "@@ -1 +1,2 @@\n"
                           "-old\n"
                           "\\ No newline at end of file\n"
                           "+old\n"
                           "+new\n"
                           "\\ No newline at end of file\n")))))

(ert-deftest majutsu-interactive-build-hunk-patch/keeps-marker-for-selected-removal ()
  "Selecting only the removal keeps the old side's no-newline marker."
  (with-temp-buffer
    (should (equal (majutsu-interactive-test--hunk-patch
                    majutsu-interactive-test--no-newline-diff
                    '("-old\n"))
                   (concat "@@ -1 +1,0 @@\n"
                           "-old\n"
                           "\\ No newline at end of file\n")))))

(ert-deftest majutsu-interactive-build-hunk-patch/inverted-selection-readds-no-newline-line ()
  "The restore complement of a removal-only selection re-adds the kept line."
  (with-temp-buffer
    (should (equal (majutsu-interactive-test--hunk-patch
                    majutsu-interactive-test--no-newline-diff
                    '("-old\n")
                    'invert)
                   (concat "@@ -1 +1,2 @@\n"
                           "-old\n"
                           "\\ No newline at end of file\n"
                           "+old\n"
                           "+new\n"
                           "\\ No newline at end of file\n")))))

(ert-deftest majutsu-interactive-build-hunk-patch/full-selection-is-unchanged ()
  "A whole-hunk selection reproduces the hunk verbatim."
  (with-temp-buffer
    (should (equal (majutsu-interactive-test--hunk-patch
                    majutsu-interactive-test--no-newline-diff
                    :all)
                   (concat "@@ -1 +1 @@\n"
                           "-old\n"
                           "\\ No newline at end of file\n"
                           "+new\n"
                           "\\ No newline at end of file\n")))))

(ert-deftest majutsu-interactive-build-hunk-patch/keeps-marker-on-context-line ()
  "A no-newline marker on a trailing context line survives partial selection."
  (with-temp-buffer
    (let ((diff (concat "diff --git a/t.txt b/t.txt\n"
                        "index 1111111..2222222 100644\n"
                        "--- a/t.txt\n+++ b/t.txt\n"
                        "@@ -1,2 +1,2 @@\n"
                        "-a\n"
                        "+b\n"
                        " last\n"
                        "\\ No newline at end of file\n")))
      (should (equal (majutsu-interactive-test--hunk-patch diff '("+b\n"))
                     (concat "@@ -1,2 +1,3 @@\n"
                             " a\n"
                             "+b\n"
                             " last\n"
                             "\\ No newline at end of file\n"))))))

(ert-deftest majutsu-interactive-build-hunk-patch/keeps-context-conversion-without-later-addition ()
  "An unselected final removal stays a context line when nothing follows it."
  (with-temp-buffer
    (let ((diff (concat "diff --git a/t.txt b/t.txt\n"
                        "index 1111111..2222222 100644\n"
                        "--- a/t.txt\n+++ b/t.txt\n"
                        "@@ -1,2 +1,2 @@\n"
                        "-a\n"
                        "+b\n"
                        "-old\n"
                        "\\ No newline at end of file\n")))
      (should (equal (majutsu-interactive-test--hunk-patch diff '("+b\n"))
                     (concat "@@ -1,2 +1,3 @@\n"
                             " a\n"
                             "+b\n"
                             " old\n"
                             "\\ No newline at end of file\n"))))))

(ert-deftest majutsu-interactive-run-replay-plan/rejects-concurrent-replay ()
  "A second patch replay for the same repository is refused while one runs."
  (let ((majutsu-process--interactive-sessions (make-hash-table :test 'equal))
        callback)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _) "/tmp/replay-repo/"))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "op"))
              ((symbol-function 'majutsu-interactive--make-operation-temp-dir)
               (lambda () "/tmp/replay-repo-tmp/"))
              ((symbol-function 'majutsu-interactive--write-patch)
               (lambda (&rest _) "/tmp/replay-repo-tmp/patch"))
              ((symbol-function 'majutsu-interactive--build-tool-config)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-interactive--retain-temp-dir-for-process)
               (lambda (&rest _) t))
              ((symbol-function 'majutsu-interactive--delete-operation-temp-dir)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-start-jj-with-editor)
               (lambda (_args _env finish &optional _keep)
                 (setq callback finish)
                 'fake-process))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (majutsu-interactive-run-replay-plan "split" nil nil '(:patch "P"))
            (should callback)
            (should-error
             (majutsu-interactive-run-replay-plan "split" nil nil '(:patch "P"))
             :type 'user-error)
            ;; Process completion releases the guard.
            (cl-letf (((symbol-function 'majutsu-process-completion-owned-p)
                       (lambda (_) nil)))
              (funcall callback 'fake-process 0))
            (should (majutsu-interactive-run-replay-plan
                     "split" nil nil '(:patch "P"))))
        (clrhash majutsu-process--interactive-sessions)))))

(ert-deftest majutsu-interactive/integration-renames-and-copies-preserve-mode-selection ()
  "Pathname operations and executable bits remain independently selectable."
  (majutsu-jj-integration-with-sandbox sandbox
    (dolist (action '(rename copy))
      (dolist (restore '(nil t))
        (dolist (selection '(content mode file neither))
          (dolist (old-mode '(#o644 #o755))
            (ert-info ((format "action=%s replay=%s selection=%s mode=%o->%o"
                               action (if restore 'complement 'selected)
                               selection old-mode
                               (if (= old-mode #o644) #o755 #o644)))
              (let* ((repo (majutsu-jj-integration-init
                            sandbox (format "%s-%s-%s-%o"
                                            action restore selection old-mode)))
                     (old (expand-file-name "old.txt" repo))
                     (new (expand-file-name "new.txt" repo))
                     (other (expand-file-name "other.txt" repo))
                     (new-mode (if (= old-mode #o644) #o755 #o644))
                     (content-selected (memq selection '(content file)))
                     (mode-selected (memq selection '(mode file)))
                     (keep-content (if restore
                                       (not content-selected) content-selected))
                     (keep-mode (if restore (not mode-selected) mode-selected))
                     (unsupported (and (eq action 'copy)
                                       keep-mode (not keep-content)))
                     plan)
                (majutsu-jj-integration-write-text old "same\n")
                (set-file-modes old old-mode)
                (majutsu-jj-integration-write-text other "old\n")
                (majutsu-jj-integration-commit repo "base")
                (if (eq action 'rename) (rename-file old new) (copy-file old new))
                (set-file-modes new new-mode)
                (majutsu-jj-integration-write-text other "new\n")
                (setq plan
                      (majutsu-jj-integration-with-washed-diff
                       repo
                       (lambda ()
                         ;; jj detects identical renames.  A copy can be rendered
                         ;; as an add by a backend; exercise its structured copy
                         ;; consumer with the equivalent explicit diff metadata.
                         (when (eq action 'copy)
                           (let ((inhibit-read-only t))
                             (erase-buffer))
                           (majutsu-interactive-test--insert-diff
                            (format
                             (concat "diff --git a/old.txt b/new.txt\n"
                                     "copy from old.txt\ncopy to new.txt\n"
                                     "old mode 100%o\nnew mode 100%o\n"
                                     "diff --git a/other.txt b/other.txt\n"
                                     "--- a/other.txt\n+++ b/other.txt\n"
                                     "@@ -1 +1 @@\n-old\n+new\n")
                             old-mode new-mode)
                            "new.txt"
                            '((:status "copied" :source "old.txt" :target "new.txt")
                              (:status "modified" :source "other.txt"
                               :target "other.txt"))))
                         (let* ((transient-current-command
                                 (if restore 'majutsu-restore 'majutsu-split))
                                (file (majutsu-interactive--file-section-for-file
                                       "new.txt"))
                                (mode (seq-find
                                       (lambda (unit)
                                         (magit-section-match 'jj-mode-change unit))
                                       (oref file children))))
                           (should mode)
                           (should (equal
                                    (plist-get (majutsu-diff-file-metadata file) :status)
                                    (if (eq action 'rename) "renamed" "copied")))
                           (pcase selection
                             ((or 'content 'file)
                              (goto-char (oref file start))
                              (majutsu-interactive-toggle-file)
                              (when (eq selection 'content)
                                (goto-char (oref mode start))
                                (majutsu-interactive-toggle-hunk)))
                             ('mode
                              (goto-char (oref mode start))
                              (majutsu-interactive-toggle-hunk))
                             ('neither
                              (majutsu-interactive-test--toggle-whole-file
                               "other.txt" transient-current-command)))
                           (if unsupported
                               (let ((before (majutsu-jj-operation-id repo)))
                                 (should before)
                                 (should-error
                                  (majutsu-interactive-build-replay-plan-if-selected
                                   nil (if restore 'complement 'selected))
                                  :type 'user-error)
                                 (should (equal (majutsu-jj-operation-id repo)
                                                before))
                                 nil)
                             (majutsu-interactive-build-replay-plan-if-selected
                              nil (if restore 'complement 'selected)))))))
                (if unsupported
                    (progn
                      (should-not plan)
                      (should (file-exists-p new))
                      (should (equal (majutsu-jj-integration-read-bytes new) "same\n"))
                      (should (= (logand (file-modes new) #o777) new-mode))
                      (should (equal (majutsu-jj-integration-read-bytes other) "new\n")))
                  (should plan)
                  (majutsu-jj-integration-run-replay
                   repo (if restore "restore" "split")
                   (if restore '("--changes-in=@")
                     '("--revision=@" "--message=selected"))
                   plan)
                  (unless restore
                    (majutsu-jj-integration-call repo "new" "@-"))
                  (let ((path (if keep-content new old)))
                    (ert-info ((format "result=%s keep-content=%s keep-mode=%s"
                                       (file-name-nondirectory path)
                                       keep-content keep-mode))
                      (should (file-exists-p path))
                      (should (equal (majutsu-jj-integration-read-bytes path) "same\n"))
                      (should (= (logand (file-modes path) #o777)
                                 (if keep-mode new-mode old-mode)))
                      (should (eq (file-exists-p new) (and keep-content t)))
                      (when (eq action 'rename)
                        (should (eq (file-exists-p old) (not keep-content)))))))
                ;; A copy never transfers its destination mode to its source,
                ;; including when an unrepresentable selection is rejected.
                (when (eq action 'copy)
                  (should (file-exists-p old))
                  (should (equal (majutsu-jj-integration-read-bytes old) "same\n"))
                  (should (= (logand (file-modes old) #o777) old-mode)))))))))))

(ert-deftest majutsu-interactive/integration-copy-does-not-inherit-source-chmod ()
  "A copied payload keeps its mode when the source is independently chmodded."
  (majutsu-jj-integration-with-sandbox sandbox
    (dolist (restore '(nil t))
      (dolist (old-mode '(#o644 #o755))
        (let* ((repo (majutsu-jj-integration-init
                      sandbox (format "copy-source-mode-%s-%o" restore old-mode)))
               (source (expand-file-name "source.txt" repo))
               (destination (expand-file-name "copy.txt" repo))
               (other (expand-file-name "other.txt" repo))
               (new-mode (if (= old-mode #o644) #o755 #o644))
               (command (if restore 'majutsu-restore 'majutsu-split))
               plan)
          (majutsu-jj-integration-write-text source "same\n")
          (set-file-modes source old-mode)
          (majutsu-jj-integration-write-text other "old\n")
          (majutsu-jj-integration-commit repo "base")
          (copy-file source destination)
          (set-file-modes destination old-mode)
          (set-file-modes source new-mode)
          (majutsu-jj-integration-write-text other "new\n")
          (setq plan
                (majutsu-jj-integration-with-washed-diff
                 repo
                 (lambda ()
                   ;; Backends may render copies as adds.  Supply the copy
                   ;; metadata while still exercising jj's real editor trees.
                   (let ((inhibit-read-only t))
                     (erase-buffer))
                   (majutsu-interactive-test--insert-diff
                    (format
                     (concat "diff --git a/source.txt b/copy.txt\n"
                             "similarity index 100%%\n"
                             "copy from source.txt\ncopy to copy.txt\n"
                             "diff --git a/source.txt b/source.txt\n"
                             "old mode 100%o\nnew mode 100%o\n"
                             "diff --git a/other.txt b/other.txt\n"
                             "--- a/other.txt\n+++ b/other.txt\n"
                             "@@ -1 +1 @@\n-old\n+new\n")
                     old-mode new-mode)
                    nil
                    '((:status "copied" :source "source.txt" :target "copy.txt")
                      (:status "modified" :source "source.txt" :target "source.txt")
                      (:status "modified" :source "other.txt" :target "other.txt")))
                   (dolist (file (if restore '("other.txt")
                                  '("copy.txt" "source.txt")))
                     (majutsu-interactive-test--toggle-whole-file file command))
                   (majutsu-interactive-build-replay-plan-if-selected
                    nil (if restore 'complement 'selected)))))
          (majutsu-jj-integration-run-replay
           repo (if restore "restore" "split")
           (if restore '("--changes-in=@")
             '("--revision=@" "--message=selected"))
           plan)
          (unless restore
            (majutsu-jj-integration-call repo "new" "@-"))
          (should (equal (majutsu-jj-integration-read-bytes source) "same\n"))
          (should (equal (majutsu-jj-integration-read-bytes destination) "same\n"))
          (should (= (logand (file-modes source) #o777) new-mode))
          (should (= (logand (file-modes destination) #o777) old-mode)))))))

(ert-deftest majutsu-interactive/integration-completion-refreshes-workspace-log ()
  "Dead or repurposed origins still refresh the actual repository log."
  (require 'majutsu-log)
  (majutsu-jj-integration-with-repo repo
    (dolist (dead '(nil t))
      (let ((origin (generate-new-buffer " *replay-origin*"))
            (log (generate-new-buffer " *replay-log*"))
            (root (file-name-as-directory repo)))
        (unwind-protect
            (progn
              (with-current-buffer log
                (majutsu-log-mode)
                (setq-local default-directory root
                            majutsu--default-directory root
                            majutsu-buffer-log-args '("-r" "@"))
                (majutsu-log-refresh))
              (let ((before (majutsu-jj-operation-id root))
                    (description (if dead "dead-origin-result" "repurposed-origin-result")))
                (when dead (kill-buffer origin))
                (majutsu-jj-integration-call repo "describe" "-m" description)
                (should (eq (majutsu-interactive-complete-repository-operation
                             root origin before)
                            'changed))
                (with-current-buffer log
                  (should (string-match-p description (buffer-string))))))
          (when (buffer-live-p origin) (kill-buffer origin))
          (when (buffer-live-p log) (kill-buffer log)))))))

(ert-deftest majutsu-interactive-replay/exclusion-precedes-temporary-files ()
  "An opaque writer blocks replay without allocating a temporary directory."
  (majutsu-jj-integration-with-repo repo
    (let* ((root (file-name-as-directory repo))
           (majutsu-process--interactive-sessions (make-hash-table :test 'equal))
           (owner (list 'blob-write))
           (temporary-file-directory (file-name-as-directory
                                      (make-temp-file "majutsu-no-temp-" t))))
      (unwind-protect
          (progn
            (majutsu-process-register-interactive-session root owner)
            (should-error
             (majutsu-interactive-run-replay-plan
              "split" nil nil '(:base left :payload-root right))
             :type 'user-error)
            (should (eq (gethash root majutsu-process--interactive-sessions) owner))
            (should-not (directory-files temporary-file-directory nil
                                         directory-files-no-dot-files-regexp))
            ;; Read-only probes remain available while the writer owns ROOT.
            (should (majutsu-jj-operation-id root)))
        (delete-directory temporary-file-directory t)))))

(ert-deftest majutsu-interactive-replay/launch-failure-cleans-slot-and-files ()
  "Failure and quit during launch remove replay files and release only its owner."
  (majutsu-jj-integration-with-repo repo
    (dolist (condition '(error quit))
      (let ((majutsu-process--interactive-sessions (make-hash-table :test 'equal))
            directory caught)
        (cl-letf (((symbol-function 'majutsu-start-jj-with-editor)
                   (lambda (&rest _)
                     (signal condition '("launch interrupted"))))
                  ((symbol-function 'majutsu-interactive--make-operation-temp-dir)
                   (lambda ()
                     (setq directory (make-temp-file "majutsu-launch-" t)))))
          (condition-case problem
              (majutsu-interactive-run-replay-plan
               "split" nil nil '(:base left :payload-root right))
            ((error quit) (setq caught (car problem)))))
        (should (eq caught condition))
        (should directory)
        (should-not (file-exists-p directory))
        (should (= (hash-table-count majutsu-process--interactive-sessions) 0))))))

(ert-deftest majutsu-interactive-replay/cancel-releases-owner-and-late-callback-is-safe ()
  "A real child cancellation cleans files without releasing a later writer."
  (majutsu-jj-integration-with-repo repo
    (let* ((root (file-name-as-directory repo))
           (majutsu-process--interactive-sessions (make-hash-table :test 'equal))
           (next-owner (list 'next-writer))
           child callback directory)
      (unwind-protect
          (cl-letf (((symbol-function 'majutsu-start-jj-with-editor)
                     (lambda (_args _message finish &optional _inhibit)
                       (setq callback finish
                             child
                             (make-process
                              :name "majutsu-replay-cancellation"
                              :command '("cat")
                              :connection-type 'pipe
                              :noquery t
                              :sentinel
                              (lambda (process _event)
                                (when (memq (process-status process) '(exit signal))
                                  (funcall finish process 1))))))))
            (majutsu-interactive-run-replay-plan
             "split" nil nil '(:base left :payload-root right))
            (setq directory (process-get child 'majutsu-interactive-temp-dir))
            (should (file-directory-p directory))
            (should-error
             (majutsu-process-register-interactive-session root next-owner)
             :type 'user-error)
            (delete-process child)
            (should-not (gethash root majutsu-process--interactive-sessions))
            (should-not (file-exists-p directory))
            (majutsu-process-register-interactive-session root next-owner)
            (funcall callback child 1)
            (should (eq (gethash root majutsu-process--interactive-sessions)
                        next-owner)))
        (when (and child (process-live-p child)) (delete-process child))
        (when (and directory (file-exists-p directory))
          (delete-directory directory t))))))

(ert-deftest majutsu-interactive-replay/preserves-link-and-binary-payloads ()
  "Rename/copy preserve links and binary bytes on either replay baseline."
  (dolist (base '(left right))
    (dolist (action '(rename copy))
      (let* ((directory (make-temp-file "majutsu-replay-payload-" t))
             (left (expand-file-name "left" directory))
             (right (expand-file-name "right" directory))
             (payload (if (eq base 'left) right left))
             (baseline (if (eq base 'left) left right))
             (bytes (unibyte-string 0 1 2 255)))
        (unwind-protect
            (progn
              (make-directory left)
              (make-directory right)
              (make-symbolic-link "missing-target"
                                  (expand-file-name "old.link" baseline))
              (make-symbolic-link "missing-target"
                                  (expand-file-name "new.link" payload))
              (majutsu-jj-integration-write-bytes
               (expand-file-name "old.bin" baseline) bytes)
              (set-file-modes (expand-file-name "old.bin" baseline) #o644)
              (majutsu-jj-integration-write-bytes
               (expand-file-name "new.bin" payload) bytes)
              (set-file-modes (expand-file-name "new.bin" payload) #o755)
              (let* ((plan (list :base base
                                 :payload-root (if (eq base 'left) 'right 'left)
                                 :file-ops
                                 (list (list :action action :source "old.link"
                                             :path "new.link")
                                       (list :action action :source "old.bin"
                                             :path "new.bin"))))
                     (patch (majutsu-interactive--write-patch "" directory))
                     (script (majutsu-interactive--write-applypatch-script
                              plan directory)))
                (should (zerop (call-process script nil nil nil left right patch))))
              (should (equal (file-symlink-p (expand-file-name "new.link" right))
                             "missing-target"))
              (should (equal (majutsu-jj-integration-read-bytes
                              (expand-file-name "new.bin" right))
                             bytes))
              (should (eq (and (file-symlink-p
                                (expand-file-name "old.link" right)) t)
                          (eq action 'copy))))
          (delete-directory directory t))))))

(ert-deftest majutsu-interactive-replay/interrupted-owned-start-retains-files ()
  "A quit after sentinel ownership keeps the slot and files until child exit."
  (majutsu-jj-integration-with-repo repo
    (let* ((root (file-name-as-directory repo))
           (majutsu-process--interactive-sessions (make-hash-table :test 'equal))
           child directory caught)
      (unwind-protect
          (cl-letf (((symbol-function 'majutsu-start-jj-with-editor)
                     (lambda (_args _message finish &optional _inhibit)
                       (setq child
                             (make-process
                              :name "majutsu-replay-interrupted-start"
                              :command '("cat") :connection-type 'pipe :noquery t
                              :sentinel
                              (lambda (process _event)
                                (when (memq (process-status process) '(exit signal))
                                  (funcall finish process 1)))))
                       (funcall majutsu-process--start-created-callback child)
                       (process-put child 'majutsu-completion-owned t)
                       (signal 'quit nil))))
            (condition-case nil
                (majutsu-interactive-run-replay-plan
                 "split" nil nil '(:base left :payload-root right))
              (quit (setq caught t)))
            (should caught)
            (setq directory (process-get child 'majutsu-interactive-temp-dir))
            (should (file-directory-p directory))
            (should (gethash root majutsu-process--interactive-sessions))
            ;; This test transport owns no repository operation to refresh.
            (process-put child 'majutsu-completion-owned nil)
            (delete-process child)
            (should-not (gethash root majutsu-process--interactive-sessions))
            (should-not (file-exists-p directory)))
        (when (and child (process-live-p child))
          (process-put child 'majutsu-completion-owned nil)
          (delete-process child))
        (when (and directory (file-exists-p directory))
          (delete-directory directory t))))))

(provide 'majutsu-interactive-test)
;;; majutsu-interactive-test.el ends here
