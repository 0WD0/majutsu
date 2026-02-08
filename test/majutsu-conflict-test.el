;;; majutsu-conflict-test.el --- Tests for majutsu-conflict -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for conflict marker parsing.

;;; Code:

(require 'ert)

(defconst majutsu-conflict-test--root
  (locate-dominating-file (or (getenv "PWD") default-directory)
                          "majutsu-conflict.el")
  "Root directory for Majutsu tests.")

(when majutsu-conflict-test--root
  (add-to-list 'load-path majutsu-conflict-test--root)
  (load (expand-file-name "majutsu-conflict.el" majutsu-conflict-test--root) nil t))

(require 'majutsu-conflict)

;;; Test Data

(defconst majutsu-conflict-test--jj-diff
  (concat
   "some text before\n"
   "<<<<<<< conflict 1 of 1\n"
   "%%%%%%% diff from: vpxusssl 38d49363 \"merge base\"\n"
   (make-string 7 ?\\) "        to: rtsqusxu 2768b0b9 \"commit A\"\n"
   " apple\n"
   "-grape\n"
   "+grapefruit\n"
   " orange\n"
   "+++++++ ysrnknol 7a20f389 \"commit B\"\n"
   "APPLE\n"
   "GRAPE\n"
   "ORANGE\n"
   ">>>>>>> conflict 1 of 1 ends\n"
   "some text after\n")
  "Sample JJ diff-style conflict.")

(defconst majutsu-conflict-test--jj-diff-long
  "<<<<<<< conflict 1 of 1
%%%%%%% diff from: utzqqyqr d1e4c728 \"snapshot refine\" (parents of rebased revision)
\\\\\\\\\\        to: utzqqyqr 3e1a7f5b \"snapshot refine\" (rebase destination)
   (majutsu-evil--define-keys '(normal visual) 'majutsu-diff-mode-map
     (kbd \"g d\") #'majutsu-jump-to-diffstat-or-diff
     (kbd \"C-<return>\") #'majutsu-diff-visit-workspace-file)
   (majutsu-evil--define-keys '(normal visual motion) 'majutsu-blob-mode-map
     (kbd \"p\") #'majutsu-blob-previous
     (kbd \"n\") #'majutsu-blob-next
     (kbd \"q\") #'majutsu-blob-quit
-    (kbd \"V\") #'majutsu-blob-visit-file
+    ;; (kbd \"V\") #'majutsu-blob-visit-file
     (kbd \"b\") #'majutsu-annotate-addition
     ;; RET visits the revision (edit)
     (kbd \"RET\") #'majutsu-edit-changeset)
+++++++ ntknkwlu 1c8cca65 \"X | refactor majutsu-evil\" (rebased revision)
          (define-key map (kbd (number-to-string n))
                      (lambda () (interactive)
                        (majutsu-conflict-keep-side n nil)))))
      map)
    \"Keymap for JJ conflict actions under Evil.\")
  (when (and (featurep 'evil) majutsu-evil-enable-integration)
    (majutsu-evil--define-keys 'normal 'majutsu-conflict-mode-map
      (kbd \"g j\") #'majutsu-conflict-next
      (kbd \"] ]\") #'majutsu-conflict-next
      (kbd \"g k\") #'majutsu-conflict-prev
      (kbd \"[ [\") #'majutsu-conflict-prev
      (kbd \"g b\") #'majutsu-conflict-keep-base
      (kbd \"g r\") majutsu-conflict-evil-resolve-map
      (kbd \"g R\") majutsu-conflict-evil-before-map)))
>>>>>>> conflict 1 of 1 ends
"
  "JJ diff-style conflict with longer content.")

(defconst majutsu-conflict-test--jj-diff-long-markers
  (concat
   (make-string 15 ?<) " conflict 1 of 1\n"
   (make-string 15 ?%) " diff from: wqvuxsty cb9217d5 \"merge base\"\n"
   (make-string 15 ?\\) "        to: kwntsput 0e15b770 \"commit A\"\n"
   "-Heading\n"
   "+HEADING\n"
   " =======\n"
   (make-string 15 ?+) " mpnwrytz 52020ed6 \"commit B\"\n"
   "New Heading\n"
   "===========\n"
   (make-string 15 ?>) " conflict 1 of 1 ends\n")
  "JJ diff-style conflict with long markers and fake short markers.")

(defconst majutsu-conflict-test--jj-diff-base-first
  (concat
   "<<<<<<< conflict 1 of 1\n"
   "+++++++ base-rev\n"
   "BASE\n"
   "%%%%%%% diff from: left-1\n"
   (make-string 7 ?\\) "        to: right-1\n"
   "-one\n"
   "+ONE\n"
   "%%%%%%% diff from: left-2\n"
   (make-string 7 ?\\) "        to: right-2\n"
   "-two\n"
   "+TWO\n"
   ">>>>>>> conflict 1 of 1 ends\n")
  "JJ diff-style conflict where snapshot base appears before diffs.")

(defconst majutsu-conflict-test--jj-diff-no-terminating-newline
  (concat
   "<<<<<<< conflict 1 of 1\n"
   "+++++++ tlwwkqxk d121763d \"commit A\" (no terminating newline)\n"
   "grapefruit\n"
   "%%%%%%% diff from: qwpqssno fe561d93 \"merge base\" (no terminating newline)\n"
   (make-string 7 ?\\) "        to: poxkmrxy c735fe02 \"commit B\"\n"
   " grape\n"
   "+\n"
   ">>>>>>> conflict 1 of 1 ends")
  "JJ diff-style conflict whose end marker has no terminating newline.")


(defconst majutsu-conflict-test--jj-snapshot
  "<<<<<<< conflict 1 of 1
+++++++ rtsqusxu 2768b0b9 \"commit A\"
apple
grapefruit
orange
------- vpxusssl 38d49363 \"merge base\"
apple
grape
orange
+++++++ ysrnknol 7a20f389 \"commit B\"
APPLE
GRAPE
ORANGE
>>>>>>> conflict 1 of 1 ends
"
  "Sample JJ snapshot-style conflict.")

(defconst majutsu-conflict-test--git
  "<<<<<<< rtsqusxu 2768b0b9 \"commit A\"
apple
grapefruit
orange
||||||| vpxusssl 38d49363 \"merge base\"
apple
grape
orange
=======
APPLE
GRAPE
ORANGE
>>>>>>> ysrnknol 7a20f389 \"commit B\"
"
  "Sample Git-style conflict.")

;;; Tests

(ert-deftest majutsu-conflict-label-parsing-test ()
  "Test parsing conflict label metadata."
  (let ((label "vpxusssl 38d49363 \"merge base\"")
        (label-with-suffix "tlwwkqxk d121763d \"commit A\" (no terminating newline)")
        (malformed "vpxusssl 38d49363"))
    (should (equal (majutsu-conflict-parse-label label)
                   '(:change-id "vpxusssl"
                     :commit-id "38d49363"
                     :description "merge base")))
    (should (equal (majutsu-conflict-parse-label label-with-suffix)
                   '(:change-id "tlwwkqxk"
                     :commit-id "d121763d"
                     :description "commit A")))
    (should-not (majutsu-conflict-parse-label nil))
    (should-not (majutsu-conflict-parse-label ""))
    (should-not (majutsu-conflict-parse-label malformed))
    (should (equal (majutsu-conflict-label-change-id label) "vpxusssl"))
    (should (equal (majutsu-conflict-label-commit-id label) "38d49363"))
    (should (equal (majutsu-conflict-label-description label) "merge base"))
    (should (equal (majutsu-conflict-label-change-id label-with-suffix) "tlwwkqxk"))
    (should (equal (majutsu-conflict-label-commit-id label-with-suffix) "d121763d"))
    (should (equal (majutsu-conflict-label-description label-with-suffix) "commit A"))
    (should-not (majutsu-conflict-label-change-id nil))
    (should-not (majutsu-conflict-label-commit-id nil))
    (should-not (majutsu-conflict-label-description nil))))

(ert-deftest majutsu-conflict-test-parse-jj-diff ()
  "Test parsing JJ diff-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let ((c (car conflicts)))
        (should (eq 'jj-diff (majutsu-conflict-style c)))
        (should (= 1 (length (majutsu-conflict-removes c))))
        (should (= 1 (length (majutsu-conflict-adds c))))
        (should (majutsu-conflict-base c))
        (should (string-match-p "GRAPE" (cdr (majutsu-conflict-base c))))))))

(ert-deftest majutsu-conflict-test-parse-jj-snapshot ()
  "Test parsing JJ snapshot-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let ((c (car conflicts)))
        (should (eq 'jj-snapshot (majutsu-conflict-style c)))
        (should (= 1 (length (majutsu-conflict-removes c))))
        (should (= 1 (length (majutsu-conflict-adds c))))
        (should (majutsu-conflict-base c))))))

(ert-deftest majutsu-conflict-test-parse-jj-diff-base-first ()
  "Test parsing JJ diff where snapshot base appears first."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-base-first)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let ((c (car conflicts)))
        (should (eq 'jj-diff (majutsu-conflict-style c)))
        (should (= 2 (length (majutsu-conflict-removes c))))
        (should (= 2 (length (majutsu-conflict-adds c))))
        (should (string-match-p "BASE" (cdr (majutsu-conflict-base c))))))))

(ert-deftest majutsu-conflict-test-parse-git ()
  "Test parsing Git-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--git)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let ((c (car conflicts)))
        (should (eq 'git (majutsu-conflict-style c)))
        (should (= 1 (length (majutsu-conflict-removes c))))
        (should (= 2 (length (majutsu-conflict-adds c))))))))

(ert-deftest majutsu-conflict-test-parse-jj-diff-long-markers ()
  "Test parsing long-marker JJ conflicts with fake short markers in content."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-long-markers)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let* ((c (car conflicts))
             (adds (majutsu-conflict-adds c)))
        (should (eq 'jj-diff (majutsu-conflict-style c)))
        (should (= 15 (majutsu-conflict-marker-len c)))
        (should (= 1 (length (majutsu-conflict-removes c))))
        (should (= 1 (length adds)))
        (should (string-match-p "===========\n" (cdr (majutsu-conflict-base c))))))))

(ert-deftest majutsu-conflict-test-parse-no-terminating-newline ()
  "Test parsing jj conflict with end marker missing terminating newline."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let* ((c (car conflicts))
             (base (cdr (majutsu-conflict-base c)))
             (add1 (cdr (car (majutsu-conflict-adds c))))
             (remove1 (cdr (car (majutsu-conflict-removes c)))))
        (should (equal base "grapefruit"))
        (should (equal remove1 "grape"))
        (should (equal add1 "grape\n"))))))

(ert-deftest majutsu-conflict-test-keep-side-indexing ()
  "Test keep-side maps N to jj add term N."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 nil)
    (should (equal (buffer-string)
                   "some text before\napple\ngrapefruit\norange\nsome text after\n"))))

(ert-deftest majutsu-conflict-test-keep-side-indexing-second-side ()
  "Test keep-side can select second jj side when present."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-base-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 nil)
    (should (equal (buffer-string)
                   "TWO\n"))))

(ert-deftest majutsu-conflict-test-keep-before-second-side ()
  "Test keep-side with BEFORE selects remove term for side N."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-base-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 t)
    (should (equal (buffer-string)
                   "two\n"))))

(ert-deftest majutsu-conflict-test-keep-base-selects-snapshot ()
  "Test keep-base chooses the snapshot base term."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-base)
    (should (equal (buffer-string)
                   "some text before\nAPPLE\nGRAPE\nORANGE\nsome text after\n"))))

(ert-deftest majutsu-conflict-test-keep-base-no-terminating-newline ()
  "Test keep-base preserves no-terminating-newline content."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-base)
    (should (equal (buffer-string) "grapefruit"))))

(ert-deftest majutsu-conflict-test-keep-side-no-terminating-newline ()
  "Test keep-side/keep-before preserve newline asymmetry from jj markers."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 nil)
    (should (equal (buffer-string) "grape\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 t)
    (should (equal (buffer-string) "grape"))))

(ert-deftest majutsu-conflict-test-at-point ()
  "Test finding conflict at point."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (should-not (majutsu-conflict-at-point))
    (search-forward "<<<<<<<")
    (should (majutsu-conflict-at-point))
    (goto-char (point-max))
    (should-not (majutsu-conflict-at-point))))

(ert-deftest majutsu-conflict-test-navigation ()
  "Test conflict navigation."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (insert "\n")
    (insert majutsu-conflict-test--git)
    (goto-char (point-min))
    (majutsu-conflict-next)
    (should (looking-at "<<<<<<<"))
    (majutsu-conflict-next)
    (should (looking-at "<<<<<<<"))
    (majutsu-conflict-prev)
    (should (looking-at "<<<<<<<"))))

(defun majutsu-conflict-test--face-at-line ()
  "Get the face at the beginning of current line.
Handles both single face and face list."
  (let ((face (get-text-property (line-beginning-position) 'face)))
    (if (listp face) (car face) face)))

(ert-deftest majutsu-conflict-test-font-lock-keywords-added ()
  "Test that font-lock keywords are added."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    ;; Check keywords are added
    (should (cl-find 'majutsu-conflict--find-conflict font-lock-keywords
                     :key (lambda (x) (if (consp x) (car x)))))))

(ert-deftest majutsu-conflict-test-match-line ()
  "Test that match-line matcher works."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    ;; Find conflict first
    (majutsu-conflict--find-conflict nil)
    (let ((conflict-end (match-end 0)))
      ;; Reset to conflict start
      (goto-char (match-beginning 0))
      (setq majutsu-conflict--in-diff nil
            majutsu-conflict--in-add nil
            majutsu-conflict--in-remove nil)
      ;; First line should be <<<<<<<
      (should (majutsu-conflict--match-line conflict-end))
      (should (match-beginning 0)))))

(ert-deftest majutsu-conflict-test-font-lock-jj-diff ()
  "Test font-lock highlighting for JJ diff-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    ;; Check marker line
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-marker-face))
    ;; Check diff marker
    (search-forward "%%%%%%%")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-marker-face))
    ;; Check removed line
    (search-forward "-grape")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-removed-face))
    ;; Check added line
    (search-forward "+grapefruit")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-added-face))
    ;; Check context line
    (search-forward " orange")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-context-face))
    ;; Check base content (+++++++ section in jj-diff)
    (search-forward "APPLE")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-base-face))))

(ert-deftest majutsu-conflict-test-font-lock-jj-diff-long ()
  "Test font-lock highlighting for JJ diff-style long markers."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-long-markers)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    ;; Check marker line
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-marker-face))
    ;; Check diff marker
    (search-forward "%%%%%%%")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-marker-face))
    ;; Check removed line
    (search-forward "-Heading")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-removed-face))
    ;; Check added line
    (search-forward "+HEADING")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-added-face))
    ;; Short fake marker line in snapshot section should be content.
    (search-forward "===========")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-base-face))))

(ert-deftest majutsu-conflict-test-font-lock-jj-snapshot ()
  "Test font-lock highlighting for JJ snapshot-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    ;; First +++++++ is base
    (goto-char (point-min))
    (search-forward "grapefruit")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-base-face))
    ;; ------- is "from" side (removed)
    (search-forward "grape\n")
    (backward-char 2)
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-removed-face))
    ;; Second +++++++ is "to" side (added)
    (search-forward "GRAPE")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-added-face))))

(ert-deftest majutsu-conflict-test-overlay-cleanup ()
  "Test that refine overlays are cleaned up."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    ;; Manually create test overlays with majutsu-conflict-refine property
    (let ((ov1 (make-overlay 10 20))
          (ov2 (make-overlay 30 40)))
      (overlay-put ov1 'majutsu-conflict-refine t)
      (overlay-put ov2 'majutsu-conflict-refine t))
    ;; Verify overlays exist
    (should (= 2 (length (cl-remove-if-not
                          (lambda (ov)
                            (overlay-get ov 'majutsu-conflict-refine))
                          (overlays-in (point-min) (point-max))))))
    ;; Clear overlays
    (majutsu-conflict--clear-overlays)
    ;; Verify all refine overlays are removed
    (should-not (cl-some (lambda (ov)
                           (overlay-get ov 'majutsu-conflict-refine))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest majutsu-conflict-test-refine-snapshot ()
  "Test word-level refinement for jj-snapshot style."
  (with-temp-buffer
    (insert "<<<<<<< conflict
+++++++ base
apple
------- from
apple
+++++++ to
apricot
>>>>>>> end
")
    (majutsu-conflict--refine-snapshots)
    ;; Should have refine overlays
    (let ((refine-ovs (cl-remove-if-not
                       (lambda (ov)
                         (overlay-get ov 'majutsu-conflict-refine))
                       (overlays-in (point-min) (point-max)))))
      (should (> (length refine-ovs) 0)))))

(ert-deftest majutsu-conflict-test-context-updates-after-edit ()
  "Test that context face updates after edits in diff section."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    ;; Insert a context line inside the diff section.
    (goto-char (point-min))
    (search-forward "+grapefruit")
    (forward-line 1)
    (insert " new-context\n")
    (forward-line -1)
    (font-lock-ensure (line-beginning-position) (line-end-position))
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-context-face))))

(ert-deftest majutsu-conflict-test-mode-does-not-modify-buffer ()
  "Test that enabling majutsu-conflict-mode does not mark buffer as modified."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (set-buffer-modified-p nil)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (should-not (buffer-modified-p))))

(ert-deftest majutsu-conflict-test-ensure-mode-jj ()
  "Test that JJ conflicts enable majutsu-conflict-mode."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (majutsu-conflict-ensure-mode)
    (should majutsu-conflict-mode)
    (should-not smerge-mode)))

(ert-deftest majutsu-conflict-test-ensure-mode-git ()
  "Test that Git conflicts enable smerge-mode."
  (with-temp-buffer
    (insert majutsu-conflict-test--git)
    (fundamental-mode)
    (majutsu-conflict-ensure-mode)
    (should smerge-mode)
    (should-not majutsu-conflict-mode)))

(provide 'majutsu-conflict-test)
;;; majutsu-conflict-test.el ends here
