;;; majutsu-conflict-test.el --- Tests for majutsu-conflict  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for conflict marker parsing.

;;; Code:

(require 'ert)
(require 'cl-lib)

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

(defconst majutsu-conflict-test--jj-diff-snapshot-first
  (concat
   "<<<<<<< conflict 1 of 1\n"
   "+++++++ side-1-rev\n"
   "FIRST\n"
   "%%%%%%% diff from: base-1\n"
   (make-string 7 ?\\) "        to: side-2-rev\n"
   "-one\n"
   "+ONE\n"
   "%%%%%%% diff from: base-2\n"
   (make-string 7 ?\\) "        to: side-3-rev\n"
   "-two\n"
   "+TWO\n"
   ">>>>>>> conflict 1 of 1 ends\n")
  "Three-sided JJ diff-style conflict where side #1 is shown as a snapshot.")

(defconst majutsu-conflict-test--jj-diff-snapshot-middle
  (concat
   "<<<<<<< conflict 1 of 1\n"
   "%%%%%%% diff from: base-1\n"
   (make-string 7 ?\\) "        to: side-1-rev\n"
   " line1\n"
   "-AAA\n"
   "+AAA-s1\n"
   " line3\n"
   "+++++++ side-2-rev\n"
   "totally different\n"
   "%%%%%%% diff from: base-2\n"
   (make-string 7 ?\\) "        to: side-3-rev\n"
   " line1\n"
   "-AAA\n"
   "+AAA-s3\n"
   " line3\n"
   ">>>>>>> conflict 1 of 1 ends\n")
  "Three-sided JJ diff-style conflict whose snapshot is a middle side.
jj emits this layout when a later side matches the base better than
the current one (diff_size heuristic); layout captured from a real
jj 0.43 materialization.")

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

(defconst majutsu-conflict-test--jj-diff-absorbed-label
  (concat
   "<<<<<<< conflict 1 of 1\n"
   "%%%%%%% diff from: absorbed changes (from kuvzpzvz d13eb0ec)\n"
   (make-string 7 ?\\)
   "        to: ntwvtmtl a368d22d \"fix(color-words): avoid wrong-side jump on neutral debug payload\" (rebase destination)\n"
   "-old\n"
   "+new\n"
   "+++++++ uyymumuy ac4c1162 \"fix: align majutsu-conflict side selection with jj semantics and long markers\" (rebased revision)\n"
   "base\n"
   ">>>>>>> conflict 1 of 1 ends\n")
  "JJ diff-style conflict containing absorb label format.")


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
        (quoted-label (concat "rtsqusxu 2768b0b9 \"fix "
                              "\"quoted\""
                              " subject\""))
        (short-label "rtsqusxu 2768b0b9 (rebased revision)")
        (absorbed-label "absorbed changes (from kuvzpzvz d13eb0ec)")
        (multi-label "rtsqusxu 2768b0b9, ysrnknol 7a20f389")
        (malformed "vpxusssl not-a-commit"))
    (should (equal (majutsu-conflict-parse-label label)
                   '(:change-id "vpxusssl"
                     :commit-id "38d49363"
                     :description "merge base")))
    (should (equal (majutsu-conflict-parse-label label-with-suffix)
                   '(:change-id "tlwwkqxk"
                     :commit-id "d121763d"
                     :description "commit A")))
    (should (equal (majutsu-conflict-parse-label quoted-label)
                   '(:change-id "rtsqusxu"
                     :commit-id "2768b0b9"
                     :description "fix \"quoted\" subject")))
    (should (equal (majutsu-conflict-parse-label short-label)
                   '(:change-id "rtsqusxu"
                     :commit-id "2768b0b9"
                     :description nil)))
    (should (equal (majutsu-conflict-parse-label absorbed-label)
                   '(:change-id "kuvzpzvz"
                     :commit-id "d13eb0ec"
                     :description "absorbed changes")))
    (should-not (majutsu-conflict-parse-label nil))
    (should-not (majutsu-conflict-parse-label ""))
    (should-not (majutsu-conflict-parse-label multi-label))
    (should-not (majutsu-conflict-parse-label malformed))
    (should (equal (majutsu-conflict-label-change-id label) "vpxusssl"))
    (should (equal (majutsu-conflict-label-commit-id label) "38d49363"))
    (should (equal (majutsu-conflict-label-description label) "merge base"))
    (should (equal (majutsu-conflict-label-change-id label-with-suffix) "tlwwkqxk"))
    (should (equal (majutsu-conflict-label-commit-id label-with-suffix) "d121763d"))
    (should (equal (majutsu-conflict-label-description label-with-suffix) "commit A"))
    (should (equal (majutsu-conflict-label-change-id absorbed-label) "kuvzpzvz"))
    (should (equal (majutsu-conflict-label-commit-id absorbed-label) "d13eb0ec"))
    (should (equal (majutsu-conflict-label-description absorbed-label) "absorbed changes"))
    (should-not (majutsu-conflict-label-change-id nil))
    (should-not (majutsu-conflict-label-commit-id nil))
    (should-not (majutsu-conflict-label-description nil))))

(ert-deftest majutsu-conflict-test-parse-jj-diff ()
  "Test parsing JJ diff-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let* ((c (car conflicts))
             (adds (majutsu-conflict-adds c))
             (removes (majutsu-conflict-removes c)))
        (should (eq 'jj-diff (majutsu-conflict-style c)))
        (should (= 1 (length removes)))
        (should (= 2 (length adds)))
        ;; Side #1 is reconstructed from the diff, side #2 is the snapshot.
        (should (string-match-p "grapefruit" (cdr (car adds))))
        (should (string-match-p "GRAPE" (cdr (cadr adds))))
        (should (string-match-p "grape" (cdr (car removes))))))))

(ert-deftest majutsu-conflict-test-parse-jj-snapshot ()
  "Test parsing JJ snapshot-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let* ((c (car conflicts))
             (adds (majutsu-conflict-adds c))
             (removes (majutsu-conflict-removes c)))
        (should (eq 'jj-snapshot (majutsu-conflict-style c)))
        (should (= 1 (length removes)))
        (should (= 2 (length adds)))
        ;; jj materializes side #1, base, side #2 in that order.
        (should (string-match-p "grapefruit" (cdr (car adds))))
        (should (string-match-p "GRAPE" (cdr (cadr adds))))
        (should (string-match-p "grape" (cdr (car removes))))))))

(ert-deftest majutsu-conflict-test-parse-jj-diff-snapshot-first ()
  "Test parsing a three-sided JJ diff where side #1 is a snapshot."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let* ((c (car conflicts))
             (adds (majutsu-conflict-adds c)))
        (should (eq 'jj-diff (majutsu-conflict-style c)))
        (should (= 2 (length (majutsu-conflict-removes c))))
        (should (= 3 (length adds)))
        (should (equal (mapcar #'cdr adds) '("FIRST\n" "ONE\n" "TWO\n")))))))

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
        (should (= 2 (length adds)))
        ;; The fake short "===========" marker stays snapshot content.
        (should (string-match-p "===========\n" (cdr (cadr adds))))))))

(ert-deftest majutsu-conflict-test-parse-jj-diff-snapshot-middle ()
  "jj may snapshot a middle side; numbering still follows document order."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-middle)
    (let* ((c (car (majutsu-conflict-parse-buffer)))
           (adds (majutsu-conflict-adds c)))
      (should c)
      (should (eq 'jj-diff (majutsu-conflict-style c)))
      (should (= 2 (length (majutsu-conflict-removes c))))
      (should (equal (mapcar #'cdr adds)
                     '("line1\nAAA-s1\nline3\n"
                       "totally different\n"
                       "line1\nAAA-s3\nline3\n")))
      (should (equal (mapcar #'car adds)
                     '("side-1-rev" "side-2-rev" "side-3-rev"))))))

(ert-deftest majutsu-conflict-test-display-parts-preserve-pairing ()
  "A snapshot in any position must not shift the wrong base onto a side."
  (dolist (case `((,majutsu-conflict-test--jj-diff
                  ((change 0 0) (snapshot add 1)))
                 (,majutsu-conflict-test--jj-diff-snapshot-first
                  ((snapshot add 0) (change 0 1) (change 1 2)))
                 (,majutsu-conflict-test--jj-diff-snapshot-middle
                  ((change 0 0) (snapshot add 1) (change 1 2)))
                 (,majutsu-conflict-test--jj-snapshot
                  ((snapshot add 0) (change 0 1)))
                 (,majutsu-conflict-test--git
                  ((snapshot add 0) (change 0 1)))))
    (with-temp-buffer
      (insert (car case))
      (let* ((conflict (car (majutsu-conflict-parse-buffer)))
             (parts (majutsu-conflict-display-parts conflict)))
        (should
         (equal
          (mapcar (lambda (part)
                    (if (eq (plist-get part :kind) 'change)
                        (list 'change
                              (plist-get (plist-get part :before) :index)
                              (plist-get (plist-get part :after) :index))
                      (list 'snapshot
                            (plist-get (plist-get part :term) :kind)
                            (plist-get (plist-get part :term) :index))))
                  parts)
          (cadr case)))))))


(ert-deftest majutsu-conflict-test-parse-no-terminating-newline ()
  "Test parsing jj conflict with end marker missing terminating newline."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (let ((conflicts (majutsu-conflict-parse-buffer)))
      (should (= 1 (length conflicts)))
      (let* ((c (car conflicts))
             (add1 (cdr (car (majutsu-conflict-adds c))))
             (add2 (cdr (cadr (majutsu-conflict-adds c))))
             (remove1 (cdr (car (majutsu-conflict-removes c)))))
        (should (equal add1 "grapefruit"))
        (should (equal add2 "grape\n"))
        (should (equal remove1 "grape"))))))

(ert-deftest majutsu-conflict-test-revision-at-point-absorbed-label ()
  "Test revision metadata extraction for absorb conflict labels."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-absorbed-label)
    (goto-char (point-min))

    ;; Absorb diff marker should resolve to absorbed source change/commit.
    (search-forward "absorbed changes")
    (beginning-of-line)
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "kuvzpzvz"))
      (should (equal (plist-get rev :commit-id) "d13eb0ec"))
      (should (eq (plist-get rev :side) 'remove)))

    ;; Added side should resolve to the "to:" label metadata.
    (search-forward "+new")
    (beginning-of-line)
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "ntwvtmtl"))
      (should (equal (plist-get rev :commit-id) "a368d22d"))
      (should (eq (plist-get rev :side) 'add)))))

(ert-deftest majutsu-conflict-test-revision-at-point-snapshot-sides ()
  "Snapshot +++++++ sections are sides, ------- sections are bases."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (goto-char (point-min))

    ;; Side #1 content resolves to commit A as an add side.
    (search-forward "grapefruit")
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "rtsqusxu"))
      (should (eq (plist-get rev :side) 'add)))

    ;; Base content resolves to the merge base as a remove term.
    (search-forward "grape\n")
    (backward-char 2)
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "vpxusssl"))
      (should (eq (plist-get rev :side) 'remove)))

    ;; Side #2 content resolves to commit B as an add side.
    (search-forward "GRAPE")
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "ysrnknol"))
      (should (eq (plist-get rev :side) 'add)))))

(ert-deftest majutsu-conflict-test-keep-side-indexing ()
  "Test keep-side maps N to jj side #N."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 nil)
    (should (equal (buffer-string)
                   "some text before\napple\ngrapefruit\norange\nsome text after\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 nil)
    (should (equal (buffer-string)
                   "some text before\nAPPLE\nGRAPE\nORANGE\nsome text after\n"))))

(ert-deftest majutsu-conflict-test-keep-side-snapshot-matches-labels ()
  "keep-side N must return the content labeled side #N in snapshot style."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 nil)
    (should (equal (buffer-string) "apple\ngrapefruit\norange\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 nil)
    (should (equal (buffer-string) "APPLE\nGRAPE\nORANGE\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-base)
    (should (equal (buffer-string) "apple\ngrape\norange\n"))))

(ert-deftest majutsu-conflict-test-keep-side-indexing-later-sides ()
  "Test keep-side counts the leading snapshot as side #1."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 nil)
    (should (equal (buffer-string)
                   "ONE\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 3 nil)
    (should (equal (buffer-string)
                   "TWO\n"))))

(ert-deftest majutsu-conflict-test-keep-before-second-side ()
  "Test keep-side with BEFORE selects remove term N."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 t)
    (should (equal (buffer-string)
                   "two\n"))))

(ert-deftest majutsu-conflict-test-keep-base-selects-base ()
  "Test keep-base chooses the first remove term (the merge base)."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-base)
    (should (equal (buffer-string)
                   "some text before\napple\ngrape\norange\nsome text after\n"))))

(ert-deftest majutsu-conflict-test-keep-base-no-terminating-newline ()
  "Test keep-base preserves no-terminating-newline content."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-base)
    (should (equal (buffer-string) "grape"))))

(ert-deftest majutsu-conflict-test-keep-side-no-terminating-newline ()
  "Test keep-side/keep-before preserve newline asymmetry from jj markers."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 nil)
    (should (equal (buffer-string) "grapefruit")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 2 nil)
    (should (equal (buffer-string) "grape\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1 t)
    (should (equal (buffer-string) "grape"))))

(ert-deftest majutsu-conflict-test-at-point ()
  "Test finding a conflict inside its half-open buffer region."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (should-not (majutsu-conflict-at-point))
    (search-forward "<<<<<<<")
    (should (majutsu-conflict-at-point))
    (let* ((conflict (car (majutsu-conflict-parse-buffer)))
           (end (majutsu-conflict-end-pos conflict)))
      (goto-char (1- end))
      (should (majutsu-conflict-at-point))
      (goto-char end)
      (should (looking-at "some text after"))
      (should-not (majutsu-conflict-at-point)))
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
    ;; Check side snapshot content (+++++++ section in jj-diff, side #2)
    (search-forward "APPLE")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-side-2-face))))

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
                'majutsu-conflict-side-2-face))))

(ert-deftest majutsu-conflict-test-font-lock-jj-snapshot ()
  "Test font-lock highlighting for JJ snapshot-style conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    ;; First +++++++ is side #1
    (goto-char (point-min))
    (search-forward "grapefruit")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-side-1-face))
    ;; ------- is the base
    (search-forward "grape\n")
    (backward-char 2)
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-base-face))
    ;; Second +++++++ is side #2
    (search-forward "GRAPE")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-side-2-face))))

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

(ert-deftest majutsu-conflict-test-ensure-mode-jj-disables-smerge ()
  "JJ conflicts should disable smerge-mode when conflict mode is enabled."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (smerge-mode 1)
    (should smerge-mode)
    (majutsu-conflict-ensure-mode)
    (should majutsu-conflict-mode)
    (should-not smerge-mode)))

(ert-deftest majutsu-conflict-test-ensure-mode-git-disables-conflict-mode ()
  "Git conflicts should disable majutsu-conflict-mode when switching to smerge."
  (with-temp-buffer
    (insert majutsu-conflict-test--git)
    (fundamental-mode)
    (majutsu-conflict-mode 1)
    (should majutsu-conflict-mode)
    (majutsu-conflict-ensure-mode)
    (should smerge-mode)
    (should-not majutsu-conflict-mode)))

(ert-deftest majutsu-conflict-test-mode-map-side-bindings ()
  "Digit bindings resolve the selected side or base of a real conflict."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (call-interactively
     (lookup-key majutsu-conflict-mode-map (kbd "C-c ^ 3")))
    (should (equal (buffer-string) "TWO\n")))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (call-interactively
     (lookup-key majutsu-conflict-mode-map (kbd "C-c ^ M-2")))
    (should (equal (buffer-string) "two\n"))))

(defun majutsu-conflict-test--keep-current-at (text search)
  "Insert TEXT, move point to SEARCH, keep the current term."
  (insert text)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (search-forward search))
  (majutsu-conflict-keep-current)
  (buffer-string))

(ert-deftest majutsu-conflict-test-keep-current-snapshot ()
  "keep-current resolves with the snapshot term around point."
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-snapshot "grapefruit")
                   "apple\ngrapefruit\norange\n")))
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-snapshot "-------")
                   "apple\ngrape\norange\n")))
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-snapshot "GRAPE")
                   "APPLE\nGRAPE\nORANGE\n"))))

(ert-deftest majutsu-conflict-test-keep-current-diff-style ()
  "keep-current maps diff lines onto the reconstructed terms."
  ;; A removed line belongs to the base.
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-diff "-grape")
                   "some text before\napple\ngrape\norange\nsome text after\n")))
  ;; Added and context lines belong to the reconstructed side.
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-diff "+grapefruit")
                   "some text before\napple\ngrapefruit\norange\nsome text after\n")))
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-diff " orange")
                   "some text before\napple\ngrapefruit\norange\nsome text after\n")))
  ;; The %%%%%%% marker names the base; the snapshot section is side #2.
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-diff "%%%%%%%")
                   "some text before\napple\ngrape\norange\nsome text after\n")))
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-diff "APPLE")
                   "some text before\nAPPLE\nGRAPE\nORANGE\nsome text after\n"))))

(ert-deftest majutsu-conflict-test-keep-current-git ()
  "keep-current works on all three git-style sections."
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--git "grapefruit")
                   "apple\ngrapefruit\norange\n")))
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--git "|||||||")
                   "apple\ngrape\norange\n")))
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--git "GRAPE")
                   "APPLE\nGRAPE\nORANGE\n"))))

(ert-deftest majutsu-conflict-test-keep-current-rejects-plain-markers ()
  "keep-current refuses jj begin markers and text outside conflicts."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (should-error (majutsu-conflict-keep-current) :type 'user-error))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (should-error (majutsu-conflict-keep-current) :type 'user-error)))

(ert-deftest majutsu-conflict-test-keep-all-concatenates-sides ()
  "keep-all keeps every side in side order and no base."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-all)
    (should (equal (buffer-string)
                   (concat "some text before\n"
                           "apple\ngrapefruit\norange\n"
                           "APPLE\nGRAPE\nORANGE\n"
                           "some text after\n")))))

(ert-deftest majutsu-conflict-test-keep-all-confirms-snapshot-concatenation ()
  "Interactive keep-all confirms unless every base is empty."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (let ((original (buffer-string))
          asked)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_prompt)
                   (setq asked t)
                   nil)))
        (should-error (call-interactively #'majutsu-conflict-keep-all)
                      :type 'user-error))
      (should asked)
      (should (equal (buffer-string) original))))
  (with-temp-buffer
    (insert (concat "<<<<<<< conflict\n"
                    "+++++++ side-1\n"
                    "a\n"
                    "------- base\n"
                    "+++++++ side-2\n"
                    "b\n"
                    ">>>>>>> conflict ends\n"))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "empty-base concatenation should not prompt"))))
      (call-interactively #'majutsu-conflict-keep-all))
    (should (equal (buffer-string) "a\nb\n"))))

(ert-deftest majutsu-conflict-test-keep-all-separates-no-eol-sides ()
  "keep-all inserts a separator after a no-newline side that is not last."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-all)
    (should (equal (buffer-string) "grapefruit\ngrape\n"))))

(ert-deftest majutsu-conflict-test-restyle-diff-to-snapshot ()
  "Restyling a diff-style conflict yields jj's snapshot rendering."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (search-forward "%%%%%%%")
    (majutsu-conflict-restyle)
    (should (equal (buffer-string)
                   (concat
                    "some text before\n"
                    "<<<<<<< conflict 1 of 1\n"
                    "+++++++ rtsqusxu 2768b0b9 \"commit A\"\n"
                    "apple\ngrapefruit\norange\n"
                    "------- vpxusssl 38d49363 \"merge base\"\n"
                    "apple\ngrape\norange\n"
                    "+++++++ ysrnknol 7a20f389 \"commit B\"\n"
                    "APPLE\nGRAPE\nORANGE\n"
                    ">>>>>>> conflict 1 of 1 ends\n"
                    "some text after\n")))))

(ert-deftest majutsu-conflict-test-restyle-cycles-and-round-trips ()
  "Each restyle step re-parses to the same terms."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (let* ((reference (car (majutsu-conflict-parse-buffer)))
           (ref-adds (mapcar #'cdr (majutsu-conflict-adds reference)))
           (ref-removes (mapcar #'cdr (majutsu-conflict-removes reference))))
      (dolist (expected-style '(jj-snapshot git jj-snapshot git))
        (goto-char (point-min))
        (search-forward "apple")
        (majutsu-conflict-restyle)
        (let ((c (car (majutsu-conflict-parse-buffer))))
          (should (eq (majutsu-conflict-style c) expected-style))
          (should (equal (mapcar #'cdr (majutsu-conflict-adds c)) ref-adds))
          (should (equal (mapcar #'cdr (majutsu-conflict-removes c))
                         ref-removes)))))))

(ert-deftest majutsu-conflict-test-restyle-snapshot-to-git-layout ()
  "A 2-sided snapshot conflict converts to git's diff3 layout."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (goto-char (point-min))
    (search-forward "grape")
    (majutsu-conflict-restyle)
    (should (equal (buffer-string)
                   (concat
                    "<<<<<<< rtsqusxu 2768b0b9 \"commit A\"\n"
                    "apple\ngrapefruit\norange\n"
                    "||||||| vpxusssl 38d49363 \"merge base\"\n"
                    "apple\ngrape\norange\n"
                    "=======\n"
                    "APPLE\nGRAPE\nORANGE\n"
                    ">>>>>>> ysrnknol 7a20f389 \"commit B\"\n")))))

(ert-deftest majutsu-conflict-test-restyle-n-sided ()
  "A many-sided diff conflict converts to snapshots; git is refused."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "FIRST")
    (majutsu-conflict-restyle)
    (should (equal (buffer-string)
                   (concat
                    "<<<<<<< conflict 1 of 1\n"
                    "+++++++ side-1-rev\n"
                    "FIRST\n"
                    "------- base-1\n"
                    "one\n"
                    "+++++++ side-2-rev\n"
                    "ONE\n"
                    "------- base-2\n"
                    "two\n"
                    "+++++++ side-3-rev\n"
                    "TWO\n"
                    ">>>>>>> conflict 1 of 1 ends\n")))
    (goto-char (point-min))
    (search-forward "FIRST")
    (should-error (majutsu-conflict-restyle) :type 'user-error)))

(ert-deftest majutsu-conflict-test-restyle-preserves-no-eol-semantics ()
  "Restyling a no-terminating-newline conflict keeps jj's EOL spreading."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-no-terminating-newline)
    (let* ((reference (car (majutsu-conflict-parse-buffer)))
           (ref-adds (mapcar #'cdr (majutsu-conflict-adds reference)))
           (ref-removes (mapcar #'cdr (majutsu-conflict-removes reference))))
      (goto-char (point-min))
      (search-forward "grapefruit")
      (majutsu-conflict-restyle)
      ;; The end marker still has no trailing newline.
      (should (string-suffix-p "conflict 1 of 1 ends" (buffer-string)))
      (let ((c (car (majutsu-conflict-parse-buffer))))
        (should (eq (majutsu-conflict-style c) 'jj-snapshot))
        (should (equal (mapcar #'cdr (majutsu-conflict-adds c)) ref-adds))
        (should (equal (mapcar #'cdr (majutsu-conflict-removes c))
                       ref-removes))))))

(ert-deftest majutsu-conflict-test-restyle-preserves-long-markers ()
  "Restyling keeps the conflict's escalated marker length."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-long-markers)
    (let* ((reference (car (majutsu-conflict-parse-buffer)))
           (ref-adds (mapcar #'cdr (majutsu-conflict-adds reference))))
      (goto-char (point-min))
      (search-forward "HEADING")
      (majutsu-conflict-restyle)
      (should (string-prefix-p (make-string 15 ?<) (buffer-string)))
      (let ((c (car (majutsu-conflict-parse-buffer))))
        (should (eq (majutsu-conflict-style c) 'jj-snapshot))
        (should (= 15 (majutsu-conflict-marker-len c)))
        ;; The fake short "===========" marker survives as content.
        (should (equal (mapcar #'cdr (majutsu-conflict-adds c)) ref-adds))))))

(ert-deftest majutsu-conflict-test-revision-at-point-git-left-is-add ()
  "The left section of a git-style conflict is a side, not a remove."
  (with-temp-buffer
    (insert majutsu-conflict-test--git)
    (goto-char (point-min))
    (search-forward "grapefruit")
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "rtsqusxu"))
      (should (eq (plist-get rev :side) 'add)))
    (search-forward "grape\n")
    (backward-char 2)
    (let ((rev (majutsu-conflict-revision-at-point)))
      (should rev)
      (should (equal (plist-get rev :change-id) "vpxusssl"))
      (should (eq (plist-get rev :side) 'base)))))

(ert-deftest majutsu-conflict-test-font-lock-git-style ()
  "Git-style conflicts get per-side and base faces too."
  (with-temp-buffer
    (insert majutsu-conflict-test--git)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "grapefruit")
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-side-1-face))
      (search-forward "grape\n")
      (backward-char 2)
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-base-face))
      (search-forward "=======")
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-marker-face))
      (search-forward "GRAPE")
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-side-2-face)))))

(ert-deftest majutsu-conflict-test-font-lock-third-side-face ()
  "A three-sided snapshot conflict cycles onto the third side face."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "FIRST")
    (majutsu-conflict-restyle)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "FIRST")
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-side-1-face))
      (search-forward "ONE")
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-side-2-face))
      (search-forward "TWO")
      (should (eq (majutsu-conflict-test--face-at-line)
                  'majutsu-conflict-side-3-face)))))

(ert-deftest majutsu-conflict-test-region-props-lifecycle ()
  "Conflict regions carry a mouse menu keymap that the mode cleans up."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "+grapefruit")
    (should (eq (get-char-property (point) 'keymap)
                majutsu-conflict--region-map))
    (should (stringp (get-char-property (point) 'help-echo)))
    (majutsu-conflict-mode -1)
    (should-not (get-char-property (point) 'keymap))
    (should-not (get-char-property (point) 'help-echo))))

(ert-deftest majutsu-conflict-test-region-overlay-dropped-on-destroyed-conflict ()
  "Destroying a conflict's markers removes its menu overlay immediately."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "+grapefruit")
    (should (eq (get-char-property (point) 'keymap)
                majutsu-conflict--region-map))
    ;; Hand-merge: delete the begin marker line, breaking the conflict.
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (delete-region (line-beginning-position)
                   (min (1+ (line-end-position)) (point-max)))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "+grapefruit")
    (should-not (get-char-property (point) 'keymap))
    (should-not (get-char-property (point) 'help-echo))))

(ert-deftest majutsu-conflict-test-unparseable-region-not-highlighted ()
  "Regions the parser rejects get neither faces nor a context menu."
  (with-temp-buffer
    ;; A git merge-style conflict without a base section is not a
    ;; parseable jj conflict.
    (insert "<<<<<<< theirs\nAAA\n=======\nBBB\n>>>>>>> ours\n")
    (insert "\n")
    (insert majutsu-conflict-test--jj-snapshot)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "AAA")
    (should-not (get-char-property (point) 'keymap))
    (should-not (majutsu-conflict-test--face-at-line))
    (let ((case-fold-search nil))
      (search-forward "grapefruit"))
    (should (eq (get-char-property (point) 'keymap)
                majutsu-conflict--region-map))
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-side-1-face))))

(ert-deftest majutsu-conflict-test-auto-leave-checks-whole-buffer ()
  "Auto-leave must look past the current narrowing."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (insert "between\n")
    (let ((second-start (point)))
      (insert majutsu-conflict-test--jj-snapshot)
      (majutsu-conflict-mode 1)
      (narrow-to-region second-start (point-max))
      (goto-char (point-min))
      (search-forward "<<<<<<<")
      (majutsu-conflict-keep-side 1)
      ;; The first conflict is outside the restriction but unresolved.
      (should majutsu-conflict-mode)
      (widen)
      (should (= 1 (length (majutsu-conflict-parse-buffer)))))))

(ert-deftest majutsu-conflict-test-mode-disables-smerge ()
  "Enabling majutsu-conflict-mode turns smerge-mode off."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (fundamental-mode)
    (smerge-mode 1)
    (should smerge-mode)
    (majutsu-conflict-mode 1)
    (should-not smerge-mode)
    (should majutsu-conflict-mode)))

(ert-deftest majutsu-conflict-test-enables-eldoc-for-plain-buffers ()
  "The mode turns on eldoc when global-eldoc gave up on the buffer."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (fundamental-mode)
    (let ((global-eldoc-mode t))
      (majutsu-conflict-mode 1))
    (should (bound-and-true-p eldoc-mode))
    (majutsu-conflict-mode -1)
    (should-not (bound-and-true-p eldoc-mode))))

(ert-deftest majutsu-conflict-test-context-menu-lists-terms ()
  "The context menu is a keymap with one entry per side and base."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff-snapshot-first)
    (goto-char (point-min))
    (search-forward "FIRST")
    (let ((menu (majutsu-conflict--context-menu (majutsu-conflict-at-point))))
      (let ((labels nil))
        (map-keymap (lambda (_key def)
                      (when (eq (car-safe def) 'menu-item)
                        (push (cadr def) labels)))
                    menu)
        (should (seq-some (lambda (l) (string-match-p "Side #3: side-3-rev" l))
                          labels))
        (should (seq-some (lambda (l) (string-match-p "Base 2: base-2" l))
                          labels))))))

(ert-deftest majutsu-conflict-test-eldoc-describes-term ()
  "Eldoc reports the term kind, number, and label at point."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "grapefruit"))
    (let ((doc (majutsu-conflict--eldoc)))
      (should (string-match-p "side 1/2" doc))
      (should (string-match-p "commit A" doc)))
    (let ((case-fold-search nil))
      (search-forward "grape\n")
      (backward-char 2))
    (let ((doc (majutsu-conflict--eldoc)))
      (should (string-match-p "base 1/1" doc))
      (should (string-match-p "merge base" doc)))))

(ert-deftest majutsu-conflict-test-auto-leave ()
  "Resolving the last conflict turns the mode off; earlier ones do not."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (insert "between\n")
    (insert majutsu-conflict-test--jj-snapshot)
    (majutsu-conflict-mode 1)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1)
    (should majutsu-conflict-mode)
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-side 1)
    (should-not majutsu-conflict-mode))
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (majutsu-conflict-mode 1)
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (let ((majutsu-conflict-auto-leave nil))
      (majutsu-conflict-keep-side 1))
    (should majutsu-conflict-mode)))

(ert-deftest majutsu-conflict-test-parse-buffer-cache ()
  "Parsing is cached per buffer tick and invalidated by edits."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (let ((first (majutsu-conflict-parse-buffer)))
      (should (eq first (majutsu-conflict-parse-buffer)))
      (goto-char (point-max))
      (insert "tail\n")
      (should-not (eq first (majutsu-conflict-parse-buffer))))))

(defconst majutsu-conflict-test--jj-snapshot-with-git-lines
  "<<<<<<< conflict 1 of 1
+++++++ rtsqusxu 2768b0b9 \"commit A\"
Title
=======
body A
------- vpxusssl 38d49363 \"merge base\"
Title
body base
+++++++ ysrnknol 7a20f389 \"commit B\"
Title
body B
>>>>>>> conflict 1 of 1 ends
"
  "Snapshot conflict whose side #1 contains a git-separator-like line.")

(defconst majutsu-conflict-test--git-with-jj-lines
  "<<<<<<< left-rev
left top
-------
left bottom
||||||| base-rev
base
=======
right
>>>>>>> right-rev
"
  "Git-style conflict whose left side contains a jj-marker-like line.")

(ert-deftest majutsu-conflict-test-term-scan-ignores-other-style-markers ()
  "Marker lines of the other style are term content, as in jj's parsers."
  ;; jj style: a git separator inside side #1 stays side #1 content.
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--jj-snapshot-with-git-lines
                    "body A")
                   "Title\n=======\nbody A\n")))
  ;; git style: a jj remove marker inside the left side stays content.
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--git-with-jj-lines
                    "left bottom")
                   "left top\n-------\nleft bottom\n"))))

(ert-deftest majutsu-conflict-test-font-lock-ignores-other-style-markers ()
  "Other-style marker lines keep their term's face, not the marker face."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot-with-git-lines)
    (fundamental-mode)
    (font-lock-mode 1)
    (majutsu-conflict-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "=======")
    (should (eq (majutsu-conflict-test--face-at-line)
                'majutsu-conflict-side-1-face))))

(ert-deftest majutsu-conflict-test-keep-current-git-end-marker ()
  "The git end marker labels side #2 and keeps it, like the begin marker."
  (with-temp-buffer
    (should (equal (majutsu-conflict-test--keep-current-at
                    majutsu-conflict-test--git ">>>>>>>")
                   "APPLE\nGRAPE\nORANGE\n"))))

(ert-deftest majutsu-conflict-test-restyle-refuses-marker-collisions ()
  "Restyling refuses when a term contains the target style's markers."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot-with-git-lines)
    (goto-char (point-min))
    (search-forward "body A")
    (should-error (majutsu-conflict-restyle) :type 'user-error)
    ;; The buffer is untouched.
    (should (equal (buffer-string)
                   majutsu-conflict-test--jj-snapshot-with-git-lines)))
  (with-temp-buffer
    (insert majutsu-conflict-test--git-with-jj-lines)
    (goto-char (point-min))
    (search-forward "left bottom")
    (should-error (majutsu-conflict-restyle) :type 'user-error)))

(ert-deftest majutsu-conflict-test-keep-all-empty-last-side ()
  "An empty last side must not force a trailing newline onto the result."
  (with-temp-buffer
    (insert (concat "<<<<<<< conflict 1 of 1\n"
                    "+++++++ a-rev (no terminating newline)\n"
                    "a\n"
                    "------- base-rev (no terminating newline)\n"
                    "b\n"
                    "+++++++ empty-rev\n"
                    "\n"
                    ">>>>>>> conflict 1 of 1 ends"))
    (goto-char (point-min))
    (search-forward "<<<<<<<")
    (majutsu-conflict-keep-all)
    (should (equal (buffer-string) "a"))))

(ert-deftest majutsu-conflict-test-restyle-keeps-point-on-term ()
  "Restyling moves point onto the same term in the new rendering."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "APPLE"))
    (majutsu-conflict-restyle)
    (let ((term (majutsu-conflict--term-at-point
                 (majutsu-conflict-at-point))))
      (should (equal term '(:kind add :index 1))))))

(ert-deftest majutsu-conflict-test-restyle-read-only-keeps-point ()
  "A failed restyle in a read-only buffer must not move point."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "APPLE"))
    (setq buffer-read-only t)
    (let ((before (point)))
      (should-error (majutsu-conflict-restyle))
      (should (= (point) before)))))

(ert-deftest majutsu-conflict-test-restyle-restores-refinement ()
  "Word-level refinement survives a restyle between jj styles."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-diff)
    (majutsu-conflict-refine)
    (should (majutsu-conflict--region-refined-p (point-min) (point-max)))
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "grapefruit"))
    (majutsu-conflict-restyle)
    (should (majutsu-conflict--region-refined-p (point-min) (point-max)))))

(ert-deftest majutsu-conflict-test-restyle-recoordinates-modes ()
  "Restyling to git hands a git-only buffer over to smerge-mode."
  (with-temp-buffer
    (insert majutsu-conflict-test--jj-snapshot)
    (fundamental-mode)
    (majutsu-conflict-mode 1)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "grapefruit"))
    (majutsu-conflict-restyle)
    (should smerge-mode)
    (should-not majutsu-conflict-mode)))

(provide 'majutsu-conflict-test)
;;; majutsu-conflict-test.el ends here
