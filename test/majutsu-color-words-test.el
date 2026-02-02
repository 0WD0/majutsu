;;; test-majutsu-color-words.el --- Tests for color-words backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for the color-words diff backend.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu)
(require 'majutsu-diff)
(require 'majutsu-color-words)

(ert-deftest majutsu-color-words-wash-diffs-creates-file-section ()
  "Color-words washer should create jj-file sections for headers."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (propertize
                    (string-join
                     '("Modified regular file foo:"
                       "   1    1: a"
                       "   2     : b"
                       "        3: c")
                     "\n")
                    'fontified nil)))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-section (seq-find (lambda (sec)
                                       (eq (oref sec type) 'jj-file))
                                     (oref diff-root children))))
        (should (eieio-object-p file-section))
        (should (equal (oref file-section value) "foo"))
        (should-not (text-properties-at 0 (oref file-section value)))))))

(ert-deftest majutsu-color-words-line-info-at-point-parses-columns ()
  "Line info parser should return from/to line numbers and column."
  (with-temp-buffer
    (insert "   2     : removed\n        3: added\n")
    (goto-char (point-min))
    (let ((info (majutsu-color-words-line-info-at-point)))
      (should (equal (plist-get info :from-line) 2))
      (should (equal (plist-get info :to-line) nil))
      (should (numberp (plist-get info :content-column))))
    (forward-line 1)
    (let ((info (majutsu-color-words-line-info-at-point)))
      (should (equal (plist-get info :from-line) nil))
      (should (equal (plist-get info :to-line) 3))
      (should (numberp (plist-get info :content-column))))))

(ert-deftest majutsu-color-words-wash-diffs-creates-hunks-and-margins ()
  "Color-words washer should split hunks and hide inline columns."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: a"
                      "   2    2: b"
                      "    ..."
                      "   5    5: e")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-section (seq-find (lambda (sec)
                                       (eq (oref sec type) 'jj-file))
                                     (oref diff-root children)))
             (hunks (seq-filter (lambda (sec)
                                  (eq (oref sec type) 'jj-hunk))
                                (oref file-section children))))
        (should (= (length hunks) 2)))
      (goto-char (point-min))
      (should (re-search-forward " a$" nil t))
      (let* ((bol (line-beginning-position))
             (prefix (get-text-property bol 'line-prefix))
             (disp (and prefix (get-text-property 0 'display prefix))))
        (should (numberp left-margin-width))
        (should (> left-margin-width 0))
        (should (eq (get-text-property bol 'invisible)
                    'majutsu-color-words-inline))
        (should (and (listp disp)
                     (equal (car disp) '(margin left-margin))))))))

(ert-deftest majutsu-color-words-paint-preserves-faces ()
  "Painting a color-words hunk should not overwrite font-lock-face."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: hello world")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      ;; Simulate ANSI-derived per-word face on the content line.
      (goto-char (point-min))
      (should (re-search-forward "hello" nil t))
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face 'ansi-color-red)
      ;; Now paint the hunk (focus highlight).
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                 (oref diff-root children)))
             (hunk (car (seq-filter (lambda (s) (eq (oref s type) 'jj-hunk))
                                    (oref file-sec children)))))
        (magit-section-paint hunk t)
        ;; The per-word face must survive.
        (should (eq (get-text-property (match-beginning 0) 'font-lock-face)
                    'ansi-color-red))
        ;; painted slot must still be set.
        (should (eq (oref hunk painted) 'highlight))
        ;; Focus highlight overlays should exist.
        (should (seq-some (lambda (ov)
                            (overlay-get ov 'majutsu-color-words-highlight))
                          (overlays-in (oref hunk start) (oref hunk end))))
        ;; Unpaint — overlays should be removed.
        (magit-section-paint hunk nil)
        (should (eq (oref hunk painted) 'plain))
        (should-not (seq-some (lambda (ov)
                                (overlay-get ov 'majutsu-color-words-highlight))
                              (overlays-in (oref hunk start) (oref hunk end))))))))

(ert-deftest majutsu-color-words-refine-skipped ()
  "Refinement should be skipped when no ANSI-underlined spans exist."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk t)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: same"
                      "   2     : old"
                      "        3: new")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                 (oref diff-root children)))
             (hunk (car (seq-filter (lambda (s) (eq (oref s type) 'jj-hunk))
                                    (oref file-sec children)))))
        ;; Trigger refine — no ANSI faces so no refinement overlays.
        (magit-section--refine hunk)
        ;; No fine overlays should exist (no underlined spans to refine).
        (should-not (seq-some (lambda (ov)
                                (eq (overlay-get ov 'diff-mode) 'fine))
                              (overlays-in (oref hunk start) (oref hunk end))))))))

(ert-deftest majutsu-color-words-strip-debug-markers ()
  "Debug wrappers should be stripped and token properties preserved."
  (with-temp-buffer
    (insert "<<diff removed token::old>><<diff added token::new>>")
    (should (majutsu-color-words--strip-debug-markers))
    (should (equal (buffer-string) "oldnew"))
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'majutsu-color-words-debug-side)
                'removed))
    (should (get-text-property (point) 'majutsu-color-words-debug-token))
    (goto-char (+ (point-min) 3))
    (should (eq (get-text-property (point) 'majutsu-color-words-debug-side)
                'added))
    (should (get-text-property (point) 'majutsu-color-words-debug-token))))

(ert-deftest majutsu-color-words-strip-debug-markers-trailing-angle ()
  "Debug stripping handles payloads ending with `>' before next marker."
  (with-temp-buffer
    ;; The first payload ends with `>', so the wrapper boundary is `>>>`.
    ;; This must still be split as data `x>' + close `>>'.
    (insert "<<diff removed::x>>><<diff added::y>>")
    (should (majutsu-color-words--strip-debug-markers))
    (should (equal (buffer-string) "x>y"))
    (should-not (save-excursion
                  (goto-char (point-min))
                  (search-forward "<<diff" nil t)))
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'majutsu-color-words-debug-side)
                'removed))
    (goto-char (+ (point-min) 2))
    (should (eq (get-text-property (point) 'majutsu-color-words-debug-side)
                'added))))

(ert-deftest majutsu-color-words-strip-debug-markers-ansi-literal-diff-text ()
  "ANSI-aware boundaries should keep literal `<<diff ...>>' payload text.
This mirrors real output where source text can contain debug-like snippets."
  (with-temp-buffer
    (insert (concat
             "\x1b[4m\x1b[38;5;2m"
             "<<diff added token::(insert \"<<diff removed::x>>><<diff added::y>>\")>>"
             "\x1b[24m\x1b[39m"))
    (should (majutsu-color-words--strip-debug-markers))
    (should (string-match-p
             (regexp-quote "(insert \"<<diff removed::x>>><<diff added::y>>\")")
             (buffer-string)))
    (goto-char (point-min))
    (should (search-forward "(insert" nil t))
    (should (eq (get-text-property (match-beginning 0)
                                   'majutsu-color-words-debug-side)
                'added))
    (should (get-text-property (match-beginning 0)
                               'majutsu-color-words-debug-token))))

(ert-deftest majutsu-color-words-collect-debug-token-spans ()
  "Collect debug token spans as removed/added underlined spans."
  (with-temp-buffer
    (insert "<<diff removed token::old>><<diff added token::new>>")
    (majutsu-color-words--strip-debug-markers)
    (let ((spans (majutsu-color-words--collect-debug-token-spans
                  (point-min) (point-max))))
      (should (equal spans
                     '((removed t 1 4)
                       (added t 4 7)))))))

(ert-deftest majutsu-color-words-collect-debug-change-spans ()
  "Collect removed/added spans with token flags from debug labels."
  (with-temp-buffer
    (insert "<<diff removed::old>><<diff removed token::x>><<diff added token::y>><<diff added::new>>")
    (majutsu-color-words--strip-debug-markers)
    (let ((spans (majutsu-color-words--collect-debug-change-spans
                  (point-min) (point-max))))
      (should (equal spans
                     '((removed nil 1 4)
                       (removed t 4 5)
                       (added t 5 6)
                       (added nil 6 9)))))))

(ert-deftest majutsu-color-words-wash-diffs-handles-debug-markers ()
  "Washer should parse jj debug-color wrappers and keep token metadata."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (string-join
                    '("<<diff header::Modified regular file foo:>>"
                      "<<diff removed line_number::   1>><<diff:: >><<diff added line_number::   1>><<diff::: hello >><<diff removed token::old>><<diff added token::new>>")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      (should-not (save-excursion
                    (goto-char (point-min))
                    (search-forward "<<diff" nil t)))
      (goto-char (point-min))
      (should (re-search-forward "old" nil t))
      (should (eq (get-text-property (match-beginning 0)
                                     'majutsu-color-words-debug-side)
                  'removed))
      (should (get-text-property (match-beginning 0)
                                 'majutsu-color-words-debug-token))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-section (seq-find (lambda (sec)
                                       (eq (oref sec type) 'jj-file))
                                     (oref diff-root children))))
        (should (eieio-object-p file-section))
        (should (equal (oref file-section value) "foo"))))))

(ert-deftest majutsu-color-words-wash-diffs-literal-diff-text-in-payload ()
  "Washer should not treat literal `<<diff ...>>' source text as wrappers."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (string-join
                    '("\x1b[38;5;3m<<diff header::Modified regular file foo:>>\x1b[39m"
                      "<<diff::    ...>>"
                      "<<diff::     >>\x1b[38;5;2m<<diff added line_number::   1>>\x1b[39m<<diff::: >>\x1b[4m\x1b[38;5;2m<<diff added token::(insert \"<<diff removed::x>>><<diff added::y>>\")>>\x1b[24m\x1b[39m")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      (should-not (save-excursion
                    (goto-char (point-min))
                    (search-forward "<<diff::" nil t)))
      (goto-char (point-min))
      (should (search-forward "<<diff removed::x>>><<diff added::y>>" nil t))
      (goto-char (point-min))
      (should (search-forward "(insert" nil t))
      (should (eq (get-text-property (match-beginning 0)
                                     'majutsu-color-words-debug-side)
                  'added))
      (should (get-text-property (match-beginning 0)
                                 'majutsu-color-words-debug-token)))))

;;; Face Classification Tests

(ert-deftest majutsu-color-words-face-type-removed ()
  "Red-dominant underline face should classify as (removed . t)."
  ;; True-color red (jj catppuccin)
  (should (equal (majutsu-color-words--face-type
                  '(ansi-color-underline (:foreground "#E78284")))
                 '(removed . t)))
  ;; Standard ANSI red
  (should (equal (majutsu-color-words--face-type
                  '(ansi-color-underline (:foreground "red3")))
                 '(removed . t))))

(ert-deftest majutsu-color-words-face-type-added ()
  "Green-dominant underline face should classify as (added . t)."
  ;; True-color green (jj catppuccin)
  (should (equal (majutsu-color-words--face-type
                  '(ansi-color-underline (:foreground "#A6D189")))
                 '(added . t)))
  ;; Standard ANSI green
  (should (equal (majutsu-color-words--face-type
                  '(ansi-color-underline (:foreground "green3")))
                 '(added . t))))

(ert-deftest majutsu-color-words-face-type-non-underlined ()
  "Non-underlined colored plist faces should return (TYPE . nil)."
  ;; Non-underlined red (whole removed line)
  (should (equal (majutsu-color-words--face-type '(:foreground "#E78284"))
                 '(removed . nil)))
  ;; Non-underlined green (whole added line)
  (should (equal (majutsu-color-words--face-type '(:foreground "#A6D189"))
                 '(added . nil)))
  ;; Standard ANSI names
  (should (equal (majutsu-color-words--face-type '(:foreground "red3"))
                 '(removed . nil)))
  (should (equal (majutsu-color-words--face-type '(:foreground "green3"))
                 '(added . nil))))

(ert-deftest majutsu-color-words-face-type-nil-for-plain ()
  "Non-color faces should classify as nil."
  (should-not (majutsu-color-words--face-type nil))
  (should-not (majutsu-color-words--face-type 'magit-diff-context))
  ;; Nested list without :foreground is not a plist
  (should-not (majutsu-color-words--face-type '((:foreground "red3")))))

;;; Change Span Collection Tests

(ert-deftest majutsu-color-words-collect-change-spans ()
  "Collect removed/added spans from font-lock-face properties."
  (with-temp-buffer
    (insert "hello oldnew world")
    ;; Mark "old" (pos 7-10) as removed (underlined)
    (put-text-property 7 10 'font-lock-face
                       '(ansi-color-underline (:foreground "red3")))
    ;; Mark "new" (pos 10-13) as added (underlined)
    (put-text-property 10 13 'font-lock-face
                       '(ansi-color-underline (:foreground "green3")))
    (let ((spans (majutsu-color-words--collect-change-spans 1 (point-max))))
      (should (= (length spans) 2))
      (should (equal (car spans) '(removed t 7 10)))
      (should (equal (cadr spans) '(added t 10 13))))))

(ert-deftest majutsu-color-words-collect-non-underlined-spans ()
  "Non-underlined colored text should also be collected as spans."
  (with-temp-buffer
    (insert "hello old new world")
    ;; Mark "old" as removed (non-underlined — whole line removal)
    (put-text-property 7 10 'font-lock-face '(:foreground "red3"))
    ;; Mark "new" as added (non-underlined — whole line addition)
    (put-text-property 11 14 'font-lock-face '(:foreground "green3"))
    (let ((spans (majutsu-color-words--collect-change-spans 1 (point-max))))
      (should (= (length spans) 2))
      (should (equal (car spans) '(removed nil 7 10)))
      (should (equal (cadr spans) '(added nil 11 14))))))

(ert-deftest majutsu-color-words-group-change-pairs ()
  "Adjacent removed+added spans should be grouped into pairs."
  ;; Paired change (underlined spans)
  (let ((pairs (majutsu-color-words--group-change-pairs
                '((removed t 7 10) (added t 10 13)))))
    (should (= (length pairs) 1))
    (should (equal (car pairs)
                   '(7 10 10 13 ((7 . 10)) ((10 . 13)) nil nil))))
  ;; Pure removal (underlined)
  (let ((pairs (majutsu-color-words--group-change-pairs
                '((removed t 7 10)))))
    (should (equal (car pairs)
                   '(7 10 nil nil ((7 . 10)) nil nil nil))))
  ;; Pure addition (underlined)
  (let ((pairs (majutsu-color-words--group-change-pairs
                '((added t 10 13)))))
    (should (equal (car pairs)
                   '(nil nil 10 13 nil ((10 . 13)) nil nil))))
  ;; Two separate paired changes
  (let ((pairs (majutsu-color-words--group-change-pairs
                '((removed t 5 8) (added t 8 12)
                  (removed t 20 25) (added t 25 30)))))
    (should (= (length pairs) 2))
    (should (equal (car pairs)
                   '(5 8 8 12 ((5 . 8)) ((8 . 12)) nil nil)))
    (should (equal (cadr pairs)
                   '(20 25 25 30 ((20 . 25)) ((25 . 30)) nil nil))))
  ;; Non-underlined (line-level) — no UL-SPANS
  (let ((pairs (majutsu-color-words--group-change-pairs
                '((removed nil 7 10) (added nil 10 13)))))
    (should (= (length pairs) 1))
    (should (equal (car pairs)
                   '(7 10 10 13 nil nil ((7 . 10)) ((10 . 13))))))
  ;; Mixed: non-underlined removed + underlined added
  (let ((pairs (majutsu-color-words--group-change-pairs
                '((removed nil 7 10) (added t 10 13)))))
    (should (equal (car pairs)
                   '(7 10 10 13 nil ((10 . 13)) ((7 . 10)) nil)))))

;;; Refinement Tests

(ert-deftest majutsu-color-words-refine-creates-overlays ()
  "Refining a color-words hunk should create region and fine overlays."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk 'all)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: hello oldnew world")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      ;; Simulate ANSI faces: "old" = removed, "new" = added (underlined).
      (goto-char (point-min))
      (should (re-search-forward "old" nil t))
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         '(ansi-color-underline (:foreground "red3")))
      (should (re-search-forward "new" nil t))
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         '(ansi-color-underline (:foreground "green3")))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                 (oref diff-root children)))
             (hunk (car (seq-filter (lambda (s) (eq (oref s type) 'jj-hunk))
                                    (oref file-sec children)))))
        ;; Trigger refinement.
        (majutsu-diff--update-hunk-refinement hunk)
        ;; refined slot should be set.
        (should (oref hunk refined))
        ;; Fine overlays should exist (2 for underlined spans + 2 for regions).
        (let* ((all-ovs (overlays-in (oref hunk start) (oref hunk end)))
               (fine-ovs (seq-filter
                          (lambda (ov) (eq (overlay-get ov 'diff-mode) 'fine))
                          all-ovs))
               (region-ovs (seq-filter
                            (lambda (ov) (overlay-get ov 'smerge--refine-region))
                            all-ovs))
               (word-ovs (seq-filter
                          (lambda (ov)
                            (and (eq (overlay-get ov 'diff-mode) 'fine)
                                 (not (overlay-get ov 'smerge--refine-region))))
                          all-ovs)))
          ;; 4 fine overlays total: 2 region + 2 word
          (should (= (length fine-ovs) 4))
          ;; 2 smerge--refine-region overlays (one removed, one added)
          (should (= (length region-ovs) 2))
          ;; 2 word-level fine overlays
          (should (= (length word-ovs) 2)))))))

(ert-deftest majutsu-color-words-refine-cleanup ()
  "Unrefinement should remove all refinement overlays including smerge regions."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk 'all)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: hello oldnew world")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      ;; Simulate ANSI faces.
      (goto-char (point-min))
      (should (re-search-forward "old" nil t))
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         '(ansi-color-underline (:foreground "red3")))
      (should (re-search-forward "new" nil t))
      (put-text-property (match-beginning 0) (match-end 0)
                         'font-lock-face
                         '(ansi-color-underline (:foreground "green3")))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                 (oref diff-root children)))
             (hunk (car (seq-filter (lambda (s) (eq (oref s type) 'jj-hunk))
                                    (oref file-sec children)))))
        ;; Apply refinement.
        (majutsu-diff--update-hunk-refinement hunk)
        (should (oref hunk refined))
        ;; Verify overlays exist before cleanup.
        (should (seq-some
                 (lambda (ov) (overlay-get ov 'smerge--refine-region))
                 (overlays-in (oref hunk start) (oref hunk end))))
        ;; Now remove refinement.
        (setq-local majutsu-diff-refine-hunk nil)
        (majutsu-diff--update-hunk-refinement hunk t)
        ;; refined slot must be cleared.
        (should-not (oref hunk refined))
        ;; No fine overlays should remain.
        (should-not (seq-some
                     (lambda (ov) (eq (overlay-get ov 'diff-mode) 'fine))
                     (overlays-in (oref hunk start) (oref hunk end))))
        ;; No smerge--refine-region overlays should remain.
        (should-not (seq-some
                     (lambda (ov) (overlay-get ov 'smerge--refine-region))
                     (overlays-in (oref hunk start) (oref hunk end))))))))

(ert-deftest majutsu-color-words-refine-pure-removal ()
  "Pure removal span (no paired added) should get diff-refine-removed face."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk 'all)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: hello removed world")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      ;; Mark "removed" as removed (underlined, no added counterpart).
      (goto-char (point-min))
      (should (re-search-forward "removed" nil t))
      (let ((rbeg (match-beginning 0))
            (rend (match-end 0)))
        (put-text-property rbeg rend 'font-lock-face
                           '(ansi-color-underline (:foreground "red3")))
        (let* ((diff-root (car (oref magit-root-section children)))
               (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                   (oref diff-root children)))
               (hunk (car (seq-filter (lambda (s) (eq (oref s type) 'jj-hunk))
                                      (oref file-sec children)))))
          (majutsu-diff--update-hunk-refinement hunk)
          ;; Should have a diff-refine-removed overlay (fine, non-region).
          (let ((fine-ovs (seq-filter
                           (lambda (ov)
                             (and (eq (overlay-get ov 'diff-mode) 'fine)
                                  (not (overlay-get ov 'smerge--refine-region))
                                  (eq (overlay-get ov 'face)
                                      'diff-refine-removed)))
                           (overlays-in rbeg rend))))
            (should (= (length fine-ovs) 1)))
          ;; Should also have a smerge--refine-region overlay.
          (let ((region-ovs (seq-filter
                             (lambda (ov)
                               (overlay-get ov 'smerge--refine-region))
                             (overlays-in rbeg rend))))
            (should (= (length region-ovs) 1))))))))

(ert-deftest majutsu-color-words-refine-pure-addition ()
  "Pure addition span (no paired removed) should get diff-refine-added face."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk 'all)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: hello added world")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      ;; Mark "added" as added (underlined, no removed counterpart).
      (goto-char (point-min))
      (should (re-search-forward "added" nil t))
      (let ((abeg (match-beginning 0))
            (aend (match-end 0)))
        (put-text-property abeg aend 'font-lock-face
                           '(ansi-color-underline (:foreground "green3")))
        (let* ((diff-root (car (oref magit-root-section children)))
               (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                   (oref diff-root children)))
               (hunk (car (seq-filter (lambda (s) (eq (oref s type) 'jj-hunk))
                                      (oref file-sec children)))))
          (majutsu-diff--update-hunk-refinement hunk)
          ;; Should have a diff-refine-added overlay (fine, non-region).
          (let ((fine-ovs (seq-filter
                           (lambda (ov)
                             (and (eq (overlay-get ov 'diff-mode) 'fine)
                                  (not (overlay-get ov 'smerge--refine-region))
                                  (eq (overlay-get ov 'face)
                                      'diff-refine-added)))
                           (overlays-in abeg aend))))
            (should (= (length fine-ovs) 1)))
          ;; Should also have a smerge--refine-region overlay.
          (let ((region-ovs (seq-filter
                             (lambda (ov)
                               (overlay-get ov 'smerge--refine-region))
                             (overlays-in abeg aend))))
            (should (= (length region-ovs) 1))))))))

(ert-deftest majutsu-color-words-refine-region-non-token-data ()
  "Region overlays should store non-token spans for shadow cursor mapping.
Token overlays should be tagged, and no fine overlay should be
cross-linked via `smerge--refine-other'."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk 'all)
           (output (string-join
                    '("Modified regular file foo:"
                      "   1    1: hello oldnew world")
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      ;; Simulate ANSI faces.
      (goto-char (point-min))
      (should (re-search-forward "hello " nil t))
      (let ((hbeg (match-beginning 0))
            (hend (match-end 0)))
        (put-text-property hbeg hend 'font-lock-face
                           '(:foreground "red3")))
      (should (re-search-forward "old" nil t))
      (let ((rbeg (match-beginning 0))
            (rend (match-end 0)))
        (put-text-property rbeg rend 'font-lock-face
                           '(ansi-color-underline (:foreground "red3")))
        (should (re-search-forward "new" nil t))
        (let ((abeg (match-beginning 0))
              (aend (match-end 0)))
          (put-text-property abeg aend 'font-lock-face
                             '(ansi-color-underline (:foreground "green3")))
          (should (re-search-forward "world" nil t))
          (let ((wbeg (match-beginning 0))
                (wend (match-end 0)))
            (put-text-property wbeg wend 'font-lock-face
                               '(:foreground "green3")))
          (let* ((diff-root (car (oref magit-root-section children)))
                 (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                     (oref diff-root children)))
                 (hunk (car (seq-filter
                             (lambda (s) (eq (oref s type) 'jj-hunk))
                             (oref file-sec children)))))
            (majutsu-diff--update-hunk-refinement hunk)
            (let* ((all-ovs (overlays-in (oref hunk start) (oref hunk end)))
                   (region-ovs (seq-filter
                                (lambda (ov)
                                  (overlay-get ov 'smerge--refine-region))
                                all-ovs))
                   (token-ovs (seq-filter
                               (lambda (ov)
                                 (overlay-get ov 'majutsu-color-words-token))
                               all-ovs))
                   (linked (seq-filter
                            (lambda (ov) (overlay-get ov 'smerge--refine-other))
                            all-ovs)))
              (should (>= (length region-ovs) 2))
              (dolist (ov region-ovs)
                (let ((spans (overlay-get ov 'majutsu-color-words-non-token)))
                  (should (listp spans))))
              ;; Token overlays are tagged but not cross-linked.
              (should (>= (length token-ovs) 2))
              (should-not linked))))))))

(ert-deftest majutsu-color-words-shadow-pos-maps-shared-stream ()
  "Shadow cursor should align shared non-token text across lines.
Uses jj `--color=debug' output where removed/added blocks do not align
line-by-line, but shared non-token text still matches in order."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (majutsu-diff-backend 'color-words)
           (majutsu-diff-refine-hunk 'all)
           (snippet (string-join
                     '("<<diff context removed line_number:: 221>><<diff context:: >><<diff context added line_number:: 221>><<diff context::: ;;;; ANSI Face Classification>>"
                       "<<diff context removed line_number:: 222>><<diff context:: >><<diff context added line_number:: 222>><<diff context::: >>"
                       "<<diff context removed line_number:: 223>><<diff context:: >><<diff context added line_number:: 223>><<diff context::: (defun majutsu-color-words--face-type (face)>>"
                       "<<diff removed line_number:: 224>><<diff::     : >><<diff removed::  \"Classify FACE as `removed'>><<diff removed token::,>><<diff removed:: `added'>><<diff removed token::, or >><<diff removed::nil>><<diff removed token::.>><<diff removed::>>"
                       "<<diff removed line_number:: 225>><<diff::     : >><<diff removed token::ANSI-processed color-words output uses>><<diff removed:: `ansi-color-underline' with a>>"
                       "<<diff removed line_number:: 226>><<diff::     : >><<diff removed::colored>><<diff removed token:: >><<diff removed::foreground >><<diff removed token::for changed words.  >><<diff removed::Red-dominant foreground >><<diff removed token::means>>"
                       "<<diff removed line_number:: 227>><<diff::     : >><<diff removed::removed; green-dominant >><<diff removed token::means>><<diff removed:: added.\">>"
                       "<<diff removed line_number:: 228>><<diff::     : >><<diff removed::  (when (>><<diff removed token::and (>><<diff removed::consp face)>><<diff removed token:: (memq 'ansi-color-underline face))>><<diff removed::>>"
                       "<<diff removed line_number:: 229>><<diff::     : >><<diff removed::    (>><<diff removed token::when->><<diff removed::let>><<diff removed token::* (>><<diff removed::(plist (seq-find #'listp face))>>"
                       "<<diff removed line_number:: 230>><<diff::     : >><<diff removed::                (fg (plist-get plist :foreground))>>"
                       "<<diff removed line_number:: 231>><<diff::     : >><<diff removed::                (rgb (color-values fg)))>>"
                       "<<diff removed line_number:: 232>><<diff::     : >><<diff removed::      (if (> (car rgb) (cadr rgb)) 'removed 'added))))>>"
                       "<<diff::     >><<diff added line_number:: 224>><<diff::: >><<diff added::  \"Classify FACE as >><<diff added token::(TYPE . UNDERLINED-P) or nil.>>"
                       "<<diff::     >><<diff added line_number:: 225>><<diff::: >><<diff added token::TYPE is >><<diff added::`removed'>><<diff added token:: or>><<diff added:: `added'>><<diff added token::; UNDERLINED-P is non->><<diff added::nil>><<diff added token:: when the face>><<diff added::>>"
                       "<<diff::     >><<diff added line_number:: 226>><<diff::: >><<diff added token::includes>><<diff added:: `ansi-color-underline' >><<diff added token::(word unique to one side).>>"
                       "<<diff::     >><<diff added line_number:: 227>><<diff::: >><<diff added token::>>"
                       "<<diff::     >><<diff added line_number:: 228>><<diff::: >><<diff added token::After `ansi-color-apply-on-region' >><<diff added::with >><<diff added token::`ansi-color-apply-text-property-face':>>"
                       "<<diff::     >><<diff added line_number:: 229>><<diff::: >><<diff added token::  Non-underlined colored: (:foreground \\\"#RRGGBB\\\")     — >><<diff added::a>><<diff added token:: plist>><<diff added::>>"
                       "<<diff::     >><<diff added line_number:: 230>><<diff::: >><<diff added token::  Underlined >><<diff added::colored>><<diff added token:::     (ansi-color-underline (:>><<diff added::foreground >><<diff added token::\\\"#RRGGBB\\\"))>>"
                       "<<diff::     >><<diff added line_number:: 231>><<diff::: >><<diff added token::>>"
                       "<<diff::     >><<diff added line_number:: 232>><<diff::: >><<diff added::Red-dominant foreground >><<diff added token::→ >><<diff added::removed; green-dominant >><<diff added token::→>><<diff added:: added.\">>"
                       "<<diff::     >><<diff added line_number:: 233>><<diff::: >><<diff added::  (when (consp face)>>"
                       "<<diff::     >><<diff added line_number:: 234>><<diff::: >><<diff added::    (let>><<diff added token:: ((underlined (memq 'ansi-color-underline face))>>"
                       "<<diff::     >><<diff added line_number:: 235>><<diff::: >><<diff added token::          >><<diff added::(plist (>><<diff added token::cond>>"
                       "<<diff::     >><<diff added line_number:: 236>><<diff::: >><<diff added token::                  ;; Underlined: (ansi-color-underline (:foreground ...))>>"
                       "<<diff::     >><<diff added line_number:: 237>><<diff::: >><<diff added token::                  ((memq 'ansi-color-underline face)>>"
                       "<<diff::     >><<diff added line_number:: 238>><<diff::: >><<diff added token::                   (>><<diff added::seq-find #'listp face))>>"
                       "<<diff::     >><<diff added line_number:: 239>><<diff::: >><<diff added::                >><<diff added token::  ;; Non-underlined plist: (:foreground ...)>>"
                       "<<diff::     >><<diff added line_number:: 240>><<diff::: >><<diff added token::                  ((plist-get face :foreground)>>"
                       "<<diff::     >><<diff added line_number:: 241>><<diff::: >><<diff added token::                   face))))>>"
                       "<<diff::     >><<diff added line_number:: 242>><<diff::: >><<diff added token::      (when-let* (>><<diff added::(fg (>><<diff added token::and plist (>><<diff added::plist-get plist :foreground))>><<diff added token::)>><<diff added::>>"
                       "<<diff::     >><<diff added line_number:: 243>><<diff::: >><<diff added::                >><<diff added token::  >><<diff added::(rgb (color-values fg)))>>"
                       "<<diff::     >><<diff added line_number:: 244>><<diff::: >><<diff added::      >><<diff added token::  (let ((type >><<diff added::(if (> (car rgb) (cadr rgb)) 'removed 'added)))>><<diff added token::>>"
                       "<<diff::     >><<diff added line_number:: 245>><<diff::: >><<diff added token::          (cons type (and underlined t))))))>><<diff added::)>>")
                     "\n"))
           (output (string-join
                    (list "<<diff header::Modified regular file majutsu-color-words.el:>>"
                          snippet)
                    "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-color-words-wash-diffs '("--color-words")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (file-sec (seq-find (lambda (s) (eq (oref s type) 'jj-file))
                                 (oref diff-root children)))
             (hunk (car (seq-filter
                         (lambda (s) (eq (oref s type) 'jj-hunk))
                         (oref file-sec children)))))
        (majutsu-diff--update-hunk-refinement hunk)
        (cl-labels
            ((find-nth-span (needle side index)
               (goto-char (point-min))
               (let ((count 0)
                     found)
                 (while (and (search-forward needle nil t) (not found))
                   (let ((pos (match-beginning 0)))
                     (when (and (eq (get-text-property pos 'majutsu-color-words-debug-side) side)
                                (not (get-text-property pos 'majutsu-color-words-debug-token)))
                       (setq count (1+ count))
                       (when (= count index)
                         (setq found (cons (match-beginning 0) (match-end 0)))))))
                 (should found)
                 found))
             (find-nth-token-span (needle side index)
               (goto-char (point-min))
               (let ((count 0)
                     found)
                 (while (and (search-forward needle nil t) (not found))
                   (let ((pos (match-beginning 0)))
                     (when (and (eq (get-text-property pos 'majutsu-color-words-debug-side) side)
                                (get-text-property pos 'majutsu-color-words-debug-token))
                       (setq count (1+ count))
                       (when (= count index)
                         (setq found (cons (match-beginning 0) (match-end 0)))))))
                 (should found)
                 found))
             (pos-at (needle side index offset)
               (let* ((span (find-nth-span needle side index))
                      (pos (+ (car span) offset)))
                 (should (< pos (cdr span)))
                 pos))
             (pos-at-token (needle side index offset)
               (let* ((span (find-nth-token-span needle side index))
                      (pos (+ (car span) offset)))
                 (should (< pos (cdr span)))
                 pos))
             (assert-shadow (from-text from-side from-index from-offset
                                       to-text to-side to-index to-offset)
               (let* ((from-pos (pos-at from-text from-side from-index from-offset))
                      (expected (pos-at to-text to-side to-index to-offset))
                      (shadow (majutsu-diff--color-words-shadow-pos from-pos)))
                 (should shadow)
                 (should (= shadow expected)))))
          (dolist (case
                   '(("\"Classify FACE as " removed 1 3 "\"Classify FACE as " added 1 3)
                     ("`removed'" removed 1 2 "`removed'" added 1 2)
                     ("`added'" removed 1 2 "`added'" added 1 2)
                     ("ansi-color-underline" removed 1 5 "ansi-color-underline" added 1 5)
                     ("Red-dominant foreground " removed 1 4 "Red-dominant foreground " added 1 4)
                     ("removed; green-dominant " removed 1 9 "removed; green-dominant " added 1 9)
                     ("(when " removed 1 2 "(when " added 1 2)
                     ("consp face" removed 1 3 "consp face" added 1 3)
                     ("seq-find #'listp face" removed 1 8 "seq-find #'listp face" added 1 8)
                     ("plist-get plist :foreground" added 1 8 "plist-get plist :foreground" removed 1 8)))
            (apply #'assert-shadow case))
          (dolist (case
                   '((", or " removed 1 1 "; UNDERLINED-P is non-" added 1 0)
                     ("means" removed 1 2 "→ " added 1 0)
                     ("includes" added 1 2 "." removed 1 0)))
            (pcase-let ((`(,from-text ,from-side ,from-index ,from-offset
                           ,to-text ,to-side ,to-index ,to-offset)
                         case))
              (let* ((from-pos (pos-at-token from-text from-side from-index from-offset))
                     (expected (pos-at-token to-text to-side to-index to-offset))
                     (shadow (majutsu-diff--color-words-shadow-pos from-pos)))
                (should shadow)
                (should (= shadow expected)))))
          (dolist (case
                   '(("TYPE is " added 1 2 "as `removed'" removed 1 3)))
            (pcase-let ((`(,from-text ,from-side ,from-index ,from-offset
                           ,to-text ,to-side ,to-index ,to-offset)
                         case))
              (let* ((from-pos (pos-at-token from-text from-side from-index from-offset))
                     (expected (pos-at to-text to-side to-index to-offset))
                     (shadow (majutsu-diff--color-words-shadow-pos from-pos)))
                (should shadow)
                (should (= shadow expected))))))))))

(provide 'test-majutsu-color-words)
;;; test-majutsu-color-words.el ends here
