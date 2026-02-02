;;; majutsu-color-words.el --- Color-words diff backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides a diff backend for jj's native color-words output.
;; It wraps color-words output into Majutsu file sections and annotates
;; line numbers for navigation without relying on unified diff hunks.

;;; Code:

(require 'majutsu-diff)
(require 'ansi-color)
(require 'magit-section)
(require 'subr-x)

(declare-function majutsu-diff--set-left-margin "majutsu-diff" (width))

(defvar-local majutsu-color-words--number-width 0
  "Maximum digit width for color-words line numbers in current buffer.")

(defconst majutsu-color-words--file-header-re
  "^\\([A-Za-z].*\\) file \\(.+\\):$"
  "Regexp matching color-words file header lines.")

(defconst majutsu-color-words--ellipsis-re
  "^\\s-*\\.\\.\\.\\s-*$"
  "Regexp matching color-words hunk separators.")

(defconst majutsu-color-words--line-re
  "^\\s-*\\([0-9]+\\)?\\s-+\\([0-9]+\\)?\\s-*: "
  "Regexp matching color-words line-number columns.
The match ends after the colon and the single separator space that
jj emits; it must NOT consume further whitespace because that
whitespace is the source line's leading indentation.")

(defconst majutsu-color-words--debug-start "<<diff"
  "Prefix used by jj's `--color=debug' formatter labels.")

(defun majutsu-color-words--debug-open-boundary-p (open has-ansi)
  "Return non-nil when OPEN looks like a real debug marker start.
When HAS-ANSI is non-nil, reject literal `<<diff ...>>' snippets that are
embedded inside payload text (for example in code examples)."
  (let ((prev (and (> open (point-min)) (char-before open))))
    (if (not has-ansi)
        t
      (or (= open (point-min))
          (eq prev ?\n)
          (eq prev ?\r)
          (eq prev ?m)))))

(defun majutsu-color-words--debug-close-boundary-p (pos has-ansi)
  "Return non-nil when POS is at a valid debug marker close boundary.
POS must be immediately after a candidate closing `>>'."
  (save-excursion
    (goto-char pos)
    (let ((saw-escape nil))
      ;; In raw output (before ANSI stripping), a close can be followed by
      ;; color-reset escapes before the next marker/newline.
      (while (looking-at "\x1b\\[[0-9;]*m")
        (setq saw-escape t)
        (goto-char (match-end 0)))
      (let ((next (char-after)))
        (or (null next)
            (eq next ?\n)
            (eq next ?\r)
            ;; ANSI-aware path: `>>` + escapes + `<<` means wrapper boundary.
            (and saw-escape
                 (eq next ?<)
                 (eq (char-after (1+ (point))) ?<))
            ;; Fallback path for marker-only unit tests where no ANSI escapes are
            ;; present in the entire buffer.
            (and (not has-ansi)
                 (eq next ?<)
                 (eq (char-after (1+ (point))) ?<)))))))

(defun majutsu-color-words--debug-find-close (from has-ansi)
  "Find the end of a debug marker body starting at FROM.
Return point at the end of the closing `>>', or nil if not found.

The debug formatter emits `<<labels::data>>', where close markers are
followed by another marker, newline, or end-of-buffer.  Prefer those
boundaries to avoid consuming `>>' that may appear in source text."
  (save-excursion
    (goto-char from)
    (let (close)
      (while (and (not close)
                  (search-forward ">>" nil t))
        (if (majutsu-color-words--debug-close-boundary-p (point) has-ansi)
            (setq close (point))
          ;; `search-forward' does not find overlapping matches.  Step back
          ;; one char so payloads ending with `>' (which form `>>>') are
          ;; parsed correctly.
          (goto-char (1- (point)))))
      close)))

(defun majutsu-color-words--debug-apply-properties (beg end label)
  "Apply debug-derived text properties to BEG..END using LABEL.
LABEL is the space-separated label stack emitted by jj debug color mode."
  (let ((side (cond
               ((string-match-p "\\_<removed\\_>" label) 'removed)
               ((string-match-p "\\_<added\\_>" label) 'added)
               (t 'both))))
    (put-text-property beg end 'majutsu-color-words-debug-label label)
    (put-text-property beg end 'majutsu-color-words-debug-side side)
    (when (string-match-p "\\_<token\\_>" label)
      (put-text-property beg end 'majutsu-color-words-debug-token t))))

(defun majutsu-color-words--strip-debug-markers ()
  "Strip jj `--color=debug' wrappers while preserving payload text.
Each marker has the form `<<labels::data>>'.  This function removes the
marker syntax in-place and attaches debug-derived properties to DATA.

Return non-nil if at least one debug marker was processed."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil)
          (has-ansi (save-excursion
                      (goto-char (point-min))
                      (search-forward "\x1b[" nil t))))
      (while (search-forward majutsu-color-words--debug-start nil t)
        (let* ((open (match-beginning 0))
               (sep (save-excursion
                      (goto-char (match-end 0))
                      (search-forward "::" nil t))))
          (if (not (majutsu-color-words--debug-open-boundary-p open has-ansi))
              ;; Likely literal text inside payload; keep searching.
              nil
            (unless sep
              (goto-char (point-max)))
            (when sep
              (let ((close (majutsu-color-words--debug-find-close sep has-ansi)))
                (unless close
                  (goto-char (point-max)))
                (when close
                  (let* ((label (buffer-substring-no-properties
                                 (+ open 2) (- sep 2)))
                         (data-beg sep)
                         (data-end (- close 2)))
                    (majutsu-color-words--debug-apply-properties
                     data-beg data-end label)
                    ;; Remove marker wrappers but keep payload and face properties.
                    (delete-region data-end close)
                    (delete-region open data-beg)
                    (goto-char open)
                    (setq found t))))))))
      found)))

(defun majutsu-color-words--inline-invisible-p (pos)
  "Return non-nil when POS is in the hidden inline columns."
  (let ((inv (get-text-property pos 'invisible)))
    (or (eq inv 'majutsu-color-words-inline)
        (and (listp inv) (memq 'majutsu-color-words-inline inv)))))

(defun majutsu-color-words--collect-debug-token-spans (beg end)
  "Collect token spans annotated by debug labels in BEG..END.
Return a list of spans in the same shape as
`majutsu-color-words--collect-change-spans':

  (TYPE UNDERLINED-P START END)

where TYPE is `removed' or `added'.  Debug token spans are always treated
as word-level changes, so UNDERLINED-P is always non-nil."
  (let (spans)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((pos (point)))
          (if (majutsu-color-words--inline-invisible-p pos)
              (goto-char (or (next-single-property-change
                              pos 'invisible nil end)
                             end))
            (let* ((token (get-text-property pos 'majutsu-color-words-debug-token))
                   (side (get-text-property pos 'majutsu-color-words-debug-side))
                   (next-token (or (next-single-property-change
                                    pos 'majutsu-color-words-debug-token nil end)
                                   end))
                   (next-side (or (next-single-property-change
                                   pos 'majutsu-color-words-debug-side nil end)
                                  end))
                   (next-inv (or (next-single-property-change
                                  pos 'invisible nil end)
                                 end))
                   (next (min next-token next-side next-inv)))
              (when (and token (memq side '(removed added)))
                (push (list side t pos next) spans))
              (goto-char next))))))
    (nreverse spans)))

(defun majutsu-color-words--collect-debug-change-spans (beg end)
  "Collect change spans annotated by debug labels in BEG..END.
Return a list of (TYPE UNDERLINED-P START END) where TYPE is `removed' or
`added', and UNDERLINED-P is non-nil when the span is marked as a token."
  (let (spans)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((pos (point)))
          (if (majutsu-color-words--inline-invisible-p pos)
              (goto-char (or (next-single-property-change
                              pos 'invisible nil end)
                             end))
            (let* ((side (get-text-property pos 'majutsu-color-words-debug-side))
                   (token (get-text-property pos 'majutsu-color-words-debug-token))
                   (next-side (or (next-single-property-change
                                   pos 'majutsu-color-words-debug-side nil end)
                                  end))
                   (next-token (or (next-single-property-change
                                    pos 'majutsu-color-words-debug-token nil end)
                                   end))
                   (next-inv (or (next-single-property-change
                                  pos 'invisible nil end)
                                 end))
                   (next (min next-side next-token next-inv)))
              (when (memq side '(removed added))
                (push (list side (and token t) pos next) spans))
              (goto-char next))))))
    (nreverse spans)))

(defun majutsu-color-words--file-path (raw)
  "Return file path parsed from RAW header tail."
  (let ((path (substring-no-properties (string-trim raw))))
    (if (string-match "^\\(.*\\) (.* => \\(.+\\))$" path)
        (string-trim (match-string 2 path))
      path)))

(defun majutsu-color-words--line-info-from-text ()
  "Return color-words line info by parsing the current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (looking-at majutsu-color-words--line-re)
      (let* ((from-str (match-string 1))
             (to-str (match-string 2))
             (from (and from-str (string-to-number from-str)))
             (to (and to-str (string-to-number to-str)))
             (col (- (match-end 0) (line-beginning-position))))
        (and (or from to)
             (list :from-line from
                   :to-line to
                   :content-column col))))))

(defun majutsu-color-words--ellipsis-line-p ()
  "Return non-nil if current line is a color-words ellipsis separator."
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at majutsu-color-words--ellipsis-re)))

(defun majutsu-color-words--line-number-width (num)
  "Return digit width of NUM, or 0 when NUM is nil."
  (if num (length (number-to-string num)) 0))

(defun majutsu-color-words--scan-number-width ()
  "Compute maximum line number width in the current buffer."
  (let ((maxw 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (or (looking-at majutsu-color-words--file-header-re)
                    (majutsu-color-words--ellipsis-line-p))
          (when-let* ((info (majutsu-color-words--line-info-from-text)))
            (setq maxw (max maxw
                            (majutsu-color-words--line-number-width
                             (plist-get info :from-line))
                            (majutsu-color-words--line-number-width
                             (plist-get info :to-line))))))
        (forward-line 1)))
    maxw))

(defun majutsu-color-words--margin-width ()
  "Return left margin width based on `majutsu-color-words--number-width'."
  (if (> majutsu-color-words--number-width 0)
      (+ (* 2 majutsu-color-words--number-width) 3)
    0))

(defun majutsu-color-words--margin-string (info)
  "Return a propertized margin string for INFO."
  (let* ((from (plist-get info :from-line))
         (to (plist-get info :to-line))
         (width majutsu-color-words--number-width)
         (from-str (if from (number-to-string from) ""))
         (to-str (if to (number-to-string to) ""))
         (face (cond
                ((and from to) 'magit-diff-context)
                (from 'magit-diff-removed)
                (to 'magit-diff-added)
                (t 'magit-diff-context)))
         (text (format (format "%%%ss %%%ss " width width) from-str to-str)))
    (propertize text 'font-lock-face face)))

(defun majutsu-color-words--apply-line-margin (info)
  "Apply left margin rendering for INFO and hide inline columns."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (prefix (propertize " "
                             'display `((margin left-margin)
                                        ,(majutsu-color-words--margin-string info)))))
    (put-text-property bol eol 'line-prefix prefix)
    (put-text-property bol eol 'wrap-prefix prefix)
    (when-let* ((col (plist-get info :content-column))
                (start bol)
                (end (min eol (+ bol col))))
      (put-text-property start end 'invisible 'majutsu-color-words-inline))))

(defun majutsu-color-words-line-info-at-point ()
  "Return a plist describing the color-words line at point, or nil."
  (let ((bol (line-beginning-position)))
    (if-let* ((info (get-text-property bol 'majutsu-color-words-line-info)))
        info
      (majutsu-color-words--line-info-from-text))))

(defun majutsu-color-words--store-line-info (info)
  "Store INFO plist on the current line as text properties."
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (put-text-property bol eol 'majutsu-color-words-line-info info)))

(defun majutsu-color-words--wash-hunk (file heading)
  "Wrap a color-words hunk for FILE into a `jj-hunk' section."
  (let* ((start (point))
         (from-min nil)
         (from-max nil)
         (to-min nil)
         (to-max nil)
         (end-marker nil))
    (while (and (not (eobp))
                (not (looking-at majutsu-color-words--file-header-re))
                (not (majutsu-color-words--ellipsis-line-p)))
      (when-let* ((info (majutsu-color-words--line-info-from-text)))
        (majutsu-color-words--store-line-info info)
        (majutsu-color-words--apply-line-margin info)
        (let ((from (plist-get info :from-line))
              (to (plist-get info :to-line)))
          (when from
            (setq from-min (if from-min (min from-min from) from))
            (setq from-max (if from-max (max from-max from) from)))
          (when to
            (setq to-min (if to-min (min to-min to) to))
            (setq to-max (if to-max (max to-max to) to)))))
      (forward-line 1))
    (setq end-marker (copy-marker (point) t))
    (let* ((from-range (and from-min from-max
                            (list from-min (1+ (- from-max from-min)))))
           (to-range (and to-min to-max
                          (list to-min (1+ (- to-max to-min))))))
      (goto-char start)
      (magit-insert-section
          (jj-hunk (cons file (cons from-range to-range)) nil
                   :from-range from-range
                   :to-range to-range)
        (magit-insert-heading
          (propertize (or heading "...") 'font-lock-face 'magit-diff-hunk-heading))
        (goto-char end-marker)))
    (goto-char end-marker)))

(defun majutsu-color-words--wash-file ()
  "Wrap the current color-words file section in Magit sections."
  (when (looking-at majutsu-color-words--file-header-re)
    (let* ((header (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
           (raw (match-string 2))
           (file (majutsu-color-words--file-path raw)))
      (majutsu-diff--delete-line)
      (magit-insert-section (jj-file file nil :header header)
        (magit-insert-heading
          (propertize header 'font-lock-face 'magit-diff-file-heading))
        (let ((pending-heading "..."))
          (while (and (not (eobp))
                      (not (looking-at majutsu-color-words--file-header-re)))
            (cond
             ((majutsu-color-words--ellipsis-line-p)
              (setq pending-heading
                    (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
              (majutsu-diff--delete-line))
             (t
              (majutsu-color-words--wash-hunk file pending-heading)
              (setq pending-heading "...")))))
        (insert "\n"))))
  t)

(defun majutsu-color-words-wash-diffs (args)
  "Parse color-words diff output already inserted into the buffer.
ARGS are the formatting arguments used to produce the output."
  ;; With `--color=debug', jj emits `<<labels::data>>' wrappers around each
  ;; write call.  Strip those wrappers first while keeping payload text and
  ;; label-derived token metadata used for precise shadow-cursor mapping.
  (majutsu-color-words--strip-debug-markers)
  ;; Apply ANSI faces only after wrappers are stripped so debug marker parsing
  ;; can use raw escape-boundaries to distinguish literal `<<diff ...>>' text.
  (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face))
    (ansi-color-apply-on-region (point-min) (point-max)))
  (setq-local majutsu-color-words--number-width
              (majutsu-color-words--scan-number-width))
  (majutsu-diff--set-left-margin (majutsu-color-words--margin-width))
  (add-to-invisibility-spec 'majutsu-color-words-inline)
  (goto-char (point-min))
  (when (member "--stat" args)
    (majutsu-diff-wash-diffstat))
  (goto-char (point-min))
  (while (re-search-forward majutsu-color-words--file-header-re nil t)
    (goto-char (match-beginning 0))
    (majutsu-color-words--wash-file))
  (unless (bolp) (insert "\n")))

;;;; ANSI Face Classification

(defun majutsu-color-words--face-type (face)
  "Classify FACE as (TYPE . UNDERLINED-P) or nil.
TYPE is `removed' or `added'; UNDERLINED-P is non-nil when the face
includes `ansi-color-underline' (word unique to one side).

After `ansi-color-apply-on-region' with `ansi-color-apply-text-property-face':
  Non-underlined colored: (:foreground \"#RRGGBB\")     — a plist
  Underlined colored:     (ansi-color-underline (:foreground \"#RRGGBB\"))

Red-dominant foreground → removed; green-dominant → added."
  (when (consp face)
    (let ((underlined (memq 'ansi-color-underline face))
          (plist (cond
                  ;; Underlined: (ansi-color-underline (:foreground ...))
                  ((memq 'ansi-color-underline face)
                   (seq-find #'listp face))
                  ;; Non-underlined plist: (:foreground ...)
                  ((plist-get face :foreground)
                   face))))
      (when-let* ((fg (and plist (plist-get plist :foreground)))
                  (rgb (color-values fg)))
        (let ((type (if (> (car rgb) (cadr rgb)) 'removed 'added)))
          (cons type (and underlined t)))))))

;;;; Change Span Collection

(defun majutsu-color-words--collect-change-spans (beg end)
  "Collect change spans between BEG and END.
Return a list of (TYPE UNDERLINED-P START END) where TYPE is
`removed' or `added' and UNDERLINED-P indicates word-level uniqueness.
Spans are returned in buffer order."
  (let (spans)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((pos (point)))
          (if (majutsu-color-words--inline-invisible-p pos)
              (goto-char (or (next-single-property-change
                              pos 'invisible nil end)
                             end))
            (let* ((face (get-text-property pos 'font-lock-face))
                   (next-face (or (next-single-property-change
                                   pos 'font-lock-face nil end)
                                  end))
                   (next-inv (or (next-single-property-change
                                  pos 'invisible nil end)
                                 end))
                   (next (min next-face next-inv))
                   (classification (majutsu-color-words--face-type face)))
              (when classification
                (push (list (car classification) (cdr classification) pos next)
                      spans))
              (goto-char next))))))
    (nreverse spans)))

(defun majutsu-color-words--group-change-pairs (spans)
  "Group SPANS into change pairs for refinement.
Each SPAN is (TYPE UNDERLINED-P START END).
Return a list of (REMOVED-BEG REMOVED-END ADDED-BEG ADDED-END
                  REMOVED-TOKEN-SPANS ADDED-TOKEN-SPANS
                  REMOVED-NON-TOKEN-SPANS ADDED-NON-TOKEN-SPANS).
Adjacent removed and added spans are merged and paired.
TOKEN-SPANS are sub-lists of underlined-only (START . END) within
each side; NON-TOKEN-SPANS are the non-underlined spans, used for
precise shadow-cursor mapping on shared context.
Unpaired removed spans produce (BEG END nil nil TOKEN nil NON nil).
Unpaired added spans produce (nil nil BEG END nil TOKEN nil NON)."
  (let (pairs)
    (while spans
      (let ((s (pop spans)))
        (pcase (car s)
          ('removed
           (let ((rbeg (nth 2 s))
                 (rend (nth 3 s))
                 (r-ul (when (nth 1 s)
                         (list (cons (nth 2 s) (nth 3 s)))))
                 (r-non (unless (nth 1 s)
                          (list (cons (nth 2 s) (nth 3 s))))))
             ;; Merge consecutive removed spans.
             (while (and spans (eq (caar spans) 'removed))
               (let ((next (pop spans)))
                 (setq rend (nth 3 next))
                 (when (nth 1 next)
                   (push (cons (nth 2 next) (nth 3 next)) r-ul))
                 (unless (nth 1 next)
                   (push (cons (nth 2 next) (nth 3 next)) r-non))))
             ;; Pair with following added span(s) if present.
             (if (and spans (eq (caar spans) 'added))
                 (let* ((first-a (pop spans))
                        (abeg (nth 2 first-a))
                        (aend (nth 3 first-a))
                        (a-ul (when (nth 1 first-a)
                                (list (cons (nth 2 first-a) (nth 3 first-a)))))
                        (a-non (unless (nth 1 first-a)
                                 (list (cons (nth 2 first-a) (nth 3 first-a))))))
                   (while (and spans (eq (caar spans) 'added))
                     (let ((next (pop spans)))
                       (setq aend (nth 3 next))
                       (when (nth 1 next)
                         (push (cons (nth 2 next) (nth 3 next)) a-ul))
                       (unless (nth 1 next)
                         (push (cons (nth 2 next) (nth 3 next)) a-non))))
                   (push (list rbeg rend abeg aend
                               (nreverse r-ul) (nreverse a-ul)
                               (nreverse r-non) (nreverse a-non))
                         pairs))
               (push (list rbeg rend nil nil (nreverse r-ul) nil
                           (nreverse r-non) nil)
                     pairs))))
          ('added
           (let ((abeg (nth 2 s))
                 (aend (nth 3 s))
                 (a-ul (when (nth 1 s)
                         (list (cons (nth 2 s) (nth 3 s)))))
                 (a-non (unless (nth 1 s)
                          (list (cons (nth 2 s) (nth 3 s))))))
             (while (and spans (eq (caar spans) 'added))
               (let ((next (pop spans)))
                 (setq aend (nth 3 next))
                 (when (nth 1 next)
                   (push (cons (nth 2 next) (nth 3 next)) a-ul))
                 (unless (nth 1 next)
                   (push (cons (nth 2 next) (nth 3 next)) a-non))))
             (push (list nil nil abeg aend nil (nreverse a-ul)
                         nil (nreverse a-non))
                   pairs))))))
    (nreverse pairs)))

;;; _
(provide 'majutsu-color-words)
;;; majutsu-color-words.el ends here
