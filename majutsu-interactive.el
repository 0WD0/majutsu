;;; majutsu-interactive.el --- Interactive partial patching for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides Magit-style partial hunk staging for Jujutsu.
;; It enables region-based and hunk-based selection within diff buffers,
;; then applies those selections via jj split/squash/restore -i commands.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'magit-section)
(require 'transient)
(require 'majutsu-base)
(require 'majutsu-diff)
(require 'majutsu-process)

;;; Options

(defgroup majutsu-interactive nil
  "Interactive partial patching for Majutsu."
  :group 'majutsu)

(defface majutsu-interactive-selected-hunk
  '((t :background "#3a5f3a"))
  "Face for selected hunks."
  :group 'majutsu-interactive)

(defface majutsu-interactive-selected-region
  '((t :background "#5f3a5f"))
  "Face for selected regions within hunks."
  :group 'majutsu-interactive)

;;; Selection Model

(defun majutsu-interactive--selection-buffer ()
  "Return buffer to operate on for interactive selections."
  (let ((buf (and (boundp 'transient--original-buffer)
                  (buffer-live-p transient--original-buffer)
                  transient--original-buffer)))
    (or buf (current-buffer))))

(defun majutsu-interactive-selection-available-p ()
  "Return non-nil when interactive selection is available."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (derived-mode-p 'majutsu-diff-mode)))

(defvar-local majutsu-interactive--selections nil
  "Hash table mapping hunk-id to selection spec.
Selection spec is either `:all' for whole hunk, or (BEG . END) for region.")

(defvar-local majutsu-interactive--overlays nil
  "List of overlays for selection visualization.")

(defun majutsu-interactive--hunk-id (section)
  "Return unique identifier for hunk SECTION."
  (oref section value))

(defun majutsu-interactive--file-for-hunk (section)
  "Return the file name for hunk SECTION."
  (let ((val (oref section value)))
    (if (consp val) (car val) val)))

(defun majutsu-interactive--ensure-selections ()
  "Ensure selections hash table exists."
  (unless majutsu-interactive--selections
    (setq majutsu-interactive--selections (make-hash-table :test 'equal))))

(defun majutsu-interactive--get-selection (hunk-id)
  "Get selection for HUNK-ID."
  (majutsu-interactive--ensure-selections)
  (gethash hunk-id majutsu-interactive--selections))

(defun majutsu-interactive--set-selection (hunk-id spec)
  "Set selection SPEC for HUNK-ID."
  (majutsu-interactive--ensure-selections)
  (if spec
      (puthash hunk-id spec majutsu-interactive--selections)
    (remhash hunk-id majutsu-interactive--selections)))

(defun majutsu-interactive--has-selections-p ()
  "Return non-nil if there are any selections."
  (and majutsu-interactive--selections
       (> (hash-table-count majutsu-interactive--selections) 0)))

(defun majutsu-interactive-has-selections-p (&optional buffer)
  "Return non-nil if BUFFER has interactive selections."
  (with-current-buffer (or buffer (current-buffer))
    (majutsu-interactive--has-selections-p)))

(defun majutsu-interactive-build-patch-if-selected (&optional buffer invert include-all-files context-on-added)
  "Return patch for BUFFER if there are selections, otherwise nil.
When INVERT is non-nil, invert the selection within each hunk.
When INCLUDE-ALL-FILES is non-nil, include hunks from all files in invert mode.
When CONTEXT-ON-ADDED is non-nil, treat unselected added lines as context."
  (with-current-buffer (or buffer (current-buffer))
    (when (majutsu-interactive--has-selections-p)
      (majutsu-interactive--build-patch invert include-all-files context-on-added))))

;;; Selection Functions

(defun majutsu-interactive-toggle-hunk ()
  "Toggle selection of the hunk at point."
  (interactive)
  (when-let* ((section (magit-current-section)))
    (when (cl-typep section 'majutsu-hunk-section)
      (let* ((hunk-id (majutsu-interactive--hunk-id section))
             (current (majutsu-interactive--get-selection hunk-id)))
        (majutsu-interactive--set-selection
         hunk-id (if current nil :all))
        (majutsu-interactive--render-overlays)
        (message "%s hunk" (if current "Deselected" "Selected"))))))

(defun majutsu-interactive--file-section-with-hunks (file)
  "Find a jj-file section for FILE that contains hunks."
  (let (result)
    (when (and file magit-root-section)
      (magit-map-sections
       (lambda (section)
         (when (and (magit-section-match 'jj-file section)
                    (equal (oref section value) file)
                    (seq-some (lambda (child)
                                (magit-section-match 'jj-hunk child))
                              (oref section children)))
           (setq result section)))
       magit-root-section))
    result))

(defun majutsu-interactive-toggle-file ()
  "Toggle selection of all hunks in the file at point."
  (interactive)
  (let (file-section)
    (magit-section-case
      (jj-hunk (setq file-section (oref it parent)))
      (jj-file (setq file-section it)))
    (let ((file (and file-section (oref file-section value))))
      (when (and file-section (magit-section-match 'jj-file file-section))
        (unless (seq-some (lambda (child) (magit-section-match 'jj-hunk child))
                          (oref file-section children))
          (setq file-section (majutsu-interactive--file-section-with-hunks file)))
        (unless file-section
          (user-error "No hunks for file at point"))
        (let* ((hunks (oref file-section children))
               (all-selected (cl-every
                              (lambda (h)
                                (majutsu-interactive--get-selection
                                 (majutsu-interactive--hunk-id h)))
                              hunks)))
          (dolist (hunk hunks)
            (when (magit-section-match 'jj-hunk hunk)
              (majutsu-interactive--set-selection
               (majutsu-interactive--hunk-id hunk)
               (unless all-selected :all))))
          (majutsu-interactive--render-overlays)
          (message "%s all hunks in file"
                   (if all-selected "Deselected" "Selected")))))))

(defun majutsu-interactive--normalize-line-range (start end limit-start limit-end)
  "Return a line-aligned range between START and END.
The range is clamped to LIMIT-START and LIMIT-END."
  (let* ((rbeg (max start limit-start))
         (rend (min end limit-end)))
    (when (< rbeg rend)
      (save-excursion
        (goto-char rbeg)
        (setq rbeg (line-beginning-position))
        (goto-char (max (1- rend) rbeg))
        (setq rend (min limit-end (1+ (line-end-position))))))
    (and rbeg rend (< rbeg rend) (cons rbeg rend))))

(defun majutsu-interactive--ranges-merge (ranges)
  "Return merged RANGES (list of cons)."
  (let* ((sorted (sort (cl-copy-list ranges) (lambda (a b) (< (car a) (car b)))))
         (result nil))
    (dolist (range sorted)
      (let ((prev (car result)))
        (if (and prev (<= (car range) (cdr prev)))
            (setcdr prev (max (cdr prev) (cdr range)))
          (push (cons (car range) (cdr range)) result))))
    (nreverse result)))

(defun majutsu-interactive-toggle-region ()
  "Toggle selection of the region within the current hunk."
  (interactive)
  (unless (use-region-p)
    (user-error "No region active"))
  (magit-section-case
    (jj-hunk
     (let* ((hunk-id (majutsu-interactive--hunk-id it))
            (range (majutsu-interactive--normalize-line-range
                    (region-beginning) (region-end)
                    (oref it content) (oref it end)))
            (current (majutsu-interactive--get-selection hunk-id)))
       (unless range
         (user-error "Region is outside hunk"))
       (cond
        ((eq current :all)
         (majutsu-interactive--set-selection hunk-id (list range)))
        ((consp current)
         (let* ((ranges (if (and current (consp (car current))) current (list current)))
                (ranges (if (member range ranges)
                            (remove range ranges)
                          (cons range ranges)))
                (ranges (majutsu-interactive--ranges-merge ranges)))
           (majutsu-interactive--set-selection hunk-id (and ranges ranges))))
        (t
         (majutsu-interactive--set-selection hunk-id (list range))))
       (majutsu-interactive--render-overlays)
       (deactivate-mark)
       (message "Region selection updated")))))

(defun majutsu-interactive-clear ()
  "Clear all selections."
  (interactive)
  (setq majutsu-interactive--selections nil)
  (majutsu-interactive--render-overlays)
  (message "Cleared all selections"))

;;; Transient Selection Infixes

(defun majutsu-interactive--call-in-selection-buffer (fn)
  "Call FN in the selection buffer."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (funcall fn)))

(transient-define-suffix majutsu-interactive:select-hunk ()
  "Select hunk."
  :key "H"
  :description "Select hunk"
  :if 'majutsu-interactive-selection-available-p
  :transient t
  (interactive)
  (majutsu-interactive--call-in-selection-buffer #'majutsu-interactive-toggle-hunk))

(transient-define-suffix majutsu-interactive:select-file ()
  "Select file."
  :key "F"
  :description "Select file"
  :if 'majutsu-interactive-selection-available-p
  :transient t
  (interactive)
  (majutsu-interactive--call-in-selection-buffer #'majutsu-interactive-toggle-file))

(transient-define-suffix majutsu-interactive:select-region ()
  "Select region."
  :key "R"
  :description "Select region"
  :if 'majutsu-interactive-selection-available-p
  :transient t
  (interactive)
  (majutsu-interactive--call-in-selection-buffer #'majutsu-interactive-toggle-region))

;;; Overlay Rendering

(defun majutsu-interactive--clear-overlays ()
  "Remove all selection overlays."
  (mapc #'delete-overlay majutsu-interactive--overlays)
  (setq majutsu-interactive--overlays nil))

(defun majutsu-interactive--render-overlays ()
  "Render overlays for current selections."
  (majutsu-interactive--clear-overlays)
  (let ((selections majutsu-interactive--selections))
    (when selections
      (maphash
       (lambda (hunk-id spec)
         (when-let* ((section (majutsu-interactive--find-hunk-section hunk-id)))
           (cond
            ((eq spec :all)
             (let ((ov (make-overlay (oref section start) (oref section end))))
               (overlay-put ov 'face 'majutsu-interactive-selected-hunk)
               (overlay-put ov 'evaporate t)
               (push ov majutsu-interactive--overlays)))
            ((consp spec)
             (let ((ranges (if (and spec (consp (car spec))) spec (list spec))))
               (dolist (range ranges)
                 (let ((ov (make-overlay (car range) (cdr range))))
                   (overlay-put ov 'face 'majutsu-interactive-selected-region)
                   (overlay-put ov 'evaporate t)
                   (push ov majutsu-interactive--overlays))))))))
       selections))))

(defun majutsu-interactive--find-hunk-section (hunk-id)
  "Find hunk section with HUNK-ID in current buffer."
  (let ((result nil))
    (when magit-root-section
      (cl-labels ((walk (section)
                    (when (and (cl-typep section 'majutsu-hunk-section)
                               (equal (majutsu-interactive--hunk-id section) hunk-id))
                      (setq result section))
                    (dolist (child (oref section children))
                      (unless result (walk child)))))
        (walk magit-root-section)))
    result))

;;; Patch Generation

(defun majutsu-interactive--collect-selected-hunks ()
  "Collect all selected hunks with their selection specs.
Returns list of (FILE-SECTION HUNK-SECTION SPEC)."
  (let ((result nil))
    (when majutsu-interactive--selections
      (maphash
       (lambda (hunk-id spec)
         (when-let* ((hunk (majutsu-interactive--find-hunk-section hunk-id)))
           (push (list (oref hunk parent) hunk spec) result)))
       majutsu-interactive--selections))
    (nreverse result)))

(defun majutsu-interactive--collect-hunks-by-file ()
  "Return hash table of FILE-SECTION to list of HUNK-SECTIONS."
  (let ((by-file (make-hash-table :test 'eq)))
    (when magit-root-section
      (magit-map-sections
       (lambda (section)
         (when (magit-section-match 'jj-hunk section)
           (let ((file (oref section parent)))
             (push section (gethash file by-file)))))
       magit-root-section))
    (maphash (lambda (file hunks)
               (puthash file (nreverse hunks) by-file))
             by-file)
    by-file))

(defun majutsu-interactive--hunk-header (section)
  "Extract hunk header line from SECTION, including newline."
  (buffer-substring-no-properties
   (oref section start)
   (oref section content)))

(defun majutsu-interactive--parse-hunk-header (header)
  "Parse unified diff HEADER into (OLD-START OLD-LEN NEW-START NEW-LEN SUFFIX).
Return nil when HEADER is not a standard @@ header."
  (let ((line (string-trim-right header)))
    (when (string-match
           "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@\\(.*\\)$"
           line)
      (list (string-to-number (match-string 1 line))
            (string-to-number (or (match-string 2 line) "1"))
            (string-to-number (match-string 3 line))
            (string-to-number (or (match-string 4 line) "1"))
            (or (match-string 5 line) "")))))

(defun majutsu-interactive--format-hunk-range (start len)
  "Format a hunk range from START and LEN."
  (if (= len 1)
      (format "%d" start)
    (format "%d,%d" start len)))

(defun majutsu-interactive--format-hunk-header (old-start old-len new-start new-len suffix)
  "Format a unified diff hunk header."
  (format "@@ -%s +%s @@%s\n"
          (majutsu-interactive--format-hunk-range old-start old-len)
          (majutsu-interactive--format-hunk-range new-start new-len)
          (or suffix "")))

(defun majutsu-interactive--line-selected-p (bol eol ranges)
  "Return non-nil if [BOL,EOL] overlaps any RANGES."
  (seq-some (lambda (range)
              (and (< (car range) eol)
                   (> (cdr range) bol)))
            ranges))

(defun majutsu-interactive--hunk-lines (section)
  "Return hunk body lines from SECTION with positions and types.
Each entry is (TEXT TYPE BOL EOL)."
  (let ((lines nil)
        (start (oref section content))
        (end (oref section end)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((bol (line-beginning-position))
               (eol (min (1+ (line-end-position)) end))
               (text (buffer-substring-no-properties bol eol))
               (type (pcase (and (> (length text) 0) (aref text 0))
                       (?\s 'context)
                       (?- 'removed)
                       (?+ 'added)
                       (?\\ 'meta)
                       (_ 'meta))))
          (push (list text type bol eol) lines))
        (forward-line 1)))
    (nreverse lines)))

(defun majutsu-interactive--build-hunk-patch (section spec &optional invert context-on-added)
  "Build a patch hunk for SECTION using SPEC and INVERT.
SPEC is `:all' or list of (BEG . END) ranges.
When INVERT is non-nil, invert selection for change lines.
When CONTEXT-ON-ADDED is non-nil, treat unselected added lines as context.
Return a hunk string or nil when no change lines remain."
  (let* ((header (majutsu-interactive--hunk-header section))
         (parsed (majutsu-interactive--parse-hunk-header header))
         (lines (majutsu-interactive--hunk-lines section))
         (ranges (cond
                  ((eq spec :all) nil)
                  ((consp spec) (if (and spec (consp (car spec))) spec (list spec)))
                  (t nil)))
         (selected-lines nil)
         (old-skip 0)
         (new-skip 0)
         (old-len 0)
         (new-len 0)
         (has-change nil)
         (started nil)
         (prev-included-change nil))
    (dolist (line lines)
      (pcase-let ((`(,text ,type ,bol ,eol) line))
        (let* ((selected (if ranges
                             (majutsu-interactive--line-selected-p bol eol ranges)
                           t))
               (include-change (if invert (not selected) selected))
               (convert-added (and context-on-added (eq type 'added) (not include-change)))
               (convert-removed (and (not context-on-added) (eq type 'removed) (not include-change)))
               (include (pcase type
                          ('context t)
                          ('added (or include-change convert-added))
                          ('removed (or include-change convert-removed))
                          ('meta prev-included-change)
                          (_ nil)))
               (old-inc (pcase type
                          ('context 1)
                          ('removed 1)
                          (_ 0)))
               (new-inc (pcase type
                          ('context 1)
                          ('added 1)
                          (_ 0))))
          (if include
              (progn
                (unless started
                  (setq started t))
                (when (and (memq type '(added removed)) include-change)
                  (setq has-change t))
                (setq prev-included-change (and (memq type '(added removed)) include-change))
                (cond
                 (convert-added
                  (setq old-len (1+ old-len))
                  (setq new-len (1+ new-len))
                  (push (concat " " (substring text 1)) selected-lines))
                 (convert-removed
                  (setq old-len (1+ old-len))
                  (setq new-len (1+ new-len))
                  (push (concat " " (substring text 1)) selected-lines))
                 (t
                  (setq old-len (+ old-len old-inc))
                  (setq new-len (+ new-len new-inc))
                  (push text selected-lines))))
            (when (and (not started) (memq type '(context added removed)))
              (setq old-skip (+ old-skip old-inc))
              (setq new-skip (+ new-skip new-inc)))
            (setq prev-included-change nil)))))
    (when (and has-change selected-lines)
      (let* ((body (mapconcat #'identity (nreverse selected-lines) ""))
             (hunk-header
              (if parsed
                  (pcase-let ((`(,old-start ,_old-len ,new-start ,_new-len ,suffix) parsed))
                    (majutsu-interactive--format-hunk-header
                     (+ old-start old-skip)
                     old-len
                     (+ new-start new-skip)
                     new-len
                     suffix))
                header)))
        (concat hunk-header body)))))

(defun majutsu-interactive--file-is-new-p (file-section)
  "Return non-nil if FILE-SECTION represents a new file."
  (let ((header (oref file-section header)))
    (string-match-p "^--- /dev/null$" header)))

(defun majutsu-interactive--build-file-patch (file-section hunks-with-specs &optional invert context-on-added)
  "Build patch string for FILE-SECTION with HUNKS-WITH-SPECS.
HUNKS-WITH-SPECS is list of (HUNK-SECTION SPEC [INVERT]).
When INVERT is non-nil, invert selected hunks by default.
When CONTEXT-ON-ADDED is non-nil, treat unselected added lines as context.
Note: context-on-added is disabled for new files (--- /dev/null)."
  (let ((header (oref file-section header))
        (hunk-patches nil)
        ;; New files cannot have context lines, so disable context-on-added
        (effective-context-on-added (and context-on-added
                                         (not (majutsu-interactive--file-is-new-p file-section)))))
    (dolist (hs hunks-with-specs)
      (let* ((hunk (nth 0 hs))
             (spec (nth 1 hs))
             (hunk-invert (if (> (length hs) 2) (nth 2 hs) invert))
             (hunk-patch (majutsu-interactive--build-hunk-patch
                          hunk spec hunk-invert effective-context-on-added)))
        (when hunk-patch
          (push hunk-patch hunk-patches))))
    (when hunk-patches
      (concat header
              (mapconcat #'identity (nreverse hunk-patches) "")))))


(defun majutsu-interactive--build-patch (&optional invert include-all-files context-on-added)
  "Build complete patch from current selections.
When INVERT is non-nil, select non-matching change lines.
When INCLUDE-ALL-FILES is non-nil, include hunks from all files in invert mode.
When CONTEXT-ON-ADDED is non-nil, treat unselected added lines as context.
Returns patch string or nil if no selections."
  (let* ((selected (majutsu-interactive--collect-selected-hunks))
         (by-file (make-hash-table :test 'eq)))
    (unless selected
      (user-error "No hunks selected"))
    ;; Group by file section
    (dolist (item selected)
      (let ((file (car item))
            (hunk (cadr item))
            (spec (caddr item)))
        (push (list hunk spec) (gethash file by-file))))
    ;; Build patches per file
    (let* ((patches nil)
           (all-hunks-by-file (and invert (majutsu-interactive--collect-hunks-by-file))))
      (cond
       ((not invert)
        (maphash
         (lambda (file hunks)
           (let ((hunks (nreverse hunks)))
             (when-let* ((patch (majutsu-interactive--build-file-patch
                                 file hunks invert context-on-added)))
               (push patch patches))))
         by-file))
       (include-all-files
        (maphash
         (lambda (file all-hunks)
           (let* ((selected (gethash file by-file))
                  (spec-by-hunk (make-hash-table :test 'eq))
                  (expanded nil))
             (dolist (hs selected)
               (puthash (car hs) (cadr hs) spec-by-hunk))
             ;; Keep all unselected hunks; invert only selected hunks.
             (dolist (hunk all-hunks)
               (let ((spec (gethash hunk spec-by-hunk)))
                 (if spec
                     (push (list hunk spec t) expanded)
                   (push (list hunk :all nil) expanded))))
             (setq expanded (nreverse expanded))
             (when-let* ((patch (majutsu-interactive--build-file-patch
                                 file expanded invert context-on-added)))
               (push patch patches))))
         all-hunks-by-file))
       (t
        (maphash
         (lambda (file hunks)
           (let* ((spec-by-hunk (make-hash-table :test 'eq))
                  (all-hunks (or (and all-hunks-by-file
                                      (gethash file all-hunks-by-file))
                                 (mapcar #'car hunks)))
                  (expanded nil))
             (dolist (hs hunks)
               (puthash (car hs) (cadr hs) spec-by-hunk))
             ;; Expand to all hunks in selected files.
             (dolist (hunk all-hunks)
               (let ((spec (gethash hunk spec-by-hunk)))
                 (if spec
                     (push (list hunk spec t) expanded)
                   (push (list hunk :all nil) expanded))))
             (setq expanded (nreverse expanded))
             (when-let* ((patch (majutsu-interactive--build-file-patch
                                 file expanded invert context-on-added)))
               (push patch patches))))
         by-file)))
      (when patches
        (majutsu-interactive--fixup-patch
         (mapconcat #'identity (nreverse patches) ""))))))


(defun majutsu-interactive--fixup-patch (patch)
  "Fix hunk header line counts in PATCH using diff-mode."
  (with-temp-buffer
    (insert patch)
    (diff-mode)
    (diff-fixup-modifs (point-min) (point-max))
    (buffer-string)))

;;; Tool Invocation

(defvar majutsu-interactive--temp-dir nil
  "Temporary directory for patch files.")

(defun majutsu-interactive--temp-dir ()
  "Return or create temporary directory."
  (unless (and majutsu-interactive--temp-dir
               (file-directory-p majutsu-interactive--temp-dir))
    (setq majutsu-interactive--temp-dir
          (make-temp-file "majutsu-interactive-" t)))
  majutsu-interactive--temp-dir)

(defun majutsu-interactive--write-patch (patch)
  "Write PATCH to a temporary file and return its path."
  (let ((file (expand-file-name "patch.diff" (majutsu-interactive--temp-dir))))
    (with-temp-file file
      (insert patch))
    file))

(defun majutsu-interactive--write-applypatch-script (reverse)
  "Write the applypatch helper script and return its path.
When REVERSE is non-nil, reset $right to $left state first, then apply patch."
  (let ((script (expand-file-name "applypatch.sh" (majutsu-interactive--temp-dir))))
    (with-temp-file script
      (insert "#!/bin/sh\n")
      (insert "# Majutsu applypatch helper\n")
      (insert "# Args: $1=left $2=right $3=patchfile\n")
      (insert "LEFT=\"$1\"\n")
      (insert "RIGHT=\"$2\"\n")
      (insert "PATCH=\"$3\"\n")
      (when reverse
        ;; For split/squash: reset $right to $left (parent) state first
        ;; Then apply the patch containing remaining content
        (insert "# Reset right to left state\n")
        (insert "rm -rf \"$RIGHT\"/* 2>/dev/null\n")
        (insert "rm -rf \"$RIGHT\"/.[!.]* 2>/dev/null\n")
        (insert "cp -a \"$LEFT\"/. \"$RIGHT\"/ 2>/dev/null || true\n"))
      (insert "cd \"$RIGHT\"\n")
      ;; Try git apply with --recount which recalculates line numbers
      (insert "git apply --recount --unidiff-zero -v \"$PATCH\" 2>&1 && exit 0\n")
      ;; Fallback: init git repo for 3way merge
      (insert "git init -q 2>/dev/null\n")
      (insert "git add -A 2>/dev/null\n")
      (insert "git commit -q -m 'base' --allow-empty 2>/dev/null\n")
      (insert "git apply --3way --recount -v \"$PATCH\" 2>&1\n")
      (insert "EXIT=$?\n")
      (insert "rm -rf .git 2>/dev/null\n")
      (insert "exit $EXIT\n"))
    (set-file-modes script #o755)
    script))

(defun majutsu-interactive--build-tool-config (patch-file reverse)
  "Build jj --config arguments for applypatch tool with PATCH-FILE.
When REVERSE is non-nil, the script will apply the patch in reverse."
  (let ((script (majutsu-interactive--write-applypatch-script reverse)))
    (list
     "--config" (format "merge-tools.majutsu-applypatch.program=%s"
                        (shell-quote-argument script))
     "--config" (format "merge-tools.majutsu-applypatch.edit-args=[\"$left\",\"$right\",%s]"
                        (prin1-to-string patch-file)))))

;;; Pending Operation Flow

(defun majutsu-interactive-run-with-patch (command args patch &optional reverse)
  "Run jj COMMAND with ARGS, applying PATCH via custom tool.
If REVERSE is non-nil, apply the patch in reverse using git apply -R."
  (let* ((patch-file (majutsu-interactive--write-patch patch))
         (tool-config (majutsu-interactive--build-tool-config patch-file reverse))
         (full-args (append (list command)
                            args
                            (list "-i" "--tool" "majutsu-applypatch")
                            tool-config)))
    (majutsu-run-jj-with-editor full-args)))

;;; _
(provide 'majutsu-interactive)
;;; majutsu-interactive.el ends here
