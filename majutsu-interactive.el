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

(require 'majutsu-base)
(require 'majutsu-diff)
(require 'majutsu-process)

(require 'cl-lib)
(require 'diff-mode)
(require 'magit-section)
(require 'transient)

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

(defface majutsu-interactive-selected-file
  '((t :background "#3a4f5f"))
  "Face for selected whole-file changes."
  :group 'majutsu-interactive)

;;; Selection Model

(defun majutsu-interactive-selection-available-p ()
  "Return non-nil when interactive selection is available."
  (derived-mode-p 'majutsu-diff-mode))

(defvar-local majutsu-interactive--selections nil
  "Hash table mapping hunk or file selection ids to selection specs.
A spec is `:all' for a complete hunk or file change, or (BEG . END) for a
region within a hunk.")

(defvar-local majutsu-interactive--overlays nil
  "List of overlays for selection visualization.")

(defun majutsu-interactive--hunk-id (section)
  "Return unique identifier for hunk SECTION."
  (oref section value))

(defun majutsu-interactive--file-for-hunk (section)
  "Return the file name for hunk SECTION."
  (let ((val (oref section value)))
    (if (consp val) (car val) val)))

(defun majutsu-interactive--file-id (file)
  "Return the selection identifier for whole-file FILE."
  (list :file file))

(defun majutsu-interactive--file-id-p (id)
  "Return non-nil when ID identifies a whole-file selection."
  (and (consp id) (eq (car id) :file)))

(defun majutsu-interactive--file-id-file (id)
  "Return the path stored in whole-file selection ID."
  (cadr id))

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

(defun majutsu-interactive--file-section-for-file (file)
  "Find the real diff file section for FILE, ignoring diffstat if possible."
  (let (fallback result)
    (when (and file magit-root-section)
      (magit-map-sections
       (lambda (section)
         (when (and (magit-section-match 'jj-file section)
                    (equal (oref section value) file))
           (unless fallback (setq fallback section))
           (when (and (not result) (oref section header))
             (setq result section))))
       magit-root-section))
    (or result fallback)))

(defun majutsu-interactive--file-section-hunks (file-section)
  "Return the hunk children of FILE-SECTION."
  (seq-filter (lambda (child) (magit-section-match 'jj-hunk child))
              (oref file-section children)))

(defun majutsu-interactive--whole-file-selection-allowed-p ()
  "Return non-nil when the active transient supports whole-file selections."
  (and (boundp 'transient-current-command)
       (memq transient-current-command
             '(majutsu-split majutsu-squash majutsu-restore))))

(defun majutsu-interactive--toggle-whole-file (file-section)
  "Toggle the whole-file selection represented by FILE-SECTION."
  (unless (majutsu-interactive--whole-file-selection-allowed-p)
    (user-error "Whole-file selections are supported by Split, Squash, and Restore"))
  (unless (majutsu-diff-file-metadata file-section)
    (user-error
     "Cannot verify this whole-file selection against structured metadata from jj; refresh the diff and try again"))
  (let* ((id (majutsu-interactive--file-id (oref file-section value)))
         (current (majutsu-interactive--get-selection id)))
    (majutsu-interactive--set-selection id (unless current :all))
    (majutsu-interactive--render-overlays)
    (message "%s whole-file change" (if current "Deselected" "Selected"))))

(defun majutsu-interactive-toggle-file ()
  "Toggle all hunks, or a whole-file change with no text hunks, at point."
  (interactive)
  (let (file-section)
    (magit-section-case
      (jj-hunk (setq file-section (oref it parent)))
      (jj-file (setq file-section it)))
    (let ((file (and file-section (oref file-section value))))
      (when (and file-section (magit-section-match 'jj-file file-section))
        (unless (majutsu-interactive--file-section-hunks file-section)
          (setq file-section
                (or (majutsu-interactive--file-section-with-hunks file)
                    (majutsu-interactive--file-section-for-file file))))
        (unless file-section
          (user-error "No file change at point"))
        (let ((hunks (majutsu-interactive--file-section-hunks file-section)))
          (if (null hunks)
              (majutsu-interactive--toggle-whole-file file-section)
            (let ((all-selected
                   (cl-every
                    (lambda (h)
                      (majutsu-interactive--get-selection
                       (majutsu-interactive--hunk-id h)))
                    hunks)))
              (dolist (hunk hunks)
                (majutsu-interactive--set-selection
                 (majutsu-interactive--hunk-id hunk)
                 (unless all-selected :all)))
              (majutsu-interactive--render-overlays)
              (message "%s all hunks in file"
                       (if all-selected "Deselected" "Selected")))))))))

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

(transient-define-suffix majutsu-interactive:select-hunk ()
  "Select hunk."
  :key "H"
  :description "Select hunk"
  :if 'majutsu-interactive-selection-available-p
  :transient t
  (interactive)
  (majutsu-interactive-toggle-hunk))

(transient-define-suffix majutsu-interactive:select-file ()
  "Select file."
  :key "F"
  :description "Select file"
  :if 'majutsu-interactive-selection-available-p
  :transient t
  (interactive)
  (majutsu-interactive-toggle-file))

(transient-define-suffix majutsu-interactive:select-region ()
  "Select region."
  :key "R"
  :description "Select region"
  :if 'majutsu-interactive-selection-available-p
  :transient t
  (interactive)
  (majutsu-interactive-toggle-region))

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
       (lambda (id spec)
         (if (majutsu-interactive--file-id-p id)
             (when-let* ((section (majutsu-interactive--find-file-section id)))
               (let ((ov (make-overlay (oref section start) (oref section end))))
                 (overlay-put ov 'face 'majutsu-interactive-selected-file)
                 (overlay-put ov 'evaporate t)
                 (push ov majutsu-interactive--overlays)))
           (when-let* ((section (majutsu-interactive--find-hunk-section id)))
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
                     (push ov majutsu-interactive--overlays)))))))))
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

(defun majutsu-interactive--find-file-section (file-id)
  "Find the real diff section identified by whole-file FILE-ID."
  (majutsu-interactive--file-section-for-file
   (majutsu-interactive--file-id-file file-id)))

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

(defun majutsu-interactive--hunkless-file-sections ()
  "Return metadata-backed file sections that have no text hunks.
The result follows the displayed diff order, rather than the unspecified
iteration order of the selection hash table."
  (let (result)
    (when magit-root-section
      (magit-map-sections
       (lambda (section)
         (when (and (magit-section-match 'jj-file section)
                    (null (majutsu-interactive--file-section-hunks section))
                    (majutsu-diff-file-metadata section))
           (push section result)))
       magit-root-section))
    (nreverse result)))

(defun majutsu-interactive--whole-file-selected-p (file-section)
  "Return non-nil when FILE-SECTION is selected as a whole file."
  (majutsu-interactive--get-selection
   (majutsu-interactive--file-id (oref file-section value))))

(defun majutsu-interactive--collect-selected-files ()
  "Return selected hunkless file sections in displayed diff order."
  (seq-filter #'majutsu-interactive--whole-file-selected-p
              (majutsu-interactive--hunkless-file-sections)))

(defun majutsu-interactive--collect-unselected-files ()
  "Return unselected hunkless file sections in displayed diff order."
  (seq-remove #'majutsu-interactive--whole-file-selected-p
              (majutsu-interactive--hunkless-file-sections)))

(defun majutsu-interactive--safe-relative-path (path)
  "Return PATH, or signal a user error if it is unsafe for the helper tool."
  (unless (and (stringp path)
               (not (string-empty-p path))
               (not (file-name-absolute-p path))
               (not (string= path "."))
               (not (string-match-p "\0" path))
               (not (member ".." (split-string path "/" t))))
    (user-error "Unsafe whole-file selection path: %S" path))
  path)

(defun majutsu-interactive--file-change (file-section)
  "Return a validated structured file change from FILE-SECTION metadata.
Never infer filesystem paths from rendered Git patch headers."
  (let ((metadata (majutsu-diff-file-metadata file-section)))
    (unless metadata
      (user-error
       "Cannot verify this whole-file selection against structured metadata from jj; refresh the diff and try again"))
    (let* ((status (plist-get metadata :status))
           (path (majutsu-interactive--safe-relative-path
                  (plist-get metadata :target)))
           (source (and (member status '("renamed" "copied"))
                        (majutsu-interactive--safe-relative-path
                         (plist-get metadata :source)))))
      (list :status status :source source :target path))))

(defun majutsu-interactive--file-operation (change)
  "Return the forward whole-file operation represented by CHANGE."
  (let ((status (plist-get change :status))
        (path (plist-get change :target))
        (source (plist-get change :source)))
      (pcase status
        ("added" (list :action 'add :path path))
        ("modified" (list :action 'modify :path path))
        ("removed" (list :action 'delete :path path))
        ("renamed" (list :action 'rename :source source :path path))
        ("copied" (list :action 'copy :source source :path path))
        (_ (user-error "Unsupported whole-file jj diff status: %S" status)))))

(defun majutsu-interactive--build-file-operations (file-sections)
  "Return forward whole-file operations for FILE-SECTIONS."
  (mapcar (lambda (section)
            (majutsu-interactive--file-operation
             (majutsu-interactive--file-change section)))
          file-sections))

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
         (prev-included-line nil))
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
                          ('meta prev-included-line)
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
                (setq prev-included-line (memq type '(context added removed)))
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
            (unless (eq type 'meta)
              (setq prev-included-line nil))))))
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

(defun majutsu-interactive-build-replay-plan-if-selected (&optional buffer mode)
  "Return a plan that reconstructs the editor's right tree from selections.
MODE is `selected' for Split and Squash, or `complement' for Restore.  A
selected plan starts from the editor's left tree and replays selected changes
from its initial right tree.  A complement plan starts from the initial right
tree and replays unselected changes from the left tree.  Both plans apply
their text patches forward; this keeps new-file and copy patches valid.

The returned plist contains :base, :payload-root, :patch, and :file-ops, or
nil when BUFFER has no selections."
  (with-current-buffer (or buffer (current-buffer))
    (when (majutsu-interactive--has-selections-p)
      (pcase (or mode 'selected)
        ('selected
         (list :base 'left
               :payload-root 'right
               :patch (and (majutsu-interactive--collect-selected-hunks)
                           (majutsu-interactive--build-patch))
               :file-ops
               (majutsu-interactive--build-file-operations
                (majutsu-interactive--collect-selected-files))))
        ('complement
         (list :base 'right
               :payload-root 'left
               :patch (majutsu-interactive--build-patch t t t)
               :file-ops
               (majutsu-interactive--build-file-operations
                (majutsu-interactive--collect-unselected-files))))
        (_ (error "Unknown replay plan mode: %S" mode))))))


(defun majutsu-interactive--fixup-patch (patch)
  "Fix hunk header line counts in PATCH using diff-mode."
  (with-temp-buffer
    (insert patch)
    (diff-mode)
    (diff-fixup-modifs (point-min) (point-max))
    (buffer-string)))

;;; Tool Invocation

(defun majutsu-interactive--make-operation-temp-dir ()
  "Create and return a private nearby directory for one jj operation."
  (make-nearby-temp-file "majutsu-interactive-" t))

(defun majutsu-interactive--write-patch (patch directory)
  "Write PATCH to a temporary file in DIRECTORY and return its path."
  (let ((file (expand-file-name "patch.diff" directory)))
    (with-temp-file file
      (insert patch))
    file))

(defun majutsu-interactive--script-path (root path)
  "Return a safely shell-quoted PATH beneath shell variable ROOT."
  (format "\"$%s\"/%s" root (shell-quote-argument path)))

(defun majutsu-interactive--write-applypatch-script (plan directory)
  "Write PLAN's applypatch helper script and return its path.
PLAN describes the editor right tree with :base (`left' or `right'),
:payload-root (`left' or `right'), a forward text patch, and explicit
whole-file operations.  Paths needed by FILE-OPS are snapshotted from the
payload root before the right tree is rebuilt.  Write the script in DIRECTORY."
  (let* ((base (plist-get plan :base))
         (payload-root (plist-get plan :payload-root))
         (file-ops (plist-get plan :file-ops))
         (script (expand-file-name "applypatch.sh" directory)))
    (unless (memq base '(left right))
      (error "Unknown replay plan base: %S" base))
    (unless (memq payload-root '(left right))
      (error "Unknown replay plan payload root: %S" payload-root))
    (with-temp-file script
      (insert "#!/bin/sh\n")
      (insert "# Majutsu applypatch helper\n")
      (insert "# Args: $1=left $2=right $3=patchfile\n")
      (insert "LEFT=\"$1\"\n")
      (insert "RIGHT=\"$2\"\n")
      (insert "PATCH=\"$3\"\n")
      (insert (format "PAYLOAD=\"$%s\"\n" (upcase (symbol-name payload-root))))
      (insert "majutsu_apply_patch() {\n")
      (insert "  [ -s \"$PATCH\" ] || return 0\n")
      (insert "  git apply --recount --unidiff-zero -v \"$PATCH\" 2>&1 && return 0\n")
      (insert "  git init -q || return $?\n")
      (insert "  git add -A || return $?\n")
      (insert "  git -c user.name=Majutsu -c user.email=majutsu.invalid commit -q -m base --allow-empty || return $?\n")
      (insert "  git apply --3way --recount -v \"$PATCH\" 2>&1\n")
      (insert "  STATUS=$?\n")
      (insert "  rm -rf -- .git || return $?\n")
      (insert "  return $STATUS\n")
      (insert "}\n")
      (when file-ops
        (insert "majutsu_remove() { rm -rf -- \"$1\"; }\n")
        (insert "majutsu_copy() {\n")
        (insert "  mkdir -p -- \"$(dirname -- \"$2\")\" || return $?\n")
        (insert "  cp -a -- \"$1\" \"$2\"\n")
        (insert "}\n")
        (insert "PRESERVED=$(mktemp -d \"${TMPDIR:-/tmp}/majutsu-interactive-right.XXXXXX\") || exit $?\n")
        (insert "majutsu_cleanup() { rm -rf -- \"$PRESERVED\"; }\n")
        (insert "trap majutsu_cleanup 0 1 2 3 15\n")
        (dolist (op file-ops)
          (when (memq (plist-get op :action) '(add modify rename copy))
            (let ((path (plist-get op :path)))
              (insert
               (format "majutsu_copy %s %s || exit $?\n"
                       (majutsu-interactive--script-path "PAYLOAD" path)
                       (majutsu-interactive--script-path "PRESERVED" path)))))))
      (when (eq base 'left)
        (insert "# Rebuild right from left state\n")
        (insert "find \"$RIGHT\" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + || exit $?\n")
        (insert "cp -a -- \"$LEFT\"/. \"$RIGHT\"/ || exit $?\n"))
      (insert "cd \"$RIGHT\" || exit $?\n")
      (insert "majutsu_apply_patch || exit $?\n")
      (dolist (op file-ops)
        (let* ((action (plist-get op :action))
               (path (plist-get op :path))
               (destination (majutsu-interactive--script-path "RIGHT" path))
               (preserved (majutsu-interactive--script-path "PRESERVED" path)))
          (pcase action
            ((or 'add 'modify)
             (insert (format "majutsu_remove %s || exit $?\n" destination))
             (insert (format "majutsu_copy %s %s || exit $?\n"
                             preserved destination)))
            ('delete
             (insert (format "majutsu_remove %s || exit $?\n" destination)))
            ('rename
             (insert
              (format "majutsu_remove %s || exit $?\n"
                      (majutsu-interactive--script-path
                       "RIGHT" (plist-get op :source))))
             (insert (format "majutsu_remove %s || exit $?\n" destination))
             (insert (format "majutsu_copy %s %s || exit $?\n"
                             preserved destination)))
            ('copy
             (insert (format "majutsu_remove %s || exit $?\n" destination))
             (insert (format "majutsu_copy %s %s || exit $?\n"
                             preserved destination)))
            (_ (error "Unknown whole-file operation: %S" action)))))
      (insert "exit 0\n"))
    (set-file-modes script #o755)
    script))

(defun majutsu-interactive--build-tool-config (patch-file plan directory)
  "Build jj --config arguments for applypatch PLAN with PATCH-FILE.
Write the helper script in DIRECTORY."
  (let* ((script (majutsu-interactive--write-applypatch-script plan directory))
         (script-path (majutsu-convert-filename-for-jj script))
         (patch-path (majutsu-convert-filename-for-jj patch-file)))
    (list
     "--config" (format "merge-tools.majutsu-applypatch.program=%s"
                        (shell-quote-argument script-path))
     "--config" (format "merge-tools.majutsu-applypatch.edit-args=[\"$left\",\"$right\",%s]"
                        (prin1-to-string patch-path)))))

;;; Pending Operation Flow

(defun majutsu-interactive--delete-operation-temp-dir (directory)
  "Best-effort removal of operation DIRECTORY."
  (when (and directory (file-exists-p directory))
    (ignore-errors (delete-directory directory t))))

(defun majutsu-interactive--temp-dir-process-sentinel (process event)
  "Run PROCESS's original sentinel for EVENT.
Remove its temporary directory when PROCESS exits or is signaled."
  (let ((original (process-get process 'majutsu-interactive-original-sentinel)))
    (unwind-protect
        (when original
          (funcall original process event))
      (when (memq (process-status process) '(exit signal))
        (majutsu-interactive--delete-operation-temp-dir
         (process-get process 'majutsu-interactive-temp-dir))))))

(defun majutsu-interactive--retain-temp-dir-for-process (process directory)
  "Keep DIRECTORY until asynchronous PROCESS exits."
  (if (and (processp process) (process-live-p process))
      (let ((original (process-sentinel process)))
        (process-put process 'majutsu-interactive-temp-dir directory)
        (process-put process 'majutsu-interactive-original-sentinel
                     original)
        (set-process-sentinel process
                              #'majutsu-interactive--temp-dir-process-sentinel)
        ;; The process can exit after the first liveness check but before the
        ;; replacement sentinel is installed.  In that case the old sentinel
        ;; may already have consumed the only exit event, so clean up here.
        (unless (process-live-p process)
          (ignore-errors (set-process-sentinel process original))
          (process-put process 'majutsu-interactive-temp-dir nil)
          (process-put process 'majutsu-interactive-original-sentinel nil)
          (majutsu-interactive--delete-operation-temp-dir directory))
        t)
    nil))

(defun majutsu-interactive-run-replay-plan (command args filesets plan)
  "Run jj COMMAND with ARGS and FILESETS using replay PLAN.
PLAN reconstructs the merge editor's right tree from its selected or
complemented text and whole-file changes."
  (let ((directory (majutsu-interactive--make-operation-temp-dir))
        retained)
    (unwind-protect
        (let* ((patch-file (majutsu-interactive--write-patch
                            (or (plist-get plan :patch) "") directory))
               (tool-config (majutsu-interactive--build-tool-config
                             patch-file plan directory))
               (args (append args
                             (list "-i" "--tool" "majutsu-applypatch")
                             tool-config))
               (full-args
                (cons command (majutsu-jj-append-filesets args filesets)))
               (process (majutsu-run-jj-with-editor full-args)))
          (setq retained
                (majutsu-interactive--retain-temp-dir-for-process
                 process directory))
          process)
      (unless retained
        (majutsu-interactive--delete-operation-temp-dir directory)))))

;;; _
(provide 'majutsu-interactive)
;;; majutsu-interactive.el ends here
