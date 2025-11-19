;;; majutsu-status.el --- Status and diff view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Status view and diff generation for Majutsu.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'magit-section)

;;; Classes

(defclass majutsu-status-section (magit-section) ())
(defclass majutsu-conflict-section (magit-section) ())
(defclass majutsu-conflict-file-section (magit-section)
  ((file :initarg :file)))
(defclass majutsu-diff-stat-section (magit-section) ())
(defclass majutsu-diff-section (magit-section) ())
(defclass majutsu-file-section (magit-section)
  ((file :initarg :file)))
(defclass majutsu-hunk-section (magit-section)
  ((file :initarg :file)
   (start :initarg :hunk-start)
   (header :initarg :header)))

;;; Status

(defun majutsu-log-insert-conflicts ()
  "Insert conflicted files section."
  (let ((output (majutsu--run-command "resolve" "--list")))
    (when (and output (not (string-empty-p output)))
      (magit-insert-section (majutsu-conflict-section)
        (magit-insert-heading "Unresolved Conflicts")
        (dolist (line (split-string output "\n" t))
          (let ((file (string-trim line)))
            (magit-insert-section (majutsu-conflict-file-section file nil :file file)
              (magit-insert-heading (propertize file 'face 'error))
              (insert "\n"))))
        (insert "\n")))))

(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (let ((status-output (majutsu--run-command "status")))
    (when (and status-output (not (string-empty-p status-output)))
      (magit-insert-section (majutsu-status-section)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)
        (insert "\n")
        ;; Analyze status and provide hints in the minibuffer
        (majutsu--analyze-status-for-hints status-output)))))

(defun majutsu--analyze-status-for-hints (status-output)
  "Analyze jj status output and provide helpful hints."
  (when (and status-output (not (string-empty-p status-output)))
    (cond
     ;; No changes
     ((string-match-p "The working copy is clean" status-output)
      (message "Working copy is clean - no changes to commit"))

     ;; Conflicts present
     ((string-match-p "There are unresolved conflicts" status-output)
      (message "💡 Resolve conflicts with 'jj resolve' or use diffedit (E/M)"))

     ;; Untracked files
     ((string-match-p "Untracked paths:" status-output)
      (message "💡 Add files with 'jj file track' or create .gitignore"))

     ;; Working copy changes
     ((string-match-p "Working copy changes:" status-output)
      (message "💡 Commit changes with 'jj commit' or describe with 'jj describe'")))))

;;; Diff

(defun majutsu-log-insert-diff ()
  "Insert jj diff with hunks into current buffer asynchronously."
  (let* ((section (magit-insert-section (majutsu-diff-section)
                    (magit-insert-heading "Working Copy Changes")
                    (insert "Loading diffs...\n")))
         (buf (current-buffer)))
    (majutsu--run-command-async
     '("diff" "--git")
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (if (and output (not (string-empty-p output)))
                   (progn
                     (majutsu--insert-diff-hunks output)
                     (insert "\n"))
                 (insert "No changes.\n"))
               ;; Update section end marker
               (set-marker (oref section end) (point)))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (insert (format "Error loading diffs: %s\n" err))
               (set-marker (oref section end) (point)))))))
     t)))

(defun majutsu--insert-diff-hunks (diff-output)
  "Parse and insert diff output as navigable hunk sections."
  (let ((lines (split-string diff-output "\n"))
        current-file
        file-section-content
        in-file-section)
    (dolist (line lines)
      (let ((clean-line (substring-no-properties line)))
        (cond
         ;; File header
         ((string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" clean-line)
          (let ((file-a (match-string 1 clean-line))
                (file-b (match-string 2 clean-line)))
            ;; Finish previous file section
            (when (and in-file-section current-file)
              (majutsu--insert-file-section current-file file-section-content))
            ;; Start new file section
            (setq current-file (or file-b file-a)
                  file-section-content (list line)
                  in-file-section t))) 
         ;; Collect lines for current file section
         (in-file-section
          (push line file-section-content))
         ;; Outside of any file section
         (t nil))))
    ;; Process final file section if any
    (when (and in-file-section current-file)
      (majutsu--insert-file-section current-file file-section-content))))

(defun majutsu--diff-line-matching-p (regexp lines)
  "Return non-nil if any string in LINES matches REGEXP."
  (seq-some (lambda (line) (and line (string-match-p regexp line))) lines))

(defun majutsu--diff-file-status (lines)
  "Infer the jj diff status for a file based on LINES."
  (cond
   ((majutsu--diff-line-matching-p "^new file" lines) "new file")
   ((majutsu--diff-line-matching-p "^deleted file" lines) "deleted")
   ((majutsu--diff-line-matching-p "^rename \\(from\\|to\\)" lines) "renamed")
   ((majutsu--diff-line-matching-p "^copy \\(from\\|to\\)" lines) "copied")
   (t "modified")))

(defun majutsu--diff-file-heading (file lines)
  "Return a formatted heading string for FILE using parsed LINES."
  (format "%-11s %s" (majutsu--diff-file-status lines) file))

(defun majutsu--insert-file-section (file lines)
  "Insert a file section with its hunks."
  ;; Loosely modeled after `magit-diff-insert-file-section' to leverage
  ;; Magit's section toggling behavior for large revisions.
  (let ((ordered-lines (nreverse lines)))
    (magit-insert-section _file-section (majutsu-file-section file nil :file file)
                          (magit-insert-heading
                            (propertize (majutsu--diff-file-heading file ordered-lines)
                                        'font-lock-face 'magit-diff-file-heading))
                          ;; Process the lines to find and insert hunks
                          (let ((hunk-lines nil)
                                (in-hunk nil)
                                (hunk-header nil))
                            (dolist (line ordered-lines)
                              (cond
                               ;; Hunk header
                               ((string-match "^@@ .* @@" line)
                                (when (and in-hunk hunk-header)
                                  (majutsu--insert-hunk-section file hunk-header (nreverse hunk-lines)))
                                (setq hunk-header line
                                      hunk-lines nil
                                      in-hunk t))
                               ;; Hunk content
                               (in-hunk
                                (push line hunk-lines))))
                            ;; Insert final hunk
                            (when (and in-hunk hunk-header)
                              (majutsu--insert-hunk-section file hunk-header (nreverse hunk-lines)))))))

(defun majutsu--insert-hunk-section (file header lines)
  "Insert a hunk section."
  (magit-insert-section _hunk-section (majutsu-hunk-section file nil :file file :header header)
                        (magit-insert-heading
                          (propertize header 'font-lock-face 'magit-diff-hunk-heading))
                        (dolist (line lines)
                          (insert (propertize line
                                              'font-lock-face
                                              (cond
                                               ((string-prefix-p "+" line) 'magit-diff-added)
                                               ((string-prefix-p "-" line) 'magit-diff-removed)
                                               (t 'magit-diff-context))))
                          (insert "\n"))))

(provide 'majutsu-status)
;;; majutsu-status.el ends here
