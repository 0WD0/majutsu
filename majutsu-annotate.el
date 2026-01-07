;;; majutsu-annotate.el --- Blame support for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line, using `jj file annotate'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'majutsu-base)
(require 'majutsu-jj)
(require 'majutsu-process)

(declare-function majutsu-diff-revset "majutsu-diff" (revset &optional args range filesets))

;;; Options

(defgroup majutsu-annotate nil
  "Annotate (blame) support for Majutsu."
  :group 'majutsu)

(defcustom majutsu-annotate-heading-format "%-8a %C %s\n"
  "Format for annotation headings.
The following %-specs are supported:
  %h  change-id (short)
  %a  author
  %C  committer time
  %s  summary (first line of description)"
  :type 'string
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-time-format "%F %H:%M"
  "Format for time strings in annotation headings."
  :type 'string
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-read-only t
  "Whether to initially make the annotated buffer read-only."
  :type 'boolean
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-mode-lighter " Annotate"
  "Mode-line lighter for `majutsu-annotate-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'majutsu-annotate)

;;; Faces

(defface majutsu-annotate-highlight
  '((((class color) (background light))
     :extend t :background "grey80" :foreground "black")
    (((class color) (background dark))
     :extend t :background "grey25" :foreground "white"))
  "Face used for highlighting when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-heading
  '((t :extend t :inherit majutsu-annotate-highlight
       :weight normal :slant normal))
  "Face used for annotation headings."
  :group 'majutsu-faces)

(defface majutsu-annotate-hash '((t nil))
  "Face used for change-ids when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-name '((t nil))
  "Face used for author names when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-date '((t nil))
  "Face used for dates when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-summary '((t nil))
  "Face used for commit summaries when annotating."
  :group 'majutsu-faces)

;;; Variables

(defvar-local majutsu-annotate-buffer-read-only nil)
(defvar-local majutsu-annotate--chunks nil
  "List of annotation chunks in current buffer.")
(defvar-local majutsu-annotate--previous-chunk nil)

;;; Chunk structure

(cl-defstruct majutsu-annotate-chunk
  change-id author timestamp line-beg line-end description)

;;; Keymaps

(defvar-keymap majutsu-annotate-mode-map
  :doc "Keymap for `majutsu-annotate-mode'."
  "C-c C-q" #'majutsu-annotate-quit)

(defvar-keymap majutsu-annotate-read-only-mode-map
  :doc "Keymap for `majutsu-annotate-read-only-mode'."
  "RET" #'majutsu-annotate-show-commit
  "p"   #'majutsu-annotate-previous-chunk
  "n"   #'majutsu-annotate-next-chunk
  "b"   #'majutsu-annotate-addition
  "q"   #'majutsu-annotate-quit
  "M-w" #'majutsu-annotate-copy-hash)

;;; Modes

(define-minor-mode majutsu-annotate-mode
  "Display annotation information inline."
  :lighter majutsu-annotate-mode-lighter
  :interactive nil
  (cond (majutsu-annotate-mode
         (add-hook 'post-command-hook #'majutsu-annotate-goto-chunk-hook nil t)
         (setq majutsu-annotate-buffer-read-only buffer-read-only)
         (when majutsu-annotate-read-only
           (read-only-mode 1)))
        (t
         (remove-hook 'post-command-hook #'majutsu-annotate-goto-chunk-hook t)
         (unless majutsu-annotate-buffer-read-only
           (read-only-mode -1))
         (majutsu-annotate-read-only-mode -1)
         (majutsu-annotate--remove-overlays)
         (setq majutsu-annotate--chunks nil))))

(define-minor-mode majutsu-annotate-read-only-mode
  "Provide keybindings for Majutsu-Annotate mode.
\\{majutsu-annotate-read-only-mode-map}")

(defun majutsu-annotate-toggle-read-only ()
  "Toggle read-only mode for annotation."
  (majutsu-annotate-read-only-mode (if buffer-read-only 1 -1)))

(defun majutsu-annotate-goto-chunk-hook ()
  "Hook to run when point moves to a different chunk."
  (when-let* ((chunk (majutsu-annotate-chunk-at (point))))
    (unless (eq chunk majutsu-annotate--previous-chunk)
      (setq majutsu-annotate--previous-chunk chunk))))

;;; Parsing

(defun majutsu-annotate--parse-output (output)
  "Parse OUTPUT from `jj file annotate' into chunks."
  (let ((lines (split-string output "\n" t))
        chunks current-chunk current-change-id)
    (dolist (line lines)
      ;; Format: "CHANGE_ID AUTHOR    DATE       LINE_NUM: CONTENT"
      ;; Example: "qpvuntsm foo      2001-02-03 08:05:08    1: line1"
      (when (string-match
             "^\\([a-z]+\\)\\s-+\\([^ ]+\\)\\s-+\\([0-9-]+\\s-+[0-9:]+\\)\\s-+\\([0-9]+\\):"
             line)
        (let ((change-id (match-string 1 line))
              (author (match-string 2 line))
              (timestamp (match-string 3 line))
              (line-num (string-to-number (match-string 4 line))))
          (if (and current-chunk (equal change-id current-change-id))
              ;; Extend current chunk
              (setf (majutsu-annotate-chunk-line-end current-chunk) line-num)
            ;; Start new chunk
            (when current-chunk
              (push current-chunk chunks))
            (setq current-change-id change-id)
            (setq current-chunk
                  (make-majutsu-annotate-chunk
                   :change-id change-id
                   :author author
                   :timestamp timestamp
                   :line-beg line-num
                   :line-end line-num))))))
    (when current-chunk
      (push current-chunk chunks))
    (nreverse chunks)))

(defun majutsu-annotate--get-description (change-id)
  "Get the description for CHANGE-ID."
  (string-trim
   (majutsu-jj-string "log" "-r" change-id "--no-graph" "-T" "description")))

;;; Overlays

(defun majutsu-annotate--line-beginning-position (line)
  "Return position of beginning of LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun majutsu-annotate--make-overlays (chunks)
  "Create overlays for CHUNKS."
  (save-excursion
    (save-restriction
      (widen)
      (dolist (chunk chunks)
        (let* ((beg (majutsu-annotate--line-beginning-position
                     (majutsu-annotate-chunk-line-beg chunk)))
               (end (majutsu-annotate--line-beginning-position
                     (1+ (majutsu-annotate-chunk-line-end chunk))))
               (ov (make-overlay beg end)))
          (overlay-put ov 'majutsu-annotate-chunk chunk)
          (overlay-put ov 'before-string
                       (majutsu-annotate--format-heading chunk))
          ;; Highlight first line
          (let ((hl-ov (make-overlay beg (save-excursion
                                           (goto-char beg)
                                           (line-end-position)))))
            (overlay-put hl-ov 'majutsu-annotate-highlight t)
            (overlay-put hl-ov 'font-lock-face 'majutsu-annotate-highlight)))))))

(defun majutsu-annotate--format-heading (chunk)
  "Format heading string for CHUNK."
  (let* ((change-id (majutsu-annotate-chunk-change-id chunk))
         (author (majutsu-annotate-chunk-author chunk))
         (timestamp (majutsu-annotate-chunk-timestamp chunk))
         (desc (or (majutsu-annotate-chunk-description chunk)
                   (let ((d (majutsu-annotate--get-description change-id)))
                     (setf (majutsu-annotate-chunk-description chunk) d)
                     d)))
         (summary (car (split-string desc "\n"))))
    (propertize
     (format-spec majutsu-annotate-heading-format
                  `((?h . ,(propertize change-id 'font-lock-face 'majutsu-annotate-hash))
                    (?a . ,(propertize author 'font-lock-face 'majutsu-annotate-name))
                    (?C . ,(propertize timestamp 'font-lock-face 'majutsu-annotate-date))
                    (?s . ,(propertize summary 'font-lock-face 'majutsu-annotate-summary))))
     'font-lock-face 'majutsu-annotate-heading)))

(defun majutsu-annotate--remove-overlays ()
  "Remove all annotation overlays."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (or (overlay-get ov 'majutsu-annotate-chunk)
                (overlay-get ov 'majutsu-annotate-highlight))
        (delete-overlay ov)))))

;;; Chunk navigation

(defun majutsu-annotate-chunk-at (pos)
  "Return the annotation chunk at POS."
  (seq-some (lambda (ov) (overlay-get ov 'majutsu-annotate-chunk))
            (overlays-at pos)))

(defun majutsu-annotate-current-chunk ()
  "Return the annotation chunk at point."
  (majutsu-annotate-chunk-at (point)))

;;; Commands

;;;###autoload
(defun majutsu-annotate-addition (&optional revision)
  "Annotate the current file showing when each line was added.
With prefix argument, prompt for REVISION."
  (interactive
   (list (and current-prefix-arg
              (majutsu-read-revset "Annotate from revision"))))
  (unless (or buffer-file-name
              (bound-and-true-p majutsu-buffer-blob-path))
    (user-error "Buffer is not visiting a file"))
  (let* ((root (majutsu-toplevel))
         (file (or (and (bound-and-true-p majutsu-buffer-blob-path)
                        majutsu-buffer-blob-path)
                   (file-relative-name buffer-file-name root)))
         (rev (or revision
                  (and (bound-and-true-p majutsu-buffer-blob-revision)
                       majutsu-buffer-blob-revision)
                  "@"))
         (default-directory root)
         (output (majutsu-jj-string "file" "annotate" "-r" rev file)))
    (when (string-empty-p output)
      (user-error "No annotation output"))
    (let ((chunks (majutsu-annotate--parse-output output)))
      (unless majutsu-annotate-mode
        (majutsu-annotate-mode 1))
      (majutsu-annotate--remove-overlays)
      (setq majutsu-annotate--chunks chunks)
      (majutsu-annotate--make-overlays chunks)
      (when majutsu-annotate-read-only
        (majutsu-annotate-read-only-mode 1))
      (message "Annotating...done"))))

(defun majutsu-annotate-quit ()
  "Turn off Majutsu-Annotate mode."
  (interactive)
  (majutsu-annotate-mode -1))

(defun majutsu-annotate-next-chunk ()
  "Move to the next annotation chunk."
  (interactive)
  (if-let* ((next (next-single-char-property-change
                   (point) 'majutsu-annotate-chunk)))
      (goto-char next)
    (user-error "No more chunks")))

(defun majutsu-annotate-previous-chunk ()
  "Move to the previous annotation chunk."
  (interactive)
  (if-let* ((prev (previous-single-char-property-change
                   (point) 'majutsu-annotate-chunk)))
      (goto-char prev)
    (user-error "No more chunks")))

(defun majutsu-annotate-show-commit ()
  "Show the commit for the current chunk."
  (interactive)
  (if-let* ((chunk (majutsu-annotate-current-chunk)))
      (majutsu-diff-revset (majutsu-annotate-chunk-change-id chunk))
    (user-error "No chunk at point")))

(defun majutsu-annotate-copy-hash ()
  "Copy the change-id of the current chunk to the kill ring."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (if-let* ((chunk (majutsu-annotate-current-chunk)))
        (kill-new (message "%s" (majutsu-annotate-chunk-change-id chunk)))
      (user-error "No chunk at point"))))

;;; _
(provide 'majutsu-annotate)
;;; majutsu-annotate.el ends here
