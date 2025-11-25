;;; majutsu-transient.el --- Transient menus for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:
;; Transient menus and state management for Majutsu.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-log)
(require 'transient)
(require 'cl-lib)

(declare-function majutsu-abandon "majutsu-commands" ())
(declare-function majutsu-commit "majutsu-commands" ())
(declare-function majutsu-describe "majutsu-commands" ())
(declare-function majutsu-diff-transient "majutsu-commands" ())
(declare-function majutsu-diffedit-emacs "majutsu-commands" ())
(declare-function majutsu-diffedit-smerge "majutsu-commands" ())
(declare-function majutsu-duplicate "majutsu-commands" (&optional arg))
(declare-function majutsu-duplicate-transient "majutsu-commands" ())
(declare-function majutsu-new "majutsu-commands" (&optional arg))
(declare-function majutsu-new-transient "majutsu-commands" ())
(declare-function majutsu-redo "majutsu-commands" ())
(declare-function majutsu-rebase-transient "majutsu-commands" ())
(declare-function majutsu-bookmark-transient "majutsu-commands" ())
(declare-function majutsu-squash-transient "majutsu-commands" ())
(declare-function majutsu-undo "majutsu-commands" ())
(declare-function majutsu-edit-changeset "majutsu-commands" ())

;;; Shared Helpers

(cl-defstruct (majutsu--transient-entry
               (:constructor majutsu--transient-entry-create))
  "Bookkeeping for transient selections bound to log entries."
  change-id
  commit-id
  overlay)

(defun majutsu--transient-entry-revset (entry)
  "Return the revset string to use for ENTRY, preferring change-id."
  (or (majutsu--transient-entry-change-id entry)
      (majutsu--transient-entry-commit-id entry)))

(defun majutsu--transient-entry-display (entry)
  "Return a human-readable identifier for ENTRY."
  (or (majutsu--transient-entry-change-id entry)
      (majutsu--transient-entry-commit-id entry)
      "?"))

(defun majutsu--transient-entry-delete-overlay (entry)
  "Delete the overlay associated with ENTRY, if present."
  (let ((overlay (majutsu--transient-entry-overlay entry)))
    (when (overlayp overlay)
      (delete-overlay overlay)
      (setf (majutsu--transient-entry-overlay entry) nil)))
  nil)

(defun majutsu--transient-clear-overlays (entries)
  "Delete overlays for all ENTRIES and return nil."
  (dolist (entry entries)
    (majutsu--transient-entry-delete-overlay entry))
  nil)

(defun majutsu--transient-make-overlay (section face label ref)
  "Create an overlay for SECTION with FACE, LABEL, and REF identifier."
  (let ((overlay (make-overlay (oref section start) (oref section end))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'before-string (concat label " "))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'majutsu-ref (or ref ""))
    overlay))

(defun majutsu--transient-entry-apply-overlay (entry section face label)
  "Attach a fresh overlay for ENTRY using SECTION, FACE, and LABEL."
  (majutsu--transient-entry-delete-overlay entry)
  (when section
    (let ((ref (majutsu--transient-entry-revset entry)))
      (when ref
        (let ((overlay (majutsu--transient-make-overlay section face label ref)))
          (setf (majutsu--transient-entry-overlay entry) overlay)
          overlay)))))

(defun majutsu--transient-entry-reapply (entry face label)
  "Reapply overlay for ENTRY after a log refresh."
  (when-let* ((section (majutsu--find-log-entry-section
                        (majutsu--transient-entry-change-id entry)
                        (majutsu--transient-entry-commit-id entry))))
    (majutsu--transient-entry-apply-overlay entry section face label)))

(defun majutsu--transient-selection-find (entries change-id commit-id)
  "Find ENTRY in ENTRIES matching CHANGE-ID (preferred) or COMMIT-ID."
  (seq-find (lambda (entry)
              (let ((entry-change (majutsu--transient-entry-change-id entry))
                    (entry-commit (majutsu--transient-entry-commit-id entry)))
                (cond
                 ((and change-id entry-change)
                  (string= entry-change change-id))
                 ((and (not change-id) commit-id entry-commit)
                  (string= entry-commit commit-id)))))
            entries))

(defun majutsu--transient-selection-remove (entries entry)
  "Return ENTRIES without ENTRY, cleaning up overlay side effects."
  (majutsu--transient-entry-delete-overlay entry)
  (delq entry entries))

(defun majutsu--transient-refresh-if-rewritten (entry new-commit face label)
  "Refresh log buffer when ENTRY's commit id changed to NEW-COMMIT.
FACE and LABEL are used to reapply the overlay post-refresh.
Return the section corresponding to the rewritten change when refresh occurs."
  (let ((old-commit (majutsu--transient-entry-commit-id entry)))
    (when (and entry old-commit new-commit (not (string= old-commit new-commit)))
      (let ((change (majutsu--transient-entry-change-id entry)))
        (majutsu--message-with-log "Change %s rewritten (%s -> %s); refreshing log"
                                   (or change "?")
                                   old-commit
                                   new-commit)
        (setf (majutsu--transient-entry-commit-id entry) new-commit)
        (majutsu--transient-entry-delete-overlay entry)
        (majutsu-log-refresh)
        (let ((section (majutsu--find-log-entry-section change new-commit)))
          (majutsu--transient-entry-apply-overlay entry section face label)
          section)))))

(cl-defun majutsu--transient--toggle-selection (&key kind label face collection-var (type 'multi))
  "Internal helper to mutate refset selections for the current log entry.
KIND/LABEL/FACE describe the UI; COLLECTION-VAR is the symbol storing entries.
TYPE is either `single' or `multi'."
  (let* ((ids (majutsu-log--ids-at-point)))
    (if (not ids)
        (message "No changeset at point to toggle")
      (let* ((change (plist-get ids :change))
             (commit (plist-get ids :commit))
             (section (plist-get ids :section))
             (entries (symbol-value collection-var))
             (existing (majutsu--transient-selection-find entries change commit)))
        (when existing
          (when-let* ((new-section (majutsu--transient-refresh-if-rewritten existing commit face label)))
            (setq section new-section)))
        (pcase type
          ('single
           (if existing
               (progn
                 (majutsu--transient-clear-overlays entries)
                 (set collection-var nil)
                 (message "Cleared %s" kind))
             (let ((entry (majutsu--transient-entry-create
                           :change-id change :commit-id commit)))
               (majutsu--transient-clear-overlays entries)
               (let ((overlay-section (or section (majutsu--find-log-entry-section change commit))))
                 (majutsu--transient-entry-apply-overlay entry overlay-section face label))
               (set collection-var (list entry))
               (message "Set %s: %s" kind (majutsu--transient-entry-display entry)))))
          (_
           (if existing
               (progn
                 (set collection-var (majutsu--transient-selection-remove entries existing))
                 (message "Removed %s: %s" kind (majutsu--transient-entry-display existing)))
             (let ((entry (majutsu--transient-entry-create
                           :change-id change :commit-id commit)))
               (let ((overlay-section (or section (majutsu--find-log-entry-section change commit))))
                 (majutsu--transient-entry-apply-overlay entry overlay-section face label))
               (set collection-var (append entries (list entry)))
               (message "Added %s: %s" kind (majutsu--transient-entry-display entry))))))))))

(cl-defun majutsu--transient-select-refset (&key kind label face collection-var)
  "Shared helper for `<REFSET>' style single selections."
  (majutsu--transient--toggle-selection
   :kind kind :label label :face face
   :collection-var collection-var :type 'single))

(cl-defun majutsu--transient-toggle-refsets (&key kind label face collection-var)
  "Shared helper for `<REFSETS>' style multi selections."
  (majutsu--transient--toggle-selection
   :kind kind :label label :face face
   :collection-var collection-var :type 'multi))

(defun majutsu--transient-normalize-revsets (items)
  "Convert ITEMS (entries or strings) into a list of clean revset strings."
  (seq-filter (lambda (rev) (and rev (not (string-empty-p rev))))
              (mapcar (lambda (item)
                        (cond
                         ((majutsu--transient-entry-p item)
                          (majutsu--transient-entry-revset item))
                         ((stringp item)
                          (substring-no-properties item))
                         (t nil)))
                      items)))

;;; Dispatch

;;;###autoload
(transient-define-prefix majutsu-dispatch ()
  "Top-level Majutsu command dispatcher."
  [:description "Majutsu Commands"
   :class transient-columns
   ["Basic Operations"
    ("g" "Refresh log" majutsu-log-refresh)
    ("c" "Commit" majutsu-commit)
    ("e" "Edit change" majutsu-edit-changeset)
    ("u" "Undo" majutsu-undo)
    ("R" "Redo" majutsu-redo)
    ("l" "Log options" majutsu-log-transient)
    ("N" "New" majutsu-new)
    ("n" "New (transient)" majutsu-new-transient)
    ("y" "Duplicate (transient)" majutsu-duplicate-transient)
    ("Y" "Duplicate" majutsu-duplicate)
    ("a" "Abandon" majutsu-abandon)
    ("d" "Describe" majutsu-describe)
    ("s" "Squash" majutsu-squash-transient)]
   ["Advanced"
    ("r" "Rebase" majutsu-rebase-transient)
    ("b" "Bookmarks" majutsu-bookmark-transient)
    ("G" "Git" majutsu-git-transient)]
   ["Diff & Fix"
    ("D" "Diff menu" majutsu-diff-transient)
    ("E" "DiffEdit (ediff)" majutsu-diffedit-emacs)
    ("M" "DiffEdit (smerge)" majutsu-diffedit-smerge)]
   ["Exit"
    ("?" "Help" transient-help)
    ("q" "Quit" transient-quit-one)]])

;;; Log Transient

(defun majutsu-log--toggle-desc (label key)
  "Return LABEL annotated with ON/OFF state for KEY."
  (if (majutsu-log--state-get key)
      (format "%s [on]" label)
    (format "%s [off]" label)))

(defun majutsu-log--value-desc (label key)
  "Return LABEL annotated with the string value stored at KEY."
  (if-let* ((value (majutsu-log--state-get key)))
      (format "%s (%s)" label value)
    label))

(defun majutsu-log--paths-desc ()
  "Return description for path filters."
  (let ((paths (majutsu-log--state-get :filesets)))
    (cond
     ((null paths) "Add path filter")
     ((= (length paths) 1) (format "Add path filter (%s)" (car paths)))
     (t (format "Add path filter (%d paths)" (length paths))))))

(defun majutsu-log-transient--redisplay ()
  "Redisplay the log transient, compatible with older transient versions."
  (if (fboundp 'transient-redisplay)
      (transient-redisplay)
    (when (fboundp 'transient--redisplay)
      (transient--redisplay))))

(defvar majutsu-log--transient-description "JJ Log Options")

(transient-define-prefix majutsu-log-transient ()
  "Transient interface for adjusting jj log options."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-log--transient-description
   :class transient-columns
   ["Revisions"
    ("r" "Revset" majutsu-log-transient-set-revisions
     :description (lambda ()
                    (majutsu-log--value-desc "Revset" :revisions))
     :transient t)
    ("R" "Clear revset" majutsu-log-transient-clear-revisions
     :if (lambda () (majutsu-log--state-get :revisions))
     :transient t)
    ("n" "Limit" majutsu-log-transient-set-limit
     :description (lambda ()
                    (majutsu-log--value-desc "Limit" :limit))
     :transient t)
    ("N" "Clear limit" majutsu-log-transient-clear-limit
     :if (lambda () (majutsu-log--state-get :limit))
     :transient t)
    ("v" "Reverse order" majutsu-log-transient-toggle-reversed
     :description (lambda ()
                    (majutsu-log--toggle-desc "Reverse order" :reversed))
     :transient t)
    ("t" "Hide graph" majutsu-log-transient-toggle-no-graph
     :description (lambda ()
                    (majutsu-log--toggle-desc "Hide graph" :no-graph))
     :transient t)]
   ["Paths"
    ("a" "Add path filter" majutsu-log-transient-add-path
     :description majutsu-log--paths-desc
     :transient t)
    ("A" "Clear path filters" majutsu-log-transient-clear-paths
     :if (lambda () (majutsu-log--state-get :filesets))
     :transient t)]
   ["Actions"
    ("g" "Apply & refresh" majutsu-log-transient-apply :transient nil)
    ("0" "Reset options" majutsu-log-transient-reset :transient t)
    ("q" "Quit" transient-quit-one)]])

;;; Squash Transient

(defvar-local majutsu-squash-from nil
  "List of entry structs selected as squash sources (for --from).")

(defvar-local majutsu-squash-into nil
  "List containing at most one entry struct for squash destination.")

(defun majutsu-squash--into-entry ()
  "Return the entry selected as squash destination, if any."
  (car majutsu-squash-into))

(defun majutsu-squash--from-display ()
  "Return a display string for the squash source."
  (when majutsu-squash-from
    (string-join (mapcar #'majutsu--transient-entry-display majutsu-squash-from) ", ")))

(defun majutsu-squash--into-display ()
  "Return a display string for the squash destination."
  (when-let* ((entry (majutsu-squash--into-entry)))
    (majutsu--transient-entry-display entry)))

(transient-define-prefix majutsu-squash-transient--internal ()
  "Internal transient for jj squash operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Squash"
             (when-let* ((from (majutsu-squash--from-display)))
               (format " | From: %s" from))
             (when-let* ((into (majutsu-squash--into-display)))
               (format " | Into: %s" into))))
   ["Selection"
    ("f" "Set from" majutsu-squash-set-from
     :description (lambda ()
                    (if majutsu-squash-from
                        (format "Set from (current: %s)" (majutsu-squash--from-display))
                      "Set from"))
     :transient t)
    ("t" "Set into" majutsu-squash-set-into
     :description (lambda ()
                    (if (majutsu-squash--into-entry)
                        (format "Set into (current: %s)" (majutsu-squash--into-display))
                      "Set into"))
     :transient t)
    ("c" "Clear selections" majutsu-squash-clear-selections
     :transient t)]
   ["Options"
    ("-k" "Keep emptied commit" "--keep")]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute
     :description (lambda ()
                    (cond
                     ((and majutsu-squash-from (majutsu-squash--into-entry))
                      (format "Squash %s into %s"
                              (majutsu-squash--from-display)
                              (majutsu-squash--into-display)))
                     (majutsu-squash-from
                      (format "Squash %s into parent" (majutsu-squash--from-display)))
                     (t "Execute squash (select commits first)")))
     :transient nil)
    ("q" "Quit" transient-quit-one)
    ("b" "Bury" majutsu-mode-bury-squash)]])

;;; New Transient

(defvar-local majutsu-new-parents nil
  "List of selected parent entries for jj new.")

(defvar-local majutsu-new-after nil
  "List of selected --after entries for jj new.")

(defvar-local majutsu-new-before nil
  "List of selected --before entries for jj new.")

(defvar-local majutsu-new-message nil
  "Cached commit message for jj new transient.")

(defvar-local majutsu-new-no-edit nil
  "Non-nil when jj new should pass --no-edit.")

(defun majutsu-new--message-preview ()
  "Return a truncated preview of `majutsu-new-message'."
  (when majutsu-new-message
    (truncate-string-to-width majutsu-new-message 30 nil nil "...")))

(defun majutsu-new--selection-summary ()
  "Return a list summarizing the current jj new selections."
  (let (parts)
    (when majutsu-new-parents
      (push (format "Parents: %s"
                    (string-join (mapcar #'majutsu--transient-entry-display majutsu-new-parents)
                                 ", "))
            parts))
    (when majutsu-new-after
      (push (format "After: %s"
                    (string-join (mapcar #'majutsu--transient-entry-display majutsu-new-after)
                                 ", "))
            parts))
    (when majutsu-new-before
      (push (format "Before: %s"
                    (string-join (mapcar #'majutsu--transient-entry-display majutsu-new-before)
                                 ", "))
            parts))
    (when majutsu-new-message
      (push (format "Message: %s" (majutsu-new--message-preview)) parts))
    (when majutsu-new-no-edit
      (push "--no-edit" parts))
    (nreverse parts)))

(defun majutsu-new--description ()
  "Compose the transient description for jj new selections."
  (let ((parts (majutsu-new--selection-summary)))
    (if parts
        (concat "JJ New | " (string-join parts " | "))
      "JJ New")))

(defun majutsu-new--action-summary ()
  "Return a short summary string for the jj new execute action."
  (let ((parts (majutsu-new--selection-summary)))
    (if parts
        (string-join parts " | ")
      "Parents: @")))

(cl-defun majutsu-new--build-args (&key parents after before message (no-edit majutsu-new-no-edit))
  "Build the argument list for jj new.
PARENTS, AFTER, BEFORE, MESSAGE, and NO-EDIT default to transient state."
  (let* ((parents (majutsu--transient-normalize-revsets (or parents majutsu-new-parents)))
         (after (majutsu--transient-normalize-revsets (or after majutsu-new-after)))
         (before (majutsu--transient-normalize-revsets (or before majutsu-new-before)))
         (message (or message majutsu-new-message))
         (args '("new")))
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (when (and message (not (string-empty-p message)))
      (setq args (append args (list "--message" message))))
    (when no-edit
      (setq args (append args '("--no-edit"))))
    (setq args (append args parents))
    args))

(transient-define-prefix majutsu-new-transient--internal ()
  "Internal transient for jj new operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-new--description
   :class transient-columns
   ["Selections"
    ("p" "Toggle parent" majutsu-new-toggle-parent
     :description (lambda ()
                    (format "Toggle parent (%d selected)"
                            (length majutsu-new-parents)))
     :transient t)
    ("a" "Toggle --after" majutsu-new-toggle-after
     :description (lambda ()
                    (format "Toggle --after (%d selected)"
                            (length majutsu-new-after)))
     :transient t)
    ("b" "Toggle --before" majutsu-new-toggle-before
     :description (lambda ()
                    (format "Toggle --before (%d selected)"
                            (length majutsu-new-before)))
     :transient t)
    ("c" "Clear selections" majutsu-new-clear-selections
     :transient t)]
   ["Options"
    ("m" "Set message" majutsu-new-edit-message
     :description (lambda ()
                    (if majutsu-new-message
                        (format "Set message (%s)"
                                (majutsu-new--message-preview))
                      "Set message"))
     :transient t)
    ("e" "Toggle --no-edit" majutsu-new-toggle-no-edit
     :description (lambda ()
                    (if majutsu-new-no-edit
                        "--no-edit (enabled)"
                      "--no-edit (disabled)"))
     :transient t)]
   ["Actions"
    ("n" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary)))
     :transient nil)
    ("RET" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary)))
     :transient nil)
    ("q" "Quit" transient-quit-one)]])

;;; Diff Transient

(defvar-local majutsu-diff-from nil
  "List containing at most one entry struct for diff --from.")

(defvar-local majutsu-diff-to nil
  "List containing at most one entry struct for diff --to.")

(defun majutsu-diff--from-entry () (car majutsu-diff-from))
(defun majutsu-diff--to-entry () (car majutsu-diff-to))

(transient-define-prefix majutsu-diff-transient--internal ()
  "Internal transient for jj diff."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Diff"
             (when-let* ((from (majutsu-diff--from-entry)))
               (format " | From: %s" (majutsu--transient-entry-display from)))
             (when-let* ((to (majutsu-diff--to-entry)))
               (format " | To: %s" (majutsu--transient-entry-display to)))))
   :class transient-columns
   ["Selection"
    ("f" "Set 'from'" majutsu-diff-set-from
     :description (lambda ()
                    (if (majutsu-diff--from-entry)
                        (format "Set 'from' (current: %s)"
                                (majutsu--transient-entry-display (majutsu-diff--from-entry)))
                      "Set 'from'"))
     :transient t)
    ("t" "Set 'to'" majutsu-diff-set-to
     :description (lambda ()
                    (if (majutsu-diff--to-entry)
                        (format "Set 'to' (current: %s)"
                                (majutsu--transient-entry-display (majutsu-diff--to-entry)))
                      "Set 'to'"))
     :transient t)
    ("c" "Clear selections" majutsu-diff-clear-selections :transient t)]
   ["Options"
    ("-s" "Stat" "--stat")
    ("-S" "Summary" "--summary")]
   ["Actions"
    ("d" "Execute" majutsu-diff-execute)
    ("q" "Quit" transient-quit-one)]])

;;; Duplicate Transient

(defvar-local majutsu-duplicate-sources nil
  "Entry structs representing revisions to duplicate.")

(defvar-local majutsu-duplicate-destinations nil
  "Entry structs representing `--destination' parents.")

(defvar-local majutsu-duplicate-after nil
  "Entry structs representing `--after' parents.")

(defvar-local majutsu-duplicate-before nil
  "Entry structs representing `--before' parents.")

(defun majutsu-duplicate--sources ()
  "Return normalized duplicate source revsets."
  (let ((sources (majutsu--transient-normalize-revsets majutsu-duplicate-sources)))
    (if (seq-empty-p sources)
        (list (or (majutsu-log--revset-at-point) "@"))
      sources)))

(defun majutsu-duplicate--sources-display ()
  "Return human-readable description of duplicate sources."
  (if majutsu-duplicate-sources
      (string-join (mapcar #'majutsu--transient-entry-display majutsu-duplicate-sources) ", ")
    (or (majutsu-log--revset-at-point) "@")))

(defun majutsu-duplicate--summary ()
  "Return a vector of descriptive fragments for duplicate state."
  (let (parts)
    (push (format "Sources: %s" (majutsu-duplicate--sources-display)) parts)
    (when majutsu-duplicate-destinations
      (push (format "Destinations: %d" (length majutsu-duplicate-destinations)) parts))
    (when majutsu-duplicate-after
      (push (format "--after: %d" (length majutsu-duplicate-after)) parts))
    (when majutsu-duplicate-before
      (push (format "--before: %d" (length majutsu-duplicate-before)) parts))
    (nreverse parts)))

(defun majutsu-duplicate--description ()
  "Build duplicate transient description."
  (let ((parts (majutsu-duplicate--summary)))
    (if parts
        (concat "JJ Duplicate | " (string-join parts " | "))
      "JJ Duplicate")))

(cl-defun majutsu-duplicate--build-args (&key sources destinations after before)
  "Construct jj duplicate argument list."
  (let* ((sources (majutsu--transient-normalize-revsets
                   (or sources majutsu-duplicate-sources)))
         (sources (if (seq-empty-p sources)
                      (majutsu-duplicate--sources)
                    sources))
         (destinations (majutsu--transient-normalize-revsets
                        (or destinations majutsu-duplicate-destinations)))
         (after (majutsu--transient-normalize-revsets
                 (or after majutsu-duplicate-after)))
         (before (majutsu--transient-normalize-revsets
                  (or before majutsu-duplicate-before)))
         (args '("duplicate")))
    (dolist (rev destinations)
      (setq args (append args (list "--destination" rev))))
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (setq args (append args sources))
    args))

(transient-define-prefix majutsu-duplicate-transient--internal ()
  "Internal transient for jj duplicate."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-duplicate--description
   :class transient-columns
   ["Sources"
    ("y" "Toggle source" majutsu-duplicate-toggle-source
     :description (lambda ()
                    (format "Toggle source (%d selected)"
                            (length majutsu-duplicate-sources)))
     :transient t)
    ("c" "Clear selections" majutsu-duplicate-clear-selections
     :transient t)]
   ["Placement"
    ("d" "Toggle --destination" majutsu-duplicate-toggle-destination
     :description (lambda ()
                    (format "--destination (%d selected)"
                            (length majutsu-duplicate-destinations)))
     :transient t)
    ("a" "Toggle --after" majutsu-duplicate-toggle-after
     :description (lambda ()
                    (format "--after (%d selected)"
                            (length majutsu-duplicate-after)))
     :transient t)
    ("b" "Toggle --before" majutsu-duplicate-toggle-before
     :description (lambda ()
                    (format "--before (%d selected)"
                            (length majutsu-duplicate-before)))
     :transient t)]
   ["Actions"
    ("RET" "Duplicate changes" majutsu-duplicate-execute :transient nil)
    ("p" "Duplicate changes" majutsu-duplicate-execute :transient nil)
    ("q" "Quit" transient-quit-one)]])

;;; Rebase Transient

(defvar-local majutsu-rebase-source nil
  "List containing at most one entry struct for the rebase source.")

(defvar-local majutsu-rebase-destinations nil
  "List of entry structs selected as rebase destinations.")

(defvar-local majutsu-rebase-source-type "-s"
  "Flag to use for rebase source (-s, -b, or -r).")

(defvar-local majutsu-rebase-dest-type "-d"
  "Flag to use for rebase destination (-d, -A, or -B).")

(defun majutsu-rebase--source-entry ()
  "Return the entry selected as rebase source, if any."
  (car majutsu-rebase-source))

(defun majutsu-rebase--destination-revsets ()
  "Return the list of destination revsets."
  (mapcar #'majutsu--transient-entry-revset majutsu-rebase-destinations))

(defun majutsu-rebase--destination-display ()
  "Return a comma-separated string for destination display."
  (string-join (mapcar #'majutsu--transient-entry-display majutsu-rebase-destinations) ", "))

(defun majutsu-rebase--source-display ()
  "Return a display string for the current source."
  (when-let* ((entry (majutsu-rebase--source-entry)))
    (majutsu--transient-entry-display entry)))



(transient-define-prefix majutsu-rebase-transient--internal ()
  "Internal transient for jj rebase operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Rebase"
             (when-let* ((source (majutsu-rebase--source-display)))
               (format " | Source (%s): %s" majutsu-rebase-source-type source))
             (when majutsu-rebase-destinations
               (format " | Destinations (%s): %s"
                       majutsu-rebase-dest-type
                       (majutsu-rebase--destination-display)))))
   :class transient-columns
   ["Selection"
    ("s" "Set source" majutsu-rebase-set-source
     :description (lambda ()
                    (if (majutsu-rebase--source-entry)
                        (format "Set source (current: %s)" (majutsu-rebase--source-display))
                      "Set source"))
     :transient t)
    ("S" "Toggle source type" majutsu-rebase-toggle-source-type
     :description (lambda () (format "Source type (%s)" majutsu-rebase-source-type))
     :transient t)
    ("d" "Toggle destination" majutsu-rebase-toggle-destination
     :description (lambda ()
                    (format "Toggle destination (%d selected)"
                            (length majutsu-rebase-destinations)))
     :transient t)
    ("D" "Toggle dest type" majutsu-rebase-toggle-dest-type
     :description (lambda () (format "Dest type (%s)" majutsu-rebase-dest-type))
     :transient t)
    ("c" "Clear selections" majutsu-rebase-clear-selections
     :transient t)]
   ["Options"
    ("-se" "Skip emptied" "--skip-emptied")
    ("-kd" "Keep divergent" "--keep-divergent")]
   ["Actions"
    ("r" "Execute rebase" majutsu-rebase-execute
     :description (lambda ()
                    (if (and (majutsu-rebase--source-entry) majutsu-rebase-destinations)
                        (format "Rebase %s -> %s"
                                (majutsu-rebase--source-display)
                                (majutsu-rebase--destination-display))
                      "Execute rebase (select source & destinations first)"))
     :transient nil)

    ("q" "Quit" transient-quit-one)]])

;;; Bookmark Transient

(transient-define-prefix majutsu-bookmark-transient--internal ()
  "Internal transient for jj bookmark operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  ["Bookmark Operations"
   [
    ("l" "List bookmarks" majutsu-bookmark-list
     :description "Show bookmark list" :transient nil)
    ("c" "Create bookmark" majutsu-bookmark-create
     :description "Create new bookmark" :transient nil)]
   [
    ("s" "Set bookmark" majutsu-bookmark-set
     :description "Create/update to commit" :transient nil)
    ("m" "Move bookmark(s)" majutsu-bookmark-move
     :description "Move existing to commit" :transient nil)
    ("M" "Move bookmark(s) --allow-backwards" majutsu-bookmark-move-allow-backwards
     :description "Move allowing backwards" :transient nil)
    ("r" "Rename bookmark" majutsu-bookmark-rename
     :description "Rename existing bookmark" :transient nil)]
   [
    ("t" "Track remote" majutsu-bookmark-track
     :description "Track remote bookmark" :transient nil)
    ("u" "Untrack remote" majutsu-bookmark-untrack
     :description "Stop tracking remote" :transient nil)]
   [
    ("d" "Delete bookmark" majutsu-bookmark-delete
     :description "Delete (propagate)" :transient nil)
    ("f" "Forget bookmark" majutsu-bookmark-forget
     :description "Forget (local)" :transient nil)]
   [("q" "Quit" transient-quit-one)]] )

;;; Git Transients

(transient-define-prefix majutsu-git-transient ()
  "Top-level transient for jj git operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description "JJ Git"
   :class transient-columns
   ["Sync"
    ("p" "Push" majutsu-git-push-transient)
    ("f" "Fetch" majutsu-git-fetch-transient)
    ("e" "Export" majutsu-git-export)
    ("m" "Import" majutsu-git-import)]
   ["Remotes"
    ("r" "Manage remotes" majutsu-git-remote-transient)
    ("o" "Git root" majutsu-git-root)]
   ["Repository"
    ("c" "Clone" majutsu-git-clone-transient)
    ("i" "Init" majutsu-git-init-transient)]
   [("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-push-transient ()
  "Transient for jj git push."
  [:description "JJ Git Push"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote" "--remote=" :choices majutsu--get-git-remotes)
    ("-b" "Bookmark" "--bookmark=" :choices majutsu--get-bookmark-names)
    ("-a" "All bookmarks" "--all")
    ("-t" "Tracked only" "--tracked")
    ("-D" "Deleted" "--deleted")
    ("-n" "Allow new" "--allow-new")
    ("-E" "Allow empty desc" "--allow-empty-description")
    ("-P" "Allow private" "--allow-private")
    ("-r" "Revisions" "--revisions=")
    ("-c" "Change" "--change=")
    ("-N" "Named X=REV" "--named=")
    ("-y" "Dry run" "--dry-run")]
   [("p" "Push" majutsu-git-push :transient nil)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-fetch-transient ()
  "Transient for jj git fetch."
  [:description "JJ Git Fetch"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote" "--remote=" :choices majutsu--get-git-remotes)
    ("-B" "Branch" "--branch=")
    ("-t" "Tracked only" "--tracked")
    ("-A" "All remotes" "--all-remotes")]
   [("f" "Fetch" majutsu-git-fetch :transient nil)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-remote-transient ()
  "Transient for managing Git remotes."
  [:description "JJ Git Remote"
   :class transient-columns
   ["Arguments (add)"
    ("-T" "Fetch tags" "--fetch-tags="
     :choices ("all" "included" "none"))]
   ["Actions"
    ("l" "List" majutsu-git-remote-list)
    ("a" "Add" majutsu-git-remote-add)
    ("d" "Remove" majutsu-git-remote-remove)
    ("r" "Rename" majutsu-git-remote-rename)
    ("u" "Set URL" majutsu-git-remote-set-url)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-clone-transient ()
  "Transient for jj git clone."
  [:description "JJ Git Clone"
   :class transient-columns
   ["Arguments"
    ("-R" "Remote name" "--remote=")
    ("-C" "Colocate" "--colocate")
    ("-x" "No colocate" "--no-colocate")
    ("-d" "Depth" "--depth=")
    ("-T" "Fetch tags" "--fetch-tags=" :choices ("all" "included" "none"))]
   [("c" "Clone" majutsu-git-clone :transient nil)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-init-transient ()
  "Transient for jj git init."
  [:description "JJ Git Init"
   :class transient-columns
   ["Arguments"
    ("-C" "Colocate" "--colocate")
    ("-x" "No colocate" "--no-colocate")
    ("-g" "Use existing git repo" "--git-repo=")]
   [("i" "Init" majutsu-git-init :transient nil)
    ("q" "Quit" transient-quit-one)]])

(provide 'majutsu-transient)
;;; majutsu-transient.el ends here
