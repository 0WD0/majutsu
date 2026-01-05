;;; majutsu-selection.el --- Selection control for magit-section  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; This library provides magit-section selection functionalities for Majutsu.

;;; Code:

(require 'magit-section)
(require 'subr-x)
(require 'transient)

(cl-defstruct (majutsu-selection-category
               (:constructor majutsu-selection-category-create))
  key
  label
  face
  type
  locate-fn
  targets-fn
  values)

(cl-defstruct (majutsu-selection-session
               (:constructor majutsu-selection-session-create))
  buffer
  categories
  overlays)

(defvar majutsu-selection--overlay-buffers
  (make-hash-table :test 'eq :weakness 'key)
  "Buffers that may contain `majutsu-selection' overlays.
Used to avoid scanning all buffers on transient exit.")

(defvar-local majutsu-selection--active-session nil
  "Selection session whose overlays are currently rendered in this buffer.")

(defun majutsu-selection--track-overlay-buffer (buffer)
  (when (buffer-live-p buffer)
    (puthash buffer t majutsu-selection--overlay-buffers)))

(defun majutsu-selection--cleanup-overlays-in-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (remove-overlays (point-min) (point-max) 'majutsu-selection t)
      (setq majutsu-selection--active-session nil))))

(defmacro majutsu-selection--with-session-buffer (session &rest body)
  "Run BODY in SESSION's buffer."
  (declare (indent 1) (debug (form &rest form)))
  (let ((buf (make-symbol "buf")))
    `(when-let* ((,buf (and ,session (majutsu-selection-session-buffer ,session)))
                 ((buffer-live-p ,buf)))
       (with-current-buffer ,buf
         ,@body))))

(defun majutsu-selection--clear-all-values (session)
  (when session
    (dolist (cat (majutsu-selection-session-categories session))
      (setf (majutsu-selection-category-values cat) nil))))

(defun majutsu-selection--transient-setup-buffer ()
  "Render selection overlays when a transient menu is being setup."
  (let* ((session (transient-scope))
         (buf (and session (majutsu-selection-session-buffer session))))
    ;; Clean up old buffers
    (maphash (lambda (old-buf _)
               (unless (eq old-buf buf)
                 (majutsu-selection--cleanup-overlays-in-buffer old-buf)
                 (remhash old-buf majutsu-selection--overlay-buffers)))
             majutsu-selection--overlay-buffers)
    ;; Setup active buffer
    (when (buffer-live-p buf)
      (majutsu-selection--track-overlay-buffer buf)
      (with-current-buffer buf
        (unless (eq majutsu-selection--active-session session)
          (majutsu-selection--cleanup-overlays-in-buffer buf)
          (setq majutsu-selection--active-session session)
          (majutsu-selection-render session))))))

(defun majutsu-selection--transient-post-exit ()
  "Remove selection overlays when leaving the transient stack."
  (maphash (lambda (buf _)
             (majutsu-selection--cleanup-overlays-in-buffer buf))
           majutsu-selection--overlay-buffers)
  (clrhash majutsu-selection--overlay-buffers))

(defun majutsu-selection-session-end-if-owner ()
  "Remove selection overlays in the current buffer.
Intended for `kill-buffer-hook'."
  (remhash (current-buffer) majutsu-selection--overlay-buffers)
  (remove-overlays (point-min) (point-max) 'majutsu-selection t)
  (setq majutsu-selection--active-session nil))

(defun majutsu-selection--targets-default ()
  "Default selection target's value."
  (or (magit-region-values nil t)
      (list (magit-section-ident-value (magit-current-section)))))

(defun majutsu-selection--locate-default (value)
  "Default locator for selection overlays.
Return the sibling section of the current section whose value equals VALUE."
  (when-let* ((cur (magit-current-section))
              (parent (oref cur parent))
              (type (oref cur type)))
    (magit-get-section
     (append `((,type . ,value)) (magit-section-ident parent)))))

(cl-defun majutsu-selection-session-begin (categories &key locate-fn targets-fn)
  "Create a transient selection session for the current buffer.

CATEGORIES is a list of plists, each containing:
- :key        symbol identifying the selection bucket
- :label      string displayed before selected commits
- :face       face (symbol or plist) applied to the label
- :type       either `single' or `multi' (defaults to `multi')
- :locate-fn  override default locate function
- :targets-fn override default targets function"
  (let ((def-locate (or locate-fn #'majutsu-selection--locate-default))
        (def-targets (or targets-fn #'majutsu-selection--targets-default)))
    (majutsu-selection-session-create
     :buffer (current-buffer)
     :categories
     (mapcar
      (lambda (spec)
        (let ((key (plist-get spec :key))
              (type (or (plist-get spec :type) 'multi)))
          (unless (symbolp key)
            (user-error "Selection category :key must be a symbol: %S" spec))
          (unless (memq type '(single multi))
            (user-error "Selection category :type must be `single' or `multi': %S" spec))
          (majutsu-selection-category-create
           :key key
           :label (or (plist-get spec :label) "")
           :face (plist-get spec :face)
           :type type
           :locate-fn (or (plist-get spec :locate-fn) def-locate)
           :targets-fn (or (plist-get spec :targets-fn) def-targets)
           :values nil)))
      categories)
     :overlays (make-hash-table :test 'equal))))

(defun majutsu-selection--category (key &optional session)
  (when-let* ((session (or session (transient-scope))))
    (seq-find (lambda (cat) (eq (majutsu-selection-category-key cat) key))
              (majutsu-selection-session-categories session))))

(defun majutsu-selection--require-category (key &optional session)
  (or (majutsu-selection--category key session)
      (user-error "No such selection category: %S" key)))

(defun majutsu-selection-values (key)
  "Return selected values for category KEY."
  (let ((cat (majutsu-selection--category key)))
    (copy-sequence (and cat (majutsu-selection-category-values cat)))))

(defun majutsu-selection-count (key)
  "Return number of selected values for category KEY."
  (length (majutsu-selection-values key)))

(defun majutsu-selection--labels-for (id &optional session)
  (when-let* ((session (or session (transient-scope))))
    (let (parts)
      (dolist (cat (majutsu-selection-session-categories session))
        (when (member id (majutsu-selection-category-values cat))
          (when-let* ((label (majutsu-selection-category-label cat))
                      ((not (string-empty-p label))))
            (push (propertize label 'face (majutsu-selection-category-face cat))
                  parts))))
      (when parts
        (concat (mapconcat #'identity (nreverse parts) " ") " ")))))

(defun majutsu-selection--overlay-range (section)
  (let ((start (oref section start))
        (end (or (oref section content) (oref section end))))
    (and start end (cons start end))))

(defun majutsu-selection--resolve-locator (session id)
  (seq-some (lambda (cat)
              (and (member id (majutsu-selection-category-values cat))
                   (majutsu-selection-category-locate-fn cat)))
            (majutsu-selection-session-categories session)))

(defun majutsu-selection--render-id (session id)
  (let ((overlays (majutsu-selection-session-overlays session)))
    (if-let* ((labels (majutsu-selection--labels-for id session)))
        ;; Render overlay
        (majutsu-selection--with-session-buffer session
          (when-let* ((locate-fn (majutsu-selection--resolve-locator session id))
                      (located (funcall locate-fn id))
                      (range (cond ((consp located) located)
                                   ((eieio-object-p located)
                                    (majutsu-selection--overlay-range located)))))
            (let* ((existing (gethash id overlays))
                   (start (car range))
                   (end (cdr range)))
              ;; Reuse existing overlay if range matches
              (unless (and existing
                           (= (overlay-start existing) start)
                           (= (overlay-end existing) end))
                (when existing (delete-overlay existing))
                (setq existing (make-overlay start end nil t))
                (overlay-put existing 'evaporate t)
                (overlay-put existing 'priority '(nil . 50))
                (overlay-put existing 'majutsu-selection t)
                (puthash id existing overlays))
              (overlay-put existing 'before-string labels))))
      ;; No labels -> remove overlay
      (when-let* ((existing (gethash id overlays)))
        (delete-overlay existing)
        (remhash id overlays)))))

(defun majutsu-selection-render (&optional session)
  "Re-render selection overlays for the current buffer."
  (let* ((explicit session)
         (session (or explicit (transient-scope))))
    (when (and session
               (or explicit
                   (eq (current-buffer) (majutsu-selection-session-buffer session))))
      (let ((selected (make-hash-table :test 'equal)))
        ;; Collect all currently selected IDs
        (dolist (cat (majutsu-selection-session-categories session))
          (dolist (id (majutsu-selection-category-values cat))
            (puthash id t selected)))
        ;; Cleanup unselected overlays
        (maphash (lambda (id ov)
                   (unless (gethash id selected)
                     (delete-overlay ov)
                     (remhash id (majutsu-selection-session-overlays session))))
                 (majutsu-selection-session-overlays session))
        ;; Render selected IDs
        (maphash (lambda (id _) (majutsu-selection--render-id session id))
                 selected)))))

(defun majutsu-selection-clear (&optional key)
  "Clear selections.
If KEY is non-nil, clear only that selection category."
  (interactive)
  (when-let* ((session (transient-scope)))
    (if key
        (when-let* ((cat (majutsu-selection--require-category key session)))
          (setf (majutsu-selection-category-values cat) nil))
      (majutsu-selection--clear-all-values session))
    (majutsu-selection-render session)))

(defun majutsu-selection-toggle (key &optional values)
  "Toggle selected values in category KEY.
VALUES defaults to the target(s) at point."
  (interactive)
  (let* ((session (or (transient-scope) (user-error "No active selection session")))
         (cat (majutsu-selection--require-category key session)))
    (unless values
      (majutsu-selection--with-session-buffer session
        (setq values (funcall (majutsu-selection-category-targets-fn cat)))))
    (unless values
      (user-error "No selection target at point"))
    
    (if (eq (majutsu-selection-category-type cat) 'single)
        ;; Single selection mode
        (let ((new (car values))
              (old (car (majutsu-selection-category-values cat))))
          (setf (majutsu-selection-category-values cat)
                (if (equal old new) nil (list new))))
      ;; Multi selection mode
      (let ((current (majutsu-selection-category-values cat)))
        (dolist (id values)
          (setq current (if (member id current)
                            (delete id current)
                          (append current (list id)))))
        (setf (majutsu-selection-category-values cat) current)))
    
    (majutsu-selection-render session)))

(defun majutsu-selection-select (key &optional values)
  "Select a single value in category KEY.
KEY must be a `single' selection category."
  (interactive)
  (let* ((session (or (transient-scope) (user-error "No active selection session")))
         (cat (majutsu-selection--require-category key session)))
    (unless (eq (majutsu-selection-category-type cat) 'single)
      (user-error "Category %S is not single-select" key))
    (majutsu-selection-toggle key values)))

(add-hook 'transient-setup-buffer-hook #'majutsu-selection--transient-setup-buffer)
(add-hook 'transient-post-exit-hook #'majutsu-selection--transient-post-exit)

(provide 'majutsu-selection)
;;; majutsu-selection.el ends here
