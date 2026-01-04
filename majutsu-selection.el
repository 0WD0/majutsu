;;; majutsu-selection.el --- Selection control for magit-section  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; This library provides magit-section selection functionalities for Majutsu

;;; Code:

(require 'magit-section)
(require 'transient)

(cl-defstruct (majutsu-selection-category
               (:constructor majutsu-selection-category-create))
  key
  label
  face
  type
  values)

(cl-defstruct (majutsu-selection-session
               (:constructor majutsu-selection-session-create))
  buffer
  categories
  overlays)

;; FIXME: 这种处理方式是不合理的，我们有可能有两层 transient，上层下层
;; 都有 selection session，它们甚至可能在两个不同的 buffer 中。
(defvar majutsu-selection--session nil
  "Active transient selection session.

Selections are keyed by the `value' of `majutsu-commit-section' instances
(i.e., the revset/id string shown in the log).  The session and overlays
are meant to exist only while a transient is active.")

(defun majutsu-selection--session-buffer ()
  "Return the buffer that owns the active selection session, if any."
  (let ((session majutsu-selection--session))
    (and session
         (buffer-live-p (majutsu-selection-session-buffer session))
         (majutsu-selection-session-buffer session))))

(defmacro majutsu-selection--with-session-buffer (&rest body)
  "Run BODY in the selection session's buffer."
  (declare (indent 0) (debug t))
  (let ((buf (make-symbol "buf")))
    `(when-let* ((,buf (majutsu-selection--session-buffer)))
       (with-current-buffer ,buf
         ,@body))))

(defun majutsu-selection--delete-all-overlays ()
  (when-let* ((session majutsu-selection--session))
    (let ((overlays (majutsu-selection-session-overlays session)))
      (maphash (lambda (_id ov)
                 (when (overlayp ov)
                   (delete-overlay ov)))
               overlays)
      (clrhash overlays))))

(defun majutsu-selection-session-end ()
  "End the active transient selection session and remove its overlays."
  (interactive)
  (when majutsu-selection--session
    (majutsu-selection--delete-all-overlays)
    (remove-hook 'transient-exit-hook #'majutsu-selection--transient-exit)
    (setq majutsu-selection--session nil)))

(defun majutsu-selection-session-end-if-owner ()
  "End the active transient selection session if owned by current buffer."
  (when-let* ((session majutsu-selection--session))
    (when (eq (current-buffer)
              (majutsu-selection-session-buffer session))
      (majutsu-selection-session-end))))

(defun majutsu-selection-session-begin (categories)
  "Begin a transient selection session in the current buffer.

CATEGORIES is a list of plists, each containing:
- :key   symbol identifying the selection bucket
- :label string displayed before selected commits
- :face  face (symbol or plist) applied to the label
- :type  either `single' or `multi' (defaults to `multi')"
  (majutsu-selection-session-end)
  (setq
   majutsu-selection--session
   (majutsu-selection-session-create
    :buffer (current-buffer)
    :categories
    (mapcar
     (lambda (spec)
       (let* ((key (plist-get spec :key))
              (label (or (plist-get spec :label) ""))
              (face (plist-get spec :face))
              (type (or (plist-get spec :type) 'multi)))
         (unless (symbolp key)
           (user-error "Selection category :key must be a symbol: %S" spec))
         (unless (memq type '(single multi))
           (user-error "Selection category :type must be `single' or `multi': %S" spec))
         (majutsu-selection-category-create
          :key key :label label :face face :type type :values nil)))
     categories)
    :overlays (make-hash-table :test 'equal)))
  (add-hook 'transient-exit-hook #'majutsu-selection--transient-exit)
  (majutsu-selection-render))

(defun majutsu-selection--category (key)
  (when-let* ((session majutsu-selection--session))
    (seq-find (lambda (cat)
                (eq (majutsu-selection-category-key cat) key))
              (majutsu-selection-session-categories session))))

(defun majutsu-selection--require-category (key)
  (or (majutsu-selection--category key)
      (user-error "No such selection category: %S" key)))

(defun majutsu-selection-values (key)
  "Return selected commit values for category KEY."
  (let ((cat (majutsu-selection--category key)))
    (copy-sequence (and cat (majutsu-selection-category-values cat)))))

(defun majutsu-selection-count (key)
  "Return number of selected commits for category KEY."
  (length (majutsu-selection-values key)))

(defun majutsu-selection--selected-any-p (id)
  (when-let* ((session majutsu-selection--session))
    (seq-some (lambda (cat)
                (member id (majutsu-selection-category-values cat)))
              (majutsu-selection-session-categories session))))

(defun majutsu-selection--labels-for (id)
  (when-let* ((session majutsu-selection--session))
    (let (parts)
      (dolist (cat (majutsu-selection-session-categories session))
        (when (member id (majutsu-selection-category-values cat))
          (let ((label (majutsu-selection-category-label cat)))
            (unless (string-empty-p label)
              (push (propertize label
                                'face (majutsu-selection-category-face cat))
                    parts)))))
      (when parts
        (concat (mapconcat #'identity (nreverse parts) " ") " ")))))

(defun majutsu-selection--transient-exit ()
  "End active selection session when leaving a transient."
  (majutsu-selection-session-end))

(defun majutsu-selection--targets-at-point ()
  (let ((values (or (magit-region-values 'jj-commit t)
                    (and-let* ((value (magit-section-value-if 'jj-commit)))
                      (list value)))))
    (seq-filter
     (lambda (v) (and v (not (string-empty-p v))))
     (mapcar #'majutsu--normalize-id-value values))))

(defun majutsu-selection--overlay-range (section)
  (let ((start (oref section start))
        (end (or (oref section content) (oref section end))))
    (and start end (cons start end))))

(defun majutsu-selection--overlay-valid-p (ov)
  (and (overlayp ov)
       (overlay-buffer ov)))

(defun majutsu-selection--delete-overlay (id)
  (when-let* ((session majutsu-selection--session)
              (overlays (majutsu-selection-session-overlays session))
              (ov (gethash id overlays)))
    (when (overlayp ov)
      (delete-overlay ov))
    (remhash id overlays)))

(defun majutsu-selection--render-id (id)
  (when-let* ((session majutsu-selection--session)
              (overlays (majutsu-selection-session-overlays session)))
    (let* ((labels (majutsu-selection--labels-for id))
           (existing (gethash id overlays)))
      (cond
       ((not labels)
        (majutsu-selection--delete-overlay id))
       (t
        (when (and existing (not (majutsu-selection--overlay-valid-p existing)))
          (remhash id overlays)
          (setq existing nil))
        (majutsu-selection--with-session-buffer
          (when-let* ((section (and (derived-mode-p 'majutsu-log-mode)
                                    (majutsu-find-revision-section id)))
                      (range (majutsu-selection--overlay-range section)))
            (pcase-let ((`(,start . ,end) range))
              (unless (and existing
                           (= (overlay-start existing) start)
                           (= (overlay-end existing) end))
                (when existing
                  (delete-overlay existing))
                (setq existing (make-overlay start end nil t))
                (overlay-put existing 'evaporate t)
                (overlay-put existing 'priority '(nil . 50))
                (overlay-put existing 'majutsu-selection t)
                (puthash id existing overlays))
              (overlay-put existing 'before-string labels)))))))))

(defun majutsu-selection-render ()
  "Re-render selection overlays for the current buffer."
  (when majutsu-selection--session
    (let* ((session majutsu-selection--session)
           (overlays (majutsu-selection-session-overlays session))
           (selected (make-hash-table :test 'equal)))
      (dolist (cat (majutsu-selection-session-categories session))
        (dolist (id (majutsu-selection-category-values cat))
          (puthash id t selected)))
      (maphash (lambda (id _ov)
                 (unless (gethash id selected)
                   (majutsu-selection--delete-overlay id)))
               overlays)
      (maphash (lambda (id _)
                 (majutsu-selection--render-id id))
               selected))))

(defun majutsu-selection-clear (&optional key)
  "Clear selections.

If KEY is non-nil, clear only that selection category.  Otherwise clear
all categories in the active session."
  (interactive)
  (when-let* ((session majutsu-selection--session))
    (if key
        (let ((cat (majutsu-selection--require-category key)))
          (setf (majutsu-selection-category-values cat) nil))
      (dolist (cat (majutsu-selection-session-categories session))
        (setf (majutsu-selection-category-values cat) nil)))
    (majutsu-selection-render)))

(defun majutsu-selection-toggle (key &optional values)
  "Toggle selected commits in category KEY.

VALUES defaults to the commit(s) at point or in the active region."
  (interactive)
  (let* ((values (or values (majutsu-selection--targets-at-point))))
    (unless values
      (user-error "No changeset at point"))
    (unless majutsu-selection--session
      (user-error "No active selection session"))
    (let ((cat (majutsu-selection--require-category key)))
      (pcase (majutsu-selection-category-type cat)
        ('single
         (let* ((new (car values))
                (old (car (majutsu-selection-category-values cat))))
           (setf (majutsu-selection-category-values cat)
                 (if (and old (equal old new)) nil (list new)))
           (when old (majutsu-selection--render-id old))
           (when new (majutsu-selection--render-id new))))
        (_
         (let ((current (majutsu-selection-category-values cat))
               (changed nil))
           (dolist (id values)
             (if (member id current)
                 (setq current (delete id current))
               (setq current (append current (list id))))
             (push id changed))
           (setf (majutsu-selection-category-values cat) current)
           (dolist (id changed)
             (majutsu-selection--render-id id))))))))

(defun majutsu-selection-select (key &optional values)
  "Toggle a single commit selection in category KEY.

KEY must be a `single' selection category.  VALUES defaults to the
commit(s) at point or in the active region."
  (interactive)
  (let* ((values (or values (majutsu-selection--targets-at-point))))
    (unless values
      (user-error "No changeset at point"))
    (unless majutsu-selection--session
      (user-error "No active selection session"))
    (let ((cat (majutsu-selection--require-category key)))
      (unless (eq (majutsu-selection-category-type cat) 'single)
        (user-error "Selection category %S is not single-select" key))
      (when (cdr values)
        (user-error "Selection category %S only accepts one changeset" key))
      (majutsu-selection-toggle key values))))

;;; _
(provide 'majutsu-selection)
;;; majutsu-selection.el ends here
