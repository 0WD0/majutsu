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

(declare-function majutsu-selection-render "majutsu-selection" (&optional session))

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
  locate-fn
  targets-fn
  categories
  overlays)

(defvar majutsu-selection--overlay-buffers
  (make-hash-table :test 'eq :weakness 'key)
  "Buffers that may contain `majutsu-selection' overlays.

Used to avoid scanning all buffers on transient exit.")

(defun majutsu-selection--track-overlay-buffer (buffer)
  (when (buffer-live-p buffer)
    (puthash buffer t majutsu-selection--overlay-buffers)))

(defun majutsu-selection--session-buffer ()
  "Return the buffer that owns the active selection session, if any."
  (when-let* ((session (transient-scope)))
    (majutsu-selection-session-buffer session)))

(defmacro majutsu-selection--with-session-buffer (session &rest body)
  "Run BODY in SESSION's buffer."
  (declare (indent 1) (debug (form &rest form)))
  (let ((buf (make-symbol "buf")))
    `(when-let* ((,buf (and ,session
                            (majutsu-selection-session-buffer ,session)
                            (buffer-live-p (majutsu-selection-session-buffer ,session))
                            (majutsu-selection-session-buffer ,session))))
       (with-current-buffer ,buf
         ,@body))))

(defun majutsu-selection--delete-all-overlays (session)
  (let ((overlays (and session (majutsu-selection-session-overlays session))))
    (when overlays
      (maphash (lambda (_id ov)
                 (when (overlayp ov)
                   (delete-overlay ov)))
               overlays)
      (clrhash overlays))))

(defun majutsu-selection--clear-all-values (session)
  (when session
    (dolist (cat (majutsu-selection-session-categories session))
      (setf (majutsu-selection-category-values cat) nil))))

(defun majutsu-selection--transient-setup-buffer ()
  "Render selection overlays when a transient menu is being setup."
  (when-let* ((session (transient-scope)))
    (when-let* ((buf (majutsu-selection-session-buffer session))
                (_ (buffer-live-p buf)))
      ;; Avoid leaking old overlays when switching between nested transients.
      ;; We can't reliably access scope in exit hooks, so clean up here.
      (majutsu-selection--track-overlay-buffer buf)
      (with-current-buffer buf
        (remove-overlays (point-min) (point-max) 'majutsu-selection t)))
    (majutsu-selection-render session)))

(defun majutsu-selection--transient-post-exit ()
  "Remove selection overlays when leaving the transient stack.

We use `transient-post-exit-hook' (not `transient-exit-hook') because
the latter also runs when another transient becomes active at the same
time (nested/replaced), at which point we might accidentally remove the
new transient's overlays."
  (maphash
   (lambda (buf _)
     (if (buffer-live-p buf)
         (with-current-buffer buf
           (remove-overlays (point-min) (point-max) 'majutsu-selection t))
       (remhash buf majutsu-selection--overlay-buffers)))
   majutsu-selection--overlay-buffers)
  (clrhash majutsu-selection--overlay-buffers))

(defun majutsu-selection-session-end ()
  "End the active transient selection session and remove its overlays.

This clears selection values and disassociates the session from the
current transient's scope."
  (interactive)
  (when-let* ((prefix (transient-active-prefix))
              (session (transient-scope)))
    (majutsu-selection--delete-all-overlays session)
    (majutsu-selection--clear-all-values session)
    (oset prefix scope nil)))

(defun majutsu-selection-session-end-if-owner ()
  "Remove selection overlays in the current buffer.

Intended for `kill-buffer-hook'."
  (remhash (current-buffer) majutsu-selection--overlay-buffers)
  (remove-overlays (point-min) (point-max) 'majutsu-selection t))

(defun majutsu-selection--targets-default ()
  "Default selection target's value."
  (or (magit-region-values nil t)
      (list (magit-section-ident-value (magit-current-section)))))

(defun majutsu-selection--locate-default (value)
  "Default locator for selection overlays."
  (when-let* ((root (oref (magit-current-section) parent))
              (type (oref (magit-current-section) type))
              (section (magit-get-section
                        (append `((,type . ,value)) (magit-section-ident root)))))
    (majutsu-selection--overlay-range section)))

(cl-defun majutsu-selection-session-begin (categories &key locate-fn targets-fn)
  "Create a transient selection session for the current buffer.

CATEGORIES is a list of plists, each containing:
- :key   symbol identifying the selection bucket
- :label string displayed before selected commits
- :face  face (symbol or plist) applied to the label
- :type  either `single' or `multi' (defaults to `multi')"
  (let ((locate-fn (or locate-fn #'majutsu-selection--locate-default))
        (targets-fn (or targets-fn #'majutsu-selection--targets-default)))
    (majutsu-selection-session-create
     :buffer (current-buffer)
     :locate-fn locate-fn
     :targets-fn targets-fn
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
     :overlays (make-hash-table :test 'equal))))

(defun majutsu-selection--category (key &optional session)
  (when-let* ((session (or session (transient-scope))))
    (seq-find (lambda (cat)
                (eq (majutsu-selection-category-key cat) key))
              (majutsu-selection-session-categories session))))

(defun majutsu-selection--require-category (key &optional session)
  (or (majutsu-selection--category key session)
      (user-error "No such selection category: %S" key)))

(defun majutsu-selection-values (key)
  "Return selected commit values for category KEY."
  (let ((cat (majutsu-selection--category key)))
    (copy-sequence (and cat (majutsu-selection-category-values cat)))))

(defun majutsu-selection-count (key)
  "Return number of selected commits for category KEY."
  (length (majutsu-selection-values key)))

(defun majutsu-selection--selected-any-p (id &optional session)
  (when-let* ((session (or session (transient-scope))))
    (seq-some (lambda (cat)
                (member id (majutsu-selection-category-values cat)))
              (majutsu-selection-session-categories session))))

(defun majutsu-selection--labels-for (id &optional session)
  (when-let* ((session (or session (transient-scope))))
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

(defun majutsu-selection--overlay-range (section)
  (let ((start (oref section start))
        (end (or (oref section content) (oref section end))))
    (and start end (cons start end))))

(defun majutsu-selection--overlay-valid-p (ov)
  (and (overlayp ov)
       (overlay-buffer ov)))

(defun majutsu-selection--delete-overlay (session id)
  (when-let* ((overlays (and session (majutsu-selection-session-overlays session)))
              (ov (gethash id overlays)))
    (when (overlayp ov)
      (delete-overlay ov))
    (remhash id overlays)))

(defun majutsu-selection--render-id (session id)
  (when-let* ((overlays (and session (majutsu-selection-session-overlays session))))
    (let* ((labels (majutsu-selection--labels-for id session))
           (existing (gethash id overlays)))
      (cond
       ((not labels)
        (majutsu-selection--delete-overlay session id))
       (t
        (when (and existing (not (majutsu-selection--overlay-valid-p existing)))
          (remhash id overlays)
          (setq existing nil))
        (majutsu-selection--with-session-buffer session
          (when-let* ((locate-fn (majutsu-selection-session-locate-fn session))
                      (range (and locate-fn (funcall locate-fn id))))
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

(defun majutsu-selection-render (&optional session)
  "Re-render selection overlays for the current buffer."
  (let* ((explicit session)
         (session (or explicit (transient-scope))))
    (when (and session
               (or explicit
                   (eq (current-buffer)
                       (majutsu-selection-session-buffer session))))
      (let* ((session session)
             (overlays (majutsu-selection-session-overlays session))
             (selected (make-hash-table :test 'equal)))
        (dolist (cat (majutsu-selection-session-categories session))
          (dolist (id (majutsu-selection-category-values cat))
            (puthash id t selected)))
        (maphash (lambda (id _ov)
                   (unless (gethash id selected)
                     (majutsu-selection--delete-overlay session id)))
                 overlays)
        (maphash (lambda (id _)
                   (majutsu-selection--render-id session id))
                 selected)))))

(defun majutsu-selection-clear (&optional key)
  "Clear selections.

If KEY is non-nil, clear only that selection category.  Otherwise clear
all categories in the active session."
  (interactive)
  (when-let* ((session (transient-scope)))
    (if key
        (let ((cat (majutsu-selection--require-category key session)))
          (setf (majutsu-selection-category-values cat) nil))
      (majutsu-selection--clear-all-values session))
    (majutsu-selection-render session)))

(defun majutsu-selection-toggle (key &optional values)
  "Toggle selected commits in category KEY.

VALUES defaults to the commit(s) at point or in the active region."
  (interactive)
  (let* ((session (or (transient-scope)
                      (user-error "No active selection session")))
         (values (or values
                     (majutsu-selection--with-session-buffer session
                       (let ((fn (majutsu-selection-session-targets-fn session)))
                         (and fn (funcall fn)))))))
    (unless values
      (user-error "No changeset at point"))
    (let* ((cat (majutsu-selection--require-category key session)))
      (pcase (majutsu-selection-category-type cat)
        ('single
         (let* ((new (car values))
                (old (car (majutsu-selection-category-values cat))))
           (setf (majutsu-selection-category-values cat)
                 (if (and old (equal old new)) nil (list new)))
           (when old (majutsu-selection--render-id session old))
           (when new (majutsu-selection--render-id session new))))
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
             (majutsu-selection--render-id session id))))))))

(defun majutsu-selection-select (key &optional values)
  "Toggle a single commit selection in category KEY.

KEY must be a `single' selection category.  VALUES defaults to the
commit(s) at point or in the active region."
  (interactive)
  (let* ((session (or (transient-scope)
                      (user-error "No active selection session")))
         (values (or values
                     (majutsu-selection--with-session-buffer session
                       (let ((fn (majutsu-selection-session-targets-fn session)))
                         (and fn (funcall fn)))))))
    (unless values
      (user-error "No changeset at point"))
    (let* ((cat (majutsu-selection--require-category key session)))
      (unless (eq (majutsu-selection-category-type cat) 'single)
        (user-error "Selection category %S is not single-select" key))
      (when (cdr values)
        (user-error "Selection category %S only accepts one changeset" key))
      (majutsu-selection-toggle key values))))

(add-hook 'transient-setup-buffer-hook #'majutsu-selection--transient-setup-buffer)
(add-hook 'transient-post-exit-hook #'majutsu-selection--transient-post-exit)

;;; _
(provide 'majutsu-selection)
;;; majutsu-selection.el ends here
