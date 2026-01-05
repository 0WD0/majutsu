;;; majutsu-selection.el --- Selection control for magit-section  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; This library provides magit-section selection for Majutsu.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)
(require 'transient)

(defclass majutsu-selection-option (transient-option)
  ((selection-key   :initarg :selection-key   :initform nil)
   (selection-label :initarg :selection-label :initform nil)
   (selection-face  :initarg :selection-face  :initform nil)
   (selection-type  :initarg :selection-type  :initform 'multi)
   (locate-fn       :initarg :locate-fn       :initform nil)
   (targets-fn      :initarg :targets-fn      :initform nil))
  "Base class for options that control majutsu selection categories.")

(cl-defstruct (majutsu-selection-session
               (:constructor majutsu-selection-session-create))
  buffer
  overlays)

(defvar majutsu-selection--overlay-buffers
  (make-hash-table :test 'eq :weakness 'key)
  "Buffers that may contain `majutsu-selection' overlays.")

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
  "Remove selection overlays in the current buffer."
  (remhash (current-buffer) majutsu-selection--overlay-buffers)
  (remove-overlays (point-min) (point-max) 'majutsu-selection t)
  (setq majutsu-selection--active-session nil))

(defun majutsu-selection--targets-default ()
  "Default selection target's value."
  (or (magit-region-values nil t)
      (list (magit-section-ident-value (magit-current-section)))))

(defun majutsu-selection--locate-default (value)
  "Default locator for selection overlays."
  (when-let* ((cur (magit-current-section))
              (parent (oref cur parent))
              (type (oref cur type)))
    (magit-get-section
     (append `((,type . ,value)) (magit-section-ident parent)))))

(defun majutsu-selection-session-begin (&rest _args)
  "Create a transient selection session for the current buffer.

Arguments are ignored for backward compatibility."
  (majutsu-selection-session-create
   :buffer (current-buffer)
   :overlays (make-hash-table :test 'equal)))

(defalias 'majutsu-selection-setup-scope 'majutsu-selection-session-begin)

(defun majutsu-selection--find-option (key)
  (when (boundp 'transient--suffixes)
    (seq-find (lambda (obj)
                (and (cl-typep obj 'majutsu-selection-option)
                     (eq (oref obj selection-key) key)))
              transient--suffixes)))

(defun majutsu-selection-values (key)
  "Return selected values for category KEY."
  (when-let* ((obj (majutsu-selection--find-option key)))
    (let ((val (oref obj value)))
      (if (listp val) val (list val)))))

(defun majutsu-selection-count (key)
  "Return number of selected values for category KEY."
  (length (majutsu-selection-values key)))

(defun majutsu-selection--overlay-range (section)
  (let ((start (oref section start))
        (end (or (oref section content) (oref section end))))
    (and start end (cons start end))))

(defun majutsu-selection--render-overlay (session id labels locate-fn)
  (majutsu-selection--with-session-buffer session
    (let ((overlays (majutsu-selection-session-overlays session))
          (locate-fn (or locate-fn #'majutsu-selection--locate-default)))
      (if labels
          (when-let* ((located (funcall locate-fn id))
                      (range (cond ((consp located) located)
                                   ((eieio-object-p located)
                                    (majutsu-selection--overlay-range located)))))
            (let* ((existing (gethash id overlays))
                   (start (car range))
                   (end (cdr range)))
              (unless (and existing
                           (= (overlay-start existing) start)
                           (= (overlay-end existing) end))
                (when existing (delete-overlay existing))
                (setq existing (make-overlay start end nil t))
                (overlay-put existing 'evaporate t)
                (overlay-put existing 'priority '(nil . 50))
                (overlay-put existing 'majutsu-selection t)
                (puthash id existing overlays))
              (overlay-put existing 'before-string
                           (concat (mapconcat #'identity (nreverse labels) " ") " "))))
        (when-let* ((existing (gethash id overlays)))
          (delete-overlay existing)
          (remhash id overlays))))))

(defun majutsu-selection-render (&optional session)
  "Re-render selection overlays for the current buffer."
  (let* ((session (or session (transient-scope)))
         (buf (and session (majutsu-selection-session-buffer session))))
    (when (and buf (buffer-live-p buf) (boundp 'transient--suffixes))
      (let ((overlays (majutsu-selection-session-overlays session))
            (active-ids (make-hash-table :test 'equal)))
        (dolist (obj transient--suffixes)
          (when (cl-typep obj 'majutsu-selection-option)
            (let ((vals (oref obj value))
                  (label (oref obj selection-label))
                  (face (oref obj selection-face)))
              (when vals
                (unless (listp vals) (setq vals (list vals)))
                (dolist (id vals)
                  (let ((existing (gethash id active-ids)))
                    (puthash id (cons (cons (propertize (or label "") 'face face)
                                            (car existing))
                                      obj)
                             active-ids)))))))
        (maphash (lambda (id ov)
                   (unless (gethash id active-ids)
                     (delete-overlay ov)
                     (remhash id overlays)))
                 overlays)
        (maphash (lambda (id data)
                   (majutsu-selection--render-overlay
                    session id (car data) (oref (cdr data) locate-fn)))
                 active-ids)))))

(defun majutsu-selection-clear (&optional key)
  "Clear selections.
If KEY is non-nil, clear only that selection category."
  (interactive)
  (when (boundp 'transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-selection-option)
                 (or (null key)
                     (eq (oref obj selection-key) key)))
        (oset obj value nil)))
    (majutsu-selection-render)))

(defun majutsu-selection-toggle (key &optional values)
  "Toggle selected values in category KEY.
VALUES defaults to the target(s) at point."
  (interactive)
  (let* ((session (or (transient-scope) (user-error "No active selection session")))
         (obj (majutsu-selection--find-option key)))
    (unless obj
      (user-error "No selection option for key: %S" key))
    (unless values
      (majutsu-selection--with-session-buffer session
        (let ((fn (or (oref obj targets-fn) #'majutsu-selection--targets-default)))
          (setq values (funcall fn)))))
    (unless values
      (user-error "No selection target at point"))
    (let ((current (oref obj value))
          (type (oref obj selection-type)))
      (if (eq type 'single)
          (let ((new (car values))
                (old (if (listp current) (car current) current)))
            (setq current (if (equal old new) nil (list new))))
        (unless (listp current) (setq current (list current)))
        (dolist (id values)
          (setq current (if (member id current)
                            (delete id current)
                          (append current (list id))))))
      (transient-infix-set obj current))))

(defun majutsu-selection-select (key &optional values)
  "Select a single value in category KEY.
KEY must be a `single' selection category."
  (interactive)
  (let* ((obj (majutsu-selection--find-option key)))
    (unless (and obj (eq (oref obj selection-type) 'single))
      (user-error "Category %S is not single-select" key))
    (majutsu-selection-toggle key values)))

(defvar majutsu-selection--infix-syncing nil
  "Prevent infinite recursion when syncing selection options.")

(defclass majutsu-selection-toggle-option (majutsu-selection-option)
  ()
  "Option class for toggling selection at point.")

(cl-defmethod transient-infix-set ((obj majutsu-selection-option) value)
  (cl-call-next-method)
  (when (and (not majutsu-selection--infix-syncing)
             (boundp 'transient--suffixes)
             (consp transient--suffixes)
             (slot-boundp obj 'argument)
             (slot-boundp obj 'selection-key))
    (let ((majutsu-selection--infix-syncing t)
          (argument (oref obj argument))
          (selection-key (oref obj selection-key)))
      (dolist (other transient--suffixes)
        (when (and (not (eq other obj))
                   (cl-typep other 'majutsu-selection-option)
                   (slot-boundp other 'argument)
                   (slot-boundp other 'selection-key)
                   (equal (oref other argument) argument)
                   (eq (oref other selection-key) selection-key))
          (transient-infix-set other value)))))
  (majutsu-selection-render))

(cl-defmethod transient-infix-read ((obj majutsu-selection-toggle-option))
  (with-current-buffer (or (and (boundp 'transient--original-buffer)
                                (buffer-live-p transient--original-buffer)
                                transient--original-buffer)
                           (current-buffer))
    (let* ((id (or (magit-section-value-if 'jj-commit)
                   (user-error "No changeset at point")))
           (id (if (stringp id) id (format "%s" id)))
           (current (oref obj value))
           (type (oref obj selection-type)))
      (if (eq type 'single)
          (if (equal current id) nil id)
        (let ((list (if (listp current) current (list current))))
          (if (member id list)
              (delete id list)
            (append list (list id))))))))

(cl-defmethod transient-infix-value ((_obj majutsu-selection-toggle-option))
  nil)

(add-hook 'transient-setup-buffer-hook #'majutsu-selection--transient-setup-buffer)
(add-hook 'transient-post-exit-hook #'majutsu-selection--transient-post-exit)

(provide 'majutsu-selection)
;;; majutsu-selection.el ends here
