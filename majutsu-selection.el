;;; majutsu-selection.el --- Selection control for magit-section  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This library provides magit-section selection for Majutsu.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)
(require 'transient)

(defclass majutsu-selection-option (transient-option)
  ((selection-label :initarg :selection-label :initform nil)
   (selection-face  :initarg :selection-face  :initform nil)
   (locate-fn       :initarg :locate-fn       :initform nil
                    :documentation "Resolve a selected value to a section or (START . END) range.")
   (targets-fn      :initarg :targets-fn      :initform nil
                    :documentation "Return the value(s) to select from point/region when toggling."))
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
         (buf (and (majutsu-selection-session-p session)
                   (majutsu-selection-session-buffer session))))
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

(defun majutsu-selection-find-section (value &optional type root)
  "Return the closest section matching VALUE.

When ROOT is non-nil, traverse from that section, otherwise from
`magit-root-section'.  TYPE defaults to the current section's type.
Exact matches are preferred; otherwise choose the nearest prefix match."
  (let* ((root (or root magit-root-section))
         (anchor (or (and-let* ((cur (magit-current-section)))
                       (oref cur start))
                     (point)))
         (type (or type
                   (when-let* ((cur (magit-current-section)))
                     (oref cur type))))
         (exact (and root value type
                     (magit-get-section
                      (append `((,type . ,value)) (magit-section-ident root)))))
         best
         best-dist)
    (or exact
        (progn
          (magit-map-sections
           (##let ((id (magit-section-value-if type %)))
                  (when (and id (stringp id)
                             (or (string-prefix-p id value)
                                 (string-prefix-p value id)))
                    (let* ((pos (oref % start))
                           (dist (abs (- pos anchor))))
                      (when (or (null best-dist) (< dist best-dist))
                        (setq best %)
                        (setq best-dist dist)))))
           root))
        best)))

(defun majutsu-selection-session-begin ()
  "Create a transient selection session for the current buffer.

Arguments are ignored for backward compatibility."
  (majutsu-selection-session-create
   :buffer (current-buffer)
   :overlays (make-hash-table :test 'equal)))

(defun majutsu-selection--selection-id (obj)
  "Return selection category identifier for OBJ."
  (when (slot-boundp obj 'argument)
    (oref obj argument)))

(defun majutsu-selection--options-for (id)
  "Return selection options with selection category ID."
  (when (and id (boundp 'transient--suffixes))
    (seq-filter (lambda (obj)
                  (and (cl-typep obj 'majutsu-selection-option)
                       (equal (majutsu-selection--selection-id obj) id)))
                transient--suffixes)))

(defun majutsu-selection--resolve-slot (obj slot)
  "Return SLOT value for OBJ or another option in the same group."
  (or (eieio-oref obj slot)
      (when-let* ((id (majutsu-selection--selection-id obj))
                  (options (majutsu-selection--options-for id)))
        (seq-some (lambda (other)
                    (and (not (eq other obj))
                         (eieio-oref other slot)))
                  options))))

(defun majutsu-selection--selection-multi-p (obj)
  "Return non-nil when OBJ's selection category is multi-value."
  (or (oref obj multi-value)
      (when-let* ((id (majutsu-selection--selection-id obj))
                  (options (majutsu-selection--options-for id)))
        (seq-some (lambda (other)
                    (oref other multi-value))
                  options))))

(defun majutsu-selection--resolve-locate-fn (obj)
  "Return locate function for OBJ's selection category."
  (majutsu-selection--resolve-slot obj 'locate-fn))

(defun majutsu-selection--resolve-targets-fn (obj)
  "Return targets function for OBJ's selection category."
  (majutsu-selection--resolve-slot obj 'targets-fn))

(defun majutsu-selection--find-option (id)
  (when (boundp 'transient--suffixes)
    (seq-find (lambda (obj)
                (and (cl-typep obj 'majutsu-selection-option)
                     (equal (majutsu-selection--selection-id obj) id)))
              transient--suffixes)))

(defun majutsu-selection--toggle-current (current values multi)
  (setq values (ensure-list values))
  (unless values
    (user-error "No selection target at point"))
  (if (not multi)
      (let ((new (car values))
            (old (if (listp current) (car current) current)))
        (if (equal old new) nil new))
    (unless (listp current)
      (setq current (and current (list current))))
    (dolist (id values)
      (setq current (if (member id current)
                        (delete id current)
                      (append current (list id)))))
    current))

(defun majutsu-selection-values (id)
  "Return selected values for category ID."
  (when-let* ((obj (majutsu-selection--find-option id)))
    (let ((val (oref obj value)))
      (if (listp val) val (list val)))))

(defun majutsu-selection-count (id)
  "Return number of selected values for category ID."
  (length (majutsu-selection-values id)))

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
                   (let* ((obj (cdr data))
                          (locate-fn (or (majutsu-selection--resolve-locate-fn obj)
                                         #'majutsu-selection--locate-default)))
                     (majutsu-selection--render-overlay
                      session id (car data) locate-fn)))
                  active-ids)))))

(defun majutsu-selection-clear (&optional id)
  "Clear selections.
If ID is non-nil, clear only that selection category."
  (interactive)
  (when (boundp 'transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-selection-option)
                 (or (null id)
                     (equal (majutsu-selection--selection-id obj) id)))
        (oset obj value nil)))
    (majutsu-selection-render)))

(defun majutsu-selection-toggle (id &optional values)
  "Toggle selected values in category ID.
VALUES defaults to the target(s) at point."
  (interactive)
  (let* ((session (or (transient-scope) (user-error "No active selection session")))
         (obj (majutsu-selection--find-option id)))
    (unless obj
      (user-error "No selection option for id: %S" id))
    (unless values
      (majutsu-selection--with-session-buffer session
        (let ((fn (or (majutsu-selection--resolve-targets-fn obj)
                      #'majutsu-selection--targets-default)))
          (setq values (funcall fn)))))
    (let ((current (majutsu-selection--toggle-current
                    (oref obj value) values
                    (majutsu-selection--selection-multi-p obj))))
      (transient-infix-set obj current))))

(defun majutsu-selection-select (id &optional values)
  "Select a single value in category ID.
ID must be a single selection category."
  (interactive)
  (let* ((obj (majutsu-selection--find-option id)))
    (unless (and obj (not (majutsu-selection--selection-multi-p obj)))
      (user-error "Category %S is not single-select" id))
    (majutsu-selection-toggle id values)))

(defvar majutsu-selection--infix-syncing nil
  "Prevent infinite recursion when syncing selection options.")

(defclass majutsu-selection-toggle-option (majutsu-selection-option)
  ((format :initform " %k %d"))
  "Option class for toggling selection at point.")

(cl-defmethod transient-infix-set ((obj majutsu-selection-option) value)
  (cl-call-next-method)
  (when (and (not majutsu-selection--infix-syncing)
             (boundp 'transient--suffixes)
             (consp transient--suffixes)
             (slot-boundp obj 'argument))
    (let ((majutsu-selection--infix-syncing t)
          (argument (oref obj argument)))
      (dolist (other transient--suffixes)
        (when (and (not (eq other obj))
                   (cl-typep other 'majutsu-selection-option)
                   (slot-boundp other 'argument)
                   (equal (oref other argument) argument))
          (transient-infix-set other value)))))
  (majutsu-selection-render))

;; TODO: 应该有更加准确的处理方式
(cl-defmethod transient-infix-read :around ((obj majutsu-selection-option))
  (let ((value (cl-call-next-method)))
    (if (majutsu-selection--selection-multi-p obj)
        (ensure-list value)
      value)))

(cl-defmethod transient-infix-read ((obj majutsu-selection-toggle-option))
  (with-current-buffer (or (and (boundp 'transient--original-buffer)
                                (buffer-live-p transient--original-buffer)
                                transient--original-buffer)
                           (current-buffer))
    (let* ((fn (or (majutsu-selection--resolve-targets-fn obj)
                   #'majutsu-selection--targets-default))
           (values (funcall fn)))
      (majutsu-selection--toggle-current
       (oref obj value) values (majutsu-selection--selection-multi-p obj)))))

(cl-defmethod transient-infix-value ((_obj majutsu-selection-toggle-option))
  nil)

(add-hook 'transient-setup-buffer-hook #'majutsu-selection--transient-setup-buffer)
(add-hook 'transient-post-exit-hook #'majutsu-selection--transient-post-exit)

(provide 'majutsu-selection)
;;; majutsu-selection.el ends here
