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
(require 'crm)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'majutsu-transient)

(defclass majutsu-selection-option (majutsu-transient-key-alias-suffix transient-option)
  ((selection-label         :initarg :selection-label         :initform nil)
   (selection-face          :initarg :selection-face          :initform nil)
   (locate-fn               :initarg :locate-fn               :initform nil
                            :documentation "Resolve a selected value to a section or (START . END) range.")
   (targets-fn              :initarg :targets-fn              :initform nil
                            :documentation "Return the value(s) to select from point/region when toggling.")
   (selection-toggle-key    :initarg :selection-toggle-key    :initform nil
                            :documentation "Additional key used to toggle values at point.")
   (selection-toggle-if-not :initarg :selection-toggle-if-not :initform nil
                            :documentation "Disable the toggle key when this predicate is non-nil."))
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

(defun majutsu-selection--cleanup-overlays-in-buffer (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (remove-overlays (point-min) (point-max) 'majutsu-selection t)
      (setq majutsu-selection--active-session nil))))

(defmacro majutsu-selection--with-session-buffer (session &rest body)
  "Run BODY in SESSION's buffer."
  (declare (indent 1) (debug (form &rest form)))
  (let ((buf (make-symbol "buf")))
    `(let ((,buf (and ,session (majutsu-selection-session-buffer ,session))))
       (unless (buffer-live-p ,buf)
         (user-error "Selection buffer is no longer live"))
       (with-current-buffer ,buf
         ,@body))))

(defun majutsu-selection--transient-setup-buffer ()
  "Render selection overlays when a transient menu is being setup."
  (let* ((session (transient-scope))
         (buf (and (majutsu-selection-session-p session)
                   (majutsu-selection-session-buffer session))))
    ;; Clean up old buffers
    (dolist (old-buf (hash-table-keys majutsu-selection--overlay-buffers))
      (unless (and (eq old-buf buf) (buffer-live-p old-buf))
        (majutsu-selection--cleanup-overlays-in-buffer old-buf)
        (remhash old-buf majutsu-selection--overlay-buffers)))
    ;; Setup active buffer
    (when (buffer-live-p buf)
      (puthash buf t majutsu-selection--overlay-buffers)
      (with-current-buffer buf
        (unless (eq majutsu-selection--active-session session)
          (majutsu-selection--cleanup-overlays-in-buffer buf)
          (setq majutsu-selection--active-session session)
          (majutsu-selection-render session))))))

(defun majutsu-selection--transient-post-exit ()
  "Remove selection overlays when leaving the transient stack."
  (dolist (buf (hash-table-keys majutsu-selection--overlay-buffers))
    (majutsu-selection--cleanup-overlays-in-buffer buf))
  (clrhash majutsu-selection--overlay-buffers))

(defun majutsu-selection-session-end-if-owner ()
  "Remove selection overlays in the current buffer."
  (remhash (current-buffer) majutsu-selection--overlay-buffers)
  (majutsu-selection--cleanup-overlays-in-buffer (current-buffer)))

(defun majutsu-selection--targets-default ()
  "Default selection target's value."
  (or (magit-region-values nil t)
      (when-let* ((section (magit-current-section))
                  (value (magit-section-ident-value section)))
        (list value))))

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
                  (when (and (stringp value)
                             (stringp id)
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
  "Create a transient selection session for the current buffer."
  (majutsu-selection-session-create
   :buffer (current-buffer)
   :overlays (make-hash-table :test 'equal)))

(defun majutsu-selection--selection-id (obj)
  "Return selection category identifier for OBJ."
  (when (slot-boundp obj 'argument)
    (oref obj argument)))

(defun majutsu-selection--selection-multi-p (obj)
  "Return non-nil when OBJ's selection category is multi-value."
  (oref obj multi-value))

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

(defun majutsu-selection--render-overlay (session id labels locate-fn)
  (majutsu-selection--with-session-buffer session
    (let* ((overlays (majutsu-selection-session-overlays session))
           (range (when labels
                    (when-let* ((located (funcall locate-fn id)))
                      (cond
                       ((and (consp located) (car located) (cdr located))
                        located)
                       ((eieio-object-p located)
                        (let ((start (oref located start))
                              (end (or (oref located content) (oref located end))))
                          (and start end (cons start end)))))))))
      (if range
          (let* ((existing (gethash id overlays))
                 (start (car range))
                 (end (cdr range)))
            (unless (and existing
                         (overlay-buffer existing)
                         (= (overlay-start existing) start)
                         (= (overlay-end existing) end))
              (when existing (delete-overlay existing))
              (setq existing (make-overlay start end nil t))
              (overlay-put existing 'evaporate t)
              (overlay-put existing 'priority '(nil . 50))
              (overlay-put existing 'majutsu-selection t)
              (puthash id existing overlays))
            (overlay-put existing 'before-string
                         (concat (mapconcat #'identity (nreverse labels) " ") " ")))
        (when-let* ((existing (gethash id overlays)))
          (delete-overlay existing)
          (remhash id overlays))))))

(defun majutsu-selection-render (&optional session)
  "Re-render selection overlays for the current buffer."
  (let* ((session (or session (transient-scope)))
         (buf (and (majutsu-selection-session-p session)
                   (majutsu-selection-session-buffer session))))
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
        (dolist (id (hash-table-keys overlays))
          (unless (gethash id active-ids)
            (delete-overlay (gethash id overlays))
            (remhash id overlays)))
        (maphash (lambda (id data)
                   (let* ((obj (cdr data))
                          (locate-fn (or (oref obj locate-fn)
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

(cl-defmethod majutsu-transient-key-aliases ((obj majutsu-selection-option))
  (append (cl-call-next-method)
          (and (majutsu-selection--toggle-active-p obj)
               (ensure-list (oref obj selection-toggle-key)))))

(defun majutsu-selection--toggle-active-p (obj)
  "Return non-nil when OBJ's toggle key is active."
  (if-let* ((predicate (oref obj selection-toggle-if-not)))
      (not (funcall predicate))
    t))

(defun majutsu-selection--toggle-key-p (obj)
  "Return non-nil when OBJ's toggle key invoked the current command."
  (and (majutsu-selection--toggle-active-p obj)
       (seq-some #'majutsu-transient-key-invoked-p
                 (ensure-list (oref obj selection-toggle-key)))))

(defun majutsu-selection--read-toggle (obj)
  "Return OBJ's value after toggling the target at point."
  (let* ((fn (or (oref obj targets-fn)
                 #'majutsu-selection--targets-default))
         (values (funcall fn)))
    (majutsu-selection--toggle-current
     (oref obj value) values (majutsu-selection--selection-multi-p obj))))

(cl-defmethod transient-init-value :after ((obj majutsu-selection-option))
  (when (and (eq (oref obj multi-value) 'repeat)
             (slot-boundp obj 'argument)
             (listp (oref obj value)))
    (let ((argument (oref obj argument)))
      (oset obj value
            (mapcar (lambda (value)
                      (or (and (stringp value)
                               (transient-arg-value argument (list value)))
                          value))
                    (oref obj value))))))

(cl-defmethod transient-infix-set ((_obj majutsu-selection-option) _value)
  (cl-call-next-method)
  (majutsu-selection-render))

(cl-defmethod transient-infix-read :around ((obj majutsu-selection-option))
  (let* ((session (transient-scope))
         (value (if (majutsu-selection-session-p session)
                    (majutsu-selection--with-session-buffer session
                      (if (majutsu-selection--toggle-key-p obj)
                          (majutsu-selection--read-toggle obj)
                        (cl-call-next-method)))
                  (if (majutsu-selection--toggle-key-p obj)
                      (majutsu-selection--read-toggle obj)
                    (cl-call-next-method)))))
    (if (not (majutsu-selection--selection-multi-p obj))
        value
      (cond
       ((null value) nil)
       ((listp value) value)
       ((and (eq (oref obj multi-value) 'repeat)
             (stringp value))
        (split-string value crm-separator t))
       (t (list value))))))

(add-hook 'transient-setup-buffer-hook #'majutsu-selection--transient-setup-buffer)
(add-hook 'transient-post-exit-hook #'majutsu-selection--transient-post-exit)

(provide 'majutsu-selection)
;;; majutsu-selection.el ends here
