;;; majutsu-core.el --- Core aggregation for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library gathers foundational Majutsu pieces so other modules
;; can depend on a single entry point, reducing circular dependencies.

;;; Code:

(require 'majutsu-base)
(require 'majutsu-process)

;;; Custom groups

(defgroup majutsu-essentials nil
  "Options that most Majutsu users should consider."
  :group 'majutsu)

;;; Shared Transients

(transient-define-argument majutsu-transient-arg-ignore-immutable ()
  :description "Ignore immutable"
  :class 'transient-switch
  :shortarg "-I"
  :argument "--ignore-immutable")

(defgroup majutsu-modes nil
  "Modes used or provided by Majutsu."
  :group 'majutsu)

(defgroup majutsu-buffers nil
  "Options concerning Majutsu buffers."
  :group 'majutsu
  :group 'majutsu-modes)

(defgroup majutsu-faces nil
  "Faces used by Majutsu."
  :group 'majutsu
  :group 'faces)

(defgroup majutsu-extensions nil
  "Extensions to Majutsu."
  :group 'majutsu)

(defgroup majutsu-related nil
  "Options relevant to Majutsu but defined elsewhere."
  :group 'majutsu
  :group 'majutsu-extensions
  :group 'majutsu-essentials)

;;; _
(provide 'majutsu-core)
;;; majutsu-core.el ends here
