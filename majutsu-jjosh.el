;;; majutsu-jjosh.el --- Compile-time jjosh declarations -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Set MAJUTSU_BACKEND to jj or jjosh before compiling the whole package.
;; The selected backend is embedded in bytecode; changing the environment
;; or executable later does not change an existing build.

;;; Code:

(eval-and-compile
  (defconst majutsu-build-backend
    (eval-when-compile
      (pcase (getenv "MAJUTSU_BACKEND")
        ((or 'nil "jj") 'jj)
        ("jjosh" 'jjosh)
        (value (error "MAJUTSU_BACKEND must be jj or jjosh, not %S" value))))
    "Backend selected when this Majutsu package was compiled.
This is a build constant, not a runtime option.  Rebuild the entire package
and restart Emacs to change it.  Source-only loading uses MAJUTSU_BACKEND
when this library is first loaded; the default is upstream jj."))

(defmacro majutsu-jjosh (&rest body)
  "Include BODY only in a jjosh build of Majutsu.
BODY can contain declarations or ordinary expressions.  This macro expands
to BODY wrapped in `progn' for jjosh, and nil for upstream jj.  It emits no
runtime backend check, capability probe, or cache lookup."
  (declare (indent 0) (debug (body)))
  (when (eq majutsu-build-backend 'jjosh)
    (cons 'progn body)))

(provide 'majutsu-jjosh)
;;; majutsu-jjosh.el ends here
