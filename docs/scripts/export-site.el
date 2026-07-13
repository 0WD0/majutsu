;;; export-site.el --- Export the Majutsu manual as ox-edn Envelope v1 -*- lexical-binding: t; -*-

(let ((ox-root (pop command-line-args-left))
      (repo-root (pop command-line-args-left))
      (source (pop command-line-args-left))
      (output (pop command-line-args-left)))
  (unless (and ox-root repo-root source output
               (null command-line-args-left))
    (error "usage: export-site.el OX_EDN_ROOT REPO_ROOT SOURCE OUTPUT"))
  (add-to-list 'load-path (expand-file-name ox-root))
  (require 'ox-edn)
  (make-directory (file-name-directory (expand-file-name output)) t)
  (ox-edn-export-file-v1
   (expand-file-name source)
   (expand-file-name output)
   (expand-file-name repo-root)
   'strict)
  (princ "DOCS_EXPORT=PASS schema=ox-edn/envelope-v1 source=docs/majutsu.org\n"))

;;; export-site.el ends here
