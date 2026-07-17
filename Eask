;; -*- mode: eask; lexical-binding: t -*-

(package "majutsu"
         "0.6.0"
         "Interface to jj version control system")

(website-url "https://github.com/0WD0/majutsu")
(keywords "vc" "tools")

(package-file "majutsu.el")

;; Test helpers (e.g. majutsu-jj-integration) live under test/ and are
;; outside the package files-spec, so expose them on load-path for ERT.
(load-paths "./test/")

(script "test" "eask test ert test/*-test.el")

(source "gnu")

(depends-on "emacs" "29.1")
(depends-on "compat" "31.0.0.1")
(depends-on "magit" "3.3.0")
(depends-on "consult" "1.0")
(depends-on "transient" "0.5.0")
(depends-on "plz" "0.9.1")

(source 'melpa)
(source 'jcs-elpa)

(development
 (depends-on "ellsp"))
