;;; majutsu-graph-record.el --- Graph-aware structured jj records  -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for jj commands whose template output is prefixed by jj's graph
;; renderer.  The template starts with a control token, so the text before that
;; token belongs to the graph prefix, while the text after it is Majutsu-owned
;; structured payload.

;;; Code:

(require 'majutsu-template)
(require 'subr-x)

(defconst majutsu-graph-record-field-separator "\x1e"
  "Separator character inserted between fields inside a graph record payload.")

(defconst majutsu-graph-record--marker "\x1d"
  "Control marker prefix for graph record boundaries.")

(defun majutsu-graph-record--start-token (name)
  "Return the start token for graph record NAME."
  (concat majutsu-graph-record--marker "GR:" name ":S"))

(defun majutsu-graph-record--end-token (name)
  "Return the end token for graph record NAME."
  (concat majutsu-graph-record--marker "GR:" name ":E"))

(defun majutsu-graph-record--interpose (separator values)
  "Return VALUES with SEPARATOR inserted between elements."
  (let (out)
    (dolist (value values)
      (when out
        (push separator out))
      (push value out))
    (nreverse out)))

(defun majutsu-graph-record-template (name fields self-type)
  "Return a jj template for graph record NAME with FIELDS and SELF-TYPE.
FIELDS are Majutsu template DSL forms.  Empty fields are preserved because the
separator is emitted explicitly between all fields."
  (majutsu-template-compile
   `[:concat
     ,(majutsu-graph-record--start-token name)
     ,@(majutsu-graph-record--interpose
        majutsu-graph-record-field-separator
        fields)
     ,(majutsu-graph-record--end-token name)
     "\n"]
   self-type))

(defun majutsu-graph-record-split-fields (payload expected)
  "Split PAYLOAD into EXPECTED fields, preserving empty fields."
  (let ((fields (split-string (or payload "") majutsu-graph-record-field-separator)))
    (and (= (length fields) expected) fields)))

(defun majutsu-graph-record--token-position (token string &optional start)
  "Return TOKEN position in STRING at START, or nil."
  (string-match-p (regexp-quote token) string (or start 0)))

(defun majutsu-graph-record-parse-line (name line)
  "Parse one graph record NAME from LINE.
Return a plist with `:graph-prefix' and `:payload', preserving text
properties, or nil when LINE does not contain a complete record."
  (let* ((start-token (majutsu-graph-record--start-token name))
         (end-token (majutsu-graph-record--end-token name))
         (start (majutsu-graph-record--token-position start-token line))
         (payload-start (and start (+ start (length start-token))))
         (end (and payload-start
                   (majutsu-graph-record--token-position end-token line payload-start))))
    (when (and start end)
      (list :graph-prefix (substring line 0 start)
            :payload (substring line payload-start end)))))

(defun majutsu-graph-record--line-string ()
  "Return the current line, preserving text properties and omitting newline."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun majutsu-graph-record-line-at-point-p (name)
  "Return non-nil when point is on a graph record NAME line."
  (majutsu-graph-record-parse-line name (majutsu-graph-record--line-string)))

(defun majutsu-graph-record-parse-at-point (name)
  "Parse graph record NAME at point.
Return a plist containing `:beg', `:end', `:graph-prefix', `:payload', and
`:suffix-lines'.  Suffix lines are graph continuation lines following this
record until the next record line.  Point moves to the end of the consumed raw
record region."
  (let* ((beg (line-beginning-position))
         (line (majutsu-graph-record--line-string))
         (record (majutsu-graph-record-parse-line name line)))
    (when record
      (forward-line 1)
      (let (suffix-lines)
        (while (and (not (eobp))
                    (not (majutsu-graph-record-line-at-point-p name)))
          (push (majutsu-graph-record--line-string) suffix-lines)
          (forward-line 1))
        (append record
                (list :beg beg
                      :end (point)
                      :suffix-lines (nreverse suffix-lines)))))))

(provide 'majutsu-graph-record)
;;; majutsu-graph-record.el ends here
