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

(defun majutsu-graph-record--payload-token (name)
  "Return the payload token for graph record NAME."
  (concat majutsu-graph-record--marker "GR:" name ":P"))

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

(defun majutsu-graph-record-template (name fields self-type &optional display)
  "Return a jj template for graph record NAME with FIELDS and SELF-TYPE.
FIELDS are hidden Majutsu template DSL forms.  Empty fields are preserved
because the separator is emitted explicitly between all fields.  DISPLAY is an
optional visible template payload; when present it may contain newlines and is
separated from hidden fields by a payload token."
  (majutsu-template-compile
   `[:concat
     ,(majutsu-graph-record--start-token name)
     ,@(majutsu-graph-record--interpose
        majutsu-graph-record-field-separator
        fields)
     ,@(when display
         (list (majutsu-graph-record--payload-token name) display))
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

(defun majutsu-graph-record--join-lines (lines)
  "Join LINES with literal newlines, preserving string properties."
  (if (null lines)
      ""
    (let ((out (car lines)))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" line))))))

(defun majutsu-graph-record-parse-at-point (name)
  "Parse graph record NAME at point.
Return a plist containing `:beg', `:end', `:graph-prefix', `:payload', and
`:display'.  `:payload' is the hidden field payload.  `:display' is the visible
text with jj graph prefixes preserved and record tokens removed.  Point moves
to the end of the consumed raw record region."
  (let* ((beg (line-beginning-position))
         (start-token (majutsu-graph-record--start-token name))
         (payload-token (majutsu-graph-record--payload-token name))
         (end-token (majutsu-graph-record--end-token name))
         (line (majutsu-graph-record--line-string))
         (start (majutsu-graph-record--token-position start-token line))
         display-start end fields display-lines)
    (when start
      (let* ((graph-prefix (substring line 0 start))
             (after-start (+ start (length start-token)))
             (payload-pos (majutsu-graph-record--token-position
                           payload-token line after-start)))
        (when payload-pos
          (setq fields (substring line after-start payload-pos)
                display-start (+ payload-pos (length payload-token))))
        (setq end (majutsu-graph-record--token-position
                   end-token line (or display-start after-start)))
        (if end
            (progn
              (setq fields (or fields (substring line after-start end)))
              (when display-start
                (push (concat graph-prefix (substring line display-start end))
                      display-lines))
              (forward-line 1))
          (setq fields (or fields (substring line after-start))
                display-lines (and display-start
                                   (list (concat graph-prefix
                                                 (substring line display-start)))))
          (forward-line 1)
          (while (and (not (eobp)) (not end))
            (setq line (majutsu-graph-record--line-string)
                  end (majutsu-graph-record--token-position end-token line))
            (if end
                (progn
                  (push (substring line 0 end) display-lines)
                  (forward-line 1))
              (push line display-lines)
              (forward-line 1))))
        (when end
          (list :beg beg
                :end (point)
                :graph-prefix graph-prefix
                :payload fields
                :display (majutsu-graph-record--join-lines
                          (nreverse display-lines))))))))

(provide 'majutsu-graph-record)
;;; majutsu-graph-record.el ends here
