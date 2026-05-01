;;; majutsu-graph-record.el --- Graph-aware structured jj records  -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for jj commands whose template output is prefixed by jj's graph
;; renderer.  This mirrors the core idea of `majutsu-log': a visible graph
;; payload is followed by hidden machine metadata in the same graph entry, with
;; Majutsu-owned tokens marking record boundaries.

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

(defun majutsu-graph-record--metadata-token (name)
  "Return the metadata token for graph record NAME."
  (concat majutsu-graph-record--marker "GR:" name ":M"))

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

(defun majutsu-graph-record-template (name fields self-type display)
  "Return a jj template for graph record NAME.
DISPLAY is the visible graph payload.  FIELDS are hidden machine-readable
metadata fields emitted after DISPLAY and a metadata token.  Empty fields are
preserved because the separator is emitted explicitly between all fields."
  (majutsu-template-compile
   `[:concat
     ,(majutsu-graph-record--start-token name)
     ,display
     ,(majutsu-graph-record--metadata-token name)
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

(defun majutsu-graph-record--line-string ()
  "Return the current line, preserving text properties and omitting newline."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun majutsu-graph-record-line-at-point-p (name)
  "Return non-nil when point is on a graph record NAME line."
  (let* ((line (majutsu-graph-record--line-string))
         (token (majutsu-graph-record--start-token name)))
    (majutsu-graph-record--token-position token line)))

(defun majutsu-graph-record--join-prefixed-lines (lines)
  "Join LINES of (PREFIX . CONTENT), preserving string properties."
  (if (null lines)
      ""
    (let* ((first (car lines))
           (out (concat (car first) (cdr first))))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" (car line) (cdr line)))))))

(defun majutsu-graph-record--line-prefix-pair (line prefix-width)
  "Split LINE into (PREFIX . CONTENT) using PREFIX-WIDTH."
  (let* ((width (max 0 (min prefix-width (length (or line "")))))
         (text (or line "")))
    (cons (substring text 0 width)
          (substring text width))))

(defun majutsu-graph-record-parse-at-point (name)
  "Parse graph record NAME at point.
Return a plist containing `:beg', `:end', `:graph-prefix', `:payload',
`:display-lines', and `:display'.  `:payload' contains hidden metadata fields.
`:display' is the visible text with jj graph prefixes preserved and record
tokens removed.  Point moves to the end of the consumed raw record region."
  (let* ((beg (line-beginning-position))
         (start-token (majutsu-graph-record--start-token name))
         (metadata-token (majutsu-graph-record--metadata-token name))
         (end-token (majutsu-graph-record--end-token name))
         (bol beg)
         (start (majutsu-graph-record--token-position start-token
                                                      (majutsu-graph-record--line-string))))
    (when start
      (let* ((indent start)
             (graph-prefix (buffer-substring bol (+ bol start)))
             (first-line t)
             display-lines payload done)
        (while (and (not done) (not (eobp)))
          (setq bol (line-beginning-position))
          (let* ((line (majutsu-graph-record--line-string))
                 (prefix-end (min indent (length line)))
                 (prefix (substring line 0 prefix-end))
                 (content-start (if first-line
                                    (+ start (length start-token))
                                  prefix-end))
                 (metadata-pos (majutsu-graph-record--token-position
                                metadata-token line content-start)))
            (if metadata-pos
                (let* ((visible (substring line content-start metadata-pos))
                       (payload-start (+ metadata-pos (length metadata-token)))
                       (end-pos (majutsu-graph-record--token-position
                                 end-token line payload-start)))
                  (unless (and (string-empty-p visible)
                               (string-empty-p prefix))
                    (push (cons prefix visible) display-lines))
                  (when end-pos
                    (setq payload (substring line payload-start end-pos)
                          done t)
                    (forward-line 1)))
              (push (cons prefix (substring line content-start)) display-lines)
              (forward-line 1)))
          (setq first-line nil))
        (when done
          (while (and (not (eobp))
                      (not (majutsu-graph-record-line-at-point-p name)))
            (push (majutsu-graph-record--line-prefix-pair
                   (majutsu-graph-record--line-string) indent)
                  display-lines)
            (forward-line 1))
          (setq display-lines (nreverse display-lines))
          (list :beg beg
                :end (point)
                :graph-prefix graph-prefix
                :payload payload
                :display-lines display-lines
                :display (majutsu-graph-record--join-prefixed-lines
                          display-lines)))))))

(provide 'majutsu-graph-record)
;;; majutsu-graph-record.el ends here
