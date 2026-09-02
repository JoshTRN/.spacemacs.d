;; -*- lexical-binding: t; -*-

;;; markdown-indent.el --- Indent Markdown files similarly to org-indent

;;; Commentary:

;; Virtual indentation for `markdown-mode', in the spirit of `org-indent'.
;;
;; Indentation is applied lazily through `jit-lock', so only the text that is
;; actually displayed is ever measured.  Opening a large file costs nothing,
;; there is no background agent walking the buffer, and prefix strings are
;; computed once and shared between every line that needs them.
;;
;; Edits are repaired as they happen: ordinary insertions re-indent the lines
;; they touch, and a change to a heading re-indents everything down to the
;; next heading, which is exactly the span whose level that heading governs.

;;; Code:

(require 'jit-lock)

;; Defined by `define-minor-mode' at the end of this file.
(defvar markdown-indent-mode)

(defgroup markdown-indent nil
  "Options concerning dynamic virtual indentation for Markdown."
  :tag "Markdown Indent"
  :group 'markdown)

(defcustom markdown-indent-boundary-char ?\s
  "Character used at the boundary of the virtual indentation."
  :group 'markdown-indent
  :type 'character
  :initialize #'custom-initialize-default
  :set #'markdown-indent--set-option)

(defcustom markdown-indent-indentation-per-level 2
  "Indentation (in number of characters) per heading level."
  :group 'markdown-indent
  :type 'integer
  :initialize #'custom-initialize-default
  :set #'markdown-indent--set-option)

(defcustom markdown-indent-mode-turns-off-electric-indent t
  "If non-nil, disabling electric indent when `markdown-indent-mode' is on."
  :group 'markdown-indent
  :type 'boolean)

(defface markdown-indent
  '((t (:inherit shadow)))
  "Face for Markdown indentation."
  :group 'markdown-faces)

(defconst markdown-indent--deepest-level 50
  "Maximum Markdown heading depth to consider.")

(defconst markdown-indent--max-cached-indentation 200
  "Largest line indentation for which prefixes are cached.")

(defconst markdown-indent--heading-re "^\\(#+\\)[ \t]"
  "Regexp matching a heading line.  Group 1 is the leading hashes.")

(defconst markdown-indent--line-re
  "^\\(?:\\(#+\\)[ \t]\\|[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\)"
  "Regexp matching a heading or a list item at the beginning of a line.
Group 1 matches for headings only; when it is nil the whole match
covers the list bullet, so its end is the body column.")

(defvar markdown-indent--heading-prefixes nil
  "Vector of prefix strings for heading lines, indexed by level.")

(defvar markdown-indent--text-prefixes nil
  "Vector of prefix strings for normal lines, indexed by level.")

(defvar markdown-indent--prefix-cache (make-hash-table :test #'eql)
  "Cache of ready-made `line-prefix'/`wrap-prefix' property lists.")

(defvar-local markdown-indent--heading-changed nil
  "Non-nil when a heading line is about to be, or has just been, modified.")


;;; Prefix strings

(defun markdown-indent--compute-prefixes ()
  "Compute the prefix strings for text and heading lines."
  (setq markdown-indent--heading-prefixes
        (make-vector markdown-indent--deepest-level nil)
        markdown-indent--text-prefixes
        (make-vector markdown-indent--deepest-level nil))
  (clrhash markdown-indent--prefix-cache)
  (dotimes (n markdown-indent--deepest-level)
    (aset markdown-indent--heading-prefixes n
          (propertize (make-string (* (max 0 (1- n))
                                      markdown-indent-indentation-per-level)
                                   ?\s)
                      'face 'markdown-indent))
    (aset markdown-indent--text-prefixes n
          (propertize
           (concat (make-string (* n markdown-indent-indentation-per-level) ?\s)
                   (char-to-string markdown-indent-boundary-char))
           'face 'markdown-indent))))

(defun markdown-indent--set-option (symbol value)
  "Set SYMBOL to VALUE, then re-indent every buffer using the mode."
  (set-default symbol value)
  (when (and (boundp 'markdown-indent-boundary-char)
             (boundp 'markdown-indent-indentation-per-level))
    (markdown-indent--compute-prefixes)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p markdown-indent-mode)
          (markdown-indent-remove-properties (point-min) (point-max))
          (jit-lock-refontify))))))

(defsubst markdown-indent--cache-key (level indentation heading)
  "Return the cache key for LEVEL, INDENTATION and HEADING."
  (+ (* (+ (* level 2) (if heading 1 0))
        markdown-indent--max-cached-indentation)
     indentation))

(defun markdown-indent--prefix-props (level indentation heading)
  "Return a property list indenting a line at LEVEL by INDENTATION.
HEADING, if non-nil, indicates the line is a heading.  The list is
shared between all lines of the same shape, so applying it costs no
allocation at all."
  (let* ((level (min level (1- markdown-indent--deepest-level)))
         (indentation (max 0 indentation))
         (cacheable (< indentation markdown-indent--max-cached-indentation))
         (key (and cacheable
                   (markdown-indent--cache-key level indentation heading))))
    (or (and cacheable (gethash key markdown-indent--prefix-cache))
        (let* ((base (aref (if heading
                               markdown-indent--heading-prefixes
                             markdown-indent--text-prefixes)
                           level))
               (props (list 'line-prefix base
                            'wrap-prefix
                            (if (zerop indentation)
                                base
                              (propertize
                               (concat base (make-string indentation ?\s))
                               'face 'markdown-indent)))))
          (when cacheable
            (puthash key props markdown-indent--prefix-cache))
          props))))


;;; Applying indentation

(defun markdown-indent-remove-properties (beg end)
  "Remove indentation properties between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(defun markdown-indent--enclosing-level (pos)
  "Return the heading level in effect on the line before POS, or 0."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (forward-line 0)
      (if (re-search-backward markdown-indent--heading-re nil t)
          (min (- (match-end 1) (match-beginning 1))
               (1- markdown-indent--deepest-level))
        0))))

(defun markdown-indent--apply (start end)
  "Indent every line overlapping START to END.
Return the region actually covered as a cons cell.  Runs of lines
sharing the same prefixes are propertized in a single pass."
  (save-match-data
    (save-excursion
      (goto-char start)
      (forward-line 0)
      (let* ((beg (point))
             (limit (progn (goto-char end)
                           (if (bolp) (point) (line-beginning-position 2))))
             (level (markdown-indent--enclosing-level beg))
             (run-beg beg)
             (run-props nil))
        (goto-char beg)
        (with-silent-modifications
          (while (< (point) limit)
            (let ((bol (point))
                  (props nil))
              (cond
               ((looking-at markdown-indent--line-re)
                (if (match-beginning 1)
                    (setq level (min (- (match-end 1) (match-beginning 1))
                                     (1- markdown-indent--deepest-level))
                          props (markdown-indent--prefix-props level 0 t))
                  ;; List item: wrapped text lines up with the item body.
                  (goto-char (match-end 0))
                  (setq props (markdown-indent--prefix-props
                               level (current-column) nil))))
               (t
                (setq props (markdown-indent--prefix-props
                             level (current-indentation) nil))))
              (unless (eq props run-props)
                (when run-props
                  (add-text-properties run-beg bol run-props))
                (setq run-beg bol
                      run-props props))
              (goto-char bol)
              (forward-line 1)))
          (when run-props
            (add-text-properties run-beg (point) run-props)))
        (cons beg (point))))))

(defun markdown-indent--jit-lock (start end)
  "Indent the lines between START and END on behalf of `jit-lock'."
  (let ((bounds (markdown-indent--apply start end)))
    `(jit-lock-bounds ,(car bounds) . ,(cdr bounds))))


;;; Keeping up with edits

(defun markdown-indent--heading-in-region-p (beg end)
  "Return non-nil if any line touched by BEG to END is a heading."
  (save-excursion
    (save-match-data
      (goto-char (min beg end))
      (forward-line 0)
      (let ((limit (save-excursion
                     (goto-char (min (max beg end) (point-max)))
                     (line-end-position))))
        (re-search-forward markdown-indent--heading-re limit t)))))

(defun markdown-indent--invalidate (beg end)
  "Mark every line between BEG and END as needing indentation again."
  (save-excursion
    (let ((start (progn (goto-char (min beg (point-max)))
                        (line-beginning-position)))
          (finish (progn (goto-char (min end (point-max)))
                         (line-beginning-position 2))))
      (jit-lock-refontify start finish))))

(defun markdown-indent--next-heading (pos)
  "Return the start of the first heading line after POS's line.
Fall back to `point-max' when there is none."
  (save-excursion
    (save-match-data
      (goto-char (min pos (point-max)))
      (forward-line 1)
      (if (re-search-forward markdown-indent--heading-re nil t)
          (match-beginning 0)
        (point-max)))))

(defun markdown-indent--note-heading-change (beg end)
  "Record whether the text about to change between BEG and END is a heading."
  (when markdown-indent-mode
    (setq markdown-indent--heading-changed
          (or markdown-indent--heading-changed
              (and (markdown-indent--heading-in-region-p beg end) t)))))

(defun markdown-indent--after-change (beg end _len)
  "Re-indent what the change between BEG and END affected."
  (when markdown-indent-mode
    (let ((heading (or markdown-indent--heading-changed
                       (markdown-indent--heading-in-region-p beg end))))
      (setq markdown-indent--heading-changed nil)
      (if heading
          ;; A heading was gained, lost or re-levelled: every following line
          ;; takes its indentation from it, up to the next heading.
          (markdown-indent--invalidate beg (markdown-indent--next-heading end))
        (markdown-indent--invalidate beg end)))))


;;; Mode

;;;###autoload
(define-minor-mode markdown-indent-mode
  "Minor mode to visually indent Markdown text based on heading levels."
  :lighter " Md-Indent"
  (cond
   (markdown-indent-mode
    (when markdown-indent-mode-turns-off-electric-indent
      (setq-local electric-indent-mode nil))
    (unless markdown-indent--heading-prefixes
      (markdown-indent--compute-prefixes))
    (setq markdown-indent--heading-changed nil)
    (add-hook 'before-change-functions #'markdown-indent--note-heading-change nil t)
    (add-hook 'after-change-functions #'markdown-indent--after-change nil t)
    (jit-lock-register #'markdown-indent--jit-lock)
    (jit-lock-refontify))
   (t
    (remove-hook 'before-change-functions #'markdown-indent--note-heading-change t)
    (remove-hook 'after-change-functions #'markdown-indent--after-change t)
    (jit-lock-unregister #'markdown-indent--jit-lock)
    (markdown-indent-remove-properties (point-min) (point-max)))))

(provide 'markdown-indent)
;;; markdown-indent.el ends here
