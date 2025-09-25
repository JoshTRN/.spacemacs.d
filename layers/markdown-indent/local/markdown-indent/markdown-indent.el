;;; markdown-indent.el --- Indent Markdown files similarly to org-indent

(require 'cl-lib)

(defgroup markdown-indent nil
  "Options concerning dynamic virtual indentation for Markdown."
  :tag "Markdown Indent"
  :group 'markdown)

(defcustom markdown-indent-boundary-char ?\s
  "Character used at the boundary of the virtual indentation."
  :group 'markdown-indent
  :type 'character)

(defcustom markdown-indent-indentation-per-level 2
  "Indentation (in number of characters) per heading level."
  :group 'markdown-indent
  :type 'integer)

(defcustom markdown-indent-mode-turns-off-electric-indent t
  "If non-nil, disabling electric indent when `markdown-indent-mode' is on."
  :group 'markdown-indent
  :type 'boolean)

(defcustom markdown-indent-post-buffer-init-functions nil
  "Hook run after `markdown-indent-mode' finishes initializing a buffer.
Each function must accept a single argument, the initialized buffer."
  :group 'markdown-indent
  :type 'hook)

(defface markdown-indent
  '((t (:inherit shadow)))
  "Face for Markdown indentation."
  :group 'markdown-faces)

(defvar markdown-indent--initial-marker nil
  "Position of initialization before interruption.")

(defvar markdown-indent-agent-timer nil
  "Timer for background indentation process.")

(defvar markdown-indent-agentized-buffers nil
  "List of buffers awaiting background indentation.")

(defvar markdown-indent-agent-resume-timer nil
  "Timer to resume indentation after yielding to other idle timers.")

(defvar markdown-indent-agent-active-delay '(0 2 0)
  "Time to run indentation aggressively if the buffer is current.")

(defvar markdown-indent-agent-passive-delay '(0 0 400000)
  "Time to run indentation passively if the buffer is not current.")

(defvar markdown-indent-agent-resume-delay '(0 0 100000)
  "Minimal time for other idle processes before resuming indentation.")

(defconst markdown-indent--deepest-level 50
  "Maximum Markdown heading depth to consider.")

(defvar markdown-indent--heading-line-prefixes nil
  "Vector of prefix strings for heading lines.")

(defvar markdown-indent--text-line-prefixes nil
  "Vector of prefix strings for normal lines.")

(defvar markdown-indent--modified-headline-flag nil
  "Non-nil if a heading was just modified or deleted.")

(defun markdown-indent--heading-level ()
  "Return the level of the Markdown heading at point, or nil if none."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(#+\\)\\s-+")
      (length (match-string 1)))))

(defun markdown-indent--is-list-item-p ()
  "Return non-nil if point is at a Markdown list item."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*\\([-+*]\\|[0-9]+[.)]\\)\\s-+")))

(defun markdown-indent--list-item-body-column ()
  "Return the column where list item body begins, or nil if not a list."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\s-*\\([-+*]\\|[0-9]+[.)]\\)\\s-+")
      (goto-char (match-end 0))
      (current-column))))

(defun markdown-indent--compute-prefixes ()
  "Compute prefix strings for text and heading lines."
  (setq markdown-indent--heading-line-prefixes
        (make-vector markdown-indent--deepest-level nil)
        markdown-indent--text-line-prefixes
        (make-vector markdown-indent--deepest-level nil))
  (dotimes (n markdown-indent--deepest-level)
    (let ((indent (* (max 0 (1- n)) markdown-indent-indentation-per-level)))
      (aset markdown-indent--heading-line-prefixes n
            (propertize (make-string indent ?\s) 'face 'markdown-indent))
      (aset markdown-indent--text-line-prefixes n
            (propertize
             (concat (make-string (* n markdown-indent-indentation-per-level) ?\s)
                     (char-to-string markdown-indent-boundary-char))
             'face 'markdown-indent)))))

(defun markdown-indent-remove-properties (beg end)
  "Remove indentation properties between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(defun markdown-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
                          '(line-prefix nil wrap-prefix nil)
                          string)
  string)

(defun markdown-indent-set-line-properties (level indentation &optional heading)
  "Set prefix properties on current line, then move to next.
LEVEL is heading depth, 0 for body text.
INDENTATION is indentation for wrapping text.
HEADING, if non-nil, indicates current line is a heading."
  (let* ((base (if heading
                   (aref markdown-indent--heading-line-prefixes level)
                 (aref markdown-indent--text-line-prefixes level)))
         (wrap (propertize (concat base (make-string indentation ?\s))
                           'face 'markdown-indent)))
    (add-text-properties
     (line-beginning-position) (line-beginning-position 2)
     `(line-prefix ,base wrap-prefix ,wrap)))
  (forward-line))

(defun markdown-indent-add-properties (beg end &optional delay)
  "Add indentation properties between BEG and END.
If DELAY is non-nil, yield after that duration."
  (save-match-data
    (save-excursion
      (goto-char beg)
      (forward-line 0)
      (let ((level (or (markdown-indent--heading-level) 0))
            (time-limit (and delay (time-add nil delay))))
        (with-silent-modifications
          (while (and (<= (point) end) (not (eobp)))
            (cond
             ((and delay (input-pending-p))
              (throw 'interrupt (point)))
             ((and delay (time-less-p time-limit nil))
              (setq markdown-indent-agent-resume-timer
                    (run-with-idle-timer
                     (time-add (current-idle-time) markdown-indent-agent-resume-delay)
                     nil #'markdown-indent-initialize-agent))
              (throw 'interrupt (point)))
             (t
              (cond
               ((markdown-indent--heading-level)
                (setq level (markdown-indent--heading-level))
                (markdown-indent-set-line-properties level 0 'heading))
               ((markdown-indent--is-list-item-p)
                (markdown-indent-set-line-properties
                 level
                 (or (markdown-indent--list-item-body-column)
                     (current-indentation))))
               (t
                (markdown-indent-set-line-properties
                 level
                 (current-indentation))))))))))))

(defun markdown-indent-notify-modified-headline (beg end)
  "Notify that a heading may have been modified in the region BEG to END."
  (when (derived-mode-p 'markdown-mode)
    (save-excursion
      (goto-char beg)
      (setq markdown-indent--modified-headline-flag
            (or (markdown-indent--heading-level)
                (re-search-forward "^\\(#+\\)\\s-+" end t))))))

(defun markdown-indent-refresh-maybe (beg end _ignored)
  "Refresh indentation in the region BEG to END if needed.
Fixes loss of indentation on newly inserted lines by reapplying
line-prefix properties from the beginning to end line boundaries."
  (when (and (boundp 'markdown-indent-mode) markdown-indent-mode)
    (save-match-data
      (save-excursion
        (let ((start (save-excursion
                       (goto-char beg)
                       (line-beginning-position)))
              (finish (save-excursion
                        (goto-char end)
                        (line-beginning-position 2)))))
        (if markdown-indent--modified-headline-flag
            (progn
              (setq markdown-indent--modified-headline-flag nil)
              (markdown-indent-remove-properties start finish)
              (markdown-indent-add-properties start finish))
          (markdown-indent-remove-properties start finish)
          (markdown-indent-add-properties start finish))))))

(defun markdown-indent-initialize-agent ()
  "Resume or initiate indentation for any buffers in queue."
  (when markdown-indent-agent-resume-timer
    (cancel-timer markdown-indent-agent-resume-timer))
  (setq markdown-indent-agentized-buffers
        (cl-remove-if-not #'buffer-live-p markdown-indent-agentized-buffers))
  (cond
   ((not markdown-indent-agentized-buffers)
    (when markdown-indent-agent-timer
      (cancel-timer markdown-indent-agent-timer)))
   ((memq (current-buffer) markdown-indent-agentized-buffers)
    (markdown-indent-initialize-buffer (current-buffer)
                                       markdown-indent-agent-active-delay))
   (t
    (markdown-indent-initialize-buffer (car markdown-indent-agentized-buffers)
                                       markdown-indent-agent-passive-delay))))

(defun markdown-indent-initialize-buffer (buffer delay)
  "Indent BUFFER from `markdown-indent--initial-marker', yielding after DELAY."
  (with-current-buffer buffer
    (when (and (boundp 'markdown-indent-mode) markdown-indent-mode)
      (goto-char (or markdown-indent--initial-marker (point-min)))
      (forward-line 0)
      (let ((interruptp
             (catch 'interrupt
               (when (and markdown-indent--initial-marker
                          (marker-position markdown-indent--initial-marker))
                 (markdown-indent-add-properties
                  markdown-indent--initial-marker (point-max) delay))
               nil)))
        (set-marker markdown-indent--initial-marker interruptp)
        (unless interruptp
          (setq markdown-indent-agentized-buffers
                (delq buffer markdown-indent-agentized-buffers))
          (run-hook-with-args 'markdown-indent-post-buffer-init-functions buffer))))))

;;;###autoload
(define-minor-mode markdown-indent-mode
  "Minor mode to visually indent Markdown text based on heading levels."
  :lighter " Md-Indent"
  (cond
   (markdown-indent-mode
    (when markdown-indent-mode-turns-off-electric-indent
      (setq-local electric-indent-mode nil))
    (setq-local markdown-indent--initial-marker (copy-marker 1))
    (markdown-indent--compute-prefixes)
    (add-hook 'after-change-functions #'markdown-indent-refresh-maybe nil 'local)
    (add-hook 'before-change-functions #'markdown-indent-notify-modified-headline nil 'local)
    (markdown-indent-remove-properties (point-min) (point-max))
    (if markdown-indent-agentized-buffers
        (push (current-buffer) markdown-indent-agentized-buffers)
      (push (current-buffer) markdown-indent-agentized-buffers)
      (setq markdown-indent-agent-timer
            (run-with-idle-timer 0.2 t #'markdown-indent-initialize-agent))))
   (t
    (setq markdown-indent-agentized-buffers
          (delq (current-buffer) markdown-indent-agentized-buffers))
    (when (markerp markdown-indent--initial-marker)
      (set-marker markdown-indent--initial-marker nil))
    (remove-hook 'after-change-functions #'markdown-indent-refresh-maybe 'local)
    (remove-hook 'before-change-functions #'markdown-indent-notify-modified-headline 'local)
    (markdown-indent-remove-properties (point-min) (point-max))
    (redraw-display))))

(provide 'markdown-indent)
;;; markdown-indent.el ends here
