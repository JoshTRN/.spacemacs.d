;;; pdf-caret.el --- Evil-style keyboard text selection for pdf-view -*- lexical-binding: t; -*-

;; Keywords: files, multimedia
;; Package-Requires: ((emacs "27.1") (pdf-tools "1.0"))

;;; Commentary:

;; pdf-view renders pages as images, so evil's visual state has no text
;; to operate on.  This mode rebuilds a caret from the PDF text layer
;; (`pdf-info-charlayout') and drives pdf-tools' own region-rendering
;; machinery from the keyboard:
;;
;;   c       enter caret mode with a bare caret (a one-char highlight)
;;   v / V   enter caret mode with a charwise / linewise selection
;;           (inside the mode they toggle the selection style, like evil)
;;   h l j k w b e 0 ^ $ gg G   move the caret; selection follows
;;   J / K   jump caret to next / previous page
;;   /       incremental search forward from the caret; the current
;;           match highlights as the query is typed (smartcase:
;;           case-insensitive unless the query has an upper-case
;;           char, like evil; wraps within the page)
;;   n / N   next / previous match, wrapping within the page
;;   o       exchange caret and anchor
;;   y       copy selection or current match (or current line when
;;           neither is active)
;;   ESC / q exit
;;
;; Selections cannot cross pages (pdf-tools regions are per-page).

;;; Code:

(require 'cl-lib)
(require 'pdf-view)
(require 'pdf-info)
(require 'pdf-util)
(require 'pdf-cache)

(defgroup pdf-caret nil
  "Keyboard text selection for `pdf-view-mode'."
  :group 'pdf-view)

(defcustom pdf-caret-scroll-step 10
  "Number of lines moved by \\[pdf-caret-half-page-down] and \\[pdf-caret-half-page-up]."
  :type 'integer)

(defcustom pdf-caret-linewise-newline t
  "If non-nil, linewise copies get a trailing newline, like evil's V-yank."
  :type 'boolean)

;;; Buffer-local state

(defvar-local pdf-caret--page nil
  "Page the caret data was built for.")
(defvar-local pdf-caret--chars nil
  "Vector of charlayout entries for the current page.")
(defvar-local pdf-caret--lines nil
  "Vector of (START . END) index ranges into `pdf-caret--chars', END exclusive.")
(defvar-local pdf-caret--idx 0
  "Caret position as an index into `pdf-caret--chars'.")
(defvar-local pdf-caret--anchor nil
  "Selection anchor index, or nil when only the caret is shown.")
(defvar-local pdf-caret--style 'glyph
  "Selection style: `glyph' for charwise, `line' for linewise.")
(defvar-local pdf-caret--goal-x nil
  "Goal column (relative x) preserved across vertical movement.")
(defvar-local pdf-caret--match-end nil
  "Inclusive end index of the current search match, or nil.
While set (and no selection is active) the caret highlight
covers the whole match; any non-search movement clears it.")
(defvar-local pdf-caret--switching nil
  "Non-nil while pdf-caret itself is changing pages.")

;;; Text-layer access

(defun pdf-caret--edges (i)
  "Relative edges (LEFT TOP RIGHT BOT) of char I."
  ;; charlayout entries are (CHAR LEFT TOP RIGHT BOT), but the docstring
  ;; promises (CHAR . ((LEFT ...))); accept both shapes.
  (let ((rest (cdr (aref pdf-caret--chars i))))
    (if (consp (car rest)) (car rest) rest)))

(defun pdf-caret--char (i)
  (car (aref pdf-caret--chars i)))

(defun pdf-caret--x-center (i)
  (let ((e (pdf-caret--edges i)))
    (/ (+ (nth 0 e) (nth 2 e)) 2.0)))

(defun pdf-caret--region (a b)
  "Region spanning chars A..B inclusive, as (X1 Y1 X2 Y2).
The endpoints sit just inside the left edge of A and the right
edge of B: poppler's glyph-style selection is empty when the two
points coincide, so a one-char region (A = B) must span its
glyph rather than sit on the center."
  (let* ((ea (pdf-caret--edges a))
         (eb (pdf-caret--edges b))
         (xa (+ (nth 0 ea) (* 0.15 (- (nth 2 ea) (nth 0 ea)))))
         (ya (/ (+ (nth 1 ea) (nth 3 ea)) 2.0))
         (xb (- (nth 2 eb) (* 0.15 (- (nth 2 eb) (nth 0 eb)))))
         (yb (/ (+ (nth 1 eb) (nth 3 eb)) 2.0)))
    (list xa ya xb yb)))

(defun pdf-caret--space-p (i)
  (memq (pdf-caret--char i) '(?\s ?\t ?\n ? )))

(defun pdf-caret--build (page)
  "Fetch and index the text layer of PAGE."
  (let ((chars (cl-remove-if (lambda (elt) (eq (car elt) ?\n))
                             (pdf-info-charlayout page))))
    (unless chars
      (user-error "Page %d has no selectable text" page))
    (setq pdf-caret--page page
          pdf-caret--chars (vconcat chars)
          pdf-caret--lines (pdf-caret--group-lines))))

(defun pdf-caret--group-lines ()
  "Group `pdf-caret--chars' into lines by vertical overlap."
  (let ((n (length pdf-caret--chars))
        (lines ())
        (start 0)
        ltop lbot)
    (dotimes (i n)
      (let* ((e (pdf-caret--edges i))
             (top (nth 1 e))
             (bot (nth 3 e)))
        (if (null ltop)
            (setq ltop top lbot bot)
          (let* ((overlap (- (min bot lbot) (max top ltop)))
                 (h (max 1e-6 (min (- bot top) (- lbot ltop)))))
            (if (< (/ overlap h) 0.3)
                ;; New line begins here.
                (progn
                  (push (cons start i) lines)
                  (setq start i ltop top lbot bot))
              (setq ltop (min ltop top)
                    lbot (max lbot bot)))))))
    (push (cons start n) lines)
    (vconcat (nreverse lines))))

(defun pdf-caret--line-of (i)
  "Line index containing char I."
  (or (cl-position-if (lambda (se) (and (>= i (car se)) (< i (cdr se))))
                      pdf-caret--lines)
      0))

(defun pdf-caret--line-char-near (li x)
  "Index of the char in line LI whose center is closest to relative X."
  (let* ((se (aref pdf-caret--lines li))
         (best (car se))
         (bestd most-positive-fixnum))
    (cl-loop for i from (car se) below (cdr se)
             for d = (abs (- (pdf-caret--x-center i) x))
             when (< d bestd) do (setq best i bestd d))
    best))

;;; Rendering

(defun pdf-caret--selection-edges ()
  "Start/end points of the active selection as (X1 Y1 X2 Y2)."
  (pdf-caret--region (min pdf-caret--anchor pdf-caret--idx)
                     (max pdf-caret--anchor pdf-caret--idx)))

(defun pdf-caret--render ()
  "Redisplay the page with the current selection or caret.
A bare caret is drawn as a regular selection highlight one char wide."
  (let ((edges (cond (pdf-caret--anchor (pdf-caret--selection-edges))
                     (pdf-caret--match-end
                      (pdf-caret--region pdf-caret--idx pdf-caret--match-end))
                     (t (pdf-caret--region pdf-caret--idx pdf-caret--idx)))))
    (setq pdf-view-active-region (cons pdf-caret--page (list edges)))
    (pdf-view-display-region pdf-view-active-region nil
                             (if pdf-caret--anchor pdf-caret--style 'glyph)))
  (ignore-errors
    (pdf-util-scroll-to-edges
     (pdf-util-scale-relative-to-pixel (pdf-caret--edges pdf-caret--idx)))))

(defun pdf-caret--visible-start-idx ()
  "Index of the first char inside the currently displayed part of the page."
  (or (ignore-errors
        (let* ((rel (pdf-util-scale-pixel-to-relative
                     (pdf-util-image-displayed-edges)))
               (x1 (nth 0 rel)) (y1 (nth 1 rel))
               (x2 (nth 2 rel)) (y2 (nth 3 rel)))
          (cl-loop for i from 0 below (length pdf-caret--chars)
                   for e = (pdf-caret--edges i)
                   for cx = (/ (+ (nth 0 e) (nth 2 e)) 2.0)
                   for cy = (/ (+ (nth 1 e) (nth 3 e)) 2.0)
                   when (and (<= x1 cx x2) (<= y1 cy y2))
                   return i)))
      0))

;;; Movement

(defun pdf-caret--goto (i &optional keep-goal keep-match)
  (setq pdf-caret--idx (max 0 (min i (1- (length pdf-caret--chars)))))
  (unless keep-goal
    (setq pdf-caret--goal-x (pdf-caret--x-center pdf-caret--idx)))
  (unless keep-match
    (setq pdf-caret--match-end nil))
  (pdf-caret--render))

(defun pdf-caret-forward-char (n)
  "Move the caret N chars forward."
  (interactive "p")
  (pdf-caret--goto (+ pdf-caret--idx n)))

(defun pdf-caret-backward-char (n)
  "Move the caret N chars backward."
  (interactive "p")
  (pdf-caret--goto (- pdf-caret--idx n)))

(defun pdf-caret--vertical (dir)
  "Move the caret one line in direction DIR (+1 or -1)."
  (let* ((li (pdf-caret--line-of pdf-caret--idx))
         (target (+ li dir))
         (goal (or pdf-caret--goal-x (pdf-caret--x-center pdf-caret--idx))))
    (setq pdf-caret--goal-x goal)
    (cond
     ((and (>= target 0) (< target (length pdf-caret--lines)))
      (pdf-caret--goto (pdf-caret--line-char-near target goal) t))
     (pdf-caret--anchor
      (user-error "Selection cannot cross pages (press y to copy, ESC to quit)"))
     (t
      (pdf-caret--switch-page (+ pdf-caret--page dir)
                              (if (> dir 0) 'first 'last))))))

(defun pdf-caret-next-line (n)
  "Move the caret N lines down, crossing to the next page at the bottom."
  (interactive "p")
  (dotimes (_ n) (pdf-caret--vertical 1)))

(defun pdf-caret-previous-line (n)
  "Move the caret N lines up, crossing to the previous page at the top."
  (interactive "p")
  (dotimes (_ n) (pdf-caret--vertical -1)))

(defun pdf-caret-half-page-down ()
  (interactive)
  (pdf-caret-next-line pdf-caret-scroll-step))

(defun pdf-caret-half-page-up ()
  (interactive)
  (pdf-caret-previous-line pdf-caret-scroll-step))

(defun pdf-caret-forward-word (n)
  "Move the caret to the start of the Nth next word."
  (interactive "p")
  (let ((i pdf-caret--idx)
        (last (1- (length pdf-caret--chars))))
    (dotimes (_ n)
      (while (and (< i last) (not (pdf-caret--space-p i))) (cl-incf i))
      (while (and (< i last) (pdf-caret--space-p i)) (cl-incf i)))
    (pdf-caret--goto i)))

(defun pdf-caret-backward-word (n)
  "Move the caret to the start of the Nth previous word."
  (interactive "p")
  (let ((i pdf-caret--idx))
    (dotimes (_ n)
      (when (> i 0) (cl-decf i))
      (while (and (> i 0) (pdf-caret--space-p i)) (cl-decf i))
      (while (and (> i 0) (not (pdf-caret--space-p (1- i)))) (cl-decf i)))
    (pdf-caret--goto i)))

(defun pdf-caret-end-of-word (n)
  "Move the caret to the end of the Nth next word."
  (interactive "p")
  (let ((i pdf-caret--idx)
        (last (1- (length pdf-caret--chars))))
    (dotimes (_ n)
      (when (< i last) (cl-incf i))
      (while (and (< i last) (pdf-caret--space-p i)) (cl-incf i))
      (while (and (< i last) (not (pdf-caret--space-p (1+ i)))) (cl-incf i)))
    (pdf-caret--goto i)))

(defun pdf-caret-line-start (arg)
  "Move the caret to the start of the line; with a pending count, digit 0."
  (interactive "P")
  (if arg
      (call-interactively #'digit-argument)
    (pdf-caret--goto (car (aref pdf-caret--lines
                                (pdf-caret--line-of pdf-caret--idx))))))

(defun pdf-caret-line-first-nonblank ()
  "Move the caret to the first non-blank char of the line."
  (interactive)
  (let* ((se (aref pdf-caret--lines (pdf-caret--line-of pdf-caret--idx)))
         (i (car se)))
    (while (and (< i (1- (cdr se))) (pdf-caret--space-p i)) (cl-incf i))
    (pdf-caret--goto i)))

(defun pdf-caret-line-end ()
  "Move the caret to the end of the line."
  (interactive)
  (pdf-caret--goto (1- (cdr (aref pdf-caret--lines
                                  (pdf-caret--line-of pdf-caret--idx))))))

(defun pdf-caret-first-line ()
  "Move the caret to the first line of the page."
  (interactive)
  (pdf-caret--goto 0))

(defun pdf-caret-last-line ()
  "Move the caret to the last line of the page."
  (interactive)
  (pdf-caret--goto (car (aref pdf-caret--lines
                              (1- (length pdf-caret--lines))))))

(defun pdf-caret--switch-page (page where)
  "Move the caret to PAGE, placing it on the first or last line per WHERE."
  (let ((npages (pdf-cache-number-of-pages)))
    (unless (<= 1 page npages)
      (user-error "No %s page" (if (> page pdf-caret--page) "next" "previous"))))
  (let ((pdf-caret--switching t)
        (goal pdf-caret--goal-x))
    (setq pdf-caret--anchor nil
          pdf-caret--match-end nil)
    (pdf-view-goto-page page)
    (pdf-caret--build page)
    (let ((li (if (eq where 'last) (1- (length pdf-caret--lines)) 0)))
      (setq pdf-caret--idx
            (if goal
                (pdf-caret--line-char-near li goal)
              (car (aref pdf-caret--lines li)))))
    (setq pdf-caret--goal-x goal)
    (pdf-caret--render)))

(defun pdf-caret-next-page ()
  "Move the caret to the top of the next page."
  (interactive)
  (pdf-caret--switch-page (1+ pdf-caret--page) 'first))

(defun pdf-caret-previous-page ()
  "Move the caret to the top of the previous page."
  (interactive)
  (pdf-caret--switch-page (1- pdf-caret--page) 'first))

;;; Search

(defvar pdf-caret-search-history nil
  "Minibuffer history for `pdf-caret-search'.")

(defvar-local pdf-caret--search-string nil
  "Last search string, reused by `pdf-caret-search-next'/`-previous'.")

(defun pdf-caret--search-matches ()
  "Indices of all matches for the current search string on the page.
Case-insensitive unless the search string contains an upper-case char."
  (let* ((text (mapconcat (lambda (c) (char-to-string (car c)))
                          pdf-caret--chars ""))
         (re (regexp-quote pdf-caret--search-string))
         ;; Detect upper-case with folding off: when case-fold-search
         ;; is on (the default), [[:upper:]] matches lower-case too.
         (case-fold-search
          (let ((case-fold-search nil))
            (not (string-match-p "[[:upper:]]" pdf-caret--search-string))))
         (start 0)
         (hits ()))
    (while (string-match re text start)
      (push (match-beginning 0) hits)
      (setq start (1+ (match-beginning 0))))
    (nreverse hits)))

(defun pdf-caret--show-match (start)
  "Move the caret to START and highlight the whole match there."
  (setq pdf-caret--match-end
        (min (1- (+ start (length pdf-caret--search-string)))
             (1- (length pdf-caret--chars))))
  (pdf-caret--goto start nil t))

(defun pdf-caret--search-preview (start query)
  "Highlight the first match for QUERY after index START, or restore START."
  (setq pdf-caret--search-string (unless (string-empty-p query) query))
  (let ((hits (and pdf-caret--search-string (pdf-caret--search-matches))))
    (if hits
        (pdf-caret--show-match (or (cl-find-if (lambda (i) (> i start)) hits)
                                   (car hits)))
      (setq pdf-caret--match-end nil)
      (pdf-caret--goto start))))

(defun pdf-caret-search ()
  "Incrementally search forward from the caret, like evil.
The current match is highlighted while the query is typed;
aborting the minibuffer restores the caret.  Wraps to the top of
the page when there is no match below the caret."
  (interactive)
  (let* ((buf (current-buffer))
         (start pdf-caret--idx)
         (old-string pdf-caret--search-string)
         (last "")
         (done nil))
    (unwind-protect
        (let ((query
               (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook 'post-command-hook
                               (lambda ()
                                 (let ((q (minibuffer-contents-no-properties)))
                                   (unless (equal q last)
                                     (setq last q)
                                     (with-current-buffer buf
                                       (ignore-errors
                                         (pdf-caret--search-preview start q))))))
                               nil t))
                 (read-string "/" nil 'pdf-caret-search-history))))
          (when (string-empty-p query)
            (user-error "Empty search string"))
          (setq pdf-caret--search-string query
                done t
                pdf-caret--idx start)
          (pdf-caret-search-next))
      ;; Quit or empty input: undo whatever the preview showed.
      (unless done
        (setq pdf-caret--search-string old-string
              pdf-caret--match-end nil)
        (pdf-caret--goto start)))))

(defun pdf-caret-search-next ()
  "Move the caret to the next match, wrapping at the end of the page."
  (interactive)
  (unless pdf-caret--search-string
    (user-error "No previous search"))
  (let ((hits (pdf-caret--search-matches)))
    (unless hits
      (user-error "No match on this page: %s" pdf-caret--search-string))
    (let ((next (cl-find-if (lambda (i) (> i pdf-caret--idx)) hits)))
      (unless next (message "Search wrapped"))
      (pdf-caret--show-match (or next (car hits))))))

(defun pdf-caret-search-previous ()
  "Move the caret to the previous match, wrapping at the top of the page."
  (interactive)
  (unless pdf-caret--search-string
    (user-error "No previous search"))
  (let ((hits (pdf-caret--search-matches)))
    (unless hits
      (user-error "No match on this page: %s" pdf-caret--search-string))
    (let ((prev (cl-find-if (lambda (i) (< i pdf-caret--idx))
                            (reverse hits))))
      (unless prev (message "Search wrapped"))
      (pdf-caret--show-match (or prev (car (last hits)))))))

;;; Selection

(defun pdf-caret-toggle-charwise ()
  "Start a charwise selection, or drop it if one is already active."
  (interactive)
  (cond
   ((and pdf-caret--anchor (eq pdf-caret--style 'glyph))
    (setq pdf-caret--anchor nil))
   (t
    (setq pdf-caret--anchor (or pdf-caret--anchor pdf-caret--idx)
          pdf-caret--style 'glyph)))
  (pdf-caret--render))

(defun pdf-caret-toggle-linewise ()
  "Start a linewise selection, or drop it if one is already active."
  (interactive)
  (cond
   ((and pdf-caret--anchor (eq pdf-caret--style 'line))
    (setq pdf-caret--anchor nil))
   (t
    (setq pdf-caret--anchor (or pdf-caret--anchor pdf-caret--idx)
          pdf-caret--style 'line)))
  (pdf-caret--render))

(defun pdf-caret-exchange ()
  "Exchange the caret and the selection anchor."
  (interactive)
  (if (not pdf-caret--anchor)
      (user-error "No active selection")
    (cl-rotatef pdf-caret--anchor pdf-caret--idx)
    (setq pdf-caret--goal-x (pdf-caret--x-center pdf-caret--idx))
    (pdf-caret--render)))

(defun pdf-caret-copy ()
  "Copy what is highlighted to the kill ring and exit.
That is the selection, the current search match, or failing
both, the caret's whole line."
  (interactive)
  (let* ((linewise (if pdf-caret--anchor
                       (eq pdf-caret--style 'line)
                     (not pdf-caret--match-end)))
         (edges (cond (pdf-caret--anchor (pdf-caret--selection-edges))
                      (pdf-caret--match-end
                       (pdf-caret--region pdf-caret--idx pdf-caret--match-end))
                      (t (pdf-caret--region pdf-caret--idx pdf-caret--idx))))
         (txt (pdf-info-gettext pdf-caret--page edges
                                (if linewise 'line 'glyph))))
    (when (and linewise pdf-caret-linewise-newline (not (string-empty-p txt)))
      (setq txt (concat txt "\n")))
    (kill-new txt)
    (pdf-caret-quit)
    (message "Copied: %s"
             (truncate-string-to-width
              (string-trim (replace-regexp-in-string "\n" " " txt))
              70 nil nil "…"))))

(defun pdf-caret-quit ()
  "Leave caret mode and clear any highlight."
  (interactive)
  (pdf-caret-mode -1))

;;; Mode plumbing

(defvar pdf-caret-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" #'pdf-caret-backward-char)
    (define-key map "l" #'pdf-caret-forward-char)
    (define-key map "j" #'pdf-caret-next-line)
    (define-key map "k" #'pdf-caret-previous-line)
    (define-key map "w" #'pdf-caret-forward-word)
    (define-key map "b" #'pdf-caret-backward-word)
    (define-key map "e" #'pdf-caret-end-of-word)
    (define-key map "0" #'pdf-caret-line-start)
    (define-key map "^" #'pdf-caret-line-first-nonblank)
    (define-key map "$" #'pdf-caret-line-end)
    (define-key map "G" #'pdf-caret-last-line)
    (define-key map "gg" #'pdf-caret-first-line)
    (define-key map "J" #'pdf-caret-next-page)
    (define-key map "K" #'pdf-caret-previous-page)
    (define-key map (kbd "C-d") #'pdf-caret-half-page-down)
    (define-key map (kbd "C-u") #'pdf-caret-half-page-up)
    (define-key map "v" #'pdf-caret-toggle-charwise)
    (define-key map "V" #'pdf-caret-toggle-linewise)
    (define-key map "o" #'pdf-caret-exchange)
    (define-key map "/" #'pdf-caret-search)
    (define-key map "n" #'pdf-caret-search-next)
    (define-key map "N" #'pdf-caret-search-previous)
    (define-key map "y" #'pdf-caret-copy)
    (define-key map "q" #'pdf-caret-quit)
    (define-key map [escape] #'pdf-caret-quit)
    (dolist (d '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (define-key map d #'digit-argument))
    map)
  "Keymap for `pdf-caret-mode'.")

(declare-function evil-make-intercept-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function evil-local-set-key "evil-core")
(defvar pdf-caret-mode)

;;;###autoload
(defun pdf-caret-setup-evil-keys ()
  "Bind v/V to pdf-caret buffer-locally, for `pdf-view-mode-hook'.
Buffer-local state bindings outrank the evilified-state defaults
that Spacemacs stamps into the mode map during evilification."
  (when (fboundp 'evil-local-set-key)
    (dolist (state '(evilified normal))
      (evil-local-set-key state "c" #'pdf-caret-start)
      (evil-local-set-key state "v" #'pdf-caret-select-char)
      (evil-local-set-key state "V" #'pdf-caret-select-line))))

(with-eval-after-load 'evil
  ;; Win over the evilified/normal state bindings while the mode is on.
  (evil-make-intercept-map pdf-caret-mode-map))

(defun pdf-caret--on-page-change ()
  ;; Bail out if something else (isearch, outline, ...) changed the page.
  (when (and pdf-caret-mode
             (not pdf-caret--switching)
             (not (eq (pdf-view-current-page) pdf-caret--page)))
    (pdf-caret-mode -1)))

;;;###autoload
(define-minor-mode pdf-caret-mode
  "Keyboard caret and text selection for `pdf-view-mode'."
  :lighter " Caret"
  :keymap pdf-caret-mode-map
  (if pdf-caret-mode
      (progn
        (unless (derived-mode-p 'pdf-view-mode)
          (setq pdf-caret-mode nil)
          (user-error "Not in a pdf-view buffer"))
        (add-hook 'pdf-view-after-change-page-hook
                  #'pdf-caret--on-page-change nil t))
    (remove-hook 'pdf-view-after-change-page-hook
                 #'pdf-caret--on-page-change t)
    (setq pdf-caret--anchor nil
          pdf-caret--match-end nil)
    (if pdf-view-active-region
        (pdf-view-deactivate-region)
      (pdf-view-redisplay t)))
  ;; The intercept map only outranks evil's state maps (evilified h/j/k/l
  ;; scrolling) once evil re-normalizes; toggling a minor mode doesn't.
  (when (and (fboundp 'evil-normalize-keymaps)
             (bound-and-true-p evil-local-mode))
    (evil-normalize-keymaps)))

(defun pdf-caret--enter (style &optional select)
  (pdf-caret--build (pdf-view-current-page))
  (setq pdf-caret--style style
        pdf-caret--goal-x nil)
  (setq pdf-caret--idx (pdf-caret--visible-start-idx)
        pdf-caret--anchor (and select pdf-caret--idx)
        pdf-caret--match-end nil)
  (pdf-caret-mode 1)
  (pdf-caret--render)
  (message (substitute-command-keys
            "Caret: \\`h' \\`j' \\`k' \\`l' \\`w' \\`b' \\`0' \\`$' \\`gg' \\`G' move, \\`/' search, \\`v'/\\`V' select, \\`o' swap ends, \\`y' copy, \\`ESC' quit")))

;;;###autoload
(defun pdf-caret-start ()
  "Enter caret mode with a bare one-character caret, no selection."
  (interactive)
  (pdf-caret--enter 'glyph))

;;;###autoload
(defun pdf-caret-select-char ()
  "Start an evil-style charwise text selection on the current page."
  (interactive)
  (pdf-caret--enter 'glyph t))

;;;###autoload
(defun pdf-caret-select-line ()
  "Start an evil-style linewise text selection on the current page."
  (interactive)
  (pdf-caret--enter 'line t))

(provide 'pdf-caret)
;;; pdf-caret.el ends here
