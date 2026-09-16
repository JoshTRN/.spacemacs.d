;;; posframe-timer.el --- Timer and clock display in a posframe -*- lexical-binding: t; -*-

;; Author: Joshua
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (posframe "1.0.0"))
;; Keywords: convenience, tools, calendar

;;; Commentary:

;; A floating timer/clock display in a posframe pinned to the top-right
;; corner of the frame, rendered at roughly twice the normal font height.
;; Three things can be on screen at once, stacked top to bottom:
;;
;;  - the running org clock (automatic): the clocked heading is shown
;;    word-wrapped to the width of the clock, above an elapsed count-up
;;    that starts at 00:00 and widens to 00:00:00 past an hour;
;;  - a "free" clock unattached to any org buffer:
;;    `posframe-timer-clock-in' starts it, `posframe-timer-clock-out'
;;    prompts for an org heading and files a closed CLOCK line under it
;;    in `posframe-timer-timesheet-file' (created when missing);
;;  - the original countdown timer.
;;
;; Countdown entry points:
;;   `posframe-timer-start'        start (or resume) the countdown;
;;                                 with a numeric prefix, that many minutes
;;   `posframe-timer-set'          prompt for a duration and start it
;;   `posframe-timer-toggle-pause' pause / resume
;;   `posframe-timer-stop'         pause and hide; `posframe-timer-start' resumes
;;   `posframe-timer-clear'        cancel, hide, and reset to the default
;;   `posframe-timer-toggle-display' flip between posframe and mode-line
;;
;; Free clock entry points:
;;   `posframe-timer-clock-in'     start a free clock (optional label)
;;   `posframe-timer-clock-out'    stop it and file it in the timesheet
;;   `posframe-timer-clock-cancel' discard it
;;
;; From Lisp, `posframe-timer-clock-in' also accepts ON-OUT and
;; ON-CANCEL handlers that claim the clock's outcome in place of the
;; timesheet flow — this is how zoho-desk runs its ticket timer.
;;
;; When the posframe display is off (`posframe-timer-use-posframe' nil, a
;; terminal frame, or posframe unavailable) the countdown and free clock
;; fall back to the mode line, taking over the `display-time-mode' clock
;; slot while active.  The org clock is not duplicated there because
;; org-clock maintains its own mode-line display.
;;
;; The posframe is draggable: hold mouse-1 on it and move.  mouse-3
;; toggles countdown pause.  A drag pins the frame to the dropped
;; position; `posframe-timer-reset-position' snaps it back to the corner.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-delete "posframe")
(declare-function posframe-delete-frame "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function alert "alert")
(declare-function org-clocking-p "org-clock")
(declare-function org-clock-find-position "org-clock" (find-unclosed))
(declare-function org-time-stamp-format "org" (&optional with-time inactive))
(declare-function org-find-exact-headline-in-buffer "org" (heading &optional buffer pos-only))
(declare-function org-link-display-format "ol" (s))

(defvar org-clock-start-time)
(defvar org-clock-current-task)
(defvar org-complex-heading-regexp)

(defgroup posframe-timer nil
  "Countdown timer and clock display in a posframe."
  :group 'convenience
  :prefix "posframe-timer-")

(defcustom posframe-timer-default-duration (* 30 60)
  "Default countdown length in seconds."
  :type 'natnum)

(defcustom posframe-timer-use-posframe t
  "When non-nil, display timers and clocks in a posframe.
When nil (or when posframe cannot be used, e.g. in a terminal frame),
the countdown and free clock take over the mode-line clock instead."
  :type 'boolean)

(defcustom posframe-timer-org-clock-integration t
  "When non-nil, mirror the running org clock in the posframe.
The clocked heading is shown word-wrapped above an elapsed count-up, so
whether a clock is running is always visible at a glance."
  :type 'boolean)

(defcustom posframe-timer-timesheet-file "~/org/timesheet.org"
  "Org file that receives clocks from `posframe-timer-clock-out'.
Created (with parent directories) on first use when missing."
  :type 'file)

(defcustom posframe-timer-margin 24
  "Pixel gap between the posframe and the top/right frame edges."
  :type 'natnum)

(defcustom posframe-timer-urgent-threshold 60
  "Seconds remaining below which the countdown uses the urgent face."
  :type 'natnum)

(defcustom posframe-timer-finished-hook nil
  "Hook run when the countdown reaches zero."
  :type 'hook)

(defface posframe-timer-face
  '((t :weight bold :height 2.2))
  "Face for the countdown.  `:height' scales the normal font size.")

(defface posframe-timer-paused-face
  '((t :inherit (shadow posframe-timer-face)))
  "Face for the countdown while paused.")

(defface posframe-timer-urgent-face
  '((t :inherit (error posframe-timer-face)))
  "Face for the countdown during the final stretch and at zero.")

(defface posframe-timer-clock-face
  '((t :inherit posframe-timer-face))
  "Face for count-up clocks (the org clock and free clocks).")

(defface posframe-timer-heading-face
  '((t :height 1.0 :weight normal))
  "Face for the wrapped heading shown above a count-up clock.
Its `:height' relative to `posframe-timer-clock-face' determines how
many heading characters fit above the clock before wrapping.")

(defconst posframe-timer--buffer " *posframe-timer*")

;; State.  The countdown is `posframe-timer--remaining' seconds; while
;; running, remaining is recomputed from `posframe-timer--end-time' each
;; tick so pauses and delays never accumulate drift.
(defvar posframe-timer--remaining nil
  "Seconds left, or nil when no countdown is set up.")
(defvar posframe-timer--end-time nil
  "Absolute end time while running, nil while paused/stopped.")
(defvar posframe-timer--countdown-hidden nil
  "Non-nil after `posframe-timer-stop': keep the countdown but don't draw it.")
(defvar posframe-timer--tick-timer nil)
(defvar posframe-timer--duration posframe-timer-default-duration
  "Length of the most recently started countdown, for restarts.")
(defvar posframe-timer--clock-start nil
  "Start time of the free clock, or nil when not clocked in.")
(defvar posframe-timer--clock-label nil
  "Optional label given at `posframe-timer-clock-in', or nil.")
(defvar posframe-timer--clock-on-out nil
  "Handler owning `posframe-timer-clock-out', or nil for the timesheet flow.
Called with the clock's start time, end time and label, after the
clock state has been cleared.  Set by the ON-OUT argument of
`posframe-timer-clock-in'.")
(defvar posframe-timer--clock-on-cancel nil
  "Handler notified by `posframe-timer-clock-cancel', or nil.
Called with the discarded clock's start time and label, after the
clock state has been cleared.  Set by the ON-CANCEL argument of
`posframe-timer-clock-in'.")
(defvar posframe-timer--frame nil
  "The child frame posframe is drawing into, when live.")
(defvar posframe-timer--last-layout nil
  "Per-line display widths of the last render, to skip needless re-shows.")
(defvar posframe-timer--position nil
  "Pixel position pinned by dragging, or nil for the top-right corner.")
(defvar posframe-timer--clock-taken-over nil
  "Non-nil when `display-time-string' has been swapped out of the mode line.")

(defvar posframe-timer-mode-line-string ""
  "Display string spliced into `global-mode-string' in fallback mode.")
(put 'posframe-timer-mode-line-string 'risky-local-variable t)

;;; Predicates

(defun posframe-timer--running-p ()
  (and posframe-timer--tick-timer posframe-timer--end-time))

(defun posframe-timer--paused-p ()
  (and posframe-timer--remaining (not posframe-timer--end-time)))

(defun posframe-timer--org-clock-active-p ()
  "Non-nil when the org clock is running and integration is enabled."
  (and posframe-timer-org-clock-integration
       (featurep 'org-clock)
       (fboundp 'org-clocking-p)
       (org-clocking-p)
       org-clock-start-time))

(defun posframe-timer--tick-needed-p ()
  "Non-nil when something on screen changes over time."
  (or posframe-timer--end-time
      posframe-timer--clock-start
      (posframe-timer--org-clock-active-p)))

(defun posframe-timer--posframe-usable-p ()
  (and posframe-timer-use-posframe
       (display-graphic-p)
       (require 'posframe nil t)
       (posframe-workable-p)))

;;; Formatting

(defun posframe-timer--format (&optional for-mode-line)
  "Render the countdown as a propertized string.
FOR-MODE-LINE renders compactly at normal height."
  (let* ((secs (max 0 (ceiling (or posframe-timer--remaining 0))))
         (h (/ secs 3600))
         (m (/ (% secs 3600) 60))
         (s (% secs 60))
         (text (if (> h 0)
                   (format "%d:%02d:%02d" h m s)
                 (format "%02d:%02d" m s)))
         (face (cond ((posframe-timer--paused-p) 'posframe-timer-paused-face)
                     ((<= secs posframe-timer-urgent-threshold)
                      'posframe-timer-urgent-face)
                     (t 'posframe-timer-face))))
    (when (posframe-timer--paused-p)
      (setq text (concat text " ⏸")))
    (if for-mode-line
        (propertize (concat " ⏱ " text " ")
                    'face (if (eq face 'posframe-timer-face) 'mode-line-emphasis face))
      (propertize (concat " " text " ") 'face face))))

(defun posframe-timer--format-elapsed (start)
  "Format time since START as MM:SS, widening to HH:MM:SS past an hour."
  (let* ((secs (max 0 (floor (float-time (time-subtract nil start)))))
         (h (/ secs 3600))
         (m (/ (% secs 3600) 60))
         (s (% secs 60)))
    (if (> h 0)
        (format "%02d:%02d:%02d" h m s)
      (format "%02d:%02d" m s))))

(defun posframe-timer--face-scale (face)
  "Height of FACE as a multiple of the default face height."
  (let ((h (face-attribute face :height nil t)))
    (cond ((floatp h) h)
          ((integerp h)
           (/ h (float (face-attribute 'default :height))))
          (t 1.0))))

(defun posframe-timer--wrap-heading (text budget)
  "Word-wrap TEXT to at most BUDGET columns, returning a list of lines."
  (with-temp-buffer
    (insert text)
    (let ((fill-column (max 8 budget)))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t "[ \t]+")))

(defun posframe-timer--clock-section (title start)
  "Render TITLE word-wrapped above the time elapsed since START.
TITLE wraps visual-line style at the pixel width of the time line
(its column budget scaled by the two faces' relative heights), so
the section is never wider than the clock itself.  A nil or empty
TITLE renders the bare clock."
  (let* ((time-line (propertize
                     (concat " " (posframe-timer--format-elapsed start) " ")
                     'face 'posframe-timer-clock-face))
         (scale (/ (posframe-timer--face-scale 'posframe-timer-clock-face)
                   (max 0.1 (posframe-timer--face-scale
                             'posframe-timer-heading-face))))
         (budget (floor (* (string-width time-line) scale))))
    (if (or (null title) (string-empty-p title))
        time-line
      (concat
       (mapconcat
        (lambda (line)
          (let ((pad (max 0 (/ (- budget (string-width line)) 2))))
            (propertize (concat (make-string pad ?\s) line)
                        'face 'posframe-timer-heading-face)))
        (posframe-timer--wrap-heading title budget)
        "\n")
       "\n" time-line))))

(defun posframe-timer--org-heading ()
  "The currently clocked org heading, cleaned for display."
  (let ((task (and (boundp 'org-clock-current-task) org-clock-current-task)))
    (cond ((null task) "org clock")
          ((fboundp 'org-link-display-format)
           (org-link-display-format (substring-no-properties task)))
          (t (substring-no-properties task)))))

(defun posframe-timer--render ()
  "Render every active section, or nil when there is nothing to show.
Sections stack: org clock, then the free clock, then the countdown."
  (let ((sections
         (delq nil
               (list
                (when (posframe-timer--org-clock-active-p)
                  (posframe-timer--clock-section (posframe-timer--org-heading)
                                                 org-clock-start-time))
                (when posframe-timer--clock-start
                  (posframe-timer--clock-section posframe-timer--clock-label
                                                 posframe-timer--clock-start))
                (when (and posframe-timer--remaining
                           (not posframe-timer--countdown-hidden))
                  (posframe-timer--format))))))
    (when sections
      (mapconcat #'identity sections "\n"))))

(defun posframe-timer--format-mode-line ()
  "Compact fallback string: the free clock and the countdown.
The org clock is left out because org-clock maintains its own
mode-line display while clocked in."
  (concat
   (when posframe-timer--clock-start
     (propertize (format " ⏱ %s%s "
                         (if posframe-timer--clock-label
                             (concat posframe-timer--clock-label " ")
                           "")
                         (posframe-timer--format-elapsed
                          posframe-timer--clock-start))
                 'face 'mode-line-emphasis))
   (when (and posframe-timer--remaining
              (not posframe-timer--countdown-hidden))
     (posframe-timer--format t))))

;;; Posframe display

(defun posframe-timer--poshandler (info)
  "Place the posframe at the top-right corner, inset by `posframe-timer-margin'."
  (cons (- (plist-get info :parent-frame-width)
           (plist-get info :posframe-width)
           posframe-timer-margin)
        posframe-timer-margin))

(defvar posframe-timer--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'posframe-timer-drag)
    (define-key map [mouse-1] #'ignore)
    (define-key map [mouse-3] #'posframe-timer-toggle-pause)
    map)
  "Mouse bindings active on the display text.")

(defun posframe-timer--root-frame ()
  "Nearest top-level ancestor of the selected frame.
`posframe-show' parents the posframe to the selected frame, so calling
it while a child frame is selected (this posframe mid-drag, a
helm-posframe session) must not be allowed to create a parent cycle."
  (let ((frame (selected-frame)))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defun posframe-timer--show-posframe (text)
  (let* ((text (propertize text
                           'keymap posframe-timer--keymap
                           'pointer 'vdrag
                           'help-echo "drag: move · mouse-3: pause/resume"))
         (layout (mapcar #'string-width (split-string text "\n"))))
    (with-current-buffer (get-buffer-create posframe-timer--buffer)
      (setq-local cursor-type nil)
      (use-local-map posframe-timer--keymap)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)))
    ;; The buffer update alone redisplays in the live frame; call
    ;; `posframe-show' only when the frame is gone/hidden or the text
    ;; shape changed, since each call re-parents to the selected frame.
    (unless (and (frame-live-p posframe-timer--frame)
                 (frame-visible-p posframe-timer--frame)
                 (equal layout posframe-timer--last-layout))
      (setq posframe-timer--last-layout layout)
      (with-selected-frame (posframe-timer--root-frame)
        (setq posframe-timer--frame
              (posframe-show posframe-timer--buffer
                             :position posframe-timer--position
                             :poshandler (unless posframe-timer--position
                                           #'posframe-timer--poshandler)
                             :border-width 2
                             :border-color (face-attribute 'mode-line-emphasis
                                                           :foreground nil t)
                             :accept-focus nil))))))

(defun posframe-timer-drag (start-event)
  "Drag the timer posframe with the mouse; the dropped position sticks."
  (interactive "e")
  (let ((frame posframe-timer--frame))
    (when (frame-live-p frame)
      (let* ((start-mouse (mouse-absolute-pixel-position))
             (start-pos (frame-position frame)))
        (ignore start-event)
        (track-mouse
          (catch 'posframe-timer--drag-done
            (while t
              (let ((ev (read-event)))
                (if (mouse-movement-p ev)
                    (let ((mouse (mouse-absolute-pixel-position)))
                      (set-frame-position
                       frame
                       (+ (car start-pos) (- (car mouse) (car start-mouse)))
                       (+ (cdr start-pos) (- (cdr mouse) (cdr start-mouse)))))
                  (throw 'posframe-timer--drag-done nil))))))
        (setq posframe-timer--position (frame-position frame))))))

(defun posframe-timer-reset-position ()
  "Forget the dragged position and snap back to the top-right corner.
The frame is deleted and recreated rather than re-shown: a window
manager can move the frame behind posframe's back, and posframe
skips repositioning while its cached coordinates still look right."
  (interactive)
  (setq posframe-timer--position nil
        posframe-timer--last-layout nil)
  (when (and (featurep 'posframe)
             (get-buffer posframe-timer--buffer)
             (frame-live-p posframe-timer--frame))
    (posframe-delete-frame posframe-timer--buffer)
    (setq posframe-timer--frame nil))
  (posframe-timer--refresh))

;;; Mode-line fallback

(defun posframe-timer--mode-line-on ()
  "Splice the display into `global-mode-string', displacing the clock."
  (when (and (memq 'display-time-string global-mode-string)
             (not posframe-timer--clock-taken-over))
    (setq global-mode-string
          (mapcar (lambda (e)
                    (if (eq e 'display-time-string)
                        'posframe-timer-mode-line-string
                      e))
                  global-mode-string))
    (setq posframe-timer--clock-taken-over t))
  (unless (memq 'posframe-timer-mode-line-string global-mode-string)
    (setq global-mode-string
          (append (or global-mode-string '(""))
                  '(posframe-timer-mode-line-string)))))

(defun posframe-timer--mode-line-off ()
  "Remove the display from the mode line, restoring the clock."
  (if posframe-timer--clock-taken-over
      (setq global-mode-string
            (mapcar (lambda (e)
                      (if (eq e 'posframe-timer-mode-line-string)
                          'display-time-string
                        e))
                    global-mode-string))
    (setq global-mode-string
          (delq 'posframe-timer-mode-line-string global-mode-string)))
  (setq posframe-timer--clock-taken-over nil
        posframe-timer-mode-line-string "")
  (force-mode-line-update t))

;;; Refresh / hide

(defun posframe-timer--refresh ()
  "Redraw all active sections, or take everything off screen."
  (let ((text (posframe-timer--render)))
    (cond
     ((null text)
      (posframe-timer--hide))
     ((posframe-timer--posframe-usable-p)
      (when posframe-timer--clock-taken-over (posframe-timer--mode-line-off))
      (posframe-timer--show-posframe text))
     (t
      (when (frame-live-p posframe-timer--frame)
        (posframe-hide posframe-timer--buffer))
      (posframe-timer--mode-line-on)
      (setq posframe-timer-mode-line-string (posframe-timer--format-mode-line))
      (force-mode-line-update t)))))

(defun posframe-timer--hide ()
  "Take the display off screen in both display modes."
  (when (and (featurep 'posframe) (get-buffer posframe-timer--buffer))
    (posframe-hide posframe-timer--buffer))
  (setq posframe-timer--last-layout nil)
  (posframe-timer--mode-line-off))

;;; Tick engine

(defun posframe-timer--tick ()
  (when posframe-timer--end-time
    (let ((remaining (float-time
                      (time-subtract posframe-timer--end-time nil))))
      (if (> remaining 0)
          (setq posframe-timer--remaining remaining)
        (posframe-timer--finish))))
  (posframe-timer--refresh)
  (unless (posframe-timer--tick-needed-p)
    (posframe-timer--cancel-tick)))

(defun posframe-timer--cancel-tick ()
  (when posframe-timer--tick-timer
    (cancel-timer posframe-timer--tick-timer)
    (setq posframe-timer--tick-timer nil)))

(defun posframe-timer--sync-tick ()
  "Start or stop the half-second tick to match what needs updating."
  (if (posframe-timer--tick-needed-p)
      (unless posframe-timer--tick-timer
        (setq posframe-timer--tick-timer
              (run-at-time 0 0.5 #'posframe-timer--tick)))
    (posframe-timer--cancel-tick)))

(defun posframe-timer--run (seconds)
  "Begin counting down SECONDS from now."
  (setq posframe-timer--remaining (float seconds)
        posframe-timer--end-time (time-add nil seconds)
        posframe-timer--countdown-hidden nil)
  (posframe-timer--sync-tick)
  (posframe-timer--refresh))

(defun posframe-timer--finish ()
  "Handle the countdown reaching zero.  Called from the tick."
  (setq posframe-timer--remaining 0
        posframe-timer--end-time nil)
  (run-hooks 'posframe-timer-finished-hook)
  (if (require 'alert nil t)
      (alert (format-message "Timer finished (%s)"
                             (posframe-timer--format-duration
                              posframe-timer--duration))
             :title "posframe-timer" :severity 'high)
    (message "posframe-timer: time's up!"))
  (ding))

(defun posframe-timer--format-duration (seconds)
  (let ((m (/ (round seconds) 60)) (s (% (round seconds) 60)))
    (cond ((zerop s) (format "%dm" m))
          ((zerop m) (format "%ds" s))
          (t (format "%dm%02ds" m s)))))

;;; Duration parsing

(defun posframe-timer--parse-duration (input)
  "Parse INPUT into seconds.
Accepts plain minutes (\"25\", \"7.5\"), clock forms (\"25:30\",
\"1:10:00\"), and unit suffixes (\"90s\", \"25m\", \"1h30m\")."
  (let ((s (downcase (string-trim input))))
    (cond
     ((string-match-p "\\`[0-9]*\\.?[0-9]+\\'" s)
      (round (* 60 (string-to-number s))))
     ((string-match
       "\\`\\([0-9]+\\):\\([0-9]\\{1,2\\}\\)\\(?::\\([0-9]\\{1,2\\}\\)\\)?\\'" s)
      (let ((a (string-to-number (match-string 1 s)))
            (b (string-to-number (match-string 2 s)))
            (c (match-string 3 s)))
        (if c
            (+ (* 3600 a) (* 60 b) (string-to-number c))
          (+ (* 60 a) b))))
     (t
      (let ((total 0) (pos 0))
        (while (eq pos (string-match
                        "\\([0-9]*\\.?[0-9]+\\)\\([hms]\\)" s pos))
          (cl-incf total (* (string-to-number (match-string 1 s))
                            (pcase (match-string 2 s)
                              ("h" 3600) ("m" 60) ("s" 1))))
          (setq pos (match-end 0)))
        (if (and (> pos 0) (= pos (length s)))
            (round total)
          (user-error "posframe-timer: cannot parse duration %S" input)))))))

;;; Org clock integration

(defun posframe-timer--org-clock-update (&rest _)
  "React to the org clock starting, stopping, or being cancelled."
  (posframe-timer--sync-tick)
  (posframe-timer--refresh))

(defun posframe-timer--org-clock-setup ()
  "Install the org-clock hooks and pick up an already-running clock.
Display is gated by `posframe-timer-org-clock-integration', so the
hooks themselves are unconditional and cost nothing when it is nil."
  (add-hook 'org-clock-in-hook #'posframe-timer--org-clock-update)
  (add-hook 'org-clock-out-hook #'posframe-timer--org-clock-update)
  (add-hook 'org-clock-cancel-hook #'posframe-timer--org-clock-update)
  (when (posframe-timer--org-clock-active-p)
    (posframe-timer--org-clock-update)))

(with-eval-after-load 'org-clock
  (posframe-timer--org-clock-setup))

;;; Free clocks and the timesheet

(defun posframe-timer--clock-reset ()
  "Clear all free-clock state and take the clock off the display."
  (setq posframe-timer--clock-start nil
        posframe-timer--clock-label nil
        posframe-timer--clock-on-out nil
        posframe-timer--clock-on-cancel nil)
  (posframe-timer--sync-tick)
  (posframe-timer--refresh))

(defun posframe-timer--timesheet-buffer ()
  "The buffer visiting `posframe-timer-timesheet-file', creating dirs."
  (let ((file (expand-file-name posframe-timer-timesheet-file)))
    (make-directory (file-name-directory file) t)
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defun posframe-timer--buffer-headings (buffer)
  "All heading titles in org BUFFER (sans TODO keywords), for completion."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (headings)
          (while (re-search-forward org-complex-heading-regexp nil t)
            (when-let* ((title (match-string-no-properties 4)))
              (push (string-trim title) headings)))
          (delete-dups (nreverse headings)))))))

(defun posframe-timer--clock-minutes (start end)
  "Whole minutes between START and END as org computes them.
Org timestamps have minute resolution, so both ends are truncated to
the minute first; the stored duration then always matches what org
would recompute from the timestamps."
  (max 0 (- (floor (float-time end) 60)
            (floor (float-time start) 60))))

(defun posframe-timer--clock-line (start end)
  "A closed org CLOCK line for the interval START..END."
  (let ((mins (posframe-timer--clock-minutes start end)))
    (format "CLOCK: %s--%s => %2d:%02d"
            (format-time-string (org-time-stamp-format t t) start)
            (format-time-string (org-time-stamp-format t t) end)
            (/ mins 60) (% mins 60))))

(defun posframe-timer--insert-clock (buffer heading start end)
  "File a closed CLOCK line for START..END under HEADING in BUFFER.
The heading is created at the end of the file when missing.  The clock
line goes wherever org-clock would put it (`org-clock-find-position'
creates and fills the LOGBOOK drawer per `org-clock-into-drawer')."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((pos (org-find-exact-headline-in-buffer heading nil t)))
          (if pos
              (goto-char pos)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " heading "\n")
            (forward-line -1)))
        (org-clock-find-position nil)
        (insert-before-markers "\n")
        (backward-char 1)
        (insert (posframe-timer--clock-line start end))))
    (save-buffer)))

;;; Commands

;;;###autoload
(defun posframe-timer-start (&optional arg)
  "Start the countdown, or resume it if paused.
With numeric prefix ARG, count down that many minutes.  With no
countdown pending, use the last set duration (initially
`posframe-timer-default-duration')."
  (interactive "P")
  (setq posframe-timer--countdown-hidden nil)
  (cond
   (arg
    (setq posframe-timer--duration (* 60 (prefix-numeric-value arg)))
    (posframe-timer--run posframe-timer--duration))
   ((posframe-timer--running-p)
    (posframe-timer--refresh)
    (message "posframe-timer: already running (%s left)"
             (string-trim (substring-no-properties (posframe-timer--format)))))
   ((posframe-timer--paused-p)
    (posframe-timer-resume))
   (t
    (posframe-timer--run posframe-timer--duration))))

;;;###autoload
(defun posframe-timer-set (duration)
  "Prompt for DURATION and start counting it down.
Accepts minutes (\"25\"), clock forms (\"25:30\", \"1:10:00\"), or unit
suffixes (\"90s\", \"1h30m\").  Becomes the duration `posframe-timer-start'
restarts with."
  (interactive
   (list (read-string
          (format "Timer duration (default %s): "
                  (posframe-timer--format-duration posframe-timer--duration))
          nil nil
          (posframe-timer--format-duration posframe-timer--duration))))
  (let ((seconds (if (numberp duration)
                     duration
                   (posframe-timer--parse-duration duration))))
    (when (<= seconds 0)
      (user-error "posframe-timer: duration must be positive"))
    (setq posframe-timer--duration seconds)
    (posframe-timer--run seconds)))

;;;###autoload
(defun posframe-timer-pause ()
  "Pause the countdown, freezing the remaining time."
  (interactive)
  (unless (posframe-timer--running-p)
    (user-error "posframe-timer: not running"))
  (setq posframe-timer--remaining
        (max 0 (float-time (time-subtract posframe-timer--end-time nil)))
        posframe-timer--end-time nil)
  (posframe-timer--sync-tick)
  (posframe-timer--refresh))

;;;###autoload
(defun posframe-timer-resume ()
  "Resume a paused countdown."
  (interactive)
  (unless (posframe-timer--paused-p)
    (user-error "posframe-timer: nothing paused"))
  (posframe-timer--run posframe-timer--remaining))

;;;###autoload
(defun posframe-timer-toggle-pause ()
  "Pause the countdown if running, resume it if paused."
  (interactive)
  (cond ((posframe-timer--running-p) (posframe-timer-pause))
        ((posframe-timer--paused-p) (posframe-timer-resume))
        (t (user-error "posframe-timer: no countdown to pause"))))

;;;###autoload
(defun posframe-timer-stop ()
  "Pause the countdown and take it off the display.
`posframe-timer-start' resumes from where it stopped.  Any running
clocks stay on screen."
  (interactive)
  (when (posframe-timer--running-p)
    (setq posframe-timer--remaining
          (max 0 (float-time (time-subtract posframe-timer--end-time nil)))
          posframe-timer--end-time nil))
  (setq posframe-timer--countdown-hidden t)
  (posframe-timer--sync-tick)
  (posframe-timer--refresh))

;;;###autoload
(defun posframe-timer-clear ()
  "Cancel the countdown and reset it to the default.
Any running clocks stay on screen; with none, the posframe is deleted."
  (interactive)
  (setq posframe-timer--remaining nil
        posframe-timer--end-time nil
        posframe-timer--duration posframe-timer-default-duration
        posframe-timer--position nil
        posframe-timer--countdown-hidden nil)
  (posframe-timer--sync-tick)
  (posframe-timer--refresh)
  (when (and (null (posframe-timer--render))
             (featurep 'posframe)
             (get-buffer posframe-timer--buffer))
    (posframe-delete posframe-timer--buffer)
    (setq posframe-timer--frame nil)))

;;;###autoload
(defun posframe-timer-clock-in (&optional label on-out on-cancel)
  "Start a free clock, not attached to any org buffer.
It counts up in the posframe like an org clock.  LABEL, when
non-empty, is shown word-wrapped above the clock and offered as the
default heading at `posframe-timer-clock-out'.

Lisp callers can claim the clock's outcome: ON-OUT, when non-nil,
is called by `posframe-timer-clock-out' with the start time, end
time and label in place of the timesheet flow; ON-CANCEL, when
non-nil, is called by `posframe-timer-clock-cancel' with the start
time and label.  Both run after the clock state has been cleared,
so they may start a new clock."
  (interactive (list (read-string "Clock label (optional): ")))
  (when posframe-timer--clock-start
    (user-error "posframe-timer: already clocked in since %s"
                (format-time-string "%H:%M" posframe-timer--clock-start)))
  (setq posframe-timer--clock-start (current-time)
        posframe-timer--clock-label
        (let ((l (string-trim (or label ""))))
          (unless (string-empty-p l) l))
        posframe-timer--clock-on-out on-out
        posframe-timer--clock-on-cancel on-cancel)
  (posframe-timer--sync-tick)
  (posframe-timer--refresh)
  (message "posframe-timer: clocked in%s"
           (if posframe-timer--clock-label
               (format " (%s)" posframe-timer--clock-label)
             "")))

;;;###autoload
(defun posframe-timer-clock-out ()
  "Stop the free clock and file it in `posframe-timer-timesheet-file'.
A clock started with an ON-OUT handler (see `posframe-timer-clock-in')
is handed to that handler instead.  Otherwise this prompts for the org
heading to deposit the CLOCK line under, with completion over the
file's existing headings; the heading and the file are created when
they do not exist yet.  The clock stops at the moment this command is
invoked, not when the prompt is answered."
  (interactive)
  (unless posframe-timer--clock-start
    (user-error "posframe-timer: no free clock running"))
  (if posframe-timer--clock-on-out
      (let ((on-out posframe-timer--clock-on-out)
            (start posframe-timer--clock-start)
            (label posframe-timer--clock-label)
            (end (current-time)))
        (posframe-timer--clock-reset)
        (funcall on-out start end label))
    (require 'org)
    (require 'org-clock)
    (let* ((start posframe-timer--clock-start)
           (end (current-time))
           (buffer (posframe-timer--timesheet-buffer))
           (heading (string-trim
                     (completing-read
                      (format "File clock under heading%s: "
                              (if posframe-timer--clock-label
                                  (format " (default %s)"
                                          posframe-timer--clock-label)
                                ""))
                      (posframe-timer--buffer-headings buffer)
                      nil nil nil nil posframe-timer--clock-label))))
      (when (string-empty-p heading)
        (user-error "posframe-timer: heading must not be empty"))
      (posframe-timer--insert-clock buffer heading start end)
      (posframe-timer--clock-reset)
      (let ((mins (posframe-timer--clock-minutes start end)))
        (message "posframe-timer: clocked %d:%02d to \"%s\" in %s"
                 (/ mins 60) (% mins 60) heading
                 (abbreviate-file-name (buffer-file-name buffer)))))))

;;;###autoload
(defun posframe-timer-clock-cancel ()
  "Discard the free clock without writing anything.
A clock started with an ON-CANCEL handler (see
`posframe-timer-clock-in') has that handler notified."
  (interactive)
  (unless posframe-timer--clock-start
    (user-error "posframe-timer: no free clock running"))
  (let ((elapsed (posframe-timer--format-elapsed posframe-timer--clock-start))
        (on-cancel posframe-timer--clock-on-cancel)
        (start posframe-timer--clock-start)
        (label posframe-timer--clock-label))
    (posframe-timer--clock-reset)
    (when on-cancel (funcall on-cancel start label))
    (message "posframe-timer: discarded clock (%s)" elapsed)))

;;;###autoload
(defun posframe-timer-toggle-display ()
  "Switch the live display between the posframe and the mode line."
  (interactive)
  (setq posframe-timer-use-posframe (not posframe-timer-use-posframe))
  (when (posframe-timer--render)
    (unless (posframe-timer--posframe-usable-p)
      (when (and (featurep 'posframe) (get-buffer posframe-timer--buffer))
        (posframe-hide posframe-timer--buffer)))
    (posframe-timer--refresh))
  (message "posframe-timer: %s display"
           (if (posframe-timer--posframe-usable-p) "posframe" "mode-line")))

(provide 'posframe-timer)
;;; posframe-timer.el ends here
