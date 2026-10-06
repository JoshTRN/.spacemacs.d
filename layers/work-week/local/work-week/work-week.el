;;; work-week.el --- Work-week calendar grid backed by org files -*- lexical-binding: t; -*-

;;; Commentary:

;; An Outlook-style week view: one column per workday (5 by default,
;; 7 with a prefix argument or `w'), 48 half-hour rows, point lands on
;; the current time cell.  Events live as org files under
;; `work-week-directory', one folder per day (YYYY-MM-DD), one file
;; per event.  An event file's first active timestamp with a time
;; range (<2026-10-05 Mon 09:00-10:30>) places it on the grid; its
;; first heading is the displayed title.
;;
;; h/j/k/l move by day and half hour, `v' anchors a block selection,
;; `V' a whole-week one, and RET opens a floating capture frame -- a
;; plain org-mode buffer whose first heading becomes the event title
;; (remaining lines become the body).  RET on an existing event
;; visits its file.  Tiling window managers need a rule to float the
;; frame; machine-local config overrides
;; `work-week-capture-frame-function' to register one.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-ex-search-next "evil-search")
(declare-function evil-ex-search-previous "evil-search")

(defgroup work-week nil
  "Half-hour work-week calendar over org files."
  :group 'applications
  :prefix "work-week-")

(defcustom work-week-directory "~/org/work-week/"
  "Root directory holding one folder per day (YYYY-MM-DD) of event files."
  :type 'directory)

(defcustom work-week-day-count 5
  "Number of day columns shown by default (5 workdays or 7 full week)."
  :type '(choice (const 5) (const 7)))

(defface work-week-header
  '((t :weight bold))
  "Face for day column headers.")

(defface work-week-today
  '((t :inherit (font-lock-keyword-face work-week-header)))
  "Face for today's column header.")

(defface work-week-time
  '((t :inherit font-lock-comment-face))
  "Face for the hour labels in the gutter.")

(defface work-week-grid
  '((t :inherit shadow))
  "Face for the grid lines.")

(defface work-week-event
  '((t :inherit highlight))
  "Face for cells covered by an event.")

(defface work-week-event-border
  '((t :inherit font-lock-comment-face))
  "Face supplying the outline color for event blocks.")

(defface work-week-selection
  '((t :inherit region))
  "Face for the cells spanned by an in-progress selection.")

(defface work-week-now
  '((t :underline (:color "orange")))
  "Face underlining the last completed half-hour row of today.")

(defface work-week-today-column
  '((t nil))
  "Face layered under today's column.
When it specifies no background, the frame's default background is
used, which makes today stand out in solaire-style buffers.")

(defconst work-week--gutter-width 7
  "Columns reserved for the hour labels.")

(defvar-local work-week--week-start nil
  "Time value (noon) of the Monday opening the displayed week.")

(defvar-local work-week--days 5)
(defvar-local work-week--day 0)
(defvar-local work-week--slot 16)
(defvar-local work-week--anchor nil
  "Cons (DAY . SLOT) where v/V anchored the selection, or nil.")

(defvar-local work-week--selection-week nil
  "Non-nil when the selection spans every displayed day (V).")

(defvar-local work-week--lane 0
  "Index into the current cell's overlapping events (sorted by lane).
Movement resets it to 0; n/p cycle it.")

(defvar-local work-week--events nil
  "Vector indexed by day of event plists (:start :end :title :file).")

(defvar-local work-week--cell-width 18)
(defvar-local work-week--selection-overlays nil)
(defvar-local work-week--now-overlay nil)

(defvar-local work-week--filled-height nil
  "Window pixel height the rows were last stretched to fill.")

;;;; Time helpers

(defun work-week--monday (&optional time)
  "Return noon of the Monday of the week containing TIME."
  (let* ((dec (decode-time time))
         (delta (mod (+ (nth 6 dec) 6) 7)))
    (encode-time 0 0 12 (- (nth 3 dec) delta) (nth 4 dec) (nth 5 dec))))

(defun work-week--day-time (day)
  (time-add work-week--week-start (days-to-time day)))

(defun work-week--day-string (day)
  (format-time-string "%Y-%m-%d" (work-week--day-time day)))

(defun work-week--day-directory (day)
  (expand-file-name (work-week--day-string day) work-week-directory))

(defun work-week--slot-time (slot)
  "Render SLOT (0-48) as HH:MM; 48 clamps to 23:59 for end-of-day."
  (if (>= slot 48)
      "23:59"
    (format "%02d:%02d" (/ slot 2) (* 30 (% slot 2)))))

(defun work-week--slot-label (slot)
  (if (/= 0 (% slot 2))
      ""
    (format "%02d:00" (/ slot 2))))

(defun work-week--sync-to-now ()
  "Point the view at the current week, day, and half hour."
  (setq work-week--week-start (work-week--monday))
  (let* ((dec (decode-time))
         (dow (mod (+ (nth 6 dec) 6) 7)))
    (setq work-week--day (min dow (1- work-week--days))
          work-week--slot (+ (* 2 (nth 2 dec))
                             (if (>= (nth 1 dec) 30) 1 0)))))

(defun work-week--ensure-directories ()
  (dotimes (day work-week--days)
    (make-directory (work-week--day-directory day) t)))

;;;; Event scanning

(defconst work-week--timestamp-regexp
  (concat "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\(?: [^ >]+\\)?"
          " \\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)"
          "\\(?: \\(\\+[0-9]+[dwmy]\\)\\)?>")
  "Active org timestamp with a time range; groups are DATE H M H M REPEAT.")

(defun work-week--parse-event (file)
  "Return the event plist for FILE, or nil if it has no timed timestamp."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (goto-char (point-min))
    (when (re-search-forward work-week--timestamp-regexp nil t)
      (let* ((date (match-string 1))
             (repeat (match-string 6))
             (start (+ (* 2 (string-to-number (match-string 2)))
                       (if (>= (string-to-number (match-string 3)) 30) 1 0)))
             (end-minute (string-to-number (match-string 5)))
             (end (+ (* 2 (string-to-number (match-string 4)))
                     (cond ((= end-minute 0) 0)
                           ((<= end-minute 30) 1)
                           (t 2))))
             (title (progn
                      (goto-char (point-min))
                      (if (re-search-forward "^\\*+ \\(.+\\)$" nil t)
                          (match-string 1)
                        (file-name-base file)))))
        (list :start (min start 47)
              :end (max end (1+ start))
              :title title
              :file file
              :date date
              :repeat repeat)))))

(defun work-week--date-days (date)
  "Absolute day number of DATE (YYYY-MM-DD)."
  (pcase-let ((`(,year ,month ,day)
               (mapcar #'string-to-number (split-string date "-"))))
    (time-to-days (encode-time 0 0 12 day month year))))

(defun work-week--occurrences (origin repeat)
  "Displayed day indexes hit by an event at ORIGIN repeating every REPEAT."
  (when (string-match "\\+\\([0-9]+\\)\\([dw]\\)" repeat)
    (let ((interval (* (string-to-number (match-string 1 repeat))
                       (if (equal (match-string 2 repeat) "w") 7 1)))
          (origin-days (work-week--date-days origin))
          result)
      (when (> interval 0)
        (dotimes (day work-week--days)
          (let ((diff (- (work-week--date-days (work-week--day-string day))
                         origin-days)))
            (when (and (>= diff 0) (zerop (mod diff interval)))
              (push day result)))))
      (nreverse result))))

(defun work-week--scan-events ()
  "Collect events for the displayed week.
Day folders place their files directly; files with a d/w repeater
are projected from their timestamp date onto every matching
displayed day, whichever week's folder they live in."
  (let ((events (make-vector work-week--days nil))
        (day-indexes (make-hash-table :test #'equal)))
    (dotimes (day work-week--days)
      (puthash (work-week--day-string day) day day-indexes))
    (when (file-directory-p work-week-directory)
      (dolist (dir (directory-files
                    work-week-directory t
                    "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"))
        (when (file-directory-p dir)
          (let ((day (gethash (file-name-nondirectory dir) day-indexes)))
            (dolist (file (directory-files dir t "\\`[^.].*\\.org\\'"))
              (when-let* ((event (work-week--parse-event file)))
                (let ((repeat (plist-get event :repeat)))
                  (if (and repeat (string-match-p "[dw]\\'" repeat))
                      (dolist (target (work-week--occurrences
                                       (plist-get event :date) repeat))
                        ;; A copy per occurrence: lanes are per-day.
                        (push (copy-sequence event) (aref events target)))
                    (when day
                      (push event (aref events day)))))))))))
    (dotimes (day work-week--days)
      (aset events day
            (work-week--assign-lanes
             (sort (aref events day)
                   (lambda (a b)
                     (let ((start-a (plist-get a :start))
                           (start-b (plist-get b :start)))
                       (if (/= start-a start-b)
                           (< start-a start-b)
                         (> (plist-get a :end) (plist-get b :end)))))))))
    events))

(defun work-week--assign-lanes (events)
  "Assign :lane and :lanes to EVENTS (sorted by start); return EVENTS.
Transitively-overlapping events form a cluster whose members render
side by side, splitting the day column into :lanes sub-columns."
  (let (actives cluster (lanes 0))
    (dolist (event events)
      (setq actives (seq-filter (lambda (active)
                                  (> (plist-get active :end)
                                     (plist-get event :start)))
                                actives))
      (unless actives
        (dolist (member cluster)
          (plist-put member :lanes lanes))
        (setq cluster nil
              lanes 0))
      (let ((lane 0))
        (while (seq-find (lambda (active)
                           (= (plist-get active :lane) lane))
                         actives)
          (setq lane (1+ lane)))
        (plist-put event :lane lane)
        (setq lanes (max lanes (1+ lane))))
      (push event actives)
      (push event cluster))
    (dolist (member cluster)
      (plist-put member :lanes lanes)))
  events)

(defun work-week--events-at (day slot)
  (seq-filter (lambda (event)
                (and (<= (plist-get event :start) slot)
                     (< slot (plist-get event :end))))
              (aref work-week--events day)))

(defun work-week--lane-events (day slot)
  "Events covering DAY/SLOT, sorted by lane."
  (sort (work-week--events-at day slot)
        (lambda (a b)
          (< (or (plist-get a :lane) 0)
             (or (plist-get b :lane) 0)))))

(defun work-week--event-at (day slot)
  "The leftmost-lane event covering DAY/SLOT, or nil."
  (car (work-week--lane-events day slot)))

(defun work-week--selected-event ()
  "The event under the cursor's lane at the current cell, or nil."
  (let ((events (work-week--lane-events work-week--day work-week--slot)))
    (nth (min work-week--lane (max 0 (1- (length events)))) events)))

;;;; Rendering

(defun work-week--pad (string width)
  (let ((string (truncate-string-to-width string width)))
    (concat string (make-string (- width (string-width string)) ?\s))))

(defun work-week--compose-cell (day slot width)
  "Return the WIDTH-wide string for the DAY/SLOT cell.
Overlapping events render side by side in their assigned lanes,
with a one-column gutter between occupied lanes."
  (let ((covering (work-week--events-at day slot)))
    (if (null covering)
        (make-string width ?\s)
      (let ((segments
             (sort
              (mapcar
               (lambda (event)
                 (let* ((lanes (max 1 (or (plist-get event :lanes) 1)))
                        (lane (or (plist-get event :lane) 0))
                        (from (/ (* lane width) lanes))
                        (to (/ (* (1+ lane) width) lanes)))
                   (when (< (1+ lane) lanes)
                     (setq to (1- to)))
                   (list from (min width (max to (1+ from))) event)))
               covering)
              (lambda (a b) (< (car a) (car b)))))
            (parts nil)
            (cursor 0))
        (pcase-dolist (`(,from ,to ,event) segments)
          (when (> from cursor)
            (push (make-string (- from cursor) ?\s) parts)
            (setq cursor from))
          (when (> to cursor)
            (let* ((border (face-attribute 'work-week-event-border
                                           :foreground nil t))
                   (border (if (stringp border) border t))
                   (edges (append
                           (when (= slot (plist-get event :start))
                             `((:overline ,border)))
                           (when (= slot (1- (plist-get event :end)))
                             `((:underline ,border)))))
                   (bar (propertize "▎" 'face
                                    `(,@edges
                                      ,@(when (stringp border)
                                          `((:foreground ,border)))
                                      work-week-event)))
                   (body (propertize
                          (work-week--pad
                           (if (= slot (plist-get event :start))
                               (plist-get event :title)
                             "")
                           (- to cursor 1))
                          'face `(,@edges work-week-event))))
              (push (concat bar body) parts))
            (setq cursor to)))
        (when (< cursor width)
          (push (make-string (- width cursor) ?\s) parts))
        (apply #'concat (nreverse parts))))))

(defun work-week--render ()
  (let* ((inhibit-read-only t)
         (window (get-buffer-window (current-buffer)))
         (total (if window (window-body-width window) (frame-width)))
         (width (max 12 (1- (/ (- total work-week--gutter-width)
                               work-week--days))))
         (separator (propertize "│" 'face 'work-week-grid))
         (today (format-time-string "%Y-%m-%d")))
    (setq work-week--cell-width width)
    (setq work-week--filled-height nil)
    (work-week--clear-selection-overlays)
    (erase-buffer)
    (setq header-line-format
          (list
           ;; Align the header with buffer column 0 across the fringe.
           (propertize " " 'display '(space :align-to 0))
           (propertize (format "%6s " (format-time-string
                                       "W%V" work-week--week-start))
                       'face 'work-week-time)
           (mapconcat
            (lambda (day)
              (concat separator
                      (propertize
                       (work-week--pad
                        (format-time-string " %-d %a" (work-week--day-time day))
                        width)
                       'face (if (string= (work-week--day-string day) today)
                                 'work-week-today
                               'work-week-header))))
            (number-sequence 0 (1- work-week--days)))))
    (let ((today-face
           `(work-week-today-column
             ,@(unless (face-background 'work-week-today-column nil t)
                 (when-let* ((bg (face-background 'default)))
                   `((:background ,bg)))))))
      (dotimes (slot 48)
        (insert (propertize (format "%6s " (work-week--slot-label slot))
                            'face 'work-week-time))
        (dotimes (day work-week--days)
          (let ((cell (work-week--compose-cell day slot width)))
            (when (string= (work-week--day-string day) today)
              (add-face-text-property 0 (length cell) today-face t cell))
            (insert separator cell)))
        (insert "\n")))
    (work-week--update-now-line)))

(defun work-week--update-now-line ()
  "Underline the row of today's last completed half hour."
  (when work-week--now-overlay
    (delete-overlay work-week--now-overlay)
    (setq work-week--now-overlay nil))
  (let* ((today (format-time-string "%Y-%m-%d"))
         (dec (decode-time))
         (slot (1- (+ (* 2 (nth 2 dec)) (if (>= (nth 1 dec) 30) 1 0))))
         (shown nil))
    (dotimes (day work-week--days)
      (when (string= (work-week--day-string day) today)
        (setq shown t)))
    (when (and shown (>= slot 0) (> (buffer-size) 0))
      (save-excursion
        (goto-char (point-min))
        (forward-line slot)
        (setq work-week--now-overlay
              (make-overlay (point) (line-end-position)))
        (overlay-put work-week--now-overlay 'face 'work-week-now)))))

(defvar work-week--now-timer nil
  "Minute timer keeping the now underline current while the buffer lives.")

(defun work-week--now-line-tick ()
  (let ((buffer (get-buffer "*work-week*")))
    (if (not (buffer-live-p buffer))
        (progn
          (cancel-timer work-week--now-timer)
          (setq work-week--now-timer nil))
      (with-current-buffer buffer
        (when (derived-mode-p 'work-week-mode)
          (work-week--update-now-line))))))

(defun work-week--schedule-now-line ()
  (unless work-week--now-timer
    (setq work-week--now-timer
          (run-at-time 60 60 #'work-week--now-line-tick))))

(defun work-week--fill-window-height ()
  "Stretch the 48 rows so the grid ends flush at the mode line.
Distributes the window's pixel height across the rows via the
`line-height' property; windows shorter than the natural grid are
left alone (the property never shrinks a line)."
  (when-let* ((window (get-buffer-window (current-buffer))))
    (let ((pixels (window-body-height window t)))
      (unless (eql pixels work-week--filled-height)
        (setq work-week--filled-height pixels)
        (let ((base (/ pixels 48))
              (extra (% pixels 48))
              (inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (dotimes (row 48)
              (end-of-line)
              (when (< (point) (point-max))
                (put-text-property (point) (1+ (point)) 'line-height
                                   (if (< row extra) (1+ base) base)))
              (forward-line 1))))))))

(defun work-week--window-resized (_window-or-frame)
  (work-week--fill-window-height))

(defun work-week--hold-window-start (window start)
  "Snap WINDOW back to the top whenever something scrolls the grid.
Skipped when the window is shorter than the natural 48 rows, where
scrolling is the only way to reach the rest of the day."
  (when (and (/= start 1)
             (>= (window-body-height window t)
                 (* 48 (frame-char-height (window-frame window)))))
    (set-window-vscroll window 0 t)
    (set-window-start window 1 t)))

(defun work-week--refill-visible ()
  "Re-stretch the grid after anything that changed window metrics.
Frame zoom resizes the font (and so the body pixel height) without
reliably firing the buffer-local resize hook; this runs from
`window-state-change-hook' and no-ops when the height is unchanged."
  (let ((buffer (get-buffer "*work-week*")))
    (when (and buffer (get-buffer-window buffer t))
      (with-current-buffer buffer
        (when (derived-mode-p 'work-week-mode)
          (work-week--fill-window-height))))))

(add-hook 'window-state-change-hook #'work-week--refill-visible)

(defun work-week--cell-position (day slot)
  (save-excursion
    (goto-char (point-min))
    (forward-line slot)
    (+ (point)
       work-week--gutter-width
       (* day (1+ work-week--cell-width))
       1)))

(defun work-week--sync-from-point ()
  "Derive day/slot/lane from wherever point actually is.
Runs from `pre-command-hook': searches, mouse clicks, or any other
motion move point without updating the model, so every command
re-reads it from the buffer position first."
  (ignore-errors
    (when (and work-week--events (> (buffer-size) 0))
      (let* ((slot (min 47 (max 0 (1- (line-number-at-pos)))))
             (column (current-column))
             (cell (1+ work-week--cell-width))
             (day (min (1- work-week--days)
                       (max 0 (/ (- column work-week--gutter-width) cell))))
             (offset (- column work-week--gutter-width (* day cell) 1))
             (lane 0)
             (index 0))
        (setq work-week--slot slot
              work-week--day day)
        (dolist (event (work-week--lane-events day slot))
          (let* ((lanes (max 1 (or (plist-get event :lanes) 1)))
                 (l (or (plist-get event :lane) 0))
                 (from (/ (* l work-week--cell-width) lanes))
                 (to (/ (* (1+ l) work-week--cell-width) lanes)))
            (when (and (<= from offset) (< offset to))
              (setq lane index)))
          (setq index (1+ index)))
        (setq work-week--lane lane)))))

(defun work-week--goto-cell (&optional keep-lane)
  (unless keep-lane
    (setq work-week--lane 0))
  (let ((offset 0)
        (events (work-week--lane-events work-week--day work-week--slot)))
    (when (and events (> work-week--lane 0))
      (setq work-week--lane (min work-week--lane (1- (length events))))
      (let* ((event (nth work-week--lane events))
             (lanes (max 1 (or (plist-get event :lanes) 1)))
             (lane (or (plist-get event :lane) 0)))
        (setq offset (/ (* lane work-week--cell-width) lanes))))
    (goto-char (+ (work-week--cell-position work-week--day work-week--slot)
                  offset)))
  (work-week--update-selection))

(defun work-week--clear-selection-overlays ()
  (mapc #'delete-overlay work-week--selection-overlays)
  (setq work-week--selection-overlays nil))

(defun work-week--selection-days ()
  "Day indexes covered by the live selection, in ascending order."
  (if work-week--selection-week
      (number-sequence 0 (1- work-week--days))
    (number-sequence (min (car work-week--anchor) work-week--day)
                     (max (car work-week--anchor) work-week--day))))

(defun work-week--update-selection ()
  (work-week--clear-selection-overlays)
  (when work-week--anchor
    (dolist (day (work-week--selection-days))
      (dolist (slot (number-sequence
                     (min (cdr work-week--anchor) work-week--slot)
                     (max (cdr work-week--anchor) work-week--slot)))
        (let* ((start (work-week--cell-position day slot))
               (overlay (make-overlay start (+ start work-week--cell-width))))
          (overlay-put overlay 'face 'work-week-selection)
          (push overlay work-week--selection-overlays))))))

;;;; Commands

(defun work-week-refresh ()
  "Rescan the day folders and redraw the grid."
  (interactive)
  (setq work-week--anchor nil)
  (setq work-week--events (work-week--scan-events))
  (work-week--render)
  (work-week--fill-window-height)
  (work-week--goto-cell))

(defun work-week-left ()
  "Move left: the previous lane here, else the previous day's last lane."
  (interactive)
  (if (> work-week--lane 0)
      (progn
        (setq work-week--lane (1- work-week--lane))
        (work-week--goto-cell t))
    (when (> work-week--day 0)
      (setq work-week--day (1- work-week--day))
      (setq work-week--lane
            (max 0 (1- (length (work-week--events-at work-week--day
                                                     work-week--slot)))))
      (work-week--goto-cell t))))

(defun work-week-backward-day ()
  "Move to the same time row the previous day."
  (interactive)
  (setq work-week--day (max 0 (1- work-week--day)))
  (work-week--goto-cell))

(defun work-week-right ()
  "Move right: the next lane here, else the next day at the same time."
  (interactive)
  (let ((count (length (work-week--events-at work-week--day work-week--slot))))
    (if (and (> count 1) (< work-week--lane (1- count)))
        (progn
          (setq work-week--lane (1+ work-week--lane))
          (work-week--goto-cell t))
      (when (< work-week--day (1- work-week--days))
        (setq work-week--day (1+ work-week--day))
        (work-week--goto-cell)))))

(defun work-week-forward-day ()
  "Move to the same time row the next day."
  (interactive)
  (setq work-week--day (min (1- work-week--days) (1+ work-week--day)))
  (work-week--goto-cell))

(defun work-week-down ()
  "Move 30 minutes later."
  (interactive)
  (setq work-week--slot (min 47 (1+ work-week--slot)))
  (work-week--goto-cell))

(defun work-week-up ()
  "Move 30 minutes earlier."
  (interactive)
  (setq work-week--slot (max 0 (1- work-week--slot)))
  (work-week--goto-cell))

(defun work-week--time-slot (count)
  "Slot for COUNT read as a 24-hour time: 14 → 14:00, 930/1430 → 9:30/14:30."
  (let* ((hour (if (> count 23) (/ count 100) count))
         (minute (if (> count 23) (% count 100) 0)))
    (unless (and (<= 0 hour 23) (<= 0 minute 59))
      (user-error "No such time: %d" count))
    (+ (* 2 hour) (if (>= minute 30) 1 0))))

(defun work-week-goto-time (&optional count)
  "Jump to COUNT read as a 24-hour time (10 → 10:00, 1430 → 14:30).
Without a count, jump to the last row of the day."
  (interactive "P")
  (setq work-week--slot
        (if count (work-week--time-slot (prefix-numeric-value count)) 47))
  (work-week--goto-cell))

(defun work-week-goto-time-or-first (&optional count)
  "Jump to COUNT read as a 24-hour time, else the first row of the day."
  (interactive "P")
  (setq work-week--slot
        (if count (work-week--time-slot (prefix-numeric-value count)) 0))
  (work-week--goto-cell))

(defun work-week-begin-selection ()
  "Anchor a block selection at the current cell; h/j/k/l stretch it."
  (interactive)
  (setq work-week--anchor (cons work-week--day work-week--slot)
        work-week--selection-week nil)
  (work-week--update-selection))

(defun work-week-begin-week-selection ()
  "Anchor a selection spanning every day column; j/k stretch the rows."
  (interactive)
  (setq work-week--anchor (cons work-week--day work-week--slot)
        work-week--selection-week t)
  (work-week--update-selection))

(defun work-week-cancel ()
  "Drop the in-progress selection."
  (interactive)
  (setq work-week--anchor nil
        work-week--selection-week nil)
  (work-week--update-selection))

(defun work-week-ret ()
  "Capture the selected span, visit the event at point, or capture one slot."
  (interactive)
  (cond
   (work-week--anchor
    (let ((days (work-week--selection-days))
          (start (min (cdr work-week--anchor) work-week--slot))
          (end (1+ (max (cdr work-week--anchor) work-week--slot))))
      (work-week-cancel)
      (work-week--capture days start end)))
   ((work-week--selected-event)
    (find-file (plist-get (work-week--selected-event) :file)))
   (t
    (work-week--capture (list work-week--day) work-week--slot
                        (1+ work-week--slot)))))

(defun work-week--stop< (a b)
  "Order (DAY SLOT LANE-INDEX) stops lexicographically."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
           (or (< (nth 1 a) (nth 1 b))
               (and (= (nth 1 a) (nth 1 b))
                    (< (nth 2 a) (nth 2 b)))))))

(defun work-week--event-stops ()
  "Chronological (DAY SLOT LANE-INDEX) stops, one per event start.
Overlapping events each get their own stop, ordered by lane."
  (let (stops)
    (dotimes (day work-week--days)
      (dolist (event (aref work-week--events day))
        (let ((slot (plist-get event :start)))
          (push (list day slot
                      (or (seq-position (work-week--lane-events day slot)
                                        event)
                          0))
                stops))))
    (sort stops #'work-week--stop<)))

(defun work-week--goto-stop (stop)
  (unless stop
    (user-error "No events this week"))
  (setq work-week--day (nth 0 stop)
        work-week--slot (nth 1 stop)
        work-week--lane (nth 2 stop))
  (work-week--goto-cell t))

(defun work-week-next-event ()
  "Jump to the next event start, lane by lane, wrapping around."
  (interactive)
  (let ((here (list work-week--day work-week--slot work-week--lane))
        (stops (work-week--event-stops)))
    (work-week--goto-stop
     (or (seq-find (lambda (stop) (work-week--stop< here stop)) stops)
         (car stops)))))

(defun work-week-previous-event ()
  "Jump to the previous event start, lane by lane, wrapping around."
  (interactive)
  (let ((here (list work-week--day work-week--slot work-week--lane))
        (stops (work-week--event-stops)))
    (work-week--goto-stop
     (or (seq-find (lambda (stop) (work-week--stop< stop here))
                   (reverse stops))
         (car (last stops))))))

(defun work-week-next-lane ()
  "Cycle the cursor to the next overlapping event's lane here."
  (interactive)
  (let ((count (length (work-week--events-at work-week--day work-week--slot))))
    (if (< count 2)
        (user-error "No overlapping events here")
      (setq work-week--lane (mod (1+ work-week--lane) count))
      (work-week--goto-cell t))))

(defun work-week-previous-lane ()
  "Cycle the cursor to the previous overlapping event's lane here."
  (interactive)
  (let ((count (length (work-week--events-at work-week--day work-week--slot))))
    (if (< count 2)
        (user-error "No overlapping events here")
      (setq work-week--lane (mod (1- work-week--lane) count))
      (work-week--goto-cell t))))

(defun work-week-delete-event ()
  "Delete the event file under point after confirming."
  (interactive)
  (let ((event (work-week--selected-event)))
    (unless event
      (user-error "No event here"))
    (when (y-or-n-p (format "Delete \"%s\"%s? "
                            (plist-get event :title)
                            (if (plist-get event :repeat)
                                " (recurring; removes all occurrences)"
                              "")))
      (delete-file (plist-get event :file) t)
      (work-week-refresh))))

(defun work-week-delete-all-events ()
  "Delete every event covering this cell after confirming.
With a single event here this is the same as `work-week-delete-event'."
  (interactive)
  (let ((events (work-week--lane-events work-week--day work-week--slot)))
    (cond
     ((null events)
      (user-error "No event here"))
     ((null (cdr events))
      (work-week-delete-event))
     ((y-or-n-p (format "Delete all %d events here (%s)%s? "
                        (length events)
                        (mapconcat (lambda (event) (plist-get event :title))
                                   events ", ")
                        (if (seq-some (lambda (event)
                                        (plist-get event :repeat))
                                      events)
                            " (some recurring; removes all occurrences)"
                          "")))
      (dolist (event events)
        (delete-file (plist-get event :file) t))
      (work-week-refresh)))))

(defun work-week-goto-today ()
  "Jump back to the current time cell of the current week."
  (interactive)
  (work-week--sync-to-now)
  (work-week--ensure-directories)
  (work-week-refresh))

(defun work-week-previous-week ()
  "Show the previous week."
  (interactive)
  (setq work-week--week-start
        (time-subtract work-week--week-start (days-to-time 7)))
  (work-week--ensure-directories)
  (work-week-refresh))

(defun work-week-next-week ()
  "Show the next week."
  (interactive)
  (setq work-week--week-start
        (time-add work-week--week-start (days-to-time 7)))
  (work-week--ensure-directories)
  (work-week-refresh))

(defun work-week-toggle-weekend ()
  "Toggle between the 5-day work week and the full 7-day week."
  (interactive)
  (setq work-week--days (if (= work-week--days 7) 5 7))
  (setq work-week--day (min work-week--day (1- work-week--days)))
  (work-week--ensure-directories)
  (work-week-refresh))

;;;; Capture frame

(defvar work-week--capture-buffer "*work-week capture*")

(defconst work-week-capture-frame-name "work-week-capture"
  "Frame name (and so window-manager title) of the capture frame.
Window-manager float rules match on it.")

(defvar work-week-capture-frame-function #'work-week-make-capture-frame
  "Function creating the floating frame for the capture buffer.
Called with the parent frame; must return a new frame.  Tiling
window managers tile new frames, so machine-local config overrides
this with a wrapper that registers a float rule first (Hyprland,
Aerospace, ...) and then calls `work-week-make-capture-frame'.")

(defun work-week-make-capture-frame (parent)
  "Return a frame 80% the size of PARENT, centered over it when possible."
  (let ((params `((name . ,work-week-capture-frame-name)
                  (width . ,(round (* 0.8 (frame-width parent))))
                  (height . ,(round (* 0.8 (frame-height parent))))))
        (parent-left (frame-parameter parent 'left))
        (parent-top (frame-parameter parent 'top)))
    ;; Wayland ignores frame positions; elsewhere center over the parent.
    (when (and (integerp parent-left) (integerp parent-top))
      (push (cons 'left (+ parent-left
                           (round (* 0.1 (frame-pixel-width parent)))))
            params)
      (push (cons 'top (+ parent-top
                          (round (* 0.1 (frame-pixel-height parent)))))
            params))
    (make-frame params)))

(defvar-local work-week--capture-target nil
  "List (CALENDAR-BUFFER DAY-TIMES START-SLOT END-SLOT) for the capture.
DAY-TIMES is a list of time values, one per selected day.")

(defvar-local work-week--capture-parent-frame nil)
(defvar-local work-week--capture-frame nil)

(defvar work-week-capture-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'work-week-capture-finish)
    (define-key map (kbd "C-c C-k") #'work-week-capture-cancel)
    map))

(define-minor-mode work-week-capture-minor-mode
  "Commit/cancel bindings for a work-week capture buffer.
The buffer itself is plain `org-mode', so every org binding and
leader key works as usual."
  :lighter " WW-capture")

(defun work-week--calm-capture-styling ()
  "Restore a normal-height header line inside the capture buffer.
The 4x header-line remap comes from org-mode-hook; every other org
face is left exactly as configured."
  (setq-local face-remapping-alist
              (assq-delete-all 'header-line
                               (copy-alist face-remapping-alist)))
  (face-remap-add-relative
   'header-line `(:height ,(face-attribute 'default :height nil 'default)))
  (setq-local show-trailing-whitespace nil))

(defun work-week--capture (days start end)
  "Pop a floating org capture frame for an event on DAYS from START to END.
DAYS is a list of day indexes; commit writes one event file per day."
  (let ((calendar-buffer (current-buffer))
        (parent-frame (selected-frame))
        (day-times (mapcar #'work-week--day-time days))
        (buffer (get-buffer-create work-week--capture-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (work-week-capture-minor-mode 1)
      (work-week--calm-capture-styling)
      (insert "* "))
    (let ((frame (funcall work-week-capture-frame-function parent-frame)))
      (select-frame-set-input-focus frame)
      (switch-to-buffer buffer)
      (with-current-buffer buffer
        (setq work-week--capture-target
              (list calendar-buffer day-times start end))
        (setq work-week--capture-parent-frame parent-frame)
        (setq work-week--capture-frame frame)
        (setq header-line-format
              (format " New event  %s %s–%s   , , commit   , k cancel"
                      (if (cdr day-times)
                          (format "%s – %s"
                                  (format-time-string "%a %b %-d"
                                                      (car day-times))
                                  (format-time-string "%a %b %-d"
                                                      (car (last day-times))))
                        (format-time-string "%a %b %-d" (car day-times)))
                      (work-week--slot-time start)
                      (work-week--slot-time end)))
        (goto-char (point-max))
        (when (and (fboundp 'evil-insert-state)
                   (bound-and-true-p evil-local-mode))
          (evil-insert-state))))))

(defun work-week--capture-teardown (frame parent-frame)
  "Close the capture FRAME, refocus PARENT-FRAME, drop the buffer."
  (when (and frame (frame-live-p frame))
    (delete-frame frame))
  (when (frame-live-p parent-frame)
    (select-frame-set-input-focus parent-frame))
  (when-let* ((buffer (get-buffer work-week--capture-buffer)))
    (kill-buffer buffer)))

(defun work-week--unique-file (directory base)
  (let ((file (expand-file-name (concat base ".org") directory))
        (n 1))
    (while (file-exists-p file)
      (setq file (expand-file-name (format "%s-%d.org" base n) directory))
      (setq n (1+ n)))
    file))

(defun work-week-capture-finish ()
  "Write the capture out as an event file per day and redraw the calendar."
  (interactive)
  (pcase-let ((`(,calendar-buffer ,day-times ,start ,end)
               work-week--capture-target)
              (parent-frame work-week--capture-parent-frame)
              (capture-frame work-week--capture-frame))
    (let* ((lines (split-string (buffer-string) "\n"))
           (title (string-trim
                   (replace-regexp-in-string "\\`\\*+[ \t]*" "" (car lines))))
           (body (string-trim (string-join (cdr lines) "\n")))
           (slug (string-trim
                  (replace-regexp-in-string "[^[:alnum:]]+" "-"
                                            (downcase title))
                  "-" "-"))
           (created nil))
      (when (string-empty-p title)
        (user-error "Event needs a title on the first line"))
      (dolist (day-time day-times)
        (let* ((date (format-time-string "%Y-%m-%d" day-time))
               (directory (expand-file-name date work-week-directory))
               (file (work-week--unique-file
                      directory
                      (format "%s-%s"
                              (string-replace ":" ""
                                              (work-week--slot-time start))
                              slug))))
          (make-directory directory t)
          (with-temp-file file
            (insert (format "* %s\n<%s %s %s-%s>\n"
                            title date
                            (format-time-string "%a" day-time)
                            (work-week--slot-time start)
                            (work-week--slot-time end)))
            (unless (string-empty-p body)
              (insert "\n" body "\n")))
          (push file created)))
      (work-week--capture-teardown capture-frame parent-frame)
      (when (buffer-live-p calendar-buffer)
        (with-current-buffer calendar-buffer
          (work-week-refresh)))
      (message "Created %s"
               (if (cdr created)
                   (format "%d events" (length created))
                 (file-name-nondirectory (car created)))))))

(defun work-week-capture-cancel ()
  "Abandon the capture."
  (interactive)
  (work-week--capture-teardown work-week--capture-frame
                               work-week--capture-parent-frame))

;;;; Mode

(defvar work-week-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" #'work-week-left)
    (define-key map "l" #'work-week-right)
    (define-key map "j" #'work-week-down)
    (define-key map "k" #'work-week-up)
    (define-key map "v" #'work-week-begin-selection)
    (define-key map "V" #'work-week-begin-week-selection)
    (define-key map "D" #'work-week-delete-all-events)
    (define-key map (kbd "RET") #'work-week-ret)
    (define-key map (kbd "<tab>") #'work-week-next-event)
    (define-key map (kbd "TAB") #'work-week-next-event)
    (define-key map (kbd "<backtab>") #'work-week-previous-event)
    (define-key map "[" #'work-week-previous-week)
    (define-key map "]" #'work-week-next-week)
    (define-key map "t" #'work-week-goto-today)
    (define-key map "w" #'work-week-forward-day)
    (define-key map "b" #'work-week-backward-day)
    (define-key map "T" #'work-week-toggle-weekend)
    (define-key map "r" #'work-week-refresh)
    (define-key map "G" #'work-week-goto-time)
    map))

(define-derived-mode work-week-mode special-mode "Work-Week"
  "Half-hour grid calendar over org files in `work-week-directory'."
  (setq-local truncate-lines t
              show-trailing-whitespace nil
              cursor-in-non-selected-windows nil
              ;; The global scroll-margin 4 keeps the cursor off the mode
              ;; line, which reads as dead space under the 11:30 PM row.
              scroll-margin 0)
  (add-hook 'window-size-change-functions
            #'work-week--window-resized nil t)
  (add-hook 'window-scroll-functions
            #'work-week--hold-window-start nil t)
  (add-hook 'pre-command-hook #'work-week--sync-from-point nil t)
  (work-week--schedule-now-line)
  (buffer-disable-undo))

;;;###autoload
(defun work-week (&optional arg)
  "Open the work-week calendar on the current time cell.
With prefix ARG, show the full 7-day week instead of
`work-week-day-count' columns."
  (interactive "P")
  (let ((buffer (get-buffer-create "*work-week*")))
    (pop-to-buffer-same-window buffer)
    (unless (derived-mode-p 'work-week-mode)
      (work-week-mode))
    (setq work-week--days (if arg 7 work-week-day-count))
    (work-week--sync-to-now)
    (work-week--ensure-directories)
    (work-week-refresh)))

;;;; Evil integration

;; Evil's state maps outrank major-mode maps, so the vim motions must
;; be registered with Evil directly (same approach as zoho-desk).
(with-eval-after-load 'evil
  (evil-set-initial-state 'work-week-mode 'normal)
  (evil-define-key* 'normal work-week-mode-map
    "h" #'work-week-left
    "l" #'work-week-right
    "j" #'work-week-down
    "k" #'work-week-up
    "v" #'work-week-begin-selection
    "V" #'work-week-begin-week-selection
    (kbd "dd") #'work-week-delete-event
    "D" #'work-week-delete-all-events
    (kbd "RET") #'work-week-ret
    (kbd "<tab>") #'work-week-next-event
    (kbd "TAB") #'work-week-next-event
    (kbd "<backtab>") #'work-week-previous-event
    (kbd "<escape>") #'work-week-cancel
    "[" #'work-week-previous-week
    "]" #'work-week-next-week
    "t" #'work-week-goto-today
    "w" #'work-week-forward-day
    "b" #'work-week-backward-day
    "T" #'work-week-toggle-weekend
    "G" #'work-week-goto-time
    (kbd "gg") #'work-week-goto-time-or-first
    (kbd "g r") #'work-week-refresh
    "q" #'quit-window
    [wheel-up] #'ignore
    [wheel-down] #'ignore
    [wheel-left] #'ignore
    [wheel-right] #'ignore
    ;; The global config remaps searches to zz-style centered variants;
    ;; the grid fills the window exactly, so centering just shears it.
    (kbd "<remap> <evil-ex-search-next>") #'evil-ex-search-next
    (kbd "<remap> <evil-ex-search-previous>") #'evil-ex-search-previous))

(provide 'work-week)
;;; work-week.el ends here
