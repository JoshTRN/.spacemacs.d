;;; darwin.el --- macOS-specific config
;; -*- lexical-binding: t -*-

;; Rounded undecorated frames for all new frames
(add-to-list 'default-frame-alist '(undecorated-round . t))


(require 'org)
(require 'org-element)
(require 'subr-x)  ;; for string-empty-p

;; ---- Backward-compatible alert helpers ----

(defconst org-alert-default-icon
  "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/ToolbarInfo.icns")

(defun send-macos-alert-with-snooze (title msg icon snooze-str)
  "Show a macOS alert dialog with ICON and Snooze/OK buttons.
If Snooze is pressed, reschedule the alert after SNOOZE-STR delay
with the same TITLE, MSG, and ICON."
  (let* ((snooze-label (format "Snooze %s" snooze-str))
         (script (format
                  "display dialog %S with title %S with icon POSIX file %S buttons {\"%s\",\"OK\"} default button \"OK\""
                  msg title icon snooze-label))
         (result (with-temp-buffer
                   (call-process "osascript" nil t nil "-e" script)
                   (buffer-string))))
    (when (and result (string-match snooze-label result))
      ;; Re-queue a *pure data* lambda — no buffer context
      (run-at-time (timer-duration snooze-str) nil
                   (lambda ()
                     (send-macos-alert-with-snooze
                      title msg icon snooze-str))))))

(defun send-macos-alert-no-snooze (title msg icon)
  "Show a macOS alert dialog with ICON and OK button only."
  (let ((script (format
                 "display dialog %S with title %S with icon POSIX file %S buttons {\"OK\"} default button \"OK\""
                 msg title icon)))
    (call-process "osascript" nil 0 nil "-e" script)))

;; ---- Priority → emoji + icon mapping ----

(defun org-priority->icon (priority)
  "Return (emoji . icns-path) for PRIORITY (\"A\"/\"B\"/\"C\"/nil)."
  (pcase priority
    ("A" (cons "⚡" "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/AlertStopIcon.icns"))
    ("B" (cons "🛠" "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/ToolbarCustomizeIcon.icns"))
    ("C" (cons "🎈" "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/Clock.icns"))
    (_   (cons ""   org-alert-default-icon))))

;; ---- Timer registry (so we can clear/replace safely) ----

(defvar org-alert-timers (make-hash-table :test 'equal)
  "Map of org reminder keys → list of active timer objects.")

(defun clear-org-alert-timers ()
  "Cancel and clear all timers created for Org reminders."
  (maphash (lambda (_key timers)
             (dolist (tm timers) (when (timerp tm) (cancel-timer tm))))
           org-alert-timers)
  (clrhash org-alert-timers))

(defun register-org-alert-timer (key timer)
  "Associate TIMER with KEY in `org-alert-timers`, replacing any existing ones."
  (when-let ((existing (gethash key org-alert-timers)))
    (dolist (tm existing) (when (timerp tm) (cancel-timer tm))))
  (puthash key (list timer) org-alert-timers))

;; ---- Named dispatcher (shows up in `list-timers`) ----

(defun org-alert-dispatch (title msg &optional icon snooze-str)
  "Run a macOS alert with TITLE, MSG, ICON and SNOOZE-STR."
  (send-macos-alert-with-snooze title msg icon snooze-str))

;; ---- Main scheduler ----

;; -*- lexical-binding: t -*-

(require 'subr-x)

(defun parse-future-clocktime (time-str)
  "Return an absolute Emacs time for TIME-STR.
TIME-STR may be \"+N sec/min\" (relative) or \"HH:MM\" (24h clock).
If \"HH:MM\" has already passed today, schedule for tomorrow."
  (if (string-match-p "^\\s-*\\+" time-str)
      ;; relative (+N sec/min)
      (timer-relative-time (current-time) (timer-duration time-str))
    ;; absolute HH:MM
    (let* ((now (current-time))
           (tm  (decode-time now))
           (hh  (string-to-number (substring time-str 0 2)))
           (mm  (string-to-number (substring time-str 3 5)))
           (target (encode-time 0 mm hh
                                (nth 3 tm) (nth 4 tm) (nth 5 tm))))
      (if (time-less-p now target)
          target
        (time-add target (days-to-time 1))))))

(defun schedule-macos-alert ()
  "Prompt for a message/time (+ optional snooze) and schedule a macOS alert.
Title shows the actual firing time."
  (interactive)
  (let* ((msg       (read-string "Alert message: "))
         (time-str  (read-string "Time (HH:MM or +N sec/min): "))
         (abs-time  (parse-future-clocktime time-str))
         (title     (format-time-string "%H:%M:%S" abs-time))
         (snooze?   (y-or-n-p "Enable snooze? "))
         (snooze    (and snooze? (read-string "Snooze interval (e.g. +5 min, +2 sec): " "+5 min")))
         (icon "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/ToolbarInfo.icns"))
    (if snooze?
        (run-at-time abs-time nil #'send-macos-alert-with-snooze title msg icon snooze)
      (run-at-time abs-time nil #'send-macos-alert-no-snooze   title msg icon))
    (message "\"%s\" timer set for %s%s"
             msg title (if snooze? (format " (snooze %s)" snooze) ""))))

(defun ingest-org-schedules ()
  "Rebuild all Org reminder timers."
  (interactive)
  (clear-org-alert-timers)
  (schedule-org-alerts))

(add-hook 'emacs-startup-hook #'ingest-org-schedules)
(run-at-time "08:00" 86400 #'ingest-org-schedules)

(defun ingest-on-agenda-save ()
  (when-let* ((fname (and buffer-file-name (expand-file-name buffer-file-name)))
              (ag (mapcar #'expand-file-name (org-agenda-files))))
    (when (member fname ag)
      (ingest-org-schedules))))
(add-hook 'after-save-hook #'ingest-on-agenda-save)

(defun schedule-org-alerts ()
  "Schedule macOS alerts for SCHEDULED Org items in `org-agenda-files`,
limited to TODO states: \"TODO\" and \"IN PROGRESS\".
Existing timers for the same (file+heading+timestamp) are replaced."
  (interactive)
  (require 'org)
  (require 'org-element)
  (dolist (file (org-agenda-files))
    (with-current-buffer (find-file-noselect file)
      (let ((ast (org-element-parse-buffer)))
        (org-element-map ast 'headline
          (lambda (h)
            (let* ((todo   (org-element-property :todo-keyword h))
                   (title  (org-element-property :raw-value h))
                   (sched  (org-element-property :scheduled h))
                   (prio   (org-element-property :priority h)))
              (when (and todo
                         (member todo '("TODO" "IN PROGRESS"))
                         (consp sched))
                (let* ((when-time (org-timestamp-to-time sched))
                       (delay    (- (float-time when-time) (float-time (current-time)))))
                  (when (> delay 0)
                    (let* ((key   (format "%s|%s|%s"
                                          (expand-file-name file)
                                          title
                                          (format-time-string "%F %R" when-time)))
                           (decor  (org-priority->icon (and prio (char-to-string prio))))
                           (emoji  (car decor))
                           (icon   (cdr decor))
                           (shown-title (if (string-empty-p emoji)
                                            (format-time-string "%H:%M" when-time)
                                          (format "%s %s %s" emoji (format-time-string "%H:%M" when-time) emoji)))
                           (snooze-str "+5 min")
                           (timer (run-at-time delay nil
                                               (apply-partially #'send-macos-alert-with-snooze
                                                                shown-title title icon snooze-str))))
                      (register-org-alert-timer key timer))))))))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                     WHICH-KEY RESPECT POSFRAME
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq which-key-posframe-parameters
      '((undecorated . nil)
        (no-accept-focus . t)
        (internal-border-width . 8)
        (left-fringe . 6)
        (right-fringe . 6)))

(setq insert-directory-program "gls"
      dired-use-ls-dired t)
