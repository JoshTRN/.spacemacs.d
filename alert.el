;;; alert.el --- Cross-platform alert backend -*- lexical-binding: t; -*-
;;
;; Provides a unified API for sending alerts across OSes.
;; Consumers should only use `alert-dispatch` and `alert-priority->icon`.
;;
;; Example:
;;   (alert-dispatch "Break" "Time to stretch!" nil "+10 min")
;;

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                                VARS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar alert-default-icon nil
  "Default icon path or spec for alerts on this OS.")

(defvar alert-priority-icons nil
  "Alist mapping priorities (\"A\"/\"B\"/\"C\"/nil) to (emoji . icon).")

(defvar alert-backend-snooze-fn nil
  "Function (TITLE MSG ICON SNOOZE) to show alert with snooze.")

(defvar alert-backend-nosnooze-fn nil
  "Function (TITLE MSG ICON) to show alert without snooze.")

(defvar alert-enabled t
  "Non-nil means alerts are enabled. Set to nil to silence all alerts.")

(defvar alert-default-snooze "+5 min"
  "Default snooze interval used by `alert-now-snooze' and `alert-schedule'
when no explicit snooze is provided.")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                             MINOR MODE
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;;###autoload
(define-minor-mode alert-enabled-mode
  "Global minor mode to enable or disable alerts.
When enabled, `alert-dispatch` will send notifications.
When disabled, all alerts are suppressed."
  :global t
  :lighter " Alert"
  (setq alert-enabled alert-enabled-mode))

(defun alert-disable ()
  "Disable alerts globally."
  (interactive)
  (alert-enabled-mode -1))

(defun alert-enable ()
  "Enable alerts globally."
  (interactive)
  (alert-enabled-mode 1))

(defun alert-toggle ()
  "Toggle alerts globally."
  (interactive)
  (if alert-enabled
      (alert-disable)
    (alert-enable)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                                API
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun alert-priority->icon (priority)
  "Return (emoji . icon) for PRIORITY."
  (or (cdr (assoc priority alert-priority-icons))
      (cdr (assoc nil alert-priority-icons))))

(defun alert-dispatch (title msg &optional icon snooze-str)
  "Send an alert with TITLE and MSG.
ICON overrides the default. SNOOZE-STR reschedules if non-nil.
Respects `alert-enabled`."
  (when alert-enabled
    (if snooze-str
        (funcall alert-backend-snooze-fn title msg (or icon alert-default-icon) snooze-str)
      (funcall alert-backend-nosnooze-fn title msg (or icon alert-default-icon)))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           MAC-OS BACKEND
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(when (eq system-type 'darwin)
  (setq alert-default-icon
        "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/ToolbarInfo.icns")

  (setq alert-priority-icons
        '(("A" . ("⚡" . "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/AlertStopIcon.icns"))
          ("B" . ("🛠" . "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/ToolbarCustomizeIcon.icns"))
          ("C" . ("🎈" . "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/Clock.icns"))
          (nil . (""   . "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/ToolbarInfo.icns"))))

  (setq alert-backend-snooze-fn
        (lambda (title msg icon snooze-str)
          (let* ((snooze-label (format "Snooze %s" snooze-str))
                 (script (format
                          "display dialog %S with title %S with icon POSIX file %S buttons {\"%s\",\"OK\"} default button \"OK\""
                          msg title icon snooze-label))
                 (result (with-temp-buffer
                           (call-process "osascript" nil t nil "-e" script)
                           (buffer-string))))
            (when (and result (string-match snooze-label result))
              (run-at-time (timer-duration snooze-str) nil
                           (lambda ()
                             (funcall alert-backend-snooze-fn title msg icon snooze-str)))))))

  (setq alert-backend-nosnooze-fn
        (lambda (title msg icon)
          (let ((script (format
                         "display dialog %S with title %S with icon POSIX file %S buttons {\"OK\"} default button \"OK\""
                         msg title icon)))
            (call-process "osascript" nil 0 nil "-e" script)))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           LINUX BACKEND
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(when (eq system-type 'gnu/linux)
  (setq alert-default-icon "dialog-information")

  (setq alert-priority-icons
        '(("A" . ("⚡" . "dialog-error"))
          ("B" . ("🛠" . "applications-system"))
          ("C" . ("🎈" . "alarm"))
          (nil . (""   . "dialog-information"))))

  (setq alert-backend-snooze-fn
        (lambda (title msg icon snooze-str)
          (apply #'call-process "notify-send" nil 0 nil
                 (delq nil (list "-i" icon title msg)))
          (run-at-time (timer-duration snooze-str) nil
                       (lambda ()
                         (funcall alert-backend-snooze-fn title msg icon snooze-str)))))

  (setq alert-backend-nosnooze-fn
        (lambda (title msg icon)
          (apply #'call-process "notify-send" nil 0 nil
                 (delq nil (list "-i" icon title msg))))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                      FUTURE: WINDOWS BACKEND
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Could be added here with PowerShell `New-BurntToastNotification`
;; or `msg.exe`.

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                             MAIN LOGIC
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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

(defun alert-schedule ()
  "Prompt for a message/time (+ optional snooze) and schedule an alert.
Title shows the actual firing time. Uses `alert-dispatch` backend.
If `alert-enabled-mode` is disabled, no timer will be scheduled."
  (interactive)
  (if (not alert-enabled-mode)
      (message "Alerts are currently DISABLED. Run `M-x alert-enable` or `M-x alert-enabled-mode` to enable them.")
    (let* ((msg       (read-string "Alert message: "))
           (time-str  (read-string "Time (HH:MM or +N sec/min): "))
           (abs-time  (parse-future-clocktime time-str))
           (title     (format-time-string "%H:%M:%S" abs-time))
           (snooze?   (y-or-n-p "Enable snooze? "))
           (snooze    (and snooze?
                           (read-string (format "Snooze interval (default %s): " alert-default-snooze)
                                        alert-default-snooze))))
      (if snooze?
          (run-at-time abs-time nil #'alert-dispatch title msg nil snooze)
        (run-at-time abs-time nil #'alert-dispatch title msg nil))
      (message "\"%s\" timer set for %s%s"
               msg title (if snooze? (format " (snooze %s)" snooze) "")))))

(defun alert-now (title msg &optional icon)
  "Fire an alert immediately with TITLE and MSG.
If called interactively, prompts for TITLE and MSG.
Uses `alert-dispatch` backend without snooze."
  (interactive
   (list (read-string "Alert title: ")
         (read-string "Alert message: ")))
  (if (not alert-enabled-mode)
      (message "Alerts are currently DISABLED. Run `M-x alert-enable` to enable them.")
    (alert-dispatch title msg (or icon alert-default-icon))))

(defun alert-now-snooze (title msg &optional icon snooze-str)
  "Fire an alert immediately with TITLE and MSG, including a snooze option.
If SNOOZE-STR is nil, use `alert-default-snooze'.
If called interactively, prompts for TITLE and MSG."
  (interactive
   (list (read-string "Alert title: ")
         (read-string "Alert message: ")))
  (if (not alert-enabled-mode)
      (message "Alerts are currently DISABLED. Run `M-x alert-enable` to enable them.")
    (let ((snooze (or snooze-str alert-default-snooze)))
      (alert-dispatch title msg (or icon alert-default-icon) snooze))))

;;; alert.el ends here
