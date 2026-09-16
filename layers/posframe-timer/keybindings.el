;;; keybindings.el --- posframe-timer Spacemacs bindings -*- lexical-binding: t; -*-

(spacemacs/declare-prefix "aT" "timer")
(spacemacs/set-leader-keys
  "aTt" #'posframe-timer-start
  "aTs" #'posframe-timer-set
  "aTp" #'posframe-timer-toggle-pause
  "aTS" #'posframe-timer-stop
  "aTc" #'posframe-timer-clear
  "aTi" #'posframe-timer-clock-in
  "aTo" #'posframe-timer-clock-out
  "aTk" #'posframe-timer-clock-cancel
  "aTd" #'posframe-timer-toggle-display
  "aTr" #'posframe-timer-reset-position)

;;; keybindings.el ends here
