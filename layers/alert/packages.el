;;; packages.el --- alert layer packages -*- lexical-binding: t; -*-

(defconst alert-packages '((alert :location local))
  "Packages owned by the alert layer.")

(defun alert/init-alert ()
  "Initialize the repository-local cross-platform alert backend."
  (use-package alert
    :defer t
    :commands (alert-dispatch
               alert-priority->icon
               alert-schedule
               alert-now
               alert-now-snooze
               alert-enabled-mode
               alert-enable
               alert-disable
               alert-toggle)))

;;; packages.el ends here
