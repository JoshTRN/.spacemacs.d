;;; packages.el --- posframe-timer layer packages -*- lexical-binding: t; -*-

(defconst posframe-timer-packages '(posframe (posframe-timer :location local))
  "Packages owned by the posframe-timer layer.")

(defun posframe-timer/post-init-posframe ()
  "Extend `posframe', which draws the floating countdown.
`posframe' is owned by the `spacemacs-visual' layer; this layer
only depends on it, so it must not define an init function."
  nil)

(defun posframe-timer/init-posframe-timer ()
  "Initialize the repository-local countdown timer package."
  (use-package posframe-timer
    :defer t
    :init
    ;; The org-clock mirror must be live the moment a clock can start,
    ;; not wait for a timer command; the package installs its
    ;; org-clock-{in,out,cancel}-hook handlers at load time.
    (with-eval-after-load 'org-clock
      (require 'posframe-timer))
    :commands (posframe-timer-start
               posframe-timer-set
               posframe-timer-pause
               posframe-timer-resume
               posframe-timer-toggle-pause
               posframe-timer-stop
               posframe-timer-clear
               posframe-timer-clock-in
               posframe-timer-clock-out
               posframe-timer-clock-cancel
               posframe-timer-toggle-display
               posframe-timer-reset-position)))

;;; packages.el ends here
