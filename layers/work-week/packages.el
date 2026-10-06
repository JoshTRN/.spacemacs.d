;;; packages.el --- work-week layer packages -*- lexical-binding: t; -*-

(defconst work-week-packages '((work-week :location local))
  "Packages owned by the work-week layer.")

(defun work-week/init-work-week ()
  "Initialize the repository-local work-week calendar package."
  (use-package work-week
    :defer t
    :commands (work-week)
    :config
    (spacemacs/set-leader-keys-for-minor-mode 'work-week-capture-minor-mode
      "," #'work-week-capture-finish
      "k" #'work-week-capture-cancel)))

;;; packages.el ends here
