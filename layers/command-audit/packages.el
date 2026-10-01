;;; packages.el --- command-audit layer packages -*- lexical-binding: t; -*-

(defconst command-audit-packages '((command-audit :location local))
  "Packages owned by the command-audit layer.")

(defun command-audit/init-command-audit ()
  "Initialize the repository-local command usage tally.
Loaded eagerly: the recorder rides `post-command-hook', so it has
to be live from startup to count every interactive invocation."
  (use-package command-audit))

;;; packages.el ends here
