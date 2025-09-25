;;; packages.el --- markdown-indent layer packages file for Spacemacs

(defconst markdown-indent-packages
  '((markdown-indent :location local))
  "The list of Lisp packages required by the markdown-indent layer.")

(defun markdown-indent/init-markdown-indent ()
  (use-package markdown-indent
    :defer t
    :commands (markdown-indent-mode)
    :init
    ;; Automatically enable for markdown buffers
    (add-hook 'markdown-mode-hook 'markdown-indent-mode)))
