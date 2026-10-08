;; -*- lexical-binding: t; -*-
(defconst ww2-separator "│" "The separator I'm going to use to draw the lanes")

;;;###autoload
(define-generic-mode ww2-mode
  ()
  ()
  ()
  ()
  (list (lambda ()
          (get-buffer-create "*ww2-buffer*")
          (switch-to-buffer "*ww2-buffer*")
          (insert ww2-separator)))
  "A mode for teams like work-week calendar")


(provide 'ww2-mode)
