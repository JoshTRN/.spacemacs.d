(defconst mini-posframe-packages
  '((mini-posframe :location local)))

(defun mini-posframe/init-mini-posframe ()
  (use-package mini-posframe
    :defer nil))   ;; don’t defer, load immediately
