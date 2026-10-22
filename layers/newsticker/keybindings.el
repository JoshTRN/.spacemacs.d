;;; keybindings.el --- newsticker layer keybindings  -*- lexical-binding: t; -*-

(spacemacs/declare-prefix "ar" "rss")
(spacemacs/set-leader-keys "arn" 'newsticker-show-news)

;; evil normal state shadows the whole treeview keymap (RET was
;; evil-ret), so the useful keys are registered with evil directly.
(with-eval-after-load 'newst-treeview
  (evil-define-key* 'normal newsticker-treeview-mode-map
    (kbd "RET") #'newsticker-treeview-tree-do-click
    (kbd "q") #'newsticker-treeview-quit
    (kbd "g r") #'newsticker-treeview-get-news
    (kbd "g R") #'newsticker-get-all-news
    (kbd "u") #'newsticker-treeview-update
    (kbd "J") #'newsticker-treeview-next-feed
    (kbd "K") #'newsticker-treeview-prev-feed
    (kbd "C-j") #'newsticker-treeview-next-item
    (kbd "C-k") #'newsticker-treeview-prev-item
    (kbd "N") #'newsticker-treeview-next-new-or-immortal-item
    (kbd "o") #'newsticker-treeview-mark-item-old
    (kbd "O") #'newsticker-treeview-mark-list-items-old
    (kbd "I") #'newsticker-treeview-toggle-item-immortal
    (kbd "b") #'newsticker-treeview-browse-url
    (kbd "a") #'newsticker-add-url)
  (evil-define-key* 'normal newsticker-treeview-list-mode-map
    (kbd "RET") #'newsticker-treeview-show-item)
  (evil-define-key* 'normal newsticker-treeview-item-mode-map
    (kbd "RET") #'newsticker-treeview-scroll-item))
