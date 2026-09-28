;; -*- lexical-binding: t -*-

;;; packages.el --- pdf-caret layer packages file for Spacemacs

(defconst pdf-caret-packages
  '((pdf-caret :location local))
  "The list of Lisp packages required by the pdf-caret layer.")

(defun pdf-caret/init-pdf-caret ()
  (use-package pdf-caret
    :defer t
    :commands (pdf-caret-start pdf-caret-select-char pdf-caret-select-line
               pdf-caret-mode pdf-caret-setup-evil-keys)
    :init
    ;; Buffer-local evil bindings: evilified-state stamps its own v/V
    ;; (visual state) into the mode map's evilified keymap when the first
    ;; pdf buffer opens, so global/aux bindings get clobbered.
    (add-hook 'pdf-view-mode-hook #'pdf-caret-setup-evil-keys)))
