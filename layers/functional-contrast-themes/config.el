;; -*- lexical-binding: t -*-

;;; config.el --- functional-contrast-themes layer configuration file for Spacemacs

(defconst functional-contrast-themes-directory
  (expand-file-name "themes/" (file-name-directory load-file-name))
  "Directory holding this layer's theme files.")

;; Layers load after `spacemacs/load-default-theme' has already run
;; (core-spacemacs.el calls it inside `spacemacs/init'; layers are loaded from
;; ~/.emacs.d/init.el afterwards), so adding the path here is too late for the
;; theme Spacemacs picks at startup -- `dotspacemacs/user-init' handles that.
;; This is the belt-and-braces copy: it keeps the path correct for theme cycling
;; (SPC T n) and for `load-theme' at runtime, and it is idempotent.
(add-to-list 'custom-theme-load-path functional-contrast-themes-directory)
