;; -*- lexical-binding: t -*-

;;; packages.el --- tree-sitter-queries layer packages file for Spacemacs

;; `tree-sitter-langs' is owned by the built-in `tree-sitter' layer; this layer
;; only hooks onto it, so it declares no packages of its own and uses post-init.

(defconst tree-sitter-queries-packages
  '(tree-sitter-langs)
  "The list of Lisp packages required by the tree-sitter-queries layer.")

(defun tree-sitter-queries/post-init-tree-sitter-langs ()
  (advice-add 'tree-sitter-langs-install-grammars
              :after #'tree-sitter-install-custom-queries))
