;; -*- lexical-binding: t -*-

;;; config.el --- tree-sitter-queries layer configuration file for Spacemacs

(defvar tree-sitter-custom-query-dir
  (expand-file-name "queries/" (file-name-directory load-file-name))
  "Directory of this layer's tree-sitter queries, one subdirectory per language.")

(defvar tree-sitter-custom-query-langs '("java")
  "Languages whose highlights.scm is taken from `tree-sitter-custom-query-dir'.

Only java is overridden: elm was merged upstream, and the haskell queries no
longer apply cleanly to upstream's grammar.  Both are still kept in queries/
for reference -- add the name here to start overriding one again.")
