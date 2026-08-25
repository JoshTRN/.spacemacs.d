;; -*- lexical-binding: t -*-

;;; funcs.el --- tree-sitter-queries layer functions file for Spacemacs

(defun tree-sitter-install-custom-queries (&rest _)
  "Copy this layer's highlights.scm files over the ones tree-sitter-langs ships.

`tree-sitter-langs-install-grammars' unpacks the upstream bundle over the
package's own queries/ directory, so any local edit is lost on every grammar
update.  This runs automatically afterwards.

Also useful interactively after the tree-sitter-langs package itself is
updated, which installs a whole new versioned directory without ever calling
`tree-sitter-langs-install-grammars'."
  (interactive)
  (require 'tree-sitter-langs-build)
  (dolist (lang tree-sitter-custom-query-langs)
    (let ((src (expand-file-name (concat lang "/highlights.scm")
                                 tree-sitter-custom-query-dir))
          (dst-dir (file-name-as-directory
                    (expand-file-name lang tree-sitter-langs--queries-dir))))
      (if (not (file-exists-p src))
          (message "tree-sitter: no custom query for %s at %s" lang src)
        (make-directory dst-dir t)
        (copy-file src (expand-file-name "highlights.scm" dst-dir) :ok-if-exists)
        (message "tree-sitter: installed custom highlights.scm for %s" lang)))))
