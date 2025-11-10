(setq
 custom-layers
 '(
   (auto-completion :variables auto-completion-enable-snippets-in-popup t)
   better-defaults
   clojure
   csv
   dap
   debug
   docker
   (elm :variables
        elm-format-on-save t
        elm-sort-imports-on-save t)
   emacs-lisp
   emoji
   eww
   (git :variables git-enable-magit-delta-plugin t)
   (go :variables
       go-backend 'lsp
       go-tab-width 2)
   (groovy :variables
           groovy-backend 'lsp
           groovy-lsp-jar-path "~/code/groovy-language-server/build/libs/groovy-language-server-all.jar")
   haskell
   (helm :variables helm-position 'top helm-use-posframe t)
   html
   java
   (javascript :variables js2-mode-show-strict-warnings nil)
   kotlin
   kubernetes
   llm-client
   (lsp :variables lsp-lens-enable t)
   lua
   markdown
   markdown-indent
   multiple-cursors
   nginx
   nixos
   (node :variables node-add-modules-path t)
   (org :variables
        org-enable-appear-support t
        org-enable-epub-support t
        org-enable-modern-support t
        org-enable-org-journal-support t
        org-enable-roam-protocol t
        org-enable-roam-support t
        org-enable-roam-ui t
        org-enable-transclusion-support t
        org-enable-valign t
        org-enable-verb-support t)
   pandoc
   (pdf :variables
        pdf-tools t)
   prettier
   (python :variables python-backend 'lsp python-lsp-server 'pyright)
   rust
   scala
   search-engine
   shell-scripts
   (shell :variables
          shell-default-height 30
          shell-default-position 'full
          shell-default-shell 'multi-vterm)
   spell-checking
   sql
   syntax-checking
   systemd
   terraform
   themes-megapack
   treemacs
   ruby
   (tree-sitter :variables
                tree-sitter-langs '(groovy python javascript))
   (typescript :variables
               javascript-backend 'lsp
               typescript-backend 'lsp
               typescript-fmt-on-save t
               typescript-fmt-tool 'lsp
               typescript-linter 'tslint
               typescript-tsx-mode t)
   (unicode-fonts :variables unicode-fonts-enable-ligatures t)
   (version-control :variables
                    version-control-diff-side 'left
                    version-control-global-margin t)
   vimscript
   (vue :variables vue-backend 'lsp)
   windows-scripts
   yaml
   ))
