(setq custom-additional-packages
      '(
        gradle-mode
        direnv
        exec-path-from-shell
        helm-posframe
        indium
        js-comint
        magit-delta
        ob-http
        (ob-kubectl :location (recipe
                               :fetcher github
                               :repo "ifitzpat/ob-kubectl"))
        (org-modern-indent :location (recipe
                                      :fetcher github
                                      :repo "jdtsmith/org-modern-indent"))
        (lsp-bridge :location (recipe
                               :fetcher github
                               :repo "manateelazycat/lsp-bridge"))
        (helm-posframe :location (recipe :fetcher github :repo "tumashu/helm-posframe"))
        ob-rust
        compat
        org-fancy-priorities
        calfw
        calfw-org
        ox-timeline
        ob-powershell
        prettier-js
        quelpa
        rjsx-mode
        solaire-mode
        tldr
        yasnippet-snippets
        cyberpunk-2019-theme
        ))
