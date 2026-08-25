;; -*- lexical-binding: t; -*-
(setq custom-additional-packages
      '(
        adaptive-wrap
        agent-shell
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
        (helm-posframe :location (recipe :fetcher github :repo "tumashu/helm-posframe"))
        (mini-posframe :location (recipe
                                  :fetcher github
                                  :repo "JoshTRN/mini-posframe"))
        org-super-agenda
        ob-rust
        ob-mermaid
        compat
        org-fancy-priorities
        ox-timeline
        ob-powershell
        prettier-js
        quelpa
        ready-player
        rjsx-mode
        solaire-mode
        tldr
        (whisper :location (recipe
                            :fetcher github
                            :repo "natrys/whisper.el"))
        yasnippet-snippets
        cyberpunk-2019-theme
        ))
