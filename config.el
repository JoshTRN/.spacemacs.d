;;; init.el --- Spacemacs user configuration -*- lexical-binding: t; -*-
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           MISC-FUNCTIONS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun host-config-file ()
  (interactive)
  (unless (file-exists-p host-config-file)
    ;; create empty file
    (with-temp-buffer
      (write-file host-config-file)))
  (find-file host-config-file))

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
  this way compose-region called by prettify-symbols-mode
  will use the correct width of the symbols
  instead of the width measured by char-width." ;
  (mapcar #'(lambda (el)
              (setcdr el (string ?\t (cdr el)))
              el)
          pretty-alist))

(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) #'(lambda (prompt) t)))
    (apply orig-fun args)))

(defun my-setup-indent (n)
  (setq javascript-indent-level n)
  (setq typescript-indent-level n)
  (setq js-indent-level n)
  (setq c-basic-offset n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n)
  (setq groovy-indent-offset n))

(defun pretty-print ()
  (interactive)
  (hl-line-mode -1)
  (visual-fill-column-mode 1)
  (message "Pretty Printing Buffer")
  (visual-line-mode 1))

(defun pretty-print-header ()
  (interactive)
  (pretty-print)
  (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)))
  (setq header-line-format "")
  )

(defun insert-comment-heading (title)
  "Insert a centered comment heading with TITLE (forced uppercase)."
  (interactive "sHeading: ")
  (let* ((title (upcase title))
         (width 68)
         (border (make-string width ?━)) ;; heavy line ━ (U+2501)
         (padding (max 0 (/ (- width (length title)) 2)))
         (line (concat ";;" (make-string padding ?\s) title)))
    (insert ";; " border "\n")
    (insert line "\n")
    (insert ";; " border "\n")))

(defun insert-bash-comment-heading (title)
  "Insert a centered bash-style comment heading with TITLE (forced uppercase)."
  (interactive "sHeading: ")
  (let* ((title (upcase title))
         (width 68)
         (border (make-string width ?━))
         (padding (max 0 (/ (- width (length title)) 2)))
         (line (concat (make-string padding ?\s) title)))
    (insert "echo -e \"\\n" border "\"\n")
    (insert "echo \"" line "\"\n")
    (insert "echo -e \"" border "\\n\"\n")))

(defun finder (&optional path)
  "Open PATH (or current directory) in macOS Finder and bring Finder to front."
  (interactive)
  (let ((target (expand-file-name (or path default-directory))))
    (shell-command
     (format "osascript -e 'tell application \"Finder\" to activate' \
-e 'tell application \"Finder\" to open (POSIX file \"%s\") as alias'"
             target))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           MISC-SETTINGS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun insert-java-log-heading (title)
  "Insert a formatted Java log.info heading with TITLE (forced uppercase)."
  (interactive "sHeading: ")
  (let* ((width 68)
         (border (make-string width ?━)) ;; heavy line ━ (U+2501)
         (padding (max 0 (/ (- width (length title)) 2)))
         (spaced-title (concat (make-string padding ?\s) title))
         (border-line (concat "IQNOX: " border))
         (title-line (concat "IQNOX:" spaced-title)))
    (insert "log.info(")
    (insert "\"\"\"\n")
    (insert border-line "\n")
    (insert title-line "\n")
    (insert border-line "\n")
    (insert "\"\"\");\n")))

(setq-default
 fill-column 110
 highlight-indent-guides-character 9615
 visual-fill-column-center-text t
 visual-fill-column-width 130
 valign-fancy-bar t
 )

(setq
 git-gutter-fr:side 'left-fringe
 helm-move-to-line-cycle-in-source nil
 helm-swoop-speed-or-color t
 jit-lock-chunk-size 5000
 tab-width 2
 tree-sitter-hl-use-font-lock-keywords nil
 treemacs-position 'right
 treemacs-width 50
 vterm-max-scrollback 100000
 vterm-timer-delay 0.01
 )

(spacemacs/declare-prefix "oi" "Insert")
(spacemacs/set-leader-keys "oh" #'host-config-file)
(spacemacs/set-leader-keys "oih" #'insert-comment-heading)
(spacemacs/set-leader-keys "oie" #'emoji-search)
(spacemacs/set-leader-keys "oij" #'insert-java-log-heading)
(spacemacs/set-leader-keys "oF" #'finder)



;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           ORG-FUNCTIONS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(defun org-modern-bullet-coloring ()
  "Color list bullets (+ - *) with a uniform muted purple."
  (font-lock-add-keywords
   nil
   '(("^[ \t]*\\([+*-]\\)\\s-" 1 'my/org-bullet prepend))
   'append))

(defun my-org-mode-hook ()
  (org-modern-indent-mode)
  (set-face-attribute 'org-document-title nil :height 1.6 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.05)
  (set-face-attribute 'org-block nil :family "Monospace")
  (pretty-print-header)
  (org-fancy-priorities-mode)
  (org-indent-mode t)
  (setq global-hl-line-mode nil)
  (solaire-mode -1)
  )

(defun org-babel-copy-src-body ()
  "Copy only the body of the Org Babel source block at point, excluding the BEGIN/END lines.
Restores point to the original cursor position after copying."
  (interactive)
  (when (org-in-src-block-p)
    (let ((original-point (point)))
      (save-excursion
        (let* ((element (org-element-at-point))
               (body-begin (org-element-property :begin element))
               (body-end (org-element-property :end element)))
          (goto-char body-begin)
          (re-search-forward "^[ \t]*#\\+BEGIN_SRC[^\n]*\n" nil t)
          (let ((start (point)))
            (goto-char body-end)
            (re-search-backward "^[ \t]*#\\+END_SRC" nil t)
            (kill-ring-save start (point)))))
      (goto-char original-point)
      (message "Source block body copied."))))

(defun org-show-link-in-minibuffer ()
  "Display the actual Org hyperlink in the minibuffer when hovering over it.
Only runs in Org Mode buffers."
  (when (derived-mode-p 'org-mode) ;; Ensure it's an Org buffer
    (let ((context (org-element-context)))
      (when (eq (org-element-type context) 'link)
        (let ((raw-link (org-element-property :raw-link context)))
          (when raw-link
            (message "%s" (org-link-unescape raw-link))))))))

;; Enable it dynamically, but only in Org Mode buffers
(defun org-enable-hover-links ()
  "Enable displaying Org links in the minibuffer only in Org buffers."
  (when (derived-mode-p 'org-mode)
    (add-hook 'post-command-hook #'org-show-link-in-minibuffer nil t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa      . t)
   (haskell    . t)
   (http       . t)
   (java       . t)
   (js         . t)
   (latex      . t)
   (mermaid    . t)
   (powershell . t)
   (python     . t)
   (shell      . t)
   (sql        . t)
   ))

(defun my-org-capture-reset-header-line ()
  "In `org-capture-mode` buffers, keep the default capture header line
but reset any face remapping applied elsewhere."
  (setq-local face-remapping-alist (assq-delete-all 'header-line face-remapping-alist)))

(defun org-babel-execute:typescript (body params)
  (let ((org-babel-js-cmd "npx ts-node < "))
    (org-babel-execute:js body params)))

(defalias 'org-babel-execute:ts 'org-babel-execute:typescript)

(org-fancy-priorities-mode 1)

(defun org-inline-code-pad ()
  "Add left/right padding to inline ~code~ and =verbatim=."
  (remove-overlays (point-min) (point-max) 'my-org-inline-pad t)
  (save-excursion
    (goto-char (point-min))
    ;; Match ~code~ or =verbatim= (doesn't cross newlines)
    (while (re-search-forward "[=~][^=~\n]+?[=~]" nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (ov  (make-overlay beg end)))
        (overlay-put ov 'my-org-inline-pad t)
        (overlay-put ov 'face 'org-verbatim) ; keep your face
        ;; add 1 space “padding” on each side, colored like org-verbatim

        (overlay-put ov 'before-string
                     (propertize " " 'face '(variable-pitch org-verbatim)))
        (overlay-put ov 'after-string
                     (propertize " " 'face '(variable-pitch org-verbatim)))
        ))))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                              ORG-VARS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq
 org-confirm-babel-evaluate nil
 org-modern-checkbox '((?X . "✅") (?- . "⏺️") (?\s . "⬜"))
 org-modern-list '((?+ . "●") (?- . "⪧") (?* . "•"))
 org-ellipsis " ⤵"
 org-fold-core-style 'text-properties
 org-hide-emphasis-markers t
 org-modern-fold-stars '(("" . "") ("" . "") ("" . "") ("" . "") ("" . ""))
 org-modern-hide-stars t
 org-modern-star 'fold
 org-pandoc-options '((wrap . "none") (metadata . "nil") (standalone . "false"))
 org-roam-directory "~/org-roam"
 org-src-tab-acts-natively  nil
 org-use-sub-superscripts '{}
 )

(defface my/org-bullet
  '((t :foreground "#5FAFFF"))
  "Face for all org-modern list bullets.")

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                             ORG-HOOKS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(with-eval-after-load 'org
  (ligature-set-ligatures 'org-mode '("->" "=>" "<=" ">=" "==" "!=" "<-")))

(add-hook 'org-mode-hook #'org-enable-hover-links)
(add-hook 'org-mode-hook #'org-modern-bullet-coloring)
(add-hook 'org-capture-mode-hook #'my-org-capture-reset-header-line)
(add-hook 'org-mode-hook #'my-org-mode-hook)
(add-hook 'org-modern-mode-hook (lambda () (font-lock-flush) (font-lock-ensure)))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-c b") 'org-babel-tangle-block)))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               #'(lambda ()
                   (org-display-inline-images)
                   (evil-define-key 'normal org-present-mode-keymap
                     (kbd "<left>")  'org-present-prev
                     (kbd "<right>") 'org-present-next
                     "q"             'org-present-quit)
                   (text-scale-set 2)
                   (setq visual-fill-column-width 50)
                   (funcall #'(lambda ()
                                (message "setting org shift tab")
                                (org-shifttab)
                                ))
                   ))
     (add-hook 'org-present-mode-quit-hook
               #'(lambda ()
                   (org-remove-inline-images)
                   (text-scale-set 1)
                   (setq visual-fill-column-width 100)
                   ))))

(add-hook 'org-mode-hook
          (lambda ()
            (org-inline-code-pad)
            ;; refresh padding when editing
            (add-hook 'after-change-functions
                      (lambda (&rest _) (when (derived-mode-p 'org-mode)
                                          (org-inline-code-pad)))
                      nil t)))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                            ORG-BINDINGS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(spacemacs/set-leader-keys-for-major-mode 'org-mode "bC" 'org-babel-copy-src-body)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                             ORG-AGENDA
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq
 org-agenda-block-separator ?─
 org-agenda-custom-commands '(("v" "A better agenda view"
                               ((tags "PRIORITY=\"A\""
                                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                       (org-agenda-overriding-header "High-priority unfinished tasks:")))
                                (tags "PRIORITY=\"B\""
                                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                       (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
                                (tags "PRIORITY=\"C\""
                                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                       (org-agenda-overriding-header "Low-priority unfinished tasks:")))

                                (agenda "")
                                (alltodo ""))))
 org-agenda-time-grid '((daily today require-timed)
                        (800 1000 1200 1400 1600 1800 2000)
                        ;; now line: leading space, no trailing
                        ""
                        ;; grid line: no leading, trailing space
                        ""
                        ))

(custom-set-faces '(org-agenda-block-separator
                    ((t (:foreground "#3A4A5A"))))) ;; pick a subtle gray/blue

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                             EVIL-MODE
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun evil-jump-backward-centered (&optional count)
  (interactive)
  (evil-jump-backward count)
  (evil-scroll-line-to-center count))

(defun evil-search-next-centered (&optional count)
  (interactive)
  (evil-ex-search-next)
  (evil-scroll-line-to-center count))

(defun evil-search-previous-centered (&optional count)
  (interactive)
  (evil-ex-search-previous)
  (evil-scroll-line-to-center count))

(defun evil-jump-forward-centered (&optional count)
  (interactive)
  (evil-jump-forward count)
  (evil-scroll-line-to-center count))

(defun evil-scroll-down-centered (&optional count)
  (interactive)
  (evil-scroll-down count)
  (evil-scroll-line-to-center count))

(defun evil-scroll-up-centered (&optional count)
  (interactive)
  (evil-scroll-up count)
  (evil-scroll-line-to-center count))

(with-eval-after-load 'evil
  (evil-add-command-properties 'evil-search-next :repeat 'abort)
  (evil-add-command-properties 'evil-search-previous :repeat 'abort))

(setq evil-emacs-state-cursor '("SeaGreen4" box)
      evil-normal-state-cursor '("SeaGreen4" box)
      evil-operator-state-cursor '("red" hollow)
      evil-replace-state-cursor '("red" bar)
      evil-visual-state-cursor '("cyan" box))

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-ex-search-next>") 'evil-search-next-centered )
(define-key evil-motion-state-map (kbd "<remap> <evil-ex-search-previous>") 'evil-search-previous-centered)
(define-key evil-motion-state-map (kbd "<remap> <evil-jump-backward>") 'evil-jump-backward-centered)
(define-key evil-motion-state-map (kbd "<remap> <evil-jump-forward>") 'evil-jump-forward-centered)
(define-key evil-motion-state-map (kbd "<remap> <evil-scroll-down>") 'evil-scroll-down-centered)
(define-key evil-motion-state-map (kbd "<remap> <evil-scroll-up>") 'evil-scroll-up-centered)

(defun my-yaml-hook ()
  (spacemacs/toggle-absolute-line-numbers-on)
  (highlight-indentation-mode 1))

(defun my-markdown-mode-hook ()
  (pretty-print-header)
  (setq global-hl-line-mode nil)
  (markdown-toggle-markup-hiding 1)
  (markdown-indent-mode 1)
  (solaire-mode -1))

(defun kube-hook ()
  (pretty-print-header)
  (solaire-mode -1))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                          SPACEMACS REMAPS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(spacemacs/set-leader-keys-for-major-mode 'elm-mode "fr" 'lsp-ui-peek-find-references)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "ug" 'lsp-ui-doc-glance)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "uf" 'lsp-ui-doc-focus-frame)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "us" 'lsp-ui-doc-show)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "uh" 'lsp-ui-doc-hide)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "fr" 'lsp-ui-peek-find-references)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "ug" 'lsp-ui-doc-glance)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "uf" 'lsp-ui-doc-focus-frame)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "us" 'lsp-ui-doc-show)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "uh" 'lsp-ui-doc-hide)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "al" 'lsp-avy-lens)

(spacemacs/set-leader-keys-for-minor-mode 'gptel-mode "RET" 'gptel-send)

(spacemacs/set-leader-keys "gn" 'diff-hl-next-hunk)
(spacemacs/set-leader-keys "gp" 'diff-hl-previous-hunk)

(spacemacs/set-leader-keys "Fp" 'posframe-delete-all)

(add-to-list 'auto-mode-alist '("\\.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))
(add-to-list 'custom-theme-load-path "~/code/emacs-configurations/themes")

(require 'dap-node)
(require 'dap-chrome)
(require 'dap-firefox)

(remove-hook 'org-present-mode-hook 'spacemacs//org-present-start)

(add-hook 'markdown-mode-hook #'my-markdown-mode-hook)
(add-hook 'help-mode-hook #'pretty-print)
(add-hook 'lsp-help-mode-hook #'pretty-print)
(add-hook 'yaml-mode-hook #'my-yaml-hook)
(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)
(add-hook 'elm-mode-hook #'pretty-lambdas-haskell)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'eww-mode-hook #'pretty-print)
(add-hook 'kubernetes-overview-mode-hook 'kube-hook)
(add-hook 'js-mode-hook #'(lambda () (buffer-face-set :foreground "white")))
(add-hook 'org-modern-mode-hook #'(lambda () (variable-pitch-mode t)))

(define-derived-mode ts-mode typescript-mode "ts"
  "Major mode for editing ts code blocks.")

(my-setup-indent 2)
(helm-ff-icon-mode)
(spacemacs/toggle-vi-tilde-fringe-off)
(solaire-global-mode +1)
(pixel-scroll-precision-mode)
(make-variable-buffer-local 'global-hl-line-mode)
(advice-add 'ediff-quit :around #'disable-y-or-n-p)
(display-time-mode t)
(helm-posframe-enable)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           HELM-POSFRAME
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq helm-posframe-poshandler 'posframe-poshandler-frame-center
      helm-posframe-width      (round (* 0.618 (frame-width)))
      helm-posframe-height     (round (* 0.618 (frame-height)))
      helm-posframe-parameters '((internal-border-width . 2)
                                 (left-fringe           . 4)
                                 (right-fringe          . 4)
                                 (undecorated           . nil)))
(with-eval-after-load 'helm-swoop
  (defun jw/helm-swoop--use-posframe-when-available (orig &rest args)
    (let* ((posframe-enabled (and (featurep 'helm-posframe)
                                  (boundp 'helm-display-function)
                                  (not (eq helm-display-function #'helm-default-display-buffer))))
           ;; helm-swoop rebinds `helm-display-function' to `helm-swoop-split-window-function'.
           ;; Feed it the current Helm display function (posframe) when enabled.
           (helm-swoop-split-window-function
            (if posframe-enabled
                helm-display-function
              helm-swoop-split-window-function)))
      (apply orig args)))

  (advice-add 'helm-swoop :around #'jw/helm-swoop--use-posframe-when-available)
  (advice-add 'helm-multi-swoop--exec :around #'jw/helm-swoop--use-posframe-when-available))

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           MARKDOWN-MODE
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar markdown-block-type-faces
  '(("[!success]" . 'markdown-success-face)
    ("[!warning]" . 'markdown-warning-face)
    ("[!info]" . 'markdown-info-face)
    ("[!danger]" . 'markdown-danger-face))
  "Alist mapping block types to their corresponding faces in Markdown.")

(defface markdown-success-face
  '((t (:foreground "green" :weight bold)))
  "Face for success blocks in Markdown.")

(defface markdown-warning-face
  '((t (:foreground "orange" :weight bold)))
  "Face for warning blocks in Markdown.")

(defface markdown-info-face
  '((t (:foreground "blue" :weight bold)))
  "Face for info blocks in Markdown.")

(defface markdown-danger-face
  '((t (:foreground "red" :weight bold)))
  "Face for danger blocks in Markdown.")

(defun set-markdown-block-faces ()
  (interactive)
  "Set custom faces for block types in Markdown."
  (font-lock-add-keywords
   nil
   (mapcar (lambda (entry)
             (let ((block-type (car entry))
                   (face (cdr entry)))
               (cons (format "^> \[%s\].*" (regexp-quote block-type))
                     `(0 ',face t))))
           markdown-block-type-faces)))

(add-hook 'markdown-mode-hook 'set-markdown-block-faces)
(markdown-indent-mode 1)


;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                               DIRED
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun dired-hide-details-customization ()
  "Customize `dired-hide-details-mode` to hide timestamps, file sizes, and hard links while preserving permissions, user, and group."
  ;; Use `ls` switches to customize the output
  (setq-local dired-listing-switches "-l --group-directories-first --time-style=long-iso")
  ;; Hide details explicitly (e.g., timestamps and file sizes)
  (setq-local dired-hide-details-hide-symlink-targets nil))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1) ; Enable hide details by default
            (dired-hide-details-customization)))

;; (setq dired-hide-details-preserved-columns '(1 2 3 4))
(setq dired-hide-details-preserved-columns nil)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                               DOCKER
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(with-eval-after-load 'docker-container
  ;; Spacemacs-style major-mode leader bindings
  (spacemacs/set-leader-keys-for-major-mode 'docker-container-mode
    "h" 'docker-container-help
    "t" 'docker-container-vterm-selection
    "r" 'docker-container-restart
    "s" 'docker-container-stop
    "l" 'docker-containers)

  ;; Optional: M-/ as quick help (works under Evil too)
  (with-eval-after-load 'evil
    (evil-define-key 'normal docker-container-mode-map
      (kbd "M-/") 'docker-container-help)))

(setq org-html-checkbox-type 'html)

(setq org-html-checkbox-type 'html)
(setq org-html-checkbox-type 'html)

(setq docker-container-columns
      '((:name "Names" :width 50 :template "{{ json .Names }}" :sort nil :format nil)
        (:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)
        (:name "Image" :width 15 :template "{{ json .Image }}" :sort nil :format nil)
        (:name "Command" :width 30 :template "{{ json .Command }}" :sort nil :format nil)
        (:name "Created" :width 23 :template "{{ json .CreatedAt }}" :sort nil
               :format (lambda (x)
                         (format-time-string "%F %T" (date-to-time x))))
        (:name "Status" :width 20 :template "{{ json .Status }}" :sort nil :format nil)
        (:name "Ports" :width 10 :template "{{ json .Ports }}" :sort nil :format nil)))


(setq vterm-always-compile-module t)

(defun reload-term ()
  "Compile vterm, then reinstall vterm and multi-vterm packages."
  (interactive)
  ;; Step 1: Compile vterm module
  ;; Step 2: Reinstall Lisp packages
  (package-reinstall 'vterm)
  (package-reinstall 'multi-vterm)
  (message "vterm compiled and packages reinstalled."))

;; Run it once after Spacemacs has fully loaded
(spacemacs/set-leader-keys "or" #'reload-term)


(remove-hook 'org-mode-hook 'org-superstar-mode)

(setq org-html-postamble nil)
(setq org-html-with-author nil)
(setq org-html-with-date nil)

(defun fix-shades-of-purple-region-face (theme &rest _)
  "Dull down Evil visual selection highlight when using doom-shades-of-purple."
  (when (eq theme 'doom-shades-of-purple)
    (set-face-attribute 'region nil :background "#3b2f4a" :foreground nil)
    (set-face-attribute 'match nil :background "#35618a" :foreground "#ffffff" :weight 'semi-bold)
    ))

(advice-add 'load-theme :after #'fix-shades-of-purple-region-face)

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           MINI-POSFRAME
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(mini-posframe-mode 1)

(spacemacs/declare-prefix "om" "mini-posframe")

(spacemacs/set-leader-keys
  "omr" #'mini-posframe-resize

  ;; Top row
  "omq" #'mini-posframe-top-left
  "ome" #'mini-posframe-top-center
  "omt" #'mini-posframe-top-right

  ;; Middle row
  "oma" #'mini-posframe-center-left
  "omd" #'mini-posframe-center
  "omg" #'mini-posframe-center-right

  ;; Bottom row
  "omz" #'mini-posframe-bottom-left
  "omc" #'mini-posframe-bottom-center
  "omb" #'mini-posframe-bottom-right
  )

;; --- emacs-lsp-booster integration ---

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Parse LSP booster bytecode if present, otherwise use regular JSON parser."
  (or (when (equal (following-char) ?#)
        (let ((bc (read (current-buffer))))
          (when (byte-code-function-p bc)
            (funcall bc))))
      (apply old-fn args)))

(advice-add (if (fboundp 'json-parse-buffer)
                'json-parse-buffer
              'json-read)
            :around #'lsp-booster--advice-json-parse)


(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Wrap LSP server command with emacs-lsp-booster if available."
  (let ((orig (funcall old-fn cmd test?)))
    (if (and (not test?)
             (executable-find "emacs-lsp-booster"))
        (cons "emacs-lsp-booster" orig)
      orig)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                           PDF VIEW MODE
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun update-pdf-view-midnight-colors ()
  "Set `pdf-view-midnight-colors` to match the current theme background.
If `solaire-default-face` is available, use its background; otherwise use the default face."
  (let* ((default-fg (face-foreground 'default nil t))
         (default-bg (face-background 'default nil t))
         (solaire-bg (when (facep 'solaire-default-face)
                       (face-background 'solaire-default-face nil t)))
         (chosen-bg (or solaire-bg default-bg)))
    (setq pdf-view-midnight-colors (cons default-fg chosen-bg))
    (message "pdf-view-midnight-colors set to: %s" pdf-view-midnight-colors)))

(update-pdf-view-midnight-colors)

(advice-add 'load-theme :after (lambda (&rest _) (update-pdf-view-midnight-colors)))

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (set-window-scroll-bars (selected-window) nil nil)
            (setq vertical-scroll-bar nil)))

(defun disable-large-file-warning-for-pdf (orig-fun filename &rest args)
  "Skip large file warning for PDFs, otherwise use ORIG-FUN normally."
  (let ((large-file-warning-threshold
         (if (string-match-p "\\.pdf\\'" filename)
             nil
           large-file-warning-threshold)))
    (apply orig-fun filename args)))

(advice-add 'find-file-noselect :around #'disable-large-file-warning-for-pdf)

(with-eval-after-load 'org
  (custom-set-faces
   '(org-modern-todo
     ((t (:foreground "#D77070"
                      :weight bold
                      :background "#3A2020"
                      :box (:line-width 1 :color "#D77070")))))
   '(org-modern-todo-faces
     `(("IN PROGRESS"
        :family ,(face-attribute 'default :family nil 'default)
        :height ,(let ((h (face-attribute 'org-modern-todo :height nil 'default)))
                   (if (eq h 'unspecified) 1.0 h))
        :weight ,(let ((w (face-attribute 'org-modern-todo :weight nil 'default)))
                   (if (eq w 'unspecified) 'bold w))
        :foreground "#D4AA00"
        :background "#3A2F00"
        :box (:line-width 1 :color "#D4AA00"))))))

(advice-add 'groovy-mode :after (lambda (&rest _) (tree-sitter-hl-mode -1)))


;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;                             AIDERMACS
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(setq aidermacs-backend 'vterm
      aidermacs-default-model "openrouter/minimax/minimax-m2:free"
      aidermacs-program "aider")


(spacemacs/set-leader-keys "op" 'yank-media)
