;;; packages.el --- agent-shell layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
;;
;; Author: joshua <joshua@joshua-xps13>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ACP coding agents (Claude Code, Codex, Gemini, ...) inside Emacs via
;; the agent-shell package, on the applications prefix (SPC a a).

;;; Code:

(defconst agent-shell-packages
  '(agent-shell
    (agent-shell-dashboard :location (recipe :fetcher github
                                             :repo "wandersoncferreira/agent-shell-dashboard"))
    (agent-shell-hq :location (recipe :fetcher github
                                      :repo "sreenivasvrao/agent-shell-hq")))
  "The list of Lisp packages required by the agent-shell layer.")

(defun agent-shell/init-agent-shell ()
  (use-package agent-shell
    :commands (agent-shell
               agent-shell-openai-start-codex
               agent-shell-toggle
               agent-shell-new-shell
               agent-shell-new-temp-shell
               agent-shell-new-worktree-shell
               agent-shell-other-buffer
               agent-shell-resume-session
               agent-shell-fork
               agent-shell-interrupt
               agent-shell-send-clipboard-image
               agent-shell-send-clipboard-image-to
               agent-shell-send-screenshot
               agent-shell-send-region
               agent-shell-send-file
               agent-shell-set-session-model
               agent-shell-set-session-mode)
    :init
    (spacemacs/declare-prefix "aa" "agent-shell")
    (spacemacs/set-leader-keys
      "aaa" 'agent-shell
      "aat" 'agent-shell-toggle
      "aan" 'agent-shell-new-shell
      "aaT" 'agent-shell-new-temp-shell
      "aaw" 'agent-shell-new-worktree-shell
      "aab" 'agent-shell-other-buffer
      "aaR" 'agent-shell-resume-session
      "aaF" 'agent-shell-fork
      "aak" 'agent-shell-interrupt
      "aai" 'agent-shell-send-clipboard-image
      "aaI" 'agent-shell-send-clipboard-image-to
      "aas" 'agent-shell-send-screenshot
      "aar" 'agent-shell-send-region
      "aaf" 'agent-shell-send-file
      "aam" 'agent-shell-set-session-model
      "aap" 'agent-shell-set-session-mode)
    :config
    ;; IDs as advertised by the Claude Code ACP agent under "Available
    ;; models" / "Available modes" at shell startup.
    (setq agent-shell-anthropic-default-model-id "claude-fable-5[1m]")
    (setq agent-shell-anthropic-default-session-mode-id "bypassPermissions")
    ;; evil's state maps (motion `?', visual `>') outrank
    ;; `agent-shell-mode-map', so bind buffer-locally per state instead.
    (defun agent-shell-quote-or-shift-right ()
      "Quote read-only selections into the prompt; shift writable ones."
      (interactive)
      (if (text-property-not-all (region-beginning) (region-end)
                                 'read-only nil)
          (call-interactively #'agent-shell-quote-region)
        (call-interactively #'evil-shift-right)))
    (defun agent-shell-evil-local-keys ()
      "Bind agent-shell commands across evil states, buffer-locally."
      (when (fboundp 'evil-local-set-key)
        (dolist (state '(normal motion))
          (evil-local-set-key state (kbd "?") #'agent-shell-help-menu))
        (evil-local-set-key 'visual (kbd ">")
                            #'agent-shell-quote-or-shift-right)))
    (add-hook 'agent-shell-mode-hook #'agent-shell-evil-local-keys)
    ;; shell-maker resolves a shell's process from its buffer name, so
    ;; renames must go through `shell-maker-set-buffer-name' (a bare
    ;; `rename-buffer' detaches the session); keep any viewport buffer
    ;; in step too.
    (defun agent-shell-rename-session-buffer (buf name)
      "Rename shell BUF to NAME, dragging its viewport buffer along."
      (let ((viewport (get-buffer (concat (buffer-name buf) " [viewport]"))))
        (shell-maker-set-buffer-name buf name)
        (when (buffer-live-p viewport)
          (with-current-buffer viewport
            (rename-buffer (concat name " [viewport]") t)))))
    (defun agent-shell-name-buffer-after-first-prompt ()
      "Rename this session's buffer from its first submitted prompt."
      (agent-shell-subscribe-to
       :shell-buffer (current-buffer)
       :event 'input-submitted
       :on-event
       (lambda (event)
         ;; `input-submitted' is emitted before `agent-shell--send-command'
         ;; seeds the session title, so a nil title marks the first prompt.
         (unless (map-nested-elt agent-shell--state '(:session :title))
           (when-let* ((prompt (map-nested-elt event '(:data :prompt)))
                       (line (car (split-string prompt "\n" t "[ \t]+"))))
             (agent-shell-rename-session-buffer
              (current-buffer)
              (truncate-string-to-width line 48 nil nil "…")))))))
    (add-hook 'agent-shell-mode-hook
              #'agent-shell-name-buffer-after-first-prompt)))

(defun agent-shell/init-agent-shell-dashboard ()
  (use-package agent-shell-dashboard
    :commands (agent-shell-dashboard)
    :init
    (spacemacs/set-leader-keys "aad" 'agent-shell-dashboard)
    ;; The dashboard is a special-mode buffer driven entirely by its own
    ;; single-key map (c, g, f, K, ...); emacs state keeps evil's normal
    ;; state from shadowing it.
    (with-eval-after-load 'evil
      (evil-set-initial-state 'agent-shell-dashboard-mode 'emacs))
    :config
    ;; The transcript header already names the agent that ran a session
    ;; (the config's :mode-line-name, e.g. "Claude"), so resuming from
    ;; the dashboard shouldn't pop the "Resume with agent:" picker.
    (defun agent-shell-dashboard-resume-with-recorded-agent (session)
      "Resume SESSION with the agent recorded in its transcript header.
Falls back to the prompting default when no known config matches."
      (let* ((agent (plist-get session :agent))
             (config (and agent
                          (seq-find
                           (lambda (c)
                             (member agent (list (map-elt c :mode-line-name)
                                                 (map-elt c :buffer-name))))
                           (agent-shell--resolved-agent-configs)))))
        (if config
            (let ((default-directory (or (plist-get session :cwd)
                                         default-directory)))
              (agent-shell-start :config config
                                 :session-id (plist-get session :id)))
          (agent-shell-dashboard--resume-recent-default session))))
    (setq agent-shell-dashboard-resume-recent-function
          #'agent-shell-dashboard-resume-with-recorded-agent)

    ;; The recent-sessions scan probes `.agent-shell/transcripts/' under
    ;; every projectile-known project root; a remote (TRAMP) root makes
    ;; that probe open an SSH connection (even `expand-file-name' on
    ;; "/ssh:host:~/" connects, to resolve ~) and hangs the dashboard
    ;; when the host is unreachable.  Drop remote roots at their three
    ;; sources before any path is expanded -- `file-remote-p' only
    ;; parses the name, it never connects.
    (defun agent-shell-dashboard-skip-remote-roots (orig &rest args)
      "Call ORIG with remote (TRAMP) project roots filtered out."
      (let ((projects (symbol-function 'projectile-relevant-known-projects))
            (cwd (symbol-function 'agent-shell-dashboard--cwd)))
        (cl-letf (((symbol-function 'projectile-relevant-known-projects)
                   (lambda () (and projects
                                   (seq-remove #'file-remote-p
                                               (funcall projects)))))
                  ((symbol-function 'agent-shell-dashboard--cwd)
                   (lambda (b) (let ((d (funcall cwd b)))
                                 (and d (not (file-remote-p d)) d))))
                  (default-directory (if (file-remote-p default-directory)
                                         temporary-file-directory
                                       default-directory)))
          (apply orig args))))
    (advice-add 'agent-shell-dashboard--transcript-dirs :around
                #'agent-shell-dashboard-skip-remote-roots)))

(defun agent-shell/init-agent-shell-hq ()
  (use-package agent-shell-hq
    :commands (agent-shell-hq-toggle
               agent-shell-hq-toggle-jump-to-sidebar
               agent-shell-hq-peek
               agent-shell-hq-label)
    :init
    (spacemacs/set-leader-keys
      "aah" 'agent-shell-hq-toggle
      "aaP" 'agent-shell-hq-peek)
    ;; The sidebar/peek buffers are fundamental-mode with their own
    ;; single-key local maps; with no major mode to hang
    ;; `evil-set-initial-state' on, match the buffer names instead.
    (with-eval-after-load 'evil
      (add-to-list 'evil-buffer-regexps '("\\` \\*agent-shell-hq-" . emacs)))
    :config
    ;; Emacs state keeps evil off these buffers but also loses the SPC
    ;; leader; the package maps never bind SPC, so hang the leader map
    ;; off it directly (SPC w l etc. keep working).
    (define-key agent-shell-hq-toggle-map (kbd "SPC") spacemacs-default-map)
    (define-key agent-shell-hq-peek-map (kbd "SPC") spacemacs-default-map)
    ;; Joshua prefers a plain rename over the package's LLM titler, so
    ;; override `agent-shell-hq-label' (r / R in the sidebar) wholesale.
    (defun agent-shell-hq-rename (&optional shell-buf)
      "Prompt for a new name for SHELL-BUF (or the current shell)."
      (interactive)
      (let* ((buf (or shell-buf
                      (agent-shell--shell-buffer :no-create t :no-error t)
                      (user-error "No agent-shell buffer found")))
             (name (string-trim
                    (read-string "Rename session: " (buffer-name buf)))))
        (unless (or (string-empty-p name)
                    (string= name (buffer-name buf)))
          (agent-shell-rename-session-buffer buf name)
          (when (get-buffer-window " *agent-shell-hq-sidebar*")
            (agent-shell-hq-toggle-refresh)))))
    (advice-add 'agent-shell-hq-label :override #'agent-shell-hq-rename)
    ;; q and RET hide the sidebar instead of leaving the workspace
    ;; (C-g still leaves); aah re-shows a hidden sidebar.
    (defun agent-shell-hq-close-sidebar ()
      "Hide the sidebar window, keeping the shell in the main window."
      (interactive)
      (when-let ((win (get-buffer-window agent-shell-hq-toggle--sidebar-name)))
        (when (window-live-p agent-shell-hq-toggle--main-window)
          (select-window agent-shell-hq-toggle--main-window))
        (delete-window win)))
    (define-key agent-shell-hq-toggle-map (kbd "q") #'agent-shell-hq-close-sidebar)
    (unless (assq 'agent-shell-hq-close-sidebar agent-shell-hq-toggle--hints)
      (setq agent-shell-hq-toggle--hints
            (append (butlast agent-shell-hq-toggle--hints)
                    '((agent-shell-hq-close-sidebar . "hide")
                      (agent-shell-hq-toggle        . "quit")))))
    (defun agent-shell-hq-select-close-sidebar (orig)
      (let ((entry (nth agent-shell-hq-toggle--current-idx
                        agent-shell-hq-toggle--entries)))
        (funcall orig)
        (when (eq (plist-get entry :type) 'buffer)
          (agent-shell-hq-close-sidebar))))
    (advice-add 'agent-shell-hq-toggle-select
                :around #'agent-shell-hq-select-close-sidebar)
    (defun agent-shell-hq-toggle-reshow-sidebar (orig)
      (if (and (string= (safe-persp-name (get-current-persp))
                        agent-shell-hq-toggle--persp-name)
               (get-buffer agent-shell-hq-toggle--sidebar-name)
               (not (get-buffer-window agent-shell-hq-toggle--sidebar-name)))
          (progn
            (display-buffer-in-side-window
             (get-buffer agent-shell-hq-toggle--sidebar-name)
             `((side          . left)
               (window-width  . ,agent-shell-hq-toggle-sidebar-width)
               (window-parameters . ((no-delete-other-windows . t)))))
            (unless (window-live-p agent-shell-hq-toggle--main-window)
              (setq agent-shell-hq-toggle--main-window
                    (seq-find (lambda (w)
                                (not (window-parameter w 'window-side)))
                              (window-list))))
            (select-window
             (get-buffer-window agent-shell-hq-toggle--sidebar-name))
            (agent-shell-hq-toggle-refresh))
        (funcall orig)))
    (advice-add 'agent-shell-hq-toggle
                :around #'agent-shell-hq-toggle-reshow-sidebar)))
