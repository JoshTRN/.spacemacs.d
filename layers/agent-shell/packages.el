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
                                             :repo "wandersoncferreira/agent-shell-dashboard")))
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
    ;; evil's motion-state `?' (evil-search-backward) outranks
    ;; `agent-shell-mode-map', so bind buffer-locally per state instead.
    (defun agent-shell-help-menu-local-keys ()
      "Bind ? to `agent-shell-help-menu' across evil states, buffer-locally."
      (when (fboundp 'evil-local-set-key)
        (dolist (state '(normal motion))
          (evil-local-set-key state (kbd "?") #'agent-shell-help-menu))))
    (add-hook 'agent-shell-mode-hook #'agent-shell-help-menu-local-keys)))

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
          #'agent-shell-dashboard-resume-with-recorded-agent)))
