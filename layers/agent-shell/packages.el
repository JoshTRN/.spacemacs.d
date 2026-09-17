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
  '(agent-shell)
  "The list of Lisp packages required by the agent-shell layer.")

(defun agent-shell/init-agent-shell ()
  (use-package agent-shell
    :commands (agent-shell
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
               agent-shell-send-file)
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
      "aaf" 'agent-shell-send-file)))
