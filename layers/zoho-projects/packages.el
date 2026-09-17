;;; packages.el --- zoho-projects layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

;; Zoho Projects inside Emacs: projects/tasks dashboard, org-mode task
;; documents and time logging, mirroring the zoho-tickets layer.

;;; Code:

(defconst zoho-projects-packages
  '((zoho-projects :location local))
  "The list of Lisp packages required by the zoho-projects layer.")

(defun zoho-projects/init-zoho-projects ()
  (use-package zoho-projects
    :commands (zoho-projects-dashboard
               zoho-projects-quickfind
               zoho-projects-select-project
               zoho-projects-authorize
               zoho-projects-add-time-entry
               zoho-projects-log-time-from-org
               zoho-projects-start-task-timer
               zoho-projects-finish-task-timer
               zoho-projects-cancel-task-timer)
    :init
    (spacemacs/declare-prefix "az" "zoho")
    (spacemacs/declare-prefix "azp" "projects")
    (spacemacs/set-leader-keys
      "azpd" 'zoho-projects-dashboard
      "azpf" 'zoho-projects-quickfind
      "azpa" 'zoho-projects-authorize
      "azpt" 'zoho-projects-add-time-entry
      "azpl" 'zoho-projects-log-time-from-org
      "azpi" 'zoho-projects-start-task-timer
      "azpo" 'zoho-projects-finish-task-timer
      "azpk" 'zoho-projects-cancel-task-timer)
    :config
    ;; Major-mode leader (, / SPC m) in the dashboard panes.
    (spacemacs/set-leader-keys-for-major-mode 'zoho-projects-statuses-mode
      "r" 'zoho-projects-refresh-tasks
      "p" 'zoho-projects-select-project
      "f" 'zoho-projects-quickfind
      "b" 'zoho-projects-browse-project
      "q" 'zoho-projects-quit)
    (spacemacs/set-leader-keys-for-major-mode 'zoho-projects-tasks-mode
      "o" 'zoho-projects-open-task-at-point
      "f" 'zoho-projects-quickfind
      "r" 'zoho-projects-refresh-tasks
      "p" 'zoho-projects-select-project
      "t" 'zoho-projects-add-time-entry
      "T" 'zoho-projects-start-task-timer
      "w" 'zoho-projects-copy-org-snippet
      "y" 'zoho-projects-copy-task-url
      "#" 'zoho-projects-copy-task-key
      "b" 'zoho-projects-browse-task
      "q" 'zoho-projects-quit)
    ;; Task documents are org-mode; the zoho commands ride the
    ;; minor-mode leader so org's own leader keys stay available.
    (spacemacs/set-leader-keys-for-minor-mode 'zoho-projects-task-minor-mode
      "1" 'zoho-projects-tab-overview
      "2" 'zoho-projects-tab-comments
      "3" 'zoho-projects-tab-time-logs
      "l" 'zoho-projects-submit-time-log
      "." 'zoho-projects-pick-date
      "c" 'zoho-projects-add-comment
      "t" 'zoho-projects-add-time-entry
      "T" 'zoho-projects-start-task-timer
      "r" 'zoho-projects-refresh-task
      "b" 'zoho-projects-browse-task
      "w" 'zoho-projects-copy-org-snippet
      "y" 'zoho-projects-copy-task-url
      "#" 'zoho-projects-copy-task-key)
    (spacemacs/set-leader-keys-for-major-mode 'zoho-projects-comment-mode
      "s" 'zoho-projects-comment-send
      "k" 'zoho-projects-comment-abort)))
