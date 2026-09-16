;;; packages.el --- zoho-tickets layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `zoho-tickets-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `zoho-tickets/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `zoho-tickets/pre-init-PACKAGE' and/or
;;   `zoho-tickets/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst zoho-tickets-packages
  '((zoho-desk :location local))
  "The list of Lisp packages required by the zoho-tickets layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun zoho-tickets/init-zoho-desk ()
  (use-package zoho-desk
    :commands (zoho-desk-dashboard
               zoho-desk-tickets
               zoho-desk-open-ticket-url
               zoho-desk-quickfind
               zoho-desk-authorize
               zoho-desk-add-time-entry
               zoho-desk-log-time-from-org
               zoho-desk-start-ticket-timer
               zoho-desk-finish-ticket-timer
               zoho-desk-cancel-ticket-timer)
    :init
    (spacemacs/declare-prefix "oz" "zoho")
    (spacemacs/set-leader-keys
      "ozz" 'zoho-desk-dashboard
      "ozu" 'zoho-desk-open-ticket-url
      "ozf" 'zoho-desk-quickfind
      "oza" 'zoho-desk-authorize
      "ozt" 'zoho-desk-add-time-entry
      "ozl" 'zoho-desk-log-time-from-org
      "ozi" 'zoho-desk-start-ticket-timer
      "ozo" 'zoho-desk-finish-ticket-timer
      "ozk" 'zoho-desk-cancel-ticket-timer)
    :config
    ;; Major-mode leader (, / SPC m) in the dashboard panes.
    (spacemacs/set-leader-keys-for-major-mode 'zoho-desk-views-mode
      "t" 'zoho-desk-toggle-view-at-point
      "r" 'zoho-desk-refresh-views
      "d" 'zoho-desk-select-department
      "q" 'zoho-desk-quit)
    (spacemacs/set-leader-keys-for-major-mode 'zoho-desk-tickets-mode
      "o" 'zoho-desk-open-ticket-at-point
      "r" 'zoho-desk-refresh-table
      "n" 'zoho-desk-next-page
      "p" 'zoho-desk-previous-page
      "t" 'zoho-desk-add-time-entry
      "T" 'zoho-desk-start-ticket-timer
      "w" 'zoho-desk-copy-org-snippet
      "y" 'zoho-desk-copy-ticket-url
      "#" 'zoho-desk-copy-ticket-number
      "b" 'zoho-desk-browse-ticket
      "d" 'zoho-desk-select-department
      "q" 'zoho-desk-quit)
    ;; Ticket documents are org-mode; the zoho commands ride the
    ;; minor-mode leader so org's own leader keys stay available.
    (spacemacs/set-leader-keys-for-minor-mode 'zoho-desk-ticket-minor-mode
      "1" 'zoho-desk-tab-overview
      "2" 'zoho-desk-tab-thread
      "3" 'zoho-desk-tab-comments
      "4" 'zoho-desk-tab-time-logs
      "e" 'zoho-desk-expand-thread-at-point
      "s" 'zoho-desk-send-reply
      "l" 'zoho-desk-submit-time-log
      "." 'zoho-desk-pick-executed-time
      "a" 'zoho-desk-add-email
      "ie" 'zoho-desk-insert-image
      "c" 'zoho-desk-add-comment
      "t" 'zoho-desk-add-time-entry
      "T" 'zoho-desk-start-ticket-timer
      "r" 'zoho-desk-refresh-ticket
      "b" 'zoho-desk-browse-ticket
      "w" 'zoho-desk-copy-org-snippet
      "y" 'zoho-desk-copy-ticket-url
      "#" 'zoho-desk-copy-ticket-number)
    (spacemacs/set-leader-keys-for-major-mode 'zoho-desk-comment-mode
      "s" 'zoho-desk-comment-send
      "k" 'zoho-desk-comment-abort)))
