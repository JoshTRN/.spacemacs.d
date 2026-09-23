;;; zoho-desk.el --- Zoho Desk ticketing inside Emacs  -*- lexical-binding: t; -*-

;; Author: joshua
;; Keywords: tools, comm
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A Zoho Desk client shaped like a dashboard:
;;
;; - `zoho-desk-dashboard'        two panes: view checkboxes on the left,
;;                                ticket table on the right.  Checked views
;;                                are OR-ed: the table shows the union of
;;                                their tickets.
;; - RET on a ticket              org-mode ticket document with Overview,
;;                                Thread, Comments and Time Logs tabs.
;;                                Comments run newest first under a New
;;                                Comment field (C-c z m posts it); typing
;;                                @ there completes an agent mention
;; - `zoho-desk-add-time-entry'   post a time entry to a ticket
;; - `zoho-desk-log-time-from-org' send org-clocked time to a ticket
;; - `zoho-desk-start-ticket-timer' clock the posframe timer in against
;;                                a ticket; `zoho-desk-finish-ticket-timer'
;;                                (from anywhere) fills the ticket's New
;;                                Time Log fields with the elapsed time
;; - `zoho-desk-authorize'        one-time exchange of a self-client
;;                                grant code for a refresh token
;;
;; All API traffic is non-blocking.  Reads pop up a buffer immediately
;; with a "Fetching …" placeholder that fills in when the responses
;; arrive; writes (replies, comments, time entries) send in the
;; background so you can keep working — success lands in the echo
;; area, and failure refocuses the buffer holding the unsent content
;; with a loud error, ready to retry.
;;
;; Authentication uses a Zoho "Self Client" (https://api-console.zoho.com):
;; create one, note the Client ID and Secret, then generate a grant code
;; with scopes:
;;
;;   Desk.basic.ALL,Desk.tickets.ALL,Desk.settings.READ,Desk.search.READ,Desk.contacts.READ,Desk.agents.READ
;;
;; and run M-x zoho-desk-authorize to obtain a refresh token.  Store the
;; three secrets in ~/.authinfo.gpg:
;;
;;   machine zoho-desk login client-id password 1000.XXXX
;;   machine zoho-desk login client-secret password XXXX
;;   machine zoho-desk login refresh-token password 1000.XXXX
;;
;; or set `zoho-desk-client-id', `zoho-desk-client-secret' and
;; `zoho-desk-refresh-token' directly.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'url)
(require 'json)
(require 'auth-source)
(require 'tabulated-list)
(require 'shr)
(require 'color)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(declare-function posframe-timer-clock-in "posframe-timer")
(declare-function posframe-timer-clock-out "posframe-timer")
(declare-function posframe-timer-clock-cancel "posframe-timer")

(declare-function outline-show-subtree "outline")

(declare-function org-mode "org")
(declare-function org-entry-get "org")
(declare-function org-get-heading "org")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-meta-data "org")
(declare-function org-end-of-subtree "org")
(declare-function org-narrow-to-subtree "org")
(declare-function org-read-date "org")
(declare-function org-time-string-to-time "org")
(declare-function org-time-stamp "org")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-show-all "org")
(declare-function org-fold-hide-subtree "org-fold")
(declare-function outline-hide-subtree "outline")
(declare-function org-display-inline-images "org")
(declare-function org-export-string-as "ox")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-define-minor-mode-key "evil-core")

;;;; Configuration

(defgroup zoho-desk nil
  "Zoho Desk client."
  :group 'tools
  :prefix "zoho-desk-")

(defcustom zoho-desk-base-url "https://desk.zoho.com/api/v1"
  "Base URL of the Zoho Desk API.
Use the domain matching your data center, e.g. desk.zoho.eu or
desk.zoho.in."
  :type 'string)

(defcustom zoho-desk-accounts-url "https://accounts.zoho.com"
  "Zoho accounts server used for OAuth token exchange.
Must match the data center of `zoho-desk-base-url'."
  :type 'string)

(defcustom zoho-desk-client-id nil
  "OAuth client id of the Zoho self client.
When nil, looked up in auth-source as host `zoho-desk', login
`client-id'."
  :type '(choice (const nil) string))

(defcustom zoho-desk-client-secret nil
  "OAuth client secret of the Zoho self client.
When nil, looked up in auth-source as host `zoho-desk', login
`client-secret'."
  :type '(choice (const nil) string))

(defcustom zoho-desk-refresh-token nil
  "OAuth refresh token (obtain with `zoho-desk-authorize').
When nil, looked up in auth-source as host `zoho-desk', login
`refresh-token'."
  :type '(choice (const nil) string))

(defcustom zoho-desk-org-id nil
  "Zoho Desk organization id.
When nil, the first organization returned by the API is used."
  :type '(choice (const nil) string))

(defcustom zoho-desk-department-id nil
  "Zoho Desk department whose views the dashboard shows.
Views are per-department in Zoho Desk, so the dashboard needs one.
When nil, the dashboard prompts once per session (switch later with
`zoho-desk-select-department', bound to D in the sidebar)."
  :type '(choice (const nil) string))

(defcustom zoho-desk-starred-view-names
  '("Open" "Escalated" "Hold" "Unclosed" "Codebeamer Support Level 2"
    "Closed" "My Cases" "All Cases")
  "View names listed in the sidebar's Starred Views section, in order.
The public API does not expose the web UI's starred views (the
/starredViews endpoint returns nothing for them), so the starred
section is driven by this list instead.  Names are matched
case-insensitively.  Note the web UI renames the system views: \"My
Tickets\" is \"My Cases\" in the API, \"All Tickets\" is \"All Cases\"."
  :type '(repeat string))

(defcustom zoho-desk-page-size 50
  "Number of tickets fetched per view and page."
  :type 'integer)

(defcustom zoho-desk-quickfind-limit 300
  "How many of a department's newest tickets quickfind offers.
`zoho-desk-quickfind' fetches this many tickets (in parallel
pages of 100) as the helm fuzzy-find candidate pool."
  :type 'integer)

(defcustom zoho-desk-sidebar-width 32
  "Width of the views sidebar in the dashboard."
  :type 'integer)

(defcustom zoho-desk-ticket-window-height 0.85
  "Height of the ticket window opened below the ticket table.
A float is a fraction of the frame height, an integer a number of
lines.  Only applied when the window is created; resizing it by hand
sticks until the window is closed."
  :type '(choice (float :tag "Fraction of frame height")
                 (integer :tag "Lines")))

(defcustom zoho-desk-thread-prefetch 5
  "How many of the newest threads are fetched in full when a ticket opens.
Older threads show their summary; expand them with
`zoho-desk-expand-thread-at-point'."
  :type 'integer)

(defcustom zoho-desk-comments-public nil
  "When non-nil, comments are posted as public (visible to the contact)."
  :type 'boolean)

(defcustom zoho-desk-reply-from nil
  "Support email address used as the From of email replies.
Must be one of the department's configured support addresses.  When
nil, the address is derived from the latest inbound email thread
(the address the customer wrote to)."
  :type '(choice (const nil) string))

(defcustom zoho-desk-signature nil
  "Plain-text signature appended to outgoing email replies, when non-nil."
  :type '(choice (const nil) string))

(defcustom zoho-desk-reply-html t
  "When non-nil, replies are exported from org to HTML before sending.
Org markup (bold, lists, tables, links) renders in the email, and
[[file:...]] image links are sent as attachments of the reply and
shown as \"[image: ...]\" markers in the body (the public API
cannot embed true inline images).  When nil, the reply is sent as
plain text."
  :type 'boolean)

(defcustom zoho-desk-pandoc-program "pandoc"
  "Pandoc executable used to convert incoming HTML to org markup.
With it, thread bodies, descriptions and comments keep their
formatting on ingest — bold, tables, code blocks, links — as org
markup.  When nil, or when the program is not installed, they
degrade to the plain-text rendering."
  :type '(choice (string :tag "Program") (const :tag "Disabled" nil)))

(defcustom zoho-desk-inline-images t
  "When non-nil, fetch and display inline images in ticket buffers.
Incoming emails reference their embedded images as relative
/api/v1/.../inlineImages/... links; those are downloadable through
the authenticated API, so they are fetched in the background and
shown in place of the link.  Fetched images are cached under
`temporary-file-directory' for the session."
  :type 'boolean)

(defcustom zoho-desk-send-inline-images t
  "When non-nil, [[file:...]] images in replies are sent truly inline.
Each image is uploaded through the agent console's composer
servlet and referenced so that Zoho's mailer converts it to a
real cid MIME part — the recipient sees the image in the email
body, not as an attachment.  The servlet only accepts the
browser's session cookie (there is no OAuth equivalent), so
`zoho-desk-refresh-session-cookie' must be run when the session
expires; a send with images is refused, not degraded, until then.
When nil, images go out as attachments with [image: ...] markers."
  :type 'boolean)

(defcustom zoho-desk-inline-image-max-width 500
  "Maximum display width of an inline image, in pixels."
  :type 'natnum)

(defcustom zoho-desk-department-reply-from nil
  "Alist mapping departmentId to the support address used as From.
Filled in automatically when you confirm the From prompt on a
ticket whose department has no known support address yet."
  :type '(alist :key-type string :value-type string))

(defcustom zoho-desk-status-colors
  '(("Open" . "#1cb8ec")
    ("Escalated to PTC" . "#1cb8ec")
    ("Waiting on Customer" . "#fdc428")
    ("On Hold" . "#fdc428")
    ("Closed" . "#5cc064")
    ("Copied from PTC" . "#9aa0a6")
    ("Waiting on IQNOX" . "#9aa0a6"))
  "Status badge colors, matching the Zoho Desk status dots.
Keys are status names, matched case-insensitively; the color
becomes the badge background of the ticket document's status
heading.  Unlisted statuses fall back to the plain org-modern
todo/done faces."
  :type '(alist :key-type string :value-type color))

(defcustom zoho-desk-request-timeout 30
  "Timeout in seconds for API requests."
  :type 'integer)

;;;; Faces

(defface zoho-desk-accent
  '((t :weight bold))
  "Accent text face for highlights (ticket numbers, checked views).
Synced to the modeline accent by `zoho-desk--sync-accent-faces'.")

(defface zoho-desk-tab-active
  '((t :weight bold :box t))
  "Face of the active header-line tab.
Synced to the modeline accent by `zoho-desk--sync-accent-faces'.")

(defface zoho-desk-input
  '((t :extend t))
  "Background of the editable input fields in ticket buffers.
Synced to the midpoint of the default and solaire backgrounds by
`zoho-desk--sync-accent-faces', so the fields read as neither
plain text nor a code block.")

(defface zoho-desk-mention
  '((t :inherit zoho-desk-accent))
  "Face of @agent mentions in comment fields and comment entries.")

(defun zoho-desk--blend-colors (a b)
  "Return the hex color midway between color names A and B.
Nil when either name does not resolve (e.g. tty frames)."
  (let ((a (color-name-to-rgb a))
        (b (color-name-to-rgb b)))
    (when (and a b)
      (apply #'color-rgb-to-hex
             `(,@(cl-mapcar (lambda (x y) (/ (+ x y) 2)) a b) 2)))))

(defun zoho-desk--sync-accent-faces ()
  "Derive the zoho-desk faces from the theme's modeline accent.
Uses the `powerline-active2' segment color (the modeline accent),
falling back to the mode-line background.  Also places the input
field background halfway between the default and solaire (block)
backgrounds."
  (when-let* ((bg (face-background 'default nil t))
              (block-bg (or (and (facep 'solaire-default-face)
                                 (face-background 'solaire-default-face
                                                  nil t))
                            (face-background 'org-block nil t)))
              (mid (zoho-desk--blend-colors bg block-bg)))
    (set-face-attribute 'zoho-desk-input nil :background mid))
  (when-let* ((accent (or (and (facep 'powerline-active2)
                               (face-background 'powerline-active2 nil t))
                          (face-background 'mode-line nil t))))
    (set-face-attribute 'zoho-desk-tab-active nil
                        :background accent
                        :foreground (or (face-foreground 'powerline-active2
                                                         nil t)
                                        (face-foreground 'mode-line nil t)
                                        'unspecified)
                        :weight 'bold :box t)
    (set-face-attribute 'zoho-desk-accent nil
                        :foreground (or (ignore-errors
                                          (color-lighten-name accent 25))
                                        accent)
                        :weight 'bold)))

;;;; Authentication

(defvar zoho-desk--access-token nil)
(defvar zoho-desk--token-expiry 0)
(defvar zoho-desk--cached-org-id nil)

(defun zoho-desk--secret (login)
  "Return the auth-source password for host `zoho-desk' and LOGIN."
  (when-let* ((entry (car (auth-source-search :host "zoho-desk"
                                              :user login :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun zoho-desk--credential (var login)
  "Return VAR if non-nil, else the auth-source secret for LOGIN."
  (or var (zoho-desk--secret login)
      (user-error "Zoho Desk: no %s configured (see zoho-desk.el commentary)"
                  login)))

(defun zoho-desk--parse-json (string)
  "Parse STRING as JSON into nested alists and lists."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun zoho-desk--response-body ()
  "Return the decoded body of the url response in the current buffer."
  (goto-char (or url-http-end-of-headers (point-min)))
  (decode-coding-string
   (buffer-substring-no-properties (point) (point-max)) 'utf-8))

(defun zoho-desk--response-body-raw ()
  "Return the undecoded body bytes of the url response buffer.
`url-http-end-of-headers' can sit before the headers' final
newline; that byte is skipped, it must not leak into the body."
  (goto-char (or url-http-end-of-headers (point-min)))
  (when (eq (char-after) ?\n)
    (forward-char 1))
  (buffer-substring-no-properties (point) (point-max)))

(defun zoho-desk--token-request (params)
  "POST PARAMS to the Zoho OAuth token endpoint, return parsed JSON."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (url-build-query-string params))
         (buf (url-retrieve-synchronously
               (concat zoho-desk-accounts-url "/oauth/v2/token")
               t t zoho-desk-request-timeout)))
    (unless buf (error "Zoho Desk: token request timed out"))
    (with-current-buffer buf
      (unwind-protect
          (zoho-desk--parse-json (zoho-desk--response-body))
        (kill-buffer buf)))))

(defun zoho-desk--refresh-access-token ()
  "Obtain a fresh access token using the refresh token."
  (let* ((response
          (zoho-desk--token-request
           `(("refresh_token" ,(zoho-desk--credential
                                zoho-desk-refresh-token "refresh-token"))
             ("client_id" ,(zoho-desk--credential
                            zoho-desk-client-id "client-id"))
             ("client_secret" ,(zoho-desk--credential
                                zoho-desk-client-secret "client-secret"))
             ("grant_type" "refresh_token"))))
         (token (alist-get 'access_token response)))
    (unless token
      (error "Zoho Desk: token refresh failed: %S" response))
    (setq zoho-desk--access-token token
          zoho-desk--token-expiry (+ (float-time)
                                     (- (or (alist-get 'expires_in response)
                                            3600)
                                        60)))
    token))

(defun zoho-desk--ensure-token ()
  "Return a valid access token, refreshing if necessary."
  (if (and zoho-desk--access-token
           (< (float-time) zoho-desk--token-expiry))
      zoho-desk--access-token
    (zoho-desk--refresh-access-token)))

(defun zoho-desk--persist-refresh-token (refresh)
  "Save REFRESH as the zoho-desk refresh-token authinfo line.
Writes to the first existing file in `auth-sources' (falling back
to ~/.authinfo), replacing any previous refresh-token line."
  (let ((file (or (seq-find #'file-exists-p
                            (mapcar #'expand-file-name
                                    (seq-filter #'stringp auth-sources)))
                  (expand-file-name "~/.authinfo"))))
    (when (y-or-n-p (format "Save refresh token to %s? " file))
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file))
        (goto-char (point-min))
        (flush-lines "^machine zoho-desk login refresh-token ")
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "machine zoho-desk login refresh-token password %s\n"
                        refresh))
        (write-region (point-min) (point-max) file nil 'silent))
      (set-file-modes file #o600)
      (auth-source-forget-all-cached)
      (message "Refresh token saved to %s — no more grant codes needed"
               file)
      t)))

;;;; Agent-console session cookie
;;
;; Sending a NEW image truly inline needs the composer's upload
;; servlet (ImageUpload.do), which only accepts the browser's session
;; cookie — there is no OAuth equivalent (verified by probing; see
;; README).  Two cookies suffice: __Secure-iamsdt (the IAM session,
;; HttpOnly so only visible in the DevTools Network pane) and crmcsr
;; (the CSRF token, echoed as header and form field).

(defvar zoho-desk--ticket)              ; buffer-local, defined below

(defvar zoho-desk--session-cookie nil
  "Minimal agent-console cookie string, or nil until captured.")

(defconst zoho-desk--browser-user-agent
  (concat "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
          "(KHTML, like Gecko) Chrome/150.0.0.0 Safari/537.36")
  "The composer servlet rejects requests without a browser UA.")

(defun zoho-desk--current-session-cookie ()
  "Return the session cookie, loading it from auth-source if needed."
  (or zoho-desk--session-cookie
      (setq zoho-desk--session-cookie
            (zoho-desk--secret "session-cookie"))))

(defun zoho-desk--ticket-portal-base (ticket)
  "Return TICKET's (BASE . PORTAL) pair, or nil when underivable.
BASE is the https://<host> origin and PORTAL the portal slug,
both taken from the customer-portal webUrl the API returns."
  (when-let* ((web (alist-get 'webUrl ticket)))
    (when (string-match
           "\\`\\(https://[^/]+\\)/\\(?:support\\|portal\\)/\\([^/]+\\)/" web)
      (cons (match-string 1 web) (match-string 2 web)))))

(defun zoho-desk--session-cookie-live-p (cookie base portal)
  "Probe whether COOKIE is a live agent-console session.
GETs BASE/agent/PORTAL without following redirects: a signed-in
agent session is served directly, while a dead or help-center-only
session (the customer-portal view sets cookies too, but the
composer servlet refuses them) 302s to the login page, and a
malformed one gets a 4xx.  Only those definite signatures count as
dead — network trouble or an unfamiliar response never rejects a
cookie the servlet might accept."
  (let* ((url-request-method "GET")
         (url-user-agent zoho-desk--browser-user-agent)
         (url-max-redirections 0)
         (url-request-extra-headers
          `(("Cookie" . ,(encode-coding-string cookie 'utf-8))))
         (buf (url-retrieve-synchronously (format "%s/agent/%s" base portal)
                                          t t zoho-desk-request-timeout)))
    (if (not buf)
        t
      (with-current-buffer buf
        (prog1
            (not (or (and (numberp url-http-response-status)
                          (>= url-http-response-status 400))
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward
                        "^Location: .*\\(?:login\\.sas\\|accounts\\.zoho\\)"
                        (or url-http-end-of-headers (point-max)) t))))
          (kill-buffer))))))

(defun zoho-desk--session-cookie-parse (header)
  "Extract the two needed cookies from a pasted Cookie HEADER.
Returns the minimal cookie string, or nil when either cookie is
missing from the paste."
  (let (sdt csr)
    (dolist (pair (split-string (or header "") ";[ \t]*" t))
      (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" pair)
        (let ((name (string-trim (match-string 1 pair)))
              (value (match-string 2 pair)))
          (cond ((equal name "__Secure-iamsdt") (setq sdt value))
                ((equal name "crmcsr") (setq csr value))))))
    (when (and sdt csr)
      (format "__Secure-iamsdt=%s;crmcsr=%s" sdt csr))))

(defun zoho-desk--persist-session-cookie (cookie)
  "Save COOKIE as the zoho-desk session-cookie authinfo line."
  (let ((file (or (seq-find #'file-exists-p
                            (mapcar #'expand-file-name
                                    (seq-filter #'stringp auth-sources)))
                  (expand-file-name "~/.authinfo"))))
    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file))
      (goto-char (point-min))
      (flush-lines "^machine zoho-desk login session-cookie ")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "machine zoho-desk login session-cookie password \"%s\"\n"
                      cookie))
      (write-region (point-min) (point-max) file nil 'silent))
    (set-file-modes file #o600)
    (auth-source-forget-all-cached)))

(defun zoho-desk--refresh-session-cookie-flow (ticket)
  "Open TICKET's agent console in the browser, then prompt for a Cookie header.
Returns the minimal cookie string, now current and persisted.
Quitting the prompt (C-g / ESC) returns nil — callers must then
leave the reply alone.  A paste without the needed cookies, or one
whose session is not actually signed in to the agent console (a
fresh browser lands on the customer-portal view, whose cookies the
composer servlet refuses), aborts with an explanatory error."
  (let ((base-portal (zoho-desk--ticket-portal-base ticket)))
    (when-let* ((url (and ticket
                          (or (zoho-desk--agent-ticket-url ticket)
                              (and base-portal
                                   (format "%s/agent/%s"
                                           (car base-portal) (cdr base-portal)))
                              (alist-get 'webUrl ticket)))))
      (browse-url url))
    (let ((input (condition-case nil
                     (read-string
                      (concat "Zoho session cookie refresh — in the browser: "
                              "SIGN IN to the /agent/ console if it isn't "
                              "already (a fresh browser shows the customer "
                              "portal — that session won't do), reload, then "
                              "F12 → Network → click a fresh support-host "
                              "request → Request Headers → copy the whole "
                              "Cookie line: "))
                   (quit nil))))
      (when input
        (let ((cookie (zoho-desk--session-cookie-parse input)))
          (unless cookie
            (user-error (concat "Zoho Desk: that paste has no __Secure-iamsdt"
                                " + crmcsr — copy the full Cookie request"
                                " header from the Network pane")))
          (when (and base-portal
                     (not (zoho-desk--session-cookie-live-p
                           cookie (car base-portal) (cdr base-portal))))
            (user-error (concat "Zoho Desk: that session is not signed in to"
                                " the agent console — open %s/agent/%s, sign"
                                " in, reload, and copy the Cookie line from a"
                                " fresh request")
                        (car base-portal) (cdr base-portal)))
          (setq zoho-desk--session-cookie cookie)
          (zoho-desk--persist-session-cookie cookie)
          cookie)))))

;;;###autoload
(defun zoho-desk-refresh-session-cookie ()
  "Capture a fresh agent-console session cookie for inline images.
Opens the current ticket's agent console in the browser, then
prompts for the browser's Cookie request header.  In the browser:
make sure the /agent/ console is actually signed in — a fresh
browser session lands on the customer-portal view, whose cookies
the composer servlet refuses — then DevTools (F12) → Network tab →
click a fresh request to the support host → Request Headers → copy
the whole \"Cookie:\" line.  The needed parts (__Secure-iamsdt and
crmcsr) are extracted, checked against the agent console (a
signed-out paste is rejected on the spot), and persisted to
authinfo."
  (interactive)
  (if (zoho-desk--refresh-session-cookie-flow
       (or zoho-desk--ticket (ignore-errors (zoho-desk--ticket-at-point))))
      (message "Zoho Desk: session cookie updated")
    (message "Zoho Desk: session cookie refresh cancelled")))

;;;###autoload
(defun zoho-desk-authorize (code)
  "Exchange self-client grant CODE for a refresh token.
Generate the code at https://api-console.zoho.com under your Self
Client's \"Generate Code\" tab with scopes
Desk.basic.ALL,Desk.tickets.ALL,Desk.settings.READ,
Desk.search.READ,Desk.contacts.READ,Desk.agents.READ (as one
comma-separated line).  Offers to persist the refresh token into
authinfo so this is a one-time step per Zoho account."
  (interactive "sGrant code from Zoho API console: ")
  (let* ((response
          (zoho-desk--token-request
           `(("code" ,code)
             ("client_id" ,(zoho-desk--credential
                            zoho-desk-client-id "client-id"))
             ("client_secret" ,(zoho-desk--credential
                                zoho-desk-client-secret "client-secret"))
             ("grant_type" "authorization_code"))))
         (refresh (alist-get 'refresh_token response)))
    (unless refresh
      (error "Zoho Desk: authorization failed: %S" response))
    (setq zoho-desk-refresh-token refresh)
    (when-let* ((token (alist-get 'access_token response)))
      (setq zoho-desk--access-token token
            zoho-desk--token-expiry (+ (float-time) 3000)))
    (unless (zoho-desk--persist-refresh-token refresh)
      (kill-new refresh)
      (message (concat "Refresh token copied to kill ring.  Save it as: "
                       "machine zoho-desk login refresh-token password "
                       "<token> in your authinfo file")))))

;;;; HTTP

(defun zoho-desk--request-url (path params)
  "Return the full request URL for PATH with query PARAMS, as unibyte."
  (encode-coding-string
   (concat zoho-desk-base-url path
           (and params (concat "?" (url-build-query-string params))))
   'utf-8))

(defun zoho-desk--request-headers (token org-id content-type)
  "Build the request header alist from TOKEN, ORG-ID and CONTENT-TYPE.
Header values must be unibyte: strings pulled out of parsed JSON
\(token, orgId, ticket ids) are multibyte, and concatenating any
multibyte string with a binary body makes url-http reject the
request (\"Multibyte text in HTTP request\")."
  (mapcar
   (lambda (header)
     (cons (car header)
           (encode-coding-string (format "%s" (cdr header)) 'utf-8)))
   (append
    `(("Authorization" . ,(concat "Zoho-oauthtoken " token)))
    (when org-id `(("orgId" . ,org-id)))
    (when content-type `(("Content-Type" . ,content-type))))))

(cl-defun zoho-desk--request (method path &key params payload raw-payload
                                     content-type no-org (retries 1))
  "Perform an authenticated METHOD request against PATH, blocking.
PARAMS is a query-parameter list for `url-build-query-string',
PAYLOAD an alist serialized as the JSON body.  RAW-PAYLOAD is a
pre-encoded body sent verbatim with CONTENT-TYPE (used for
multipart uploads).  NO-ORG omits the orgId header (needed only
for /organizations itself).  Returns the parsed JSON response, or
nil for empty responses.  Only interactive prompt flows (authorize,
the department chooser) still block here; everything else goes
through `zoho-desk--request-async'."
  (let* ((url (zoho-desk--request-url path params))
         (url-request-method method)
         (url-request-extra-headers
          (zoho-desk--request-headers
           (zoho-desk--ensure-token)
           (unless no-org (zoho-desk--ensure-org-id))
           (and (or payload raw-payload)
                (or content-type "application/json"))))
         (url-request-data
          (or raw-payload
              (when payload
                (encode-coding-string (json-encode payload) 'utf-8))))
         (buf (url-retrieve-synchronously url t t zoho-desk-request-timeout))
         status body)
    (unless buf (error "Zoho Desk: request timed out: %s %s" method path))
    (with-current-buffer buf
      (setq status url-http-response-status
            body (zoho-desk--response-body))
      (kill-buffer buf))
    (cond
     ((and (eq status 401) (> retries 0))
      (setq zoho-desk--access-token nil)
      (zoho-desk--request method path :params params :payload payload
                          :raw-payload raw-payload :content-type content-type
                          :no-org no-org :retries (1- retries)))
     ((memq status '(200 201))
      (unless (string-empty-p (string-trim body))
        (zoho-desk--parse-json body)))
     ((eq status 204) nil)
     (t (error "Zoho Desk API %s %s failed (HTTP %s): %s"
               method path status (string-trim body))))))

(defun zoho-desk--ensure-org-id ()
  "Return the orgId header value, fetching organizations if needed."
  (or zoho-desk-org-id
      zoho-desk--cached-org-id
      (let* ((response (zoho-desk--request "GET" "/organizations" :no-org t))
             (org (car (alist-get 'data response))))
        (unless org (error "Zoho Desk: no organizations visible to this token"))
        (setq zoho-desk--cached-org-id (format "%s" (alist-get 'id org))))))

;;;; Async HTTP
;;
;; All reads (views, ticket table, ticket documents, thread bodies) go
;; through `zoho-desk--request-async': commands render a "Fetching …"
;; placeholder immediately and a `url-retrieve' callback fills the
;; buffer in when the response lands, so Emacs never blocks on the
;; network.  Each fetching command bumps a generation counter and its
;; callback checks it, so a stale response can never clobber a newer
;; one.

(defun zoho-desk--ensure-token-async (callback)
  "Call CALLBACK with (TOKEN ERR), refreshing the token if expired."
  (if (and zoho-desk--access-token
           (< (float-time) zoho-desk--token-expiry))
      (funcall callback zoho-desk--access-token nil)
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data
           (url-build-query-string
            `(("refresh_token" ,(zoho-desk--credential
                                 zoho-desk-refresh-token "refresh-token"))
              ("client_id" ,(zoho-desk--credential
                             zoho-desk-client-id "client-id"))
              ("client_secret" ,(zoho-desk--credential
                                 zoho-desk-client-secret "client-secret"))
              ("grant_type" "refresh_token")))))
      (url-retrieve
       (concat zoho-desk-accounts-url "/oauth/v2/token")
       (lambda (_status)
         (let* ((response (condition-case err
                              (zoho-desk--parse-json
                               (zoho-desk--response-body))
                            (error `((error . ,(error-message-string err))))))
                (token (alist-get 'access_token response)))
           (kill-buffer)
           (if (not token)
               (funcall callback nil
                        (format "token refresh failed: %S" response))
             (setq zoho-desk--access-token token
                   zoho-desk--token-expiry
                   (+ (float-time)
                      (- (or (alist-get 'expires_in response) 3600) 60)))
             (funcall callback token nil))))
       nil t t))))

(defun zoho-desk--ensure-org-id-async (callback)
  "Call CALLBACK with (ORG-ID ERR), fetching organizations if needed."
  (if-let* ((cached (or zoho-desk-org-id zoho-desk--cached-org-id)))
      (funcall callback cached nil)
    (zoho-desk--request-async
     "GET" "/organizations"
     (lambda (response err)
       (if err
           (funcall callback nil err)
         (if-let* ((org (car (alist-get 'data response))))
             (funcall callback
                      (setq zoho-desk--cached-org-id
                            (format "%s" (alist-get 'id org)))
                      nil)
           (funcall callback nil "no organizations visible to this token"))))
     :no-org t)))

(cl-defun zoho-desk--request-async (method path callback
                                           &key params payload raw-payload
                                           content-type no-org raw (retries 1))
  "Perform METHOD PATH in the background; CALLBACK gets (RESULT ERR).
The non-blocking counterpart of `zoho-desk--request'; the keyword
arguments mean the same.  CALLBACK is invoked exactly once, with
the parsed JSON response and nil, or with nil and an error message
string.  With RAW non-nil the response body is handed over as
undecoded bytes instead of parsed JSON (for image downloads).  It
may run in an arbitrary buffer, so it must `with-current-buffer'
its target."
  (let ((opts (list :params params :payload payload
                    :raw-payload raw-payload :content-type content-type
                    :no-org no-org :raw raw :retries retries)))
    (zoho-desk--ensure-token-async
     (lambda (token token-err)
       (cond
        (token-err (funcall callback nil token-err))
        (no-org (zoho-desk--dispatch-async method path callback token nil
                                           opts))
        (t (zoho-desk--ensure-org-id-async
            (lambda (org-id org-err)
              (if org-err
                  (funcall callback nil org-err)
                (zoho-desk--dispatch-async method path callback token org-id
                                           opts))))))))))

(defun zoho-desk--dispatch-async (method path callback token org-id opts)
  "Fire the `url-retrieve' behind `zoho-desk--request-async'.
OPTS is that function's keyword plist.  Handles the 401-retry, a
`zoho-desk-request-timeout' watchdog (async retrievals have no
built-in timeout) and guarantees CALLBACK runs exactly once."
  (let* ((params (plist-get opts :params))
         (payload (plist-get opts :payload))
         (raw-payload (plist-get opts :raw-payload))
         (retries (plist-get opts :retries))
         (url-request-method method)
         (url-request-extra-headers
          (zoho-desk--request-headers
           token org-id
           (and (or payload raw-payload)
                (or (plist-get opts :content-type) "application/json"))))
         (url-request-data
          (or raw-payload
              (when payload
                (encode-coding-string (json-encode payload) 'utf-8))))
         (finished nil)
         (finish (lambda (result err)
                   (unless finished
                     (setq finished t)
                     (funcall callback result err))))
         (request-buffer
          (condition-case err
              (url-retrieve
               (zoho-desk--request-url path params)
               (lambda (status)
                 (let ((code url-http-response-status)
                       (net-error (plist-get status :error))
                       (body (if (plist-get opts :raw)
                                 (zoho-desk--response-body-raw)
                               (zoho-desk--response-body))))
                   (kill-buffer)
                   (cond
                    (finished)
                    ;; Check this before net-error: url-http reports a
                    ;; 401 as a network error when an Authorization
                    ;; header was already present (Bug#50511), and the
                    ;; retry must still refresh the stale token.
                    ((and (eq code 401) (> retries 0))
                     (setq finished t
                           zoho-desk--access-token nil)
                     (apply #'zoho-desk--request-async method path callback
                            (plist-put (copy-sequence opts)
                                       :retries (1- retries))))
                    (net-error
                     (funcall finish nil (format "%s %s: %S"
                                                 method path net-error)))
                    ((memq code '(200 201))
                     (if (plist-get opts :raw)
                         (funcall finish body nil)
                       (condition-case err
                           (funcall finish
                                    (unless (string-empty-p (string-trim body))
                                      (zoho-desk--parse-json body))
                                    nil)
                         (error (funcall finish nil
                                         (error-message-string err))))))
                    ((eq code 204) (funcall finish nil nil))
                    (t (funcall finish
                                nil
                                (format "API %s %s failed (HTTP %s): %s"
                                        method path code
                                        (string-trim body)))))))
               nil t t)
            (error (funcall finish nil (error-message-string err))
                   nil))))
    (when (and request-buffer zoho-desk-request-timeout)
      (run-at-time zoho-desk-request-timeout nil
                   (lambda ()
                     (unless finished
                       (setq finished t)
                       (when (buffer-live-p request-buffer)
                         (when-let* ((proc (get-buffer-process
                                            request-buffer)))
                           (delete-process proc))
                         (kill-buffer request-buffer))
                       (funcall callback
                                nil (format "request timed out: %s %s"
                                            method path))))))))

(defun zoho-desk--request-all-async (specs callback)
  "Run request SPECS concurrently; CALLBACK gets (RESULTS ERR).
Each spec is a list (METHOD PATH KEYWORDS...) as accepted by
`zoho-desk--request-async', plus the extra keyword :soft-errors,
which turns that request's failure into a nil result instead of
failing the batch.  RESULTS preserves the order of SPECS.
CALLBACK is invoked exactly once, on the first hard error or once
every request has answered."
  (if (null specs)
      (funcall callback nil nil)
    (let* ((results (make-vector (length specs) nil))
           (pending (length specs))
           (failed nil))
      (seq-do-indexed
       (lambda (spec index)
         (pcase-let* ((`(,method ,path . ,keys) spec)
                      (soft (plist-get keys :soft-errors))
                      (keys (cl-loop for (key value) on keys by #'cddr
                                     unless (eq key :soft-errors)
                                     append (list key value))))
           (apply #'zoho-desk--request-async method path
                  (lambda (result err)
                    (cond
                     (failed)
                     ((and err (not soft))
                      (setq failed t)
                      (funcall callback nil err))
                     (t
                      (aset results index (unless err result))
                      (when (zerop (cl-decf pending))
                        (funcall callback (append results nil) nil)))))
                  keys)))
       specs))))

(defun zoho-desk--announce-write-failure (what err &optional buf)
  "Loudly announce that the background write WHAT failed with ERR.
Writes are fire-and-forget, so failure must pull the user back:
when BUF is live it is refocused (it still holds the unsent
content, ready to retry)."
  (when (buffer-live-p buf)
    (pop-to-buffer buf))
  (ding)
  (message "%s" (propertize (format "Zoho Desk: %s failed: %s" what err)
                            'face 'error)))

;;;; Helpers

(defvar-local zoho-desk--ticket nil
  "Full ticket alist shown in a ticket buffer.")

(defun zoho-desk--fmt-time (iso)
  "Format ISO timestamp for display."
  (if (and (stringp iso) (not (string-empty-p iso)))
      (format-time-string "%b %d %H:%M" (date-to-time iso))
    "-"))

(defun zoho-desk--org-timestamp (iso)
  "Format ISO timestamp as an inactive org timestamp."
  (if (and (stringp iso) (not (string-empty-p iso)))
      (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time iso))
    "[unknown]"))

(defun zoho-desk--person-name (person &optional fallback)
  "Return a display name from a PERSON alist, or FALLBACK."
  (or (and person
           (let ((joined (string-join
                          (delq nil (list (alist-get 'firstName person)
                                          (alist-get 'lastName person)))
                          " ")))
             (unless (string-empty-p joined) joined)))
      (and person (alist-get 'name person))
      fallback "-"))

(defun zoho-desk--contact-name (ticket)
  "Return the contact display name of TICKET."
  (zoho-desk--person-name (alist-get 'contact ticket)))

(defun zoho-desk--html-to-text (html)
  "Render HTML to plain text."
  (cond
   ((not (stringp html)) "")
   ((fboundp 'libxml-parse-html-region)
    (with-temp-buffer
      (insert html)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (erase-buffer)
        (let ((shr-width 76)
              (shr-use-fonts nil)
              (shr-inhibit-images t))
          (shr-insert-document dom)))
      (buffer-substring-no-properties (point-min) (point-max))))
   (t (replace-regexp-in-string "<[^>]*>" "" html))))

(defun zoho-desk--wrap-pre-code (html)
  "Wrap bare <pre> contents in <code> tags in HTML.
Pandoc only reads <pre><code> as a code block; a bare <pre> (what
ox-html and most mail clients emit) collapses to a plain
paragraph.  The tag is rebuilt with only the block's language,
recovered from an ox-html-style \"src-LANG\" class (Zoho mangles
it to \"x_NNNsrc-LANG\") — every other attribute is dropped, so
inline styles cannot leak into the org block's header line."
  (replace-regexp-in-string
   "</pre>" "</code></pre>"
   (replace-regexp-in-string
    "<pre\\([^>]*\\)>"
    (lambda (tag)
      ;; save-match-data: replace-regexp-in-string needs its own
      ;; match data intact after this function returns.
      (save-match-data
        (if (string-match "class=\"[^\"]*src-\\([A-Za-z0-9_+-]+\\)" tag)
            (format "<pre class=\"%s\"><code>" (match-string 1 tag))
          "<pre><code>")))
    html)))

(defun zoho-desk--strip-org-linebreaks (org)
  "Remove the trailing \\\\ hard line breaks pandoc makes of <br>.
The newline stays, so the visual line break survives; only inside
verbatim blocks (example, src, export) are the backslashes left
alone, since there they are content.  Markup blocks like quote
and center hold prose, so theirs are stripped too."
  (with-temp-buffer
    (insert org)
    (goto-char (point-min))
    (let ((literal nil))
      (while (not (eobp))
        (cond
         ((looking-at-p "[ \t]*#\\+begin_\\(example\\|src\\|export\\)\\_>")
          (setq literal t))
         ((looking-at-p "[ \t]*#\\+end_\\(example\\|src\\|export\\)\\_>")
          (setq literal nil))
         ((and (not literal)
               (re-search-forward "\\\\\\\\$" (line-end-position) t))
          (replace-match "")))
        (forward-line 1)))
    (buffer-string)))

(defun zoho-desk--html-to-org (html)
  "Convert HTML to org markup via `zoho-desk-pandoc-program'.
Falls back to the plain-text rendering when pandoc is disabled,
missing, or chokes on the input."
  (or (and (stringp html)
           zoho-desk-pandoc-program
           (executable-find zoho-desk-pandoc-program)
           (with-temp-buffer
             (insert (zoho-desk--wrap-pre-code html))
             (let ((coding-system-for-write 'utf-8)
                   (coding-system-for-read 'utf-8))
               (when (zerop (call-process-region
                             (point-min) (point-max)
                             zoho-desk-pandoc-program t '(t nil) nil
                             "-f" "html-auto_identifiers" "-t" "org"))
                 (zoho-desk--strip-org-linebreaks
                  (buffer-substring-no-properties (point-min)
                                                  (point-max)))))))
      (zoho-desk--html-to-text html)))

(defun zoho-desk--org-body (text)
  "Indent TEXT two spaces so it can never form org headings.
Trailing whitespace is stripped and empty lines stay truly empty."
  (let ((clean (replace-regexp-in-string
                "[ \t]+$" "" (string-trim (or text "")))))
    (concat (replace-regexp-in-string "^." "  \\&" clean) "\n")))

(defun zoho-desk--html-to-org-body (html)
  "Convert HTML to indented org markup usable as an org entry body."
  (zoho-desk--org-body (zoho-desk--html-to-org html)))

(defun zoho-desk--ticket-at-point ()
  "Return the ticket alist relevant to the current buffer/point."
  (cond
   ((derived-mode-p 'zoho-desk-tickets-mode)
    (or (tabulated-list-get-id) (user-error "No ticket on this line")))
   ((bound-and-true-p zoho-desk-ticket-minor-mode)
    zoho-desk--ticket)
   (t nil)))

(defvar zoho-desk--department nil
  "Department id chosen interactively for this session.")
(defvar zoho-desk--departments nil
  "Cached list of department alists.")

(defun zoho-desk--department-params ()
  "Return departmentId query params when a department is known."
  (when-let* ((department (or zoho-desk-department-id zoho-desk--department)))
    `(("departmentId" ,department))))

(defun zoho-desk--ensure-departments ()
  "Return the cached department alists, fetching them if needed."
  (or zoho-desk--departments
      (setq zoho-desk--departments
            (alist-get 'data (zoho-desk--request
                              "GET" "/departments"
                              :params '(("limit" 50)))))))

(defun zoho-desk--read-department ()
  "Prompt for a department; set and return its id."
  (let* ((departments (zoho-desk--ensure-departments))
         (names (mapcar (lambda (d) (alist-get 'name d)) departments))
         (choice (completing-read "Zoho department: " names nil t))
         (department (seq-find (lambda (d)
                                 (equal (alist-get 'name d) choice))
                               departments)))
    (setq zoho-desk--department (format "%s" (alist-get 'id department)))
    (message "Using department %s (%s) — setq zoho-desk-department-id to skip this prompt"
             choice zoho-desk--department)
    zoho-desk--department))

(defun zoho-desk--ensure-department ()
  "Return the department id to use, prompting once when unset."
  (or zoho-desk-department-id
      zoho-desk--department
      (zoho-desk--read-department)))

;;;; Dashboard state

(defvar zoho-desk--views nil
  "Cached list of ticket views (alists).")
(defvar zoho-desk--selected-view-ids nil
  "Ids of the checked views; the table shows the union of their tickets.")
(defvar zoho-desk--view-counts (make-hash-table :test #'equal)
  "Fetched-ticket counts per view id: (COUNT . MORE-P).")
(defvar zoho-desk--from 0
  "Current pagination offset, applied to every selected view.")
(defvar zoho-desk--saved-window-configuration nil)

(defconst zoho-desk--views-buffer-name "*zoho-views*")
(defconst zoho-desk--tickets-buffer-name "*zoho-tickets*")

(defvar zoho-desk--ticket-buffer nil
  "Single buffer reused for ticket details, renamed per ticket.")

(defun zoho-desk--starred-position (view)
  "Return VIEW's position in `zoho-desk-starred-view-names', or nil."
  (seq-position (mapcar #'downcase zoho-desk-starred-view-names)
                (downcase (or (alist-get 'name view) ""))))

(defun zoho-desk--combine-views (api-starred all)
  "Order views: starred ones first (tagged `starred'), then the rest.
Starred views are those in API-STARRED (from /starredViews) plus
any view in ALL whose name appears in
`zoho-desk-starred-view-names' (the public API does not expose the
web UI's stars)."
  (let* ((api-starred-ids (mapcar (lambda (v) (alist-get 'id v)) api-starred))
         (name-starred
          (sort (seq-filter
                 (lambda (v)
                   (and (zoho-desk--starred-position v)
                        (not (member (alist-get 'id v) api-starred-ids))))
                 all)
                (lambda (a b) (< (zoho-desk--starred-position a)
                                 (zoho-desk--starred-position b)))))
         (starred (append api-starred name-starred))
         (starred-ids (mapcar (lambda (v) (alist-get 'id v)) starred)))
    (append
     (mapcar (lambda (v) (cons '(starred . t) v)) starred)
     (seq-remove (lambda (v) (member (alist-get 'id v) starred-ids))
                 all))))

(defvar zoho-desk--views-generation 0
  "Bumped per views fetch so a stale response cannot win.")

(defun zoho-desk--fetch-views-async (callback)
  "Fetch ticket views in the background; CALLBACK gets (VIEWS ERR)."
  (let ((generation (cl-incf zoho-desk--views-generation))
        (params (append '(("module" "tickets"))
                        (zoho-desk--department-params))))
    (zoho-desk--request-all-async
     `(("GET" "/starredViews" :params ,params :soft-errors t)
       ("GET" "/views" :params ,(append params '(("limit" 100)))
        :soft-errors t))
     (lambda (results err)
       (when (= generation zoho-desk--views-generation)
         (if err
             (funcall callback nil err)
           (if-let* ((views (zoho-desk--combine-views
                             (alist-get 'data (nth 0 results))
                             (alist-get 'data (nth 1 results)))))
               (funcall callback views nil)
             (funcall callback nil "no ticket views found"))))))))

(defun zoho-desk--default-view ()
  "Return the view checked on first open: the starred \"Open\" view.
Falls back to the first starred view, then the first view."
  (let ((starred (seq-filter (lambda (v) (alist-get 'starred v))
                             zoho-desk--views)))
    (or (seq-find (lambda (v)
                    (string= (downcase (or (alist-get 'name v) "")) "open"))
                  (or starred zoho-desk--views))
        (car starred)
        (car zoho-desk--views))))

(defun zoho-desk--view-name (view-id)
  "Return the name of the cached view VIEW-ID."
  (or (alist-get 'name
                 (seq-find (lambda (v) (equal (alist-get 'id v) view-id))
                           zoho-desk--views))
      view-id))

;;;; Sidebar pane

(defvar zoho-desk-views-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zoho-desk-toggle-view-at-point)
    (define-key map (kbd "SPC") #'zoho-desk-toggle-view-at-point)
    (define-key map (kbd "x") #'zoho-desk-toggle-view-at-point)
    (define-key map (kbd "<mouse-1>") #'zoho-desk-mouse-toggle-view)
    (define-key map (kbd "g") #'zoho-desk-refresh-views)
    (define-key map (kbd "D") #'zoho-desk-select-department)
    (define-key map (kbd "s") #'zoho-desk-toggle-sidebar)
    (define-key map (kbd "q") #'zoho-desk-quit)
    map))

(define-derived-mode zoho-desk-views-mode special-mode "ZohoViews"
  "Sidebar listing Zoho Desk views as OR-filter checkboxes.

\\{zoho-desk-views-mode-map}"
  (setq truncate-lines t)
  (setq-local window-size-fixed 'width))

(defun zoho-desk--insert-view-row (view)
  "Insert one checkbox row for VIEW."
  (let* ((id (alist-get 'id view))
         (active (member id zoho-desk--selected-view-ids))
         (count (gethash id zoho-desk--view-counts))
         (start (point)))
    (insert (format "%s %-20s %s\n"
                    (if active "[x]" "[ ]")
                    (truncate-string-to-width
                     (or (alist-get 'name view) "?") 20 nil nil "…")
                    (if count
                        (format "%d%s" (car count)
                                (if (cdr count) "+" ""))
                      "")))
    (add-text-properties start (point)
                         `(zoho-desk-view ,id
                           mouse-face highlight
                           face ,(if active 'zoho-desk-accent 'shadow)))))

(defun zoho-desk--render-views ()
  "Render starred views, then the remaining views, into the sidebar."
  (with-current-buffer (get-buffer-create zoho-desk--views-buffer-name)
    (unless (derived-mode-p 'zoho-desk-views-mode)
      (zoho-desk-views-mode))
    (let ((inhibit-read-only t)
          (line (line-number-at-pos))
          (starred (seq-filter (lambda (v) (alist-get 'starred v))
                               zoho-desk--views))
          (rest (seq-remove (lambda (v) (alist-get 'starred v))
                            zoho-desk--views)))
      (erase-buffer)
      (if (null zoho-desk--views)
          (insert (propertize "Fetching views…\n" 'face 'shadow))
        (insert (propertize "Starred Views (RET toggles, OR)\n\n" 'face 'bold))
        (if starred
            (mapc #'zoho-desk--insert-view-row starred)
          (insert (propertize "  none\n" 'face 'shadow)))
        (when rest
          (insert "\n" (propertize "All Views\n\n" 'face 'bold))
          (mapc #'zoho-desk--insert-view-row rest)))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun zoho-desk-toggle-view-at-point ()
  "Toggle the view checkbox at point and refresh the ticket table."
  (interactive)
  (let ((id (get-text-property (point) 'zoho-desk-view)))
    (unless id (user-error "No view on this line"))
    (setq zoho-desk--selected-view-ids
          (if (member id zoho-desk--selected-view-ids)
              (delete id zoho-desk--selected-view-ids)
            (append zoho-desk--selected-view-ids (list id)))
          zoho-desk--from 0)
    (zoho-desk--refresh-table)
    (zoho-desk--render-views)))

(defun zoho-desk-mouse-toggle-view (event)
  "Toggle the view clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (zoho-desk-toggle-view-at-point))

(defun zoho-desk-select-department ()
  "Switch the dashboard to another department."
  (interactive)
  (zoho-desk--read-department)
  (setq zoho-desk--views nil
        zoho-desk--selected-view-ids nil
        zoho-desk--from 0)
  (clrhash zoho-desk--view-counts)
  (zoho-desk-dashboard))

(defun zoho-desk-refresh-views ()
  "Refetch the view list from Zoho in the background, then redraw."
  (interactive)
  (message "Zoho Desk: refetching views…")
  (zoho-desk--fetch-views-async
   (lambda (views err)
     (if err
         (message "Zoho Desk: %s" err)
       (setq zoho-desk--views views
             zoho-desk--selected-view-ids
             (seq-filter (lambda (id)
                           (seq-find (lambda (v)
                                       (equal (alist-get 'id v) id))
                                     views))
                         zoho-desk--selected-view-ids))
       (zoho-desk--render-views)
       (zoho-desk--refresh-table)))))

;;;; Ticket table pane

(defvar zoho-desk-tickets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zoho-desk-open-ticket-at-point)
    (define-key map (kbd "g") #'zoho-desk-refresh-table)
    (define-key map (kbd "t") #'zoho-desk-add-time-entry)
    (define-key map (kbd "T") #'zoho-desk-start-ticket-timer)
    (define-key map (kbd "u") #'zoho-desk-set-status)
    (define-key map (kbd "w") #'zoho-desk-copy-org-snippet)
    (define-key map (kbd "o") #'zoho-desk-browse-ticket)
    (define-key map (kbd "]") #'zoho-desk-next-page)
    (define-key map (kbd "[") #'zoho-desk-previous-page)
    (define-key map (kbd "s") #'zoho-desk-toggle-sidebar)
    (define-key map (kbd "q") #'zoho-desk-quit)
    map))

(define-derived-mode zoho-desk-tickets-mode tabulated-list-mode "ZohoTickets"
  "Major mode listing Zoho Desk tickets.

\\{zoho-desk-tickets-mode-map}"
  (setq tabulated-list-format
        [("Ticket" 10 t)
         ("Pri" 8 t)
         ("Status" 12 t)
         ("Due" 14 t)
         ("Contact" 20 t)
         ("Subject" 60 t)]
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun zoho-desk--ticket-entry (ticket)
  "Convert TICKET alist into a `tabulated-list-entries' element."
  (let* ((due (alist-get 'dueDate ticket))
         (overdue (and (stringp due)
                       (time-less-p (date-to-time due) (current-time)))))
    (list ticket
          (vector
           (propertize (concat "#" (or (alist-get 'ticketNumber ticket) "?"))
                       'face 'zoho-desk-accent)
           (or (alist-get 'priority ticket) "-")
           (or (alist-get 'status ticket) "-")
           (propertize (zoho-desk--fmt-time due)
                       'face (if overdue 'error 'default))
           (zoho-desk--contact-name ticket)
           (or (alist-get 'subject ticket) "")))))

(defvar zoho-desk--table-generation 0
  "Bumped per table fetch so a stale response cannot win.")

(defun zoho-desk--table-mode-line (suffix)
  "Return the ticket table's mode-line-process string, plus SUFFIX."
  (format " [%s%s%s]"
          (if zoho-desk--selected-view-ids
              (mapconcat #'zoho-desk--view-name
                         zoho-desk--selected-view-ids " ∪ ")
            "no views selected")
          (if (> zoho-desk--from 0)
              (format ", from %d" zoho-desk--from)
            "")
          suffix))

(defun zoho-desk--merge-ticket-pages (view-ids from pages)
  "Merge ticket PAGES fetched per view, newest change first.
VIEW-IDS and PAGES run in parallel; per-view fetched counts
starting at FROM are recorded in `zoho-desk--view-counts'."
  (let ((seen (make-hash-table :test #'equal))
        (tickets nil))
    (cl-mapc
     (lambda (view-id response)
       (let ((page (alist-get 'data response)))
         (puthash view-id
                  (cons (+ from (length page))
                        (= (length page) zoho-desk-page-size))
                  zoho-desk--view-counts)
         (dolist (ticket page)
           (let ((id (alist-get 'id ticket)))
             (unless (gethash id seen)
               (puthash id t seen)
               (push ticket tickets))))))
     view-ids pages)
    (sort (nreverse tickets)
          (lambda (a b)
            (string-greaterp
             (or (alist-get 'modifiedTime a) (alist-get 'createdTime a) "")
             (or (alist-get 'modifiedTime b) (alist-get 'createdTime b) ""))))))

(defun zoho-desk--refresh-table ()
  "Refresh the ticket table from the selected views, in the background.
Existing rows stay visible while the new page is fetched; the
mode line shows the fetch in flight."
  (interactive)
  (with-current-buffer (get-buffer-create zoho-desk--tickets-buffer-name)
    (unless (derived-mode-p 'zoho-desk-tickets-mode)
      (zoho-desk-tickets-mode))
    (let ((generation (cl-incf zoho-desk--table-generation))
          (buf (current-buffer))
          (view-ids zoho-desk--selected-view-ids)
          (from zoho-desk--from))
      (if (null view-ids)
          (progn
            (setq tabulated-list-entries nil
                  mode-line-process (zoho-desk--table-mode-line ""))
            (tabulated-list-print t)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize
                       "\n  Check a view in the sidebar (RET) to list tickets.\n"
                       'face 'shadow))))
        (setq mode-line-process (zoho-desk--table-mode-line ", fetching…"))
        (force-mode-line-update)
        (when (null tabulated-list-entries)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "\n  Fetching tickets…\n" 'face 'shadow))))
        (zoho-desk--request-all-async
         (mapcar (lambda (view-id)
                   `("GET" "/tickets"
                     :params ,(append `(("viewId" ,view-id)
                                        ("include" "contacts,assignee")
                                        ("limit" ,zoho-desk-page-size)
                                        ("from" ,from))
                                      (zoho-desk--department-params))))
                 view-ids)
         (lambda (pages err)
           (when (and (= generation zoho-desk--table-generation)
                      (buffer-live-p buf))
             (with-current-buffer buf
               (if err
                   (progn
                     (setq mode-line-process
                           (zoho-desk--table-mode-line ", fetch failed"))
                     (force-mode-line-update)
                     (message "Zoho Desk: %s" err))
                 (setq tabulated-list-entries
                       (mapcar #'zoho-desk--ticket-entry
                               (zoho-desk--merge-ticket-pages view-ids from
                                                              pages))
                       mode-line-process (zoho-desk--table-mode-line ""))
                 (tabulated-list-print t)
                 ;; Redraw the sidebar so the per-view counts update.
                 (zoho-desk--render-views))))))))))

(defun zoho-desk-refresh-table ()
  "Interactive alias for refreshing the ticket table."
  (interactive)
  (zoho-desk--refresh-table)
  (zoho-desk--render-views))

(defun zoho-desk-next-page ()
  "Show the next page of tickets."
  (interactive)
  (setq zoho-desk--from (+ zoho-desk--from zoho-desk-page-size))
  (zoho-desk-refresh-table))

(defun zoho-desk-previous-page ()
  "Show the previous page of tickets."
  (interactive)
  (setq zoho-desk--from (max 0 (- zoho-desk--from zoho-desk-page-size)))
  (zoho-desk-refresh-table))

(defun zoho-desk-browse-ticket ()
  "Open the ticket at point in the browser."
  (interactive)
  (let ((ticket (zoho-desk--ticket-at-point)))
    (if-let* ((url (alist-get 'webUrl ticket)))
        (browse-url url)
      (user-error "No web URL on this ticket"))))

(defun zoho-desk--agent-ticket-url (ticket)
  "Return the agent-console URL of TICKET, or nil when underivable.
The API's webUrl field carries the customer-portal form
\(<host>/support/<portal>/ShowHomePage.do#...); the agent console
lives at <host>/agent/<portal>/<department>/tickets/details/<id>,
with the department's sanitizedName as the URL slug."
  (when-let* ((base-portal (zoho-desk--ticket-portal-base ticket))
              (id (alist-get 'id ticket))
              (dept-id (alist-get 'departmentId ticket))
              (dept (seq-find
                     (lambda (d)
                       (equal (format "%s" (alist-get 'id d))
                              (format "%s" dept-id)))
                     (zoho-desk--ensure-departments)))
              (slug (alist-get 'sanitizedName dept)))
    (format "%s/agent/%s/%s/tickets/details/%s"
            (car base-portal) (cdr base-portal) slug id)))

(defun zoho-desk-copy-ticket-number ()
  "Copy the ticket number at point (the \"#ART-364\" form)."
  (interactive)
  (let* ((ticket (or (zoho-desk--ticket-at-point)
                     (user-error "No ticket in context")))
         (number (format "#%s" (alist-get 'ticketNumber ticket))))
    (kill-new number)
    (message "%s copied to kill ring" number)))

(defun zoho-desk-copy-ticket-url ()
  "Copy the agent-console URL of the ticket at point.
Falls back to the customer-portal webUrl when the agent form
cannot be derived."
  (interactive)
  (let* ((ticket (or (zoho-desk--ticket-at-point)
                     (user-error "No ticket in context")))
         (url (or (zoho-desk--agent-ticket-url ticket)
                  (alist-get 'webUrl ticket)
                  (user-error "No web URL on this ticket"))))
    (kill-new url)
    (message "%s copied to kill ring" url)))

;;;; Opening a ticket from its URL

(defvar zoho-desk--ticket-url-history nil
  "Minibuffer history for `zoho-desk-open-ticket-url'.")

(defun zoho-desk--ticket-id-from-url (url)
  "Return the ticket id embedded in URL, or nil.
Recognizes the agent-console form (…/tickets/details/<id>) and
falls back to the first run of 15 or more digits, which covers
portal links and anything else carrying the raw ticket id."
  (cond
   ((string-match "/tickets/details/\\([0-9]+\\)" url)
    (match-string 1 url))
   ((string-match "\\([0-9]\\{15,\\}\\)" url)
    (match-string 1 url))))

(defun zoho-desk--department-from-url (url)
  "Return the department alist named by URL's agent-console slug, or nil.
The slug between the portal name and /tickets/ is the
department's sanitizedName, the same field
`zoho-desk--agent-ticket-url' writes."
  (when (string-match "/agent/[^/]+/\\([^/]+\\)/tickets/" url)
    (let ((slug (match-string 1 url)))
      (seq-find (lambda (d)
                  (equal (alist-get 'sanitizedName d) slug))
                (zoho-desk--ensure-departments)))))

(defun zoho-desk--ticket-url-in-kill-ring ()
  "Return the newest kill when it looks like a Zoho ticket URL."
  (when-let* ((kill (ignore-errors
                      (substring-no-properties (current-kill 0 t)))))
    (let ((kill (string-trim kill)))
      (and (string-match-p "\\`https?://" kill)
           (zoho-desk--ticket-id-from-url kill)
           kill))))

(defun zoho-desk--switch-department (department-id)
  "Make DEPARTMENT-ID the session department unless it already is.
Resets the dashboard's per-department state and, when the
dashboard is on screen, redraws it for the new department."
  (let ((id (format "%s" department-id)))
    (unless (equal id (format "%s" (or zoho-desk-department-id
                                       zoho-desk--department
                                       "")))
      (setq zoho-desk--department id)
      ;; The defcustom outranks the session variable in
      ;; `zoho-desk--department-params', so when one is configured it
      ;; must follow the switch too (this session only, never saved).
      (when zoho-desk-department-id
        (setq zoho-desk-department-id id))
      (setq zoho-desk--views nil
            zoho-desk--selected-view-ids nil
            zoho-desk--from 0)
      (clrhash zoho-desk--view-counts)
      (when (get-buffer-window zoho-desk--views-buffer-name)
        (zoho-desk-dashboard))
      (let ((department (seq-find
                         (lambda (d)
                           (equal (format "%s" (alist-get 'id d)) id))
                         zoho-desk--departments)))
        (message "Zoho Desk: switched to department %s"
                 (or (alist-get 'name department) id))))))

;;;###autoload
(defun zoho-desk-open-ticket-url (url)
  "Open the ticket named by URL, switching departments when needed.
URL is read from the minibuffer, prefilled from the kill ring
when the newest kill looks like a ticket URL.  The agent-console
form (…/agent/<portal>/<department>/tickets/details/<id>) names
its department directly; any other URL carrying the ticket's long
numeric id works too, at the cost of one extra lookup to learn
the department from the ticket itself."
  (interactive
   (list (read-string "Zoho ticket URL: "
                      (zoho-desk--ticket-url-in-kill-ring)
                      'zoho-desk--ticket-url-history)))
  (let* ((id (or (zoho-desk--ticket-id-from-url url)
                 (user-error "No ticket id found in %s" url)))
         (department (zoho-desk--department-from-url url)))
    (if department
        (progn
          (zoho-desk--switch-department (alist-get 'id department))
          (zoho-desk--show-ticket id))
      ;; No department slug in the URL: fetch the ticket first so the
      ;; department switch (and any dashboard redraw) happens before
      ;; the ticket window pops up.
      (message "Zoho Desk: looking up ticket %s…" id)
      (zoho-desk--request-async
       "GET" (format "/tickets/%s" id)
       (lambda (ticket err)
         (if err
             (message "Zoho Desk: %s" err)
           (when-let* ((dept-id (alist-get 'departmentId ticket)))
             (zoho-desk--switch-department dept-id))
           (zoho-desk--show-ticket id
                                   (alist-get 'ticketNumber ticket))))))))

;;;; Quickfind

(declare-function helm "ext:helm")
(declare-function helm-make-source "ext:helm-source")
(declare-function helm-make-actions "ext:helm-lib")

(defun zoho-desk--jump-to-ticket-number (number)
  "Look up the ticket whose number is NUMBER and open it.
The search spans all departments; the session department is
switched to the ticket's own when it differs."
  (message "Zoho Desk: looking up #%s…" number)
  (zoho-desk--request-async
   "GET" "/tickets/search"
   (lambda (result err)
     (cond
      (err (message "Zoho Desk: %s" err))
      ((null (alist-get 'data result))
       (message "Zoho Desk: no ticket #%s" number))
      (t (let ((ticket (car (alist-get 'data result))))
           ;; Leave the url.el sentinel context before touching windows.
           (run-at-time
            0 nil
            (lambda ()
              (when-let* ((department (alist-get 'departmentId ticket)))
                (zoho-desk--switch-department department))
              (zoho-desk--show-ticket (alist-get 'id ticket)
                                      (alist-get 'ticketNumber ticket))))))))
   :params `(("ticketNumber" ,number) ("limit" 1))))

(defun zoho-desk--quickfind-tickets-async (department-id callback)
  "Fetch DEPARTMENT-ID's newest tickets for the quickfind pool.
Up to `zoho-desk-quickfind-limit' tickets are fetched as parallel
pages; CALLBACK gets (TICKETS ERR)."
  (zoho-desk--request-all-async
   (mapcar (lambda (from)
             `("GET" "/tickets"
               :params (("departmentId" ,department-id)
                        ("include" "contacts,assignee")
                        ("sortBy" "-createdTime")
                        ("limit" 100)
                        ("from" ,from))
               :soft-errors t))
           (number-sequence 0 (1- (max 100 zoho-desk-quickfind-limit)) 100))
   (lambda (pages err)
     (if err
         (funcall callback nil err)
       (let ((seen (make-hash-table :test #'equal))
             (tickets nil))
         (dolist (page pages)
           (dolist (ticket (alist-get 'data page))
             (let ((id (alist-get 'id ticket)))
               (unless (gethash id seen)
                 (puthash id t seen)
                 (push ticket tickets)))))
         (funcall callback (nreverse tickets) nil))))))

(defun zoho-desk--quickfind-candidate (ticket)
  "Return TICKET's helm candidate as a (DISPLAY . TICKET) pair."
  (cons (format "%-12s %-14s %-20s %s"
                (propertize (format "#%s"
                                    (or (alist-get 'ticketNumber ticket) "?"))
                            'face 'zoho-desk-accent)
                (or (alist-get 'status ticket) "-")
                (truncate-string-to-width (zoho-desk--contact-name ticket)
                                          20 nil nil t)
                (or (alist-get 'subject ticket) ""))
        ticket))

(defun zoho-desk--quickfind-helm (tickets &optional input)
  "Fuzzy-find among TICKETS with helm; INPUT seeds the pattern."
  (require 'helm)
  (helm :sources
        (helm-make-source "Zoho tickets" 'helm-source-sync
          :candidates (mapcar #'zoho-desk--quickfind-candidate tickets)
          :fuzzy-match t
          :candidate-number-limit (max 500 zoho-desk-quickfind-limit)
          :action
          (helm-make-actions
           "Open ticket"
           (lambda (ticket)
             (zoho-desk--show-ticket (alist-get 'id ticket)
                                     (alist-get 'ticketNumber ticket)))
           "Open in browser"
           (lambda (ticket)
             (browse-url (or (zoho-desk--agent-ticket-url ticket)
                             (alist-get 'webUrl ticket)
                             (user-error "No web URL on this ticket"))))))
        :input input
        :prompt "Ticket: "
        :buffer "*helm zoho tickets*"))

;;;###autoload
(defun zoho-desk-quickfind (query)
  "Jump straight to a ticket by number, or helm-fuzzy-find one.
A QUERY of the whole-ticket-number form (\"#ART-364\", \"ART-364\"
or \"#364\") opens that ticket directly, switching departments to
the ticket's own when needed.  Any other QUERY — including none —
prompts for a department and fuzzy-finds over its newest
`zoho-desk-quickfind-limit' tickets, with QUERY as the initial
helm pattern."
  (interactive "sTicket (#ART-364 jumps, anything else fuzzy-finds): ")
  (let ((query (string-trim query)))
    (if (string-match-p "\\`\\(?:#?[A-Za-z]+-[0-9]+\\|#[0-9]+\\)\\'" query)
        (zoho-desk--jump-to-ticket-number (string-remove-prefix "#" query))
      (let* ((departments (zoho-desk--ensure-departments))
             (current-id (format "%s" (or zoho-desk-department-id
                                          zoho-desk--department
                                          "")))
             (default (seq-find (lambda (d)
                                  (equal (format "%s" (alist-get 'id d))
                                         current-id))
                                departments))
             (choice (completing-read
                      "Zoho department: "
                      (mapcar (lambda (d) (alist-get 'name d)) departments)
                      nil t nil nil (alist-get 'name default)))
             (department (seq-find (lambda (d)
                                     (equal (alist-get 'name d) choice))
                                   departments)))
        (zoho-desk--switch-department (alist-get 'id department))
        (message "Zoho Desk: fetching tickets of %s…" choice)
        (zoho-desk--quickfind-tickets-async
         (format "%s" (alist-get 'id department))
         (lambda (tickets err)
           (cond
            (err (message "Zoho Desk: %s" err))
            ((null tickets)
             (message "Zoho Desk: no tickets in %s" choice))
            ;; helm runs its own minibuffer loop; don't start it from
            ;; inside the url.el sentinel.
            (t (run-at-time 0 nil #'zoho-desk--quickfind-helm tickets
                            (unless (string-empty-p query) query))))))))))

;;;; Dashboard layout

;;;###autoload
(defun zoho-desk-dashboard ()
  "Open the Zoho Desk dashboard: views sidebar plus ticket table.
The window layout appears immediately; views and tickets are
fetched in the background and stream in."
  (interactive)
  (zoho-desk--sync-accent-faces)
  (zoho-desk--ensure-department)
  (unless (window-configuration-p zoho-desk--saved-window-configuration)
    (setq zoho-desk--saved-window-configuration (current-window-configuration)))
  (delete-other-windows)
  (let* ((sidebar (selected-window))
         (table (split-window sidebar zoho-desk-sidebar-width 'right)))
    (zoho-desk--render-views)
    (with-current-buffer (get-buffer-create zoho-desk--tickets-buffer-name)
      (unless (derived-mode-p 'zoho-desk-tickets-mode)
        (zoho-desk-tickets-mode)))
    (set-window-buffer sidebar (get-buffer zoho-desk--views-buffer-name))
    (set-window-buffer table (get-buffer zoho-desk--tickets-buffer-name))
    (set-window-dedicated-p sidebar t)
    (select-window table)
    (if zoho-desk--views
        (zoho-desk--refresh-table)
      (zoho-desk--fetch-views-async
       (lambda (views err)
         (if err
             (message "Zoho Desk: %s" err)
           (setq zoho-desk--views views)
           ;; Start with only the starred "Open" view checked.
           (unless zoho-desk--selected-view-ids
             (setq zoho-desk--selected-view-ids
                   (list (alist-get 'id (zoho-desk--default-view)))))
           (zoho-desk--render-views)
           (zoho-desk--refresh-table)))))))

;;;###autoload
(defalias 'zoho-desk-tickets #'zoho-desk-dashboard)

(defun zoho-desk-quit ()
  "Close the dashboard and restore the previous window layout."
  (interactive)
  (when (window-configuration-p zoho-desk--saved-window-configuration)
    (set-window-configuration zoho-desk--saved-window-configuration))
  (setq zoho-desk--saved-window-configuration nil))

(defun zoho-desk-toggle-sidebar ()
  "Collapse the views sidebar, or reopen it when it is hidden.
The ticket table keeps its filters either way; reopening splits
the sidebar back off the table window at `zoho-desk-sidebar-width'."
  (interactive)
  (if-let* ((sidebar (get-buffer-window zoho-desk--views-buffer-name)))
      (delete-window sidebar)
    (let* ((table (or (get-buffer-window zoho-desk--tickets-buffer-name)
                      (user-error "The Zoho Desk dashboard is not open")))
           ;; Negative SIZE sizes the new window, not the table.
           (sidebar (split-window table (- zoho-desk-sidebar-width) 'left)))
      (zoho-desk--render-views)
      (set-window-buffer sidebar (get-buffer zoho-desk--views-buffer-name))
      (set-window-dedicated-p sidebar t))))

;;;; Ticket document (org-mode)

(defvar zoho-desk-ticket-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c z 1") #'zoho-desk-tab-overview)
    (define-key map (kbd "C-c z 2") #'zoho-desk-tab-thread)
    (define-key map (kbd "C-c z 3") #'zoho-desk-tab-comments)
    (define-key map (kbd "C-c z 4") #'zoho-desk-tab-time-logs)
    (define-key map (kbd "C-c z s") #'zoho-desk-send-reply)
    (define-key map (kbd "C-c z l") #'zoho-desk-submit-time-log)
    (define-key map (kbd "C-c z a") #'zoho-desk-add-email)
    (define-key map (kbd "C-c z i") #'zoho-desk-insert-image)
    (define-key map (kbd "C-c z k") #'zoho-desk-refresh-session-cookie)
    (define-key map (kbd "C-c z e") #'zoho-desk-expand-thread-at-point)
    (define-key map (kbd "C-c z c") #'zoho-desk-add-comment)
    (define-key map (kbd "C-c z m") #'zoho-desk-submit-comment)
    (define-key map (kbd "C-c z t") #'zoho-desk-add-time-entry)
    (define-key map (kbd "C-c z T") #'zoho-desk-start-ticket-timer)
    (define-key map (kbd "C-c z u") #'zoho-desk-set-status)
    (define-key map (kbd "C-c z g") #'zoho-desk-refresh-ticket)
    (define-key map (kbd "C-c z o") #'zoho-desk-browse-ticket)
    (define-key map (kbd "C-c z w") #'zoho-desk-copy-org-snippet)
    (define-key map (kbd "C-c z y") #'zoho-desk-copy-ticket-url)
    (define-key map (kbd "C-c z #") #'zoho-desk-copy-ticket-number)
    ;; TAB cycles the input fields when point is in one and keeps its
    ;; org folding role everywhere else; C-c . opens the org date
    ;; picker on the Executed field.
    (define-key map (kbd "M-n") #'zoho-desk-next-field)
    (define-key map (kbd "M-p") #'zoho-desk-previous-field)
    (define-key map (kbd "C-c .") #'zoho-desk-org-timestamp-dwim)
    ;; Plain self-insert everywhere except the New Comment field,
    ;; where @ completes an agent mention.
    (define-key map (kbd "@") #'zoho-desk-electric-mention)
    map))

(defvar-local zoho-desk--current-tab nil
  "Heading of the currently narrowed tab, nil for Overview.")

(defvar-local zoho-desk--tab-padding-overlay nil
  "Overlay drawing blank-line padding above a narrowed tab's heading.")

(defconst zoho-desk--tabs
  '(("Overview" . nil)
    ("Thread" . "Email Thread")
    ("Comments" . "Comments")
    ("Time Logs" . "Time Logs"))
  "Tab labels and the top-level org heading each narrows to.")

(defvar-local zoho-desk--threads nil
  "Thread list of the ticket in this buffer, newest first.")

(defun zoho-desk--input-field-matcher (limit)
  "Font-lock matcher for the input field backgrounds.
Matches the next stretch of the To address, reply body, comment
body, Executed or End time, duration values or time log
description before LIMIT.
Registered with the `append' override, so faces org has already
applied — src block and quote backgrounds included — keep
precedence over the field background."
  (let* ((to (zoho-desk--reply-to-field))
         (executed (zoho-desk--time-executed-field))
         (end (zoho-desk--time-end-field))
         (fields (delq nil
                       ;; The full-line fields take in their line's
                       ;; newline so the `:extend' background runs to
                       ;; the window edge; the duration values only
                       ;; paint their digits, and the body fields
                       ;; already end with their own newline, just
                       ;; before the end separator's.
                       (list (and to (cons (car to) (1+ (cdr to))))
                             (zoho-desk--reply-body-field)
                             (zoho-desk--comment-body-field)
                             (and executed
                                  (cons (car executed) (1+ (cdr executed))))
                             (and end (cons (car end) (1+ (cdr end))))
                             (zoho-desk--duration-field 'hours)
                             (zoho-desk--duration-field 'minutes)
                             (zoho-desk--duration-field 'seconds)
                             (zoho-desk--time-body-field))))
         hit)
    (dolist (field fields)
      (let ((start (max (point) (car field)))
            (end (min limit (cdr field))))
        (when (and (< start end)
                   (or (null hit) (< start (car hit))))
          (setq hit (cons start end)))))
    (when hit
      (set-match-data (list (car hit) (cdr hit)))
      (goto-char (cdr hit))
      t)))

(defun zoho-desk--status-badge-matcher (limit)
  "Font-lock matcher for the status badge heading text before LIMIT."
  (let* ((start (if (get-text-property (point) 'zoho-desk-status-badge)
                    (point)
                  (next-single-property-change
                   (point) 'zoho-desk-status-badge nil limit)))
         (end (and start (< start limit)
                   (get-text-property start 'zoho-desk-status-badge)
                   (next-single-property-change
                    start 'zoho-desk-status-badge nil limit))))
    (when end
      (set-match-data (list start end))
      (goto-char end)
      t)))

(defun zoho-desk--status-badge-star-matcher (limit)
  "Font-lock matcher for the badge heading's leading star before LIMIT.
The star is hidden by the org bullet setup, but its face — the
scaled, variable-pitch `org-level-1' — still sets the row's
ascent, sinking the badge text to the baseline of a much taller
line.  Matching it here re-faces it to the default height so the
badge line hugs the badge."
  (let* ((start (if (get-text-property (point) 'zoho-desk-status-badge-star)
                    (point)
                  (next-single-property-change
                   (point) 'zoho-desk-status-badge-star nil limit)))
         (end (and start (< start limit)
                   (get-text-property start 'zoho-desk-status-badge-star)
                   (next-single-property-change
                    start 'zoho-desk-status-badge-star nil limit))))
    (when end
      (set-match-data (list start end))
      (goto-char end)
      t)))

(defun zoho-desk--mention-matcher (limit)
  "Font-lock matcher for @agent mention spans before LIMIT.
Matches text carrying the `zoho-desk-mention' property: mentions
picked in the New Comment field and ones resolved out of fetched
comments."
  (let* ((start (if (get-text-property (point) 'zoho-desk-mention)
                    (point)
                  (next-single-property-change
                   (point) 'zoho-desk-mention nil limit)))
         (end (and start (< start limit)
                   (get-text-property start 'zoho-desk-mention)
                   (next-single-property-change
                    start 'zoho-desk-mention nil limit))))
    (when end
      (set-match-data (list start end))
      (goto-char end)
      t)))

(defun zoho-desk--badge-background (color)
  "Return COLOR blended into the theme background, mostly background.
The badge background reads as COLOR at low opacity; nil when the
colors do not resolve (tty frames)."
  (let ((fg (color-name-to-rgb color))
        (bg (color-name-to-rgb (face-background 'default nil t))))
    (when (and fg bg)
      (apply #'color-rgb-to-hex
             `(,@(cl-mapcar (lambda (c b) (+ (* 0.25 c) (* 0.75 b)))
                            fg bg)
               2)))))

(defun zoho-desk--status-badge-face ()
  "Return the face of the status badge at the current match.
The status reads as a * TODO keyword badge: org-modern's label
look when available (org's own todo/done faces otherwise), in the
status's `zoho-desk-status-colors' color — text and border in
the color itself, over a dim blend of it into the theme
background."
  (let* ((done (eq (get-text-property (match-beginning 0)
                                      'zoho-desk-status-badge)
                   'done))
         (base (if done
                   (if (facep 'org-modern-done) 'org-modern-done 'org-done)
                 (if (facep 'org-modern-todo) 'org-modern-todo 'org-todo)))
         (color (cdr (assoc-string
                      (string-trim
                       (buffer-substring-no-properties (match-beginning 0)
                                                       (match-end 0)))
                      zoho-desk-status-colors t))))
    (if color
        `(:inherit ,base
          :foreground ,color
          :background ,(or (zoho-desk--badge-background color)
                           'unspecified)
          :box (:color ,color :line-width -1))
      base)))

(defconst zoho-desk--input-font-lock-keywords
  '((zoho-desk--input-field-matcher (0 'zoho-desk-input append))
    (zoho-desk--mention-matcher (0 'zoho-desk-mention prepend))
    (zoho-desk--status-badge-matcher
     (0 (zoho-desk--status-badge-face) t))
    (zoho-desk--status-badge-star-matcher (0 'default t)))
  "Font-lock keywords for the input backgrounds and the status badge.
Appended after org's own keywords by
`zoho-desk-ticket-minor-mode', with the `append' face override, so
the field background sits under whatever org fontifies inside the
fields — code blocks stay darker than the field.")

(define-minor-mode zoho-desk-ticket-minor-mode
  "Commands and tabs on top of an org-mode Zoho ticket document.
\\{zoho-desk-ticket-minor-mode-map}"
  :lighter " ZohoTicket"
  ;; Remove before adding so repeated enables (one per render) stay
  ;; idempotent; adding at the end keeps the append merge after org's
  ;; own keywords.
  (font-lock-remove-keywords nil zoho-desk--input-font-lock-keywords)
  (if zoho-desk-ticket-minor-mode
      (progn
        (setq header-line-format '(:eval (zoho-desk--header-line)))
        (font-lock-add-keywords nil zoho-desk--input-font-lock-keywords
                                'append))
    (setq header-line-format nil))
  (font-lock-flush))

(defun zoho-desk--tab-keymap (heading)
  "Return a header-line keymap switching to the HEADING tab on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (with-selected-window (posn-window (event-start event))
                    (zoho-desk--set-tab heading))))
    map))

(defun zoho-desk--header-line ()
  "Return the tab header line for a ticket buffer."
  (concat
   " "
   (mapconcat
    (lambda (tab)
      (propertize (format " %s " (car tab))
                  'face (if (equal zoho-desk--current-tab (cdr tab))
                            'zoho-desk-tab-active
                          'shadow)
                  'mouse-face 'highlight
                  'help-echo (format "mouse-1: show %s" (car tab))
                  'keymap (zoho-desk--tab-keymap (cdr tab))))
    zoho-desk--tabs
    " ")
   (propertize "   (click, C-c z 1-4, or gt)" 'face 'shadow)))

(defun zoho-desk--set-tab (heading)
  "Narrow the ticket buffer to HEADING, or widen when nil."
  (widen)
  (when zoho-desk--tab-padding-overlay
    (delete-overlay zoho-desk--tab-padding-overlay)
    (setq zoho-desk--tab-padding-overlay nil))
  (setq zoho-desk--current-tab heading)
  (goto-char (point-min))
  (when heading
    (if (re-search-forward (concat "^\\* " (regexp-quote heading) "$") nil t)
        (progn (beginning-of-line)
               (org-narrow-to-subtree)
               (goto-char (point-min))
               ;; Narrowing leaves the heading flush against the
               ;; header line; give it the same breathing room the
               ;; Overview's real blank lines provide, without
               ;; touching buffer text.
               (setq zoho-desk--tab-padding-overlay
                     (make-overlay (point-min) (point-min)))
               (overlay-put zoho-desk--tab-padding-overlay
                            'before-string "\n\n"))
      (message "No %s section in this ticket" heading)))
  (if (fboundp 'org-fold-show-all) (org-fold-show-all) (org-show-all))
  (zoho-desk--fold-time-log-entries)
  (force-mode-line-update))

(defun zoho-desk--fold-time-log-entries ()
  "Collapse every entry under the Time Logs heading.
Tab switches unfold the whole document, so this runs after each
one: the time logs (New Time Log included) stay a compact index
until an entry is opened by hand (TAB on its heading)."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Time Logs$" nil t)
      (beginning-of-line)
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (while (re-search-forward "^\\*\\* " end t)
          (beginning-of-line)
          (if (fboundp 'org-fold-hide-subtree)
              (org-fold-hide-subtree)
            (outline-hide-subtree))
          (org-end-of-subtree t t))))))

(defun zoho-desk-tab-overview ()
  "Show the whole ticket document."
  (interactive)
  (zoho-desk--set-tab nil))

(defun zoho-desk-tab-thread ()
  "Narrow to the Email Thread section."
  (interactive)
  (zoho-desk--set-tab "Email Thread"))

(defun zoho-desk-tab-comments ()
  "Narrow to the Comments section."
  (interactive)
  (zoho-desk--set-tab "Comments"))

(defun zoho-desk-tab-time-logs ()
  "Narrow to the Time Logs section."
  (interactive)
  (zoho-desk--set-tab "Time Logs"))

(defun zoho-desk-tab-next (&optional backward)
  "Cycle to the next tab, or previous when BACKWARD."
  (interactive)
  (let* ((current (seq-position zoho-desk--tabs zoho-desk--current-tab
                                (lambda (tab tab-heading)
                                  (equal (cdr tab) tab-heading))))
         (next (mod (+ (or current 0) (if backward -1 1))
                    (length zoho-desk--tabs))))
    (zoho-desk--set-tab (cdr (nth next zoho-desk--tabs)))))

(defun zoho-desk-tab-previous ()
  "Cycle to the previous tab."
  (interactive)
  (zoho-desk-tab-next t))

(defun zoho-desk--insert-thread (thread)
  "Insert THREAD as an org subheading.
Threads whose full `content' was prefetched show it; the rest
show their summary with a not-fetched marker."
  (insert (format "** %s %s (%s %s)\n"
                  (zoho-desk--org-timestamp (alist-get 'createdTime thread))
                  (zoho-desk--person-name (alist-get 'author thread) "unknown")
                  (or (alist-get 'channel thread) "?")
                  (or (alist-get 'direction thread) "?"))
          ":PROPERTIES:\n"
          (format ":THREAD_ID: %s\n" (alist-get 'id thread))
          ":END:\n")
  (if-let* ((content (alist-get 'content thread)))
      (insert (zoho-desk--html-to-org-body content))
    (insert (zoho-desk--org-body
             (concat (string-trim (or (alist-get 'summary thread) ""))
                     "\n\n[not fetched — C-c z e here to load]")))))

(defun zoho-desk--insert-time-entry (entry)
  "Insert time ENTRY as an org subheading with its details.
The heading carries the essentials (when, who, how long, a
description snippet) because entries are collapsed by default;
the body holds the full description and billing details."
  (let* ((hours (string-to-number
                 (format "%s" (or (alist-get 'hoursSpent entry) 0))))
         (minutes (string-to-number
                   (format "%s" (or (alist-get 'minutesSpent entry) 0))))
         (seconds (string-to-number
                   (format "%s" (or (alist-get 'secondsSpent entry) 0))))
         ;; The API stores only the start (executedTime) and the
         ;; duration; the interval's end is computed for display.
         (end (when-let* ((iso (alist-get 'executedTime entry))
                          ((stringp iso))
                          ((not (string-empty-p iso)))
                          (total (+ (* 3600 hours) (* 60 minutes) seconds))
                          ((cl-plusp total)))
                (format-time-string "–%H:%M"
                                    (time-add (date-to-time iso) total))))
         (description (alist-get 'description entry))
         (html (and (stringp description)
                    (string-match-p "<[a-zA-Z!/]" description)))
         (text (string-trim
                (if html
                    (zoho-desk--html-to-text description)
                  (or description ""))))
         (summary (car (split-string text "\n" t "[ \t]+")))
         (owner (and (alist-get 'owner entry)
                     (zoho-desk--person-name (alist-get 'owner entry)))))
    (insert (format "** %s%s (%dh %02dm)%s%s\n"
                    (zoho-desk--org-timestamp
                     (or (alist-get 'executedTime entry)
                         (alist-get 'createdTime entry)))
                    (or end "")
                    hours minutes
                    (if owner (concat " " owner) "")
                    (if summary
                        (concat " — " (truncate-string-to-width
                                       summary 48 nil nil "…"))
                      ""))
            ":PROPERTIES:\n"
            (format ":TIME_ENTRY_ID: %s\n" (alist-get 'id entry))
            ":END:\n")
    (dolist (detail `(("Charge Type" . ,(alist-get 'requestChargeType entry))
                      ("Additional Cost" . ,(alist-get 'additionalCost entry))
                      ("Total Cost" . ,(alist-get 'totalCost entry))))
      (when (cdr detail)
        (insert (format "  - %s :: %s\n" (car detail) (cdr detail)))))
    (unless (string-empty-p text)
      (insert (if html
                  (zoho-desk--html-to-org-body description)
                (zoho-desk--org-body description))))))

(defun zoho-desk--render-ticket-org (ticket threads comments time-entries)
  "Fill the current buffer with an org document for TICKET."
  (let ((ticket-id (alist-get 'id ticket))
        (inhibit-read-only t))
    (erase-buffer)
    ;; Breathing room above the title; harmless since the buffer is
    ;; read-only outside the Reply section.
    (insert "\n\n")
    (insert (format "#+title: #%s %s\n\n"
                    (alist-get 'ticketNumber ticket)
                    (alist-get 'subject ticket)))
    ;; The status stands alone as a heading-sized badge, styled like
    ;; an org-modern * TODO keyword by the font-lock matcher.  The
    ;; surrounding spaces live inside the badge, giving the label a
    ;; character of padding within the border; the marked star gets
    ;; re-faced to the default height so the hidden bullet's huge
    ;; `org-level-1' ascent cannot push the badge down the line.
    (insert (propertize "* " 'zoho-desk-status-badge-star t)
            (propertize (concat " " (upcase (or (alist-get 'status ticket)
                                                "?"))
                                " ")
                        'zoho-desk-status-badge
                        (if (string= (downcase (or (alist-get 'statusType ticket)
                                                   (alist-get 'status ticket)
                                                   ""))
                                     "closed")
                            'done 'todo))
            "\n")
    (insert "* Details\n"
            (format "- Priority :: %s\n" (or (alist-get 'priority ticket) "-"))
            (format "- Due :: %s\n"
                    (if (alist-get 'dueDate ticket)
                        (zoho-desk--org-timestamp (alist-get 'dueDate ticket))
                      "-"))
            (format "- Contact :: %s\n" (zoho-desk--contact-name ticket))
            (format "- Assignee :: %s\n"
                    (zoho-desk--person-name (alist-get 'assignee ticket)))
            (format "- Ticket ID :: %s\n" ticket-id)
            (if-let* ((url (alist-get 'webUrl ticket)))
                (format "- Web :: [[%s][open in Zoho Desk]]\n" url)
              ""))
    (when-let* ((description (alist-get 'description ticket)))
      (insert "* Description\n"
              (zoho-desk--html-to-org-body description)))
    (insert "* Email Thread\n"
            "** Reply\n"
            ;; The label is its own read-only island: front-sticky so
            ;; nothing can be typed before it, rear-nonsticky so the
            ;; address right after it stays editable.
            (propertize "To: "
                        'zoho-desk-to-label t
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            (or (zoho-desk--default-reply-to ticket threads) "")
            "\n"
            ;; Separator line; its newline anchors the body field's
            ;; start.  The next newline is the (initially empty) body
            ;; line, and the marked one after it pins the field's end
            ;; so the body always grows in front of it.
            (propertize "\n" 'zoho-desk-body-start t)
            "\n"
            (propertize "\n" 'zoho-desk-body-end t))
    (if (null threads)
        (insert "  No emails yet.\n")
      ;; Newest first, matching the Zoho UI; only the newest
      ;; `zoho-desk-thread-prefetch' arrive with full bodies.
      (mapc #'zoho-desk--insert-thread threads))
    (insert "* Comments\n"
            "** New Comment\n"
            ;; Same separator pattern as the Reply body: the marked
            ;; newlines pin the editable field between them.  Type @
            ;; in the field to mention an agent; C-c z m posts.
            (propertize "\n" 'zoho-desk-comment-body-start t)
            "\n"
            (propertize "\n" 'zoho-desk-comment-body-end t))
    (if (null comments)
        (insert "  No comments.\n")
      ;; Newest first, matching the email thread; the API hands them
      ;; back oldest first.
      (dolist (comment (seq-sort-by
                        (lambda (comment)
                          (or (alist-get 'commentedTime comment) ""))
                        #'string> (append comments nil)))
        (insert (format "** %s %s\n"
                        (zoho-desk--org-timestamp
                         (alist-get 'commentedTime comment))
                        (zoho-desk--person-name
                         (alist-get 'commenter comment) "unknown")))
        (let ((content (alist-get 'content comment)))
          (insert (zoho-desk--resolve-comment-mentions
                   (if (equal (alist-get 'contentType comment) "html")
                       (zoho-desk--html-to-org-body content)
                     (zoho-desk--org-body content))
                   comment)))))
    (insert "* Time Logs\n"
            "** New Time Log\n"
            ;; Same input-field pattern as the Reply section:
            ;; read-only label islands with editable gaps after them,
            ;; then a description body between two marked separator
            ;; newlines.  The labels are fully rear-nonsticky so
            ;; typed text never inherits their marker properties, and
            ;; the mid-line duration labels are not front-sticky so
            ;; typing at the end of the value before them stays
            ;; legal.  The Description label doubles as the start
            ;; separator's line; `zoho-desk--protect-buffer' locks it
            ;; along with everything else outside the fields.
            (propertize "Executed: "
                        'zoho-desk-executed-label t
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            (format-time-string "[%Y-%m-%d %a %H:%M]")
            "\n"
            ;; The End line starts blank: filled (by hand or by the
            ;; ticket timer), it dates the entry's Executed..End
            ;; interval and the duration is computed from it instead
            ;; of the Hours / Minutes / Seconds values.
            (propertize "End: "
                        'zoho-desk-end-label t
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            "\n"
            ;; Each duration value starts as editable spaces so the
            ;; input face paints a visible box even while it's blank;
            ;; the spaces are trimmed away on submit.
            (propertize "Hours: "
                        'zoho-desk-duration 'hours
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            "    "
            (propertize "  Minutes: "
                        'zoho-desk-duration 'minutes
                        'read-only t
                        'rear-nonsticky t)
            "    "
            (propertize "  Seconds: "
                        'zoho-desk-duration 'seconds
                        'read-only t
                        'rear-nonsticky t)
            "    "
            "\n"
            "Description:"
            (propertize "\n" 'zoho-desk-time-body-start t)
            "\n"
            (propertize "\n" 'zoho-desk-time-body-end t))
    (mapc #'zoho-desk--insert-time-entry time-entries)
    (goto-char (point-min))))

(defun zoho-desk--normalize-buffer-style ()
  "Tone down prose-oriented styling that hurts ticket buffers.
Org heading sizes are kept as configured; only the 4x header-line
remap is replaced so the tab bar stays readable, and
trailing-whitespace highlighting is turned off."
  (setq-local face-remapping-alist
              (assq-delete-all 'header-line
                               (copy-alist face-remapping-alist)))
  (face-remap-add-relative
   'header-line `(:height ,(face-attribute 'default :height nil 'default)))
  (setq-local show-trailing-whitespace nil))

(defun zoho-desk--marker-field (start-prop end-prop)
  "Return (START . END) of the editable area between two marked newlines.
START-PROP and END-PROP name the text properties placed on the
separator newlines at render time; nil when either is missing."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((sep (text-property-any (point-min) (point-max)
                                          start-prop t))
                  (end (text-property-any sep (point-max) end-prop t)))
        (cons (1+ sep) end)))))

(defun zoho-desk--reply-body-field ()
  "Return (START . END) of the editable reply body area, or nil."
  (zoho-desk--marker-field 'zoho-desk-body-start 'zoho-desk-body-end))

(defun zoho-desk--time-body-field ()
  "Return (START . END) of the New Time Log description area, or nil."
  (zoho-desk--marker-field 'zoho-desk-time-body-start
                           'zoho-desk-time-body-end))

(defun zoho-desk--comment-body-field ()
  "Return (START . END) of the New Comment area, or nil."
  (zoho-desk--marker-field 'zoho-desk-comment-body-start
                           'zoho-desk-comment-body-end))

(defun zoho-desk--editable-fields ()
  "Return the buffer's editable input field ranges, sorted by position."
  (sort (delq nil (list (zoho-desk--reply-to-field)
                        (zoho-desk--reply-body-field)
                        (zoho-desk--comment-body-field)
                        (zoho-desk--time-executed-field)
                        (zoho-desk--time-end-field)
                        (zoho-desk--duration-field 'hours)
                        (zoho-desk--duration-field 'minutes)
                        (zoho-desk--duration-field 'seconds)
                        (zoho-desk--time-body-field)))
        (lambda (a b) (< (car a) (car b)))))

(defun zoho-desk--protect-buffer ()
  "Make everything except the input fields read-only.
Only the Reply section's To address and body, the New Comment
body and the New Time Log's Executed / End times, duration values
and description stay editable; the separators around them are
locked so the layout survives any edit."
  (let ((inhibit-read-only t))
    (save-excursion
      (save-restriction
        (widen)
        (let ((fields (zoho-desk--editable-fields))
              (pos (point-min)))
          (if (null fields)
              (add-text-properties (point-min) (point-max)
                                   '(read-only t front-sticky (read-only)))
            (dolist (field fields)
              (when (< pos (car field))
                (add-text-properties pos (car field) '(read-only t))
                ;; Nothing can be typed before the document start.
                (when (= pos (point-min))
                  (add-text-properties pos (car field)
                                       '(front-sticky (read-only))))
                ;; Last locked char before the field: rear-nonsticky
                ;; so typing at the field's start stays legal, and
                ;; fully so — a label's marker property must not leak
                ;; into text typed at the field's start either.
                (add-text-properties (1- (car field)) (car field)
                                     '(rear-nonsticky t)))
              (setq pos (max pos (cdr field))))
            ;; No front-sticky after the last field: text typed at a
            ;; field's end must stay editable.
            (when (< pos (point-max))
              (add-text-properties pos (point-max) '(read-only t)))))))))

(defun zoho-desk--field-jump (direction)
  "Move point to the end of the next input field in DIRECTION.
Cycles through the fields visible under the current narrowing;
when point is not inside a field, jumps to the nearest one in
DIRECTION instead, wrapping around."
  (let* ((fields (seq-filter (lambda (field)
                               (and (>= (car field) (point-min))
                                    (<= (cdr field) (point-max))))
                             (zoho-desk--editable-fields)))
         (count (length fields))
         (pos (seq-position fields (point)
                            (lambda (field p)
                              (and (>= p (car field))
                                   (<= p (cdr field)))))))
    (when (zerop count)
      (user-error "No input fields in this view"))
    (let* ((index (cond (pos (mod (+ pos direction) count))
                        ((> direction 0)
                         (or (seq-position fields (point)
                                           (lambda (field p)
                                             (> (car field) p)))
                             0))
                        (t (mod (1- (seq-count (lambda (field)
                                                 (< (cdr field) (point)))
                                               fields))
                                count))))
           (next (nth index fields)))
      ;; Land after the field's content, not after the blank
      ;; padding that keeps an empty field's box visible.
      (goto-char (cdr next))
      (skip-chars-backward " " (car next)))))

(defun zoho-desk-next-field ()
  "Jump to the next input field."
  (interactive)
  (zoho-desk--field-jump 1))

(defun zoho-desk-previous-field ()
  "Jump to the previous input field."
  (interactive)
  (zoho-desk--field-jump -1))

(defvar zoho-desk--show-ticket-generation 0
  "Bumped per ticket fetch so a stale response cannot win.")

(defun zoho-desk--live-ticket-buffer ()
  "Return the reusable ticket buffer, creating it if needed."
  (if (buffer-live-p zoho-desk--ticket-buffer)
      zoho-desk--ticket-buffer
    (setq zoho-desk--ticket-buffer
          (generate-new-buffer "*zoho ticket*"))))

(defun zoho-desk--show-fetch-error (buf err)
  "Show ERR in ticket buffer BUF and in the echo area."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (widen)
        (erase-buffer)
        (insert (propertize (format "\n\n  Zoho Desk: %s\n" err)
                            'face 'error)))))
  (message "Zoho Desk: %s" err))

(defun zoho-desk--show-ticket (ticket-id &optional ticket-number tab
                                         background)
  "Fetch TICKET-ID in the background and show it as an org document.
The ticket buffer pops up immediately with a fetching notice
naming TICKET-NUMBER (when known) and is filled in when the
responses arrive.  TAB, when non-nil, is selected once rendered.
BACKGROUND refreshes the buffer without popping or selecting it
\(used after a reply lands, when the user may have moved on)."
  (let* ((id (format "%s" ticket-id))
         (generation (cl-incf zoho-desk--show-ticket-generation))
         (buf (zoho-desk--live-ticket-buffer)))
    (with-current-buffer buf
      (when ticket-number
        (rename-buffer (format "*zoho #%s*" ticket-number) t))
      ;; Drop the previous ticket's state so commands fired while the
      ;; fetch is in flight cannot act on stale data.
      (zoho-desk-ticket-minor-mode -1)
      (setq zoho-desk--ticket nil
            zoho-desk--threads nil)
      (let ((inhibit-read-only t))
        (widen)
        (erase-buffer)
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (zoho-desk--normalize-buffer-style)
        (insert (propertize (format "\n\n  Fetching #%s …\n"
                                    (or ticket-number id))
                            'face 'shadow))
        (add-text-properties (point-min) (point-max)
                             '(read-only t front-sticky (read-only)))))
    (unless background
      (pop-to-buffer buf
                     `((display-buffer-reuse-window
                        display-buffer-below-selected)
                       (window-height . ,zoho-desk-ticket-window-height))))
    (zoho-desk--request-all-async
     `(("GET" ,(format "/tickets/%s" id)
        :params (("include" "contacts,assignee")))
       ("GET" ,(format "/tickets/%s/threads" id) :params (("limit" 20)))
       ;; Comments and time entries are best-effort, as before
       ;; (ignore-errors then).
       ("GET" ,(format "/tickets/%s/comments" id)
        :params (("limit" 50)) :soft-errors t)
       ("GET" ,(format "/tickets/%s/timeEntry" id)
        :params (("limit" 50)) :soft-errors t))
     (lambda (results err)
       (when (= generation zoho-desk--show-ticket-generation)
         (if err
             (zoho-desk--show-fetch-error buf err)
           (zoho-desk--prefetch-thread-bodies
            id generation buf tab
            (nth 0 results)
            (alist-get 'data (nth 1 results))
            (alist-get 'data (nth 2 results))
            (alist-get 'data (nth 3 results)))))))))

(defun zoho-desk--prefetch-thread-bodies (id generation buf tab
                                             ticket threads comments
                                             time-entries)
  "Fetch full bodies of TICKET's newest THREADS, then render into BUF.
Only the newest `zoho-desk-thread-prefetch' THREADS are fetched;
each fetched body is merged into its thread alist as `content'.
GENERATION guards against a newer fetch; TAB is the tab to select;
COMMENTS and TIME-ENTRIES are passed through to the render."
  (let ((prefetch (seq-take threads zoho-desk-thread-prefetch)))
    (zoho-desk--request-all-async
     (mapcar (lambda (thread)
               (list "GET" (format "/tickets/%s/threads/%s"
                                   id (alist-get 'id thread))
                     :soft-errors t))
             prefetch)
     (lambda (fulls _err)
       (when (and (= generation zoho-desk--show-ticket-generation)
                  (buffer-live-p buf))
         (zoho-desk--render-ticket
          buf ticket
          (append (seq-map-indexed
                   (lambda (thread index)
                     (if-let* ((full (nth index fulls)))
                         (cons (cons 'content
                                     (or (alist-get 'content full)
                                         (alist-get 'summary full)))
                               thread)
                       thread))
                   prefetch)
                  (nthcdr (length prefetch) threads))
          comments time-entries tab))))))

;;;; Inline images

(defvar zoho-desk--inline-image-dir
  (expand-file-name "zoho-desk-inline-images" temporary-file-directory)
  "Session cache directory for downloaded inline images.")

(defconst zoho-desk--inline-image-link-re
  "\\[\\[\\(/api/v1/[^][]*/inlineImages/[^][]+\\)\\]\\]"
  "Org link whose target is an API-relative inline image.")

(defun zoho-desk--inline-image-file (path)
  "Cache file name for the inline image at API PATH."
  (let ((ext (if (string-match "[?&]f=[^&]*\\.\\([A-Za-z0-9]+\\)\\'" path)
                 (concat "." (downcase (match-string 1 path)))
               "")))
    (expand-file-name (concat (md5 path) ext) zoho-desk--inline-image-dir)))

(defun zoho-desk--overlay-inline-image (path file)
  "Show image FILE over every org link to PATH in the current buffer.
The links sit in read-only text, so the image goes on an overlay
instead of the text itself; `evaporate' cleans the overlay up when
a re-render erases the buffer."
  (when-let* ((image (ignore-errors
                       (create-image
                        file nil nil
                        :max-width zoho-desk-inline-image-max-width))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((target (concat "[[" path "]]")))
          (while (search-forward target nil t)
            (unless (cl-some (lambda (ov) (overlay-get ov 'zoho-desk-image))
                             (overlays-at (match-beginning 0)))
              (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'zoho-desk-image t)
                (overlay-put ov 'display image)
                (overlay-put ov 'evaporate t)))))))))

(defun zoho-desk--fetch-inline-image (path file)
  "Download the inline image at API PATH into FILE, then display it.
PATH already carries Zoho's et/ha access parameters; the request
still needs the usual OAuth headers."
  (let ((buf (current-buffer)))
    (zoho-desk--request-async
     "GET" (string-remove-prefix "/api/v1" path)
     (lambda (data err)
       (cond
        (err (message "Zoho Desk: inline image fetch failed: %s" err))
        ((or (null data) (string-empty-p data)))
        (t
         (make-directory zoho-desk--inline-image-dir t)
         (let ((coding-system-for-write 'binary))
           (write-region data nil file nil 'silent))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (zoho-desk--overlay-inline-image path file))))))
     :raw t)))

(defun zoho-desk--display-inline-images ()
  "Fetch and overlay the API inline images referenced in the buffer.
Cached images show immediately; the rest arrive in the background."
  (when (and zoho-desk-inline-images (display-graphic-p))
    (let (paths)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward zoho-desk--inline-image-link-re nil t)
            (push (match-string-no-properties 1) paths))))
      (dolist (path (delete-dups (nreverse paths)))
        (let ((file (zoho-desk--inline-image-file path)))
          (if (file-exists-p file)
              (zoho-desk--overlay-inline-image path file)
            (zoho-desk--fetch-inline-image path file)))))))

(defun zoho-desk--render-ticket (buf ticket threads comments time-entries tab)
  "Fill BUF with TICKET's org document and select TAB."
  (with-current-buffer buf
    (rename-buffer (format "*zoho #%s*" (alist-get 'ticketNumber ticket)) t)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer))
    (zoho-desk--render-ticket-org ticket threads comments time-entries)
    (setq zoho-desk--ticket ticket
          zoho-desk--threads threads
          buffer-offer-save nil)
    (zoho-desk-ticket-minor-mode 1)
    (zoho-desk--sync-accent-faces)
    (zoho-desk--normalize-buffer-style)
    (zoho-desk--protect-buffer)
    (zoho-desk--set-tab tab)
    (zoho-desk--fill-pending-time-log)
    (zoho-desk--display-inline-images)))

(defun zoho-desk-open-ticket-at-point ()
  "Open the ticket on the current list line."
  (interactive)
  (let ((ticket (zoho-desk--ticket-at-point)))
    (zoho-desk--show-ticket (alist-get 'id ticket)
                            (alist-get 'ticketNumber ticket))))

(defun zoho-desk-refresh-ticket ()
  "Re-fetch the ticket shown in this buffer, keeping the current tab."
  (interactive)
  (unless zoho-desk--ticket (user-error "Not in a ticket buffer"))
  (zoho-desk--show-ticket (alist-get 'id zoho-desk--ticket)
                          (alist-get 'ticketNumber zoho-desk--ticket)
                          zoho-desk--current-tab))

(defun zoho-desk--replace-thread-body (thread-id body)
  "Replace the body of the thread entry THREAD-ID with BODY.
The entry is found by its THREAD_ID property, so this is a no-op
when it is gone (e.g. the ticket was refreshed meanwhile)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^[ \t]*:THREAD_ID: %s$"
                     (regexp-quote (format "%s" thread-id)))
             nil t)
        (let ((inhibit-read-only t))
          (org-back-to-heading t)
          (org-end-of-meta-data t)
          (delete-region (point) (save-excursion (org-end-of-subtree t t)))
          (insert body)
          (zoho-desk--protect-buffer)
          (zoho-desk--display-inline-images))))))

(defun zoho-desk-expand-thread-at-point ()
  "Fetch the full content of the org thread entry at point."
  (interactive)
  (let ((thread-id (org-entry-get nil "THREAD_ID")))
    (unless thread-id (user-error "Not on a thread entry"))
    (let ((ticket-id (alist-get 'id zoho-desk--ticket))
          (buf (current-buffer)))
      (zoho-desk--replace-thread-body
       thread-id (zoho-desk--org-body "[fetching …]"))
      (zoho-desk--request-async
       "GET" (format "/tickets/%s/threads/%s" ticket-id thread-id)
       (lambda (thread err)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (if err
                 (progn
                   (message "Zoho Desk: %s" err)
                   (zoho-desk--replace-thread-body
                    thread-id
                    (zoho-desk--org-body
                     (format "[fetch failed: %s — C-c z e here to retry]"
                             err))))
               (zoho-desk--replace-thread-body
                thread-id
                (zoho-desk--html-to-org-body
                 (or (alist-get 'content thread)
                     (alist-get 'summary thread)
                     "")))))))))))

;;;; Email reply

(defun zoho-desk--bare-email (address)
  "Extract the bare email from ADDRESS like \"Name\"<user@host>.
Zoho's thread fields carry display-name forms, but its sendReply
API only accepts plain addresses."
  (when (stringp address)
    (let ((trimmed (string-trim address)))
      (cond
       ((string-match "<\\([^>]+\\)>" trimmed) (match-string 1 trimmed))
       ((string-empty-p trimmed) nil)
       (t trimmed)))))

(defun zoho-desk--first-address (field)
  "Return the first bare email in FIELD, a To-style address list."
  (when (stringp field)
    (if (string-match "<\\([^>]+\\)>" field)
        (match-string 1 field)
      (car (split-string field "[,;]" t "[ \t]+")))))

(defvar zoho-desk--session-reply-from nil
  "Session alist of (departmentId . from-address) learned from tickets.")

(defun zoho-desk--ticket-department ()
  "Return the department id of the current ticket, as a string."
  (format "%s" (or (alist-get 'departmentId zoho-desk--ticket)
                   zoho-desk-department-id
                   zoho-desk--department
                   "")))

(defun zoho-desk--prompt-reply-from (department)
  "Ask for the From support address of DEPARTMENT; offer to remember it."
  (let ((from (string-trim
               (completing-read
                "From (department support address): "
                (delete-dups
                 (delq nil (mapcar #'cdr
                                   (append zoho-desk--session-reply-from
                                           zoho-desk-department-reply-from))))))))
    (when (string-empty-p from)
      (user-error "No From address"))
    (when (y-or-n-p (format "Remember %s as this department's From address? "
                            from))
      (setf (alist-get department zoho-desk-department-reply-from
                       nil nil #'equal)
            from)
      (ignore-errors
        (customize-save-variable 'zoho-desk-department-reply-from
                                 zoho-desk-department-reply-from)))
    from))

(defun zoho-desk--default-reply-to (ticket threads)
  "Return the default To address for TICKET with THREADS.
The sender of the latest inbound email, falling back to the
ticket's contact email; nil when neither exists."
  (or (zoho-desk--bare-email
       (alist-get 'fromEmailAddress
                  (seq-find (lambda (thread)
                              (and (equal (alist-get 'direction thread) "in")
                                   (alist-get 'fromEmailAddress thread)))
                            threads)))
      (zoho-desk--bare-email (alist-get 'email ticket))
      (zoho-desk--bare-email
       (alist-get 'email (alist-get 'contact ticket)))))

(defun zoho-desk--reply-from ()
  "Return the From support address for a reply on the current ticket.
Resolved in order: the global `zoho-desk-reply-from', the
department entry in `zoho-desk-department-reply-from', an address
learned earlier this session, the From of an outbound email on
this ticket, the address an inbound email was sent to, and finally
a prompt.  The result is remembered for the department, so fresh
tickets there need no derivation source of their own."
  (let* ((department (zoho-desk--ticket-department))
         (inbound (seq-find (lambda (thread)
                              (and (equal (alist-get 'direction thread) "in")
                                   (alist-get 'fromEmailAddress thread)))
                            zoho-desk--threads))
         (outbound (seq-find (lambda (thread)
                               (and (equal (alist-get 'direction thread) "out")
                                    (alist-get 'fromEmailAddress thread)))
                             zoho-desk--threads))
         (from (or zoho-desk-reply-from
                   (cdr (assoc department zoho-desk-department-reply-from))
                   (cdr (assoc department zoho-desk--session-reply-from))
                   (zoho-desk--bare-email
                    (alist-get 'fromEmailAddress outbound))
                   (zoho-desk--first-address (alist-get 'to inbound))
                   (zoho-desk--prompt-reply-from department))))
    (setf (alist-get department zoho-desk--session-reply-from
                     nil nil #'equal)
          from)
    from))

(defun zoho-desk--label-line-field (label-prop)
  "Return (START . END) of the editable rest of a labeled input line.
LABEL-PROP is the text property carried by the line's read-only
label; the field runs from the label's end to the end of line."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((label (next-single-property-change
                          (point-min) label-prop))
                  (start (next-single-property-change label label-prop)))
        (goto-char start)
        (cons start (line-end-position))))))

(defun zoho-desk--reply-to-field ()
  "Return (START . END) of the To line's editable address area, or nil."
  (zoho-desk--label-line-field 'zoho-desk-to-label))

(defun zoho-desk--time-executed-field ()
  "Return (START . END) of the Executed line's editable area, or nil."
  (zoho-desk--label-line-field 'zoho-desk-executed-label))

(defun zoho-desk--time-end-field ()
  "Return (START . END) of the End line's editable area, or nil."
  (zoho-desk--label-line-field 'zoho-desk-end-label))

(defun zoho-desk--duration-field (unit)
  "Return (START . END) of the duration UNIT's editable value, or nil.
UNIT is one of the symbols `hours', `minutes' or `seconds'; its
value runs from the end of its label to the start of the next
duration label, or to the end of the line for the last one.  An
empty value is a zero-width range."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((label (text-property-any (point-min) (point-max)
                                            'zoho-desk-duration unit))
                  (start (text-property-not-all label (point-max)
                                                'zoho-desk-duration unit)))
        (goto-char start)
        (cons start
              ;; A duration property right at START means the next
              ;; label is adjacent: the value is empty.
              (if (get-text-property start 'zoho-desk-duration)
                  start
                (min (or (next-single-property-change
                          start 'zoho-desk-duration)
                         (point-max))
                     (line-end-position))))))))

(defun zoho-desk--duration-input (unit)
  "Return the duration UNIT field's value as a whole number, 0 when blank."
  (let* ((field (or (zoho-desk--duration-field unit)
                    (user-error "No New Time Log section in this buffer")))
         (text (string-trim (buffer-substring-no-properties
                             (car field) (cdr field)))))
    (cond ((string-empty-p text) 0)
          ((string-match-p "\\`[0-9]+\\'" text) (string-to-number text))
          (t (user-error "%s must be a whole number, not %S"
                         (capitalize (symbol-name unit)) text)))))

(defun zoho-desk--field-time-value (field what)
  "Return FIELD's content as an Emacs time value, nil when blank.
FIELD is a (START . END) range or nil; WHAT names the field in the
`user-error' raised when its content is not a readable org
timestamp."
  (when field
    (let ((text (string-trim (buffer-substring-no-properties
                              (car field) (cdr field)))))
      (unless (string-empty-p text)
        (condition-case nil
            (org-time-string-to-time text)
          (error (user-error "Unreadable %s time: %s" what text)))))))

(defun zoho-desk--time-executed-value ()
  "Return the Executed field's time as an Emacs time value.
nil when the field is blank (meaning: now); a `user-error' when
its content is not a readable org timestamp."
  (zoho-desk--field-time-value (zoho-desk--time-executed-field) "Executed"))

(defun zoho-desk--time-end-value ()
  "Return the End field's time as an Emacs time value.
nil when the field is blank (meaning: the duration comes from the
Hours / Minutes / Seconds values); a `user-error' when its content
is not a readable org timestamp."
  (zoho-desk--field-time-value (zoho-desk--time-end-field) "End"))

(defun zoho-desk--set-field (field text)
  "Replace the editable FIELD's content with TEXT.
FIELD is a (START . END) range as returned by the field lookup
functions; nil is a no-op."
  (when field
    (let ((inhibit-read-only t))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (car field))
          (delete-region (car field) (cdr field))
          (insert text))))))

(defun zoho-desk--reply-to-address ()
  "Return the address written on the To line, or nil when blank."
  (when-let* ((field (zoho-desk--reply-to-field)))
    (let ((address (string-trim (buffer-substring-no-properties
                                 (car field) (cdr field)))))
      (unless (string-empty-p address) address))))

(defun zoho-desk--set-reply-to-address (address)
  "Write ADDRESS into the To line, replacing its current content."
  (zoho-desk--set-field (zoho-desk--reply-to-field) address))

(defun zoho-desk--reply-body ()
  "Return the text written in the reply body field, sans comment lines.
Only the content of the editable area under the ** Reply
subheading is sent."
  (save-restriction
    (widen)
    (let* ((field (or (zoho-desk--reply-body-field)
                      (user-error "No Reply section in this buffer")))
           (body (buffer-substring-no-properties (car field) (cdr field))))
      ;; Only true org comments ("# ..."), not #+keyword lines like
      ;; #+begin_src, which must survive for the HTML export.
      (string-trim
       (replace-regexp-in-string "^[ \t]*#\\(?: .*\\)?$" "" body)))))

(defun zoho-desk--reply-to-candidates ()
  "Return known addresses for the To prompt: contact and thread senders."
  (delete-dups
   (delq nil
         (mapcar #'zoho-desk--bare-email
                 (append
                  (list (alist-get 'email zoho-desk--ticket)
                        (alist-get 'email
                                   (alist-get 'contact zoho-desk--ticket)))
                  (mapcar (lambda (thread)
                            (alist-get 'fromEmailAddress thread))
                          zoho-desk--threads))))))

(defvar zoho-desk--org-email-candidates nil
  "Cached \"Name <email>\" strings of the org's contacts.
Fetched once per session by `zoho-desk-add-email'; a prefix
argument there re-fetches.")

(defun zoho-desk--fetch-org-email-candidates ()
  "Fetch every org contact's email addresses, blocking.
Returns \"Name <email>\" strings, one per known address.  The
/contacts endpoint needs the Desk.contacts.READ scope; tokens
generated without it get a re-authorize hint instead of a raw
HTTP error."
  (let ((from 0) (page-size 100) candidates page)
    (condition-case err
        (while (progn
                 (setq page (alist-get
                             'data
                             (zoho-desk--request
                              "GET" "/contacts"
                              :params `(("from" ,from)
                                        ("limit" ,page-size)
                                        ("sortBy" "firstName")))))
                 (dolist (contact (append page nil))
                   (let ((name (zoho-desk--person-name contact)))
                     (dolist (email (list (alist-get 'email contact)
                                          (alist-get 'secondaryEmail
                                                     contact)))
                       (when (and email (not (string-empty-p email)))
                         (push (format "%s <%s>" name email) candidates)))))
                 (setq from (+ from page-size))
                 (= (length page) page-size)))
      (error
       (if (string-match-p "SCOPE_MISMATCH" (error-message-string err))
           (user-error (concat "Token lacks Desk.contacts.READ — generate "
                               "a grant code with the scopes listed in "
                               "`zoho-desk-authorize' and run it again"))
         (signal (car err) (cdr err)))))
    (delete-dups (nreverse candidates))))

(defun zoho-desk-add-email (&optional refresh)
  "Fuzzy-pick an org contact's email and add it to the To field.
Offers the whole org contact list (fetched once per session; a
prefix argument REFRESH re-fetches).  The picked address is
appended after a comma when the To line already has one.
Free-typed input that matches no contact goes in verbatim."
  (interactive "P")
  (unless (zoho-desk--reply-to-field)
    (user-error "No Reply section in this buffer"))
  (when (or refresh (null zoho-desk--org-email-candidates))
    (message "Fetching org contacts…")
    (setq zoho-desk--org-email-candidates
          (zoho-desk--fetch-org-email-candidates))
    (message "Fetching org contacts…done (%d addresses)"
             (length zoho-desk--org-email-candidates)))
  (let* ((choice (completing-read "Add email: "
                                  zoho-desk--org-email-candidates))
         (email (or (zoho-desk--bare-email choice) ""))
         (current (zoho-desk--reply-to-address)))
    (when (string-empty-p email)
      (user-error "No address picked"))
    (zoho-desk--set-reply-to-address
     (if current (concat current "," email) email))))

(defun zoho-desk--upload-spec (file)
  "Return an async request spec uploading FILE to the uploads store.
The response id goes in the `uploads' field of a sendReply
payload, which attaches the file to that reply's thread."
  (let* ((boundary (format "----zoho-desk-%06x%06x"
                           (random #xffffff) (random #xffffff)))
         (body (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert "--" boundary "\r\n"
                         (format (concat "Content-Disposition: form-data; "
                                         "name=\"file\"; filename=\"%s\"\r\n")
                                 (file-name-nondirectory file))
                         "Content-Type: application/octet-stream\r\n\r\n")
                 (insert-file-contents-literally file nil nil nil)
                 (goto-char (point-max))
                 (insert "\r\n--" boundary "--\r\n")
                 (buffer-string))))
    (list "POST" "/uploads"
          :raw-payload body
          :content-type (concat "multipart/form-data; boundary=" boundary))))

(defun zoho-desk--reply-image-files (org-text)
  "Return existing files referenced as [[file:...]] links in ORG-TEXT."
  (let ((start 0) files)
    (while (string-match "\\[\\[file:\\([^]]+\\)\\]" org-text start)
      (setq start (match-end 0))
      (let ((file (expand-file-name (match-string 1 org-text))))
        (when (file-exists-p file)
          (push file files))))
    (delete-dups (nreverse files))))

(defun zoho-desk--image-content-type (file)
  "MIME type for image FILE, from its extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "jpg" "jpeg") "image/jpeg")
    ("gif" "image/gif")
    ("webp" "image/webp")
    (_ "image/png")))

(defun zoho-desk--composer-upload-image (ticket file cookie)
  "Upload image FILE through TICKET's portal composer servlet.
The servlet (ImageUpload.do) answers with an ImageDisplay URL
whose blockId Zoho's mailer converts into a real cid inline image
at send time.  COOKIE is the minimal session cookie.  Returns the
URL, or nil when the servlet refuses — in practice a stale
session cookie."
  (let* ((base-portal
          (or (zoho-desk--ticket-portal-base ticket)
              (user-error "Zoho Desk: cannot derive the portal from %S"
                          (alist-get 'webUrl ticket))))
         (base (car base-portal))
         (portal (cdr base-portal))
         (csrf (or (and (string-match "crmcsr=\\([^;]+\\)" cookie)
                        (match-string 1 cookie))
                   (user-error "Zoho Desk: session cookie has no crmcsr")))
         (boundary (format "----zoho-desk-%06x%06x"
                           (random #xffffff) (random #xffffff)))
         (body (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert "--" boundary "\r\n"
                         (format (concat "Content-Disposition: form-data; "
                                         "name=\"img_file\"; filename=\"%s\"\r\n")
                                 (file-name-nondirectory file))
                         "Content-Type: " (zoho-desk--image-content-type file)
                         "\r\n\r\n")
                 (insert-file-contents-literally file nil nil nil)
                 (goto-char (point-max))
                 (insert "\r\n--" boundary "\r\n"
                         "Content-Disposition: form-data; "
                         "name=\"crmcsrfparam\"\r\n\r\n"
                         csrf "\r\n"
                         "--" boundary "--\r\n")
                 (buffer-string)))
         (url-request-method "POST")
         (url-request-data body)
         ;; The servlet rejects a non-browser User-Agent.  url.el emits
         ;; its own UA header from `url-user-agent', so set that rather
         ;; than adding a second (duplicate) header via extra-headers.
         (url-user-agent zoho-desk--browser-user-agent)
         ;; Ask for no gzip; the reply is a short URL and
         ;; `zoho-desk--response-body' does not decompress.
         (url-mime-encoding-string "identity")
         (url-request-extra-headers
          (mapcar (lambda (h)
                    (cons (car h)
                          (encode-coding-string (cdr h) 'utf-8)))
                  `(("Content-Type"
                     . ,(concat "multipart/form-data; boundary=" boundary))
                    ("Cookie" . ,cookie)
                    ("X-ZCSRF-TOKEN" . ,(concat "crmcsrfparam=" csrf))
                    ("Origin" . ,base)
                    ("Referer" . ,(concat base "/agent/")))))
         (buf (url-retrieve-synchronously
               (format "%s/support/%s/ImageUpload.do?uploadMode=newzeImageUpload"
                       base portal)
               t t zoho-desk-request-timeout)))
    (when buf
      (with-current-buffer buf
        (prog1
            (let ((answer (string-trim (zoho-desk--response-body))))
              (and (eq url-http-response-status 200)
                   (string-match-p "\\`https://[^ \n]*ImageDisplay\\?" answer)
                   answer))
          (kill-buffer))))))

(defun zoho-desk--upload-reply-images-inline (ticket files)
  "Upload FILES via the composer servlet for true inline sending.
Returns an alist of (FILE . URL).  A missing or stale session
cookie opens the agent console and prompts for a fresh one (see
`zoho-desk-refresh-session-cookie'); quitting that prompt aborts
the whole send with the reply buffer untouched."
  (let ((cookie (zoho-desk--current-session-cookie))
        (refreshed nil)
        result)
    (unless cookie
      (setq cookie (zoho-desk--refresh-session-cookie-flow ticket)
            refreshed t)
      (unless cookie
        (user-error (concat "Zoho Desk: inline images require a session"
                            " cookie refresh — reply not sent"))))
    (dolist (file files)
      (let ((url (zoho-desk--composer-upload-image ticket file cookie)))
        (unless url
          (if refreshed
              (user-error "Zoho Desk: inline upload failed for %s — reply not sent"
                          (file-name-nondirectory file))
            (setq cookie (zoho-desk--refresh-session-cookie-flow ticket)
                  refreshed t)
            (unless cookie
              (user-error (concat "Zoho Desk: inline images require a session"
                                  " cookie refresh — reply not sent")))
            (setq url (zoho-desk--composer-upload-image ticket file cookie))
            (unless url
              (user-error "Zoho Desk: inline upload failed for %s — reply not sent"
                          (file-name-nondirectory file)))))
        (push (cons file url) result)))
    (nreverse result)))

(defun zoho-desk--email-safe-block-styles (html)
  "Inline the block styles that body-only HTML has no stylesheet for.
Zoho keeps <pre>, <blockquote> and style attributes intact (only
cid:/data: images are stripped), but org's class-based styling is
lost in mail clients, so quote blocks get an italic left-border
style and pre blocks a monospace box using the current theme's
colors — matching the htmlize span colors, which come from the
same theme."
  (let* ((bg (let ((c (face-background 'default nil t)))
               (if (stringp c) c "#2b2b3a")))
         (fg (let ((c (face-foreground 'default nil t)))
               (if (stringp c) c "#e6e6e6")))
         (pre-style
          (format (concat "font-family: monospace; background: %s; "
                          "color: %s; padding: 8px 10px; "
                          "border-radius: 4px; overflow-x: auto")
                  bg fg)))
    (setq html (replace-regexp-in-string
                "<pre\\( class=\"[^\"]*\"\\)?>"
                (format "<pre\\1 style=\"%s\">" pre-style)
                html))
    (replace-regexp-in-string
     "<blockquote>"
     (concat "<blockquote style=\"font-style: italic; "
             "border-left: 3px solid #888888; margin: 0 0 0 4px; "
             "padding: 2px 12px\">")
     html t t)))

(defun zoho-desk--rewrite-thread-image-links (org-text)
  "Turn thread inlineImages org links in ORG-TEXT into img snippets.
The one src form Zoho's mailer converts to a real cid MIME part
at send time is its own /threads/.../inlineImages/... URL
(verified by probing; uploads hrefs and external URLs pass
through untouched).  So a thread-image link quoted into the reply
— the incoming images render as exactly these links — reaches the
recipient as a true inline image.  The et/ha access parameters
are re-minted on every thread fetch, so links taken from the
current buffer are valid at send time."
  (replace-regexp-in-string
   zoho-desk--inline-image-link-re
   (lambda (link)
     (save-match-data
       (string-match zoho-desk--inline-image-link-re link)
       (format "@@html:<img src=\"%s%s\">@@"
               (replace-regexp-in-string "/api/v1\\'" "" zoho-desk-base-url)
               (replace-regexp-in-string "&" "&amp;"
                                         (match-string 1 link)))))
   org-text))

(defun zoho-desk--org-to-html (org-text images)
  "Export ORG-TEXT to body-only HTML.
IMAGES is an alist of (FILE . INLINE-URL) for the [[file:...]]
images in the reply.  A file with an INLINE-URL becomes an <img>
pointing at it — Zoho's mailer converts its own composer URLs
into real cid inline images at send time.  A file with nil falls
back to a \"[image: NAME -- attached]\" marker (the file rides
along as an attachment), since Zoho strips cid: and data: imgs
from API sends.  Images already inline in a thread are handled by
`zoho-desk--rewrite-thread-image-links'.  Single newlines are
kept as line breaks so the email reads like the compose buffer."
  (require 'ox-html)
  (let ((html (zoho-desk--email-safe-block-styles
               (org-export-string-as
                (zoho-desk--rewrite-thread-image-links org-text)
                'html t '(:preserve-breaks t)))))
    (dolist (entry images html)
      (let ((file (car entry))
            (inline-url (cdr entry)))
        (setq html (replace-regexp-in-string
                    (concat "<img [^>]*src=\"\\(?:file://\\)?"
                            (regexp-quote file) "\"[^>]*>")
                    (if inline-url
                        (format "<img src=\"%s\" style=\"max-width: 100%%;\">"
                                (replace-regexp-in-string
                                 "&" "&amp;" inline-url))
                      (format "<em>[image: %s &mdash; attached]</em>"
                              (file-name-nondirectory file)))
                    html t t))))))

(defun zoho-desk--clipboard-image-data ()
  "Return raw PNG data from the clipboard, or nil."
  (let ((data (or (ignore-errors (gui-get-selection 'CLIPBOARD 'image/png))
                  (cond
                   ((and (getenv "WAYLAND_DISPLAY")
                         (executable-find "wl-paste"))
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (when (zerop (call-process "wl-paste" nil t nil
                                                 "--type" "image/png"))
                        (buffer-string))))
                   ((executable-find "xclip")
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (when (zerop (call-process "xclip" nil t nil
                                                 "-selection" "clipboard"
                                                 "-t" "image/png" "-o"))
                        (buffer-string))))))))
    (when (and (stringp data)
               (string-prefix-p "\211PNG" (if (multibyte-string-p data)
                                              (encode-coding-string data
                                                                    'binary)
                                            data)))
      data)))

(defun zoho-desk--clipboard-image-file ()
  "Save the clipboard image to a temp PNG file; return the path or nil."
  (when-let* ((data (zoho-desk--clipboard-image-data))
              (file (make-temp-file "zoho-embed-" nil ".png")))
    (let ((coding-system-for-write 'binary))
      (write-region (if (multibyte-string-p data)
                        (encode-coding-string data 'binary)
                      data)
                    nil file nil 'silent))
    file))

(defun zoho-desk-insert-image (&optional file)
  "Attach the clipboard image to the reply at point.
With a prefix argument, prompt for an image FILE instead; also
falls back to the prompt when the clipboard holds no image.  On
send the image is attached to the reply and its position in the
body becomes an \"[image: ...]\" marker."
  (interactive
   (list (when current-prefix-arg (read-file-name "Embed image file: "))))
  (let ((file (or file
                  (zoho-desk--clipboard-image-file)
                  (read-file-name "No image in clipboard — image file: "))))
    (insert (format "[[file:%s]]\n" (expand-file-name file)))
    (when (fboundp 'org-display-inline-images)
      (org-display-inline-images nil t))))

(defun zoho-desk-send-reply ()
  "Send the Reply section of this ticket buffer as an email reply.
The address on the Reply section's To line is used as-is — edit it
inline (it is prefilled with the latest inbound sender; any
address works, so a test mail to yourself does too).  When the To
line is blank, prompts with the ticket's known addresses instead.
With `zoho-desk-reply-html' the org text is exported to HTML and
image links are embedded and attached."
  (interactive)
  (unless zoho-desk--ticket (user-error "Not in a ticket buffer"))
  (let* ((body (zoho-desk--reply-body))
         (from (zoho-desk--reply-from))
         (ticket-id (alist-get 'id zoho-desk--ticket))
         ;; Undocumented but schema-valid sendReply field: link the
         ;; outgoing email to an existing thread so it carries
         ;; In-Reply-To/References and lands in the recipient's
         ;; conversation instead of starting a new one (what the web
         ;; UI does on Reply).  Newest email thread wins so follow-up
         ;; sends keep chaining.
         (reply-thread-id
          (alist-get 'id
                     (seq-find (lambda (thread)
                                 (equal (alist-get 'channel thread) "EMAIL"))
                               zoho-desk--threads))))
    (when (string-empty-p body)
      (user-error "The Reply section is empty"))
    (when zoho-desk-signature
      (setq body (concat body "\n\n" zoho-desk-signature)))
    (let ((to (or (zoho-desk--reply-to-address)
                  ;; Blank To line: fall back to picking an address.
                  (let* ((default (zoho-desk--default-reply-to
                                   zoho-desk--ticket zoho-desk--threads))
                         (choice (string-trim
                                  (completing-read
                                   (if default
                                       (format "To (default %s): " default)
                                     "To: ")
                                   (zoho-desk--reply-to-candidates)
                                   nil nil nil nil default))))
                    (when (string-empty-p choice)
                      (user-error "No To address"))
                    ;; Keep the To line honest about where it went.
                    (zoho-desk--set-reply-to-address choice)
                    choice))))
      (let* ((buf (current-buffer))
             (ticket-number (alist-get 'ticketNumber zoho-desk--ticket))
             (image-files (and zoho-desk-reply-html
                               (zoho-desk--reply-image-files body)))
             ;; Inline path: upload through the composer servlet up
             ;; front (may prompt for a session cookie refresh and
             ;; abort the send — nothing has gone out yet).
             (inline-urls (and image-files zoho-desk-send-inline-images
                               (zoho-desk--upload-reply-images-inline
                                zoho-desk--ticket image-files)))
             (images (or inline-urls
                         (mapcar (lambda (file) (cons file nil))
                                 image-files)))
             (send
              (lambda (upload-ids)
                (zoho-desk--request-async
                 "POST" (format "/tickets/%s/sendReply" ticket-id)
                 (lambda (_result err)
                   (if err
                       ;; The Reply section still holds the text — the
                       ;; refocused buffer is ready for another , s.
                       (zoho-desk--announce-write-failure
                        (format "reply to %s" to) err buf)
                     (message "Reply sent to %s" to)
                     ;; Re-fetch so the sent email appears in the thread
                     ;; and the Reply section is emptied for the next
                     ;; round — but only if the buffer still shows this
                     ;; ticket, and without stealing focus.
                     (when (and (buffer-live-p buf)
                                (equal (alist-get
                                        'id (buffer-local-value
                                             'zoho-desk--ticket buf))
                                       ticket-id))
                       (zoho-desk--show-ticket
                        ticket-id ticket-number
                        (buffer-local-value 'zoho-desk--current-tab buf)
                        t))))
                 :payload (append
                           `(("channel" . "EMAIL")
                             ("to" . ,to)
                             ("fromEmailAddress" . ,from))
                           (when reply-thread-id
                             `(("inReplyToThreadId" . ,reply-thread-id)))
                           (if zoho-desk-reply-html
                               `(("contentType" . "html")
                                 ("content" . ,(zoho-desk--org-to-html
                                                body images)))
                             `(("contentType" . "plainText")
                               ("content" . ,body)))
                           (when upload-ids
                             `(("uploads" . ,upload-ids))))))))
        (message "Sending reply to %s…" to)
        (if (or (null image-files) inline-urls)
            ;; No images, or all of them inlined — nothing to attach.
            (funcall send nil)
          (zoho-desk--request-all-async
           (mapcar #'zoho-desk--upload-spec image-files)
           (lambda (responses err)
             (let ((ids (mapcar (lambda (r) (alist-get 'id r)) responses)))
               (cond
                (err (zoho-desk--announce-write-failure
                      "image upload" err buf))
                ((memq nil ids)
                 (zoho-desk--announce-write-failure
                  "image upload" "no upload id in response" buf))
                (t (funcall send
                            (mapcar (lambda (id) (format "%s" id))
                                    ids))))))))))))

;;;; Agent mentions

(defvar zoho-desk--org-agent-candidates nil
  "Cached (\"Name <email>\" . ZUID) pairs of the org's agents.
Fetched once per session for @mention completion; a prefix
argument on `zoho-desk-electric-mention' re-fetches.  The ZUID
string is what Zoho's zsu[@user:…]zsu comment markup carries.")

(defun zoho-desk--fetch-org-agent-candidates ()
  "Fetch every org agent, blocking; return mention candidate pairs.
The /agents endpoint needs the Desk.agents.READ scope; tokens
generated without it get a re-authorize hint instead of a raw
HTTP error."
  (let ((from 0) (page-size 100) candidates page)
    (condition-case err
        (while (progn
                 (setq page (alist-get
                             'data
                             (zoho-desk--request
                              "GET" "/agents"
                              :params `(("from" ,from)
                                        ("limit" ,page-size)))))
                 (dolist (agent (append page nil))
                   (when-let* ((zuid (or (alist-get 'zuid agent)
                                         (alist-get 'id agent))))
                     (let ((name (zoho-desk--person-name agent))
                           (email (or (alist-get 'emailId agent)
                                      (alist-get 'email agent))))
                       (push (cons (if email
                                       (format "%s <%s>" name email)
                                     name)
                                   (format "%s" zuid))
                             candidates))))
                 (setq from (+ from page-size))
                 (= (length page) page-size)))
      (error
       (if (string-match-p "SCOPE_MISMATCH" (error-message-string err))
           (user-error (concat "Token lacks Desk.agents.READ — generate "
                               "a grant code with the scopes listed in "
                               "`zoho-desk-authorize' and run it again"))
         (signal (car err) (cdr err)))))
    (nreverse candidates)))

(defun zoho-desk--ensure-agent-candidates (&optional refresh)
  "Fill the agent mention candidate cache, blocking; REFRESH re-fetches."
  (when (or refresh (null zoho-desk--org-agent-candidates))
    (message "Fetching org agents…")
    (setq zoho-desk--org-agent-candidates
          (zoho-desk--fetch-org-agent-candidates))
    (message "Fetching org agents…done (%d agents)"
             (length zoho-desk--org-agent-candidates))))

(defun zoho-desk--mention-context-p ()
  "Return non-nil when point is where a comment @mention makes sense:
anywhere in a comment compose buffer, or inside a ticket buffer's
New Comment field."
  (or (derived-mode-p 'zoho-desk-comment-mode)
      (and (bound-and-true-p zoho-desk-ticket-minor-mode)
           (when-let* ((field (zoho-desk--comment-body-field)))
             (and (>= (point) (car field))
                  (<= (point) (cdr field)))))))

(defun zoho-desk--insert-mention (&optional refresh)
  "Fuzzy-pick an org agent and insert an @mention at point.
The inserted name carries the agent's ZUID as a text property, so
posting can rebuild Zoho's mention markup no matter how the text
around it is edited.  Free-typed input that matches no agent goes
in as plain text after the @.  REFRESH re-fetches the agent list."
  (zoho-desk--ensure-agent-candidates refresh)
  (let* ((choice (completing-read "Mention agent: "
                                  zoho-desk--org-agent-candidates))
         (entry (assoc choice zoho-desk--org-agent-candidates))
         (name (string-trim (car (split-string (or choice "") "<")))))
    (when (string-empty-p name)
      (user-error "No agent picked"))
    (insert (if entry
                ;; rear-nonsticky: text typed right after the mention
                ;; must not inherit the ZUID and grow the tag.
                (propertize (concat "@" name)
                            'zoho-desk-mention (cdr entry)
                            'rear-nonsticky t)
              (concat "@" name))
            " ")))

(defun zoho-desk-electric-mention (&optional refresh)
  "Insert @, completing an agent mention where comments are composed.
The completion prompt only appears inside the New Comment field
\(or anywhere in a comment compose buffer) and only at the start
of a word — @ mid-word, as in an email address, stays plain.
Quitting the prompt leaves a literal @ too.  A prefix argument
REFRESH re-fetches the agent list before completing."
  (interactive "P")
  (if (and (zoho-desk--mention-context-p)
           (or (bolp) (memq (char-before) '(?\s ?\t ?\n))))
      (condition-case nil
          (zoho-desk--insert-mention refresh)
        (quit (insert "@")))
    (self-insert-command 1 ?@)))

(defun zoho-desk--mention-markup-string (start end)
  "Return the buffer text between START and END with Zoho mention markup.
Each span carrying a `zoho-desk-mention' ZUID becomes
\"zsu[@user:ZUID]zsu\", the syntax the comments API expects for
tagging an agent; everything else is copied verbatim."
  (let ((chunks nil) (pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'zoho-desk-mention
                                               nil end))
            (zuid (get-text-property pos 'zoho-desk-mention)))
        (push (if (stringp zuid)
                  (format "zsu[@user:%s]zsu" zuid)
                (buffer-substring-no-properties pos next))
              chunks)
        (setq pos next)))
    (apply #'concat (nreverse chunks))))

(defun zoho-desk--comment-mention-names (comment)
  "Return a ZUID → display name alist from COMMENT's mention records.
The records' exact shape is undocumented, so a surprise never
gets to break the render — it just falls back to the agent cache."
  (ignore-errors
    (delq nil
          (mapcar (lambda (mention)
                    (when-let* ((zuid (or (alist-get 'zuid mention)
                                          (alist-get 'id mention))))
                      (cons (format "%s" zuid)
                            (or (alist-get 'name mention)
                                (zoho-desk--person-name mention nil)))))
                  (append (or (alist-get 'mention comment)
                              (alist-get 'mentions comment))
                          nil)))))

(defun zoho-desk--resolve-comment-mentions (text comment)
  "Replace Zoho's zsu[@user:…]zsu markup in TEXT with @Name.
Names come from COMMENT's own mention records when the API sends
them, then from the cached agent list; an id nobody recognizes
stays visible as @<zuid>.  Resolved mentions keep the ZUID in the
`zoho-desk-mention' property, which also hands them the mention
face."
  (let ((names (zoho-desk--comment-mention-names comment)))
    (replace-regexp-in-string
     "zsu\\[@user:\\([0-9]+\\)\\]zsu"
     (lambda (match)
       (save-match-data
         (let* ((zuid (and (string-match ":\\([0-9]+\\)\\]" match)
                           (match-string 1 match)))
                (name (or (cdr (assoc zuid names))
                          (when-let* ((entry (rassoc
                                              zuid
                                              zoho-desk--org-agent-candidates)))
                            (string-trim
                             (car (split-string (car entry) "<")))))))
           (propertize (concat "@" (or name zuid "?"))
                       'zoho-desk-mention (or zuid t)))))
     text t t)))

;;;; Comments

(defvar-local zoho-desk--compose-ticket-id nil)

(defvar zoho-desk-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'zoho-desk-comment-send)
    (define-key map (kbd "C-c C-k") #'zoho-desk-comment-abort)
    (define-key map (kbd "@") #'zoho-desk-electric-mention)
    map))

(define-derived-mode zoho-desk-comment-mode text-mode "ZohoComment"
  "Compose a Zoho Desk ticket comment.
\\<zoho-desk-comment-mode-map>Post with \\[zoho-desk-comment-send], \
abort with \\[zoho-desk-comment-abort]; @ completes an agent mention."
  (font-lock-add-keywords
   nil '((zoho-desk--mention-matcher (0 'zoho-desk-mention prepend)))))

(defun zoho-desk-add-comment ()
  "Compose a comment for the current ticket."
  (interactive)
  (let* ((ticket (or (zoho-desk--ticket-at-point)
                     (user-error "No ticket in context")))
         (buf (get-buffer-create
               (format "*zoho comment #%s*"
                       (alist-get 'ticketNumber ticket)))))
    (with-current-buffer buf
      (zoho-desk-comment-mode)
      (setq zoho-desk--compose-ticket-id (alist-get 'id ticket))
      (erase-buffer))
    (pop-to-buffer buf)
    (message "C-c C-c to post %s comment, C-c C-k to abort, @ mentions"
             (if zoho-desk-comments-public "a PUBLIC" "a private"))))

(defun zoho-desk--refresh-shown-ticket (ticket-id)
  "Background-refresh the ticket buffer when it still shows TICKET-ID.
Same rules as after a sent reply: only if the reusable ticket
buffer is live and on this ticket, keeping its tab, without
stealing focus."
  (let ((tbuf (and (buffer-live-p zoho-desk--ticket-buffer)
                   zoho-desk--ticket-buffer)))
    (when-let* ((ticket (and tbuf
                             (buffer-local-value 'zoho-desk--ticket tbuf))))
      (when (equal (alist-get 'id ticket) ticket-id)
        (zoho-desk--show-ticket ticket-id
                                (alist-get 'ticketNumber ticket)
                                (buffer-local-value
                                 'zoho-desk--current-tab tbuf)
                                t)))))

(defun zoho-desk--post-comment (ticket-id content buf on-success)
  "POST CONTENT as a comment on TICKET-ID, in the background.
Failure is announced by refocusing BUF, which still holds the
unsent text; success runs ON-SUCCESS and refreshes the ticket
buffer when it still shows this ticket, so the comment appears at
the top of the Comments list."
  (message "Posting comment to ticket %s…" ticket-id)
  (zoho-desk--request-async
   "POST" (format "/tickets/%s/comments" ticket-id)
   (lambda (_result err)
     (if err
         (zoho-desk--announce-write-failure "comment" err buf)
       (message "Comment posted to ticket %s" ticket-id)
       (funcall on-success)
       (zoho-desk--refresh-shown-ticket ticket-id)))
   :payload `(("content" . ,content)
              ("isPublic" . ,(if zoho-desk-comments-public t
                               :json-false)))))

(defun zoho-desk-comment-send ()
  "Post the comment in the current compose buffer, in the background.
The compose window closes immediately; the buffer is only killed
once the post succeeds, and is brought back should it fail."
  (interactive)
  (let ((content (string-trim (zoho-desk--mention-markup-string
                               (point-min) (point-max))))
        (ticket-id zoho-desk--compose-ticket-id)
        (buf (current-buffer)))
    (when (string-empty-p content)
      (user-error "Comment is empty"))
    (quit-window)
    (zoho-desk--post-comment
     ticket-id content buf
     (lambda ()
       (when (buffer-live-p buf)
         (kill-buffer buf))))))

(defun zoho-desk-submit-comment ()
  "Post the New Comment section of this ticket buffer.
The comment sends in the background; on success the ticket is
re-fetched so it appears at the top of the Comments list and the
field is emptied for the next one.  @mentions picked in the field
\(type @) notify the tagged agents.  `zoho-desk-comments-public'
decides whether the contact sees it."
  (interactive)
  (unless zoho-desk--ticket (user-error "Not in a ticket buffer"))
  (let* ((field (or (zoho-desk--comment-body-field)
                    (user-error "No New Comment section in this buffer")))
         (content (string-trim (zoho-desk--mention-markup-string
                                (car field) (cdr field)))))
    (when (string-empty-p content)
      (user-error "The New Comment field is empty"))
    (zoho-desk--post-comment (alist-get 'id zoho-desk--ticket) content
                             (current-buffer) #'ignore)))

(defun zoho-desk-comment-abort ()
  "Abort the comment being composed."
  (interactive)
  (kill-buffer))

;;;; Ticket status

(defvar zoho-desk--ticket-statuses nil
  "Cached status picklist of the tickets module, in layout order.")

(defun zoho-desk--ensure-ticket-statuses ()
  "Return the valid ticket status names, fetching them once per session.
The picklist is the `allowedValues' of the tickets module's
status field (GET /organizationFields) — the same list the web
UI's status dropdown offers."
  (or zoho-desk--ticket-statuses
      (setq zoho-desk--ticket-statuses
            (let* ((fields (alist-get 'data
                                      (zoho-desk--request
                                       "GET" "/organizationFields"
                                       :params '(("module" "tickets")))))
                   (status (seq-find
                            (lambda (field)
                              (equal (alist-get 'apiName field) "status"))
                            fields)))
              (or (mapcar (lambda (choice) (alist-get 'value choice))
                          (alist-get 'allowedValues status))
                  (error "Zoho Desk: no status picklist in the tickets module"))))))

;;;###autoload
(defun zoho-desk-set-status (ticket-id status &optional buf)
  "Set TICKET-ID's status to STATUS, in the background.
Interactively, the ticket is taken from the list line or ticket
buffer at point (falling back to a prompt) and STATUS is
completed from the status field's picklist, fetched from the API
once per session.  On success the ticket buffer or table the
update was issued from refreshes to show the new status."
  (interactive
   (let ((ticket (zoho-desk--ticket-at-point)))
     (list (if ticket
               (alist-get 'id ticket)
             (read-string "Ticket id: "))
           (completing-read (format "Status%s: "
                                    (if-let* ((current (alist-get 'status
                                                                  ticket)))
                                        (format " (now %s)" current)
                                      ""))
                            (zoho-desk--ensure-ticket-statuses) nil t)
           (current-buffer))))
  (message "Setting ticket %s to %s…" ticket-id status)
  (zoho-desk--request-async
   "PATCH" (format "/tickets/%s" ticket-id)
   (lambda (result err)
     (if err
         (zoho-desk--announce-write-failure
          (format "status update on ticket %s" ticket-id) err)
       (message "Ticket %s is now %s"
                (if-let* ((number (alist-get 'ticketNumber result)))
                    (format "#%s" number)
                  ticket-id)
                (or (alist-get 'status result) status))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (cond
            ;; Same rules as after a sent time log: refresh only if
            ;; the buffer still shows this ticket, without stealing
            ;; focus.
            ((equal (alist-get 'id zoho-desk--ticket) ticket-id)
             (zoho-desk--show-ticket ticket-id
                                     (alist-get 'ticketNumber
                                                zoho-desk--ticket)
                                     zoho-desk--current-tab t))
            ((derived-mode-p 'zoho-desk-tickets-mode)
             (zoho-desk--refresh-table)))))))
   :payload `(("status" . ,status))))

;;;; Time entries

(defun zoho-desk--format-duration (hours minutes seconds)
  "Format a duration for messages; SECONDS only when they matter."
  (concat (format "%dh %02dm" hours minutes)
          (if (zerop seconds) "" (format " %02ds" seconds))))

(defun zoho-desk--post-time-entry (ticket-id duration description
                                             &optional buf callback executed)
  "POST a time entry of DURATION with DESCRIPTION to TICKET-ID.
DURATION is a number of minutes or an (HOURS MINUTES SECONDS)
list, normalized either way; EXECUTED is the entry's executed
time as an Emacs time value, defaulting to now.  On failure BUF,
when given, is refocused (it still holds the unsent content); on
success CALLBACK, when given, is called with no arguments."
  (let* ((total (if (numberp duration)
                    (* 60 (round duration))
                  (+ (* 3600 (nth 0 duration))
                     (* 60 (nth 1 duration))
                     (nth 2 duration))))
         (hours (/ total 3600))
         (minutes (/ (% total 3600) 60))
         (seconds (% total 60))
         (pretty (zoho-desk--format-duration hours minutes seconds)))
    (message "Logging %s on ticket %s…" pretty ticket-id)
    (zoho-desk--request-async
     "POST" (format "/tickets/%s/timeEntry" ticket-id)
     (lambda (_result err)
       (if err
           (zoho-desk--announce-write-failure
            (format "time entry on ticket %s" ticket-id) err buf)
         (message "Logged %s on ticket %s" pretty ticket-id)
         (when callback (funcall callback))))
     :payload `(("hoursSpent" . ,(number-to-string hours))
                ("minutesSpent" . ,(number-to-string minutes))
                ("secondsSpent" . ,(number-to-string seconds))
                ("executedTime" . ,(format-time-string
                                    "%Y-%m-%dT%H:%M:%S.000Z" executed t))
                ("description" . ,description)))))

(defun zoho-desk-pick-executed-time ()
  "Fill the New Time Log's Executed field with the org date picker."
  (interactive)
  (let ((field (or (zoho-desk--time-executed-field)
                   (user-error "No New Time Log section in this buffer")))
        (time (org-read-date t t nil "Executed time: ")))
    (zoho-desk--set-field field
                          (format-time-string "[%Y-%m-%d %a %H:%M]" time))))

(defun zoho-desk-pick-end-time ()
  "Fill the New Time Log's End field with the org date picker."
  (interactive)
  (let ((field (or (zoho-desk--time-end-field)
                   (user-error "No New Time Log section in this buffer")))
        (time (org-read-date t t nil "End time: ")))
    (zoho-desk--set-field field
                          (format-time-string "[%Y-%m-%d %a %H:%M]" time))))

(defun zoho-desk-org-timestamp-dwim ()
  "Pick the Executed or End time when point is in either field,
else org's C-c .."
  (interactive)
  (let ((executed (zoho-desk--time-executed-field))
        (end (zoho-desk--time-end-field)))
    (cond
     ((and executed (<= (car executed) (point)) (>= (cdr executed) (point)))
      (zoho-desk-pick-executed-time))
     ((and end (<= (car end) (point)) (>= (cdr end) (point)))
      (zoho-desk-pick-end-time))
     (t (call-interactively (if (fboundp 'org-timestamp)
                                'org-timestamp
                              'org-time-stamp))))))

(defun zoho-desk-submit-time-log ()
  "Post the New Time Log section of this ticket buffer.
Reads the Executed time, the entry's duration and the description
typed below them (the Time Logs tab's counterpart of the Reply
section) and sends in the background; on success the ticket is
re-fetched so the new entry appears in the Time Logs list and the
fields are reset for the next one.

The duration comes from one of two places: an End time, making the
entry the Executed..End interval, or the Hours / Minutes / Seconds
values.  Giving both is refused rather than second-guessed."
  (interactive)
  (unless zoho-desk--ticket (user-error "Not in a ticket buffer"))
  (let* ((body (or (zoho-desk--time-body-field)
                   (user-error "No New Time Log section in this buffer")))
         (duration (list (zoho-desk--duration-input 'hours)
                         (zoho-desk--duration-input 'minutes)
                         (zoho-desk--duration-input 'seconds)))
         (executed (zoho-desk--time-executed-value))
         (end (zoho-desk--time-end-value))
         (description (string-trim (buffer-substring-no-properties
                                    (car body) (cdr body))))
         (buf (current-buffer))
         (ticket-id (alist-get 'id zoho-desk--ticket))
         (ticket-number (alist-get 'ticketNumber zoho-desk--ticket)))
    (cond
     ((and end (not executed))
      (user-error "An End time needs an Executed time to start from"))
     (end
      (when (cl-some #'cl-plusp duration)
        (user-error
         "Both an End time and a duration are set — clear one of them"))
      (let ((secs (round (float-time (time-subtract end executed)))))
        (unless (cl-plusp secs)
          (user-error "The End time must be after the Executed time"))
        (setq duration (list (/ secs 3600) (/ (% secs 3600) 60)
                             (% secs 60)))))
     ((zerop (+ (* 3600 (nth 0 duration))
                (* 60 (nth 1 duration))
                (nth 2 duration)))
      (user-error "The duration is empty")))
    (zoho-desk--post-time-entry
     ticket-id duration description buf
     (lambda ()
       ;; Only if the buffer still shows this ticket, and without
       ;; stealing focus — same rules as after a sent reply.
       (when (and (buffer-live-p buf)
                  (equal (alist-get 'id (buffer-local-value
                                         'zoho-desk--ticket buf))
                         ticket-id))
         (zoho-desk--show-ticket
          ticket-id ticket-number
          (buffer-local-value 'zoho-desk--current-tab buf)
          t)))
     executed)))

;;;###autoload
(defun zoho-desk-add-time-entry (ticket-id minutes description)
  "Add a time entry of MINUTES with DESCRIPTION to TICKET-ID.
Interactively, the ticket is taken from the list line or ticket
buffer at point, falling back to a prompt."
  (interactive
   (let ((ticket (zoho-desk--ticket-at-point)))
     (list (if ticket
               (alist-get 'id ticket)
             (read-string "Ticket id: "))
           (read-number "Minutes spent: " 30)
           (read-string "Description: "))))
  (zoho-desk--post-time-entry ticket-id (round minutes) description))

;;;###autoload
(defun zoho-desk-log-time-from-org ()
  "Send the clocked time of the org entry at point to Zoho Desk.
The target ticket is read from the ZOHO_TICKET_ID property
(inherited).  The prompt defaults to the entry's total clocked
minutes and its heading as description."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org buffer"))
  (require 'org-clock)
  (let* ((ticket-id (or (org-entry-get nil "ZOHO_TICKET_ID" t)
                        (user-error "No ZOHO_TICKET_ID property on this entry")))
         (clocked (save-excursion (org-clock-sum-current-item)))
         (minutes (read-number "Minutes to log: " (or clocked 0)))
         (description (read-string "Description: "
                                   (org-get-heading t t t t))))
    (zoho-desk--post-time-entry ticket-id (round minutes) description)))

;;;; Ticket timer (posframe-timer integration)

(defvar zoho-desk--timer-ticket nil
  "Ticket alist the posframe-timer clock is running against, or nil.")

(defvar zoho-desk--pending-time-log nil
  "Finished timer waiting to land in a ticket's New Time Log fields.
A list (TICKET-ID START END) of the clocked interval, consumed by
`zoho-desk--fill-pending-time-log' once a buffer showing TICKET-ID
is rendered.")

;;;###autoload
(defun zoho-desk-start-ticket-timer ()
  "Clock the posframe timer in against the ticket at point.
The ticket number and subject are shown above the running clock in
the posframe.  From anywhere in Emacs, finish with
`zoho-desk-finish-ticket-timer' (or `posframe-timer-clock-out') to
land the elapsed time in the ticket's New Time Log fields, ready to
describe and submit; discard with `zoho-desk-cancel-ticket-timer'."
  (interactive)
  (unless (require 'posframe-timer nil t)
    (user-error "The posframe-timer package is not available"))
  (let ((ticket (or (zoho-desk--ticket-at-point)
                    (user-error "No ticket in context"))))
    ;; clock-in user-errors when a clock is already running, so the
    ;; ticket is only remembered once the clock is really ours.
    (posframe-timer-clock-in
     (format "#%s %s"
             (alist-get 'ticketNumber ticket)
             (or (alist-get 'subject ticket) ""))
     #'zoho-desk--ticket-timer-out
     #'zoho-desk--ticket-timer-cancelled)
    (setq zoho-desk--timer-ticket ticket)))

;;;###autoload
(defun zoho-desk-finish-ticket-timer ()
  "Stop the ticket timer and open its New Time Log, interval filled in.
Callable from anywhere: the ticket buffer pops up on its Time Logs
tab with Executed and End set to the clocked start and end times —
describe the work and submit with `zoho-desk-submit-time-log'."
  (interactive)
  (unless zoho-desk--timer-ticket
    (user-error "No ticket timer running"))
  (posframe-timer-clock-out))

;;;###autoload
(defun zoho-desk-cancel-ticket-timer ()
  "Discard the ticket timer without logging anything."
  (interactive)
  (unless zoho-desk--timer-ticket
    (user-error "No ticket timer running"))
  (posframe-timer-clock-cancel))

(defun zoho-desk--ticket-timer-cancelled (_start _label)
  "Forget the ticket the discarded clock was running against."
  (setq zoho-desk--timer-ticket nil))

(defun zoho-desk--ticket-timer-out (start end _label)
  "Land the clocked interval START..END in the ticket's New Time Log.
The interval is parked in `zoho-desk--pending-time-log', then
the ticket buffer is brought up on its Time Logs tab: filled
immediately when it already shows the ticket, otherwise once the
fetch renders it."
  (let* ((ticket zoho-desk--timer-ticket)
         (id (format "%s" (alist-get 'id ticket)))
         (buf zoho-desk--ticket-buffer))
    ;; The Executed / End fields hold minute-resolution timestamps, so
    ;; a clock shorter than a minute is stretched to one: the interval
    ;; must survive the round trip through the rendered fields.
    (when (= (floor (float-time start) 60) (floor (float-time end) 60))
      (setq end (time-add start 60)))
    (setq zoho-desk--timer-ticket nil
          zoho-desk--pending-time-log (list id start end))
    (if (and (buffer-live-p buf)
             (equal id (format "%s" (alist-get 'id (buffer-local-value
                                                    'zoho-desk--ticket buf)))))
        (progn
          (pop-to-buffer buf
                         `((display-buffer-reuse-window
                            display-buffer-below-selected)
                           (window-height . ,zoho-desk-ticket-window-height)))
          (zoho-desk--set-tab "Time Logs")
          (zoho-desk--fill-pending-time-log))
      (zoho-desk--show-ticket id (alist-get 'ticketNumber ticket)
                              "Time Logs"))))

(defun zoho-desk--fill-pending-time-log ()
  "Write the pending timer interval into this buffer's New Time Log.
No-op unless `zoho-desk--pending-time-log' targets the ticket shown
here; the pending entry is consumed, and point lands in the
description field ready for `zoho-desk-submit-time-log'.  The
clocked start and end land in the Executed and End fields — the
duration values stay blank (they are the interval's alternative,
not its echo) and the duration is computed at submit time."
  (when-let* ((pending zoho-desk--pending-time-log)
              ((equal (car pending)
                      (format "%s" (alist-get 'id zoho-desk--ticket)))))
    (pcase-let ((`(,_id ,start ,end) pending))
      ;; Each field is looked up fresh because every insertion shifts
      ;; the positions of the fields after it.  The duration values
      ;; are reset to blank editable spaces (the rendered initial
      ;; state) in case the buffer carried leftovers from hand edits.
      (zoho-desk--set-field (zoho-desk--time-executed-field)
                            (format-time-string "[%Y-%m-%d %a %H:%M]" start))
      (zoho-desk--set-field (zoho-desk--time-end-field)
                            (format-time-string "[%Y-%m-%d %a %H:%M]" end))
      (zoho-desk--set-field (zoho-desk--duration-field 'hours) "    ")
      (zoho-desk--set-field (zoho-desk--duration-field 'minutes) "    ")
      (zoho-desk--set-field (zoho-desk--duration-field 'seconds) "    ")
      (setq zoho-desk--pending-time-log nil)
      (when-let* ((body (zoho-desk--time-body-field)))
        (goto-char (car body))
        ;; The tab switch collapsed every time log entry, New Time Log
        ;; included; open it back up so the filled fields are visible,
        ;; and land in the description ready to type.  Window points
        ;; are set explicitly because the render may happen while the
        ;; user's selected window is elsewhere.
        (save-excursion
          (org-back-to-heading t)
          (if (fboundp 'org-fold-show-subtree)
              (org-fold-show-subtree)
            (outline-show-subtree)))
        (dolist (win (get-buffer-window-list nil nil t))
          (set-window-point win (car body)))
        (when (and (fboundp 'evil-insert-state)
                   (bound-and-true-p evil-local-mode))
          (evil-insert-state)))
      (let ((secs (round (float-time (time-subtract end start)))))
        (message
         "Timer stopped: %s–%s (%s) — describe the work and %s to submit"
         (format-time-string "%H:%M" start)
         (format-time-string "%H:%M" end)
         (zoho-desk--format-duration (/ secs 3600) (/ (% secs 3600) 60)
                                     (% secs 60))
         (substitute-command-keys "\\[zoho-desk-submit-time-log]"))))))

;;;; Org snippet

(defun zoho-desk-copy-org-snippet ()
  "Copy an org heading with ZOHO_TICKET_ID for the ticket at point."
  (interactive)
  (let ((ticket (or (zoho-desk--ticket-at-point)
                    (user-error "No ticket in context"))))
    (kill-new (format "* TODO #%s %s\n:PROPERTIES:\n:ZOHO_TICKET_ID: %s\n:END:\n"
                      (alist-get 'ticketNumber ticket)
                      (alist-get 'subject ticket)
                      (alist-get 'id ticket)))
    (message "Org heading for #%s copied to kill ring"
             (alist-get 'ticketNumber ticket))))

;;;; Evil integration

;; Evil's state maps outrank major-mode maps, so RET and friends must be
;; registered with Evil directly (same approach as noumena-mode).
(with-eval-after-load 'evil
  (evil-set-initial-state 'zoho-desk-views-mode 'normal)
  (evil-set-initial-state 'zoho-desk-tickets-mode 'normal)
  (evil-define-key* 'normal zoho-desk-views-mode-map
    (kbd "RET") #'zoho-desk-toggle-view-at-point
    (kbd "SPC") #'zoho-desk-toggle-view-at-point
    (kbd "x") #'zoho-desk-toggle-view-at-point
    (kbd "<mouse-1>") #'zoho-desk-mouse-toggle-view
    (kbd "g r") #'zoho-desk-refresh-views
    (kbd "D") #'zoho-desk-select-department
    (kbd "q") #'zoho-desk-quit)
  (evil-define-key* 'normal zoho-desk-tickets-mode-map
    (kbd "RET") #'zoho-desk-open-ticket-at-point
    (kbd "g r") #'zoho-desk-refresh-table
    (kbd "t") #'zoho-desk-add-time-entry
    (kbd "T") #'zoho-desk-start-ticket-timer
    (kbd "u") #'zoho-desk-set-status
    (kbd "w") #'zoho-desk-copy-org-snippet
    (kbd "o") #'zoho-desk-browse-ticket
    (kbd "]") #'zoho-desk-next-page
    (kbd "[") #'zoho-desk-previous-page
    (kbd "q") #'zoho-desk-quit)
  (dolist (state '(normal motion))
    (evil-define-minor-mode-key state 'zoho-desk-ticket-minor-mode
      (kbd "gt") #'zoho-desk-tab-next
      (kbd "gT") #'zoho-desk-tab-previous))
  ;; evil-org's state maps outrank the plain minor-mode map, so field
  ;; cycling must be registered with Evil too.
  (dolist (state '(normal insert))
    (evil-define-minor-mode-key state 'zoho-desk-ticket-minor-mode
      (kbd "M-n") #'zoho-desk-next-field
      (kbd "M-p") #'zoho-desk-previous-field)))

(provide 'zoho-desk)
;;; zoho-desk.el ends here
