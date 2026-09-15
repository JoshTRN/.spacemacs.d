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
;;                                Thread and Comments tabs
;; - `zoho-desk-add-time-entry'   post a time entry to a ticket
;; - `zoho-desk-log-time-from-org' send org-clocked time to a ticket
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
;;   Desk.basic.ALL,Desk.tickets.ALL,Desk.settings.READ,Desk.search.READ,Desk.contacts.READ
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

(declare-function org-mode "org")
(declare-function org-entry-get "org")
(declare-function org-get-heading "org")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-meta-data "org")
(declare-function org-end-of-subtree "org")
(declare-function org-narrow-to-subtree "org")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-show-all "org")
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

(defcustom zoho-desk-department-reply-from nil
  "Alist mapping departmentId to the support address used as From.
Filled in automatically when you confirm the From prompt on a
ticket whose department has no known support address yet."
  :type '(alist :key-type string :value-type string))

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

;;;###autoload
(defun zoho-desk-authorize (code)
  "Exchange self-client grant CODE for a refresh token.
Generate the code at https://api-console.zoho.com under your Self
Client's \"Generate Code\" tab with scopes
Desk.basic.ALL,Desk.tickets.ALL,Desk.settings.READ,
Desk.search.READ,Desk.contacts.READ (as one comma-separated
line).  Offers to persist the refresh token into authinfo so this
is a one-time step per Zoho account."
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
                                           content-type no-org (retries 1))
  "Perform METHOD PATH in the background; CALLBACK gets (RESULT ERR).
The non-blocking counterpart of `zoho-desk--request'; the keyword
arguments mean the same.  CALLBACK is invoked exactly once, with
the parsed JSON response and nil, or with nil and an error message
string.  It may run in an arbitrary buffer, so it must
`with-current-buffer' its target."
  (let ((opts (list :params params :payload payload
                    :raw-payload raw-payload :content-type content-type
                    :no-org no-org :retries retries)))
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
                       (body (zoho-desk--response-body)))
                   (kill-buffer)
                   (cond
                    (finished)
                    (net-error
                     (funcall finish nil (format "%s %s: %S"
                                                 method path net-error)))
                    ((and (eq code 401) (> retries 0))
                     (setq finished t
                           zoho-desk--access-token nil)
                     (apply #'zoho-desk--request-async method path callback
                            (plist-put (copy-sequence opts)
                                       :retries (1- retries))))
                    ((memq code '(200 201))
                     (condition-case err
                         (funcall finish
                                  (unless (string-empty-p (string-trim body))
                                    (zoho-desk--parse-json body))
                                  nil)
                       (error (funcall finish nil
                                       (error-message-string err)))))
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

(defun zoho-desk--org-body (text)
  "Indent TEXT two spaces so it can never form org headings.
Trailing whitespace is stripped and empty lines stay truly empty."
  (let ((clean (replace-regexp-in-string
                "[ \t]+$" "" (string-trim (or text "")))))
    (concat (replace-regexp-in-string "^." "  \\&" clean) "\n")))

(defun zoho-desk--html-to-org-body (html)
  "Convert HTML to indented plain text usable as an org entry body."
  (zoho-desk--org-body (zoho-desk--html-to-text html)))

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
    (define-key map (kbd "w") #'zoho-desk-copy-org-snippet)
    (define-key map (kbd "o") #'zoho-desk-browse-ticket)
    (define-key map (kbd "]") #'zoho-desk-next-page)
    (define-key map (kbd "[") #'zoho-desk-previous-page)
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
  (when-let* ((web (alist-get 'webUrl ticket))
              (id (alist-get 'id ticket))
              (dept-id (alist-get 'departmentId ticket)))
    (when (string-match
           "\\`\\(https://[^/]+\\)/\\(?:support\\|portal\\)/\\([^/]+\\)/" web)
      ;; Grab the matches before `zoho-desk--ensure-departments' can
      ;; clobber the match data with its own regexp work.
      (let ((base (match-string 1 web))
            (portal (match-string 2 web)))
        (when-let* ((dept (seq-find
                           (lambda (d)
                             (equal (format "%s" (alist-get 'id d))
                                    (format "%s" dept-id)))
                           (zoho-desk--ensure-departments)))
                    (slug (alist-get 'sanitizedName dept)))
          (format "%s/agent/%s/%s/tickets/details/%s"
                  base portal slug id))))))

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

;;;; Ticket document (org-mode)

(defvar zoho-desk-ticket-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c z 1") #'zoho-desk-tab-overview)
    (define-key map (kbd "C-c z 2") #'zoho-desk-tab-thread)
    (define-key map (kbd "C-c z 3") #'zoho-desk-tab-comments)
    (define-key map (kbd "C-c z s") #'zoho-desk-send-reply)
    (define-key map (kbd "C-c z a") #'zoho-desk-add-email)
    (define-key map (kbd "C-c z i") #'zoho-desk-insert-image)
    (define-key map (kbd "C-c z e") #'zoho-desk-expand-thread-at-point)
    (define-key map (kbd "C-c z c") #'zoho-desk-add-comment)
    (define-key map (kbd "C-c z t") #'zoho-desk-add-time-entry)
    (define-key map (kbd "C-c z g") #'zoho-desk-refresh-ticket)
    (define-key map (kbd "C-c z o") #'zoho-desk-browse-ticket)
    (define-key map (kbd "C-c z w") #'zoho-desk-copy-org-snippet)
    (define-key map (kbd "C-c z y") #'zoho-desk-copy-ticket-url)
    (define-key map (kbd "C-c z #") #'zoho-desk-copy-ticket-number)
    map))

(defvar-local zoho-desk--current-tab nil
  "Heading of the currently narrowed tab, nil for Overview.")

(defconst zoho-desk--tabs
  '(("Overview" . nil)
    ("Thread" . "Email Thread")
    ("Comments" . "Comments"))
  "Tab labels and the top-level org heading each narrows to.")

(defvar-local zoho-desk--threads nil
  "Thread list of the ticket in this buffer, newest first.")

(defun zoho-desk--input-field-matcher (limit)
  "Font-lock matcher for the reply input field backgrounds.
Matches the next stretch of the To address or reply body before
LIMIT.  Registered with the `append' override, so faces org has
already applied — src block and quote backgrounds included — keep
precedence over the field background."
  (let* ((to (zoho-desk--reply-to-field))
         (fields (delq nil
                       ;; The To chunk takes in its line's newline so
                       ;; the `:extend' background runs to the window
                       ;; edge; the body field already ends with its
                       ;; own newline, just before the end separator's.
                       (list (and to (cons (car to) (1+ (cdr to))))
                             (zoho-desk--reply-body-field))))
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

(defconst zoho-desk--input-font-lock-keywords
  '((zoho-desk--input-field-matcher (0 'zoho-desk-input append)))
  "Font-lock keywords painting the reply input field backgrounds.
Appended after org's own keywords by
`zoho-desk-ticket-minor-mode', with the `append' face override, so
the field background sits under whatever org fontifies inside the
reply — code blocks stay darker than the field.")

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
   (propertize "   (click, C-c z 1-3, or gt)" 'face 'shadow)))

(defun zoho-desk--set-tab (heading)
  "Narrow the ticket buffer to HEADING, or widen when nil."
  (widen)
  (setq zoho-desk--current-tab heading)
  (goto-char (point-min))
  (when heading
    (if (re-search-forward (concat "^\\* " (regexp-quote heading) "$") nil t)
        (progn (beginning-of-line)
               (org-narrow-to-subtree)
               (goto-char (point-min)))
      (message "No %s section in this ticket" heading)))
  (if (fboundp 'org-fold-show-all) (org-fold-show-all) (org-show-all))
  (force-mode-line-update))

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

(defun zoho-desk--render-ticket-org (ticket threads comments)
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
    (insert "* Details\n"
            (format "- Status :: %s\n" (or (alist-get 'status ticket) "-"))
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
                        'rear-nonsticky '(read-only))
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
    (insert "* Comments\n")
    (if (null comments)
        (insert "  No comments.\n")
      (dolist (comment comments)
        (insert (format "** %s %s\n"
                        (zoho-desk--org-timestamp
                         (alist-get 'commentedTime comment))
                        (zoho-desk--person-name
                         (alist-get 'commenter comment) "unknown")))
        (let ((content (alist-get 'content comment)))
          (insert (if (equal (alist-get 'contentType comment) "html")
                      (zoho-desk--html-to-org-body content)
                    (zoho-desk--org-body content))))))
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

(defun zoho-desk--reply-body-field ()
  "Return (START . END) of the editable reply body area, or nil.
The area sits between the separator newlines marked
`zoho-desk-body-start' and `zoho-desk-body-end' at render time."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((sep (text-property-any (point-min) (point-max)
                                          'zoho-desk-body-start t))
                  (end (text-property-any sep (point-max)
                                          'zoho-desk-body-end t)))
        (cons (1+ sep) end)))))

(defun zoho-desk--protect-buffer ()
  "Make everything except the Reply input fields read-only.
Only the To line's address and the reply body between its
separator lines stay editable; the separators themselves are
locked so the layout survives any edit."
  (let ((inhibit-read-only t))
    (save-excursion
      (save-restriction
        (widen)
        (let ((to-field (zoho-desk--reply-to-field))
              (body (zoho-desk--reply-body-field)))
          (if (not (and to-field body))
              (add-text-properties (point-min) (point-max)
                                   '(read-only t front-sticky (read-only)))
            (add-text-properties (point-min) (car to-field)
                                 '(read-only t
                                   front-sticky (read-only)
                                   rear-nonsticky (read-only)))
            ;; To-line newline: rear-sticky (the default) so nothing
            ;; can be typed between it and the separator below.
            (add-text-properties (cdr to-field) (1- (car body))
                                 '(read-only t))
            ;; Separator before the body: rear-nonsticky so typing at
            ;; the body's start stays legal.
            (add-text-properties (1- (car body)) (car body)
                                 '(read-only t rear-nonsticky (read-only)))
            ;; No front-sticky from the end separator on: text typed
            ;; at the end of the body must stay editable.
            (add-text-properties (cdr body) (point-max)
                                 '(read-only t))))))))

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
       ;; Comments are best-effort, as before (ignore-errors then).
       ("GET" ,(format "/tickets/%s/comments" id)
        :params (("limit" 50)) :soft-errors t))
     (lambda (results err)
       (when (= generation zoho-desk--show-ticket-generation)
         (if err
             (zoho-desk--show-fetch-error buf err)
           (zoho-desk--prefetch-thread-bodies
            id generation buf tab
            (nth 0 results)
            (alist-get 'data (nth 1 results))
            (alist-get 'data (nth 2 results)))))))))

(defun zoho-desk--prefetch-thread-bodies (id generation buf tab
                                             ticket threads comments)
  "Fetch full bodies of TICKET's newest THREADS, then render into BUF.
Only the newest `zoho-desk-thread-prefetch' THREADS are fetched;
each fetched body is merged into its thread alist as `content'.
GENERATION guards against a newer fetch; TAB is the tab to select."
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
          comments tab))))))

(defun zoho-desk--render-ticket (buf ticket threads comments tab)
  "Fill BUF with TICKET's org document and select TAB."
  (with-current-buffer buf
    (rename-buffer (format "*zoho #%s*" (alist-get 'ticketNumber ticket)) t)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer))
    (zoho-desk--render-ticket-org ticket threads comments)
    (setq zoho-desk--ticket ticket
          zoho-desk--threads threads
          buffer-offer-save nil)
    (zoho-desk-ticket-minor-mode 1)
    (zoho-desk--sync-accent-faces)
    (zoho-desk--normalize-buffer-style)
    (zoho-desk--protect-buffer)
    (zoho-desk--set-tab tab)))

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
          (zoho-desk--protect-buffer))))))

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

(defun zoho-desk--reply-to-field ()
  "Return (START . END) of the To line's editable address area, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((label (next-single-property-change
                          (point-min) 'zoho-desk-to-label))
                  (start (next-single-property-change
                          label 'zoho-desk-to-label)))
        (goto-char start)
        (cons start (line-end-position))))))

(defun zoho-desk--reply-to-address ()
  "Return the address written on the To line, or nil when blank."
  (when-let* ((field (zoho-desk--reply-to-field)))
    (let ((address (string-trim (buffer-substring-no-properties
                                 (car field) (cdr field)))))
      (unless (string-empty-p address) address))))

(defun zoho-desk--set-reply-to-address (address)
  "Write ADDRESS into the To line, replacing its current content."
  (when-let* ((field (zoho-desk--reply-to-field)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (car field))
        (delete-region (car field) (cdr field))
        (insert address)))))

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

(defun zoho-desk--org-to-html (org-text images)
  "Export ORG-TEXT to body-only HTML, with IMAGES as attachment markers.
True inline embedding is impossible through the public API: Zoho
strips cid: and data: img references at send time, and its own
inline mechanism (ImageDisplay blockId URLs converted to MIME cid
parts) is only reachable with a browser session.  So each image
is sent as a regular attachment and its img tag becomes a
\"[image: NAME -- attached]\" marker in the body.  Single
newlines are kept as line breaks so the email reads like the
compose buffer."
  (require 'ox-html)
  (let ((html (zoho-desk--email-safe-block-styles
               (org-export-string-as org-text 'html t
                                     '(:preserve-breaks t)))))
    (dolist (file images html)
      (setq html (replace-regexp-in-string
                  (concat "<img [^>]*src=\"\\(?:file://\\)?"
                          (regexp-quote file) "\"[^>]*>")
                  (format "<em>[image: %s &mdash; attached]</em>"
                          (file-name-nondirectory file))
                  html t t)))))

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
             (images (and zoho-desk-reply-html
                          (zoho-desk--reply-image-files body)))
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
        (if (null images)
            (funcall send nil)
          (zoho-desk--request-all-async
           (mapcar #'zoho-desk--upload-spec images)
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

;;;; Comments

(defvar-local zoho-desk--compose-ticket-id nil)

(defvar zoho-desk-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'zoho-desk-comment-send)
    (define-key map (kbd "C-c C-k") #'zoho-desk-comment-abort)
    map))

(define-derived-mode zoho-desk-comment-mode text-mode "ZohoComment"
  "Compose a Zoho Desk ticket comment.
\\<zoho-desk-comment-mode-map>Post with \\[zoho-desk-comment-send], \
abort with \\[zoho-desk-comment-abort].")

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
    (message "C-c C-c to post %s comment, C-c C-k to abort"
             (if zoho-desk-comments-public "a PUBLIC" "a private"))))

(defun zoho-desk-comment-send ()
  "Post the comment in the current compose buffer, in the background.
The compose window closes immediately; the buffer is only killed
once the post succeeds, and is brought back should it fail."
  (interactive)
  (let ((content (string-trim (buffer-string)))
        (ticket-id zoho-desk--compose-ticket-id)
        (buf (current-buffer)))
    (when (string-empty-p content)
      (user-error "Comment is empty"))
    (message "Posting comment to ticket %s…" ticket-id)
    (quit-window)
    (zoho-desk--request-async
     "POST" (format "/tickets/%s/comments" ticket-id)
     (lambda (_result err)
       (if err
           (zoho-desk--announce-write-failure "comment" err buf)
         (message "Comment posted to ticket %s" ticket-id)
         (when (buffer-live-p buf)
           (kill-buffer buf))))
     :payload `(("content" . ,content)
                ("isPublic" . ,(if zoho-desk-comments-public t
                                 :json-false))))))

(defun zoho-desk-comment-abort ()
  "Abort the comment being composed."
  (interactive)
  (kill-buffer))

;;;; Time entries

(defun zoho-desk--post-time-entry (ticket-id minutes description)
  "POST a time entry of MINUTES with DESCRIPTION to TICKET-ID."
  (message "Logging %dh %02dm on ticket %s…"
           (/ minutes 60) (% minutes 60) ticket-id)
  (zoho-desk--request-async
   "POST" (format "/tickets/%s/timeEntry" ticket-id)
   (lambda (_result err)
     (if err
         (zoho-desk--announce-write-failure
          (format "time entry on ticket %s" ticket-id) err)
       (message "Logged %dh %02dm on ticket %s"
                (/ minutes 60) (% minutes 60) ticket-id)))
   :payload `(("hoursSpent" . ,(number-to-string (/ minutes 60)))
              ("minutesSpent" . ,(number-to-string (% minutes 60)))
              ("executedTime" . ,(format-time-string
                                  "%Y-%m-%dT%H:%M:%S.000Z" nil t))
              ("description" . ,description))))

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
    (kbd "w") #'zoho-desk-copy-org-snippet
    (kbd "o") #'zoho-desk-browse-ticket
    (kbd "]") #'zoho-desk-next-page
    (kbd "[") #'zoho-desk-previous-page
    (kbd "q") #'zoho-desk-quit)
  (dolist (state '(normal motion))
    (evil-define-minor-mode-key state 'zoho-desk-ticket-minor-mode
      (kbd "gt") #'zoho-desk-tab-next
      (kbd "gT") #'zoho-desk-tab-previous)))

(provide 'zoho-desk)
;;; zoho-desk.el ends here
