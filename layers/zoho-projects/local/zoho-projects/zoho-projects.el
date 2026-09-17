;;; zoho-projects.el --- Zoho Projects tasks and time logs inside Emacs  -*- lexical-binding: t; -*-

;; Author: joshua
;; Keywords: tools, comm
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A Zoho Projects client shaped like the zoho-desk dashboard:
;;
;; - `zoho-projects-dashboard'      pick a project, then two panes: the
;;                                  project's task statuses as OR-filter
;;                                  checkboxes on the left, the checked
;;                                  statuses' tasks on the right
;; - `zoho-projects-quickfind'      helm over a project's tasks, one
;;                                  section per status
;; - RET on a task                  org-mode task document with Overview,
;;                                  Comments and Time Logs tabs
;; - `zoho-projects-add-time-entry' post a time log to a task
;; - `zoho-projects-log-time-from-org' send org-clocked time to a task
;; - `zoho-projects-start-task-timer' clock the posframe timer in against
;;                                  a task; `zoho-projects-finish-task-timer'
;;                                  (from anywhere) fills the task's New
;;                                  Time Log fields with the elapsed time
;; - `zoho-projects-authorize'      one-time exchange of a self-client
;;                                  grant code for a refresh token
;;
;; All API traffic is non-blocking, same contract as zoho-desk: reads
;; pop up a buffer immediately with a "Fetching …" placeholder that
;; fills in when the responses arrive; writes (time logs, comments)
;; send in the background — success lands in the echo area, failure
;; refocuses the buffer holding the unsent content with a loud error.
;;
;; Zoho Projects is a separate product from Zoho Desk with its own API
;; (https://projectsapi.zoho.com/restapi) and its own OAuth scopes:
;;
;;   ZohoProjects.portals.READ,ZohoProjects.projects.READ,
;;   ZohoProjects.tasklists.READ,ZohoProjects.tasks.ALL,
;;   ZohoProjects.timesheets.ALL,ZohoProjects.users.READ
;;
;; The same Self Client from the zoho-desk setup works: generate a new
;; grant code with the scopes above and run M-x zoho-projects-authorize.
;; Store the secrets in ~/.authinfo.gpg:
;;
;;   machine zoho-projects login refresh-token password 1000.XXXX
;;
;; The client id and secret are looked up under host `zoho-projects'
;; first and fall back to the `zoho-desk' entries, so when both layers
;; share one Self Client only the refresh-token line is new.

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
(declare-function outline-hide-subtree "outline")

(declare-function org-mode "org")
(declare-function org-entry-get "org")
(declare-function org-get-heading "org")
(declare-function org-back-to-heading "org")
(declare-function org-narrow-to-subtree "org")
(declare-function org-end-of-subtree "org")
(declare-function org-read-date "org")
(declare-function org-time-string-to-time "org")
(declare-function org-time-stamp "org")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-show-all "org")
(declare-function org-fold-hide-subtree "org-fold")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-define-minor-mode-key "evil-core")

;;;; Configuration

(defgroup zoho-projects nil
  "Zoho Projects client."
  :group 'tools
  :prefix "zoho-projects-")

(defcustom zoho-projects-base-url "https://projectsapi.zoho.com/restapi"
  "Base URL of the Zoho Projects API.
Use the domain matching your data center, e.g. projectsapi.zoho.eu
or projectsapi.zoho.in."
  :type 'string)

(defcustom zoho-projects-accounts-url "https://accounts.zoho.com"
  "Zoho accounts server used for OAuth token exchange.
Must match the data center of `zoho-projects-base-url'."
  :type 'string)

(defcustom zoho-projects-client-id nil
  "OAuth client id of the Zoho self client.
When nil, looked up in auth-source as host `zoho-projects', login
`client-id', falling back to the `zoho-desk' entry so both layers
can share one Self Client."
  :type '(choice (const nil) string))

(defcustom zoho-projects-client-secret nil
  "OAuth client secret of the Zoho self client.
When nil, looked up in auth-source as host `zoho-projects', login
`client-secret', falling back to the `zoho-desk' entry."
  :type '(choice (const nil) string))

(defcustom zoho-projects-refresh-token nil
  "OAuth refresh token (obtain with `zoho-projects-authorize').
When nil, looked up in auth-source as host `zoho-projects', login
`refresh-token'.  No zoho-desk fallback here: Desk and Projects
tokens carry different scopes."
  :type '(choice (const nil) string))

(defcustom zoho-projects-portal-id nil
  "Zoho Projects portal id.
When nil and the token sees exactly one portal, that portal is
used; several portals prompt once per session."
  :type '(choice (const nil) string))

(defcustom zoho-projects-project-status "active"
  "Status filter for the projects sidebar (active/archived/template)."
  :type 'string)

(defcustom zoho-projects-project-fetch-limit 200
  "How many projects are fetched for the sidebar (parallel pages of 100)."
  :type 'integer)

(defcustom zoho-projects-task-fetch-limit 500
  "How many of a project's tasks are fetched, across every status.
The dashboard fetches this many tasks (in parallel pages of 100)
as one pool; the sidebar's status filters and quickfind both draw
from it."
  :type 'integer)

(defcustom zoho-projects-sidebar-width 42
  "Width of the projects sidebar in the dashboard."
  :type 'integer)

(defcustom zoho-projects-task-window-height 0.85
  "Height of the task window opened below the task table.
A float is a fraction of the frame height, an integer a number of
lines."
  :type '(choice (float :tag "Fraction of frame height")
                 (integer :tag "Lines")))

(defcustom zoho-projects-default-bill-status "Billable"
  "Bill status prefilled in the New Time Log's Billing field.
One of \"Billable\" or \"Non Billable\"."
  :type '(choice (const "Billable") (const "Non Billable")))

(defcustom zoho-projects-timelog-months 12
  "How many months of project time logs a task's Time Logs tab scans.
The v1 API only serves time logs per project and month (there is
no task-scoped GET), so the task document fetches the months
spanning the task's dates — capped at this many, newest first —
in parallel and filters the logs down to the task."
  :type 'integer)

(defcustom zoho-projects-pandoc-program "pandoc"
  "Pandoc executable used to convert incoming HTML to org markup.
Task descriptions and comments arrive as HTML; with pandoc they
keep their formatting as org markup.  When nil, or when the
program is not installed, they degrade to plain text."
  :type '(choice (string :tag "Program") (const :tag "Disabled" nil)))

(defcustom zoho-projects-request-timeout 30
  "Timeout in seconds for API requests."
  :type 'integer)

;;;; Faces

(defface zoho-projects-accent
  '((t :weight bold))
  "Accent text face for highlights (task keys, the selected project).
Synced to the modeline accent by `zoho-projects--sync-accent-faces'.")

(defface zoho-projects-tab-active
  '((t :weight bold :box t))
  "Face of the active header-line tab.
Synced to the modeline accent by `zoho-projects--sync-accent-faces'.")

(defface zoho-projects-input
  '((t :extend t))
  "Background of the editable input fields in task buffers.
Synced to the midpoint of the default and solaire backgrounds by
`zoho-projects--sync-accent-faces'.")

(defun zoho-projects--blend-colors (a b)
  "Return the hex color midway between color names A and B.
Nil when either name does not resolve (e.g. tty frames)."
  (let ((a (color-name-to-rgb a))
        (b (color-name-to-rgb b)))
    (when (and a b)
      (apply #'color-rgb-to-hex
             `(,@(cl-mapcar (lambda (x y) (/ (+ x y) 2)) a b) 2)))))

(defun zoho-projects--sync-accent-faces ()
  "Derive the zoho-projects faces from the theme's modeline accent.
Same derivation as zoho-desk's, so the two dashboards match."
  (when-let* ((bg (face-background 'default nil t))
              (block-bg (or (and (facep 'solaire-default-face)
                                 (face-background 'solaire-default-face
                                                  nil t))
                            (face-background 'org-block nil t)))
              (mid (zoho-projects--blend-colors bg block-bg)))
    (set-face-attribute 'zoho-projects-input nil :background mid))
  (when-let* ((accent (or (and (facep 'powerline-active2)
                               (face-background 'powerline-active2 nil t))
                          (face-background 'mode-line nil t))))
    (set-face-attribute 'zoho-projects-tab-active nil
                        :background accent
                        :foreground (or (face-foreground 'powerline-active2
                                                         nil t)
                                        (face-foreground 'mode-line nil t)
                                        'unspecified)
                        :weight 'bold :box t)
    (set-face-attribute 'zoho-projects-accent nil
                        :foreground (or (ignore-errors
                                          (color-lighten-name accent 25))
                                        accent)
                        :weight 'bold)))

;;;; Authentication

(defvar zoho-projects--access-token nil)
(defvar zoho-projects--token-expiry 0)

(defun zoho-projects--secret (login &optional host)
  "Return the auth-source password for HOST (zoho-projects) and LOGIN."
  (when-let* ((entry (car (auth-source-search :host (or host "zoho-projects")
                                              :user login :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun zoho-projects--credential (var login &optional no-desk-fallback)
  "Return VAR if non-nil, else the auth-source secret for LOGIN.
Unless NO-DESK-FALLBACK, the `zoho-desk' host is tried after
`zoho-projects' — the client id/secret of a shared Self Client
live there; the refresh token never falls back (different scopes)."
  (or var (zoho-projects--secret login)
      (unless no-desk-fallback (zoho-projects--secret login "zoho-desk"))
      (user-error
       "Zoho Projects: no %s configured (see zoho-projects.el commentary)"
       login)))

(defun zoho-projects--parse-json (string)
  "Parse STRING as JSON into nested alists and lists."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun zoho-projects--response-body ()
  "Return the decoded body of the url response in the current buffer."
  (goto-char (or url-http-end-of-headers (point-min)))
  (decode-coding-string
   (buffer-substring-no-properties (point) (point-max)) 'utf-8))

(defun zoho-projects--token-request (params)
  "POST PARAMS to the Zoho OAuth token endpoint, return parsed JSON."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (url-build-query-string params))
         (buf (url-retrieve-synchronously
               (concat zoho-projects-accounts-url "/oauth/v2/token")
               t t zoho-projects-request-timeout)))
    (unless buf (error "Zoho Projects: token request timed out"))
    (with-current-buffer buf
      (unwind-protect
          (zoho-projects--parse-json (zoho-projects--response-body))
        (kill-buffer buf)))))

(defun zoho-projects--refresh-access-token ()
  "Obtain a fresh access token using the refresh token."
  (let* ((response
          (zoho-projects--token-request
           `(("refresh_token" ,(zoho-projects--credential
                                zoho-projects-refresh-token "refresh-token" t))
             ("client_id" ,(zoho-projects--credential
                            zoho-projects-client-id "client-id"))
             ("client_secret" ,(zoho-projects--credential
                                zoho-projects-client-secret "client-secret"))
             ("grant_type" "refresh_token"))))
         (token (alist-get 'access_token response)))
    (unless token
      (error "Zoho Projects: token refresh failed: %S" response))
    (setq zoho-projects--access-token token
          zoho-projects--token-expiry
          (+ (float-time)
             (- (or (alist-get 'expires_in response) 3600) 60)))
    token))

(defun zoho-projects--ensure-token ()
  "Return a valid access token, refreshing if necessary."
  (if (and zoho-projects--access-token
           (< (float-time) zoho-projects--token-expiry))
      zoho-projects--access-token
    (zoho-projects--refresh-access-token)))

(defun zoho-projects--persist-refresh-token (refresh)
  "Save REFRESH as the zoho-projects refresh-token authinfo line.
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
        (flush-lines "^machine zoho-projects login refresh-token ")
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert
         (format "machine zoho-projects login refresh-token password %s\n"
                 refresh))
        (write-region (point-min) (point-max) file nil 'silent))
      (set-file-modes file #o600)
      (auth-source-forget-all-cached)
      (message "Refresh token saved to %s — no more grant codes needed"
               file)
      t)))

;;;###autoload
(defun zoho-projects-authorize (code)
  "Exchange self-client grant CODE for a refresh token.
Generate the code at https://api-console.zoho.com under your Self
Client's \"Generate Code\" tab with scopes
ZohoProjects.portals.READ,ZohoProjects.projects.READ,
ZohoProjects.tasklists.READ,ZohoProjects.tasks.ALL,
ZohoProjects.timesheets.ALL,ZohoProjects.users.READ (as one
comma-separated line).  Offers to persist the refresh token into
authinfo so this is a one-time step per Zoho account."
  (interactive "sGrant code from Zoho API console: ")
  (let* ((response
          (zoho-projects--token-request
           `(("code" ,code)
             ("client_id" ,(zoho-projects--credential
                            zoho-projects-client-id "client-id"))
             ("client_secret" ,(zoho-projects--credential
                                zoho-projects-client-secret "client-secret"))
             ("grant_type" "authorization_code"))))
         (refresh (alist-get 'refresh_token response)))
    (unless refresh
      (error "Zoho Projects: authorization failed: %S" response))
    (setq zoho-projects-refresh-token refresh)
    (when-let* ((token (alist-get 'access_token response)))
      (setq zoho-projects--access-token token
            zoho-projects--token-expiry (+ (float-time) 3000)))
    (unless (zoho-projects--persist-refresh-token refresh)
      (kill-new refresh)
      (message (concat "Refresh token copied to kill ring.  Save it as: "
                       "machine zoho-projects login refresh-token password "
                       "<token> in your authinfo file")))))

;;;; HTTP
;;
;; The Projects v1 API differs from Desk's in three ways this layer
;; has to care about: writes take form-encoded parameters instead of
;; JSON bodies, empty collections come back as 204 No Content, and
;; some errors arrive as HTTP 200 with an {"error": ...} body.

(defun zoho-projects--request-url (path params)
  "Return the full request URL for PATH with query PARAMS, as unibyte."
  (encode-coding-string
   (concat zoho-projects-base-url path
           (and params (concat "?" (url-build-query-string params))))
   'utf-8))

(defun zoho-projects--request-headers (token content-type)
  "Build the request header alist from TOKEN and CONTENT-TYPE.
Header values must be unibyte (see zoho-desk's identical helper)."
  (mapcar
   (lambda (header)
     (cons (car header)
           (encode-coding-string (format "%s" (cdr header)) 'utf-8)))
   (append
    `(("Authorization" . ,(concat "Zoho-oauthtoken " token)))
    (when content-type `(("Content-Type" . ,content-type))))))

(defun zoho-projects--body-error (result)
  "Return the error message inside RESULT, or nil.
The v1 API sometimes answers HTTP 200 with an error object."
  (when-let* ((err (and (listp result) (alist-get 'error result))))
    (format "%s (code %s)"
            (or (alist-get 'message err) err)
            (or (alist-get 'code err) "?"))))

(cl-defun zoho-projects--request (method path &key params payload (retries 1))
  "Perform an authenticated METHOD request against PATH, blocking.
PARAMS is a query-parameter list for `url-build-query-string';
PAYLOAD is a parameter list of the same shape, form-encoded as the
body (the v1 API takes form parameters, not JSON).  Returns the
parsed JSON response, or nil for empty (204) responses.  Only the
interactive portal prompt still blocks here; everything else goes
through `zoho-projects--request-async'."
  (let* ((url (zoho-projects--request-url path params))
         (url-request-method method)
         (url-request-extra-headers
          (zoho-projects--request-headers
           (zoho-projects--ensure-token)
           (and payload "application/x-www-form-urlencoded")))
         (url-request-data (when payload (url-build-query-string payload)))
         (buf (url-retrieve-synchronously
               url t t zoho-projects-request-timeout))
         status body)
    (unless buf
      (error "Zoho Projects: request timed out: %s %s" method path))
    (with-current-buffer buf
      (setq status url-http-response-status
            body (zoho-projects--response-body))
      (kill-buffer buf))
    (cond
     ((and (eq status 401) (> retries 0))
      (setq zoho-projects--access-token nil)
      (zoho-projects--request method path :params params :payload payload
                              :retries (1- retries)))
     ((memq status '(200 201))
      (let ((result (unless (string-empty-p (string-trim body))
                      (zoho-projects--parse-json body))))
        (if-let* ((err (zoho-projects--body-error result)))
            (error "Zoho Projects API %s %s failed: %s" method path err)
          result)))
     ((memq status '(204)) nil)
     (t (error "Zoho Projects API %s %s failed (HTTP %s): %s"
               method path status (string-trim body))))))

;;;; Async HTTP
;;
;; Same contract as zoho-desk's: commands render a "Fetching …"
;; placeholder immediately and a `url-retrieve' callback fills the
;; buffer in when the response lands.  Each fetching command bumps a
;; generation counter and its callback checks it, so a stale response
;; can never clobber a newer one.

(defun zoho-projects--ensure-token-async (callback)
  "Call CALLBACK with (TOKEN ERR), refreshing the token if expired."
  (if (and zoho-projects--access-token
           (< (float-time) zoho-projects--token-expiry))
      (funcall callback zoho-projects--access-token nil)
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data
           (url-build-query-string
            `(("refresh_token" ,(zoho-projects--credential
                                 zoho-projects-refresh-token
                                 "refresh-token" t))
              ("client_id" ,(zoho-projects--credential
                             zoho-projects-client-id "client-id"))
              ("client_secret" ,(zoho-projects--credential
                                 zoho-projects-client-secret "client-secret"))
              ("grant_type" "refresh_token")))))
      (url-retrieve
       (concat zoho-projects-accounts-url "/oauth/v2/token")
       (lambda (_status)
         (let* ((response (condition-case err
                              (zoho-projects--parse-json
                               (zoho-projects--response-body))
                            (error `((error . ,(error-message-string err))))))
                (token (alist-get 'access_token response)))
           (kill-buffer)
           (if (not token)
               (funcall callback nil
                        (format "token refresh failed: %S" response))
             (setq zoho-projects--access-token token
                   zoho-projects--token-expiry
                   (+ (float-time)
                      (- (or (alist-get 'expires_in response) 3600) 60)))
             (funcall callback token nil))))
       nil t t))))

(cl-defun zoho-projects--request-async (method path callback
                                               &key params payload (retries 1))
  "Perform METHOD PATH in the background; CALLBACK gets (RESULT ERR).
The non-blocking counterpart of `zoho-projects--request'; the
keyword arguments mean the same.  CALLBACK is invoked exactly
once, with the parsed JSON response and nil, or with nil and an
error message string.  It may run in an arbitrary buffer, so it
must `with-current-buffer' its target."
  (let ((opts (list :params params :payload payload :retries retries)))
    (zoho-projects--ensure-token-async
     (lambda (token token-err)
       (if token-err
           (funcall callback nil token-err)
         (zoho-projects--dispatch-async method path callback token opts))))))

(defun zoho-projects--dispatch-async (method path callback token opts)
  "Fire the `url-retrieve' behind `zoho-projects--request-async'.
OPTS is that function's keyword plist.  Handles the 401-retry, a
`zoho-projects-request-timeout' watchdog and guarantees CALLBACK
runs exactly once."
  (let* ((params (plist-get opts :params))
         (payload (plist-get opts :payload))
         (retries (plist-get opts :retries))
         (url-request-method method)
         (url-request-extra-headers
          (zoho-projects--request-headers
           token (and payload "application/x-www-form-urlencoded")))
         (url-request-data (when payload (url-build-query-string payload)))
         (finished nil)
         (finish (lambda (result err)
                   (unless finished
                     (setq finished t)
                     (funcall callback result err))))
         (request-buffer
          (condition-case err
              (url-retrieve
               (zoho-projects--request-url path params)
               (lambda (status)
                 (let ((code url-http-response-status)
                       (net-error (plist-get status :error))
                       (body (zoho-projects--response-body)))
                   (kill-buffer)
                   (cond
                    (finished)
                    (net-error
                     (funcall finish nil (format "%s %s: %S"
                                                 method path net-error)))
                    ((and (eq code 401) (> retries 0))
                     (setq finished t
                           zoho-projects--access-token nil)
                     (apply #'zoho-projects--request-async method path
                            callback
                            (plist-put (copy-sequence opts)
                                       :retries (1- retries))))
                    ((memq code '(200 201))
                     (condition-case err
                         (let ((result
                                (unless (string-empty-p (string-trim body))
                                  (zoho-projects--parse-json body))))
                           (if-let* ((api-err
                                      (zoho-projects--body-error result)))
                               (funcall finish nil
                                        (format "API %s %s failed: %s"
                                                method path api-err))
                             (funcall finish result nil)))
                       (error (funcall finish nil
                                       (error-message-string err)))))
                    ;; Empty collections come back as 204 No Content.
                    ((eq code 204) (funcall finish nil nil))
                    (t (funcall finish
                                nil
                                (format "API %s %s failed (HTTP %s): %s"
                                        method path code
                                        (string-trim body)))))))
               nil t t)
            (error (funcall finish nil (error-message-string err))
                   nil))))
    (when (and request-buffer zoho-projects-request-timeout)
      (run-at-time zoho-projects-request-timeout nil
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

(defun zoho-projects--request-all-async (specs callback)
  "Run request SPECS concurrently; CALLBACK gets (RESULTS ERR).
Each spec is a list (METHOD PATH KEYWORDS...) as accepted by
`zoho-projects--request-async', plus the extra keyword
:soft-errors, which turns that request's failure into a nil result
instead of failing the batch.  RESULTS preserves the order of
SPECS.  CALLBACK is invoked exactly once, on the first hard error
or once every request has answered."
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
           (apply #'zoho-projects--request-async method path
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

(defun zoho-projects--announce-write-failure (what err &optional buf)
  "Loudly announce that the background write WHAT failed with ERR.
When BUF is live it is refocused (it still holds the unsent
content, ready to retry)."
  (when (buffer-live-p buf)
    (pop-to-buffer buf))
  (ding)
  (message "%s" (propertize (format "Zoho Projects: %s failed: %s" what err)
                            'face 'error)))

;;;; Helpers

(defun zoho-projects--org-timestamp-ms (ms &optional date-only)
  "Format epoch-milliseconds MS as an inactive org timestamp.
DATE-ONLY drops the clock time."
  (if (numberp ms)
      (format-time-string (if date-only "[%Y-%m-%d %a]" "[%Y-%m-%d %a %H:%M]")
                          (seconds-to-time (/ ms 1000)))
    "[unknown]"))

(defun zoho-projects--field-date (alist key &optional date-only)
  "Return ALIST's KEY date as an org timestamp string, or \"-\".
Prefers the KEY_long epoch-milliseconds field; falls back to the
portal-formatted KEY string verbatim."
  (let ((long (alist-get (intern (concat (symbol-name key) "_long")) alist)))
    (cond ((numberp long) (zoho-projects--org-timestamp-ms long date-only))
          ((alist-get key alist))
          (t "-"))))

(defun zoho-projects--duration-human (minutes)
  "Format MINUTES as a human-readable duration like \"2h38m\"."
  (let ((h (/ minutes 60))
        (m (% minutes 60)))
    (cond ((zerop minutes) "0m")
          ((zerop h) (format "%dm" m))
          ((zerop m) (format "%dh" h))
          (t (format "%dh%02dm" h m)))))

(defun zoho-projects--id (alist)
  "Return ALIST's id as a string, preferring the id_string field.
The v1 API duplicates every id as id_string because the numbers
overflow other languages; either works here, the string is just
already the right type."
  (or (alist-get 'id_string alist)
      (and (alist-get 'id alist) (format "%s" (alist-get 'id alist)))))

(defun zoho-projects--task-status (task)
  "Return TASK's status display name."
  (let ((status (alist-get 'status task)))
    (or (and (listp status) (alist-get 'name status))
        (and (stringp status) status)
        "-")))

(defun zoho-projects--task-owners (task)
  "Return TASK's owner names joined with commas."
  (let ((owners (or (alist-get 'owners (alist-get 'details task))
                    (alist-get 'owners task))))
    (if owners
        (mapconcat (lambda (o) (or (alist-get 'name o) "?")) owners ", ")
      "-")))

(defun zoho-projects--html-to-text (html)
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

(defun zoho-projects--strip-org-linebreaks (org)
  "Remove the trailing \\\\ hard line breaks pandoc makes of <br>.
Same rule as zoho-desk's: verbatim blocks keep theirs."
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

(defun zoho-projects--html-to-org (html)
  "Convert HTML to org markup via `zoho-projects-pandoc-program'.
Falls back to the plain-text rendering when pandoc is disabled,
missing, or chokes on the input."
  (or (and (stringp html)
           zoho-projects-pandoc-program
           (executable-find zoho-projects-pandoc-program)
           (with-temp-buffer
             (insert html)
             (let ((coding-system-for-write 'utf-8)
                   (coding-system-for-read 'utf-8))
               (when (zerop (call-process-region
                             (point-min) (point-max)
                             zoho-projects-pandoc-program t '(t nil) nil
                             "-f" "html-auto_identifiers" "-t" "org"))
                 (zoho-projects--strip-org-linebreaks
                  (buffer-substring-no-properties (point-min)
                                                  (point-max)))))))
      (zoho-projects--html-to-text html)))

(defun zoho-projects--org-body (text)
  "Indent TEXT two spaces so it can never form org headings."
  (let ((clean (replace-regexp-in-string
                "[ \t]+$" "" (string-trim (or text "")))))
    (concat (replace-regexp-in-string "^." "  \\&" clean) "\n")))

(defun zoho-projects--html-to-org-body (html)
  "Convert HTML to indented org markup usable as an org entry body."
  (zoho-projects--org-body (zoho-projects--html-to-org html)))

;;;; Portal

(defvar zoho-projects--portal nil
  "Portal id chosen for this session, as a string.")
(defvar zoho-projects--portals nil
  "Cached list of portal alists.")

(defun zoho-projects--ensure-portals ()
  "Return the cached portal alists, fetching them if needed."
  (or zoho-projects--portals
      (setq zoho-projects--portals
            (alist-get 'portals
                       (zoho-projects--request "GET" "/portals/")))))

(defun zoho-projects--ensure-portal ()
  "Return the portal id to use, resolving it once per session.
A single visible portal is used silently; several prompt once."
  (or zoho-projects-portal-id
      zoho-projects--portal
      (let ((portals (zoho-projects--ensure-portals)))
        (setq zoho-projects--portal
              (cond
               ((null portals)
                (user-error "Zoho Projects: no portals visible to this token"))
               ((null (cdr portals)) (zoho-projects--id (car portals)))
               (t (let* ((names (mapcar (lambda (p) (alist-get 'name p))
                                        portals))
                         (choice (completing-read "Zoho portal: " names nil t))
                         (portal (seq-find (lambda (p)
                                             (equal (alist-get 'name p)
                                                    choice))
                                           portals)))
                    (message (concat "Using portal %s (%s) — setq "
                                     "zoho-projects-portal-id to skip this")
                             choice (zoho-projects--id portal))
                    (zoho-projects--id portal))))))))

(defun zoho-projects--portal-name ()
  "Return the current portal's URL name, for building web links."
  (let ((id (format "%s" (or zoho-projects-portal-id
                             zoho-projects--portal ""))))
    (alist-get 'name
               (seq-find (lambda (p)
                           (member id (list (format "%s" (alist-get 'id p))
                                            (alist-get 'id_string p))))
                         (zoho-projects--ensure-portals)))))

(defun zoho-projects--web-base ()
  "Return the web UI base URL matching `zoho-projects-base-url'."
  (replace-regexp-in-string
   "//projectsapi\\." "//projects."
   (replace-regexp-in-string "/restapi\\'" "" zoho-projects-base-url)))

(defun zoho-projects--project-web-url (project)
  "Return a best-effort web URL for PROJECT's task list."
  (when-let* ((portal (zoho-projects--portal-name)))
    (format "%s/portal/%s#taskslist/%s"
            (zoho-projects--web-base) portal (zoho-projects--id project))))

(defun zoho-projects--task-web-url (project task)
  "Return a best-effort web URL for TASK in PROJECT."
  (when-let* ((portal (zoho-projects--portal-name)))
    (format "%s/portal/%s#taskdetail/%s/%s/%s"
            (zoho-projects--web-base) portal
            (zoho-projects--id project)
            (or (zoho-projects--id (alist-get 'tasklist task)) "0")
            (zoho-projects--id task))))

;;;; Dashboard state

(defvar zoho-projects--projects nil
  "Cached list of project alists for the sidebar.")
(defvar zoho-projects--project nil
  "Project alist whose tasks the table shows, or nil.")
(defvar zoho-projects--tasks-pool nil
  "Every fetched task of the selected project, all statuses.")
(defvar zoho-projects--selected-statuses nil
  "Status names checked in the sidebar; the table shows their union.")
(defvar zoho-projects--saved-window-configuration nil)

(defconst zoho-projects--statuses-buffer-name "*zoho-project-statuses*")
(defconst zoho-projects--tasks-buffer-name "*zoho-project-tasks*")

(defvar zoho-projects--task-buffer nil
  "Single buffer reused for task documents, renamed per task.")

(defun zoho-projects--status-groups (tasks)
  "Group TASKS into an ordered alist of (STATUS-NAME . TASKS).
Statuses keep their order of first appearance, except that
closed-type statuses (their tasks are completed) sink to the end."
  (let ((groups nil))
    (dolist (task tasks)
      (let* ((status (zoho-projects--task-status task))
             (group (assoc status groups)))
        (if group
            (push task (cdr group))
          (push (list status task) groups))))
    (setq groups (mapcar (lambda (g) (cons (car g) (nreverse (cdr g))))
                         (nreverse groups)))
    (append (seq-remove (lambda (g) (alist-get 'completed (cadr g))) groups)
            (seq-filter (lambda (g) (alist-get 'completed (cadr g))) groups))))

(defun zoho-projects--open-status-names (tasks)
  "Return the status names of TASKS whose tasks are not completed."
  (mapcar #'car
          (seq-remove (lambda (g) (alist-get 'completed (cadr g)))
                      (zoho-projects--status-groups tasks))))

;;;; Project selection

(defvar zoho-projects--projects-generation 0
  "Bumped per projects fetch so a stale response cannot win.")

(defun zoho-projects--fetch-projects-async (callback)
  "Fetch the portal's projects in the background; CALLBACK gets (PROJECTS ERR).
Up to `zoho-projects-project-fetch-limit' projects arrive as
parallel pages of 100."
  (let ((generation (cl-incf zoho-projects--projects-generation))
        (portal (zoho-projects--ensure-portal)))
    (zoho-projects--request-all-async
     (mapcar (lambda (from)
               `("GET" ,(format "/portal/%s/projects/" portal)
                 :params (("index" ,(1+ from))
                          ("range" 100)
                          ("status" ,zoho-projects-project-status))
                 :soft-errors t))
             (number-sequence
              0 (1- (max 100 zoho-projects-project-fetch-limit)) 100))
     (lambda (pages err)
       (when (= generation zoho-projects--projects-generation)
         (if err
             (funcall callback nil err)
           (let ((seen (make-hash-table :test #'equal))
                 (projects nil))
             (dolist (page pages)
               (dolist (project (alist-get 'projects page))
                 (let ((id (zoho-projects--id project)))
                   (unless (gethash id seen)
                     (puthash id t seen)
                     (push project projects)))))
             (funcall callback (nreverse projects) nil))))))))

(defun zoho-projects-refresh-projects ()
  "Refetch the project list (the selector's candidates) from Zoho."
  (interactive)
  (message "Zoho Projects: refetching projects…")
  (zoho-projects--fetch-projects-async
   (lambda (projects err)
     (if err
         (message "Zoho Projects: %s" err)
       (setq zoho-projects--projects projects)
       (message "Zoho Projects: %d projects" (length projects))))))

(defun zoho-projects--with-projects (continue)
  "Call CONTINUE with the portal's projects, fetching them when needed."
  (if zoho-projects--projects
      (funcall continue zoho-projects--projects)
    (message "Zoho Projects: fetching projects…")
    (zoho-projects--fetch-projects-async
     (lambda (projects err)
       (cond
        (err (message "Zoho Projects: %s" err))
        ((null projects) (message "Zoho Projects: no projects"))
        (t (setq zoho-projects--projects projects)
           ;; The selector must not run inside the url.el sentinel.
           (run-at-time 0 nil continue projects)))))))

(defun zoho-projects--read-project (projects)
  "Prompt for one of PROJECTS; return its alist."
  (let* ((default (and zoho-projects--project
                       (alist-get 'name zoho-projects--project)))
         (choice (completing-read
                  "Zoho project: "
                  (mapcar (lambda (p) (alist-get 'name p)) projects)
                  nil t nil nil default)))
    (or (seq-find (lambda (p) (equal (alist-get 'name p) choice))
                  projects)
        (user-error "No project named %s" choice))))

(defun zoho-projects-select-project ()
  "Pick the project the dashboard shows, then load its tasks."
  (interactive)
  (zoho-projects--with-projects
   (lambda (projects)
     (let ((project (zoho-projects--read-project projects)))
       (setq zoho-projects--project project
             zoho-projects--tasks-pool nil
             zoho-projects--selected-statuses nil)
       (zoho-projects--dashboard-layout)
       (zoho-projects--refresh-pool)))))

(defun zoho-projects-browse-project ()
  "Open the selected project in the browser."
  (interactive)
  (let ((project (or zoho-projects--project
                     (user-error "No project selected"))))
    (if-let* ((url (zoho-projects--project-web-url project)))
        (browse-url url)
      (user-error "Cannot derive a web URL for this project"))))

;;;; Statuses sidebar

(defvar zoho-projects-statuses-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zoho-projects-toggle-status-at-point)
    (define-key map (kbd "SPC") #'zoho-projects-toggle-status-at-point)
    (define-key map (kbd "x") #'zoho-projects-toggle-status-at-point)
    (define-key map (kbd "<mouse-1>") #'zoho-projects-mouse-toggle-status)
    (define-key map (kbd "g") #'zoho-projects-refresh-tasks)
    (define-key map (kbd "P") #'zoho-projects-select-project)
    (define-key map (kbd "f") #'zoho-projects-quickfind)
    (define-key map (kbd "o") #'zoho-projects-browse-project)
    (define-key map (kbd "q") #'zoho-projects-quit)
    map))

(define-derived-mode zoho-projects-statuses-mode special-mode
  "ZohoProjectStatuses"
  "Sidebar listing a project's task statuses as OR-filter checkboxes.

\\{zoho-projects-statuses-mode-map}"
  (setq truncate-lines t)
  (setq-local window-size-fixed 'width))

(defun zoho-projects--insert-status-row (group)
  "Insert one checkbox row for GROUP, a (STATUS-NAME . TASKS) pair."
  (let* ((status (car group))
         (active (member status zoho-projects--selected-statuses))
         (start (point)))
    (insert (format "%s %-24s %d\n"
                    (if active "[x]" "[ ]")
                    (truncate-string-to-width status 24 nil nil "…")
                    (length (cdr group))))
    (add-text-properties start (point)
                         `(zoho-projects-status ,status
                           mouse-face highlight
                           face ,(if active 'zoho-projects-accent
                                   'shadow)))))

(defun zoho-projects--render-statuses ()
  "Render the selected project's statuses into the sidebar."
  (with-current-buffer (get-buffer-create
                        zoho-projects--statuses-buffer-name)
    (unless (derived-mode-p 'zoho-projects-statuses-mode)
      (zoho-projects-statuses-mode))
    (let ((inhibit-read-only t)
          (line (line-number-at-pos)))
      (erase-buffer)
      (insert (propertize (if zoho-projects--project
                              (or (alist-get 'name zoho-projects--project) "?")
                            "No project")
                          'face 'bold)
              (propertize "  (P switches)\n\n" 'face 'shadow))
      (cond
       ((null zoho-projects--project)
        (insert (propertize "P to pick a project.\n" 'face 'shadow)))
       ((null zoho-projects--tasks-pool)
        (insert (propertize "Fetching tasks…\n" 'face 'shadow)))
       (t
        (insert (propertize "Statuses (RET toggles, OR)\n\n" 'face 'bold))
        (mapc #'zoho-projects--insert-status-row
              (zoho-projects--status-groups zoho-projects--tasks-pool))))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun zoho-projects-toggle-status-at-point ()
  "Toggle the status checkbox at point and re-filter the task table."
  (interactive)
  (let ((status (get-text-property (point) 'zoho-projects-status)))
    (unless status (user-error "No status on this line"))
    (setq zoho-projects--selected-statuses
          (if (member status zoho-projects--selected-statuses)
              (delete status zoho-projects--selected-statuses)
            (append zoho-projects--selected-statuses (list status))))
    (zoho-projects--render-statuses)
    (zoho-projects--render-tasks)))

(defun zoho-projects-mouse-toggle-status (event)
  "Toggle the status clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (zoho-projects-toggle-status-at-point))

;;;; Task table pane

(defvar zoho-projects-tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zoho-projects-open-task-at-point)
    (define-key map (kbd "g") #'zoho-projects-refresh-tasks)
    (define-key map (kbd "f") #'zoho-projects-quickfind)
    (define-key map (kbd "P") #'zoho-projects-select-project)
    (define-key map (kbd "t") #'zoho-projects-add-time-entry)
    (define-key map (kbd "T") #'zoho-projects-start-task-timer)
    (define-key map (kbd "w") #'zoho-projects-copy-org-snippet)
    (define-key map (kbd "o") #'zoho-projects-browse-task)
    (define-key map (kbd "q") #'zoho-projects-quit)
    map))

(define-derived-mode zoho-projects-tasks-mode tabulated-list-mode
  "ZohoProjectTasks"
  "Major mode listing a Zoho project's tasks.

\\{zoho-projects-tasks-mode-map}"
  (setq tabulated-list-format
        [("Key" 10 t)
         ("Status" 14 t)
         ("%" 4 t)
         ("Owner" 18 t)
         ("Due" 16 t)
         ("Task" 60 t)]
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun zoho-projects--task-entry (task)
  "Convert TASK alist into a `tabulated-list-entries' element."
  (let* ((due-ms (alist-get 'end_date_long task))
         (overdue (and (numberp due-ms)
                       (not (alist-get 'completed task))
                       (time-less-p (seconds-to-time (/ due-ms 1000))
                                    (current-time)))))
    (list task
          (vector
           (propertize (or (alist-get 'key task) (zoho-projects--id task) "?")
                       'face 'zoho-projects-accent)
           (zoho-projects--task-status task)
           (format "%s" (or (alist-get 'percent_complete task) "0"))
           (zoho-projects--task-owners task)
           (propertize (zoho-projects--field-date task 'end_date)
                       'face (if overdue 'error 'default))
           (or (alist-get 'name task) "")))))

(defun zoho-projects--visible-tasks ()
  "Return the pool tasks whose status is checked in the sidebar."
  (seq-filter (lambda (task)
                (member (zoho-projects--task-status task)
                        zoho-projects--selected-statuses))
              zoho-projects--tasks-pool))

(defun zoho-projects--tasks-mode-line (visible suffix)
  "Return the task table's mode-line-process string.
VISIBLE is the shown task count; SUFFIX trails the description."
  (format " [%s, %d/%d tasks%s]"
          (if zoho-projects--project
              (or (alist-get 'name zoho-projects--project) "?")
            "no project selected")
          visible
          (length zoho-projects--tasks-pool)
          suffix))

(defun zoho-projects--render-tasks (&optional state)
  "Render the checked statuses' tasks from the pool into the table.
STATE tags a fetch in flight (`fetching') or a failed one
\(`failed'); it only affects the placeholder text and mode line."
  (with-current-buffer (get-buffer-create zoho-projects--tasks-buffer-name)
    (unless (derived-mode-p 'zoho-projects-tasks-mode)
      (zoho-projects-tasks-mode))
    (let ((visible (zoho-projects--visible-tasks)))
      (setq tabulated-list-entries
            (mapcar #'zoho-projects--task-entry visible)
            mode-line-process
            (zoho-projects--tasks-mode-line
             (length visible)
             (pcase state
               ('fetching ", fetching…")
               ('failed ", fetch failed")
               (_ ""))))
      (tabulated-list-print t)
      (force-mode-line-update)
      (when (null tabulated-list-entries)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize
                   (cond
                    ((null zoho-projects--project)
                     "\n  P to pick a project.\n")
                    ((eq state 'fetching) "\n  Fetching tasks…\n")
                    ((null zoho-projects--tasks-pool)
                     "\n  No tasks here.\n")
                    (t "\n  No status checked in the sidebar.\n"))
                   'face 'shadow)))))))

(defvar zoho-projects--pool-generation 0
  "Bumped per task pool fetch so a stale response cannot win.")

(defun zoho-projects--refresh-pool ()
  "Refetch every task of the selected project, then redraw both panes."
  (let ((generation (cl-incf zoho-projects--pool-generation))
        (project zoho-projects--project))
    (zoho-projects--render-statuses)
    (zoho-projects--render-tasks (and project 'fetching))
    (when project
      (zoho-projects--fetch-all-tasks-async
       (zoho-projects--id project)
       (lambda (tasks err)
         (when (= generation zoho-projects--pool-generation)
           (if err
               (progn
                 (zoho-projects--render-tasks 'failed)
                 (message "Zoho Projects: %s" err))
             (setq zoho-projects--tasks-pool tasks)
             ;; Keep the checked statuses that still exist; when none
             ;; remain (or none were checked), default to the open ones,
             ;; falling back to every status.
             (let ((names (mapcar #'car (zoho-projects--status-groups
                                         tasks))))
               (setq zoho-projects--selected-statuses
                     (or (seq-filter (lambda (s) (member s names))
                                     zoho-projects--selected-statuses)
                         (zoho-projects--open-status-names tasks)
                         names)))
             (zoho-projects--render-statuses)
             (zoho-projects--render-tasks))))))))

(defun zoho-projects-refresh-tasks ()
  "Refetch the selected project's tasks from Zoho, then redraw."
  (interactive)
  (unless zoho-projects--project (user-error "No project selected"))
  (zoho-projects--refresh-pool))

;;;; Quickfind

(declare-function helm "ext:helm")
(declare-function helm-make-source "ext:helm-source")
(declare-function helm-make-actions "ext:helm-lib")

(defun zoho-projects--fetch-all-tasks-async (project-id callback)
  "Fetch PROJECT-ID's tasks of every status as one pool.
Up to `zoho-projects-task-fetch-limit' tasks are fetched as
parallel pages; CALLBACK gets (TASKS ERR)."
  (zoho-projects--request-all-async
   (mapcar (lambda (from)
             `("GET" ,(format "/portal/%s/projects/%s/tasks/"
                              (zoho-projects--ensure-portal) project-id)
               :params (("index" ,(1+ from))
                        ("range" 100)
                        ("status" "all"))
               :soft-errors t))
           (number-sequence 0 (1- (max 100 zoho-projects-task-fetch-limit))
                            100))
   (lambda (pages err)
     (if err
         (funcall callback nil err)
       (let ((seen (make-hash-table :test #'equal))
             (tasks nil))
         (dolist (page pages)
           (dolist (task (alist-get 'tasks page))
             (let ((id (zoho-projects--id task)))
               (unless (gethash id seen)
                 (puthash id t seen)
                 (push task tasks)))))
         (funcall callback (nreverse tasks) nil))))))

(defun zoho-projects--quickfind-candidate (task)
  "Return TASK's helm candidate as a (DISPLAY . TASK) pair."
  (cons (format "%-10s %4s%%  %-18s %s"
                (propertize (or (alist-get 'key task)
                                (zoho-projects--id task) "?")
                            'face 'zoho-projects-accent)
                (or (alist-get 'percent_complete task) "0")
                (truncate-string-to-width (zoho-projects--task-owners task)
                                          18 nil nil t)
                (or (alist-get 'name task) ""))
        task))

(defun zoho-projects--quickfind-sources (project tasks)
  "Return one helm source per status found among PROJECT's TASKS.
Statuses keep their order of first appearance, except that
closed-type statuses (their tasks are completed) sink to the end."
  (let ((actions
         (helm-make-actions
          "Open task"
          (lambda (task)
            (zoho-projects--show-task project
                                      (zoho-projects--id task)
                                      (alist-get 'key task)))
          "Open in browser"
          (lambda (task)
            (browse-url (or (zoho-projects--task-web-url project task)
                            (user-error "No web URL for this task")))))))
    (mapcar (lambda (group)
              (helm-make-source
                  (format "%s (%d)" (car group) (length (cdr group)))
                  'helm-source-sync
                :candidates (mapcar #'zoho-projects--quickfind-candidate
                                    (cdr group))
                :fuzzy-match t
                :candidate-number-limit (max 500 zoho-projects-task-fetch-limit)
                :action actions))
            (zoho-projects--status-groups tasks))))

(defun zoho-projects--quickfind-helm (project tasks)
  "Fuzzy-find among PROJECT's TASKS with helm, one section per status."
  (require 'helm)
  (helm :sources (zoho-projects--quickfind-sources project tasks)
        :prompt "Task: "
        :buffer "*helm zoho tasks*"))

(defun zoho-projects--quickfind-project (project)
  "Fetch PROJECT's tasks, then quickfind among them."
  (message "Zoho Projects: fetching tasks of %s…"
           (alist-get 'name project))
  (zoho-projects--fetch-all-tasks-async
   (zoho-projects--id project)
   (lambda (tasks err)
     (cond
      (err (message "Zoho Projects: %s" err))
      ((null tasks) (message "Zoho Projects: no tasks in %s"
                             (alist-get 'name project)))
      ;; helm runs its own minibuffer loop; don't start it from
      ;; inside the url.el sentinel.
      (t (run-at-time 0 nil #'zoho-projects--quickfind-helm
                      project tasks))))))

;;;###autoload
(defun zoho-projects-quickfind ()
  "Fuzzy-find a task in a project, across every status.
Prompts for the project (defaulting to the dashboard's current
one), fetches up to `zoho-projects-task-fetch-limit' of its tasks
regardless of status, and lists them in a helm buffer with one
section per status — closed statuses included, sunk to the end."
  (interactive)
  (zoho-projects--ensure-portal)
  (zoho-projects--with-projects
   (lambda (projects)
     (zoho-projects--quickfind-project
      (zoho-projects--read-project projects)))))

;;;; Dashboard layout

(defun zoho-projects--dashboard-layout ()
  "Show the dashboard windows: statuses sidebar plus task table."
  (unless (window-configuration-p zoho-projects--saved-window-configuration)
    (setq zoho-projects--saved-window-configuration
          (current-window-configuration)))
  (delete-other-windows)
  (let* ((sidebar (selected-window))
         (table (split-window sidebar zoho-projects-sidebar-width 'right)))
    (zoho-projects--render-statuses)
    (with-current-buffer (get-buffer-create zoho-projects--tasks-buffer-name)
      (unless (derived-mode-p 'zoho-projects-tasks-mode)
        (zoho-projects-tasks-mode)))
    (set-window-buffer sidebar
                       (get-buffer zoho-projects--statuses-buffer-name))
    (set-window-buffer table (get-buffer zoho-projects--tasks-buffer-name))
    (set-window-dedicated-p sidebar t)
    (select-window table)))

;;;###autoload
(defun zoho-projects-dashboard ()
  "Open the Zoho Projects dashboard for one project.
First a project selector prompts (helm under helm-mode); then the
sidebar lists the project's task statuses as OR-filter checkboxes
and the table shows the checked statuses' tasks.  Every status is
fetched once as a pool, so toggling filters is instant.  A
project already selected this session reopens immediately; switch
with P."
  (interactive)
  (zoho-projects--sync-accent-faces)
  (zoho-projects--ensure-portal)
  (if (null zoho-projects--project)
      (zoho-projects-select-project)
    (zoho-projects--dashboard-layout)
    (if zoho-projects--tasks-pool
        (progn (zoho-projects--render-statuses)
               (zoho-projects--render-tasks))
      (zoho-projects--refresh-pool))))

(defun zoho-projects-quit ()
  "Close the dashboard and restore the previous window layout."
  (interactive)
  (when (window-configuration-p zoho-projects--saved-window-configuration)
    (set-window-configuration zoho-projects--saved-window-configuration))
  (setq zoho-projects--saved-window-configuration nil))

;;;; Task context

(defvar-local zoho-projects--task nil
  "Full task alist shown in a task buffer.")
(defvar-local zoho-projects--task-project nil
  "Project alist the task in this buffer belongs to.")

(defun zoho-projects--task-at-point ()
  "Return (PROJECT . TASK) relevant to the current buffer/point, or nil."
  (cond
   ((derived-mode-p 'zoho-projects-tasks-mode)
    (cons (or zoho-projects--project
              (user-error "No project selected"))
          (or (tabulated-list-get-id)
              (user-error "No task on this line"))))
   ((bound-and-true-p zoho-projects-task-minor-mode)
    (cons zoho-projects--task-project zoho-projects--task))
   (t nil)))

(defun zoho-projects-browse-task ()
  "Open the task at point in the browser (best-effort URL)."
  (interactive)
  (pcase-let ((`(,project . ,task)
               (or (zoho-projects--task-at-point)
                   (user-error "No task in context"))))
    (if-let* ((url (zoho-projects--task-web-url project task)))
        (browse-url url)
      (user-error "Cannot derive a web URL for this task"))))

(defun zoho-projects-copy-task-key ()
  "Copy the task key at point (the \"AR1-T1\" form)."
  (interactive)
  (pcase-let ((`(,_project . ,task)
               (or (zoho-projects--task-at-point)
                   (user-error "No task in context"))))
    (let ((key (or (alist-get 'key task) (zoho-projects--id task))))
      (kill-new key)
      (message "%s copied to kill ring" key))))

(defun zoho-projects-copy-task-url ()
  "Copy the web URL of the task at point."
  (interactive)
  (pcase-let ((`(,project . ,task)
               (or (zoho-projects--task-at-point)
                   (user-error "No task in context"))))
    (let ((url (or (zoho-projects--task-web-url project task)
                   (user-error "Cannot derive a web URL for this task"))))
      (kill-new url)
      (message "%s copied to kill ring" url))))

;;;; Task document (org-mode)

(defvar zoho-projects-task-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c z 1") #'zoho-projects-tab-overview)
    (define-key map (kbd "C-c z 2") #'zoho-projects-tab-comments)
    (define-key map (kbd "C-c z 3") #'zoho-projects-tab-time-logs)
    (define-key map (kbd "C-c z l") #'zoho-projects-submit-time-log)
    (define-key map (kbd "C-c z c") #'zoho-projects-add-comment)
    (define-key map (kbd "C-c z C") #'zoho-projects-submit-comment)
    (define-key map (kbd "C-c z t") #'zoho-projects-add-time-entry)
    (define-key map (kbd "C-c z T") #'zoho-projects-start-task-timer)
    (define-key map (kbd "C-c z g") #'zoho-projects-refresh-task)
    (define-key map (kbd "C-c z o") #'zoho-projects-browse-task)
    (define-key map (kbd "C-c z w") #'zoho-projects-copy-org-snippet)
    (define-key map (kbd "C-c z y") #'zoho-projects-copy-task-url)
    (define-key map (kbd "C-c z #") #'zoho-projects-copy-task-key)
    (define-key map (kbd "M-n") #'zoho-projects-next-field)
    (define-key map (kbd "M-p") #'zoho-projects-previous-field)
    (define-key map (kbd "C-c .") #'zoho-projects-org-timestamp-dwim)
    map))

(defvar-local zoho-projects--current-tab nil
  "Heading of the currently narrowed tab, nil for Overview.")

(defvar-local zoho-projects--tab-padding-overlay nil
  "Overlay drawing blank-line padding above a narrowed tab's heading.")

(defconst zoho-projects--tabs
  '(("Overview" . nil)
    ("Comments" . "Comments")
    ("Time Logs" . "Time Logs"))
  "Tab labels and the top-level org heading each narrows to.")

(defun zoho-projects--input-field-matcher (limit)
  "Font-lock matcher for the input field backgrounds.
Matches the next stretch of the New Comment, Date, Hours/Minutes,
Billing or notes fields before LIMIT; registered with the `append'
override so org's own fontification keeps precedence inside the
fields."
  (let* ((date (zoho-projects--date-field))
         (billing (zoho-projects--billing-field))
         (fields (delq nil
                       ;; Full-line fields take in their newline so the
                       ;; :extend background runs to the window edge;
                       ;; the duration values only paint their digits,
                       ;; and the notes/comment bodies end with their
                       ;; own newlines.
                       (list (zoho-projects--comment-field)
                             (and date (cons (car date) (1+ (cdr date))))
                             (zoho-projects--duration-field 'hours)
                             (zoho-projects--duration-field 'minutes)
                             (and billing
                                  (cons (car billing) (1+ (cdr billing))))
                             (zoho-projects--notes-field))))
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

(defconst zoho-projects--input-font-lock-keywords
  '((zoho-projects--input-field-matcher (0 'zoho-projects-input append)))
  "Font-lock keywords painting the input field backgrounds.")

(define-minor-mode zoho-projects-task-minor-mode
  "Commands and tabs on top of an org-mode Zoho task document.
\\{zoho-projects-task-minor-mode-map}"
  :lighter " ZohoTask"
  (font-lock-remove-keywords nil zoho-projects--input-font-lock-keywords)
  (if zoho-projects-task-minor-mode
      (progn
        (setq header-line-format '(:eval (zoho-projects--header-line)))
        (font-lock-add-keywords nil zoho-projects--input-font-lock-keywords
                                'append))
    (setq header-line-format nil))
  (font-lock-flush))

(defun zoho-projects--tab-keymap (heading)
  "Return a header-line keymap switching to the HEADING tab on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (with-selected-window (posn-window (event-start event))
                    (zoho-projects--set-tab heading))))
    map))

(defun zoho-projects--header-line ()
  "Return the tab header line for a task buffer."
  (concat
   " "
   (mapconcat
    (lambda (tab)
      (propertize (format " %s " (car tab))
                  'face (if (equal zoho-projects--current-tab (cdr tab))
                            'zoho-projects-tab-active
                          'shadow)
                  'mouse-face 'highlight
                  'help-echo (format "mouse-1: show %s" (car tab))
                  'keymap (zoho-projects--tab-keymap (cdr tab))))
    zoho-projects--tabs
    " ")
   (propertize "   (click, C-c z 1-3, or gt)" 'face 'shadow)))

(defun zoho-projects--set-tab (heading)
  "Narrow the task buffer to HEADING, or widen when nil."
  (widen)
  (when zoho-projects--tab-padding-overlay
    (delete-overlay zoho-projects--tab-padding-overlay)
    (setq zoho-projects--tab-padding-overlay nil))
  (setq zoho-projects--current-tab heading)
  (goto-char (point-min))
  (when heading
    (if (re-search-forward (concat "^\\* " (regexp-quote heading) "$") nil t)
        (progn (beginning-of-line)
               (org-narrow-to-subtree)
               (goto-char (point-min))
               (setq zoho-projects--tab-padding-overlay
                     (make-overlay (point-min) (point-min)))
               (overlay-put zoho-projects--tab-padding-overlay
                            'before-string "\n\n"))
      (message "No %s section in this task" heading)))
  (if (fboundp 'org-fold-show-all) (org-fold-show-all) (org-show-all))
  (zoho-projects--fold-time-log-entries)
  (zoho-projects--fold-default-sections)
  (force-mode-line-update))

(defun zoho-projects--fold-default-sections ()
  "Collapse the sections that start folded: Details and New Comment.
Tab switches unfold the whole document, so this runs after each
one, same as `zoho-projects--fold-time-log-entries'."
  (save-excursion
    (dolist (heading '("^\\* Details$" "^\\*\\* New Comment$"))
      (goto-char (point-min))
      (when (re-search-forward heading nil t)
        (beginning-of-line)
        (if (fboundp 'org-fold-hide-subtree)
            (org-fold-hide-subtree)
          (outline-hide-subtree))))))

(defun zoho-projects--fold-time-log-entries ()
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

(defun zoho-projects-tab-overview ()
  "Show the whole task document."
  (interactive)
  (zoho-projects--set-tab nil))

(defun zoho-projects-tab-comments ()
  "Narrow to the Comments section."
  (interactive)
  (zoho-projects--set-tab "Comments"))

(defun zoho-projects-tab-time-logs ()
  "Narrow to the Time Logs section."
  (interactive)
  (zoho-projects--set-tab "Time Logs"))

(defun zoho-projects-tab-next (&optional backward)
  "Cycle to the next tab, or previous when BACKWARD."
  (interactive)
  (let* ((current (seq-position zoho-projects--tabs
                                zoho-projects--current-tab
                                (lambda (tab tab-heading)
                                  (equal (cdr tab) tab-heading))))
         (next (mod (+ (or current 0) (if backward -1 1))
                    (length zoho-projects--tabs))))
    (zoho-projects--set-tab (cdr (nth next zoho-projects--tabs)))))

(defun zoho-projects-tab-previous ()
  "Cycle to the previous tab."
  (interactive)
  (zoho-projects-tab-next t))

;;;; Task document rendering

(defun zoho-projects--insert-comment (comment)
  "Insert COMMENT as an org subheading."
  (insert (format "** %s %s\n"
                  (zoho-projects--org-timestamp-ms
                   (alist-get 'created_time_long comment))
                  (or (alist-get 'added_person comment)
                      (alist-get 'added_by comment)
                      "unknown")))
  (insert (zoho-projects--html-to-org-body
           (or (alist-get 'content comment) ""))))

(defun zoho-projects--insert-time-log (log date)
  "Insert time LOG (from the DATE group) as an org subheading.
The heading carries the essentials because entries are collapsed
by default; the body holds the notes."
  (let* ((notes (alist-get 'notes log))
         (text (string-trim
                (if (and (stringp notes) (string-match-p "<[a-zA-Z!/]" notes))
                    (zoho-projects--html-to-text notes)
                  (or notes ""))))
         (summary (car (split-string text "\n" t "[ \t]+")))
         (period (when (and (alist-get 'start_time log)
                            (alist-get 'end_time log))
                   (format " %s–%s"
                           (alist-get 'start_time log)
                           (alist-get 'end_time log)))))
    (insert (format "** %s %s%s =%s= (%s)%s\n"
                    (or date "?")
                    (or (alist-get 'owner_name log)
                        (alist-get 'added_person log)
                        "?")
                    (or period "")
                    (zoho-projects--duration-human
                     (zoho-projects--log-minutes log))
                    (or (alist-get 'bill_status log) "?")
                    (if summary
                        (concat " — " (truncate-string-to-width
                                       summary 40 nil nil "…"))
                      ""))
            ":PROPERTIES:\n"
            (format ":TIMELOG_ID: %s\n" (zoho-projects--id log))
            ":END:\n")
    (unless (string-empty-p text)
      (insert (zoho-projects--org-body text)))))

(defun zoho-projects--render-task-org (project task comments)
  "Fill the current buffer with an org document for TASK in PROJECT.
COMMENTS is the comment list.  The Time Logs section renders with
a fetching placeholder; `zoho-projects--fill-task-logs' splices
the entries in once the month fetches answer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n\n")
    (insert (format "#+title: %s %s\n\n"
                    (or (alist-get 'key task) "")
                    (or (alist-get 'name task) "?")))
    (insert "* Details\n"
            (format "- Project :: %s\n"
                    (or (alist-get 'name project) "-"))
            (format "- Status :: %s\n" (zoho-projects--task-status task))
            (format "- Owner :: %s\n" (zoho-projects--task-owners task))
            (format "- Priority :: %s\n"
                    (or (alist-get 'priority task) "None"))
            (format "- Start :: %s\n"
                    (zoho-projects--field-date task 'start_date))
            (format "- Due :: %s\n"
                    (zoho-projects--field-date task 'end_date))
            (format "- Duration :: %s\n"
                    (if (alist-get 'duration task)
                        (format "%s %s"
                                (alist-get 'duration task)
                                (or (alist-get 'duration_type task) "days"))
                      "-"))
            (format "- Work :: %s\n" (or (alist-get 'work task) "-"))
            (format "- Complete :: %s%%\n"
                    (or (alist-get 'percent_complete task) "0"))
            (format "- Task List :: %s\n"
                    (or (alist-get 'name (alist-get 'tasklist task)) "-"))
            (format "- Task ID :: %s\n" (zoho-projects--id task))
            (if-let* ((url (zoho-projects--task-web-url project task)))
                (format "- Web :: [[%s][open in Zoho Projects]]\n" url)
              ""))
    (when-let* ((description (alist-get 'description task)))
      (unless (string-empty-p (string-trim description))
        (insert "* Description\n"
                (zoho-projects--html-to-org-body description))))
    (insert "* Comments\n")
    (insert "** New Comment"
            ;; Same input-field pattern as the New Time Log notes: an
            ;; editable body between two marked separator newlines.
            (propertize "\n" 'zoho-projects-comment-start t)
            "\n"
            (propertize "\n" 'zoho-projects-comment-end t))
    (mapc #'zoho-projects--insert-comment comments)
    (insert "* Time Logs\n"
            ;; The total line doubles as the fetch status: the v1 API
            ;; serves logs per project and month, so the entries arrive
            ;; after the document and are spliced in by
            ;; `zoho-projects--fill-task-logs', which rewrites this line.
            (propertize "- Total logged :: fetching…"
                        'zoho-projects-logs-total t)
            "\n")
    (insert "** New Time Log\n"
            ;; Same input-field pattern as zoho-desk's Reply and New
            ;; Time Log sections: read-only label islands with editable
            ;; gaps after them, then a notes body between two marked
            ;; separator newlines.  `zoho-projects--protect-buffer'
            ;; locks everything outside the fields.
            (propertize "Date: "
                        'zoho-projects-date-label t
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            (format-time-string "[%Y-%m-%d %a]")
            "\n"
            ;; Each duration value starts as editable spaces so the
            ;; input face paints a visible box even while it's blank;
            ;; the spaces are trimmed away on submit.
            (propertize "Hours: "
                        'zoho-projects-duration 'hours
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            "    "
            (propertize "  Minutes: "
                        'zoho-projects-duration 'minutes
                        'read-only t
                        'rear-nonsticky t)
            "    "
            "\n"
            (propertize "Billing: "
                        'zoho-projects-billing-label t
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t)
            zoho-projects-default-bill-status
            "\n"
            "Notes:"
            (propertize "\n" 'zoho-projects-notes-start t)
            "\n"
            (propertize "\n" 'zoho-projects-notes-end t))
    (goto-char (point-min))))

(defun zoho-projects--normalize-buffer-style ()
  "Tone down prose-oriented styling that hurts task buffers.
Same treatment as zoho-desk ticket buffers: replace the huge
header-line remap so the tab bar stays readable, and turn off
trailing-whitespace highlighting."
  (setq-local face-remapping-alist
              (assq-delete-all 'header-line
                               (copy-alist face-remapping-alist)))
  (face-remap-add-relative
   'header-line `(:height ,(face-attribute 'default :height nil 'default)))
  (setq-local show-trailing-whitespace nil))

;;;; Input fields

(defun zoho-projects--marker-field (start-prop end-prop)
  "Return (START . END) of the editable area between two marked newlines."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((sep (text-property-any (point-min) (point-max)
                                          start-prop t))
                  (end (text-property-any sep (point-max) end-prop t)))
        (cons (1+ sep) end)))))

(defun zoho-projects--notes-field ()
  "Return (START . END) of the New Time Log notes area, or nil."
  (zoho-projects--marker-field 'zoho-projects-notes-start
                               'zoho-projects-notes-end))

(defun zoho-projects--comment-field ()
  "Return (START . END) of the New Comment input area, or nil."
  (zoho-projects--marker-field 'zoho-projects-comment-start
                               'zoho-projects-comment-end))

(defun zoho-projects--label-line-field (label-prop)
  "Return (START . END) of the editable rest of a labeled input line."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((label (next-single-property-change
                          (point-min) label-prop))
                  (start (next-single-property-change label label-prop)))
        (goto-char start)
        (cons start (line-end-position))))))

(defun zoho-projects--date-field ()
  "Return (START . END) of the Date line's editable area, or nil."
  (zoho-projects--label-line-field 'zoho-projects-date-label))

(defun zoho-projects--billing-field ()
  "Return (START . END) of the Billing line's editable area, or nil."
  (zoho-projects--label-line-field 'zoho-projects-billing-label))

(defun zoho-projects--duration-field (unit)
  "Return (START . END) of the duration UNIT's editable value, or nil.
UNIT is the symbol `hours' or `minutes'."
  (save-excursion
    (save-restriction
      (widen)
      (when-let* ((label (text-property-any (point-min) (point-max)
                                            'zoho-projects-duration unit))
                  (start (text-property-not-all label (point-max)
                                                'zoho-projects-duration
                                                unit)))
        (goto-char start)
        (cons start
              (if (get-text-property start 'zoho-projects-duration)
                  start
                (min (or (next-single-property-change
                          start 'zoho-projects-duration)
                         (point-max))
                     (line-end-position))))))))

(defun zoho-projects--editable-fields ()
  "Return the buffer's editable input field ranges, sorted by position."
  (sort (delq nil (list (zoho-projects--comment-field)
                        (zoho-projects--date-field)
                        (zoho-projects--duration-field 'hours)
                        (zoho-projects--duration-field 'minutes)
                        (zoho-projects--billing-field)
                        (zoho-projects--notes-field)))
        (lambda (a b) (< (car a) (car b)))))

(defun zoho-projects--protect-buffer ()
  "Make everything except the input fields read-only.
Same locking rules as zoho-desk's ticket buffers."
  (let ((inhibit-read-only t))
    (save-excursion
      (save-restriction
        (widen)
        (let ((fields (zoho-projects--editable-fields))
              (pos (point-min)))
          (if (null fields)
              (add-text-properties (point-min) (point-max)
                                   '(read-only t front-sticky (read-only)))
            (dolist (field fields)
              (when (< pos (car field))
                (add-text-properties pos (car field) '(read-only t))
                (when (= pos (point-min))
                  (add-text-properties pos (car field)
                                       '(front-sticky (read-only))))
                (add-text-properties (1- (car field)) (car field)
                                     '(rear-nonsticky t)))
              (setq pos (max pos (cdr field))))
            (when (< pos (point-max))
              (add-text-properties pos (point-max) '(read-only t)))))))))

(defun zoho-projects--field-jump (direction)
  "Move point to the end of the next input field in DIRECTION."
  (let* ((fields (seq-filter (lambda (field)
                               (and (>= (car field) (point-min))
                                    (<= (cdr field) (point-max))))
                             (zoho-projects--editable-fields)))
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
      (goto-char (cdr next))
      (skip-chars-backward " " (car next)))))

(defun zoho-projects-next-field ()
  "Jump to the next input field."
  (interactive)
  (zoho-projects--field-jump 1))

(defun zoho-projects-previous-field ()
  "Jump to the previous input field."
  (interactive)
  (zoho-projects--field-jump -1))

(defun zoho-projects--set-field (field text)
  "Replace the editable FIELD's content with TEXT; nil FIELD is a no-op."
  (when field
    (let ((inhibit-read-only t))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (car field))
          (delete-region (car field) (cdr field))
          (insert text))))))

(defun zoho-projects--duration-input (unit)
  "Return the duration UNIT field's value as a whole number, 0 when blank."
  (let* ((field (or (zoho-projects--duration-field unit)
                    (user-error "No New Time Log section in this buffer")))
         (text (string-trim (buffer-substring-no-properties
                             (car field) (cdr field)))))
    (cond ((string-empty-p text) 0)
          ((string-match-p "\\`[0-9]+\\'" text) (string-to-number text))
          (t (user-error "%s must be a whole number, not %S"
                         (capitalize (symbol-name unit)) text)))))

(defun zoho-projects--date-value ()
  "Return the Date field's time as an Emacs time value.
nil when the field is blank (meaning: today); a `user-error' when
its content is not a readable org timestamp."
  (when-let* ((field (zoho-projects--date-field)))
    (let ((text (string-trim (buffer-substring-no-properties
                              (car field) (cdr field)))))
      (unless (string-empty-p text)
        (condition-case nil
            (org-time-string-to-time text)
          (error (user-error "Unreadable Date: %s" text)))))))

(defun zoho-projects--bill-status-value ()
  "Return the Billing field's value normalized to the API's two forms."
  (let* ((field (or (zoho-projects--billing-field)
                    (user-error "No New Time Log section in this buffer")))
         (text (string-trim (buffer-substring-no-properties
                             (car field) (cdr field)))))
    (cond ((string-empty-p text) zoho-projects-default-bill-status)
          ((member (downcase text) '("billable" "b")) "Billable")
          ((member (downcase text)
                   '("non billable" "non-billable" "nonbillable" "n"))
           "Non Billable")
          (t (user-error
              "Billing must be Billable or Non Billable, not %S" text)))))

(defun zoho-projects-pick-date ()
  "Fill the New Time Log's Date field with the org date picker."
  (interactive)
  (let ((field (or (zoho-projects--date-field)
                   (user-error "No New Time Log section in this buffer")))
        (time (org-read-date nil t nil "Log date: ")))
    (zoho-projects--set-field field
                              (format-time-string "[%Y-%m-%d %a]" time))))

(defun zoho-projects-org-timestamp-dwim ()
  "Pick the log Date when point is in its field, else org's C-c .."
  (interactive)
  (let ((field (zoho-projects--date-field)))
    (if (and field (<= (car field) (point)) (>= (cdr field) (point)))
        (zoho-projects-pick-date)
      (call-interactively (if (fboundp 'org-timestamp)
                              'org-timestamp
                            'org-time-stamp)))))

;;;; Opening a task

(defvar zoho-projects--show-task-generation 0
  "Bumped per task fetch so a stale response cannot win.")

(defun zoho-projects--live-task-buffer ()
  "Return the reusable task buffer, creating it if needed."
  (if (buffer-live-p zoho-projects--task-buffer)
      zoho-projects--task-buffer
    (setq zoho-projects--task-buffer
          (generate-new-buffer "*zoho task*"))))

(defun zoho-projects--show-fetch-error (buf err)
  "Show ERR in task buffer BUF and in the echo area."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (widen)
        (erase-buffer)
        (insert (propertize (format "\n\n  Zoho Projects: %s\n" err)
                            'face 'error)))))
  (message "Zoho Projects: %s" err))

(defun zoho-projects--show-task (project task-id &optional task-key tab
                                         background)
  "Fetch TASK-ID of PROJECT in the background, show it as an org document.
The task buffer pops up immediately with a fetching notice naming
TASK-KEY (when known) and is filled in when the responses arrive.
TAB, when non-nil, is selected once rendered.  BACKGROUND
refreshes the buffer without popping or selecting it (used after a
posted time log, when the user may have moved on)."
  (let* ((portal (zoho-projects--ensure-portal))
         (project-id (zoho-projects--id project))
         (id (format "%s" task-id))
         (generation (cl-incf zoho-projects--show-task-generation))
         (buf (zoho-projects--live-task-buffer)))
    (with-current-buffer buf
      (when task-key
        (rename-buffer (format "*zoho task %s*" task-key) t))
      ;; Drop the previous task's state so commands fired while the
      ;; fetch is in flight cannot act on stale data.
      (zoho-projects-task-minor-mode -1)
      (setq zoho-projects--task nil
            zoho-projects--task-project nil)
      (let ((inhibit-read-only t))
        (widen)
        (erase-buffer)
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (zoho-projects--normalize-buffer-style)
        (insert (propertize (format "\n\n  Fetching %s …\n"
                                    (or task-key id))
                            'face 'shadow))
        (add-text-properties (point-min) (point-max)
                             '(read-only t front-sticky (read-only)))))
    (unless background
      (pop-to-buffer buf
                     `((display-buffer-reuse-window
                        display-buffer-below-selected)
                       (window-height . ,zoho-projects-task-window-height))))
    (zoho-projects--request-all-async
     `(("GET" ,(format "/portal/%s/projects/%s/tasks/%s/"
                       portal project-id id))
       ("GET" ,(format "/portal/%s/projects/%s/tasks/%s/comments/"
                       portal project-id id)
        :params (("index" 1) ("range" 100)) :soft-errors t))
     (lambda (results err)
       (when (= generation zoho-projects--show-task-generation)
         (if err
             (zoho-projects--show-fetch-error buf err)
           (let ((task (or (car (alist-get 'tasks (nth 0 results)))
                           `((id_string . ,id)))))
             (zoho-projects--render-task
              buf project task
              (alist-get 'comments (nth 1 results))
              tab)
             ;; The entries need the task's dates to pick the months,
             ;; so they are fetched after the document renders and
             ;; spliced in when they land.
             (zoho-projects--fetch-task-logs-async
              project-id task
              (lambda (result log-err)
                (when (and (= generation
                              zoho-projects--show-task-generation)
                           (buffer-live-p buf))
                  (with-current-buffer buf
                    (zoho-projects--fill-task-logs result log-err))))))))))))

;;;; Time log fetching
;;
;; The v1 API has no task-scoped GET for time logs — the timesheet
;; endpoint serves a whole project, one month (or day/week) at a time,
;; and demands its filter parameters.  So the task document fetches
;; the months spanning the task's dates in parallel and keeps only
;; the logs belonging to the task.

(defun zoho-projects--task-log-months (task)
  "Return \"MM-01-YYYY\" month anchors covering TASK's likely logs.
From the earliest of the task's created/start dates to the latest
of today and its due date, newest first, capped at
`zoho-projects-timelog-months' months."
  (let* ((now (float-time))
         (to-secs (lambda (ms) (and (numberp ms) (/ ms 1000.0))))
         (from (decode-time
                (seconds-to-time
                 (apply #'min
                        (delq nil
                              (list (funcall to-secs
                                             (alist-get 'created_time_long
                                                        task))
                                    (funcall to-secs
                                             (alist-get 'start_date_long
                                                        task))
                                    now))))))
         (to (decode-time
              (seconds-to-time
               (apply #'max
                      (delq nil
                            (list (funcall to-secs
                                           (alist-get 'end_date_long task))
                                  now))))))
         (year (nth 5 from))
         (month (nth 4 from))
         (end-index (+ (* 12 (nth 5 to)) (nth 4 to)))
         (months nil))
    (while (<= (+ (* 12 year) month) end-index)
      (push (format "%02d-01-%d" month year) months)
      (setq month (1+ month))
      (when (> month 12) (setq month 1 year (1+ year))))
    (seq-take months (max 1 zoho-projects-timelog-months))))

(defun zoho-projects--fetch-task-logs-async (project-id task callback)
  "Fetch TASK's time logs via PROJECT-ID's timesheet endpoint.
One request per month from `zoho-projects--task-log-months', run
in parallel; CALLBACK gets ((TOTAL-MINUTES . ENTRIES) ERR) where
ENTRIES are (DATE-STRING LOG) lists, newest first."
  (let ((portal (zoho-projects--ensure-portal))
        (task-id (zoho-projects--id task)))
    (zoho-projects--request-all-async
     (mapcar (lambda (date)
               `("GET" ,(format "/portal/%s/projects/%s/logs/"
                                portal project-id)
                 :params (("index" 0) ("range" 200)
                          ("users_list" "all")
                          ("view_type" "month")
                          ("date" ,date)
                          ("component_type" "task")
                          ("bill_status" "All"))))
             (zoho-projects--task-log-months task))
     (lambda (pages err)
       (if err
           (funcall callback nil err)
         (funcall callback
                  (zoho-projects--collect-task-logs pages task-id)
                  nil))))))

(defun zoho-projects--log-minutes (log)
  "Return LOG's duration in minutes."
  (let ((total (alist-get 'total_minutes log)))
    (cond ((numberp total) total)
          ((stringp total) (string-to-number total))
          (t (+ (* 60 (or (alist-get 'hours log) 0))
                (or (alist-get 'minutes log) 0))))))

(defun zoho-projects--collect-task-logs (pages task-id)
  "Return (TOTAL-MINUTES . ENTRIES) for TASK-ID from month log PAGES.
Each page is one month of the project's logs; only tasklogs whose
task id matches TASK-ID are kept, deduped across pages.  ENTRIES
are (DATE-STRING LOG) lists, newest date first."
  (let ((seen (make-hash-table :test #'equal))
        (entries nil)
        (total 0))
    (dolist (page pages)
      (let* ((timelogs (alist-get 'timelogs page))
             ;; Both response shapes exist in the wild: the date
             ;; groups directly under timelogs, or nested one level
             ;; deeper under timelog.
             (groups (or (alist-get 'date timelogs)
                         (alist-get 'date (alist-get 'timelog timelogs)))))
        (dolist (group groups)
          (let* ((date-ms (or (alist-get 'date_long group) 0))
                 (raw-date (or (alist-get 'display_format group)
                               (alist-get 'date group)))
                 (date (cond ((> date-ms 0)
                              (zoho-projects--org-timestamp-ms date-ms t))
                             ;; A date-only value rendered through the
                             ;; portal's datetime format always carries a
                             ;; meaningless midnight — drop the time part.
                             (raw-date
                              (replace-regexp-in-string
                               " [0-9]+:[0-9]+\\(?::[0-9]+\\)? ?\\(?:[AaPp][Mm]\\)?\\'"
                               "" raw-date)))))
            (dolist (log (alist-get 'tasklogs group))
              (let ((log-id (or (zoho-projects--id log)
                                (format "%s" (sxhash-equal log)))))
                (when (and (equal (zoho-projects--id (alist-get 'task log))
                                  task-id)
                           (not (gethash log-id seen)))
                  (puthash log-id t seen)
                  (cl-incf total (zoho-projects--log-minutes log))
                  (push (list date-ms date log) entries))))))))
    (cons total
          (mapcar #'cdr
                  (sort entries (lambda (a b) (> (car a) (car b))))))))

(defun zoho-projects--fill-task-logs (result err)
  "Splice fetched time logs into this task buffer.
RESULT is (TOTAL-MINUTES . ENTRIES) from
`zoho-projects--collect-task-logs'; a non-nil ERR is reported on
the total line instead.  The entries land after the New Time Log
subtree, collapsed like the initial render; the user's point and
any text typed into the input fields are left alone."
  (let* ((was-start (and (buffer-narrowed-p) (point-min)))
         (was-end (and (buffer-narrowed-p) (point-max)))
         (inhibit-read-only t))
    (save-excursion
      (widen)
      ;; Rewrite the total/status line.
      (when-let* ((pos (text-property-any (point-min) (point-max)
                                          'zoho-projects-logs-total t)))
        (goto-char pos)
        (delete-region (line-beginning-position) (line-end-position))
        (insert (propertize
                 (if err
                     (format "- Time logs :: fetch failed: %s" err)
                   (format "- Total logged :: %s"
                           (zoho-projects--duration-human (car result))))
                 'zoho-projects-logs-total t)))
      (unless err
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\* New Time Log$" nil t)
          (org-back-to-heading t)
          (org-end-of-subtree t t)
          (delete-region (point) (point-max))
          (let ((start (point)))
            (dolist (entry (cdr result))
              (zoho-projects--insert-time-log (cadr entry) (car entry)))
            ;; Collapse the inserted entries, like the initial render —
            ;; but only these, so a New Time Log the user is already
            ;; typing into stays open.
            (goto-char start)
            (while (re-search-forward "^\\*\\* " nil t)
              (beginning-of-line)
              (if (fboundp 'org-fold-hide-subtree)
                  (org-fold-hide-subtree)
                (outline-hide-subtree))
              (org-end-of-subtree t t)))))
      (zoho-projects--protect-buffer))
    ;; Restore the tab narrowing around the splice: the Time Logs tab
    ;; must grow to include the new entries; any other tab keeps its
    ;; old bounds (the splice is past them).
    (when was-start
      (if (equal zoho-projects--current-tab "Time Logs")
          (narrow-to-region was-start (point-max))
        (narrow-to-region was-start was-end)))
    (when err
      (message "Zoho Projects: time logs fetch failed: %s" err))))

(defun zoho-projects--render-task (buf project task comments tab)
  "Fill BUF with TASK's org document and select TAB."
  (with-current-buffer buf
    (rename-buffer (format "*zoho task %s*"
                           (or (alist-get 'key task)
                               (zoho-projects--id task)))
                   t)
    (let ((inhibit-read-only t))
      (widen)
      (erase-buffer))
    (zoho-projects--render-task-org project task comments)
    (setq zoho-projects--task task
          zoho-projects--task-project project
          buffer-offer-save nil)
    (zoho-projects-task-minor-mode 1)
    (zoho-projects--sync-accent-faces)
    (zoho-projects--normalize-buffer-style)
    (zoho-projects--protect-buffer)
    (zoho-projects--set-tab tab)
    (zoho-projects--fill-pending-time-log)))

(defun zoho-projects-open-task-at-point ()
  "Open the task on the current list line."
  (interactive)
  (pcase-let ((`(,project . ,task) (zoho-projects--task-at-point)))
    (zoho-projects--show-task project
                              (zoho-projects--id task)
                              (alist-get 'key task))))

(defun zoho-projects-refresh-task ()
  "Re-fetch the task shown in this buffer, keeping the current tab."
  (interactive)
  (unless zoho-projects--task (user-error "Not in a task buffer"))
  (zoho-projects--show-task zoho-projects--task-project
                            (zoho-projects--id zoho-projects--task)
                            (alist-get 'key zoho-projects--task)
                            zoho-projects--current-tab))

;;;; Comments

(defun zoho-projects-add-comment ()
  "Jump to this task buffer's New Comment input, ready to type.
The New Comment entry sits collapsed at the top of the Comments
section; this unfolds it and puts point in its input field —
write the comment there and post it with
`zoho-projects-submit-comment'."
  (interactive)
  (unless zoho-projects--task (user-error "Not in a task buffer"))
  (let ((field (or (zoho-projects--comment-field)
                   (user-error "No New Comment section in this buffer"))))
    ;; The Time Logs tab narrows the field out of view.
    (unless (and (<= (point-min) (car field)) (>= (point-max) (cdr field)))
      (zoho-projects--set-tab "Comments")
      (setq field (zoho-projects--comment-field)))
    (goto-char (car field))
    (save-excursion
      (org-back-to-heading t)
      (if (fboundp 'org-fold-show-subtree)
          (org-fold-show-subtree)
        (outline-show-subtree)))
    (when (and (fboundp 'evil-insert-state)
               (bound-and-true-p evil-local-mode))
      (evil-insert-state))
    (message "Write the comment, then %s to post it"
             (substitute-command-keys
              "\\[zoho-projects-submit-comment]"))))

(defun zoho-projects-submit-comment ()
  "Post the New Comment section of this task buffer.
Sends in the background; on success the task is re-fetched so the
comment appears in the list and the field resets."
  (interactive)
  (unless zoho-projects--task (user-error "Not in a task buffer"))
  (let* ((field (or (zoho-projects--comment-field)
                    (user-error "No New Comment section in this buffer")))
         (content (string-trim (buffer-substring-no-properties
                                (car field) (cdr field))))
         (buf (current-buffer))
         (project zoho-projects--task-project)
         (task-id (zoho-projects--id zoho-projects--task))
         (task-key (alist-get 'key zoho-projects--task)))
    (when (string-empty-p content)
      (user-error "The comment is empty"))
    (message "Posting comment to %s…" (or task-key task-id))
    (zoho-projects--request-async
     "POST" (format "/portal/%s/projects/%s/tasks/%s/comments/"
                    (zoho-projects--ensure-portal)
                    (zoho-projects--id project) task-id)
     (lambda (_result err)
       (if err
           (zoho-projects--announce-write-failure "comment" err buf)
         (message "Comment posted to %s" (or task-key task-id))
         ;; Only if the buffer still shows this task, and without
         ;; stealing focus — same rules as a posted time log.
         (when (and (buffer-live-p buf)
                    (equal (zoho-projects--id
                            (buffer-local-value 'zoho-projects--task buf))
                           task-id))
           (with-current-buffer buf
             (zoho-projects--show-task project task-id task-key
                                       zoho-projects--current-tab t)))))
     :payload `(("content" ,content)))))

;;;; Time logs

(defun zoho-projects--format-duration (hours minutes)
  "Format an HOURS and MINUTES duration for messages."
  (format "%d:%02d" hours minutes))

(defun zoho-projects--post-time-log (project-id task-id minutes
                                                &optional date bill notes
                                                buf callback)
  "POST a time log of MINUTES to TASK-ID of PROJECT-ID.
DATE is an Emacs time value (nil: today), BILL a bill_status
string (nil: the default), NOTES the log notes.  On failure BUF,
when given, is refocused; on success CALLBACK, when given, is
called with no arguments."
  (let* ((total (max 1 (round minutes)))
         (hours (/ total 60))
         (mins (% total 60))
         (pretty (zoho-projects--format-duration hours mins)))
    (message "Logging %s on task %s…" pretty task-id)
    (zoho-projects--request-async
     "POST" (format "/portal/%s/projects/%s/tasks/%s/logs/"
                    (zoho-projects--ensure-portal) project-id task-id)
     (lambda (_result err)
       (if err
           (zoho-projects--announce-write-failure
            (format "time log on task %s" task-id) err buf)
         (message "Logged %s on task %s" pretty task-id)
         (when callback (funcall callback))))
     :payload `(("date" ,(format-time-string "%m-%d-%Y"
                                             (or date (current-time))))
                ("bill_status" ,(or bill zoho-projects-default-bill-status))
                ("hours" ,(format "%d:%02d" hours mins))
                ,@(let ((notes (string-trim (or notes ""))))
                    (unless (string-empty-p notes)
                      `(("notes" ,notes))))))))

(defun zoho-projects-submit-time-log ()
  "Post the New Time Log section of this task buffer.
Reads the Date, the Hours / Minutes duration, the Billing status
and the notes typed below them, and sends in the background; on
success the task is re-fetched so the new entry appears in the
Time Logs list and the fields are reset for the next one."
  (interactive)
  (unless zoho-projects--task (user-error "Not in a task buffer"))
  (let* ((notes-field (or (zoho-projects--notes-field)
                          (user-error
                           "No New Time Log section in this buffer")))
         (minutes (+ (* 60 (zoho-projects--duration-input 'hours))
                     (zoho-projects--duration-input 'minutes)))
         (date (zoho-projects--date-value))
         (bill (zoho-projects--bill-status-value))
         (notes (string-trim (buffer-substring-no-properties
                              (car notes-field) (cdr notes-field))))
         (buf (current-buffer))
         (project zoho-projects--task-project)
         (task-id (zoho-projects--id zoho-projects--task))
         (task-key (alist-get 'key zoho-projects--task)))
    (when (zerop minutes)
      (user-error "The duration is empty"))
    (zoho-projects--post-time-log
     (zoho-projects--id project) task-id minutes date bill notes buf
     (lambda ()
       ;; Only if the buffer still shows this task, and without
       ;; stealing focus — same rules as zoho-desk after a post.
       (when (and (buffer-live-p buf)
                  (equal (zoho-projects--id
                          (buffer-local-value 'zoho-projects--task buf))
                         task-id))
         (with-current-buffer buf
           (zoho-projects--show-task project task-id task-key
                                     zoho-projects--current-tab t)))))))

;;;###autoload
(defun zoho-projects-add-time-entry (project-id task-id minutes notes)
  "Add a time log of MINUTES with NOTES to TASK-ID of PROJECT-ID.
Interactively, the task is taken from the table line or task
buffer at point, falling back to prompts for the ids."
  (interactive
   (pcase-let ((`(,project . ,task)
                (or (zoho-projects--task-at-point) '(nil . nil))))
     (list (if project (zoho-projects--id project)
             (read-string "Project id: "))
           (if task (zoho-projects--id task)
             (read-string "Task id: "))
           (read-number "Minutes spent: " 30)
           (read-string "Notes: "))))
  (zoho-projects--post-time-log project-id task-id minutes nil nil notes))

;;;###autoload
(defun zoho-projects-log-time-from-org ()
  "Send the clocked time of the org entry at point to Zoho Projects.
The target task is read from the ZOHO_PROJECT_ID and ZOHO_TASK_ID
properties (inherited).  The prompt defaults to the entry's total
clocked minutes and its heading as notes."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org buffer"))
  (require 'org-clock)
  (let* ((project-id (or (org-entry-get nil "ZOHO_PROJECT_ID" t)
                         (user-error
                          "No ZOHO_PROJECT_ID property on this entry")))
         (task-id (or (org-entry-get nil "ZOHO_TASK_ID" t)
                      (user-error
                       "No ZOHO_TASK_ID property on this entry")))
         (clocked (save-excursion (org-clock-sum-current-item)))
         (minutes (read-number "Minutes to log: " (or clocked 0)))
         (notes (read-string "Notes: " (org-get-heading t t t t))))
    (zoho-projects--post-time-log project-id task-id minutes nil nil notes)))

;;;; Task timer (posframe-timer integration)

(defvar zoho-projects--timer-target nil
  "(PROJECT . TASK) the posframe-timer clock is running against, or nil.")

(defvar zoho-projects--pending-time-log nil
  "Finished timer waiting to land in a task's New Time Log fields.
A list (TASK-ID HOURS MINUTES START), consumed by
`zoho-projects--fill-pending-time-log' once a buffer showing
TASK-ID is rendered.")

;;;###autoload
(defun zoho-projects-start-task-timer ()
  "Clock the posframe timer in against the task at point.
The task key and name are shown above the running clock in the
posframe.  From anywhere in Emacs, finish with
`zoho-projects-finish-task-timer' (or `posframe-timer-clock-out')
to land the elapsed time in the task's New Time Log fields, ready
to describe and submit; discard with
`zoho-projects-cancel-task-timer'."
  (interactive)
  (unless (require 'posframe-timer nil t)
    (user-error "The posframe-timer package is not available"))
  (pcase-let ((`(,project . ,task)
               (or (zoho-projects--task-at-point)
                   (user-error "No task in context"))))
    ;; clock-in user-errors when a clock is already running, so the
    ;; task is only remembered once the clock is really ours.
    (posframe-timer-clock-in
     (format "%s %s"
             (or (alist-get 'key task) "")
             (or (alist-get 'name task) ""))
     #'zoho-projects--task-timer-out
     #'zoho-projects--task-timer-cancelled)
    (setq zoho-projects--timer-target (cons project task))))

;;;###autoload
(defun zoho-projects-finish-task-timer ()
  "Stop the task timer and open its New Time Log, duration filled in.
Callable from anywhere: the task buffer pops up on its Time Logs
tab with Date set to when the timer started and the Hours /
Minutes fields set to the elapsed time — describe the work and
submit with `zoho-projects-submit-time-log'."
  (interactive)
  (unless zoho-projects--timer-target
    (user-error "No task timer running"))
  (posframe-timer-clock-out))

;;;###autoload
(defun zoho-projects-cancel-task-timer ()
  "Discard the task timer without logging anything."
  (interactive)
  (unless zoho-projects--timer-target
    (user-error "No task timer running"))
  (posframe-timer-clock-cancel))

(defun zoho-projects--task-timer-cancelled (_start _label)
  "Forget the task the discarded clock was running against."
  (setq zoho-projects--timer-target nil))

(defun zoho-projects--task-timer-out (start end _label)
  "Land the clocked interval START..END in the task's New Time Log.
The elapsed time is parked in `zoho-projects--pending-time-log',
then the task buffer is brought up on its Time Logs tab: filled
immediately when it already shows the task, otherwise once the
fetch renders it."
  (pcase-let* ((`(,project . ,task) zoho-projects--timer-target)
               (id (zoho-projects--id task))
               (minutes (max 1 (round (/ (float-time
                                          (time-subtract end start))
                                         60))))
               (buf zoho-projects--task-buffer))
    (setq zoho-projects--timer-target nil
          zoho-projects--pending-time-log
          (list id (/ minutes 60) (% minutes 60) start))
    (if (and (buffer-live-p buf)
             (equal id (zoho-projects--id
                        (buffer-local-value 'zoho-projects--task buf))))
        (progn
          (pop-to-buffer
           buf
           `((display-buffer-reuse-window
              display-buffer-below-selected)
             (window-height . ,zoho-projects-task-window-height)))
          (zoho-projects--set-tab "Time Logs")
          (zoho-projects--fill-pending-time-log))
      (zoho-projects--show-task project id (alist-get 'key task)
                                "Time Logs"))))

(defun zoho-projects--fill-pending-time-log ()
  "Write the pending timer duration into this buffer's New Time Log.
No-op unless `zoho-projects--pending-time-log' targets the task
shown here; the pending entry is consumed, and point lands in the
notes field ready for `zoho-projects-submit-time-log'."
  (when-let* ((pending zoho-projects--pending-time-log)
              ((equal (car pending)
                      (zoho-projects--id zoho-projects--task))))
    (pcase-let ((`(,_id ,hours ,minutes ,start) pending))
      ;; Each field is looked up fresh because every insertion shifts
      ;; the positions of the fields after it.
      (zoho-projects--set-field (zoho-projects--date-field)
                                (format-time-string "[%Y-%m-%d %a]" start))
      (zoho-projects--set-field (zoho-projects--duration-field 'hours)
                                (number-to-string hours))
      (zoho-projects--set-field (zoho-projects--duration-field 'minutes)
                                (number-to-string minutes))
      (setq zoho-projects--pending-time-log nil)
      (when-let* ((notes (zoho-projects--notes-field)))
        (goto-char (car notes))
        ;; The tab switch collapsed every time log entry, New Time Log
        ;; included; open it back up so the filled fields are visible.
        (save-excursion
          (org-back-to-heading t)
          (if (fboundp 'org-fold-show-subtree)
              (org-fold-show-subtree)
            (outline-show-subtree)))
        (dolist (win (get-buffer-window-list nil nil t))
          (set-window-point win (car notes)))
        (when (and (fboundp 'evil-insert-state)
                   (bound-and-true-p evil-local-mode))
          (evil-insert-state)))
      (message "Timer stopped at %s — describe the work and %s to submit"
               (zoho-projects--format-duration hours minutes)
               (substitute-command-keys
                "\\[zoho-projects-submit-time-log]")))))

;;;; Org snippet

(defun zoho-projects-copy-org-snippet ()
  "Copy an org heading with the task's ids for the task at point."
  (interactive)
  (pcase-let ((`(,project . ,task)
               (or (zoho-projects--task-at-point)
                   (user-error "No task in context"))))
    (let ((key (or (alist-get 'key task) (zoho-projects--id task))))
      (kill-new (format (concat "* TODO %s %s\n:PROPERTIES:\n"
                                ":ZOHO_PROJECT_ID: %s\n"
                                ":ZOHO_TASK_ID: %s\n:END:\n")
                        key
                        (or (alist-get 'name task) "")
                        (zoho-projects--id project)
                        (zoho-projects--id task)))
      (message "Org heading for %s copied to kill ring" key))))

;;;; Evil integration

;; Evil's state maps outrank major-mode maps, so RET and friends must be
;; registered with Evil directly (same approach as zoho-desk).
(with-eval-after-load 'evil
  (evil-set-initial-state 'zoho-projects-statuses-mode 'normal)
  (evil-set-initial-state 'zoho-projects-tasks-mode 'normal)
  (evil-define-key* 'normal zoho-projects-statuses-mode-map
    (kbd "RET") #'zoho-projects-toggle-status-at-point
    (kbd "SPC") #'zoho-projects-toggle-status-at-point
    (kbd "x") #'zoho-projects-toggle-status-at-point
    (kbd "<mouse-1>") #'zoho-projects-mouse-toggle-status
    (kbd "g r") #'zoho-projects-refresh-tasks
    (kbd "P") #'zoho-projects-select-project
    (kbd "f") #'zoho-projects-quickfind
    (kbd "o") #'zoho-projects-browse-project
    (kbd "q") #'zoho-projects-quit)
  (evil-define-key* 'normal zoho-projects-tasks-mode-map
    (kbd "RET") #'zoho-projects-open-task-at-point
    (kbd "g r") #'zoho-projects-refresh-tasks
    (kbd "P") #'zoho-projects-select-project
    (kbd "f") #'zoho-projects-quickfind
    (kbd "t") #'zoho-projects-add-time-entry
    (kbd "T") #'zoho-projects-start-task-timer
    (kbd "w") #'zoho-projects-copy-org-snippet
    (kbd "o") #'zoho-projects-browse-task
    (kbd "q") #'zoho-projects-quit)
  (dolist (state '(normal motion))
    (evil-define-minor-mode-key state 'zoho-projects-task-minor-mode
      (kbd "gt") #'zoho-projects-tab-next
      (kbd "gT") #'zoho-projects-tab-previous))
  ;; evil-org's state maps outrank the plain minor-mode map, so field
  ;; cycling must be registered with Evil too.
  (dolist (state '(normal insert))
    (evil-define-minor-mode-key state 'zoho-projects-task-minor-mode
      (kbd "M-n") #'zoho-projects-next-field
      (kbd "M-p") #'zoho-projects-previous-field)))

(provide 'zoho-projects)
;;; zoho-projects.el ends here
