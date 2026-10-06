;;; command-audit.el --- Tally interactive command usage into JSON -*- lexical-binding: t; -*-

;;; Commentary:

;; Records which interactive commands are actually used, per day, into
;; a JSON file of {"command": {"YYYY-MM-DD": count}} — the data
;; foundation for later analysis (which commands earn their
;; keybindings, bar charts, a web UI).
;;
;; What gets counted is controlled by `command-audit-targets'.  A
;; target is either a command-name prefix string ("foo" counts all
;; foo-... commands, the package naming convention) or a Spacemacs
;; layer symbol, which resolves to the packages the layer owns.
;;
;; Only interactive invocations are counted: the recorder runs on
;; `post-command-hook' and reads `this-command', so internal function
;; calls never register.

;;; Code:

(require 'cl-lib)
(require 'json)

(declare-function configuration-layer/get-layer "core-configuration-layer")
(declare-function cfgl-layer-owned-packages "core-configuration-layer")

(defgroup command-audit nil
  "Tally interactive command usage per day into JSON."
  :group 'convenience
  :prefix "command-audit-")

(defcustom command-audit-log-file
  (expand-file-name "command-audit.json" user-emacs-directory)
  "JSON file tallying audited command usage per day."
  :type 'file
  :group 'command-audit)

(defcustom command-audit-targets nil
  "What to audit: command-name prefixes and/or Spacemacs layers.
A string entry is a command-name prefix — \"foo\" counts every
foo-... command, matching the package naming convention.  A symbol
entry names a Spacemacs layer and audits the prefixes of all the
packages that layer owns.  After changing this in a live session,
run `command-audit-refresh-targets'."
  :type '(repeat (choice (string :tag "Command-name prefix")
                         (symbol :tag "Spacemacs layer")))
  :group 'command-audit)

(defcustom command-audit-excluded-commands nil
  "Commands never tallied, even when they match an audited target.
Some commands are interactive only incidentally — navigation,
folding, RET dispatchers — and fire constantly without reflecting
deliberate usage.  List their symbols here to keep them out of the
log.  `command-audit-purge-excluded' also deletes their existing
tallies."
  :type '(repeat symbol)
  :group 'command-audit)

(defvar command-audit--prefixes 'unresolved
  "List of command-name prefix strings, or `unresolved'.
Resolved lazily from `command-audit-targets' on first use, after
Spacemacs' layer system is fully up.")

(defvar command-audit--counts 'unloaded
  "Alist of (COMMAND . ((DAY . COUNT) …)), or `unloaded'.")

(defun command-audit--target-prefixes (target)
  "Return the list of command-name prefixes TARGET stands for.
A string is itself a prefix.  A symbol names a Spacemacs layer,
whose owned packages' names become prefixes; a layer that cannot
be resolved falls back to its own name as a prefix."
  (if (stringp target)
      (list target)
    (or (ignore-errors
          (and (fboundp 'configuration-layer/get-layer)
               (let ((layer (configuration-layer/get-layer target)))
                 (and layer
                      (mapcar (lambda (pkg)
                                (symbol-name (if (listp pkg) (car pkg) pkg)))
                              (cfgl-layer-owned-packages layer))))))
        (list (symbol-name target)))))

(defun command-audit--ensure-prefixes ()
  "Return the audited command-name prefixes, resolving them if needed."
  (when (eq command-audit--prefixes 'unresolved)
    (setq command-audit--prefixes
          (mapcan #'command-audit--target-prefixes command-audit-targets)))
  command-audit--prefixes)

(defun command-audit-refresh-targets ()
  "Re-resolve `command-audit-targets' into command-name prefixes.
Run this after changing the targets in a live session."
  (interactive)
  (setq command-audit--prefixes 'unresolved))

(defun command-audit--ensure-counts ()
  "Return the loaded tallies, reading the log file if needed."
  (when (eq command-audit--counts 'unloaded)
    (setq command-audit--counts
          (and (file-exists-p command-audit-log-file)
               (ignore-errors
                 (json-parse-string
                  (with-temp-buffer
                    (insert-file-contents command-audit-log-file)
                    (buffer-string))
                  :object-type 'alist :array-type 'list)))))
  command-audit--counts)

(defun command-audit--write-counts ()
  "Write the in-memory tallies to `command-audit-log-file'."
  (with-temp-file command-audit-log-file
    (insert (json-encode command-audit--counts))))

(defun command-audit--record ()
  "Tally `this-command' when it belongs to an audited target.
On `post-command-hook' for every command, so the cheap tests come
first and any resolution or tallying trouble is swallowed — the
log must never break editing."
  (when (and (symbolp this-command)
             (not (memq this-command command-audit-excluded-commands)))
    (condition-case nil
        (progn
          (command-audit--ensure-prefixes)
          (let ((name (symbol-name this-command)))
            (when (cl-some (lambda (prefix) (string-prefix-p prefix name))
                           command-audit--prefixes)
              (command-audit--ensure-counts)
              (cl-incf (alist-get (intern (format-time-string "%Y-%m-%d"))
                                  (alist-get this-command
                                             command-audit--counts)
                                  0))
              (command-audit--write-counts))))
      (error nil))))

(defun command-audit-purge-excluded ()
  "Delete `command-audit-excluded-commands' tallies from the log."
  (interactive)
  (command-audit--ensure-counts)
  (let ((purged (cl-remove-if-not
                 (lambda (entry)
                   (memq (car entry) command-audit-excluded-commands))
                 command-audit--counts)))
    (when purged
      (setq command-audit--counts
            (cl-set-difference command-audit--counts purged))
      (command-audit--write-counts))
    (message "command-audit: purged %d command%s from the log"
             (length purged) (if (= (length purged) 1) "" "s"))))

(add-hook 'post-command-hook #'command-audit--record)

;;;; Usage report web UI
;;
;; `command-audit-serve' runs a small HTTP server inside Emacs that
;; renders the report template with the current JSON on every request
;; — reloading the page always shows current data.

(defconst command-audit--directory
  (file-name-directory (or load-file-name buffer-file-name
                           default-directory))
  "Directory this package was loaded from; holds the report template.")

(defcustom command-audit-template-file
  (expand-file-name "command-audit.html" command-audit--directory)
  "HTML template for the usage report.
When a page is served, its __COMMAND_AUDIT_DATA__ token is
replaced with the contents of `command-audit-log-file' and its
__COMMAND_AUDIT_META__ token with the audit configuration (the
resolved prefixes the report offers as filters, the log path, and
the render time)."
  :type 'file
  :group 'command-audit)

(defcustom command-audit-server-port 8377
  "Port the usage report is served on, at http://localhost:PORT/."
  :type 'integer
  :group 'command-audit)

(defvar command-audit--server-process nil
  "The report server's listening process, or nil.")

(defun command-audit--data ()
  "Return the usage log as a JSON string, or an empty object."
  (if (file-exists-p command-audit-log-file)
      (with-temp-buffer
        (insert-file-contents command-audit-log-file)
        (buffer-string))
    "{}"))

(defun command-audit--meta ()
  "Return the report's configuration as a JSON string.
The prefixes travel to the page so its filter chips come from
`command-audit-targets' rather than being hard-coded in the
template."
  (json-encode
   (list (cons "prefixes" (vconcat (command-audit--ensure-prefixes)))
         (cons "targets" (vconcat (mapcar (lambda (target) (format "%s" target))
                                          command-audit-targets)))
         (cons "excluded" (vconcat (mapcar #'symbol-name
                                           command-audit-excluded-commands)))
         (cons "logFile" (abbreviate-file-name command-audit-log-file))
         (cons "generated" (format-time-string "%Y-%m-%dT%H:%M:%S")))))

(defun command-audit--page ()
  "Return the report HTML with the current usage data filled in."
  (with-temp-buffer
    (insert-file-contents command-audit-template-file)
    (dolist (token (list (cons "__COMMAND_AUDIT_DATA__" (command-audit--data))
                         (cons "__COMMAND_AUDIT_META__" (command-audit--meta))))
      (goto-char (point-min))
      (when (search-forward (car token) nil t)
        (replace-match (cdr token) t t)))
    (buffer-string)))

(defun command-audit--http-send (proc status content-type body)
  "Send one HTTP response on PROC and close the connection."
  (let ((bytes (encode-coding-string body 'utf-8)))
    (process-send-string
     proc
     (format (concat "HTTP/1.1 %s\r\n"
                     "Content-Type: %s\r\n"
                     "Content-Length: %d\r\n"
                     "Connection: close\r\n\r\n")
             status content-type (length bytes)))
    (process-send-string proc bytes))
  (process-send-eof proc)
  (delete-process proc))

(defun command-audit--server-filter (proc chunk)
  "Answer an HTTP request on PROC once its headers have arrived."
  (set-process-query-on-exit-flag proc nil)
  (let ((request (concat (process-get proc :request) chunk)))
    (process-put proc :request request)
    (when (string-match-p "\r\n\r\n" request)
      (condition-case nil
          (let ((path (and (string-match "\\`GET \\([^ ?]+\\)" request)
                           (match-string 1 request))))
            (cond
             ((member path '("/" "/index.html"))
              (command-audit--http-send
               proc "200 OK" "text/html; charset=utf-8"
               (command-audit--page)))
             ;; The page refetches this to refresh without a reload.
             ((equal path "/data.json")
              (command-audit--http-send
               proc "200 OK" "application/json; charset=utf-8"
               (format "{\"data\":%s,\"meta\":%s}"
                       (command-audit--data) (command-audit--meta))))
             (t
              (command-audit--http-send
               proc "404 Not Found" "text/plain" "not found"))))
        (error (ignore-errors (delete-process proc)))))))

(defun command-audit--server-start ()
  "Start (or restart) the report server; return its URL."
  (command-audit-stop-server)
  (setq command-audit--server-process
        (make-network-process
         :name "command-audit-server"
         :server t :noquery t :host 'local
         :service command-audit-server-port
         :filter #'command-audit--server-filter))
  (format "http://localhost:%d/" command-audit-server-port))

(defun command-audit-serve ()
  "Serve the usage report from Emacs and open it in the browser.
Every request re-reads `command-audit-log-file', so reloading the
page shows current data."
  (interactive)
  (let ((url (command-audit--server-start)))
    (browse-url url)
    (message "command-audit: serving usage report at %s" url)))

(defun command-audit-stop-server ()
  "Stop the usage report server."
  (interactive)
  (when (process-live-p command-audit--server-process)
    (delete-process command-audit--server-process))
  (setq command-audit--server-process nil))

(provide 'command-audit)
;;; command-audit.el ends here
