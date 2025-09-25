;;; mini-posframe.el --- Minibuffer mirrored in posframe (session-scoped) -*- lexical-binding: t; -*-

(require 'posframe)
(require 'subr-x) ;; for string-empty-p
(defcustom minibuffer-posframe-font-scale 1.4
  "Base scale for the posframe font (relative to default face)."
  :type 'number :group 'minibuffer-posframe)

(defcustom minibuffer-posframe-font-scale-min 1.0
  "Minimum scale the posframe font can shrink to when content is long."
  :type 'number :group 'minibuffer-posframe)

(defvar minibuffer-posframe-buffer " *minibuffer-posframe*"
  "Buffer used for minibuffer posframe display.")

(defcustom minibuffer-posframe-width 60
  "Fixed width of minibuffer posframe (in characters)."
  :type 'integer
  :group 'minibuffer-posframe)

(defcustom minibuffer-posframe-height 1
  "Fixed height of minibuffer posframe (in lines)."
  :type 'integer
  :group 'minibuffer-posframe)

(defface minibuffer-posframe
  '((t (:inherit default :height 1.2)))  ;; Bigger font for emphasis
  "Face for minibuffer posframe text."
  :group 'minibuffer-posframe)

(defface minibuffer-posframe-border
  '((t (:inherit default :background "#4c78cc")))  ;; nice muted blue
  "Face for minibuffer posframe border."
  :group 'minibuffer-posframe)


(defun poshandler-top-10-center (info)
  "Return (x . y) so posframe sits centered, 10% from top."
  (let* ((pfw (plist-get info :parent-frame-width))
         (pfh (plist-get info :parent-frame-height))
         (fw  (plist-get info :posframe-width))
         (x (/ (- pfw fw) 2))
         (y (max 0 (floor (* pfh 0.10)))))
    (cons x y)))

;; --- Evil search status capture ---

(defvar minibuffer-last-message nil
  "Last Evil search status message to display in minibuffer posframe.")

(defun capture-evil-search-message (orig-fn fmt &rest args)
  "Advice around `message` to capture Evil search status like `[Search failed]`.
Safely ignores nil or empty FORMAT."
  (if (null fmt)
      ;; Just clear echo area → don’t try to format
      (apply orig-fn fmt args)
    (let ((msg (apply #'format fmt args)))
      (when (and msg (string-match-p "\\[Search" msg))
        (setq minibuffer-last-message msg))
      (apply orig-fn fmt args))))

(advice-add 'message :around #'capture-evil-search-message)

;; --- Core logic ---

(defun mini-posframe-active-p ()
  "Return non-nil if minibuffer posframe should run."
  (and (active-minibuffer-window)
       (window-live-p (active-minibuffer-window))
       (minibufferp (window-buffer (active-minibuffer-window)))
       (not (bound-and-true-p helm-alive-p))))


(defun minibuffer-refresh-posframe ()
  "Safely mirror minibuffer in a posframe, with fake cursor tracking."
  (condition-case _
      (when (mini-posframe-active-p)
        (let* ((win (active-minibuffer-window))
               (buf (and (window-live-p win) (window-buffer win))))
          (when buf
            (with-current-buffer buf
              (let* ((raw (ignore-errors
                            (buffer-substring (point-min) (point-max))))
                     (status-msg (when minibuffer-last-message
                                   (propertize minibuffer-last-message 'face 'error)))
                     (max-width minibuffer-posframe-width))
                ;; Only proceed if we have a real string
                (when (and raw (stringp raw))
                  (let* ((text (if (> (length raw) max-width)
                                   (substring raw (- (length raw) max-width))
                                 raw))
                         ;; --- fake cursor insertion ---
                         (cursor-idx (max 0 (min (length text)
                                                 (- (point) (point-min)))))
                         (before (if (> cursor-idx 0)
                                     (substring text 0 cursor-idx)
                                   ""))
                         (cursor-char (if (< cursor-idx (length text))
                                          (substring text cursor-idx (1+ cursor-idx))
                                        " "))
                         (after (if (< cursor-idx (length text))
                                    (substring text (1+ cursor-idx))
                                  ""))
                         (cursor (propertize cursor-char 'face '(:inverse-video t)))
                         (with-cursor (concat before cursor after))
                         (full (if status-msg
                                   (concat with-cursor "  " status-msg)
                                 with-cursor))
                         (bg (face-background 'solaire-default-face nil t)))
                    (save-window-excursion
                      (posframe-show minibuffer-posframe-buffer
                                     :string (if (string-empty-p full) " " full)
                                     :poshandler #'poshandler-top-10-center
                                     :internal-border-width 2
                                     :lines-truncate t
                                     :width minibuffer-posframe-width
                                     :height minibuffer-posframe-height
                                     :face 'minibuffer-posframe
                                     :foreground-color (face-attribute 'minibuffer-posframe :foreground nil t)
                                     :background-color bg
                                     :internal-border-color (face-attribute 'minibuffer-posframe-border :background nil t)
                                     :font (format "%s-%d"
                                                   (face-attribute 'default :family)
                                                   (round (* 1.4
                                                             (/ (face-attribute 'default :height)
                                                                10.0)))))))))))))
    (error
     (minibuffer-hide-posframe))))


(defun minibuffer-hide-posframe ()
  "Hide minibuffer posframe and reset state."
  (ignore-errors (posframe-delete minibuffer-posframe-buffer))
  (setq minibuffer-last-message nil))

(defun hide-minibuffer-maybe ()
  "Make real minibuffer invisible (input still works)."
  (when (mini-posframe-active-p)
    (let* ((bg (face-background 'solaire-default-face nil t))
           (ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face `(:foreground ,bg :background ,bg))
      (setq-local cursor-type nil))))

;; --- Session management ---

(defun minibuffer-session-start ()
  "Enable posframe refresh only for this minibuffer session."
  (when mini-posframe-mode
    ;; Hooks are buffer-local to minibuffer
    (add-hook 'post-command-hook #'minibuffer-refresh-posframe nil t)
    (add-hook 'post-command-hook #'hide-minibuffer-maybe nil t)))

(defun minibuffer-session-end ()
  "Disable posframe refresh after minibuffer session ends."
  (minibuffer-hide-posframe)
  (remove-hook 'post-command-hook #'minibuffer-refresh-posframe t)
  (remove-hook 'post-command-hook #'hide-minibuffer-maybe t))

;; --- Mode definition ---

(define-minor-mode mini-posframe-mode
  "Global minor mode to show minibuffer input in a posframe."
  :global t
  (if mini-posframe-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'minibuffer-session-start)
        (add-hook 'minibuffer-exit-hook  #'minibuffer-session-end)
        (add-hook 'post-command-hook     #'minibuffer-auto-cleanup) ;; NEW
        (message "Mini-Posframe mode enabled"))
    (remove-hook 'minibuffer-setup-hook #'minibuffer-session-start)
    (remove-hook 'minibuffer-exit-hook  #'minibuffer-session-end)
    (remove-hook 'post-command-hook     #'minibuffer-auto-cleanup) ;; NEW
    (minibuffer-hide-posframe)
    (message "Mini-Posframe mode disabled")))

;; --- Hard reset command ---

(defun mini-posframe-reset (&optional reenable)
  "Forcefully reset mini-posframe mode.
Disables the mode, removes hooks, deletes posframe, and resets state.
With optional REENABLE non-nil, turn the mode back on afterwards."
  (interactive "P")
  (mini-posframe-mode -1)
  (minibuffer-hide-posframe)
  (message "[mini-posframe] Hard reset complete")
  (when reenable
    (mini-posframe-mode 1)
    (message "[mini-posframe] Mode re-enabled after reset")))

(defun minibuffer-auto-cleanup ()
  "Force cleanup if minibuffer hooks are still active but minibuffer is gone.
Also remove this function from `post-command-hook` until next minibuffer session."
  (unless (active-minibuffer-window)
    (minibuffer-hide-posframe)
    ;; Remove any minibuffer-local hooks that might have leaked
    (remove-hook 'post-command-hook #'minibuffer-refresh-posframe t)
    (remove-hook 'post-command-hook #'hide-minibuffer-maybe t)
    ;; Remove this global watcher until next minibuffer session
    (remove-hook 'post-command-hook #'minibuffer-auto-cleanup)
    (message "[mini-posframe] Auto-cleaned and disabled global watcher")))

(defun debug-minibuffer-exit ()
  (message "[mini-posframe DEBUG] minibuffer-exit-hook fired in %S"
           (current-buffer)))
(add-hook 'minibuffer-exit-hook #'debug-minibuffer-exit)

(defun helm-ff-delete-char-backward-advice (orig-fn &rest args)
  "Prevent Helm from executing nil subkey commands."
  (let ((cmd (apply orig-fn args)))
    (when (commandp cmd)
      (command-execute cmd))
    cmd))

(advice-add 'helm-helm-ff-delete-char-backward-with-subkeys :around
            #'helm-ff-delete-char-backward-advice)

(provide 'mini-posframe)
