;;; zp-notmuch-fetch.el --- Async fetch of mail for notmuch  -*- fill-column: 78; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defvar zp/notmuch-fetch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'kill-buffer-and-window)
    (define-key map "S-DEL" 'kill-buffer-and-window)
    map)
  "Keymap for function `zp/notmuch-fetch-mode'.")

(define-minor-mode zp/notmuch-fetch-mode
  :light " fetch"
  :keymap zp/notmuch-fetch-mode-map
  (cond (zp/notmuch-fetch-mode
         (read-only-mode +1))
        (zp/notmuch-fetch-mode
         (read-only-mode -1))))

(defcustom zp/notmuch-fetch-buffer "*notmuch - fetch mail*"
  "Org-roam buffer name."
  :type 'string)

(defun zp/notmuch-fetch--find-buffer ()
  "Find or create the buffer for zp/notmuch-fetch."
  (let* ((name zp/notmuch-fetch-buffer)
         (buffer (get-buffer-create name)))
    (display-buffer-in-side-window buffer '((side . bottom)))
    (with-current-buffer buffer
      (zp/notmuch-fetch-mode +1))
    buffer))

(defvar zp/notmuch-fetch-process nil
  "Hold the info for the current process.")

(defun zp/notmuch-fetch--live-p (&optional proc)
  "Return t if currently fetching mail.
If PROC is not given, use the value of
`zp/notmuch-fetch-process'."
  (let ((proc (or proc
                  zp/notmuch-fetch-process)))
    (process-live-p proc)))

(defun zp/notmuch-fetch-kill (&optional proc)
  "Kill the notmuch-fetch process.
If PROC is not given, use the value of `zp/notmuch-fetch-process'.
Warn if it is not finished."
  (interactive)
  (let* ((proc (or proc
                   zp/notmuch-fetch-process))
         (buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun zp/notmuch-fetch-cleanup ()
  "Clean up the buffer after a successful fetch."
  (unless (zp/notmuch-fetch--live-p)
    (zp/notmuch-fetch-kill)))

(defcustom zp/notmuch-fetch-cleanup-auto-delay 2
  "Number of seconds before automatically cleaning up.")

(defun zp/notmuch-fetch-cleanup-auto ()
  "Automatically cleanup the buffer after a successful fetch."
  (let ((delay zp/notmuch-fetch-cleanup-auto-delay))
    (run-with-timer delay nil #'zp/notmuch-fetch-cleanup)))

(defcustom zp/notmuch-fetch-post-hook '(zp/notmuch-fetch-cleanup-auto)
  "Hook run after a fetch.")

(defun zp/notmuch-fetch--post ()
  "Refresh notmuch-hello and run post fetch functions."
  (seq-do (lambda (buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'notmuch-hello-mode)
                (notmuch-refresh-this-buffer))))
          (buffer-list))
  (run-hooks 'zp/notmuch-fetch-post-hook))

(defcustom zp/notmuch-fetch-default-command "check-mail-emacs"
  "Default command to use with `zp/notmuch-fetch-new-mail'.")

(defun zp/notmuch-fetch-new-mail (&optional command)
  "Fetch new mail with external COMMAND.
If COMMAND is nil, use"
  (interactive)
  (let* ((output-buffer (zp/notmuch-fetch--find-buffer))
         (proc (progn))
         (command (or command
                      zp/notmuch-fetch-default-command)))
    (async-shell-command command output-buffer)
    ;; (start-process-shell-command "zp/notmuch-fetch" output-buffer command)
    (setq zp/notmuch-fetch-process (get-buffer-process output-buffer))))

(defun zp/notmuch-fetch-new-mail-inbox ()
  "Fetch new mail in inbox."
  (interactive)
  (zp/notmuch-fetch-new-mail "check-mail-emacs inbox"))

(provide 'zp-notmuch-fetch)
;;; zp-notmuch-fetch.el ends here
