;;; hydra-org-refile.el --- Hydra for handling refile points in org-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar zp/hydra-org-refile-chain nil
    "When non-nil, make zp/hydra-org-refile chain the commands.")

(defun zp/hydra-org-refile-chain-toggle ()
  "Toggle zp/hydra-org-refile-chain."
  (interactive)
  (setq zp/hydra-org-refile-chain
        (not zp/hydra-org-refile-chain)))

(defvar zp/hydra-org-jump-indirect t
  "When non-nil, jumping to a refile point is done in an indirect buffer.")

(defun zp/hydra-org-jump-indirect-toggle ()
  "Toggle zp/hydra-org-jump-indirect."
  (interactive)
  (setq zp/hydra-org-jump-indirect
        (not zp/hydra-org-jump-indirect)))

(defvar zp/hydra-org-jump-dedicated-buffer nil
  "When non-nil, jumping to a refile point is done in a dedicated buffer.")

(defun zp/hydra-org-jump-dedicated-buffer-toggle ()
  "Toggle zp/hydra-org-dedicated-buffer."
  (interactive)
  (setq zp/hydra-org-jump-dedicated-buffer
        (not zp/hydra-org-jump-dedicated-buffer)))

(defvar zp/hydra-org-refile-from nil
  "When non-nil, refiling is done from a refile point to another one.")

(defun zp/hydra-org-refile-from-toggle ()
  "Toggle zp/hydra-org-from."
  (interactive)
  (setq zp/hydra-org-refile-from
        (not zp/hydra-org-refile-from)))

(defun zp/org-refile (&optional print-message jump)
  "Refile the current heading to another with completion.

When JUMP is non-nil, jump to that other heading instead."
  (interactive "p")
  (let ((zp/hydra-org-jump-indirect nil)
        (in-agenda (derived-mode-p 'org-agenda-mode))
        (org-refile-use-outline-path t)
        (org-refile-history nil)
        file
        pos
        target)
    (when (and (not in-agenda)
               (org-before-first-heading-p))
      (outline-next-heading))
    (save-window-excursion
      (when in-agenda
        (org-goto-marker-or-bmk (or (get-text-property (point) 'org-marker)
                                    (get-text-property (point) 'org-hd-marker))))
      (org-refile (if jump '(4) t))
      (setq file (buffer-file-name))
      (setq pos (point-marker)))
    (zp/org-refile-to file pos print-message jump)
    (set-marker pos nil)
    (setq target (point))))

(defun zp/org-jump (&optional print-message)
  "Jump to another heading."
  (interactive "p")
  (goto-char (save-excursion (zp/org-refile print-message t))))

(defun zp/org-refile-dwim (arg)
  "Conditionally move the entry or entries at point to another heading.

With a ‘C-u’ prefix, refile to another heading within the current
restriction.

With two ‘C-u’ prefixes, refile to another heading in the other
window’s buffer."
  (interactive "P")
  (pcase arg
    ('(4) (if (buffer-narrowed-p)
              (zp/org-refile-restricted t)
            (zp/org-refile t)))
    ('(16) (zp/org-refile-to-other-buffer t))
    (_ (zp/hydra-org-refile))))

(defun zp/org-jump-dwim (arg)
  "Conditionally jump to another heading.

With a ‘C-u’ prefix, jump to another heading within the current
restriction."
  (interactive "P")
  (pcase arg
    ('(4) (if (buffer-narrowed-p)
              (zp/org-jump-restricted t)
            (zp/org-jump t)))
    (_ (zp/hydra-org-jump/body))))

(defvar zp/org-agenda-files-primary nil
  "Primary org-agenda file.")

(setq zp/org-agenda-files-primary "~/org/life.org")

(defun zp/org-refile-main (&optional print-message jump)
  "Refile current heading to another in org-agenda file.

If JUMP is non-nil, jump to it instead."
  (interactive "p")
  (let ((org-refile-targets '((zp/org-agenda-files-primary :maxlevel . 1))))
    (zp/org-refile print-message jump)
    (when (and jump
               zp/hydra-org-jump-indirect)
      (zp/org-tree-to-indirect-buffer-folded
       nil
       zp/hydra-org-jump-dedicated-buffer
       jump))))

(defun zp/org-jump-main (&optional print-message)
  "Jump to heading in main org-agenda file."
  (interactive "p")
  (let ((dedicated zp/hydra-org-jump-dedicated-buffer))
    ;; Go to primary file to suppress its name from the target points
    (with-current-buffer (find-file-noselect zp/org-agenda-files-primary)
      (zp/org-refile-main print-message t))))

(defun zp/org-refile-target-verify-exclude-separators ()
  "Exclude separators line from refile targets."
  (let ((regex "^\\* -+.*-+$"))
    ;; (message (buffer-substring-no-properties (point) (line-end-position)))
    (if (re-search-forward regex (line-end-position) t)
        nil
      t)))

(defvar zp/org-refile-target-verify-restricted--min nil)

(defvar zp/org-refile-target-verify-restricted--max nil)

(defun zp/org-refile-target-verify-restricted ()
  "Exclude refile targets which aren’t in the current restriction."
  (let ((regex "^\\* -+.*-+$")
        (min zp/org-refile-target-verify-restricted--min)
        (max zp/org-refile-target-verify-restricted--max))
    ;; (message (buffer-substring-no-properties (point) (line-end-position)))
    (cond ((< (point) min)
           (goto-char min)
           nil)
          ((> (point) max)
           (goto-char (point-max))
           nil)
          (t
           (zp/org-refile-target-verify-exclude-separators)))))

(defun zp/org-refile-restricted (&optional print-message jump)
    "Refile current heading to another within the current restriction.

If JUMP is non-nil, jump instead."
  (interactive "p")
  (let ((org-refile-targets '((nil :maxlevel . 9)))
        (org-refile-target-verify-function #'zp/org-refile-target-verify-restricted)
        (min (point-min))
        (max (point-max))
        target)
    ;; Storing restriction in dynamic variables
    (setq zp/org-refile-target-verify-restricted--min min
          zp/org-refile-target-verify-restricted--max max)
    (zp/org-refile print-message jump)))

(defun zp/org-jump-restricted (&optional print-message)
  "Jump to a heading within the current restriction."
  (interactive "p")
  (let ((indirect (not (buffer-file-name)))
        target
        (buffer (current-buffer)))
    (save-excursion
      (setq target (zp/org-refile-restricted print-message t)))
    (when indirect (switch-to-buffer buffer))
    (goto-char target)
    (org-reveal)
    (org-beginning-of-line)))

(defun zp/org-refile-to-other-buffer (&optional print-message)
  "Refile current heading to another within the other window’s buffer."
  (interactive)
  (let* ((other (next-window))
         (filepath (with-selected-window other
                     (or (buffer-file-name)
                         (buffer-file-name (buffer-base-buffer)))))
         (pos (with-selected-window other
                (save-restriction
                  (zp/org-jump-restricted)
                  (point))))
         (marker (save-window-excursion
                   (with-current-buffer (get-file-buffer filepath)
                     (goto-char pos)
                     (point-marker)))))
    (zp/org-refile-to filepath marker print-message)
    ;; (run-hooks 'zp/org-after-refile-hook)
    (with-selected-window other
      (zp/org-overview nil t t)
      (goto-char
       (bookmark-get-position
        (plist-get org-bookmark-names-plist :last-refile)))
      (org-reveal)
      (org-beginning-of-line))
    (set-marker marker nil)))

(defun zp/org-refile-internal (file headline-or-olp &optional arg)
  "Refile to a specific location.

With a ‘C-u’ prefix, we jump to that location (see ‘org-refile’).
Use ‘org-agenda-refile’ in ‘org-agenda’ mode.

If HEADLINE-OR-OLP is a string, interprets it as a heading.  If
HEADLINE-OR-OLP is a list, interprets it as an olp path (without
the filename)."
  (let* ((pos (with-current-buffer
                  (or (get-buffer file) ;Is the file open in a buffer already?
                      (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
                (or (cond ((markerp headline-or-olp)
                           headline-or-olp)
                          ((listp headline-or-olp)
                           (org-find-olp `(,(buffer-file-name) ,@headline-or-olp)))
                          (t
                           (org-find-exact-headline-in-buffer headline-or-olp)))
                    (error "Can't find headline-or-olp `%s'" headline-or-olp))))
         (filepath (buffer-file-name (marker-buffer pos))) ;If we're given a relative name, find absolute path
         (rfloc (list nil filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode)
             (not arg)) ;Don't use org-agenda-refile if we're just jumping
        (org-agenda-refile nil rfloc)
      (org-refile (when arg '(4)) nil rfloc))
    pos))

(defun zp/org-capture-refile-internal (file headline-or-olp &optional arg)
  "Copied from ‘org-capture-refile’ since it doesn't allow
passing arguments. This does."
  (unless (eq (org-capture-get :type 'local) 'entry)
    (error
     "Refiling from a capture buffer makes only sense for `entry'-type templates"))
  (let ((pos (point))
        (base (buffer-base-buffer (current-buffer)))
        (org-capture-is-refiling t)
        (kill-buffer (org-capture-get :kill-buffer 'local)))
    (org-capture-put :kill-buffer nil)
    (when (buffer-narrowed-p)
      (goto-char (point-min)))
    (org-capture-finalize)
    (prog1 (save-window-excursion
             (with-current-buffer (or base (current-buffer))
               (org-with-wide-buffer
                (goto-char pos)
                (zp/org-refile-internal file headline-or-olp arg))))
      (when kill-buffer (kill-buffer base)))))

(defvar zp/org-after-refile-hook nil
  "Hook run after a successful zp/org-refile.
Also run after a jump.")

(defun zp/org-refile-to (file headline-or-olp &optional print-message jump)
  "Refile current heading to specified destination.

When JUMP is non-nil, jump to that destination instead."
  (interactive "p")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode))
        (org-refile-history nil)
        pos)
    (setq pos (if (and is-capturing
                       (not jump))
                  (zp/org-capture-refile-internal file headline-or-olp jump)
                (zp/org-refile-internal file headline-or-olp jump)))
    (when (and is-capturing
               (not jump)
               ;; If capturing, deactivate hydra
               (setq hydra-deactivate t)))
    (when (and jump
               zp/hydra-org-jump-indirect)
      (zp/org-tree-to-indirect-buffer-folded
       nil
       zp/hydra-org-jump-dedicated-buffer
       t))
    (when print-message
      (run-hooks 'zp/org-after-refile-hook)
      (if jump
          (message "Jumped to tree: %s."
                   ;; Create string for path
                   (mapconcat 'identity
                              (org-get-outline-path t)
                              " → "))
        (message (concat "Refiled tree to "
                         (org-with-point-at pos
                           (mapconcat 'identity (org-get-outline-path t) " → "))
                         "."))))))

(defun zp/org-jump-to (file headline-or-olp &optional print-message)
  "Jump to a specified destination."
  (interactive "p")
  (zp/org-refile-to file headline-or-olp print-message t))

(defun zp/org-refile-to-or-from (file headline-or-olp &optional print-message jump)
  "Refile current heading to or from specified destination."
  (interactive "p")
  (let ((from zp/hydra-org-refile-from))
    (if zp/hydra-org-refile-from
        (zp/org-refile-from file headline-or-olp print-message jump)
      (zp/org-refile-to file headline-or-olp print-message jump))))

(defun zp/org-jump-to-or-from (file headline-or-olp &optional print-message)
  (interactive "p")
  (zp/org-refile-to-or-from file headline-or-olp print-message t))

(defun zp/org-refile-from (file headline-or-olp &optional print-message jump)
  (let* ((from-buffer (save-window-excursion
                        (let ((zp/hydra-org-jump-dedicated-buffer t))
                          (zp/org-jump-to file headline-or-olp))
                        (current-buffer)))
         (filepath (buffer-file-name (buffer-base-buffer from-buffer)))
         (pos (save-window-excursion
                (with-current-buffer from-buffer
                  (zp/org-jump-restricted)
                  (prog1 (point)
                    (kill-buffer)))))
         (marker (save-window-excursion
                   (with-current-buffer (get-file-buffer filepath)
                     (goto-char pos)
                     (point-marker)))))
    (zp/org-refile-to filepath marker print-message jump)
    (set-marker marker nil)))

(defvar zp/hydra-org-refile-active nil
  "t if currently in a hydra-org-refile session.

Used to check whether hydra-org-refile was exited
abnormally (e.g. with a C-g).")

(defun zp/hydra-org-refile-cleanup ()
  "Reset variables used by zp/hydra-org-refile to their defaults"
  (setq zp/hydra-org-jump-dedicated-buffer nil
        zp/hydra-org-jump-active nil
        zp/hydra-org-refile-from nil))

(defun zp/hydra-org-refile ()
  "Wrapper for zp/hydra-org-refile.

Ensures that the toggles are set to their default variable."
  (interactive)
  (when zp/hydra-org-refile-active
    (zp/hydra-org-refile-cleanup))
  (zp/hydra-org-refile/body))

(defun zp/hydra-org-jump ()
    "Wrapper for zp/hydra-org-jump.

Ensures that the toggles are set to their default variable."
  (interactive)
  (when zp/hydra-org-refile-active
    (zp/hydra-org-refile-cleanup))
  (zp/hydra-org-jump/body))

(defmacro zp/create-hydra-org-refile-protocol (protocol chain name docstring targets &optional heads back)
  (declare (indent defun) (doc-string 2))
  (let* ((protocol-name (symbol-name protocol))
         (hydra (intern (concat "zp/hydra-org-"
                                protocol-name
                                (when chain
                                  "-chain")
                                (when name
                                  (concat "-" (symbol-name name))))))
         (hydra-sister (intern (concat "zp/hydra-org-"
                                       protocol-name
                                       (unless chain
                                         "-chain")
                                       (when name
                                         (concat "-" (symbol-name name)))
                                       "/body")))
         (hydra-back (intern (concat "zp/hydra-org-"
                                     protocol-name
                                     (when chain
                                       "-chain")
                                     (when back
                                       (concat "-" (symbol-name back)))
                                     "/body")))
         (docstring-refile (concat "\n["
                                   (upcase protocol-name)
                                   "]\n" docstring "\n"))
         (command (pcase protocol
                    ('refile 'zp/org-refile-to-or-from)
                    ('jump 'zp/org-jump-to-or-from)))
         (jumping (if (eq protocol 'jump) t)))
    `(defhydra ,hydra
         (:foreign-keys warn
          :exit ,(if chain nil t)
          :pre (progn
                 (setq zp/hydra-org-refile-active t)
                 ,(if chain `(setq zp/hydra-org-refile-chain t) nil))
          :post (progn
                  ,(if chain `(setq zp/hydra-org-refile-chain nil) nil))
          :hint nil)
       ,docstring-refile
       ;; Create targets
       ,@(mapcar (lambda (target)
                   (let* ((key (car target))
                          (file+olp (cdr target))
                          (file (car file+olp))
                          (olp (cdr file+olp)))
                     `(,key (progn
                              (,command ,file ',olp t)
                              ,(unless chain
                                 `(zp/hydra-org-refile-cleanup))))))
                 targets)
       ;; Create other heads
       ,@(when heads
           (mapcar (lambda (head)
                     (let* ((key (car head))
                            (head-name (symbol-name (cadr head)))
                            (head-hydra (intern (concat "zp/hydra-org-"
                                                        protocol-name
                                                        (when chain
                                                          "-chain")
                                                        "-" head-name
                                                        "/body"))))
                       `(,key ,head-hydra :exit t)))
                   heads))
       ;; Conditional actions
       ("C-c" ,hydra-sister
            (concat (if zp/hydra-org-refile-chain
                        "[x]"
                      "[ ]")
                    " chain") :exit t)
       ("C-f" zp/hydra-org-refile-from-toggle
            (concat (if zp/hydra-org-refile-from
                        "[x]"
                      "[ ]")
                    " from") :exit nil)
       ,@(cond (jumping
                `(("C-i" zp/hydra-org-jump-indirect-toggle
                       (concat (if zp/hydra-org-jump-indirect
                                   "[x]"
                                 "[ ]")
                               " indirect") :exit nil)
                  ("C-d" zp/hydra-org-jump-dedicated-buffer-toggle
                       (concat (if zp/hydra-org-jump-dedicated-buffer
                                   "[x]"
                                 "[ ]")
                               " dedicated") :exit nil)
                  ("C-j" (progn (zp/org-jump-main t)
                              ,(unless chain
                                 `(zp/hydra-org-refile-cleanup))) "jump")))
               (t
                `(("C-w" (progn (zp/org-refile-main t)
                              ,(unless chain
                                 `(zp/hydra-org-refile-cleanup))) "refile")
                  ("W" zp/org-refile-with-paths "refile+paths")
                  ("0" (zp/org-refile-with-paths '(64)) "reset cache" :exit nil))))
       ,@(when name `(("<backspace>" ,hydra-back "back" :exit t)))
       ("q" (progn
              (zp/hydra-org-refile-cleanup)
              (message "Cancelled")) "cancel" :exit t))))

(defmacro zp/create-hydra-org-refile (name docstring targets &optional heads back)
  (declare (indent 2) (doc-string 2))
  `(progn
     (zp/create-hydra-org-refile-protocol refile nil
       ,name ,docstring ,targets ,heads ,back)
     (zp/create-hydra-org-refile-protocol refile t
       ,name ,docstring ,targets ,heads ,back)
     (zp/create-hydra-org-refile-protocol jump nil
       ,name ,docstring ,targets ,heads ,back)
     (zp/create-hydra-org-refile-protocol jump t
       ,name ,docstring ,targets ,heads ,back)))

(provide 'hydra-org-refile)
;;; test.el ends here
