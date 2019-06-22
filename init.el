;; -*- fill-column: 72; comment-column: 50; -*-
;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; =============  INIT FILE  ==============
;; ================== * ===================
;; =============  ~ Zaeph ~  ==============
;; =============  Dream on.  ==============
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq default-directory "/home/zaeph/")
(setq inhibit-startup-screen 1)
(setq initial-scratch-message ";; Emacs Scratch

")

;; (toggle-debug-on)
;; (toggle-debug-on-quit)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Show current filename in titlebar
(setq frame-title-format "%b")

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(add-to-list 'load-path "/home/zaeph/.emacs.d/lisp")
(add-to-list 'load-path (expand-file-name "/usr/share/emacs/site-lisp"))

(setq source-directory (concat (getenv "HOME") "/projects/forks/emacs"))

;; ;; outline-minor-mode for viewing init.el
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (make-local-variable 'outline-regexp)
;;             (setq outline-regexp "^;;; ")
;;             (make-local-variable 'outline-heading-end-regexp)
;;             (setq outline-heading-end-regexp ":\n")
;;             (outline-minor-mode 1)
;;             ))

;; -nt background off
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Force horizontal splitting
;; (setq split-width-threshold 9999)    ;Default: 160

;; Suppress warning when opening large files
(setq large-file-warning-threshold nil)

;; Configure ‘display-buffer’ behaviour for some special buffers
(setq display-buffer-alist
      `(;; Messages, errors, processes, Calendar in the bottom side window
        (,(rx bos (or "*Apropos"                ; Apropos buffers
                      "*Man"                    ; Man buffers
                      ;; "*Help"                        ; Help buffers
                      "*Warnings*"              ; Emacs warnings
                      "*Process List*"          ; Processes
                      "*Proced"                 ; Proced processes list
                      "*Compile-Log*"           ; Emacs byte compiler log
                      "*compilation"            ; Compilation buffers
                      "*Flycheck errors*"       ; Flycheck error list
                      "*Calendar"               ; Calendar window
                      "*env-info"               ; Environment information
                      "*Cargo"                  ; Cargo process buffers
                      "*Word"                   ; WordNut buffers
                      "*Reconcile*"             ; Reconcile in ledger-mode
                      (and (1+ nonl) " output*"))) ; AUCTeX command output
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.45))
        ;; REPLs on the bottom half
        (,(rx bos (or "*cider-repl"     ; CIDER REPL
                      "*intero"         ; Intero REPL
                      "*idris-repl"     ; Idris REPL
                      "*ielm"           ; IELM REPL
                      "*SQL"))          ; SQL REPL
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.50))
        ;; Open shell in a single window
        (,(rx bos "*shell")
         (display-buffer-same-window)
         (reusable-frames . nil))
        ;; Open PDFs in the right side window
        (,(rx bos "*pdf")
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (reusable-frames . visible)
         (window-width . 0.5))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))))

;; Enable disabled commands
(setq disabled-command-function nil)

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Temporary fix for helm delays
(when (= emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))

;; Auth sources
(setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))



;; ========================================
;; ============== PACKAGES ================
;; ========================================

;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Disable org’s ELPA packages
(setq package-load-list '(all
                          (org nil)
                          (org-plus-contrib nil)
                          ))

(package-initialize)

;; org-elpa
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Load extra files, and search subdirs
(let ((default-directory  "/home/zaeph/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "/home/zaeph/.emacs.d/lisp/")

;; Start server
(server-start)

;; Change indent-function to handle plists
(setq lisp-indent-function 'common-lisp-indent-function)

;; Evil
(require 'evil)
(evil-mode 0)
;; (setq evil-toggle-key "M-SPC")
;; (setq evil-default-state 'emacs)
;; (evil-set-initial-state 'help-mode 'emacs)
;; (evil-set-initial-state 'messages-buffer-mode 'emacs)
;; (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs)
;; (evil-set-initial-state 'calendar-mode 'emacs)
;; (evil-set-initial-state 'info-mode 'emacs)

;; EasyPG (for encryption)
(require 'epa-file)
(epa-file-enable)
(setq epg-gpg-program "gpg2")

;; Isearch
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

;; Ledger
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(require 'ledger-mode)
;; Using MELPA rather than the one from the built one
;; (add-to-list 'load-path
;;              (expand-file-name "/home/zaeph/builds/ledger/src/ledger-3.1.1/lisp/"))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(defvar ledger-use-iso-dates nil)
(defvar ledger-reconcile-default-commodity nil)
(defvar ledger-post-auto-adjust-amounts nil)
(setq ledger-use-iso-dates t
      ledger-reconcile-default-commodity "EUR"
      ;; Testing
      ledger-post-auto-adjust-amounts 1
      ledger-schedule-file "/home/zaeph/org/ledger/main-schedule.ledger.gpg")

(add-hook 'ledger-reconcile-mode-hook #'balance-windows)
;; (setq ledger-reconcile-mode-hook nil)

(defun zp/ledger-close-scheduled ()
  (interactive)
  (if (string-match-p (regexp-quote "*Ledger Schedule*") (buffer-name))
      (progn
        (kill-buffer)
        (other-window -1)
        (delete-other-windows))))
(define-key ledger-mode-map (kbd "S-<backspace>") 'zp/ledger-close-scheduled)

;; -----------------------------------------------------------------------------
;; Patch for inserting an empty line after copied transactions
(defvar ledger-copy-transaction-insert-blank-line-after nil
  "Non-nil means insert blank line after a transaction inserted
  with ‘ledger-copy-transaction-at-point’.")

(defun ledger-copy-transaction-at-point (date)
  "Ask for a new DATE and copy the transaction under point to
that date.  Leave point on the first amount."
  (interactive  (list
                 (ledger-read-date "Copy to date: ")))
  (let* ((extents (ledger-navigate-find-xact-extents (point)))
         (transaction (buffer-substring-no-properties (car extents) (cadr extents)))
         (encoded-date (ledger-parse-iso-date date)))
    (ledger-xact-find-slot encoded-date)
    (insert transaction
            (if ledger-copy-transaction-insert-blank-line-after
                "\n\n"
              "\n"))
    (beginning-of-line -1)
    (ledger-navigate-beginning-of-xact)
    (re-search-forward ledger-iso-date-regexp)
    (replace-match date)
    (ledger-next-amount)
    (if (re-search-forward "[-0-9]")
        (goto-char (match-beginning 0)))))

(setq ledger-copy-transaction-insert-blank-line-after t)


;; Patch for killing transaction
(defun ledger-kill-current-transaction (pos)
  "Delete the transaction surrounging POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (kill-region (car bounds) (cadr bounds))))

(define-key ledger-mode-map (kbd "C-c C-d") 'ledger-kill-current-transaction)
;; -----------------------------------------------------------------------------

;; zshrc
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\prompt_.*_setup\\'" . shell-script-mode))

;; fish
(add-to-list 'auto-mode-alist '("\\.*.fish\\'" . fish-mode))

;; fcitx
;; Disabled because of slow-downs in combination with visual-line-mode
;; (fcitx-aggressive-setup)

;; ox-hugo
(require 'ox-hugo)

;; duplicate-thing
(require 'duplicate-thing)

(global-set-key (kbd "M-U") 'universal-argument)
(define-key universal-argument-map "M-U" 'universal-argument-more)
(global-set-key (kbd "M-J") 'duplicate-thing)

(require 'volatile-highlights)
(volatile-highlights-mode)

;; Removed because of conflict with use-hard-newlines
;; (require 'clean-aindent-mode)
;; (add-hook 'prog-mode-hook #'clean-aindent-mode)

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(add-hook 'prog-mode-hook #'zp/whitespace-mode-lines-tail)

(defun zp/enable-visual-line-fringe-indicators ()
  "Enable visual line fringe indicators."
  (setq-local visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(add-hook 'prog-mode-hook #'zp/enable-visual-line-fringe-indicators)

(global-set-key (kbd "M-U") 'universal-argument)
(global-set-key (kbd "M-SPC") 'delete-horizontal-space)
(global-set-key (kbd "M-S-SPC") 'just-one-space)
(define-key universal-argument-map "\M-U" 'universal-argument-more)
(global-set-key (kbd "M-J") 'duplicate-thing)

(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-emacs-lisp-load-path 'inherit
      flycheck-display-errors-delay 0.5)

(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'cperl-mode-hook 'flycheck-mode)

;; (require 'flycheck-pos-tip)
;; (flycheck-pos-tip-mode)


;; (add-to-list 'load-path "/home/zaeph/.emacs.d/pkg/emacswiki.org/info+.el")
;; (load "/home/zaeph/.emacs.d/pkg/emacswiki.org/info+.el")
(require 'info+)
;; (define-key Info-mode-map (kbd "<mouse-4>") 'Info-mouse-scroll-up)
;; (define-key Info-mode-map (kbd "<mouse-5>") 'Info-mouse-scroll-down)
(define-key Info-mode-map (kbd "<mouse-4>") 'mwheel-scroll)
(define-key Info-mode-map (kbd "<mouse-5>") 'mwheel-scroll)
(define-key Info-mode-map (kbd "j") 'next-line)
(define-key Info-mode-map (kbd "k") 'previous-line)
;; (define-key Info-mode-map (kbd "h") 'Info-history-back)
;; (define-key Info-mode-map (kbd "l") 'Info-history-forward)

;; dired+
;; (setq diredp-hide-details-initially-flag nil)
;; (require 'dired+)

;; dired-x
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))

(require 'recentf-ext)

(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(diff-hl-flydiff-mode)

(require 'eyebrowse)
;; (eyebrowse-mode)

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 1)

(require 'lilypond-mode)

;; Nov
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.mobi\\'" . nov-mode))

(add-hook 'nov-mode-hook #'olivetti-mode)

;; Anki-Editor
;; (require 'anki-editor)

;; el-patch
(require 'el-patch)

;; org-mind-map
(require 'ox-org)
(require 'org-mind-map)

;; EXWM
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

;; line-number-mode
;; (setq display-line-numbers 'relative)

;; Fountain
(require 'fountain-mode)
(setq fountain-export-font "Courier Prime")

(setq fountain-mode-hook
      '(turn-on-visual-line-mode
        fountain-outline-hide-custom-level
        olivetti-mode))



;; ========================================
;; =============== FREE_KEYS ==============
;; ========================================

(require 'free-keys)

(setq free-keys-modifiers '("" "C" "M" "C-M" "H"))



;; ========================================
;; ================= LISPY ================
;; ========================================

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(require 'lispy)
(defun lispy-mode-unbind-keys ()
  "Modify keymaps used by ‘lispy-mode’."
  (define-key lispy-mode-map (kbd "M-o") nil))
(lispy-mode-unbind-keys)

(setq lispy-avy-keys
      '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
        ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
        ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
        ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))

(setq semantic-inhibit-functions
      (list (lambda () (not (eq major-mode org-mode)))))



;; ========================================
;; ================ ASPELL ================
;; ========================================

(require 'ispell)

(setq-default ispell-program-name "aspell")

;; Tell ispell.el that ’ can be part of a word.
(setq ispell-local-dictionary-alist
      `((nil "[[:alpha:]]" "[^[:alpha:]]" "['\x2019]" nil ("-B") nil utf-8)
        ("english" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
        ("british" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8)
        ("french" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "fr_FR") nil utf-8)))

;; Allow curvy quotes to be considered as regular apostrophe
(setq ispell-local-dictionary-alist
 (quote
  (("english" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
   ("british" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8)
   ("french" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "fr_FR") nil utf-8))))

;; Don't send ’ to the subprocess.
(defun endless/replace-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add #'ispell-send-string :filter-args
            #'endless/replace-apostrophe)

;; Convert ' back to ’ from the subprocess.
(defun endless/replace-quote (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add #'ispell-parse-output :filter-args
            #'endless/replace-quote)

;; Helm-Ispell
(defvar zp/ispell-completion-data nil)
(setq ispell-dictionary "british"
      zp/ispell-completion-data '(("English" . "british")
                                 ("French" . "french")))

(defun zp/ispell-switch-dictionary (language)
  "Change the Ispell dictionary to LANGUAGE.

LANGUAGE should be the name of an Ispell dictionary."
  (interactive)
  (let ((name (car (rassoc language zp/ispell-completion-data))))
    (if (eq language ispell-local-dictionary)
        (message "Dictionary is already loaded for this language")
      (setq ispell-local-dictionary language)
      (flyspell-mode)
      (message (concat "Local Ispell dictionary set to " name)))
    (when flyspell-mode
      (flyspell-mode -1)
      (flyspell-mode))))

(defun zp/ispell-query-dictionary ()
  (if (not (y-or-n-p "Writing in English? "))
      (ispell-change-dictionary "french")))

(defvar zp/helm-ispell-actions nil)
(setq zp/helm-ispell-actions
      '(("Change dictionary" . zp/ispell-switch-dictionary)))

(defvar zp/helm-source-ispell nil)
(setq zp/helm-source-ispell
      '((name . "*HELM Ispell - Dictionary selection*")
        (candidates . zp/ispell-completion-data)
        (action . zp/helm-ispell-actions)))

(defun zp/helm-ispell-preselect (&optional lang)
  (interactive)
  (let ((current ispell-local-dictionary))
    (helm :sources '(zp/helm-source-ispell)
          :preselect (if (or
                          (eq lang "French")
                          (eq current nil)
                          (string-match-p current "british"))
                         "French"
                       "English"))))



;; ========================================
;; ================= DEFUN ================
;; ========================================

(defun zp/get-string-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))



;; ========================================
;; ================= ERC ==================
;; ========================================

(require 'erc)
(setq erc-autojoin-timing 'connect
      erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#ranger")
        ("myanonamouse.net" "#anonamouse.net" "#am-members"))
      erc-join-buffer 'bury
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 22
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-lurker-hide-list '("JOIN" "PART" "QUIT")
      erc-lurker-threshold-time 43200
      erc-prompt-for-nickserv-password nil
      erc-server-reconnect-attempts 5
      erc-server-reconnect-timeout 3
      erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                 "324" "329" "332" "333" "353" "477"))

(add-to-list 'erc-modules 'notify)
(add-to-list 'erc-modules 'notifications)
(erc-update-modules)
(add-to-list 'erc-notify-list "toonn")
;; (setq erc-notify-list nil)

(defun zp/erc-connect ()
  (interactive)
  (erc :server "176.188.242.162" :port 15873 :nick "zaeph" :password (concat "zaeph/freenode:" (zp/get-string-from-file "/home/zaeph/org/pp/irc/freenode/pp.gpg")))
  ;; (erc :server "irc.freenode.net" :port 6667 :nick "zaeph" :password (zp/get-string-from-file "/home/zaeph/org/pp/irc/freenode/pp.gpg"))
  ;; (erc-tls :server "irc.myanonamouse.net" :port 6697 :nick "zaeph" :password (zp/get-string-from-file "/home/zaeph/org/pp/irc/mam/pp.gpg"))
  )

;; ========================================
;; ================ CIRCE =================
;; ========================================

(require 'circe)

(enable-circe-color-nicks)
(enable-lui-track-bar)

(setq circe-format-say "{nick:15s} {body}")
(setq circe-format-server-message "               *** {body}")
(setq circe-format-self-say "{nick:15s} {body}")
;; (setq circe-format-say "<{nick}> {body}")

(setq
 lui-time-stamp-position 'right-margin
 lui-fill-type nil)

(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "                "))
(add-hook 'lui-mode-hook 'my-lui-setup)

(add-hook 'lui-mode-hook 'olivetti-mode)

(defface circe-prompt-server-face nil
  "Face used for displaying the name of the server in circe.")
(set-face-attribute 'circe-prompt-server-face nil
                    :foreground "purple"
                    :weight 'normal)
(set-face-attribute 'circe-prompt-face nil
                    :foreground "black"
                    :background "LightSeaGreen")

(defun my-circe-prompt ()
  (lui-set-prompt
   (concat
    "\n"
    (format "%15s"
            (concat
             (propertize
              (concat (buffer-name) " ")
              'face 'circe-prompt-server-face)
             (concat
              (propertize
               ">"
               'face 'circe-prompt-face))))
    " ")))
(add-hook 'circe-chat-mode-hook 'my-circe-prompt)


(setq circe-reduce-lurker-spam t)
(setq circe-use-cycle-completion t)

;; (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))

;; From vifon
(define-key lui-mode-map (kbd "C-c C-o")
  (lambda ()
    (interactive)
    (ffap-next-url t)))

(setq lui-formatting-list '(("\\[[[:digit:]][[:digit:]]\\:[[:digit:]][[:digit:]]\\]" 0 font-lock-comment-face)))

(defun zp/circe-get-pp (server)
  (concat "freenode:" (zp/get-string-from-file "/home/zaeph/org/pp/irc/freenode/pp.gpg")))

(setq circe-network-options
      '(
        ;; ("ZNC SSL"
        ;;  :host "zaeph.tk"
        ;;  ;; :host "176.188.242.162"
        ;;  :port "15873"
        ;;  :tls t
        ;;  :server-buffer-name "{host} ⇄ ZNC"
        ;;  ;; :nick "zaeph"
        ;;  ;; :nickserv-password zp/circe-get-pp
        ;;  :sasl-username "zaeph"
        ;;  :sasl-password zp/circe-get-pp
        ;;  :user "zaeph/freenode"
        ;;  :pass zp/circe-get-pp
        ;;  :channels ("#ranger" "#emacs")
        ;;  )
        ("Weechat Relay"
         :host "zaeph.tk"
         ;; :host "176.188.242.162"
         :port "15873"
         :tls t
         :server-buffer-name "{host} ⇄ Weechat"
         :nick "zaeph"
         ;; :nickserv-password zp/circe-get-pp
         ;; :user "zaeph"
         :pass zp/circe-get-pp
         :channels ("#ranger")
         )
        ("Freenode Rescue"
         :nick "zaeph_"
         :host "chat.freenode.net"
         :port 6667
         :nickserv-password zp/circe-get-pp
         :channels ("#ranger")
         )))

(defun zp/switch-to-circe-ranger ()
  (interactive)
  (if (string-match "#ranger" (buffer-name))
      (mode-line-other-buffer)
    (switch-to-buffer "#ranger")))

;; (global-set-key (kbd "H-p") 'zp/switch-to-circe-ranger)

;; circe-notifications
(autoload 'enable-circe-notifications "circe-notifications" nil t)
(eval-after-load "circe-notifications"
  '(setq circe-notifications-watch-strings
      '("vifon")))

(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

;; ========================================
;; ================= MU4E =================
;; ========================================

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/zaeph/.emacs.d/pkg/mu/mu4e")
(require 'mu4e)
(require 'mu4e-alert)
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)

;; ;; -----------------------------------------------------------------------------
;; ;; To investigate
;; (defun remove-nth-element (nth list)
;;   (if (zerop nth) (cdr list)
;;     (let ((last (nthcdr (1- nth) list)))
;;       (setcdr last (cddr last))
;;       list)))
;; (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
;; (add-to-list 'mu4e-marks
;;      '(trash
;;        :char ("d" . "▼")
;;        :prompt "dtrash"
;;        :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
;;        :action (lambda (docid msg target)
;;                  (mu4e~proc-move docid
;;                               (mu4e~mark-check-target target) "+S-u-N"))))

;; (setq mu4e-alert-interesting-mail-query
;;       (concat
;;        "flag:unread maildir:/private/inbox"
;;        ;; "maildir:/private/inbox"
;;        " OR "
;;        "flag:unread maildir:/work/inbox"
;;        ;; "maildir:/work/inbox"
;;        ))

;; (setq mu4e-bookmarks
;;   `(,(make-mu4e-bookmark
;;       :name  "Inbox"
;;       :query "maildir:/private/inbox OR maildir:/work/inbox"
;;       :key ?i)
;;     ,(make-mu4e-bookmark
;;       :name  "Inbox (excluding read)"
;;       :query "flag:unread AND (maildir:/private/inbox OR maildir:/work/inbox)"
;;       :key ?I)
;;     ,(make-mu4e-bookmark
;;       :name  "Sent"
;;       :query "maildir:/private/sent OR maildir:/work/sent"
;;       :key ?s)
;;     ,(make-mu4e-bookmark
;;       :name  "Drafts"
;;       :query "maildir:/private/drafts OR maildir:/work/drafts"
;;       :key ?d)
;;     ,(make-mu4e-bookmark
;;       :name  "Archive (last week)"
;;       :query "date:1w.. AND (maildir:/private/archive OR maildir:/work/archive)"
;;       :key ?a)
;;     ,(make-mu4e-bookmark
;;       :name  "Unread messages"
;;       :query "flag:unread AND NOT (maildir:/private/trash OR maildir:/work/trash)"
;;       :key ?u)
;;     ,(make-mu4e-bookmark
;;       :name "Today's messages"
;;       :query "date:today..now"
;;       :key ?t)
;;     ,(make-mu4e-bookmark
;;       :name "Last 7 days"
;;       :query "date:7d..now"
;;       :key ?w)
;;     ,(make-mu4e-bookmark
;;       :name "Messages with images"
;;       :query "mime:image/*"
;;       :key ?p)))

;; (define-key mu4e-main-mode-map (kbd "c") 'mu4e-compose-new)
;; (define-key mu4e-headers-mode-map (kbd "c") 'mu4e-compose-new)


;; ;; show images
;; ;; (setq mu4e-view-show-images t
;; ;;       mu4e-view-image-max-width 800)

;; ;; use imagemagick, if available
;; ;; (when (fboundp 'imagemagick-register-types)
;; ;;   (imagemagick-register-types))

;; ;; IMAP takes care of deletion
;; (setq mu4e-sent-messages-behavior 'delete)

;; ;; ;; Add a column to display what email account the email belongs to.
;; ;; (add-to-list 'mu4e-header-info-custom
;; ;;        '(:account
;; ;;          :name "Account"
;; ;;          :shortname "Account"
;; ;;          :help "Which account this email belongs to"
;; ;;          :function
;; ;;          (lambda (msg)
;; ;;            (let ((maildir (mu4e-message-field msg :maildir)))
;; ;;              (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

;; ;; -----------------------------------------------------------------------------



;; (defun zp/mu4e-alert-grouped-mail-notification-formatter (mail-group all-mails)
;;   "Default function to format MAIL-GROUP for notification.

;; ALL-MAILS are the all the unread emails"
;;   (let* ((mail-count (length mail-group))
;;          (total-mails (length all-mails))
;;          (first-mail (car mail-group))
;;       ;; Other possible icon:✉
;;          ;; (title-prefix (format "<span foreground='#75d075'><span font_desc='Noto Color Emoji'>💬</span> [%d/%d] New mail%s</span>"
;;          (title-prefix (format "[%d/%d] <span foreground='#F04949'>New mail%s</span>"
;;                                mail-count
;;                                total-mails
;;                                (if (> mail-count 1) "s" "")))
;;          (field-value (mu4e-alert--get-group first-mail))
;;          (title-suffix (format (pcase mu4e-alert-group-by
;;                                  (`:from "from %s:")
;;                                  (`:to "to %s:")
;;                                  (`:maildir "in %s:")
;;                                  (`:priority "with %s priority:")
;;                                  (`:flags "with %s flags:"))
;;                                field-value))
;;          (title (format "%s %s" title-prefix title-suffix)))
;;     (list :title title
;;           :body (concat "• "
;;                         (s-join "\n• "
;;                                 (mapcar (lambda (mail)
;;                                           (replace-regexp-in-string "&" "&amp;" (plist-get mail :subject)))
;;                                         mail-group))))))

;; ;; Redefine default notify function to include icon
;; (defun mu4e-alert-notify-unread-messages (mails)
;;   "Display desktop notification for given MAILS."
;;   (let* ((mail-groups (funcall mu4e-alert-mail-grouper
;;                                mails))
;;          (sorted-mail-groups (sort mail-groups
;;                                    mu4e-alert-grouped-mail-sorter))
;;          (notifications (mapcar (lambda (group)
;;                                   (funcall mu4e-alert-grouped-mail-notification-formatter
;;                                            group
;;                                            mails))
;;                                 sorted-mail-groups)))
;;     (dolist (notification (cl-subseq notifications 0 (min 5 (length notifications))))
;;       (alert (plist-get notification :body)
;;              :title (plist-get notification :title)
;;              :category "mu4e-alert"
;;           :icon "gmail-2"))
;;     (when notifications
;;       (mu4e-alert-set-window-urgency-maybe))))

;; (setq mu4e-alert-set-window-urgency nil
;;       mu4e-alert-email-notification-types '(subjects)
;;       mu4e-alert-grouped-mail-notification-formatter 'zp/mu4e-alert-grouped-mail-notification-formatter
;;       mu4e-headers-show-threads t)



;; (defun zp/mu4e-alert-refresh ()
;;   (interactive)
;;   (mu4e-alert-enable-mode-line-display)
;;   (mu4e-alert-enable-notifications))

;; (defun zp/mu4e-update-index-and-refresh ()
;;   (interactive)
;;   (mu4e-update-index)
;;   (mu4e~request-contacts)
;;   ;; (mu4e~headers-maybe-auto-update)
;;   (zp/mu4e-alert-refresh))

;; (mu4e-alert-set-default-style 'libnotify)
;; (add-hook 'mu4e-main-mode-hook 'delete-other-windows)

;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
;; (add-hook 'mu4e-index-updated-hook 'mu4e~headers-maybe-auto-update)
;; (add-hook 'mu4e-index-updated-hook 'zp/mu4e-alert-refresh)

;; (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; (defun mu4e-headers-config ()
;;   "Modify keymaps used by `mu4e-mode'."
;;   (local-set-key (kbd "C-/") 'mu4e-headers-query-prev)
;;   (local-set-key (kbd "\\")  'mu4e-headers-query-prev)
;;   (local-set-key (kbd "C-?") 'mu4e-headers-query-next)
;;   (local-set-key (kbd "|")   'mu4e-headers-query-next)
;;   )
;; (setq mu4e-headers-mode-hook 'mu4e-headers-config)

;; (setq mu4e-maildir (expand-file-name "/home/zaeph/mail")
;;       mu4e-change-filenames-when-moving   t
;;       mu4e-get-mail-command "check-mail active"
;;       mu4e-hide-index-messages t
;;       mu4e-update-interval nil
;;       mu4e-headers-date-format "%Y-%m-%d %H:%M"
;;       )

;; (defun zp/rewrite-function (contact)
;;   (let ((name (or (plist-get contact :name) ""))
;;          (mail (plist-get contact :mail)))
;;     (cond
;;       ;; jonh smiht --> John Smith
;;       ;; ((string= "jonh smiht" name)
;;       ;;   (plist-put contact :name "John C. Smith")
;;       ;;   contact)
;;       ;; remove evilspammer from the contacts list
;;       ((string= "nic022@hotnail.fr" mail) nil)
;;       ;; others stay as the are
;;       (t contact))))

;; (setq mu4e-contact-rewrite-function 'zp/rewrite-function)

;; (setq mu4e-headers-fields
;;       '( (:date          .  25)    ;; alternatively, use :human-date
;;       (:flags         .   6)
;;       (:from          .  22)
;;       (:subject       .  nil)))

;; (setq mu4e-compose-format-flowed t)

;; ;; This allows me to use 'helm' to select mailboxes
;; (setq mu4e-completing-read-function 'completing-read)
;; ;; Why would I want to leave my message open after I've sent it?
;; (setq message-kill-buffer-on-exit t)
;; ;; Don't ask for a 'context' upon opening mu4e
;; (setq mu4e-context-policy 'pick-first)
;; ;; Don't ask to quit... why is this the default?
;; (setq mu4e-confirm-quit nil)

;; (add-to-list 'mu4e-view-actions
;;   '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; (setq mu4e-contexts
;;       `( ,(make-mu4e-context
;;         :name "Private"
;;         :match-func (lambda (msg) (when msg
;;                                     (string-prefix-p "/private" (mu4e-message-field msg :maildir))))
;;         :vars `(
;;                 (user-mail-address . ,(zp/get-string-from-file "/home/zaeph/org/pp/private/email"))
;;                 (user-full-name . "Zaeph")
;;                 (message-signature-file . "/home/zaeph/org/sig/private")

;;                 (mu4e-trash-folder  . "/private/trash")
;;                 (mu4e-refile-folder . "/private/archive")
;;                 (mu4e-sent-folder   . "/private/sent")
;;                 (mu4e-drafts-folder . "/private/drafts")

;;                 (mu4e-maildir-shortcuts . (("/private/inbox"   . ?i)
;;                                            ("/private/sent"    . ?s)
;;                                            ("/private/trash"   . ?t)
;;                                            ("/private/archive" . ?a)))
;;                 ))
;;       ,(make-mu4e-context
;;         :name "Work"
;;         :match-func (lambda (msg) (when msg
;;                                     (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
;;         :vars `(
;;                 (user-mail-address . ,(zp/get-string-from-file "/home/zaeph/org/pp/work/email"))
;;                 (user-full-name . "Leo Vivier")
;;                 (message-signature-file . "/home/zaeph/org/sig/work")

;;                 (mu4e-trash-folder  . "/work/trash")
;;                 (mu4e-refile-folder . "/work/archive")
;;                 (mu4e-sent-folder   . "/work/sent")
;;                 (mu4e-drafts-folder . "/work/drafts")

;;                 (mu4e-maildir-shortcuts . (("/work/inbox"   . ?i)
;;                                            ("/work/sent"    . ?s)
;;                                            ("/work/trash"   . ?t)
;;                                            ("/work/archive" . ?a)))
;;                 ))
;;       ))

;; ;; Editing modeline display to add a space after the `]'
;; (defun mu4e-context-label ()
;;   "Propertized string with the current context name, or \"\" if
;;   there is none."
;;   (if (mu4e-context-current)
;;     (concat "[" (propertize (mu4e~quote-for-modeline
;;                            (mu4e-context-name (mu4e-context-current)))
;;                'face 'mu4e-context-face) "] ") ""))

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-auth-credentials
;;       (expand-file-name "/home/zaeph/.authinfo.gpg")
;;       )

;; (setq send-mail-function 'sendmail-send-it)
;; (setq message-send-mail-function 'message-send-mail-with-sendmail)

;; (require 'footnote)

;; ;; Format footnotes for message-mode
;; ;; Default value had a space at the end causing it to be reflowed.
;; (setq footnote-section-tag "Footnotes:")

;; (require 'epg-config)
;; (setq mml2015-use 'epg
;;       epg-user-id (zp/get-string-from-file "/home/zaeph/org/pp/gpg/gpg-key-id")
;;       mml-secure-openpgp-sign-with-sender t
;;       mml-secure-openpgp-encrypt-to-self t)

;; (add-hook 'message-mode-hook #'flyspell-mode)
;; (add-hook 'message-mode-hook #'electric-quote-local-mode)
;; (add-hook 'message-mode-hook #'footnote-mode)
;; (add-hook 'message-mode-hook (lambda ()
;;                              (zp/helm-ispell-preselect "French")))

;; (setq electric-quote-context-sensitive 1)
;; ;; -----------------------------------------------------------------------------
;; ;; Experimental patch for electric-quote
;; ;; Not necessary since 26.1, since this function has been modified to accommodate ‘electric-quote-chars’
;; ;; (defun electric-quote-post-self-insert-function ()
;; ;;   "Function that `electric-quote-mode' adds to `post-self-insert-hook'.
;; ;; This requotes when a quoting key is typed."
;; ;;   (when (and electric-quote-mode
;; ;;              (memq last-command-event '(?\' ?\`)))
;; ;;     (let ((start
;; ;;            (if (and comment-start comment-use-syntax)
;; ;;                (when (or electric-quote-comment electric-quote-string)
;; ;;                  (let* ((syntax (syntax-ppss))
;; ;;                         (beg (nth 8 syntax)))
;; ;;                    (and beg
;; ;;                         (or (and electric-quote-comment (nth 4 syntax))
;; ;;                             (and electric-quote-string (nth 3 syntax)))
;; ;;                         ;; Do not requote a quote that starts or ends
;; ;;                         ;; a comment or string.
;; ;;                         (eq beg (nth 8 (save-excursion
;; ;;                                          (syntax-ppss (1- (point)))))))))
;; ;;              (and electric-quote-paragraph
;; ;;                   (derived-mode-p 'text-mode)
;; ;;                   (or (eq last-command-event ?\`)
;; ;;                       (save-excursion (backward-paragraph) (point)))))))
;; ;;       (when start
;; ;;         (save-excursion
;; ;;           (if (eq last-command-event ?\`)
;; ;;               (cond ((search-backward "“`" (- (point) 2) t)
;; ;;                      (replace-match "`")
;; ;;                      (when (and electric-pair-mode
;; ;;                                 (eq (cdr-safe
;; ;;                                      (assq ?‘ electric-pair-text-pairs))
;; ;;                                     (char-after)))
;; ;;                        (delete-char 1))
;; ;;                      (setq last-command-event ?“))
;; ;;               ((search-backward "‘`" (- (point) 2) t)
;; ;;                      (replace-match "“")
;; ;;                      (when (and electric-pair-mode
;; ;;                                 (eq (cdr-safe
;; ;;                                      (assq ?‘ electric-pair-text-pairs))
;; ;;                                     (char-after)))
;; ;;                        (delete-char 1))
;; ;;                      (setq last-command-event ?“))
;; ;;                     ((search-backward "`" (1- (point)) t)
;; ;;                      (replace-match "‘")
;; ;;                      (setq last-command-event ?‘)))
;; ;;             (cond ((search-backward "”'" (- (point) 2) t)
;; ;;                    (replace-match "'")
;; ;;                    (setq last-command-event ?”))
;; ;;             ((search-backward "’'" (- (point) 2) t)
;; ;;                    (replace-match "”")
;; ;;                    (setq last-command-event ?”))
;; ;;                   ((search-backward "'" (1- (point)) t)
;; ;;                    (replace-match "’")
;; ;;                    (setq last-command-event ?’)))))))))
;; ;; -----------------------------------------------------------------------------



;; ;; SMTP
;; ;; I have my "default" parameters from Gmail
;; ;; (setq mu4e-sent-folder "/home/zaeph/mail/sent"
;; ;;       ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
;; ;;       mu4e-drafts-folder "/home/zaeph/mail/drafts"
;; ;;       user-mail-address "[DATA EXPUNGED]"
;; ;;       smtpmail-default-smtp-server "smtp.gmail.com"
;; ;;       smtpmail-smtp-server "smtp.gmail.com"
;; ;;       smtpmail-smtp-service 587)

;; ;; Now I set a list of
;; ;; (defvar my-mu4e-account-alist
;; ;;   '(("personal"
;; ;;      (mu4e-sent-folder "/personal/sent")
;; ;;      (user-mail-address "[DATA EXPUNGED]")
;; ;;      (smtpmail-smtp-user "[DATA EXPUNGED]")
;; ;;      (smtpmail-local-domain "gmail.com")
;; ;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;; ;;      (smtpmail-smtp-server "smtp.gmail.com")
;; ;;      (smtpmail-smtp-service 587)
;; ;;      )
;; ;;     ("work"
;; ;;      (mu4e-sent-folder "/work/sent")
;; ;;      (user-mail-address "[DATA EXPUNGED]")
;; ;;      (smtpmail-smtp-user "[DATA EXPUNGED]")
;; ;;      (smtpmail-local-domain "gmail.com")
;; ;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;; ;;      (smtpmail-smtp-server "smtp.gmail.com")
;; ;;      (smtpmail-smtp-service 587)
;; ;;      )
;; ;;     ))

;; ;; (defun my-mu4e-set-account ()
;; ;;   "Set the account for composing a message."
;; ;;   (let* ((account
;; ;;           (if mu4e-compose-parent-message
;; ;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;; ;;                 (string-match "/\\(.*?\\)/" maildir)
;; ;;                 (match-string 1 maildir))
;; ;;             (completing-read (format "Compose with account: (%s) "
;; ;;                                      (mapconcat #'(lambda (var) (car var))
;; ;;                                                 my-mu4e-account-alist "/"))
;; ;;                              (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;; ;;                              nil t nil nil (caar my-mu4e-account-alist))))
;; ;;          (account-vars (cdr (assoc account my-mu4e-account-alist))))
;; ;;     (if account-vars
;; ;;         (mapc #'(lambda (var)
;; ;;                   (set (car var) (cadr var)))
;; ;;               account-vars)
;; ;;       (error "No email account found"))))
;; ;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)



;; ========================================
;; =============== NOTMUCH ================
;; ========================================

(require 'notmuch)
(require 'org-notmuch)

(setq message-signature
      (lambda ()
        (let* ((signature-override
                (concat (file-name-as-directory "~/org/sig")
                        (message-sendmail-envelope-from)))
               (signature-file
                (if (file-readable-p signature-override)
                    signature-override
                  "~/.signature")))
          (when (file-readable-p signature-file)
            (with-temp-buffer
              (insert-file-contents signature-file)
              (buffer-string))))))


(setq message-sendmail-envelope-from 'header)
(setq notmuch-always-prompt-for-sender t)
(setq mml-enable-flowed t)
(setq message-kill-buffer-on-exit t)

;; Enforce f=f in message-mode
;; Disabled because it’s bad practice according to the netiquette
;; (defun zp/message-mode-use-hard-newlines ()
;;   (use-hard-newlines t 'always))
;; (add-hook 'message-mode-hook #'zp/message-mode-use-hard-newlines)

;; Set the marks for inserted text with message-mark-inserted-region
(setq message-mark-insert-begin
      "--------------------------------[START]--------------------------------
"
      message-mark-insert-end
      "
---------------------------------[END]---------------------------------")



(defvar zp/email-private (zp/get-string-from-file "/home/zaeph/org/pp/private/email")
  "Email used for private communications.")

(defvar zp/email-work (zp/get-string-from-file "/home/zaeph/org/pp/work/email")
  "Email used for work-related communications.")

(defun zp/notmuch-get-email-with-alias (email alias &optional regex)
  "Create email alias from EMAIL and ALIAS.
If REGEX is non-nil, creates a regex to match the email alias."
  (let* ((email (cond
                  ((equal email "work")
                   zp/email-work)
                  ((equal email "private")
                   zp/email-private)
                  (t
                   email)))
         (email-alias (replace-regexp-in-string "@"
                                                  (concat "+" alias "@")
                                                  email)))
    (if regex
        (regexp-quote email-alias)
      email-alias)))

(defvar zp/email-org (zp/notmuch-get-email-with-alias "work" "org")
  "Email alias used for the org-mode mailing list.")

(defvar zp/email-dev (zp/notmuch-get-email-with-alias "work" "dev")
  "Email alias used for general dev work.")

(defun zp/notmuch-fcc-email-format-regex (email))

(setq notmuch-fcc-dirs
      `((,(regexp-quote zp/email-private) .
          "private/sent -inbox +sent -unread")
        (,(regexp-quote zp/email-work) .
          "work/sent -inbox +sent -unread")
        (,(regexp-quote zp/email-org) .
          "work/sent -inbox +sent -unread +org")
        (,(regexp-quote zp/email-dev) .
          "work/sent -inbox +sent -unread +dev")))

;; (setq notmuch-user-name
;;                   (lambda ()
;;                     (let* ((signature-override
;;                             (concat (file-name-as-directory "~/org/sig")
;;                                     (message-sendmail-envelope-from)))
;;                            (signature-file
;;                             (if (file-readable-p signature-override)
;;                                 signature-override
;;                               "~/.signature")))
;;                       (when (file-readable-p signature-file)
;;                         (with-temp-buffer
;;                           (insert-file-contents signature-file)
;;                        (buffer-string))))))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials
      (expand-file-name "/home/zaeph/.authinfo.gpg")
      )

(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

(setq notmuch-search-oldest-first nil)

(define-key notmuch-search-mode-map "d"
  (lambda (&optional untrash beg end)
    "mark thread as spam"
    (interactive (cons current-prefix-arg (notmuch-interactive-region)))
    (if untrash
        (notmuch-search-tag (list "-deleted"))
      (notmuch-search-tag (list "+deleted" "-inbox")) beg end)
    (notmuch-search-next-thread)))

(define-key notmuch-show-mode-map "d"
  (lambda (&optional beg end)
    "mark thread as spam"
    (interactive (notmuch-interactive-region))
    (notmuch-show-tag (list "+deleted" "-inbox" "-draft"))
    (notmuch-show-next-thread-show)))

;; -----------------------------------------------------------------------------
;; Patch submitted upstream
;; Waiting for approval
(defvar notmuch-search-refine-replace-buffer nil
  "Should ‘not-much-refine’ replace the current search?")

(defun notmuch-search-refine (query &optional replace)
  "Refine the current query string.

When REPLACE is non-nil, do not create another buffer.  See also
‘notmuch-search-refine-replace-buffer’."
  (interactive (list (minibuffer-with-setup-hook
                         (lambda ()
                           (next-history-element 1)
                           (end-of-line)
                           (insert " "))
                       (notmuch-read-query "Refine search: "))))
  (let ((grouped-query (notmuch-group-disjunctive-query-string
                        query)))
    (when (or replace
	      notmuch-search-refine-replace-buffer)
      (notmuch-bury-or-kill-this-buffer))
    (notmuch-search grouped-query notmuch-search-oldest-first)))
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Movements for message-mode

(defun zp/message-goto-bottom-1 ()
  (let ((newline message-signature-insert-empty-line))
    (goto-char (point-max))
    (when (re-search-backward message-signature-separator nil t)
      (end-of-line (if newline -1 0)))
    (point)))

(defun zp/message-goto-bottom ()
  "Go to the end of the message or buffer.
Go to the end of the message (before signature) or, if already there, go to the
end of the buffer."
  (interactive)
  (let ((old-position (point))
        (message-position (save-excursion (message-goto-body) (point)))
        (newline message-signature-insert-empty-line))
    (zp/message-goto-bottom-1)
    (when (equal (point) old-position)
      (goto-char (point-max)))))

(defun zp/message-goto-top-1 ()
  "Go to the beginning of the message."
  (interactive)
  (message-goto-body-1)
  (point))

(defun zp/message-goto-top ()
  "Go to the beginning of the message or buffer.
Go to the beginning of the message or, if already there, go to the
beginning of the buffer."
  (interactive)
  (let ((old-position (point)))
    (zp/message-goto-top-1)
    (when (equal (point) old-position)
      (goto-char (point-min)))))

(defun zp/message-goto-body-1 ()
  "Go to the beginning of the body of the message."
  (zp/message-goto-top-1)
  (forward-line 2)
  (point))

(defun zp/message-goto-body ()
  "Move point to the beginning of the message body."
  (interactive)
  (let ((old-position (point))
        (greeting (save-excursion
                    (zp/message-goto-top-1)
                    (re-search-forward "^[^>]+.*,$" (point-at-eol) t)))
        (modified))
    (zp/message-goto-top-1)
    (cond (greeting
           (forward-line 2))
          ((save-excursion
             (re-search-forward "writes:$" (point-at-eol) t))
           (insert "\n\n")
           (forward-char -2)
           (setq modified t))
          (t
           (insert "\n")
           (forward-char -1)
           (setq modified t)))
    ;; (cond ((re-search-forward "writes:$" (point-at-eol) t)
    ;;        (beginning-of-line)
    ;;        (insert "\n\n")
    ;;        (forward-char -2))
    ;;       ((re-search-forward "^[^>]+.*,$" (line-end-position) t)
    ;;        (zp/message-goto-body-1))
    ;;       (t
    ;;        (insert "\n")
    ;;        (forward-char -1)))
    (when (and (not modified)
               (equal (point) old-position))
      (zp/message-goto-top-1)
      (goto-char (1- (line-end-position))))))

(defun zp/message-goto-body-end-1 ()
  (zp/message-goto-bottom-1)
  (re-search-backward "[^[:space:]]")
  (end-of-line)
  (point))

(defun zp/message-goto-body-end ()
  (interactive)
  (let* ((old-position (point))
         (top-posting (save-excursion
                        (zp/message-goto-top-1)
                        (re-search-forward "writes:$" nil t)
                        (when (< old-position (line-beginning-position 0))
                          (line-beginning-position))))
         (sign-off (save-excursion
                     (or
                      (progn
                        (zp/message-goto-bottom-1)
                        (beginning-of-line)
                        (re-search-forward "^[^>]+.*,$" (line-end-position) t))
                      (and top-posting
                           (progn
                             (goto-char top-posting)
                             (beginning-of-line -1)
                             (re-search-forward "^[^>]+.*,$" (line-end-position) t))))))
         (modified))
    (if sign-off
        (progn
          (goto-char sign-off)
          (beginning-of-line 0)
          (re-search-backward "^[^>[:space:]]+" nil t)
          (end-of-line))
      (cond (top-posting
             (goto-char top-posting)
             (insert "\n\n")
             (forward-char -2)
             (setq modified t))
            (t
             (zp/message-kill-to-signature)
             (unless (bolp) (insert "\n"))
             (insert "\n")
             (setq modified t))))
    (when (and (not modified)
               (equal (point) old-position))
      (goto-char (1- sign-off)))))

(defun zp/message-kill-to-signature (&optional arg)
  "Kill all text up to the signature.
If a numeric argument or prefix arg is given, leave that number
of lines before the signature intact."
  (interactive "P")
  (let ((newline message-signature-insert-empty-line))
    (save-excursion
      (save-restriction
        (let ((point (point)))
	  (narrow-to-region point (point-max))
	  (message-goto-signature)
	  (unless (eobp)
	    (if (and arg (numberp arg))
	        (forward-line (- -1 arg))
	      (end-of-line (if newline -2 -1))))
	  (unless (= point (point))
	    (kill-region point (point))
	    (unless (bolp)
	      (insert "\n"))))))))

(defun zp/message-kill-to-signature (&optional arg)
  (interactive "P")
  (let ((newline message-signature-insert-empty-line)
        (at-end (save-excursion (= (point) (zp/message-goto-bottom-1)))))
    (when at-end
        (error "Already at end"))
    (message-kill-to-signature arg)
    (unless (bolp) (insert "\n"))
    (when newline
      (insert "\n")
      (forward-char -1))))
;; -----------------------------------------------------------------------------

(define-key notmuch-search-mode-map "y" #'notmuch-search-refine)
(define-key notmuch-hello-mode-map "q" #'zp/notmuch-hello-quit)
(define-key notmuch-search-mode-map "g" #'notmuch-refresh-this-buffer)

(setq user-full-name "Leo Vivier"
      mail-host-address "hidden")

(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox" :key "i")
        (:name "unread" :query "tag:unread" :key "u")
        (:name "flagged" :query "tag:flagged" :key "f")
        (:name "drafts" :query "tag:draft" :key "d")
        (:name "sent (last week)" :query "tag:sent date:\"7d..today\"" :key "s")
        (:name "archive (last week)" :query "* date:\"7d..today\"" :key "a")
        (:name "sent" :query "tag:sent" :key "S")
        (:name "archive" :query "*" :key "A")
        (:name "trash" :query "tag:deleted" :key "t")))

;; (require 'footnote)

;; Format footnotes for message-mode
;; Default value had a space at the end causing it to be reflowed when
;; using f=f.
;; (setq footnote-section-tag "Footnotes: ")

(require 'epg-config)
(setq mml2015-use 'epg
      epg-user-id (zp/get-string-from-file "/home/zaeph/org/pp/gpg/gpg-key-id")
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t)

(defvar zp/message-ispell-alist nil
  "Alist of emails and the language they typically use.
The language should be the name of a valid Ispell dictionary.")

(setq zp/message-ispell-alist
      `((,zp/email-private . "french")
        (,zp/email-work . "french")
        (,zp/email-org . "british")
        (,zp/email-dev . "british")))

(defun zp/message-flyspell-auto ()
  "Start Ispell with the language associated with the email.

Looks for the email in the ‘From:’ field and chooses a language
based on ‘zp/message-mode-ispell-alist’."
  (let* ((sender (message-sendmail-envelope-from))
         (language (cdr (assoc sender zp/message-ispell-alist))))
    (zp/ispell-switch-dictionary language)))

(setq electric-quote-context-sensitive 1)

(defun zp/message-sendmail-envelope-to ()
  "Return the envelope to."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^To: " nil t)
      (substring-no-properties
     (buffer-substring
      (point)
      (point-at-eol))))))

(defun zp/message-retrieve-to ()
  "Create a list of emails from ‘To:’."
  (let ((to-raw (zp/message-sendmail-envelope-to))
        (emails))
    (with-temp-buffer
      (insert to-raw)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((bound (save-excursion
                       (if (re-search-forward "," nil t)
                           (progn (forward-char -1)
                                  (point))
                         (point-max)))))
          (re-search-forward "@")
          (if (re-search-backward " " nil t)
              (forward-char)
            (goto-char (point-min)))
          (setq framed (looking-at-p "<"))
          (push (substring-no-properties
                 (buffer-substring (if framed
                                       (1+ (point))
                                     (point))
                                   (if framed
                                       (1- bound)
                                     bound)))
                emails)
          (goto-char (1+ bound))))
      (setq email-list emails))))

(defun zp/notmuch-confirm-before-sending (&optional arg)
  (interactive "P")
  (if (y-or-n-p "Ready to send? ")
      (notmuch-mua-send-and-exit arg)))

(defun zp/notmuch-message-mode-config ()
  "Modify keymaps used by ‘notmuch-show-mode’."
  (local-set-key (kbd "C-c C-c") #'zp/notmuch-confirm-before-sending)
  (local-set-key (kbd "C-c C-b") #'zp/message-goto-body)
  (local-set-key (kbd "C-c C-.") #'zp/message-goto-body-end)
  (local-set-key (kbd "M-<") #'zp/message-goto-top)
  (local-set-key (kbd "M->") #'zp/message-goto-bottom)
  (local-set-key (kbd "C-c C-z") #'zp/message-kill-to-signature))

(require 'orgalist)
(add-hook 'message-setup-hook #'flyspell-mode)
(add-hook 'message-setup-hook #'orgalist-mode)
(add-hook 'message-setup-hook #'zp/message-flyspell-auto)
(add-hook 'message-setup-hook #'electric-quote-local-mode)
(add-hook 'message-setup-hook #'zp/notmuch-message-mode-config)
;; (add-hook 'message-mode-hook #'footnote-mode)

(defun zp/notmuch-show-mode-config ()
  "Modify keymaps used by ‘notmuch-show-mode’."
  (local-set-key (kbd "C-c C-o") #'goto-address-at-point))

(add-hook 'notmuch-show-mode-hook #'zp/notmuch-show-mode-config)



;; ========================================
;; ============== MODE-LINE ===============
;; ========================================

;; Spaceline
(require 'spaceline-config)



;; ========================================
;; ================ MODES =================
;; ========================================

;; Modes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)
(fringe-mode 20)
(show-paren-mode 1)
(yas-global-mode 1)
(column-number-mode 1)
(blink-cursor-mode -1)
(winner-mode 1)
(set-default 'truncate-lines t)
(electric-quote-mode 1)
;; (dumb-jump-mode)
;; (global-visible-mark-mode 1)

(setq next-line-add-newlines nil)
(setq-default require-final-newline nil)
(setq scroll-preserve-screen-position 't)


;; (global-linum-mode 1)
;; (global-hl-line-mode 1)

;; Windmove
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)
;; (desktop-save-mode 1)

;; Focus follows mouse
;; (setq focus-follows-mouse t)
;; (setq mouse-autoselect-window t)
(setq focus-follows-mouse nil)
(setq mouse-autoselect-window nil)

;; Linum parameters
(require 'linum)
(setq linum-format " %d ")              ;Add spaces before and after

;; Mouse & Scrolling options
(setq mouse-wheel-flip-direction 1
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control)))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      auto-hscroll-mode 'current-line)

;; Disable side movements
;; (global-set-key (kbd "<mouse-6>") 'ignore)
;; (global-set-key (kbd "<mouse-7>") 'ignore)
;; (global-set-key (kbd "<triple-mouse-7>") 'ignore)
;; (global-set-key (kbd "<triple-mouse-6>") 'ignore)

;; Bell
(setq visible-bell 1)

;; Fullscreen
(toggle-frame-maximized)

;; Time
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; DocView
;; (add-hook 'doc-view-mode-hook 'auto-revert-mode)

(defun zp/toggle-pdf-view-auto-slice-minor-mode ()
  (interactive)
  (call-interactively 'pdf-view-auto-slice-minor-mode)
  (if (not pdf-view-auto-slice-minor-mode)
      (progn
        (pdf-view-reset-slice))))

;; pdf-tools
(pdf-tools-install)
(require 'pdf-view)
(define-key pdf-view-mode-map (kbd "m") 'pdf-view-midnight-minor-mode)
(define-key pdf-view-mode-map (kbd "s") 'zp/toggle-pdf-view-auto-slice-minor-mode)
(define-key pdf-view-mode-map (kbd "M") 'pdf-view-set-slice-using-mouse)
(define-key pdf-view-mode-map (kbd "c") 'zp/pdf-view-continuous-toggle)
(define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window)

(define-prefix-command 'slice-map)
(define-key pdf-view-mode-map (kbd "S") 'slice-map)
(define-key pdf-view-mode-map (kbd "S b") 'pdf-view-set-slice-from-bounding-box)
(define-key pdf-view-mode-map (kbd "S m") 'pdf-view-set-slice-using-mouse)
(define-key pdf-view-mode-map (kbd "S r") 'pdf-view-reset-slice)

(require 'pdf-links)
(define-key pdf-links-minor-mode-map (kbd "f") 'pdf-view-fit-page-to-window)

(add-hook #'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
(add-hook #'pdf-view-mode-hook #'pdf-view-auto-slice-minor-mode)

(setq pdf-view-continuous nil)

(defun zp/pdf-view-continuous-toggle ()
  (interactive)
  (cond ((not pdf-view-continuous)
         (setq pdf-view-continuous t)
         (message "Page scrolling: Continous"))
        (t
         (setq pdf-view-continuous nil)
         (message "Page scrolling: Constrained"))))

;; Sublimity
;; (sublimity-mode 0)
;; (require 'sublimity-scroll)


;; Suppress bells for reaching beginning and end of buffer
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)


;; Only hl-line from end of line

;; (defun my-hl-line-range-function ()
;;   (cons (line-end-position) (line-beginning-position 2)))
;; (setq hl-line-range-function #'my-hl-line-range-function)

;; (when window-system
;;   (require 'hl-line)
;;   (set-face-attribute 'hl-line nil :inherit nil :background "#111111")
;;   (setq global-hl-line-sticky-flag t)
;;   (global-hl-line-mode 1))

;; Way to enable minor modes based on filenames
;; Added with the package `auto-minor-mode-alist'
;; ...but can also add them with file-variables
(add-to-list 'auto-minor-mode-alist '("\\journal.*\\'" . visual-line-mode))
(add-to-list 'auto-minor-mode-alist '("\\journal.*\\'" . olivetti-mode))
(add-to-list 'auto-minor-mode-alist '("\\journal.*\\'" . flyspell-mode))

(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . visual-line-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . olivetti-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . flyspell-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . save-silently-mode))

;; (add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . visual-line-mode))
;; (add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . olivetti-mode))
;; (add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . flyspell-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . save-silently-mode))
;; (setq auto-minor-mode-alist nil)

(defun zp/kanji-add-furigana ()
  "Adds furigana to the kanji at point.
If text is selected, adds furigana to the selected kanji instead."
  (interactive)
  (if (not (region-active-p))
      (progn
        (call-interactively 'set-mark-command)
        (call-interactively 'forward-char)))
  (yas-expand-snippet (yas-lookup-snippet "anki-ruby")))

(defun zp/save-buffers-kill-terminal-silently ()
  (interactive)
  (save-buffers-kill-terminal t))

(define-minor-mode save-silently-mode
  "Save buffers silently when exiting."
  :lighter " SS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-c") 'zp/save-buffers-kill-terminal-silently)
            (define-key map (kbd "C-c C-k") 'zp/kanji-add-furigana)
            (define-key map (kbd "M-n") 'zp/kanji-add-furigana)
            map))

;; Recentf
(setq recentf-max-menu-items 100)

;; Anzu
;; Display search information in mode-line
(global-anzu-mode +1)
(setq anzu-cons-mode-line-p nil)

;; Tramp
(setq tramp-default-method "ssh")

;; realGUD
(require 'realgud)
(setq realgud-safe-mode nil)



;; ========================================
;; ============= INPUT METHODS ============
;; ========================================

(setq lang 'en)

;; Old input method

;; ;; Korean
;; (global-set-key (kbd "C-S-k")
;;              (lambda ()
;;                (interactive)
;;                (if (not (eq lang 'ko))
;;                    (progn
;;                      (set-input-method 'korean-hangul)
;;                      (setq lang 'ko))
;;                  (progn
;;                    (set-input-method 'nil)
;;                    (setq lang 'en)))))
;; ;; Latin
;; (global-set-key (kbd "C-S-l")
;;              (lambda ()
;;                (interactive)
;;                (if (not (eq lang 'fr))
;;                    (progn
;;                      (set-input-method 'latin-1-prefix)
;;                      (setq lang 'fr))
;;                  (progn
;;                    (set-input-method 'nil)
;;                    (setq lang 'en)))))
;; ;; IPA
;; (global-set-key (kbd "C-M-;")
;;              (lambda ()
;;                (interactive)
;;                (if (not (eq lang 'ipa))
;;                    (progn
;;                      (set-input-method 'ipa-x-sampa)
;;                      (setq lang 'ipa))
;;                  (progn
;;                    (set-input-method 'nil)
;;                    (setq lang 'en)))))



;; ========================================
;; ================ BACKUP ================
;; ========================================

;; By default, Emacs only create a backup only once per editing
;; session right before the first save. In other words, it preserves
;; the state of the file before Emacs touched it.

(setq
 ;; Don't clobber symlinks.
 backup-by-copying t

 ;; Use versioned backups
 version-control t

 ;; Also backup versioned files
 vc-make-backup-files t

 ;; Number of backups to keep
 kept-new-versions 10
 kept-old-versions 0
 delete-old-versions t

 backup-directory-alist `(("." . "/home/zaeph/.saves")))

;; Diff backend (default)
(setq diff-command "diff"
      diff-switches "-u")

(defun zp/set-diff-backend-git-diff ()
  "Set diff backend to ‘git diff’.
Modifies ‘diff-command’ and ‘diff-switches’ to use ‘git diff’."
  (setq-local diff-command "git --no-pager diff")
  (setq-local diff-switches "--textconv"))

(add-hook 'backup-walker-mode-hook #'zp/set-diff-backend-git-diff)



;; ========================================
;; ================ DIARY =================
;; ========================================

(setq diary-file "/home/zaeph/diary")



;; ========================================
;; =============== PYTHON =================
;; ========================================

(require 'python)

;; (defun zp/recenter-bottom (arg)
;;   "Recenter screen at the end of the buffer."
;;   (interactive "p")
;;   (let ((inhibit-message t))
;;     (goto-char (point-max))
;;     (end-of-buffer)
;;     (recenter-top-bottom arg)
;;     (recenter-top-bottom arg)
;;     (scroll-up-line)))

(defun zp/inferior-python-mode-config ()
  "Modify keymaps for ‘inferior-python-mode’."
  (local-set-key (kbd "C-l") #'comint-clear-buffer))

(setq inferior-python-mode-hook 'zp/inferior-python-mode-config)



;; ========================================
;; =============== AUCTeX =================
;; ========================================

(require 'latex)

;; Set default library
(setq-default TeX-engine 'luatex
              TeX-save-query nil
              TeX-parse-self t
              TeX-auto-save t
              LaTeX-includegraphics-read-file 'LaTeX-includegraphics-read-file-relative)
(setq reftex-default-bibliography '("/home/zaeph/org/bib/monty-python.bib"))
;; (setq reftex-default-bibliography nil)
(setq warning-suppress-types nil)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; Used to prevent radio tables from having trailing $
(setq LaTeX-verbatim-environments '("verbatim" "verbatim*" "comment"))

(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")
(setq TeX-open-quote "\\enquote{"
      TeX-close-quote "}")

(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)

(set-default 'preview-scale-function 3)

;; Enable LaTeX modes for Orgmode
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'orgtbl-mode)

(eval-after-load "org"
  '(require 'ox-beamer nil t)
  )
(require 'ox-latex)

(setq org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv")
      org-export-async-debug nil)

(setq reftex-plug-into-AUCTeX t)

(setq LaTeX-font-list '((1 #1="" #1# "\\mathcal{" "}")
                        (2 "\\textbf{" "}" "\\mathbf{" "}")
                        (3 "\\textsc{" "}")
                        (5 "\\emph{" "}")
                        (6 "\\textsf{" "}" "\\mathsf{" "}")
                        (9 "\\textit{" "}" "\\mathit{" "}")
                        (? "\\underline{" "}")
                        (13 "\\textmd{" "}")
                        (14 "\\textnormal{" "}" "\\mathnormal{" "}")
                        (18 "\\textrm{" "}" "\\mathrm{" "}")
                        (19 "\\textsl{" "}" "\\mathbb{" "}")
                        (20 "\\texttt{" "}" "\\mathtt{" "}")
                        (21 "\\textup{" "}")
                        (4 #1# #1# t)))

;; TeX view program
(defvar zp/tex-view-program nil)

;; -----------------------------------------------------------------------------
;; Patch submitted to mailing list

(defcustom TeX-view-pdf-tools-keep-focus nil
  "Whether AUCTeX retains the focus when viewing PDF files with pdf-tools.

When calling `TeX-pdf-tools-sync-view', the pdf-tools buffer
normally captures the focus. If this option is set to non-nil,
the AUCTeX buffer will retain the focus."
  :group 'TeX-view
  :type 'boolean)

(defun TeX-pdf-tools-sync-view ()
  "Focus the focused page/paragraph in `pdf-view-mode'.
If `TeX-source-correlate-mode' is disabled, only find and pop to
the output PDF file.  Used by default for the PDF Tools viewer
entry in `TeX-view-program-list-builtin'."
  ;; Make sure `pdf-tools' is at least in the `load-path', but the user must
  ;; take care of properly loading and installing the package.  We used to test
  ;; "(featurep 'pdf-tools)", but that doesn't play well with deferred loading.
  (unless (fboundp 'pdf-tools-install)
    (error "PDF Tools are not available"))
  (unless TeX-PDF-mode
    (error "PDF Tools only work with PDF output"))
  (add-hook 'pdf-sync-backward-redirect-functions
	    #'TeX-source-correlate-handle-TeX-region)
  (if (and TeX-source-correlate-mode
	   (fboundp 'pdf-sync-forward-search))
      (with-current-buffer (or (when TeX-current-process-region-p
			    	 (get-file-buffer (TeX-region-file t)))
			       (current-buffer))
	(pdf-sync-forward-search))
    (let* ((pdf (concat file "." (TeX-output-extension)))
           (buffer (or (find-buffer-visiting pdf)
		       (find-file-noselect pdf))))
      (if TeX-view-pdf-tools-keep-focus
          (display-buffer buffer)
        (pop-to-buffer buffer)))))
;; -----------------------------------------------------------------------------

(setq TeX-view-pdf-tools-keep-focus t)

(defun zp/tex-view-program-set-pdf-tools ()
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        zp/tex-view-program 'pdf-tools)
  (add-hook #'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(defun zp/tex-view-program-set-evince ()
  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Evince")
          (output-html "xdg-open"))
        TeX-source-correlate-start-server 'ask
        zp/tex-view-program 'evince)
  (remove-hook #'TeX-after-compilation-finished-functions
               #'TeX-revert-document-buffer))

(defun zp/tex-view-program-switch ()
  (interactive)
  (cond ((eq zp/tex-view-program 'pdf-tools)
         (zp/tex-view-program-set-evince)
         (message "TeX view program: Evince"))
        ((or (eq zp/tex-view-program 'evince)
             (not (bound-and-true-p zp/tex-view-program)))
         (zp/tex-view-program-set-pdf-tools)
         (message "TeX view program: pdf-tools"))))

(zp/tex-view-program-set-pdf-tools)

;; Update PDF buffers after successful LaTeX runs

;; Smart quotes
(setq org-export-default-language "en-gb"
      org-export-with-smart-quotes t)
(add-to-list 'org-export-smart-quotes-alist
             '("en-gb"
               (primary-opening   :utf-8 "‘" :html "&lsquo ;" :latex "\\enquote{"  :texinfo "`")
               (primary-closing   :utf-8 "’" :html "&rsquo;" :latex "}"           :texinfo "'")
               (secondary-opening :utf-8 "“" :html "&ldquo;" :latex "\\enquote*{" :texinfo "``")
               (secondary-closing :utf-8 "”" :html "&rdquo;" :latex "}"           :texinfo "''")
               (apostrophe        :utf-8 "’" :html "&rsquo;" :latex "'")))

(defun zp/LaTeX-remove-macro ()
  "Remove current macro and return `t'.  If no macro at point,
return `nil'."
  (interactive)
  (when (TeX-current-macro)
    (let ((bounds (TeX-find-macro-boundaries))
          (brace  (save-excursion
                    (goto-char (1- (TeX-find-macro-end)))
                    (TeX-find-opening-brace))))
      (delete-region (1- (cdr bounds)) (cdr bounds))
      (delete-region (car bounds) (1+ brace)))
    t))

;; Source: https://www.reddit.com/r/emacs/comments/5f99nv/help_with_auctex_how_to_delete_an_environment/dailbtu/
(defun zp/LaTeX-remove-environment ()
  "Remove current environment and return `t'.  If no environment at point,
return `nil'."
  (interactive)
  (when (LaTeX-current-environment)
    (save-excursion
      (let* ((begin-start (save-excursion
                            (LaTeX-find-matching-begin)
                            (point)))
             (begin-end (save-excursion
                          (goto-char begin-start)
                          (search-forward-regexp "begin{.*?}")))
             (end-end (save-excursion
                        (LaTeX-find-matching-end)
                        (point)))
             (end-start (save-excursion
                          (goto-char end-end)
                          (1- (search-backward-regexp "\\end")))))
        ;; delete end first since if we delete begin first it shifts the
        ;; location of end
        (delete-region end-start end-end)
        (delete-region begin-start begin-end)))
    t))

;; Movements

(defvar zp/LaTeX-narrow-previous-positions nil)

(defun zp/LaTeX-narrow-to-environment (&optional count)
  (interactive "p")
  (LaTeX-narrow-to-environment)
  (LaTeX-mark-environment 1)
  (TeX-pin-region (region-beginning) (region-end))
  (deactivate-mark)
  (move-end-of-line 1)
  (message "Narrowing to enviroment"))

(defun zp/LaTeX-widen ()
  (interactive)
  (widen)
  (message "Removing narrowing"))

(defun zp/LaTeX-narrow-forwards (&optional arg)
  (interactive "P")
  (widen)
  (when (search-forward-regexp "^\\\\begin{frame}" nil t)
    (LaTeX-narrow-to-environment)
    (unless arg
      (LaTeX-mark-environment 1)
      (TeX-pin-region (region-beginning) (region-end))
      (deactivate-mark)
      (move-end-of-line 1))
    (message "Narrowing to next frame")))

(defun zp/LaTeX-narrow-backwards (&optional arg)
  (interactive "P")
  (widen)
  (when (and (search-backward-regexp "^\\\\begin{frame}" nil t)
             (search-backward-regexp "^\\\\begin{frame}" nil t))
    (search-forward-regexp "^\\\\begin{frame}" nil t)
    (LaTeX-narrow-to-environment)
    (unless arg
      (LaTeX-mark-environment 1)
      (TeX-pin-region (region-beginning) (region-end))
      (deactivate-mark)
      (move-end-of-line 1))
    (message "Narrowing to previous frame")))

(defun zp/LaTeX-narrow-up ()
  (interactive)
  (widen)
  (LaTeX-mark-environment 2)
  (narrow-to-region (region-beginning) (region-end))
  (call-interactively #'narrow-to-region)
  (deactivate-mark)
  (move-end-of-line 1)
  (message "Narrowing to parent environment"))

(define-key LaTeX-mode-map (kbd "C-x n e") #'zp/LaTeX-narrow-to-environment)
(define-key LaTeX-mode-map (kbd "C-x n w") #'zp/LaTeX-widen)
(define-key LaTeX-mode-map (kbd "C-x n f") #'zp/LaTeX-narrow-forwards)
(define-key LaTeX-mode-map (kbd "C-x n b") #'zp/LaTeX-narrow-backwards)
(define-key LaTeX-mode-map (kbd "C-x n u") #'zp/LaTeX-narrow-up)

;; Hook
(defun zp/LaTeX-mode-config ()
  "Modify keymaps used by `latex-mode'."
  (local-set-key (kbd "C-x n e") #'zp/LaTeX-narrow-to-environment)
  (local-set-key (kbd "C-c DEL") 'zp/LaTeX-remove-macro)
  (local-set-key (kbd "C-c <C-backspace>") 'zp/LaTeX-remove-macro)
  (local-set-key (kbd "C-c <M-backspace>") 'zp/LaTeX-remove-environment)
  (local-set-key (kbd "C-c C-t C-v") 'zp/tex-view-program-switch))
(setq LaTeX-mode-hook '(zp/LaTeX-mode-config))



;; ========================================
;; ============== ORG-LATEX ===============
;; ========================================

;; Loaded on file-basis now
;; (add-to-list 'org-latex-packages-alist '("frenchb,british" "babel" t))
(setq org-latex-default-class "koma-article")
(setq org-latex-compiler "xelatex")

;; (setq org-latex-default-packages-alist '(("" "graphicx" t)
;;                                          ("" "grffile" t)
;;                                          ("" "longtable" nil)
;;                                          ("" "wrapfig" nil)
;;                                          ("" "rotating" nil)
;;                                          ("normalem" "ulem" t)
;;                                          ("" "amsmath" t)
;;                                          ("" "textcomp" t)
;;                                          ("" "amssymb" t)
;;                                          ("" "capt-of" nil)
;;                                          ("" "setspace" nil)
;;                                          ("" "titletoc" nil)
;;                                          ("" "hyperref" nil)
;;                                          ))

(setq org-latex-default-packages-alist nil)


;; KOMA
(setq org-latex-classes
      '(("koma-article-default"
         "\\documentclass{scrartcl}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("koma-article"
         "\\documentclass[
,a4paper
,DIV=12
,12pt
,abstract
,bibliography=totoc
]{scrartcl}

\\usepackage[
,babel=english
,header=false
,geometry
,autolang=hyphen
,numbers=osf
]{zpart}

"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; BibTeX
(setq bibtex-autokey-year-length '4)

;; XeTeX
(defvar zp/org-latex-pdf-process-mode)
(defun zp/toggle-org-latex-pdf-process ()
  "Toggle the number of steps in the XeTeX PDF process."
  (interactive)
  (if (or (not (bound-and-true-p zp/org-latex-pdf-process-mode))
          (string= zp/org-latex-pdf-process-mode "full"))
      (progn (setq org-latex-pdf-process '("xelatex -shell-escape\
                                                  -interaction nonstopmode\
                                                  -output-directory %o %f")
                   org-export-async-init-file "/home/zaeph/.emacs.d/async/main-short.el"
                   zp/org-latex-pdf-process-mode 'short)
             (message "XeLaTeX process mode: Short"))
    (progn (setq org-latex-pdf-process '("xelatex -shell-escape\
                                                    -interaction nonstopmode\
                                                    -output-directory %o %f"
                                           "biber %b"
                                           "xelatex -shell-escape\
                                                    -interaction nonstopmode\
                                                    -output-directory %o %f"
                                           "xelatex -shell-escape\
                                                    -interaction nonstopmode\
                                                    -output-directory %o %f")
                 org-export-async-init-file "/home/zaeph/.emacs.d/async/main-full.el"
                 zp/org-latex-pdf-process-mode 'full)
           (message "XeLaTeX process mode: Full"))))
(zp/toggle-org-latex-pdf-process)

;; latexmk
;; (setq org-latex-pdf-process '("latexmk -xelatex %f"))
;; (setq org-latex-to-pdf-process (list "latexmk -f -pdf %f"))

;; pdfTeX
;; Need to rework the packages I use before I can use pdfTeX
;; Will need tinkering.
;;
;; microtype:
;; #+LATEX_HEADER: \usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=1100,stretch=10,shrink=10]{microtype}
;;
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "biber %b"
;;      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; LuaTeX
;; Not as good as XeTeX, since I can't figure out how to make CJK characters work.
;; To investigate.
;;
;; # LuaTeXJA
;; #+LATEX_HEADER: \usepackage{luatexja-fontspec}
;; #+LATEX_HEADER: \setmainjfont{A-OTF Ryumin Pr5}
;;
;; (setq org-latex-pdf-process
;;       '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "biber %b"
;;      "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Suppress creation of labels when converting org→tex
(defun remove-orgmode-latex-labels ()
  "Remove labels generated by org-mode"
  (interactive)
  (let ((case-fold-search nil))
   (goto-char 1)
   (replace-regexp "\\\\label{sec:org[0-9][^}]*}" "")))

(defun zp/org-latex-remove-section-labels (string backend info)
  "Remove section labels generated by org-mode"
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "\\\\label{sec:.*?}" "" string)))

(add-to-list 'org-export-filter-final-output-functions
             #'zp/org-latex-remove-section-labels)

;; Minted
(setq org-latex-listings 'minted)
(setq org-src-preserve-indentation t)



;; ========================================
;; ================ VISUAL ================
;; ========================================

(define-fringe-bitmap 'left-curly-arrow
    (vector #b0011111110000000
            #b0011111110000000
            #b0011111110000000
            #b0011100000000000
            #b0011100000000000
            #b0011100000000000
            #b0011100000000000
            #b0011100001000000
            #b0011100001100000
            #b0011100001110000
            #b0011111111111000
            #b0011111111111100
            #b0011111111111000
            #b0000000001110000
            #b0000000001100000
            #b0000000001000000
            )
  16 16)

(define-fringe-bitmap 'right-curly-arrow
    (vector #b0000000111111100
            #b0000000111111100
            #b0000000111111100
            #b0000000000011100
            #b0000000000011100
            #b0000000000011100
            #b0000000000011100
            #b0000001000011100
            #b0000011000011100
            #b0000111000011100
            #b0001111111111100
            #b0011111111111100
            #b0001111111111100
            #b0000111000000000
            #b0000011000000000
            #b0000001000000000
            )
  16 16)

(define-fringe-bitmap 'left-arrow
    (vector #b0000000001000000
            #b0000000011000000
            #b0000000111000000
            #b0000001111000000
            #b0000011110000000
            #b0000111100000000
            #b0001111111111100
            #b0011111111111100
            #b0011111111111100
            #b0001111111111100
            #b0000111100000000
            #b0000011110000000
            #b0000001111000000
            #b0000000111000000
            #b0000000011000000
            #b0000000001000000
            )
  16 16)

(define-fringe-bitmap 'right-arrow
    (vector #b0000001000000000
            #b0000001100000000
            #b0000001110000000
            #b0000001111000000
            #b0000000111100000
            #b0000000011110000
            #b0011111111111000
            #b0011111111111100
            #b0011111111111100
            #b0011111111111000
            #b0000000011110000
            #b0000000111100000
            #b0000001111000000
            #b0000001110000000
            #b0000001100000000
            #b0000001000000000
            )
  16 16)

(define-fringe-bitmap 'right-triangle
    (vector #b0000000000000000
            #b0000000000000000
            #b0011000000000000
            #b0011110000000000
            #b0011111100000000
            #b0011111111000000
            #b0011111111110000
            #b0011111111111100
            #b0011111111111100
            #b0011111111110000
            #b0011111111000000
            #b0011111100000000
            #b0011110000000000
            #b0011000000000000
            #b0000000000000000
            #b0000000000000000
            )
  16 16)



;; ========================================
;; ================= PERL =================
;; ========================================

;; Prefer cperl-mode to perl-mode
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)

(defun zp/perl-eval-region ()
  "Run selected region as Perl code"
  (interactive)
  (shell-command-on-region (mark) (point) "perl"))

(defun zp/perl-eval-buffer ()
  "Run current buffer as Perl code"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "perl"))

(define-key cperl-mode-map (kbd "M-RET") 'zp/perl-eval-buffer)
(define-key cperl-mode-map (kbd "<C-return>") 'zp/perl-eval-region)



;; ========================================
;; =============== ORG-MODE ===============
;; ========================================

;; Load org-habit
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
;; (setq org-habit-show-habits-only-for-today nil)

;; Load org-collector
;; (require 'org-collector)
;; (add-to-list 'org-modules 'org-collector)



;; Experimental way to handle narrowing from org-agenda for my journal
;; Couldn't figure out how to limit it to a particular custom-agenda
;; (advice-add 'org-agenda-goto :after
;;             (lambda (&rest args)
;;               (org-narrow-to-subtree)))
;; (advice-add 'org-agenda-switch-to :after
;;             (lambda (&rest args)
;;               (org-narrow-to-subtree)))

;; Possibe solution to async export
;; (setq org-export-async-init-file "/home/zaeph/.emacs.d/async-init.el")

;;; Hook for org-clock-in
;;; Deprecated because I don't really need it
;; (defun zp/org-todo-started ()
;;   "Mark entry as started.
;; Used as a hook to `org-clock-in-hook'."
;;   (org-todo "STRT"))

;; (add-hook 'org-clock-in-hook 'zp/org-todo-started)

;; Options
(setq org-agenda-inhibit-startup nil
      org-clock-into-drawer "LOGBOOK-CLOCK"
      ;; org-clock-into-drawer nil
      org-log-into-drawer "LOGBOOK-NOTES"
      ;; org-log-into-drawer nil
      org-log-state-notes-insert-after-drawers nil
      org-special-ctrl-a/e 't
      ;; org-log-into-drawer nil
      ;; org-log-state-notes-insert-after-drawers t
      org-log-done 'time
      org-enforce-todo-dependencies t ;Careful with this one, easy to fuck up
      ;; org-enforce-todo-dependencies nil
      org-adapt-indentation nil
      org-clock-sound t
      org-export-in-background t
      org-export-with-sub-superscripts nil
      org-image-actual-width nil ;Ensures that images displayed within emacs can be resized with #+ATTR_ORG:
      org-hide-emphasis-markers t               ;Fontification
      org-ellipsis "…"
      org-track-ordered-property-with-tag "ORDERED"
      ;; org-tags-exclude-from-inheritance '("project")
      org-tags-exclude-from-inheritance nil
      org-agenda-hide-tags-regexp "recurring\\|waiting\\|standby"
      org-catch-invisible-edits 'error
      org-footnote-define-inline 1)

;; Prevent auto insertion of blank-lines before heading but not for lists
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

;; Prevent blank-lines from being displayed between headings in folded state
(setq org-cycle-separator-lines 0)

;; Add curly quotes to list of pre- and post-matches for emphasis markers
(setq org-emphasis-regexp-components '("-       ('‘\"“’{" "-    .,:!?;'’\"”)}\\[" "     
" "." 1))

;; Enable resetting checks plain-list when marking a repeated tasks DONE
;; Set the RESET_CHECK_BOXES property to t for the heading
(require 'org-checklist)

;; (eval-after-load 'org '(require 'org-pdfview))

(add-to-list 'org-file-apps
             '("\\.pdf\\'" . (lambda (file link)
                               (org-pdfview-open link))))

;; Enforce French spacing when filling paragraphs
(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)

;; (setq org-file-apps '(("\\.pdf\\'" lambda
;;                     (file link)
;;                     (org-pdfview-open link))
;;                    (auto-mode . emacs)
;;                    ("\\.mm\\'" . default)
;;                    ("\\.x?html?\\'" . default)
;;                    ("\\.pdf\\'" . default)))

;; -----------------------------------------------------------------------------
;; Template for new emphasis / fontify
;; Do not modify org-emphasis-alist, it's only for modifying the default ones
;; Also, has a little bug where it colours the rest of the line white
;; (defun org-add-my-extra-fonts ()
;;   "Add alert and overdue fonts."
;;   (add-to-list 'org-font-lock-extra-keywords '("\\(!\\)\\([^\n\r\t]+?\\)\\(!\\)" (1 '(face org-habit-overdue-face invisible t)) (2 'org-todo) (3 '(face org-habit-overdue-face invisible t)))))
;;   (add-to-list 'org-font-lock-extra-keywords '("\\(%\\)\\([^\n\r\t]+?\\)\\(%\\)" (1 '(face org-habit-overdue-face invisible t)) (2 'org-done) (3 '(face org-habit-overdue-face invisible t)))))

;; (add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)
;; -----------------------------------------------------------------------------

(setq org-todo-keywords '(
        ;; Default
        (sequence "TODO(t)" "NEXT(n)" "STRT(S!)" "|" "DONE(d)")
        ;; Action
        ;; (sequence "SCHD(m!)" "PREP(!)" "READ(r!)" "DBRF(D!)" "REVW(R!)" "|" "DONE(d!)")
        ;; Extra
        ;; (sequence "STBY(s)" "WAIT(w@/!)" "|" "CXLD(x@/!)")
        (sequence "STBY(s)" "|" "CXLD(x@/!)")
        (sequence "WAIT(w!)" "|" "CXLD(x@/!)")
        ))
(setq org-todo-keyword-faces '(
                               ("TODO" :inherit org-todo-todo)
                               ("NEXT" :inherit org-todo-next)
                               ("STRT" :inherit org-todo-strt)
                               ("DONE" :inherit org-todo-done)

                               ("STBY" :inherit org-todo-stby)
                               ("WAIT" :inherit org-todo-wait)
                               ("CXLD" :inherit org-todo-cxld)))

                               ;; ("SCHD" :inherit org-todo-box :background "DeepPink1")
                               ;; ("PREP" :inherit org-todo-box :background "DeepPink1")
                               ;; ("READ" :inherit org-todo-box :background "SkyBlue4")
                               ;; ("DBRF" :inherit org-todo-box :background "gold3")
                               ;; ("REVW" :inherit org-todo-box :background "gold3")

(setq org-highest-priority 65
      org-lowest-priority 69
      org-default-priority 68)

;; (setq org-priority-faces '((65 . (:foreground "red1"))
;;                         (66 . (:foreground "DarkOrange1"))
;;                         (67 . (:foreground "gold1"))
;;                         (68 . (:foreground "gray40"))
;;                         (69 . (:foreground "gray20"))))

;; DEFCON theme
(setq org-priority-faces '((?A . (:inherit org-priority-face-a))
                           (?B . (:inherit org-priority-face-b))
                           (?C . (:inherit org-priority-face-c))
                           (?D . (:inherit org-priority-face-d))
                           (?E . (:inherit org-priority-face-e))))
(setq org-tags-column -77)              ;Default: -77
;; Default: auto OR -80.
;; 89 and not 90 because of org-agenda-category-icon-alist
(setq org-agenda-tags-column -94)
(setq org-habit-graph-column 50)

(setq org-todo-state-tags-triggers
      '(("CXLD" ("cancelled" . t) ("standby") ("waiting"))
        ("STBY" ("standby"   . t) ("cancelled") ("waiting"))
        ("WAIT" ("waiting"   . t) ("cancelled") ("standby"))
        ("TODO" ("cancelled") ("standby") ("waiting"))
        ("NEXT" ("cancelled") ("standby") ("waiting"))
        ("STRT" ("cancelled") ("standby") ("waiting"))
        ("WAIT" ("cancelled") ("standby") ("waiting"))
        ("DONE" ("cancelled") ("standby") ("waiting"))
        (""     ("cancelled") ("standby") ("waiting")))
      ;; Custom colours for specific tags
      ;; Used to cause problem when rescheduling items in the agenda, but it seems to be gone now.
      org-tag-faces
      '(("@home"        . org-tag-location)
        ("@work"        . org-tag-location)
        ("@town"        . org-tag-location)
        ("standby"      . org-tag-todo)
        ("cxld"         . org-tag-todo)
        ("waiting"      . org-tag-todo)
        ("recurring"    . org-tag-todo)
        ("assignment"   . org-tag-important)
        ("exam"         . org-tag-important)
        ("important"    . org-tag-important)
        ("reading"      . org-tag-reading)
        ("french"       . org-tag-french)))



;; (setq org-columns-default-format "%TODO(State) %Effort(Effort){:} %CLOCKSUM %70ITEM(Task)")
(setq org-columns-default-format "%55ITEM(Task) %TODO(State) %Effort(Effort){:} %CLOCKSUM")

;; Global values for properties
(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 5:00 5:30 6:00 0:00")
                                    ("STYLE_ALL" . "habit")
                                    ("APPT_WARNTIME_ALL" . "5 10 15 20 25 30 35 40 45 50 55 60 0")
                                    ("SESSION_DURATION_ALL" . "0:45 0:15 0:20 0:30 1:00")
                                    )))

;; (setq org-global-properties nil)


(setq org-archive-location "%s.archive::")
;; (setq org-archive-location "%s.archive.org.gpg::")
;; (setq org-archive-location "archive.org.gpg::")
;; (setq org-archive-location "/home/zaeph/org/archive.org.gpg::* From %s")

;; Keep hierarchy when archiving
;; Source: https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (location (org-archive--compute-location
			(or (org-entry-get nil "ARCHIVE" 'inherit)
			    org-archive-location)))
         (afile (car location))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

;; Category icons
(defvar zp/org-agenda-include-category-icons nil
  "When non-nil, show category icons in the agenda")

(defvar zp/org-agenda-category-icon-alist nil
  "Alist of category icon to be displayed in agenda views.

Custom variable to hold the content when the icons are toggled
off.")

(setq zp/org-agenda-include-category-icons t)

(setq zp/org-agenda-category-icon-alist
      '(
        ;; Life
        ("^inbox$" "~/org/svg/icons/gmail.svg" nil nil :ascent center)
        ("^curios$" "~/org/svg/icons/question.svg" nil nil :ascent center)
        ("^style$" "~/org/svg/icons/suit.svg" nil nil :ascent center)
        ("^nicolas$" "~/org/svg/icons/leaf.svg" nil nil :ascent center)
        ("^swim$" "~/org/svg/icons/wave.svg" nil nil :ascent center)
        ("^run$" "~/org/svg/icons/running.svg" nil nil :ascent center)
        ("^awakening$" "~/org/svg/icons/aperture-green.svg" nil nil :ascent center)
        ("^journal$" "~/org/svg/icons/spellbook-p.svg" nil nil :ascent center)
        ("^psy$" "~/org/svg/icons/solution.svg" nil nil :ascent center)
        ("^anki$" "~/org/svg/icons/anki-2-p.svg" nil nil :ascent center)
        ("^plan$" "~/org/svg/icons/planning-p.svg" nil nil :ascent center)
        ("^typography$" "~/org/svg/icons/typography.svg" nil nil :ascent center)

        ;; Activism
        ("^pol$" "~/org/svg/icons/fist.svg" nil nil :ascent center)

        ;; Professional
        ("^research$" "~/org/svg/icons/research.svg" nil nil :ascent center)
        ("^university$" "~/org/svg/icons/aperture-yellow.svg" nil nil :ascent center)

        ;; Hacking
        ("^hack$" "~/org/svg/icons/engineering-2.svg" nil nil :ascent center)
        ("^emacs$" "~/org/svg/icons/spacemacs.svg" nil nil :ascent center)
        ("^org$" "~/org/svg/icons/org-mode-unicorn.svg" nil nil :ascent center)
        ("^python$" "~/org/svg/icons/python.svg" nil nil :ascent center)
        ("^perl$" "~/org/svg/icons/perl.svg" nil nil :ascent center)
        ("^contrib$" "~/org/svg/icons/chill.svg" nil nil :ascent center)
        ("^bug$" "~/org/svg/icons/cross.svg" nil nil :ascent center)
        ("^elisp$" "~/org/svg/icons/spacemacs-elisp.svg" nil nil :ascent center)
        ("^tex$" "~/org/svg/icons/file-2-p.svg" nil nil :ascent center)
        ("^linux$" "~/org/svg/icons/nixos.svg" nil nil :ascent center)
        ("^opsec$" "~/org/svg/icons/cyber-security-b.svg" nil nil :ascent center)
        ("^git$" "~/org/svg/icons/git.svg" nil nil :ascent center)

        ;; Media
        ("^media$" "~/org/svg/icons/library.svg" nil nil :ascent center)
        ("^news$" "~/org/svg/icons/world.svg" nil nil :ascent center)
        ("^books$" "~/org/svg/icons/book-2.svg" nil nil :ascent center)
        ("^trackers$" "~/org/svg/icons/share.svg" nil nil :ascent center)
        ("^music$" "~/org/svg/icons/compact-disc.svg" nil nil :ascent center)
        ("^film$" "~/org/svg/icons/film.svg" nil nil :ascent center)

        ;; Maintenance
        ("^mx$" "~/org/svg/icons/recycle.svg" nil nil :ascent center)
        ("^fin$" "~/org/svg/icons/money-p.svg" nil nil :ascent center)
        ("^cooking$" "~/org/svg/icons/salad.svg" nil nil :ascent center)
        ("^plants$" "~/org/svg/icons/sansevieria.svg" nil nil :ascent center)
        ("^animals$" "~/org/svg/icons/animals.svg" nil nil :ascent center)
        ("^health$" "~/org/svg/icons/health.svg" nil nil :ascent center)
        ("^supplies$" "~/org/svg/icons/box.svg" nil nil :ascent center)
        ("^social$" "~/org/svg/icons/happy.svg" nil nil :ascent center)
        ("^grooming$" "~/org/svg/icons/razor.svg" nil nil :ascent center)
        ("^cleaning$" "~/org/svg/icons/bucket.svg" nil nil :ascent center)

        (".*" '(space . (:width (24))) nil nil :ascent center)))

;; Shows icons by default
(setq org-agenda-category-icon-alist zp/org-agenda-category-icon-alist)

;; Babel
(require 'ob-async)
(add-hook 'ob-async-pre-execute-src-block-hook
          '(lambda ()
            (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")))

;; LaTeX export
(setq org-format-latex-options
      '(:foreground auto
        :background default
        :scale 3.0
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.0
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; Deactivated since migrated to Linux
(org-babel-do-load-languages 'org-babel-load-languages
                             '((R . t)
                               (python . t)
                               (latex . t)
                               (ledger . t)))

;; Prototype babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)
                               (ditaa . t)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")


(add-to-list 'safe-local-variable-values
             '(after-save-hook . org-html-export-to-html))
(add-to-list 'safe-local-variable-values
             '(org-confirm-babel-evaluate . nil))
(add-to-list 'safe-local-variable-values
             '( . nil))

(defun zp/org-overview (arg)
  (interactive "P")
  (let ((pos-before (point))
        (indirect (not (buffer-file-name))))
    (setq-local zp/org-narrow-previous-position pos-before)
    ;; Do not widen buffer if in indirect buffer
    (save-excursion
      (goto-char (point-min))
      (widen)
      (if indirect
          (org-narrow-to-subtree)
        (org-display-inline-images)))
    (zp/org-fold arg)))

(defun zp/org-fold (arg)
  (interactive "P")
  (let ((indirectp (not (buffer-file-name)))
        (org-startup-folded 'overview))
    ;; Fold drawers
    (org-set-startup-visibility)
    ;; Fold trees
    (org-overview)
    (when (not (equal arg '(4)))
      (beginning-of-buffer))
    (recenter-top-bottom)
    (save-excursion
      (goto-char (point-min))
      (org-cycle))))

(defun zp/org-show-all (arg)
  (interactive "p")
  (let ((pos-before (point))
        (indirect (not (buffer-file-name))))
    (setq-local zp/org-narrow-previous-position pos-before)
    ;; Do not widen buffer if in indirect buffer
    (unless indirect
      (widen)
      (org-display-inline-images))
    ;; Unfold everything
    (org-show-all)
    (when (not (eq arg 4))
      (beginning-of-buffer))
    (recenter-top-bottom)))

;; org-narrow movements

(defun zp/org-narrow-to-subtree ()
  "Move to the next subtree at same level, and narrow the buffer to it."
  (interactive)
  (org-narrow-to-subtree)
  (zp/org-fold nil)
  (message "Narrowing to tree at point."))

(defun zp/org-widen ()
  "Move to the next subtree at same level, and narrow the buffer to it."
  (interactive)
  (let ((pos-before (point)))
    (setq-local zp/org-narrow-previous-position pos-before))
  (widen)
  (message "Removing narrowing."))

(defun zp/org-narrow-forwards ()
  "Move to the next subtree at same level, and narrow the buffer to it."
  (interactive)
  (widen)
  (org-forward-heading-same-level 1)
  (org-narrow-to-subtree)
  (zp/org-fold nil)
  (message "Narrowing to next tree."))

(defun zp/org-narrow-backwards ()
  "Move to the next subtree at same level, and narrow the buffer to it."
  (interactive)
  (widen)
  (org-backward-heading-same-level 1)
  (org-narrow-to-subtree)
  (zp/org-fold nil)
  (message "Narrowing to previous tree."))

(defun zp/org-narrow-up-heading (arg)
  "Move to the upper subtree, and narrow the buffer to it."
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "No narrowing"))
  (let ((pos-before (point)))
    (setq-local zp/org-narrow-previous-position pos-before)
    (widen)
    (org-reveal)
    (outline-up-heading 1)
    (org-narrow-to-subtree)
    (if (equal arg '(4))
        (progn
          (goto-char pos-before)
          (recenter-top-bottom)))
    (zp/org-fold arg)
    (message "Narrowing to tree above.")))

(defun zp/org-narrow-up-heading-dwim (arg)
  "Narrow to the upper subtree, and narrow the buffer to it.

If the buffer is already narrowed to level-1 heading, overview
the entire buffer."
  (interactive "P")
  (if (save-excursion
        ;; Narrowed to a level-1 heading?
        (goto-char (point-min))
        (and (buffer-narrowed-p)
             (equal (org-outline-level) 1)))
      (zp/org-overview arg)
    (zp/org-narrow-up-heading arg)))

(defun zp/org-narrow-previous-heading (arg)
  "Move to the previously narrowed tree, and narrow the buffer to it."
  (interactive "p")
  (if (bound-and-true-p zp/org-narrow-previous-position)
      (let ((pos-before zp/org-narrow-previous-position))
        (goto-char zp/org-narrow-previous-position)
        (org-reveal)
        (org-cycle)
        (org-narrow-to-subtree)
        (setq zp/org-narrow-previous-position nil)
        (message "Narrowing to previously narrowed tree."))
    (message "Couldn’t find a previous position.")))

;; Toggle fontifications
(defun zp/org-toggle-emphasis-markers (&optional arg)
  "Toggle emphasis markers."
  (interactive "p")
  (let ((markers org-hide-emphasis-markers))
    (if markers
        (setq-local org-hide-emphasis-markers nil)
      (setq-local org-hide-emphasis-markers t))
    (when arg
      (font-lock-fontify-buffer))))

(defun zp/org-toggle-link-display (&optional arg)
  "Toggle the literal or descriptive display of links in the current buffer."
  (interactive "p")
  (if org-link-descriptive (remove-from-invisibility-spec '(org-link))
    (add-to-invisibility-spec '(org-link)))
  (setq-local org-link-descriptive (not org-link-descriptive))
  (when arg
    (font-lock-fontify-buffer)))

(defun zp/org-toggle-fontifications (&optional arg)
  "Toggle emphasis markers or the link display.

Without a C-u argument, toggle the emphasis markers.

With a C-u argument, toggle the link display."
  (interactive "P")
  (let ((markers org-hide-emphasis-markers)
        (links org-link-descriptive))
    (if arg
        (zp/org-toggle-link-display)
      (zp/org-toggle-emphasis-markers))
    (font-lock-fontify-buffer)))

;; Hook
(defun org-mode-config ()
  "Modify keymaps used by `org-mode'."
  (local-set-key (kbd "C-c i") 'org-indent-mode)
  ;; (local-set-key (kbd "C-c C-,") 'org-priority)
  (local-set-key (kbd "C-c [") 'nil)
  (local-set-key (kbd "C-c ]") 'nil)
  (local-set-key (kbd "C-c C-.") 'org-time-stamp)
  (local-set-key (kbd "C-c C-x r") 'zp/org-set-appt-warntime)
  (local-set-key (kbd "C-c C-x l") 'zp/org-set-location)
  (local-set-key (kbd "C-c C-x d") 'org-delete-property)
  (local-set-key (kbd "C-c C-x D") 'org-insert-drawer)
  (local-set-key (kbd "S-<backspace>") 'zp/org-kill-indirect-buffer)
  (local-set-key (kbd "C-x n o") 'zp/org-overview)
  (local-set-key (kbd "C-x n a") 'zp/org-show-all)
  (local-set-key (kbd "C-x n u") 'zp/org-narrow-up-heading-dwim)
  (local-set-key (kbd "C-x n y") 'zp/org-narrow-previous-heading)
  (local-set-key (kbd "C-x n s") 'zp/org-narrow-to-subtree)
  (local-set-key (kbd "C-x n f") 'zp/org-narrow-forwards)
  (local-set-key (kbd "C-x n b") 'zp/org-narrow-backwards)
  (local-set-key (kbd "C-x n w") 'zp/org-widen)
  (local-set-key (kbd "C-c ,") 'zp/hydra-org-priority/body)
  (local-set-key (kbd "M-p") 'org-metaup)
  (local-set-key (kbd "M-n") 'org-metadown)
  (local-set-key (kbd "M-[") 'org-metaleft)
  (local-set-key (kbd "M-]") 'org-metaright)
  (local-set-key (kbd "M-{") 'org-shiftmetaleft)
  (local-set-key (kbd "M-}") 'org-shiftmetaright)
  (local-set-key (kbd "C-a") 'org-beginning-of-line)
  (local-set-key (kbd "C-e") 'org-end-of-line)
  (local-set-key (kbd "M-I") 'org-indent-mode)
  (local-set-key (kbd "M-*") 'zp/org-toggle-fontifications)
  (local-set-key (kbd "C-c C-j") 'zp/org-jump-dwim)
  ;; (local-set-key (kbd "C-c C-w") 'org-refile)
  ;; (local-set-key (kbd "C-c C-S-w") 'zp/org-refile-with-paths)
  )
(setq org-mode-hook 'org-mode-config)
(define-key mode-specific-map (kbd "a") 'org-agenda)

(defun zp/org-agenda-redo-all ()
  (interactive)
  (let ((inhibit-message t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo))))))

;; (run-with-idle-timer 300 t #'zp/org-agenda-redo-all)



;; ========================================
;; =============== SHORTCUTS ==============
;; ========================================

(define-prefix-command 'ledger-map)
(global-set-key (kbd "C-c l") 'ledger-map)

(define-prefix-command 'projects-map)
(global-set-key (kbd "C-c p") 'projects-map)

(define-prefix-command 'projects-hacking-map)
(global-set-key (kbd "C-c p h") 'projects-hacking-map)

(define-prefix-command 'classes-map)
(global-set-key (kbd "C-c p c") 'classes-map)

;; (define-prefix-command 'activism-map)
;; (global-set-key (kbd "C-c p a") 'activism-map)

(defun zp/set-shortcuts (alist)
  (mapc
   (lambda (x)
     (let ((file-shortcut (car x))
           (file-path (cdr x)))
       (global-set-key (kbd (concat "C-c " file-shortcut))
                       `(lambda ()
                          (interactive)
                          (find-file ',file-path)))))
   alist))


(setq zp/shortcuts-alist
      '(
        ;; Misc
        ("e" . "/home/zaeph/.emacs.d/init.el")
        ("I" . "/home/zaeph/org/info.org.gpg")
        ("p d" . "/ssh:asus:/home/zaeph/Downloads/Sharing/dl.org")

        ;; Ledger
        ("l l" . "/home/zaeph/org/ledger/main.ledger.gpg")
        ("l s" . "/home/zaeph/org/ledger/main-schedule.ledger.gpg")
        ;; ("l f" . "/home/zaeph/org/ledger/french-house.ledger.gpg")

        ;; Research
        ("p T" . "/home/zaeph/org/projects/university/research/thesis/thesis.tex")
        ;; ("p T" . "/home/zaeph/org/projects/university/research/presentation/presentation.tex")
        ("p b" . "/home/zaeph/org/bib/monty-python.bib")
        ("p B" . "/home/zaeph/org/projects/university/research/thesis/bibliography/bibliography.tex")
        ;; ("p c" . "/home/zaeph/org/projects/university/research/sty/zaeph.sty")
        ;; ("p C" . "/home/zaeph/org/projects/university/research/sty/presentation.sty")
        ;; ("p d" . "/tmp/asus/home/zaeph/Downloads/Sharing/dl.org")

        ;; Journal
        ("j" . "/home/zaeph/org/journal.org.gpg")

        ;; Projects
        ("p w" . "/home/zaeph/org/projects/writing/writing.org.gpg")
        ;; ("p t" . "/home/zaeph/org/projects/tavocat/tavocat.org.gpg")
        ;; ("p k". "/home/zaeph/org/projects/kendeskiñ/kendeskiñ.org.gpg")
        ("p t" . "/home/zaeph/org/projects/typography/typography.org.gpg")

        ;; University
        ("p u" . "/home/zaeph/org/projects/university/university.org.gpg")
        ("p r" . "/home/zaeph/org/projects/university/research/research.org.gpg")
        ;; ("p c l"     . "/home/zaeph/org/projects/university/classes/university/ling/ling.org.gpg")
        ;; ("p c u"     . "/home/zaeph/org/projects/university/classes/university/civ-us/civ-us.org.gpg")
        ;; ("p c g"     . "/home/zaeph/org/projects/university/classes/university/civ-gb/civ-gb.org.gpg")
        ;; ("p c s"     . "/home/zaeph/org/projects/university/classes/university/space/space.org.gpg")
        ;; ("p c i"     . "/home/zaeph/org/projects/university/classes/university/lit/lit.org.gpg")
        ;; ("p c s"     . "/home/zaeph/org/projects/university/classes/university/syn/syn.org.gpg")
        ;; ("p c t"     . "/home/zaeph/org/projects/university/classes/espe/tronc-commun.org.gpg")

        ;; Languages
        ("p j" . "/home/zaeph/org/projects/lang/ja/ja.org.gpg")
        ("p g" . "/home/zaeph/org/projects/lang/de/de.org.gpg")

        ;; Activism
        ("p a" . "/home/zaeph/org/projects/activism/politics/politics.org.gpg")
        ;; ("p a d"  . "[DATA EXPUNGED]")
        ;; ("p a s"  . "[DATA EXPUNGED]")
        ;; ("p a c"  . "[DATA EXPUNGED]")
        ;; ("p a m"  . "[DATA EXPUNGED]")

        ;; Media
        ("p n" . "/home/zaeph/org/projects/media/news/news.org.gpg")

        ;; Music
        ("p P" "/home/zaeph/org/piano.org.gpg")

        ;; Awakening
        ("p A" . "/home/zaeph/org/projects/awakening/awakening.org.gpg")

        ;; Psychotherapy
        ("p p" . "/home/zaeph/org/projects/psychotherapy/psychotherapy.org.gpg")
        ;; Sports
        ("p S" . "/home/zaeph/org/sports/swimming/swimming.org.gpg")
        ("p R" . "/home/zaeph/org/sports/running/running.org.gpg")

        ;; Hacking
        ("p h e" . "/home/zaeph/org/projects/hacking/emacs/emacs.org.gpg")
        ("p h l" . "/home/zaeph/org/projects/hacking/linux/linux.org.gpg")
        ("p h o" . "/home/zaeph/org/projects/hacking/opsec/opsec.org.gpg")
        ("p h h" . "/home/zaeph/org/projects/hacking/hacking.org.gpg")
        ("p h p" . "/home/zaeph/org/projects/hacking/python/python.org.gpg")

        ;; Media
        ("b" . "/home/zaeph/org/media.org.gpg")

        ;; Life
        ("o" . "/home/zaeph/org/life.org.gpg")))

(defun zp/set-shortcuts-all ()
  (zp/set-shortcuts zp/shortcuts-alist))

;; ========================================
;; ================= HELM =================
;; ========================================

(require 'helm)

;; (helm-mode 1)

;; Disable helm-mode for given functions
;; Used to be necessary, but now it works just fine
;; (add-to-list 'helm-completing-read-handlers-alist '(org-set-property))

(define-prefix-command 'zp/helm-map)
(global-set-key (kbd "C-c h") 'zp/helm-map)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "<menu>") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-s M-s") 'helm-occur)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h f") 'helm-find-files)
(global-set-key (kbd "C-c h r") 'helm-regexp)
(global-set-key (kbd "C-c h x") 'helm-register)
(global-set-key (kbd "C-c h b") 'helm-resume)
(global-set-key (kbd "C-c h c") 'helm-colors)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-c h i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-h C-SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h a") 'helm-apropos)
(global-set-key (kbd "C-c h /") 'helm-find)
(global-set-key (kbd "C-c h <tab>") 'helm-lisp-completion-at-point)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)

(define-key helm-map (kbd "C-S-o") 'helm-previous-source)


;; Helm Projectile

(global-set-key (kbd "C-c h p") 'helm-projectile)



;; ========================================
;; ================= IVY ==================
;; ========================================

(defun zp/counsel-grep-or-swiper (&optional arg)
  "Call ‘swiper’ for small buffers and ‘counsel-grep’ for large ones.
Wrapper to always use swiper for gpg-encrypted files and
indirect-buffers."
  (interactive "P")
  (let* ((file (buffer-file-name))
         (ext (if file (file-name-extension file))))
    (if (or (equal arg '(4))                      ;Forcing?
            (not file)                            ;Indirect buffer?
            (string= ext "gpg"))                  ;Encrypted buffer?
        (swiper)
      (counsel-grep-or-swiper))))

;; Use rg insted of grep
(setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) "
      ivy-height 10                     ;Default
      counsel-find-file-at-point t)
;; (global-set-key "\C-s" 'swiper)
(global-set-key "\C-s" 'zp/counsel-grep-or-swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "<menu>") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)




;; Shortcut for opening Keep (not yet implemented: can't find a good key for it)
;; (global-set-key (kbd "C-<f1>")
;;              (lambda () (interactive) (org-open-link-from-string "https://keep.google.com/")))



;; ========================================
;; =============== CALENDAR ===============
;; ========================================

(setq calendar-week-start-day 1
      calendar-latitude 48.1119800
      calendar-longitude -1.6742900
      calendar-location-name "Rennes, France")



;; ========================================
;; =============== OLIVETTI ===============
;; ========================================

(require 'olivetti)

(setq-default olivetti-body-width 0.6
              olivetti-minimum-body-width 80)

;; (defun zp/olivetti-toggle-hide-mode-line ()
;;   (interactive)
;;   (olivetti-toggle-hide-mode-line)
;;   (toggle-frame-fullscreen)
;;   ;; (mode-line-other-buffer)
;;   ;; (mode-line-other-buffer)
;;   )

;; (define-key olivetti-mode-map (kbd "M-I") 'zp/olivetti-toggle-hide-mode-line)

;; (add-hook 'olivetti-mode-hook #'electric-quote-local-mode)
;; (setq olivetti-mode-hook nil)



;; ========================================
;; ============= ORG-PRIORITY =============
;; ========================================

(defvar zp/hydra-org-priority-chain nil
  "When non-nil, make zp/hydra-org-priority chain the commands.")

(defun zp/hydra-org-priority-chain-toggle ()
  "Toggle zp/hydra-org-priority-chain."
  (interactive)
  (if zp/hydra-org-priority-chain
      (setq zp/hydra-org-priority-chain nil)
    (setq zp/hydra-org-priority-chain t)))

(defun zp/hydra-org-priority-set (&optional action show)
  "Change the priority of an item, and possibly chain the commands."
  (interactive)
  (let ((in-agenda (eq major-mode 'org-agenda-mode)))
    (if in-agenda
        (org-agenda-priority action)
      (org-priority action show))
    (when (and zp/hydra-org-priority-chain
               (zp/hydra-org-priority-goto-sibling))
      (zp/hydra-org-priority/body))))

(defun zp/hydra-org-priority-raise (&optional lower)
  "Raise the priority of an item, and possibly chain the commands.
When LOWER is non-nil, raise the priority instead."
  (interactive)
  (let ((in-agenda (derived-mode-p 'org-agenda-mode))
        (fun-agenda (if lower
                        'org-agenda-priority-down
                      'org-agenda-priority-up))
        (fun-org (if lower
                     'org-priority-down
                   'org-priority-up)))
    (cond (in-agenda
           (funcall fun-agenda))
          (t
           (funcall fun-org)))
    (when zp/hydra-org-priority-chain
      (zp/hydra-org-priority/body))))

(defun zp/hydra-org-priority-goto-sibling (&optional previous)
  (interactive)
  (let ((in-agenda (eq major-mode 'org-agenda-mode))
        (fun-agenda (if previous
                        'org-agenda-previous-item
                      'org-agenda-next-item)))
    (cond (in-agenda
           (funcall fun-agenda 1))
          (t
           (org-goto-sibling (if previous t))))))

(defun zp/hydra-org-priority-todo ()
  (interactive)
  (let ((in-agenda (derived-mode-p 'org-agenda-mode)))
    (cond (in-agenda
           (org-agenda-todo current-prefix-arg))
          (t
           (org-todo)))
    (zp/hydra-org-priority/body)))

(defhydra zp/hydra-org-priority (:color blue
                                 :hint nil)
  "
_a_: #A    _p_: previous  _w_: refile
_b_: #B    _n_: next      _t_: todo
_c_: #C    _l_: lower
_d_: #D    _r_: raise
_e_: #E    _SPC_: remove

"
  ("x" zp/hydra-org-priority-chain-toggle (concat (if zp/hydra-org-priority-chain
                                                      "[x]"
                                                    "[ ]")
                                                  " chain") :exit nil)
  ("a" (zp/hydra-org-priority-set ?a))
  ("b" (zp/hydra-org-priority-set ?b))
  ("c" (zp/hydra-org-priority-set ?c))
  ("d" (zp/hydra-org-priority-set ?d))
  ("e" (zp/hydra-org-priority-set ?e))

  ("p" (zp/hydra-org-priority-goto-sibling t) :exit nil)
  ("n" (zp/hydra-org-priority-goto-sibling) :exit nil)

  ("l" (zp/hydra-org-priority-raise t) :exit nil)
  ("r" (zp/hydra-org-priority-raise) :exit nil)

  ("w" (zp/hydra-org-refile/body))

  ("t" (zp/hydra-org-priority-todo))

  ("SPC" (zp/hydra-org-priority-set 'remove))

  ("q" nil "cancel"))



;; ========================================
;; ============== ORG-AGENDA ==============
;; ========================================

(setq org-agenda-todo-ignore-with-date nil
      ;; org-agenda-todo-ignore-scheduled 'future
      org-agenda-todo-ignore-scheduled nil
      ;; org-agenda-show-future-repeats 'next
      org-agenda-show-future-repeats t
      ;; org-agenda-todo-ignore-scheduled 'past
      org-agenda-todo-ignore-deadlines nil
      org-agenda-include-deadlines 'all
      zp/projects-include-waiting t
      zp/org-agenda-include-scheduled 'all
      org-agenda-cmp-user-defined 'zp/org-agenda-sort-wait
      zp/org-agenda-sorting-strategy-user-defined 'priority
      org-agenda-window-setup 'current-window
      org-deadline-warning-days 30
      org-agenda-tags-todo-honor-ignore-options 1
      org-agenda-compact-blocks nil
      org-agenda-todo-list-sublevels t
      org-agenda-dim-blocked-tasks nil
      org-agenda-skip-scheduled-if-done 1
      org-agenda-skip-timestamp-if-done 1
      org-agenda-skip-deadline-if-done 1
      org-agenda-entry-text-maxlines 10
      org-agenda-use-time-grid nil
      org-agenda-sticky 1
      org-agenda-block-separator 126
      org-clock-report-include-clocking-task t
      org-clock-out-remove-zero-time-clocks t
      org-agenda-exporter-settings
      '((ps-print-color-p t)
        (ps-landscape-mode t)
        (ps-print-header nil)
        (ps-default-bg t))
      org-agenda-clockreport-parameter-plist '(:link t :narrow 50 :maxlevel 2 :fileskip0 t)
      )

(defun zp/org-agenda-benchmark (&optional arg)
  "Rebuild the agenda and display the time it took to do so.

With a prefix argument, do so in all agenda buffers."
  (interactive "P")
  (cond ((equal arg '(4))
         (with-timer "Rebuilding agenda buffer"
           (zp/org-agenda-redo-all)))
        (t
         (with-timer "Rebuilding agenda buffer"
           (org-agenda-redo)))))



(setq org-agenda-clock-consistency-checks '(:max-duration "10:00"
                                            :min-duration 0
                                            :max-gap "0:05"
                                            :gap-ok-around ("4:00" "12:30" "19:30")
                                            :default-face zp/org-agenda-block-info-face
                                            :gap-face nil
                                            :no-end-time-face nil
                                            :long-face nil
                                            :short-face nil))



(defun zp/update-org-agenda-files ()
  (interactive)
  (setq org-agenda-files '("/home/zaeph/org/life.org.gpg"))
  (zp/set-shortcuts-all))

(zp/update-org-agenda-files)

(run-at-time "06:00" 86400 #'zp/org-agenda-redo-all)

;; Force habits to be shown if they’ve been disabled the previous day
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

;; ========================================
;; =============== SORTING ================
;; ========================================

(defun org-cmp-test-todo (todo a b)
  "Compare the todo states of strings A and B."
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (fa (and ma (marker-buffer ma)))
         (fb (and mb (marker-buffer mb)))
         (todo-kwds
          (or (and fa (with-current-buffer fa org-todo-keywords-1))
              (and fb (with-current-buffer fb org-todo-keywords-1))))
         (ta (or (get-text-property 1 'todo-state a) ""))
         (tb (or (get-text-property 1 'todo-state b) ""))
         (la (- (length (member ta todo-kwds))))
         (lb (- (length (member tb todo-kwds))))
         (donepa (member ta org-done-keywords-for-agenda))
         (donepb (member tb org-done-keywords-for-agenda)))
    (cond ((and (string-match-p ta todo) (not (string-match-p tb todo))) +1)
          ((and (string-match-p tb todo) (not (string-match-p ta todo))) -1))))

(defun zp/org-agenda-sort-special (a b)
  (cond
   ((org-cmp-test-todo "WAIT|STBY" a b))
   ((org-cmp-test-todo "STRT" a b))))

(defun zp/org-agenda-sort-wait (a b)
  (cond
   ((org-cmp-test-todo "WAIT|STBY" a b))))



;; ========================================
;; =============== HEADERS ================
;; ========================================

(defun zp/org-agenda-format-header-align (header)
  (let ((tags-column org-agenda-tags-column))
    (format
     (concat
      "%"
      (number-to-string
       (/ (+ (- tags-column) (length header)) 2))
      "s")
     header)))

(defun zp/org-agenda-format-word-list (word-list)
  (let ((word-list-linked (s-join ";" word-list)))
    (if (not (eq word-list nil))
        (concat " " "(" word-list-linked ")"))))

(defun zp/org-agenda-format-header-main (header)
  "Format the main header block in org-agenda."
  (let* ((tags-column org-agenda-tags-column)
         (flanking-symbol "~")
         (header-formatted
          (zp/org-agenda-format-header-align
           (concat flanking-symbol " " header " " flanking-symbol)))
         (word-list ()))
    (if (eq org-agenda-include-deadlines nil)
        (add-to-list 'word-list "-deadlines" t))
    (if (eq org-habit-show-habits nil)
        (add-to-list 'word-list "-habits" t))
    (if (eq zp/org-agenda-include-category-icons nil)
        (add-to-list 'word-list "-icons" t))
    (if (eq zp/org-agenda-include-scheduled nil)
        (add-to-list 'word-list "-scheduled" t))
    (let ((word-list-formatted (s-join ";" word-list)))
      (if (not (eq word-list nil))
          (setq word-list-formatted (concat " " "(" word-list-formatted ")")))
      (concat
       header-formatted word-list-formatted "\n"))))

(defun zp/org-agenda-format-header-block (header)
  "Format header blocks in org-agenda."
  (let* ((tags-column org-agenda-tags-column)
         (header-formatted (zp/org-agenda-format-header-align header)))
    (concat
     header-formatted "\n")))

(defun zp/org-agenda-format-header-block-with-settings (header)
  "Format header blocks in org-agenda, and display important
agenda settings after them."
  (let ((word-list ()))
    (if (eq zp/org-agenda-sorting-strategy-user-defined 'special)
        (add-to-list 'word-list "+S↓" t))
    (if (eq org-agenda-todo-ignore-scheduled 'future)
        (add-to-list 'word-list "-future" t))
    (let ((header-formatted (zp/org-agenda-format-header-align header))
          (word-list-formatted (zp/org-agenda-format-word-list word-list)))
      (concat header-formatted word-list-formatted))))

;; Special blocks
(defun zp/org-agenda-format-header-projects-stuck ()
  "Format headers of ‘Stuck Projects’ in org-agenda, and display important
agenda settings after them."
  (let* ((tags-column org-agenda-tags-column)
         (flanking-symbol-left "[")
         (flanking-symbol-right "]")
         (header "Stuck Projects")
         (header-formatted
          (zp/org-agenda-format-header-align
           (concat flanking-symbol-left " " header " " flanking-symbol-right)))
         (word-list ()))
    (let ((word-list-formatted (zp/org-agenda-format-word-list word-list)))
      (concat header-formatted word-list-formatted "\n"))))

(defun zp/org-agenda-format-header-scheduled ()
  "Format headers of ‘Scheduled’ in org-agenda, and display
important agenda settings after them."
  (let* ((tags-column org-agenda-tags-column)
         (header "Scheduled")
         (word-list ()))
    (if (eq zp/org-agenda-sorting-strategy-user-defined 'special)
        (add-to-list 'word-list "+S↓" t))
    (if (eq org-agenda-todo-ignore-scheduled 'future)
        (add-to-list 'word-list "-future" t))
    (if (eq org-agenda-todo-ignore-scheduled nil)
        (add-to-list 'word-list "+future" t))
    (let ((header-formatted (zp/org-agenda-format-header-align header))
          (word-list-formatted (zp/org-agenda-format-word-list word-list)))
      (concat header-formatted word-list-formatted "\n"))))

(defun zp/org-agenda-format-header-projects ()
  "Format header blocks in org-agenda, and display important
agenda settings after them."
  (let* ((tags-column org-agenda-tags-column)
         (header "Projects")
         (word-list ()))
    (if (eq zp/org-agenda-sorting-strategy-user-defined 'special)
        (add-to-list 'word-list "+S↓" t))
    (if (eq zp/projects-include-waiting nil)
        (add-to-list 'word-list "-waiting" t))
    (let ((header-formatted (zp/org-agenda-format-header-align header))
          (word-list-formatted (zp/org-agenda-format-word-list word-list)))
      (concat header-formatted word-list-formatted))))



;; ========================================
;; ================ BLOCKS ================
;; ========================================

(defun zp/org-super-agenda-item-in-agenda-groups-p (item groups)
  "Check if ITEM is in agenda GROUPS."
  (let ((marker (or (get-text-property 0 'org-marker item)
                    (get-text-property 0 'org-hd-marker item))))
    (zp/org-task-in-agenda-groups-p groups nil marker)))

(defun zp/org-super-agenda-groups (header groups)
  "Create org-super-agenda section for GROUPS with HEADER."
  `(:name ,header
          :pred (lambda (item)
                  (zp/org-super-agenda-item-in-agenda-groups-p item ',groups))))

(defun zp/org-agenda-block-agenda-main (header &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-span 'day)
            (org-super-agenda-groups
             '(,(zp/org-super-agenda-groups "Life" '("life"))
               ,(zp/org-super-agenda-groups "Professional" '("pro"))
               ,(zp/org-super-agenda-groups "Activism" '("act"))
               ,(zp/org-super-agenda-groups "Maintenance" '("mx"))
               ,(zp/org-super-agenda-groups "Hacking" '("hack"))
               ,(zp/org-super-agenda-groups "Curiosities" '("curios"))
               ,(zp/org-super-agenda-groups "Media" '("media")))))))

(defun zp/org-agenda-block-agenda (header &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-header (header)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            (org-agenda-files nil)
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-agenda-with-group-filter (header groups &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-skip-function
             '(zp/skip-tasks-not-belonging-to-agenda-groups ',groups))
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-agenda-week (header &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-span 'week)
            (org-agenda-tag-filter-preset '("-recurring"))
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("CXLD")))
            (org-agenda-dim-blocked-tasks 'dimmed)
            (org-deadline-warning-days 0))))

(defun zp/org-agenda-block-agenda-week-appointments-only (header &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-span 'week)
            (org-agenda-tag-filter-preset '("-recurring"))
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("CXLD") 'scheduled 'deadline))
            (org-agenda-dim-blocked-tasks 'dimmed))))

(defun zp/org-agenda-block-tasks (&optional file)
  `(tags-todo "-recurring-standby-reading"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block-with-settings "Tasks"))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-sorting-strategy
                '(user-defined-down priority-down category-keep))
               ;; (org-agenda-skip-function 'zp/skip-non-tasks-and-scheduled))))
               (org-agenda-skip-function 'bh/skip-non-tasks)
               (org-agenda-todo-ignore-scheduled 'all)
               )))

(defun zp/org-agenda-block-tasks-with-group-filter (groups &optional file)
  `(tags-todo "-standby-recurring"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block-with-settings "Tasks"))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-sorting-strategy
                '(scheduled-up user-defined-down priority-down category-keep))
               ;; (org-agenda-skip-function 'zp/skip-non-tasks-and-scheduled))))
               ;; (org-agenda-skip-function 'bh/skip-non-tasks)
               (org-agenda-skip-function
                '(or (zp/skip-tasks-not-belonging-to-agenda-groups ',groups)
                  (bh/skip-non-tasks)))
               ;; (org-agenda-todo-ignore-scheduled 'all)
               (org-super-agenda-groups
                '((:name "Overdue"
                   :and (:scheduled past
                         :not (:habit t)))
                  (:name "Waiting"
                   :and (:scheduled nil
                         :tag "waiting"))
                  (:name "Scheduled today"
                   :and (:scheduled today
                         :not (:habit t)))
                  (:name "Active"
                   :and (:scheduled nil
                         :not (:tag "waiting")))
                  (:name "Scheduled later"
                   :scheduled future))))))

(defun zp/org-agenda-block-projects-stuck (&optional file)
  (let ((org-agenda-cmp-user-defined 'org-cmp-todo-state-wait))
    `(tags-todo "-standby"
                ((org-agenda-overriding-header
                  (zp/org-agenda-format-header-projects-stuck))
                 ,@(if (bound-and-true-p file)
                       `((org-agenda-files ',file)))
                 (org-agenda-skip-function 'zp/skip-non-stuck-projects)
                 (org-agenda-todo-ignore-scheduled nil)
                 (org-agenda-dim-blocked-tasks 'dimmed)))))

(defun zp/org-agenda-block-projects-stuck-with-group-filter (groups &optional file)
  `(tags-todo "-standby"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-projects-stuck))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               ;; (org-agenda-skip-function 'zp/skip-non-stuck-projects)
               (org-agenda-skip-function
                '(or (zp/skip-tasks-not-belonging-to-agenda-groups ',groups t)
                  (zp/skip-non-stuck-projects)))
               (org-agenda-todo-ignore-scheduled nil)
               (org-agenda-dim-blocked-tasks 'dimmed))))

(defun zp/org-agenda-block-projects (&optional file)
  `(tags-todo "-standby-reading"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-projects))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-skip-function 'zp/skip-non-projects-and-waiting)
               (org-agenda-sorting-strategy
                '(user-defined-down priority-down category-keep))
               (org-agenda-todo-ignore-scheduled nil)
               (org-agenda-dim-blocked-tasks nil))))

(defun zp/org-agenda-block-projects-with-group-filter (groups &optional file)
  `(tags-todo "-standby"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-projects))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               ;; (org-agenda-skip-function 'zp/skip-non-unstuck-projects-and-waiting)
               (org-agenda-skip-function
                '(or (zp/skip-tasks-not-belonging-to-agenda-groups ',groups t)
                  (zp/skip-non-unstuck-projects-and-waiting)))
               (org-agenda-sorting-strategy
                '(user-defined-down priority-down category-keep))
               (org-agenda-todo-ignore-scheduled nil)
               (org-agenda-dim-blocked-tasks nil)
               (org-super-agenda-groups
                '((:name "Waiting"
                   :tag "waiting")
                  (:name "Active"
                   :anything))))))

(defun zp/org-agenda-groups-format-regex (list)
  "Format LIST of agenda groups as a regex"
  (string-join
   (mapcar (lambda (arg)
             (if (not arg)
                 "^$"
                 (concat "\\b"
                         arg
                         "\\b")))
           list)
   "\\|"))

(defun zp/org-agenda-groups-format-regex-for-filtering (list)
  "Format LIST of agenda groups as a regex for tags-todo"
  (concat
   "+AGENDA_GROUP={"
   (zp/org-agenda-groups-format-regex list)
   "}"))



(defun zp/org-agenda-blocks-main (header groups &optional file)
  "Format the main agenda blocks.

HEADER is the string to be used as the header of the the agenda
view.

GROUPS should be a list of strings of AGENDA_GROUPS to
match (‘or’ is implied).

It creates 4 blocks:
- An ‘agenda’ block displaying the HEADER and the date
- A ‘tags-todo’ block displaying the non-stuck projects
- A ‘tags-todo’ block displaying the stuck projects
- A ‘tags-todo’ block displaying the tasks"
  `(,(zp/org-agenda-block-header
      header)
     ,(zp/org-agenda-block-projects-stuck-with-group-filter
       groups file)
     ,(zp/org-agenda-block-tasks-with-group-filter
       groups file)
     ,(zp/org-agenda-block-projects-with-group-filter
       groups file)))

(defun zp/org-agenda-block-tasks-special (&optional file)
  `(tags-todo "-standby/!WAIT|STRT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-tasks-waiting))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-skip-function 'bh/skip-non-tasks))))

(defun zp/org-agenda-block-scheduled (&optional file)
  `(tags-todo "-recurring-reading"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-scheduled))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'notscheduled))
               (org-agenda-dim-blocked-tasks 'dimmed)
               (org-agenda-sorting-strategy '(timestamp-up user-defined-down priority-down)))))

(defun zp/org-agenda-block-scheduled-with-filter (filter &optional file)
  `(tags-todo ,(concat filter "-recurring")
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-scheduled))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'notscheduled))
               (org-agenda-dim-blocked-tasks 'dimmed)
               (org-agenda-sorting-strategy '(timestamp-up user-defined-down priority-down)))))

(defun zp/org-agenda-block-tasks-waiting (&optional file)
  `(tags-todo "-recurring-reading/!WAIT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-main "Waiting & Stand-by Tasks"))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               ;; (org-agenda-skip-function
               ;;       '(org-agenda-skip-entry-if 'notscheduled))
               (org-agenda-dim-blocked-tasks 'dimmed)
               (org-agenda-sorting-strategy '(timestamp-up user-defined-down priority-down)))))

(defun zp/org-agenda-block-deadines ()
  '(agenda ""
           ((org-agenda-span 'day)
            (org-agenda-overriding-header
             (zp/org-agenda-format-header-main "Deadlines"))
            (org-agenda-entry-types '(:deadline))
            (org-agenda-include-deadlines t)
            (org-agenda-dim-blocked-tasks 'dimmed)
            (org-deadline-warning-days 31)
            (org-agenda-sorting-strategy
             '((agenda habit-down time-up deadline-up priority-down category-keep)
               (tags priority-down category-keep)
               (todo priority-down category-keep)
               (search category-keep))))))

(defun zp/org-agenda-block-reading-next-and-started ()
  '(tags-todo "+reading-recurring-standby/!NEXT|STRT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block "Next & Started")))))

(defun zp/org-agenda-block-reading-list ()
  '(tags-todo "+reading-recurring-standby/!-NEXT-STRT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block "Next & Started")))))

(defun zp/org-agenda-block-journal ()
  '(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main "Journal")))))

(setq org-use-property-inheritance '("AGENDA_GROUP"))

(require 'org-super-agenda)
(org-super-agenda-mode)

(setq org-agenda-custom-commands
      `(("n" "Agenda"
             (,(zp/org-agenda-block-agenda-main "Agenda" org-agenda-files)))

        ("N" "Agenda (w/o groups)"
             (,(zp/org-agenda-block-agenda "Agenda (w/o groups)" org-agenda-files)))

        ("k" "Weekly agenda (-recurring)"
             (,(zp/org-agenda-block-agenda-week "Weekly Agenda")))

        ("K" "Weekly appointments (-recurring)"
             (,(zp/org-agenda-block-agenda-week-appointments-only "Weekly Appointments")))

        ("i" "Inbox"
             (,@(zp/org-agenda-blocks-main "Inbox" '("inbox"))))

        ("l" "Life"
             (,@(zp/org-agenda-blocks-main "Life" '("life" "pro" "act" "mx"))))

        ("L" "Life (strict)"
             (,@(zp/org-agenda-blocks-main "Life (strict)" '("life" "mx"))))

        ("c" "Curiosities"
             (,@(zp/org-agenda-blocks-main "Curiosities" '("curios"))))

        ("a" "Activism"
             (,@(zp/org-agenda-blocks-main "Activism" '("act"))))

        ("p" "Professional"
             (,@(zp/org-agenda-blocks-main "Professional" '("pro"))))

        ("g" "Groupless Tasks"
             (,@(zp/org-agenda-blocks-main "Groupless Tasks" '(nil))))

        ("x" "Maintenance"
             (,@(zp/org-agenda-blocks-main "Maintenance" '("mx"))))

        ("j" "Journal entries"
             (,(zp/org-agenda-block-journal))
             ((org-agenda-files '("/home/zaeph/org/journal.org.gpg"))))

        ("r" "Reading (-standby)"
             (,(zp/org-agenda-block-agenda "Reading")
               ;; ,(zp/org-agenda-block-projects-stuck)
               ,(zp/org-agenda-block-reading-next-and-started)
               ,(zp/org-agenda-block-reading-list))
             ((org-agenda-tag-filter-preset (list "+reading"))
              (org-agenda-hide-tags-regexp "reading")))

        ;; ("b" "Media"
        ;;      (,(zp/org-agenda-block-agenda "Media")
        ;;        ,(zp/org-agenda-block-projects)
        ;;        ,(zp/org-agenda-block-projects-stuck)
        ;;        ,(zp/org-agenda-block-scheduled)
        ;;        ,(zp/org-agenda-block-tasks))
        ;;      ((org-agenda-files zp/org-agenda-files-media)))

        ("b" "Media"
             (,@(zp/org-agenda-blocks-main "Media" '("media"))))

        ("B" "Music"
             (,@(zp/org-agenda-blocks-main "Music" '("music"))))

        ("f" "Film"
             (,@(zp/org-agenda-blocks-main "Film" '("film"))))

        ("h" "Hacking"
             (,@(zp/org-agenda-blocks-main "Hacking" '("hack"))))

        ("o" "OPSEC"
             (,@(zp/org-agenda-blocks-main "OPSEC" '("opsec"))))

        ("C" "Contributing & Troubleshooting"
             (,@(zp/org-agenda-blocks-main "Contributing & Troubleshooting" '("contrib"))))

        ("d" "Deadlines"
             (,(zp/org-agenda-block-deadines)))

        ("w" "Waiting list"
             (,(zp/org-agenda-block-tasks-waiting)))

        ("A" "Meditation records"
             ((agenda ""
                      ((org-agenda-files zp/org-agenda-files-awakening)
                       (org-agenda-log-mode))))
             ((org-agenda-skip-timestamp-if-done nil)))

        ("S" "Swimming records"
             ((agenda ""
                      ((org-agenda-files zp/org-agenda-files-sports))))
             ((org-agenda-skip-timestamp-if-done nil)))))

;; Example for layers
;; ("h" . "Test")
;; ("he" . "Another test")
;; ("hec" tags "+home")


(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (timeline . "  % s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))



;; (setq org-agenda-sorting-strategy
(setq org-agenda-sorting-strategy '((agenda habit-down deadline-up time-up scheduled-up priority-down category-keep)
                                    (tags priority-down category-keep)
                                    ;; (tags category-keep priority-down)
                                    (todo priority-down category-keep)
                                    (search category-keep)))

;; Previous priority for agenda
;; '((agenda habit-down time-up priority-down category-keep)



;; Cumulate TODO-keywords
;; Remember the `|'
;; (tags-todo "+TODO=\"NEXT\"|+TODO=\"STARTED\"")

;; Prototype for inserting colours
;; (insert (propertize "Test" 'font-lock-face '(:foreground "green" :background "gray6")))

(defun zp/org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(defface zp/org-agenda-block-info-face nil
  "Info for blocked faces in org-agenda.")

(defface zp/org-agenda-block-warning-face nil
  "Warning for blocked faces in org-agenda.")

(defun zp/org-agenda-hi-lock ()
  (highlight-regexp "([-+].*?)" 'zp/org-agenda-block-info-face)
  (highlight-regexp "^[[:space:]]*? \\[ Stuck Projects \\]" 'zp/org-agenda-block-warning-face)
  (highlight-regexp "^~~.*~~$" 'font-lock-comment-face))

(defun zp/org-agenda-remove-mouse-face ()
  "Remove mouse-face from org-agenda."
  (remove-text-properties(point-min) (point-max) '(mouse-face t)))

(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-hi-lock)
(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-delete-empty-blocks)
(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-remove-mouse-face)

;; Good example for a toggle based on variables
(defun zp/toggle-org-habit-show-all-today ()
  "Toggle the display of habits between showing only the habits
due today, and showing all of them."
  (interactive)
  (cond ((bound-and-true-p org-habit-show-all-today)
         (setq org-habit-show-all-today nil)
         (org-agenda-redo)
         (message "Habits: Showing today"))
        (t
         (setq org-habit-show-all-today 1)
         (org-agenda-redo)
         (message "Habits: Showing all"))))

(defun zp/toggle-org-agenda-include-deadlines ()
  "Toggle the inclusion of deadlines in the agenda."
  (interactive)
  (cond ((bound-and-true-p org-agenda-include-deadlines)
         (setq org-agenda-include-deadlines nil)
         (org-agenda-redo)
         (message "Deadlines: Hidden"))
        (t
         (setq org-agenda-include-deadlines t)
         (org-agenda-redo)
         (message "Deadlines: Visible"))))

(defun zp/toggle-org-agenda-include-scheduled ()
  "Toggle the inclusion of scheduled items in the agenda."
  (interactive)
  (cond ((eq zp/org-agenda-include-scheduled 'all)
         (setq org-agenda-entry-types '(:deadline :timestamp :sexp)
               zp/org-agenda-include-scheduled nil)
         (org-agenda-redo)
         (message "Scheduled: Hidden"))
        (t
         (setq org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp)
               zp/org-agenda-include-scheduled 'all)
         (org-agenda-redo)
         (message "Scheduled: Visible"))))

(defun zp/toggle-org-agenda-category-icons ()
  "Toggle the inclusion of category icons in the agenda."
  (interactive)
  (cond (zp/org-agenda-include-category-icons
         (setq org-agenda-category-icon-alist nil
               zp/org-agenda-include-category-icons nil)
         (org-agenda-redo)
         (message "Icons: Hidden"))
        (t
         (setq org-agenda-category-icon-alist zp/org-agenda-category-icon-alist
               zp/org-agenda-include-category-icons t)
         (org-agenda-redo)
         (message "Icons: Visible"))))

(defun zp/toggle-org-agenda-cmp-user-defined ()
  "Toggle the skip function used by the agenda."
  (interactive)
  (cond ((eq zp/org-agenda-sorting-strategy-user-defined 'priority)
         (setq org-agenda-cmp-user-defined 'zp/org-agenda-sort-special
               zp/org-agenda-sorting-strategy-user-defined 'special)
         (org-agenda-redo)
         (message "Sorting: Special first"))
        (t
         (setq org-agenda-cmp-user-defined 'zp/org-agenda-sort-wait
               zp/org-agenda-sorting-strategy-user-defined 'priority)
         (org-agenda-redo)
         (message "Sorting: Priority first"))))

(defun zp/toggle-org-deadline-warning-days-range ()
  "Toggle the range of days for deadline to show up on the agenda."
  (interactive)
  (cond ((eq org-deadline-warning-days 7)
         (setq org-deadline-warning-days 31)
         (org-agenda-redo)
         (message "Deadline range: 1 month"))
        ((eq org-deadline-warning-days 31)
         (setq org-deadline-warning-days 7)
         (org-agenda-redo)
         (message "Deadline range: 1 week"))))

(defun zp/toggle-org-agenda-todo-ignore-future-scheduled ()
  "Toggle the range of days for deadline to show up on the agenda."
  (interactive)
  (cond ((eq org-agenda-todo-ignore-scheduled 'future)
         (setq org-agenda-todo-ignore-scheduled nil)
         (org-agenda-redo)
         (message "Scheduled: All (Today + Future)"))
        ((eq org-agenda-todo-ignore-scheduled nil)
         (setq org-agenda-todo-ignore-scheduled 'future)
         (org-agenda-redo)
         (message "Scheduled: Only Today"))))

(defun zp/toggle-org-agenda-projects-include-waiting ()
  "Toggle whether to include projects with a waiting task."
  (interactive)
  (cond ((eq zp/projects-include-waiting nil)
         (setq zp/projects-include-waiting t)
         (org-agenda-redo)
         (message "Projects: Showing All"))
        ((eq zp/projects-include-waiting t)
         (setq zp/projects-include-waiting nil)
         (org-agenda-redo)
         (message "Projects: Hiding Waiting"))))

(defun zp/toggle-org-agenda-dim-blocked-tasks ()
  "Toggle the dimming of blocked tags in the agenda."
  (interactive)
  (cond ((or (eq org-agenda-dim-blocked-tasks nil)
             (eq org-agenda-dim-blocked-tasks 'invisible))
         (setq org-agenda-dim-blocked-tasks t)
         (org-agenda-redo)
         (message "Blocked tasks: Dimmed"))
        (t
         (setq org-agenda-dim-blocked-tasks nil)
         (org-agenda-redo)
         (message "Blocked tasks: Plain"))))

(defun zp/toggle-org-agenda-hide-blocked-tasks ()
  "Toggle the visibility of blocked tags in the agenda."
  (interactive)
  (cond ((or (eq org-agenda-dim-blocked-tasks nil)
             (eq org-agenda-dim-blocked-tasks t))
         (setq org-agenda-dim-blocked-tasks 'invisible)
         (org-agenda-redo)
         (message "Blocked tasks: Invisible"))
        (t
         (setq org-agenda-dim-blocked-tasks t)
         (org-agenda-redo)
         (message "Blocked tasks: Dimmed"))))

;; Dangerous, since it might hide important tasks to do.
;; (defun zp/org-tags-match-list-sublevels ()
;;   "Toggle the inclusion of sublevels TODO in the agenda.."
;;   (interactive)
;;   (cond ((bound-and-true-p org-tags-match-list-sublevels)
;;          (setq org-tags-match-list-sublevels nil)
;;       (org-agenda-redo)
;;       (message "Subtasks: Hidden"))
;;         (t
;;          (setq org-tags-match-list-sublevels t)
;;       (org-agenda-redo)
;;       (message "Subtasks: Showing"))))

;; (defun zp/toggle-org-agenda-dim-blocked-tasks ()
;;   "Toggle a distraction-free environment for writing."
;;   (interactive)
;;   (cond ((eq org-agenda-dim-blocked-tasks 'invisible)
;;          (setq org-agenda-dim-blocked-tasks nil)
;;       (org-agenda-redo)
;;       (message "Blocked tasks: Plain"))
;;      ((eq org-agenda-dim-blocked-tasks nil)
;;          (setq org-agenda-dim-blocked-tasks t)
;;       (org-agenda-redo)
;;       (message "Blocked tasks: Dimmed"))
;;      ((eq org-agenda-dim-blocked-tasks t)
;;       (setq org-agenda-dim-blocked-tasks 'invisible)
;;       (org-agenda-redo)
;;       (message "Blocked tasks: Invisible"))
;;         ))

(defun zp/org-agenda-set-property (property-function)
  "Set a property for the current headline in the agenda.
Based on `org-agenda-set-property'."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (call-interactively property-function)))))

(defun zp/org-agenda-delete-property ()
  "Delete a property for the current headline in the agenda."
  (interactive)
  (zp/org-agenda-set-property 'org-delete-property))

(defun zp/org-set-appt-warntime (&optional arg)
  "Set the `APPT_WARNTIME' property."
  (interactive "P")
  (if arg
      (org-delete-property "APPT_WARNTIME")
    (org-set-property "APPT_WARNTIME" (org-read-property-value "APPT_WARNTIME"))))

(defun zp/org-agenda-set-appt-warntime (arg)
  "Set the `APPT_WARNTIME' for the current entry in the agenda."
  (interactive "P")
  (zp/org-agenda-set-property 'zp/org-set-appt-warntime)
  (zp/org-agenda-to-appt arg))

(defun zp/org-set-location ()
  "Set the `LOCATION' property."
  (interactive)
  (org-set-property "LOCATION" (org-read-property-value "LOCATION")))
(defun zp/org-agenda-set-location ()
  "Set the `LOCATION' for the current entry in the agenda."
  (interactive)
  (zp/org-agenda-set-property 'zp/org-set-location))

(defun zp/org-agenda-date-prompt-and-update-appt (arg)
  "Combine ‘org-agenda-date-prompt’ and ‘zp/org-agenda-to-appt’.

Check their respective docstrings for more info."
  (interactive "P")
  (org-agenda-date-prompt arg)
  (zp/org-agenda-to-appt))

(defun zp/org-agenda-schedule-and-update-appt (arg &optional time)
  "Combine ‘org-agenda-schedule’ and ‘zp/org-agenda-to-appt’.

Check their respective dosctrings for more info."
  (interactive "P")
  (org-agenda-schedule arg time)
  (zp/org-agenda-to-appt))

(defun zp/org-agenda-mode-config ()
  "For use with `org-agenda-mode'."
  (local-set-key (kbd "M-n") 'org-agenda-next-date-line)
  (local-set-key (kbd "M-p") 'org-agenda-previous-date-line)
  (local-set-key (kbd "k") 'zp/org-agenda-capture)
  (local-set-key (kbd "C-,") 'sunrise-sunset)
  (local-set-key (kbd ",") 'zp/hydra-org-priority/body)
  (local-set-key (kbd "M-k") 'zp/toggle-org-habit-show-all-today)
  (local-set-key (kbd "M-i") 'zp/toggle-org-agenda-category-icons)
  (local-set-key (kbd "M-t") 'org-agenda-todo-yesterday)
  (local-set-key (kbd "D") 'zp/toggle-org-agenda-include-deadlines)
  (local-set-key (kbd "S") 'zp/toggle-org-agenda-include-scheduled)
  (local-set-key (kbd "M-d") 'zp/toggle-org-deadline-warning-days-range)
  (local-set-key (kbd "r") 'zp/org-agenda-benchmark)
  (local-set-key (kbd "h") 'zp/toggle-org-agenda-cmp-user-defined)
  (local-set-key (kbd "F") 'zp/toggle-org-agenda-todo-ignore-future-scheduled)
  (local-set-key (kbd "W") 'zp/toggle-org-agenda-projects-include-waiting)
  (local-set-key (kbd "C-c C-x r") 'zp/org-agenda-set-appt-warntime)
  (local-set-key (kbd "C-c C-x l") 'zp/org-agenda-set-location)
  (local-set-key (kbd "C-c C-x d") 'zp/org-agenda-delete-property)
  (local-set-key (kbd ">") 'zp/org-agenda-date-prompt-and-update-appt)
  (local-set-key (kbd "C-c C-s") 'zp/org-agenda-schedule-and-update-appt)
  ;; (local-set-key (kbd "C-c C-w") 'zp/org-agenda-refile)
  (local-set-key (kbd "C-c C-S-w") 'zp/org-agenda-refile-with-paths)
  (local-set-key (kbd "Z") 'org-resolve-clocks)
  (local-set-key (kbd "C-<return>") 'org-agenda-switch-to)
  (local-set-key (kbd "<return>") 'zp/org-agenda-tree-to-indirect-buffer-without-grabbing-focus)
  (local-set-key (kbd "S-<return>") 'zp/org-agenda-tree-to-indirect-buffer)
  (local-set-key (kbd "M-<return>") 'zp/org-agenda-tree-to-indirect-buffer-maximise)
  (local-set-key (kbd "<backspace>") 'zp/org-agenda-kill-other-buffer-and-window))

(add-hook 'org-agenda-mode-hook 'zp/org-agenda-mode-config)



(defun zp/unfill-document ()
  "fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (point-min) (point-max))))

(defun zp/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun zp/unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun zp/unfill-context ()
  (interactive)
  (if (region-active-p)
      (zp/unfill-region)
    (zp/unfill-paragraph)))



;; ========================================
;; ============= ORG-CAPTURE ==============
;; ========================================

(setq org-default-notes-file "/home/zaeph/org/life.org.gpg")

;;; Helper functions

(defun zp/convert-m-to-hm (min-str)
  (let* ((min (string-to-number min-str))
         (h (/ min 60))
         (m (% min 60)))
    (format "%1s:%02d" h m)))

(defvar zp/org-capture-web-action nil
  "Action to be taken on the webpage captured by org-capture-web.sh.")
(defvar zp/org-capture-web-title nil
  "Title of the webpage captured by org-capture-web.sh.")
(defvar zp/org-capture-web-url nil
  "URL of the webpage captured by org-capture-web.sh.")

(defun zp/org-capture-web (action title url)
  "Capture the website based on the info provided by org-capture-web.sh.

TITLE and URL are those of the webpage.  TEMPLATE is the
subtemplate to use."
  (interactive)
  (setq zp/org-capture-web-action action)
  (setq zp/org-capture-web-title title)
  (setq zp/org-capture-web-url url)
  (org-capture nil (concat "Wa"))
  (message (concat "Link added to template: \n" url)))

(defvar zp/org-capture-web-letterboxd-title nil
  "Title of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-url nil
  "Letterboxd URL of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-director nil
  "Name of the director of the film captured by
  org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-year nil
  "Year of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-duration nil
  "Duration of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-template nil
  "Default template for capturing films from Letterboxd with org-capture.web.sh.")

(defun zp/org-capture-web-letterboxd (title url director year duration)
  "Capture a film based on the info provided by org-capture-web.sh.

TITLE, DIRECTOR, YEAR and DURATION are related to the film.

URL is the url to the Letterboxd page of the film."
  (let ((duration-str (if (string= duration "")
                          "???"
                        (zp/convert-m-to-hm duration))))
    (setq zp/org-capture-web-letterboxd-title title)
    (setq zp/org-capture-web-letterboxd-url url)
    (setq zp/org-capture-web-letterboxd-director director)
    (setq zp/org-capture-web-letterboxd-year year)
    (setq zp/org-capture-web-letterboxd-duration duration-str)
    (org-capture nil "Wf")))

(setq zp/org-capture-web-letterboxd-template
      "* %(print zp/org-capture-web-letterboxd-title)%?
:PROPERTIES:
:MEDIA_LINK: [[%(print zp/org-capture-web-letterboxd-url)][Letterboxd]]
:MEDIA_DIRECTOR: %(print zp/org-capture-web-letterboxd-director)
:MEDIA_YEAR: %(print zp/org-capture-web-letterboxd-year)
:MEDIA_DURATION: %(print zp/org-capture-web-letterboxd-duration)
:END:")

(defun zp/org-capture-web-kill-new (title url)
  "Make website the latest kill in the kill ring.

Based on the info provided by org-capture-web.sh.

TITLE and URL are those of the webpage."
  (interactive)
  (kill-new (concat "[["
                    url
                    "]["
                    title
                    "]]"))
  (message (concat "Link added to kill-ring: \n" url)))

(setq org-capture-templates
      `(("n" "Note" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* %?" :add-created t)
        ("f" "Todo" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* TODO %?" :add-created t)
        ("F" "Todo + Clock" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* TODO %?\n" :add-created t :clock-in t)
        ("r" "Todo with Context" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* TODO %?\n%a" :add-created t)
        ("R" "Todo with Context + Clock" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* TODO %?\n%a" :add-created t :clock-in t)
        ;; ("r" "Todo + Reminder" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
        ;;  "* TODO %?\nSCHEDULED: %^T\n:PROPERTIES:\n:APPT_WARNTIME:  %^{APPT_WARNTIME|5|15|30|60}\n:END:")
        ;; ("T" "Todo (with keyword selection)" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
        ;;  "* %^{State|TODO|NEXT|STBY|WAIT} %?")
        ;; ("e" "Todo + Creation time" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
        ;;  "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
        ;; ("C" "Todo + Clock" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
        ;;  "* TODO %^{Task}%?" :clock-in t)
        ;; ("C" "Todo + Clock (with keyword selection)" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
        ;;  "* %^{State|TODO|NEXT} %?" :clock-in t)
        ("d" "Date" entry (file+headline "/home/zaeph/org/life.org.gpg" "Calendar")
             "* %?\n" :add-created t)
        ("e" "Date + Context" entry (file+headline "/home/zaeph/org/life.org.gpg" "Calendar")
             "* %?\n%a" :add-created t)

        ;; ("D" "Date + Reminder" entry (file+headline "/home/zaeph/org/life.org.gpg" "Calendar")
        ;;  "* %?\n%^T\n\n%^{APPT_WARNTIME}p")
        ;; ("R" "Reminder" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
        ;;  "* %?\n%^T%^{APPT_WARNTIME}p")

        ("p" "Phone-call" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* TODO Phone-call with %^{Interlocutor|Nicolas|Mum}%?\n:STATES:\n- State \"TODO\"       from              %U\n:END:" :clock-in t)
        ("m" "Meeting" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
             "* TODO Meeting with %^{Meeting with}%?" :clock-in t)

        ("s" "Special")
        ("ss" "Code Snippet" entry (file "/home/zaeph/org/projects/hacking/snippets.org.gpg")
              ;; Prompt for tag and language
              "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ;; ("sf" "Film recommendation" entry (file+olp "/home/zaeph/org/media.org.gpg" "Films" "List")
        ;;  "* %(zp/org-capture-set-media-link-letterboxd)%?%^{MEDIA_DIRECTOR}p%^{MEDIA_YEAR}p%^{MEDIA_DURATION}p")
        ;; ("sf" "Film recommendation" entry (file+olp "/home/zaeph/org/media.org.gpg" "Films" "List")
        ;;  "* %(zp/letterboxd-set-link)%?%^{MEDIA_DIRECTOR}p%^{MEDIA_YEAR}p%(zp/letterboxd-set-duration)")
        ("sf" "Film" entry (file+olp "/home/zaeph/org/media.org.gpg" "Films" "List")
              "* %(zp/letterboxd-capture)")
        ("sF" "Film (insert at top)" entry (file+olp "/home/zaeph/org/media.org.gpg" "Films" "List")
              "* %(zp/letterboxd-capture)" :prepend t)
        ("sw" "Swimming workout" entry (file+weektree+prompt "/home/zaeph/org/sports/swimming/swimming.org.gpg")
              "* DONE Training%^{SWIM_DISTANCE}p%^{SWIM_DURATION}p\n%t%(print zp/swimming-workout-default)")

        ("j" "Journal")
        ("jj" "Journal" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Life")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("ja" "Awakening" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Awakening")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("jp" "Psychotherapy" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Psychotherapy")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("jw" "Writing" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Writing")
              "* %^{Title|Entry} %^g\n%T\n\n%?" :full-frame t)
        ("jr" "Research" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Research")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("ju" "University" entry (file+olp "/home/zaeph/org/journal.org.gpg" "University")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("jh" "Hacking" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Hacking")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("jm" "Music" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Music")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)
        ("js" "Swimming" entry (file+olp "/home/zaeph/org/journal.org.gpg" "Swimming")
              "* %^{Title|Entry}\n%T\n\n%?" :full-frame t)

        ;; Daily Record of Dysfunctional Thoughts
        ("D" "Record Dysfunctional Thoughts" entry (file+headline "/home/zaeph/org/journal.org.gpg" "Psychotherapy")
             "* Record of Dysfunctional Thoughts\n%T\n** Situation\n%?\n** Emotions\n** Thoughts")

        ;; Pain Diary
        ("P" "Pain Diary" entry (file+olp "/home/zaeph/org/life.org.gpg" "Psychotherapy" "Pain Diary")
             "* Entry: %U
** What were you doing or what happened?
%?
** What did you start struggling with psychologically?
** What thoughts came up in association with that struggle?")

        ("a" "Meditation session" entry (file+headline "/home/zaeph/org/projects/awakening/awakening.org.gpg" "Sessions")
             "* DONE Session%^{SESSION_DURATION}p\n%t" :immediate-finish t)

        ("W" "Web")
        ("Wa" "Automatic template" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
              "* TODO %(print zp/org-capture-web-action) [[%?%(print zp/org-capture-web-url)][%(print zp/org-capture-web-title)]] :web:"
              :add-created t)
        ("Wf" "S: Film" entry (file+olp "/home/zaeph/org/life.org.gpg" "Film" "List")
              ,zp/org-capture-web-letterboxd-template
              :prepend t)))



(zp/convert-m-to-hm "145")

(defvar zp/swimming-workout-default nil)
(setq zp/swimming-workout-default "
|-----+-----------------------------------|
| 500 | warmup crawl/fly                  |
| 500 | 100 pull / 100 pull fast          |
| 500 | 5*25 kick steady / 5*25 kick fast |
| 500 | 100 pull / 100 pull fast          |
| 500 | 50 fly / 100 crawl                |
| 500 | 100 pull / 100 pull fast          |
| 500 | 50 fly / 100 crawl                |
| 100 | warmdown                          |
|-----+-----------------------------------|")

(setq zp/org-agenda-capture-templates
      '(("f" "Todo" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
         "* TODO %?\nSCHEDULED: %t")
        ("r" "Todo (+time)" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
         "* TODO %?\nSCHEDULED: %^T")

        ("d" "Date" entry (file+olp "/home/zaeph/org/life.org.gpg" "Life" "Calendar")
         "* %?\n%t")
        ("e" "Date (+time)" entry (file+olp "/home/zaeph/org/life.org.gpg" "Life" "Calendar")
         "* %?\n%^T")

        ("s" "Todo & Scheduled" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
         "* TODO %?\nSCHEDULED: %t")
        ("w" "Todo & Scheduled (+time)" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
         "* TODO %?\nSCHEDULED: %^T")

        ("g" "Todo + Deadline" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
         "* TODO %?\nDEADLINE: %t")
        ("t" "Todo & Deadline (+time)" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
         "* TODO %?\nDEADLINE: %^T")))

(defun zp/org-agenda-capture (&optional arg)
  (interactive "P")
  (let ((org-capture-templates zp/org-agenda-capture-templates))
    (org-agenda-capture arg)))



        ;; ("v" "Vocabulary")
        ;; ("ve" "EN" entry (file+olp "/home/zaeph/org/life.org.gpg" "Vocabulary")
        ;;  "* EN: %?\n%U\n")
        ;; ("vf" "FR" entry (file+olp "/home/zaeph/org/life.org.gpg" "Vocabulary")
        ;;  "* FR: %?\n%U\n")
        ;; ("vj" "JA" entry (file+olp "/home/zaeph/org/life.org.gpg" "Vocabulary")
        ;;  "* JA: %?\n%U\n")
        ;; ("vk" "KO" entry (file+olp "/home/zaeph/org/life.org.gpg" "Vocabulary")
        ;;  "* KO: %?\n%U\n")))

;; ;; Empty lines before and after
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
;;       "* TODO %?" :empty-lines-before 1 :empty-lines-after 1)
;;      ("T" "Todo+" entry (file+headline "/home/zaeph/org/life.org.gpg" "Inbox")
;;       "* TODO %?\n%U\n%a\n" :empty-lines-before 1 :empty-lines-after 1)
;;      ("j" "Journal" entry (file+datetree "/home/zaeph/org/journal.org")
;;       "* %?\n%U\n")))

(defvar zp/org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq zp/org-capture-before-config (current-window-configuration)))

(defun zp/org-capture-make-full-frame ()
  "Maximise the org-capture frame if :full-frame is non-nil."
  (let ((full-frame (plist-get org-capture-plist :full-frame)))
    (if full-frame
        (delete-other-windows))))

(add-hook 'org-capture-mode-hook 'zp/org-capture-make-full-frame)



;; ========================================
;; ============== ORG-REFILE ==============
;; ========================================

(setq org-refile-targets '((nil :maxlevel . 9))
      org-refile-use-cache nil
      org-refile-target-verify-function 'zp/org-refile-target-verify-exclude-separators)



(setq org-outline-path-complete-in-steps nil
      org-refile-use-outline-path nil)

(defvar zp/hydra-org-refile-chain nil
  "When non-nil, make zp/hydra-org-refile chain the commands.")

(defun zp/hydra-org-refile-chain-toggle ()
  "Toggle zp/hydra-org-refile-chain."
  (interactive)
  (if zp/hydra-org-refile-chain
      (setq zp/hydra-org-refile-chain nil)
    (setq zp/hydra-org-refile-chain t)))

(defvar zp/hydra-org-jump-indirect t
  "When non-nil, jumping to a refile point is done in an indirect buffer.")

(defun zp/hydra-org-jump-indirect-toggle ()
  "Toggle zp/hydra-org-jump-indirect."
  (interactive)
  (if zp/hydra-org-jump-indirect
      (setq zp/hydra-org-jump-indirect nil)
    (setq zp/hydra-org-jump-indirect t)))

(defvar zp/hydra-org-jump-dedicated-buffer nil
  "When non-nil, jumping to a refile point is done in dedicated buffer.")

(defun zp/hydra-org-jump-dedicated-buffer-toggle ()
  "Toggle zp/hydra-org-dedicated-buffer."
  (interactive)
  (if zp/hydra-org-jump-dedicated-buffer
      (setq zp/hydra-org-jump-dedicated-buffer nil)
    (setq zp/hydra-org-jump-dedicated-buffer t)))

(defun zp/org-refile-with-paths (&optional arg default-buffer rfloc msg)
  (interactive "P")
  (let ((org-refile-use-outline-path 1)
        (org-refile-targets
         '((nil :maxlevel . 9)
           (org-agenda-files :maxlevel . 3))))
    (org-refile arg default-buffer rfloc msg)))

(defun zp/org-agenda-refile (&optional arg default-buffer rfloc msg goto rfloc no-update)
  (interactive "P")
  (if current-prefix-arg
      (org-refile arg default-buffer rfloc msg)
    (org-agenda-refile goto rfloc no-update)))

(defun zp/org-agenda-refile-with-paths (&optional arg default-buffer rfloc msg goto rfloc no-update)
  (interactive "P")
  (let ((org-refile-use-outline-path 1)
        (org-refile-targets
         '((nil :maxlevel . 9)
           (org-agenda-files :maxlevel . 3))))
    (if current-prefix-arg
        (org-refile arg default-buffer rfloc msg)
      (org-agenda-refile goto rfloc no-update))))



(defun zp/org-refile-internal (file headline-or-olp &optional arg)
  "Refile to a specific location.

With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode.

If HEADLINE-OR-OLP is a string, interprets it as a heading.  If
HEADLINE-OR-OLP is a list, interprets it as an olp path (without
the filename)."
  (let* ((pos (with-current-buffer
                  (or (get-buffer file) ;Is the file open in a buffer already?
                      (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
                (or (if (not (listp headline-or-olp))
                        (org-find-exact-headline-in-buffer headline-or-olp)
                      (org-find-olp `(,(buffer-file-name) ,@headline-or-olp)))
                    (error "Can't find headline-or-olp `%s'" headline-or-olp))))
         (filepath (buffer-file-name (marker-buffer pos))) ;If we're given a relative name, find absolute path
         (rfloc (list headline-or-olp filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode)
             (not arg)) ;Don't use org-agenda-refile if we're just jumping
        (org-agenda-refile nil rfloc)
      (org-refile arg nil rfloc))))

(defun zp/org-refile (&optional jump)
  (interactive "P")
  (let (;; (org-refile-targets '((nil :maxlevel . 9)))
        (capturing (and (boundp 'org-capture-mode) org-capture-mode))
        (in-agenda (derived-mode-p 'org-agenda-mode))
        (org-refile-use-outline-path t)
        ;; (org-refile-target-verify-function 'zp/org-refile-target-verify-exclude-separators)
        target)
    (when (org-before-first-heading-p)
      (outline-next-heading))
    (cond ((and (not jump)
                capturing)
           (org-capture-refile))
          ((and (not jump)
                in-agenda)
           (org-agenda-refile))
          (t
           (org-refile jump)))
    (setq target (point))))

;; (defun zp/org-refile (&optional arg)
;;   (interactive "P")
;;   (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode))
;;         (is-agenda (derived-mode-p 'org-agenda-mode)))
;;     (cond
;;       (is-capturing
;;        (org-capture-refile))
;;       (is-agenda
;;        (org-agenda-refile))
;;      (t
;;       (org-refile arg)))
;;     (cond ((or arg is-capturing)
;;            (setq hydra-deactivate t))
;;           (zp/hydra-org-refile-chain
;;            (zp/hydra-org-refile/body)))))


(defun zp/org-jump ()
  (interactive)
  (goto-char (save-excursion (zp/org-refile t))))

(defun zp/org-refile-dwim (arg)
  (interactive "P")
  (pcase arg
    ('(4) (if (buffer-narrowed-p)
              (zp/org-refile-restricted)
            (zp/org-refile)))
    (_ (zp/hydra-org-refile/body))))

(defun zp/org-jump-dwim (arg)
  (interactive "P")
  (pcase arg
    ('(4) (if (buffer-narrowed-p)
              (zp/org-jump-restricted)
            (zp/org-jump)))
    (_ (zp/hydra-org-jump/body))))

(defun zp/org-refile-main (&optional jump)
  "Refile to headline in main org-agenda file (life.org.gpg).

If JUMP is non-nil, jump instead."
  (interactive "P")
  (let ((org-refile-targets '(("~/org/life.org.gpg" :maxlevel . 1))))
    (zp/org-refile jump)))

(defun zp/org-jump-main ()
  "Jump to headline in main org-agenda file (life.org.gpg)."
  (interactive)
  (with-current-buffer (find-file-noselect "~/org/life.org.gpg")
    (save-excursion
      (zp/org-refile-main t)
      (zp/org-tree-to-indirect-buffer-folded
       ;; Create a dedicated buffer?
       (when zp/hydra-org-jump-dedicated-buffer
         (not (zp/hydra-org-jump-dedicated-buffer-toggle)))))))

(defun zp/org-refile-target-verify-exclude-separators ()
  (let ((regex "^\\* -+.*-+$"))
    ;; (message (buffer-substring-no-properties (point) (line-end-position)))
    (if (re-search-forward regex (line-end-position) t)
        nil
      t)))

(defun zp/org-refile-target-verify-restricted ()
  (let ((regex "^\\* -+.*-+$"))
    ;; (message (buffer-substring-no-properties (point) (line-end-position)))
    (cond ((< (point) min)
           (goto-char min)
           nil)
          ((> (point) max)
           (goto-char (point-max))
           nil)
          (t
           (zp/org-refile-target-verify-exclude-separators)))
    ))

(defun zp/org-refile-restricted (&optional jump)
  "Refile to headline in current file (life.org.gpg).

If JUMP is non-nil, jump instead."
  (interactive "P")
  (let ((org-refile-targets '((nil :maxlevel . 9)))
        ;; (org-refile-use-outline-path t)
        (org-refile-target-verify-function 'zp/org-refile-target-verify-restricted)
        target
        ;; Restriction info for verify function
        (min (point-min))
        (max (point-max)))
    (zp/org-refile jump)))

(defun zp/org-jump-restricted ()
  "Jump to headline in current org-agenda file (life.org.gpg)."
  (interactive)
  (let ((indirect (not (buffer-file-name)))
        target
        (buffer (current-buffer)))
    (save-excursion
      (setq target (zp/org-refile-restricted t)))
    (when indirect (switch-to-buffer buffer))
    (goto-char target)
    (org-reveal)
    (org-beginning-of-line)))

(defun zp/org-tree-to-indirect-buffer-folded (&optional dedicated)
  "Clone tree to indirect buffer in a folded state.

When called with a C-u argument or when DEDICATED is non-nil,
create a dedicated frame."
  (interactive "P")
  (let ((org-indirect-buffer-display 'current-window)
        (last-ibuf org-last-indirect-buffer)
        (buffer))
    (when dedicated
      (setq org-last-indirect-buffer nil))
    (org-tree-to-indirect-buffer)
    (when dedicated
      (setq org-last-indirect-buffer last-ibuf))
    (setq buffer (current-buffer))
    (mode-line-other-buffer)
    (bury-buffer)
    (switch-to-buffer buffer t)
    (let ((org-startup-folded nil))
      (org-set-startup-visibility))
    (org-overview)
    (org-cycle)))

(defun zp/org-refile-to (file headline-or-olp &optional jump)
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
    (if is-capturing
        (zp/org-capture-refile-internal file headline-or-olp jump)
      (zp/org-refile-internal file headline-or-olp (if jump jump nil)))
    (cond (is-capturing
           ;; If capturing, deactivate hydra
           (setq hydra-deactivate t)))))

(defun zp/org-jump-to (file headline-or-olp)
  (let ((indirect zp/hydra-org-jump-indirect))
    (zp/org-refile-to file headline-or-olp t)
    (when indirect
      (zp/org-tree-to-indirect-buffer-folded
       ;; Create a dedicated buffer?
       (when zp/hydra-org-jump-dedicated-buffer
         (not (zp/hydra-org-jump-dedicated-buffer-toggle)))))))

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
    (org-capture-finalize)
    (save-window-excursion
      (with-current-buffer (or base (current-buffer))
        (org-with-wide-buffer
         (goto-char pos)
         (zp/org-refile-internal file headline-or-olp arg))))
    (when kill-buffer (kill-buffer base))))



(defmacro zp/create-hydra-org-refile-protocol (protocol chain name docstring targets heads &optional back)
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
                    ('refile 'zp/org-refile-to)
                    ('jump 'zp/org-jump-to)))
         (jumping (if (eq protocol 'jump) t)))
    `(defhydra ,hydra
         (:foreign-keys warn
          :exit ,(if chain nil t)
          :pre (progn
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
                     `(,key (,command ,file ',olp))))
                 targets)
       ;; Create other heads
       ,@(mapcar (lambda (head)
                   (let* ((key (car head))
                          (head-name (symbol-name (cadr head)))
                          (head-hydra (intern (concat "zp/hydra-org-"
                                                      protocol-name
                                                      (when chain
                                                        "-chain")
                                                      "-" head-name
                                                      "/body"))))
                     `(,key ,head-hydra :exit t)))
                 heads)
       ;; Conditional actions
       ("C" ,hydra-sister
            (concat (if zp/hydra-org-refile-chain
                        "[x]"
                      "[ ]")
                    " chain") :exit t)
       ,@(cond (jumping
                `(("T" zp/hydra-org-jump-indirect-toggle
                       (concat (if zp/hydra-org-jump-indirect
                                   "[x]"
                                 "[ ]")
                               " indirect") :exit nil)
                  ("J" zp/hydra-org-jump-dedicated-buffer-toggle
                       (concat (if zp/hydra-org-jump-dedicated-buffer
                                   "[x]"
                                 "[ ]")
                               " dedicated") :exit nil)
                  ("j" zp/org-jump-main "jump")))
               (t
                `(("w" zp/org-refile-main "refile")
                  ("W" zp/org-refile-with-paths "refile+paths")
                  ("0" (zp/org-refile-with-paths '(64)) "reset cache" :exit nil))))
       ,@(when name `(("<backspace>" ,hydra-back "back" :exit t)))
       ("q" (progn
              (setq zp/hydra-org-jump-dedicated-buffer nil)
              (message "Cancelled")) "cancel" :exit t))))

(defmacro zp/create-hydra-org-refile (name docstring targets heads &optional back)
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

(zp/create-hydra-org-refile nil
    "
^Life^              ^Prog^                  ^Pro & Act^        ^Mental^
^^^^^^^^----------------------------------------------------------------------
_i_: Inbox          _h_: Hacking            _u_: University    _A_: Awakening
_o_: Life           _l_: Linux              _r_: Research      _P_: Psychotherapy
_I_: Curiosities    _e_: Emacs
_s_: Social         _E_: Elisp              _p_: Politics
_n_: Nicolas        _O_: Org
_S_: Swimming       _T_: LaTeX
_R_: Running        _g_: Git
_t_: Typography     _b_: Troubleshooting
^^                  _B_: Contributing
_x_: Maintenance
_m_: Media
_c_: Calendars
"
  (("i" "~/org/life.org.gpg" "Inbox")
   ("o" "~/org/life.org.gpg" "Life")
   ("I" "~/org/life.org.gpg" "Curiosities")
   ("s" "~/org/life.org.gpg" "Social")
   ("n" "~/org/life.org.gpg" "Social" "Nicolas")
   ("S" "~/org/life.org.gpg" "Swimming")
   ("R" "~/org/life.org.gpg" "Running")
   ("M" "~/org/life.org.gpg" "Media")
   ("t" "~/org/life.org.gpg" "Typography")
   ("X" "~/org/life.org.gpg" "Maintenance")
   ("A" "~/org/life.org.gpg" "Awakening")
   ("P" "~/org/life.org.gpg" "Psychotherapy")
   ("p" "~/org/life.org.gpg" "Politics")
   ("u" "~/org/life.org.gpg" "University")
   ("r" "~/org/life.org.gpg" "Research")
   ("h" "~/org/life.org.gpg" "Hacking")
   ("B" "~/org/life.org.gpg" "Contributing")
   ("b" "~/org/life.org.gpg" "Troubleshooting")
   ("T" "~/org/life.org.gpg" "LaTeX")
   ("e" "~/org/life.org.gpg" "Emacs")
   ("O" "~/org/life.org.gpg" "Org")
   ("E" "~/org/life.org.gpg" "Elisp")
   ("l" "~/org/life.org.gpg" "Linux")
   ("g" "~/org/life.org.gpg" "Git"))
  (("c" calendars)
   ("x" maintenance)
   ("m" media)))

(zp/create-hydra-org-refile calendars
    "
^Calendars^
^^----------------------------------------------------------------------
_o_: Life
_P_: Psychotherapy
_p_: Politics
_m_: Media
_n_: Nicolas
_a_: Animals
_s_: Social
_f_: Finances
_h_: Hacking
_u_: University
"
  (("o" "/home/zaeph/org/life.org.gpg" "Life" "Calendar")
   ("p" "/home/zaeph/org/life.org.gpg" "Politics" "Calendar")
   ("h" "/home/zaeph/org/life.org.gpg" "Hacking" "Calendar")
   ("u" "/home/zaeph/org/life.org.gpg" "University" "Calendar")
   ("P" "/home/zaeph/org/life.org.gpg" "Psychotherapy" "Calendar")
   ("m" "/home/zaeph/org/life.org.gpg" "Media" "Calendar")
   ("n" "/home/zaeph/org/life.org.gpg" "Social" "Nicolas" "Calendar")
   ("a" "/home/zaeph/org/life.org.gpg" "Animals" "Calendar")
   ("s" "/home/zaeph/org/life.org.gpg" "Social" "Calendar")
   ("f" "/home/zaeph/org/life.org.gpg" "Finances" "Calendar"))
  nil)

(zp/create-hydra-org-refile maintenance
    "
^Maintenance^
^^----------------------------------------------------------------------
_._: Root
_c_: Cleaning
_k_: Cooking
_h_: Health
_s_: Supplies
_f_: Finances
"
  (("." "/home/zaeph/org/life.org.gpg" "Maintenance")
   ("c" "/home/zaeph/org/life.org.gpg" "Cleaning")
   ("k" "/home/zaeph/org/life.org.gpg" "Cooking")
   ("h" "/home/zaeph/org/life.org.gpg" "Health")
   ("f" "/home/zaeph/org/life.org.gpg" "Finances")
   ("s" "/home/zaeph/org/life.org.gpg" "Supplies"))
  nil
  nil)

(zp/create-hydra-org-refile media
    "
^Media^
^^----------------------------------------------------------------------
_._: Root
_m_: Music
_n_: News
_f_: Film
_b_: Books
"
  (("." "/home/zaeph/org/life.org.gpg" "Media")
   ("M" "/home/zaeph/org/life.org.gpg" "Music")
   ("n" "/home/zaeph/org/life.org.gpg" "News")
   ("F" "/home/zaeph/org/life.org.gpg" "Film")
   ("B" "/home/zaeph/org/life.org.gpg" "Books")
   ("W" "/home/zaeph/org/life.org.gpg" "Film"))
  (("m" music)
   ("b" books)
   ("f" film))
  nil)

(zp/create-hydra-org-refile film
    "
^Film^
^^----------------------------------------------------------------------
_._: Root
_l_: List
_d_: Read
"
  (("." "/home/zaeph/org/life.org.gpg" "Film")
   ("l" "/home/zaeph/org/life.org.gpg" "Film" "List")
   ("d" "/home/zaeph/org/life.org.gpg" "Film" "Watchedn"))
  nil
  media)

(zp/create-hydra-org-refile books
    "
^Books^
^^----------------------------------------------------------------------
_._: Root
_l_: List
_d_: Read
"
  (("." "/home/zaeph/org/life.org.gpg" "Books")
   ("l" "/home/zaeph/org/life.org.gpg" "Books" "List")
   ("d" "/home/zaeph/org/life.org.gpg" "Books" "Watched"))
  nil
  media)

(zp/create-hydra-org-refile music
    "
^Music^
^^----------------------------------------------------------------------
_._: Root
_c_: Classical
_J_: Jazz
"
  (("." "/home/zaeph/org/life.org.gpg" "Music")
   ("c" "/home/zaeph/org/life.org.gpg" "Music" "List of classical pieces")
   ("J" "/home/zaeph/org/life.org.gpg" "Music" "List of jazz pieces"))
  nil
  media)



(global-set-key (kbd "C-c C-w") 'zp/hydra-org-refile/body)
(global-set-key (kbd "C-c C-j") 'zp/hydra-org-jump/body)
(define-key org-capture-mode-map (kbd "C-c C-w") 'zp/hydra-org-refile/body)
(define-key org-mode-map (kbd "C-c C-w") 'zp/org-refile-dwim)
(define-key org-agenda-mode-map (kbd "C-c C-w") 'zp/hydra-org-refile/body)



;; ========================================
;; ================= APPT =================
;; ========================================

(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 15
      appt-display-interval 5
      appt-display-mode-line nil)
;; (setq appt-display-interval 1)


; Use appointment data from org-mode
(defun zp/org-agenda-to-appt (&optional arg)
  "Update appt-list based on org-agenda items."
  (interactive "P")
  (setq appt-time-msg-list nil)
  (unless (not (equal arg '(4)))
    (appt-check)
    (message "Appt has been reset"))
  (let ((inhibit-message t))
    (org-agenda-to-appt)))

(defun zp/org-agenda-to-appt-on-load ()
  "Hook to `org-agenda-finalize-hook' which creates the appt-list
on init and them removes itself."
  (zp/org-agenda-to-appt)
  (remove-hook 'org-agenda-finalize-hook #'zp/org-agenda-to-appt-on-load))

(defun zp/org-agenda-to-appt-on-save ()
  ;; (if (string= (buffer-file-name) (concat (getenv "HOME") "/org/life.org.gpg"))
  ;; (if (string< (buffer-file-name) "org.gpg")
  (if (member buffer-file-name org-agenda-files)
      (zp/org-agenda-to-appt)))

(defun zp/org-agenda-to-appt-on-state-change ()
  "Hook to ‘org-after-todo-state-change-hook’ and update
appt-list when an item is marked as DONE."
  (when (string-equal org-state "DONE")
    (zp/org-agenda-to-appt)))


;; ----------------------------------------
;; Update reminders when...

;; Starting Emacs
;; (zp/org-agenda-to-appt)

;; Everyday at 12:05am
;; (run-at-time "12:05am" (* 24 3600) 'zp/org-agenda-to-appt)

;; When saving org-agenda-files
(add-hook 'after-save-hook #'zp/org-agenda-to-appt-on-save)

;; When loading org-agenda for the first time
(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-to-appt-on-load)

;; When marking an item as done
(add-hook 'org-after-todo-state-change-hook #'zp/org-agenda-to-appt-on-state-change)

;; ----------------------------------------
;; Remove hooks
;; (remove-hook 'after-save-hook 'zp/org-agenda-to-appt-on-save)
;; (remove-hook 'org-agenda-finalize-hook 'zp/org-agenda-to-appt)

;; ----------------------------------------

; Display appointments as a window manager notification
(setq appt-disp-window-function 'zp/appt-display)
(setq appt-delete-window-function (lambda () t))

;; (setq my-appt-notification-app (concat (getenv "HOME") "/bin/appt-notification"))
(setq zp/appt-notification-app "/home/zaeph/.bin/appt-notify")

(defun zp/appt-display (min-to-app new-time msg)
  (if (atom min-to-app)
    (start-process "zp/appt-notification-app" nil zp/appt-notification-app min-to-app msg)
  (dolist (i (number-sequence 0 (1- (length min-to-app))))
    (start-process "zp/appt-notification-app" nil zp/appt-notification-app (nth i min-to-app) (nth i msg)))))



;; ========================================
;; ================ MAGIT =================
;; ========================================

(require 'magit)
(setq magit-diff-refine-hunk 'all)
(magit-wip-mode)



;; ========================================
;; =============== CHRONOS ================
;; ========================================

(require 'chronos)
(load "/home/zaeph/.emacs.d/lisp/helm-chronos-patched.el")
(require 'helm-chronos)  ;; Doesn't support creating new timers from helm

(setq chronos-expiry-functions '(chronos-notify))

(defun chronos-notify (c)
  "Notify expiration of timer C using custom script."
  (chronos--shell-command "Chronos notification"
                          "chronos-notify"
                          (list (chronos--time-string c)
                                (chronos--message c))))

;; Fix for adding new timers
(defvar helm-chronos--fallback-source
  (helm-build-dummy-source "Enter <expiry time spec>/<message>"
    :filtered-candidate-transformer
    (lambda (_candidates _source)
      (list (or (and (not (string= helm-pattern ""))
                     helm-pattern)
                "Enter a timer to start")))
    :action '(("Add timer" . (lambda (candidate)
                               (if (string= helm-pattern "")
                                   (message "No timer")
                                 (helm-chronos--parse-string-and-add-timer helm-pattern)))))))

(setq helm-chronos-recent-timers-limit 100
      helm-chronos-standard-timers
      '(
        "Green Tea              3/Green Tea: Remove tea bag"
        "Black Tea              4/Black Tea: Remove tea bag"
        "Herbal Tea             10/Herbal Tea: Remove tea bag"
        "Timebox                25/Finish and Reflect + 5/Back to it"
        "Break                  30/Back to it"
        "Charge Phone           30/Unplug Phone"
        "Charge Tablet          30/Unplug Tablet"
        ))

(defun zp/chronos-edit-selected-line-time (time prefix)
  (interactive)
  (interactive "sTime: \nP")
  (let ((c chronos--selected-timer))
    (when (chronos--running-or-paused-p c)
      (let ((ftime (chronos--parse-timestring time
                                              (if prefix
                                                  nil
                                                (chronos--expiry-time c)))))
            ;; (msg (read-from-minibuffer "Message: " (chronos--message c))))
        (chronos--set-expiry-time c ftime)
        ;; (chronos--set-message c msg)
        (chronos--set-action c (not (chronos--expiredp c)))
        (chronos--update-display)))))

(defun zp/chronos-edit-quick (time string)
  (interactive)
  (zp/chronos-edit-selected-line-time time nil)
  (if (string-match-p "-" time)
      (message (concat "Subtracted " string " from selected timer."))
    (message (concat "Added " string " to selected timer."))))

(defun zp/chronos-quit (&optional arg)
  "Kill chronos window on quit when there are no more timers
running."
  (interactive "P")
  (let* ((timers chronos--timers-list)
         (last-timer-is-now (not (nth 1(nth 0 (last timers)))))
         (no-running-timer (if (> (length timers) 1)
                               nil
                             't)))
    (if (or (and last-timer-is-now
                 no-running-timer)
            (eq arg '(4)))
        (quit-window 1)
      (quit-window))))

;; Hook
(defun chronos-mode-config ()
  "Modify keymaps used by `org-mode'."
  (local-set-key (kbd "U") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "-0:00:05" "5 s")))
  (local-set-key (kbd "I") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "+0:00:05" "5 s")))
  (local-set-key (kbd "u") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "-0:00:15" "15 s")))
  (local-set-key (kbd "i") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "+0:00:15" "15 s")))
  (local-set-key (kbd "j") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "-0:01:00" "1 min")))
  (local-set-key (kbd "k") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "+0:01:00" "1 min")))
  (local-set-key (kbd "J") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "-0:05:00" "5 min")))
  (local-set-key (kbd "K") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "+0:05:00" "5 min")))
  (local-set-key (kbd "m") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "-0:10:00" "10 min")))
  (local-set-key (kbd ",") (lambda ()
                             (interactive)
                             (zp/chronos-edit-quick "+0:10:00" "10 min")))
  (local-set-key (kbd "a") 'helm-chronos-add-timer)
  )
(setq chronos-mode-hook 'chronos-mode-config)



;; ========================================
;; =========== WHITESPACE-MODE ============
;; ========================================

(require 'whitespace)
(defun zp/whitespace-mode-lines-tail ()
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (progn
        (whitespace-mode -1)
        (message "Whitespace mode disabled in current buffer"))
    (let ((whitespace-style '(face trailing lines-tail))
          (whitespace-line-column 80))
      (whitespace-mode t)
      (message "Whitespace mode enabled in current buffer"))))



;; ========================================
;; ============== ORG-NOTER ===============
;; ========================================

;; (load "/home/zaeph/.emacs.d/lisp/org-noter-patched.el")
;; (load "/home/zaeph/.emacs.d/pkg/org-noter/org-noter.el")
(require 'org-noter)
(setq org-noter-hide-other t
      org-noter-auto-save-last-location t
      org-noter-doc-split-fraction '(0.6 0.4))

(add-hook #'org-noter-notes-mode-hook #'visual-line-mode)

;;; Fix for hiding truncation
(defun org-noter--set-notes-scroll (window &rest ignored)
  nil)

;; Fix for visual-line-mode with PDF files
(defun org-noter--note-after-tipping-point (point note-property view)
  nil)

;; -----------------------------------------------------------------------------
;;; Fix for truncation indicators in the margins
;; (el-patch-feature org-noter)
;; (with-eval-after-load 'org-noter
;;   (el-patch-defun org-noter--create-session (ast document-property-value notes-file-path)
;;                (let* ((raw-value-not-empty (> (length (org-element-property :raw-value ast)) 0))
;;                       (display-name (if raw-value-not-empty
;;                                         (org-element-property :raw-value ast)
;;                                       (file-name-nondirectory document-property-value)))
;;                       (frame-name (format "Emacs Org-noter - %s" display-name))

;;                       (document (find-file-noselect document-property-value))
;;                       (document-path (expand-file-name document-property-value))
;;                       (document-major-mode (buffer-local-value 'major-mode document))
;;                       (document-buffer-name
;;                        (generate-new-buffer-name (concat (unless raw-value-not-empty "Org-noter: ") display-name)))
;;                       (document-buffer
;;                        (if (eq document-major-mode 'nov-mode)
;;                            document
;;                          (make-indirect-buffer document document-buffer-name t)))

;;                       (notes-buffer
;;                        (make-indirect-buffer
;;                         (or (buffer-base-buffer) (current-buffer))
;;                         (generate-new-buffer-name (concat "Notes of " display-name)) t))

;;                       (session
;;                        (make-org-noter--session
;;                         :id (org-noter--get-new-id)
;;                         :display-name display-name
;;                         :frame
;;                         (if (or org-noter-always-create-frame
;;                                 (catch 'has-session
;;                                   (dolist (test-session org-noter--sessions)
;;                                     (when (eq (org-noter--session-frame test-session) (selected-frame))
;;                                       (throw 'has-session t)))))
;;                             (make-frame `((name . ,frame-name) (fullscreen . maximized)))
;;                           (set-frame-parameter nil 'name frame-name)
;;                           (selected-frame))
;;                         :doc-mode document-major-mode
;;                         :property-text document-property-value
;;                         :notes-file-path notes-file-path
;;                         :doc-buffer document-buffer
;;                         :notes-buffer notes-buffer
;;                         :level (org-element-property :level ast)
;;                         :window-behavior (org-noter--property-or-default notes-window-behavior)
;;                         :window-location (org-noter--property-or-default notes-window-location)
;;                         :doc-split-fraction (org-noter--property-or-default doc-split-fraction)
;;                         :auto-save-last-location (org-noter--property-or-default auto-save-last-location)
;;                         :hide-other (org-noter--property-or-default hide-other)
;;                         :closest-tipping-point (org-noter--property-or-default closest-tipping-point)
;;                         :modified-tick -1))

;;                       (target-location org-noter--start-location-override)
;;                       (starting-point (point)))

;;                  (add-hook 'delete-frame-functions 'org-noter--handle-delete-frame)
;;                  (push session org-noter--sessions)

;;                  (with-current-buffer document-buffer
;;                    (cond
;;                     ;; NOTE(nox): PDF Tools
;;                     ((eq document-major-mode 'pdf-view-mode)
;;                      (setq buffer-file-name document-path)
;;                      (pdf-view-mode)
;;                      (add-hook 'pdf-view-after-change-page-hook 'org-noter--doc-location-change-handler nil t))

;;                     ;; NOTE(nox): DocView
;;                     ((eq document-major-mode 'doc-view-mode)
;;                      (setq buffer-file-name document-path)
;;                      (doc-view-mode)
;;                      (advice-add 'doc-view-goto-page :after 'org-noter--location-change-advice))

;;                     ;; NOTE(nox): Nov.el
;;                     ((eq document-major-mode 'nov-mode)
;;                      (rename-buffer document-buffer-name)
;;                      (advice-add 'nov-render-document :after 'org-noter--nov-scroll-handler)
;;                      (add-hook 'window-scroll-functions 'org-noter--nov-scroll-handler nil t))

;;                     (t (error "This document handler is not supported :/")))

;;                    (org-noter-doc-mode 1)
;;                    (setq org-noter--session session)
;;                    (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t))

;;                  (with-current-buffer notes-buffer
;;                    (org-noter-notes-mode 1)
;;                    ;; NOTE(nox): This is needed because a session created in an indirect buffer would use the point of
;;                    ;; the base buffer (as this buffer is indirect to the base!)
;;                    (goto-char starting-point)
;;                    (setq buffer-file-name notes-file-path
;;                          org-noter--session session
;;                          (el-patch-remove fringe-indicator-alist '((truncation . nil)))
;;                          ;; fringe-indicator-alist '((truncation . nil))
;;                          )
;;                    (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t)
;;                    (add-hook 'window-scroll-functions 'org-noter--set-notes-scroll nil t)
;;                    (org-noter--set-text-properties (org-noter--parse-root (vector notes-buffer document-property-value))
;;                                                    (org-noter--session-id session))
;;                    (unless target-location
;;                      (setq target-location (org-noter--location-property (org-noter--get-containing-heading t)))))

;;                  (org-noter--setup-windows session)

;;                  ;; NOTE(nox): This timer is for preventing reflowing too soon.
;;                  (run-with-idle-timer
;;                   0.05 nil
;;                   (lambda ()
;;                     (with-current-buffer document-buffer
;;                       (let ((org-noter--inhibit-location-change-handler t))
;;                         (when target-location (org-noter--doc-goto-location target-location)))
;;                       (org-noter--doc-location-change-handler)))))))
;; -----------------------------------------------------------------------------

(define-key org-noter-doc-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(define-key org-noter-doc-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)

;;; Default function in org-noter.el
;; (defun org-noter--set-notes-scroll (window &rest ignored)
;;   (when window
;;     (with-selected-window window
;;       (org-noter--with-valid-session
;;        (let* ((level (org-noter--session-level session))
;;               (goal (* (1- level) 2))
;;               (current-scroll (window-hscroll)))
;;          (when (and (bound-and-true-p org-indent-mode) (< current-scroll goal))
;;            (scroll-right current-scroll)
;;            (scroll-left goal t)))))))

(defun zp/org-noter (arg)
  "Start org-noter session.

In org-agenda, visit the subtree first."
  (interactive "P")
  (if (derived-mode-p 'org-agenda-mode)
      (let ((marker (get-text-property (point) 'org-marker)))
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-noter arg)))
    (org-noter arg)))



;; ========================================
;; ============= HTML EXPORT ==============
;; ========================================

(setq org-html-postamble nil)
;; (setq html-validation-link nil)



;; ========================================
;; ================ SOUNDS ================
;; ========================================

;; Might have to simplify the code one day. Lots of repeat.

;; Windows legacy
;; (defun org-timer-done-sound ()
;;   (start-process-shell-command "play-sound" nil "sounder /id timer-sound /unique ../.emacs.d/sfx/timer.wav"))
;; (setq org-timer-done-hook 'org-timer-done-sound)

;; Max volume is 65536

;; (defun org-timer-done-sound ()
;;   (start-process-shell-command "play-sound" nil "foobar"))
;; (setq org-timer-done-hook 'org-timer-done-sound)

(defun zp/play-sound-clock-in ()
  (start-process-shell-command "play-sound" nil "notification-sound-org clock-in"))
(add-hook 'org-clock-in-prepare-hook 'zp/play-sound-clock-in)

(defun zp/play-sound-clock-out ()
  (start-process-shell-command "play-sound" nil "notification-sound-org clock-out"))
(add-hook 'org-clock-out-hook 'zp/play-sound-clock-out)

;;; Extra sounds

(defun zp/play-sound-reward ()
  (when (string-equal org-state "DONE")
    ;; (org-clock-out-if-current)               ;Default value
    (start-process-shell-command "play-sound" nil "notification-sound-org done")))
(add-hook 'org-after-todo-state-change-hook 'zp/play-sound-reward)

(defun zp/play-sound-start-capture ()
  (start-process-shell-command "play-sound" nil "notification-sound-org open"))
(add-hook 'org-capture-mode-hook 'zp/play-sound-start-capture)

(defun zp/play-sound-after-capture ()
  (start-process-shell-command "play-sound" nil "notification-sound-org close"))
(add-hook 'org-capture-after-finalize-hook 'zp/play-sound-after-capture)

(defun zp/play-sound-after-refile ()
  (start-process-shell-command "play-sound" nil "notification-sound-org move"))
(add-hook 'org-after-refile-insert-hook 'zp/play-sound-after-refile)

(defun zp/play-sound-turn-page ()
  (start-process-shell-command "play-sound" nil "notification-sound-org page"))

(defmacro zp/advise-commands (method commands where function)
  (let ((where-keyword (intern-soft (concat ":" (symbol-name where)))))
    `(progn
       ,@(cond ((string= method 'add)
                (mapcar (lambda (command)
                          `(advice-add ',command ,where-keyword #',function))
                        commands))
               ((string= method 'remove)
                (mapcar (lambda (command)
                          `(advice-remove ',command  ',function))
                        commands))))))

(defun zp/movement--play-sound-turn-page (orig-fun &rest args)
  (prog1
      (apply orig-fun args)
    (zp/play-sound-turn-page)))

(zp/advise-commands
 add
 (zp/org-overview
  zp/org-show-all
  zp/org-narrow-to-subtree
  zp/org-widen
  zp/org-narrow-forwards
  zp/org-narrow-backwards
  zp/org-narrow-up-heading
  zp/org-narrow-previous-heading
  zp/org-refile
  zp/org-refile-to
  zp/org-refile-main
  zp/org-kill-indirect-buffer
  zp/org-kill-indirect-buffer-and-window
  zp/org-agenda-tree-to-indirect-buffer
  zp/LaTeX-narrow-to-environment
  zp/LaTeX-widen
  zp/LaTeX-narrow-forwards
  zp/LaTeX-narrow-backwards
  zp/LaTeX-narrow-up)
 around
 zp/movement--play-sound-turn-page)



;; ========================================
;; =============== ORG-REF ================
;; ========================================

(require 'org-ref)

(setq org-ref-bibliography-notes "/home/zaeph/org/bib/notes.org"
      reftex-default-bibliography '("/home/zaeph/org/bib/monty-python.bib")
      org-ref-default-bibliography '("/home/zaeph/org/bib/monty-python.bib")
      org-ref-pdf-directory "/home/zaeph/org/bib/pdf")



;; ========================================
;; ============= HELM BIBTEX ==============
;; ========================================

(require 'helm-bibtex)



;; ------------------------------------------------------------------------------
;; helm-bibtex-select-bib

(defvar zp/bibtex-completion-bib-data-alist nil
  "Alist of the bibliography files and their labels.")

(defvar zp/bibtex-completion-bib-data nil
  "Processed alist of the bibliography files and their labels,
  including an entry with all of them.")

(defun zp/bibtex-completion-bib-data-format ()
  (interactive)
  (setq zp/bibtex-completion-bib-data zp/bibtex-completion-bib-data-alist)
  (map-put zp/bibtex-completion-bib-data
           "All entries" (list (mapcar 'cdr zp/bibtex-completion-bib-data))))

(defun zp/bibtex-select-bib-init ()
  (zp/bibtex-completion-bib-data-format)
  (setq bibtex-completion-bibliography
        (cdr (assoc "All entries" zp/bibtex-completion-bib-data))))

(defun zp/bibtex-select-bib-select (candidate)
  (setq bibtex-completion-bibliography candidate
        reftex-default-bibliography candidate
        org-ref-default-bibliography (list candidate)))

(defun zp/bibtex-select-bib-select-open (candidate)
  (zp/bibtex-select-bib-select candidate)
  (helm-bibtex))

(setq zp/bibtex-completion-select-bib-actions
      '(("Open bibliography" . zp/bibtex-select-bib-select-open)
        ("Select bibliography" . zp/bibtex-select-bib-select)))

(setq zp/helm-source-bibtex-select-bib
      '((name . "*HELM Bibtex - Bibliography selection*")
        (candidates . zp/bibtex-completion-bib-data)
        (action . zp/bibtex-completion-select-bib-actions)))

(defun zp/helm-bibtex-select-bib (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (progn
        ;; Refresh reftex if inside AUCTeX
        (when (derived-mode-p 'latex-mode)
          (reftex-reset-mode))
        ;; Refresh org-ref
        (setq org-ref-bibliography-files nil)
        (zp/bibtex-select-bib-init)))
  (helm :sources '(zp/helm-source-bibtex-select-bib)))
;; ------------------------------------------------------------------------------



(setq zp/bibtex-completion-bib-data-alist
      '(("Monty Python" . "/home/zaeph/org/bib/monty-python.bib")
        ;; ("Monty Python - Extra" . "/home/zaeph/org/bib/monty-python-extra.bib")
        ("FromSoftware" . "/home/zaeph/org/bib/fromsoftware.bib")))

(zp/bibtex-select-bib-init)

;; Autokey generation
(setq bibtex-align-at-equal-sign t
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-year-length 4
      bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an")
      bibtex-autokey-titleword-length 20
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titlewords 0)

(setq bibtex-completion-pdf-field "file")

(setq bibtex-completion-pdf-symbol "P"
      bibtex-completion-notes-symbol "N")

;; Set default dialect to biblatex
(setq bibtex-dialect 'biblatex)

;; Additional fields
(setq bibtex-user-optional-fields '(("subtitle" "Subtitle")
                                    ("booksubtitle" "Book subtitle")
                                    ("langid" "Language to use with BibLaTeX")
                                    ("library" "Library where the resource is held")
                                    ("shelf" "Shelf number at the library")
                                    ("annote" "Personal annotation (ignored)")
                                    ("keywords" "Personal keywords")
                                    ("tags" "Personal tags")
                                    ("file" "Path to file")
                                    ("url" "URL to reference"))

      helm-bibtex-additional-search-fields '(subtitle booksubtitle keywords tags library))

(define-key bibtex-mode-map (kbd "C-c M-o") 'bibtex-Online)

;; Define which citation function to use on a buffer basis
(setq bibtex-completion-format-citation-functions
      '(;; (org-mode      . org-ref-bibtex-completion-format-org)
        (org-mode      . org-ref-format-citation)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (bibtex-mode   . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))

;; Default citation command
(setq bibtex-completion-cite-default-command "autocite")

;; PDF open function
(setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)

;; Helm
(defun zp/helm-bibtex-with-local-bibliography (&optional arg)
  "Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (let* ((local-bib-org org-ref-bibliography-files)
         (local-bib (or (bibtex-completion-find-local-bibliography)
                        (if (cl-every 'file-exists-p local-bib-org)
                            local-bib-org)))
         (bibtex-completion-bibliography (or local-bib
                                             bibtex-completion-bibliography)))
    (helm-bibtex arg local-bib)))



;; Custom action: Select current document

;; (defvar zp/current-document)
;; (defun zp/select-current-document ()
;;   (interactive)
;;   (setq zp/current-document
;;      (read-string
;;       (concat "Current document(s)"
;;               (if (bound-and-true-p zp/current-document)
;;                   (concat " [" zp/current-document "]"))
;;               ": "))))

;; (defun zp/bibtex-completion-select-current-document (keys)
;;   (setq zp/current-document (s-join ", " keys)))
;; (helm-bibtex-helmify-action zp/bibtex-completion-select-current-document helm-bibtex-select-current-document)



(defvar zp/bibtex-completion-key-last nil
  "Last inserted keys.")

(defun zp/bibtex-completion-format-citation-comma (keys)
  "Default formatter for keys, separates multiple keys with
commas."
  (s-join "," keys))

(defun zp/bibtex-completion-format-citation-comma-space (keys)
  "Formatter for keys, separates multiple keys with
commas and space."
  (s-join ", " keys))

(defun zp/bibtex-completion-insert-key (keys)
  "Insert BibTeX key at point."
  (let ((current-keys (zp/bibtex-completion-format-citation-comma-space keys)))
    (insert current-keys)
    (setq zp/bibtex-completion-key-last keys)))

(defun zp/bibtex-completion-insert-key-last ()
  (interactive)
  (let ((last-keys (zp/bibtex-completion-format-citation-default
                    zp/bibtex-completion-key-last)))
    (if (bound-and-true-p last-keys)
        (insert last-keys)
      (zp/helm-bibtex-solo-action-insert-key))))

(defun zp/bibtex-completion-message-key-last ()
  (interactive)
  (let ((keys (zp/bibtex-completion-format-citation-comma-space
               zp/bibtex-completion-key-last)))
    (if (bound-and-true-p keys)
        (message (concat "Last key(s) used: " keys "."))
      (message "No previous key used."))))

;; Add to helm
(helm-bibtex-helmify-action zp/bibtex-completion-insert-key zp/helm-bibtex-insert-key)
(helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
(helm-add-action-to-source "Insert BibTeX key" 'zp/helm-bibtex-insert-key helm-source-bibtex 4)

;; Define solo action: Insert BibTeX key
(setq zp/helm-source-bibtex-insert-key '(("Insert BibTeX key" . zp/helm-bibtex-insert-key)))
(defun zp/helm-bibtex-solo-action-insert-key ()
  (interactive)
  (let ((inhibit-message t)
        (previous-actions (helm-attr 'action helm-source-bibtex))
        (new-action zp/helm-source-bibtex-insert-key))
    (helm-attrset 'action new-action helm-source-bibtex)
    (helm-bibtex)
    ;; Wrapping with (progn (foo) nil) suppress the output
    (progn (helm-attrset 'action previous-actions helm-source-bibtex) nil)))





;; OLD CONFIGURATION
;; DISABLED
;; Uncomment entire block to resume function

;; (setq bibtex-completion-pdf-symbol "P"
;;       bibtex-completion-notes-symbol "N")

;; ;; (setq bibtex-completion-bibliography '("/home/zaeph/org/uni/phonology/refs.bib"
;; ;;                                  "/home/zaeph/org/uni/civ/refs.bib"))

;; ;; Obsolete with `helm-bibtex-switch'
;; ;; (setq helm-bibtex-pdf-open-function 'org-open-file
;; ;;       helm-bibtex-full-frame nil)

;; ;; Autokey generation
;; (setq bibtex-align-at-equal-sign t
;;       bibtex-autokey-name-year-separator ""
;;       bibtex-autokey-year-title-separator ""
;;       bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an")
;;       bibtex-autokey-titleword-length 20
;;       bibtex-autokey-titlewords-stretch 0
;;       bibtex-autokey-titlewords 1)

;; ;; Additional fields
;; (setq helm-bibtex-additional-search-fields '(keywords tags library)
;;       bibtex-user-optional-fields '(("langid" "Language to use with BibLaTeX")
;;                                  ("library" "Library where the resource is held")
;;                                  ("shelf" "Shelf number at the library")
;;                                  ("annote" "Personal annotation (ignored)")
;;                                  ("keywords" "Personal keywords")
;;                                  ("tags" "Personal tags")))

;; ;; Define which citation function to use on a buffer basis
;; (setq bibtex-completion-format-citation-functions
;;       '((org-mode      . bibtex-completion-format-citation-)
;;      (latex-mode    . bibtex-completion-format-citation-cite)
;;      (bibtex-mode   . bibtex-completion-format-citation-cite)
;;      (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;;      (default       . bibtex-completion-format-citation-default)))

;; ;; Helm source
;; (defun bibtex-completion-insert-org-link (_)
;;   "Inserts an org-link to the marked entries."
;;   (let ((keys (helm-marked-candidates :with-wildcard t))
;;         (format-function 'bibtex-completion-format-citation-org-link-to-PDF))
;;     (with-helm-current-buffer
;;       (insert
;;        (funcall format-function keys)))))

;; (defun bibtex-completion-open-pdf-with-org-open-file (candidates)
;;   "Open the PDFs associated with the marked entries using the
;; function specified in `bibtex-completion-pdf-open-function'.  All paths
;; in `bibtex-completion-library-path' are searched.  If there are several
;; matching PDFs for an entry, the first is opened."
;;   (--if-let
;;       (-flatten
;;        (-map 'bibtex-completion-find-pdf
;;              (if (listp candidates) candidates (list candidates))))
;;       (-each it 'org-open-file)
;;     (message "No PDF(s) found.")))

;; (defun bibtex-completion-open-pdf-with-find-file (candidates)
;;   "Open the PDFs associated with the marked entries using the
;; function specified in `bibtex-completion-pdf-open-function'.  All paths
;; in `bibtex-completion-library-path' are searched.  If there are several
;; matching PDFs for an entry, the first is opened."
;;   (--if-let
;;       (-flatten
;;        (-map 'bibtex-completion-find-pdf
;;              (if (listp candidates) candidates (list candidates))))
;;       (-each it 'find-file)
;;     (message "No PDF(s) found.")))

;; (setq helm-source-bibtex
;;   '((name                                      . "BibTeX entries")
;;     (init                                      . bibtex-completion-init)
;;     (candidates                                . bibtex-completion-candidates)
;;     (filtered-candidate-transformer            . helm-bibtex-candidates-formatter) ;helm-bibtex-candidates-formatter doesn't seem to work for some reason
;;     (action . (("Open PDF file externally (if present)" . bibtex-completion-open-pdf-with-org-open-file)
;;             ("Insert citation"                       . bibtex-completion-insert-citation)
;;             ("Insert org-link to PDF"                . bibtex-completion-insert-org-link)
;;                ("Edit notes"                            . bibtex-completion-edit-notes)
;;                ("Show entry"                            . bibtex-completion-show-entry)
;;                ("Insert BibTeX key"                     . bibtex-completion-insert-key)
;;                ("Insert BibTeX entry"                   . bibtex-completion-insert-bibtex)
;;             ("Open PDF file internally (if present)" . bibtex-completion-open-pdf-with-find-file)
;;             ("Open URL or DOI in browser"            . bibtex-completion-open-url-or-doi)
;;             ("Insert reference"                      . bibtex-completion-insert-reference)
;;                ("Attach PDF to email"                   . bibtex-completion-add-PDF-attachment)))))

;; ;; bib files
;; (setq bib-data '(("All" . ("/home/zaeph/org/old/uni/phonology/refs.bib"
;;                         "/home/zaeph/org/old/uni/civilisation/refs.bib"
;;                         "/home/zaeph/org/old/uni/physics/refs.bib"))
;;               ("Phonology" . "/home/zaeph/org/old/uni/phonology/refs.bib")
;;               ("Civilisation" . "/home/zaeph/org/old/uni/civilisation/refs.bib")
;;               ("Physics" . "/home/zaeph/org/old/uni/physics/refs.bib")
;;               ("Labour" . "/home/zaeph/org/projects/university/courses/civilisation/refs.bib")))

;; ;; Folders where the PDFs and notes are
;; (setq bibtex-completion-notes-path "/home/zaeph/org/refs/notes.org"
;;       bibtex-completion-library-path '("/home/zaeph/../pdfs/phonology"
;;                                     "/home/zaeph/../pdfs/civilisation"
;;                                     "k:/pdfs/phonology"
;;                                     "k:/pdfs/civilisation"))

;; ;; Init on all entries
;; (defun bibtex-completion-default ()
;;   "Sets the default bibliography and buffer-name to be used by helm-bibtex."
;;   (setq bibtex-completion-bibliography (cdr (assoc "All" bib-data))
;;      bibtex-completion-buffer "*Helm BibTeX* All"))

;; (bibtex-completion-default)

;; (defun bibtex-completion-open (candidate &optional arg)
;;   (bibtex-completion-switch arg
;;                    :bib (cdr (assoc candidate bib-data))
;;                    :buffer (concat "*Helm BibTeX* " (car (assoc candidate bib-data)))))
;; (defun bibtex-completion-open-full-frame (candidate &optional arg)
;;   (bibtex-completion-switch arg
;;                    :bib (cdr (assoc candidate bib-data))
;;                    :buffer (concat "*Helm BibTeX* " (car (assoc candidate bib-data)))
;;                    :full-frame t))

;; (cl-defun bibtex-completion-switch (&optional arg &key bib buffer (full-frame nil) (candidate-number-limit 500))
;;   "Search BibTeX entries."
;;   (interactive)
;;   (setq bibtex-completion-bibliography bib)
;;   (setq bibtex-completion-buffer buffer)
;;   (when (eq arg 4)
;;     (setq bibtex-completion-bibliography-hash ""))
;;   (when (eq arg 16)
;;     (bibtex-completion-default))
;;   (helm :sources '(helm-source-bibtex helm-source-fallback-options)
;;      :full-frame full-frame
;;      :buffer bibtex-completion-buffer
;;      :candidate-number-limit candidate-number-limit))

;; (setq bibtex-completion-sources
;;       `((name . "*Helm BibTeX* Sources")
;;         (candidates . ,(mapcar 'car bib-data))
;;         (action
;;       ("Load bibliography" . bibtex-completion-open)
;;       ("Load bibliography (full window)" . bibtex-completion-open-full-frame)
;;       ("Reload bibliography"))))

;; (global-set-key (kbd "C-c b") (lambda (&optional arg)
;;                              (interactive "p")
;;                              (bibtex-completion-switch arg
;;                                                  :bib bibtex-completion-bibliography
;;                                                  :buffer bibtex-completion-buffer
;;                                                  :full-frame nil)))
;; ;; (global-set-key (kbd "C-c B") (lambda (&optional arg)
;; ;;                           (interactive "p")
;; ;;                           (bibtex-completion-switch arg
;; ;;                                               :bib bibtex-completion-bibliography
;; ;;                                               :buffer bibtex-completion-buffer
;; ;;                                               :full-frame t)))
;; (global-set-key (kbd "C-c B") (lambda ()
;;                              (interactive)
;;                              (helm :sources bibtex-completion-sources)))



;; ========================================
;; ============ EXPERIMENTAL ==============
;; ========================================

;; (require 'notifications)
;; (notifications-notify :title "Achtung!"
;;                       :body (format "You have an appointment in %d minutes" 10)
;;                       :app-name "Emacs: Org"
;;                    :urgency "critical"
;;                       :sound-name "/home/zaeph/SFX/Misc/rimshot.mp3")

;; Source: https://gitlab.com/marcowahl/herald-the-mode-lined
(defun herald-the-mode-line ()
  "Show the modeline in the minibuffer.
Use case: when the modeline is to short for its content this
command reveals the other lines."
  (interactive)
  (message
   "%s"
   (format-mode-line
    (or mode-line-format
        hide-mode-line))))



;; ========================================
;; ============== EXTERNAL ================
;; ========================================

;;; Obsolete, but useful for creating toggles
;; (defun clean-mode ()
;;   "Removes scroll bars.
;; Generates flicker on certain elements (e.g. linum)."
;;   (interactive)
;;   (if (bound-and-true-p scroll-bar-mode)
;;       (scroll-bar-mode -1)
;;     (scroll-bar-mode 1)))

;; (defun org-mode-enhanced-reading ()
;;   "Activate both org-indent-mode and visual-line-mode."
;;   (interactive)
;;   (if (bound-and-true-p org-indent-mode)
;;       (progn
;;         (org-indent-mode -1))
;;     (progn
;;       (org-indent-mode t))))



;;; Not sure what it does anymore... ?
;; (defun push-mark-no-activate ()
;;   "Pushes `point' to `mark-ring' and does not activate the region
;;    Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
;;   (interactive)
;;   (push-mark (point) t nil)
;;   (message "Pushed mark to ring"))

;; (global-set-key (kbd "C-`") 'push-mark-no-activate)

;; (defun jump-to-mark ()
;;   "Jumps to the local mark, respecting the `mark-ring' order.
;;   This is the same as using \\[set-mark-command] with the prefix argument."
;;   (interactive)
;;   (set-mark-command 1))
;; (global-set-key (kbd "M-`") 'jump-to-mark)



;; Increment/Decrement integer at point
(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))(require 'thingatpt)

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))



(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))




;; (defun zp/switch-to-agenda ()
;;   (interactive)
;;   (if (string-match ".*Org Agenda.*" (buffer-name))
;;       (mode-line-other-buffer)
;;     (switch-to-buffer "*Org Agenda(n)*")))

(defun zp/create-agenda-view (arg)
  (interactive "P")
  (if (get-buffer "*Org Agenda(l)*")
      (switch-to-buffer "*Org Agenda(l)*")
    (org-agenda arg "l"))
  (delete-other-windows)
  (split-window-right)
  (if (get-buffer "*Org Agenda(n)*")
      (switch-to-buffer "*Org Agenda(n)*")
    (org-agenda arg "n"))
  (other-window 1)
  (balance-windows))

(defun zp/switch-to-agenda (arg)
  "Toggles a custom org-agenda context, or creates it if it doesn’t
  exist.

The layout of the org-agenda context is stored as a frame
parameter, which implies that each frame can have its own
independent org-agenda context.

With a ‘C-u’ prefix argument, forces the re-creation of the
org-agenda context."

  (interactive "P")
  (let* ((current-config (current-window-configuration))
         (current-frame (window-configuration-frame current-config)))

    (cond ((and
            (frame-parameter current-frame
                             'zp/org-agenda-session-p)
            (not arg))
           (set-frame-parameter current-frame
                                'zp/org-agenda-window-config
                                current-config)
           (set-window-configuration
            (frame-parameter current-frame
                             'zp/org-agenda-window-config-before))
           (set-frame-parameter current-frame
                                'zp/org-agenda-session-p nil))

          (t
           (set-frame-parameter current-frame
                                'zp/org-agenda-window-config-before
                                current-config)
           (if (or arg
                   (not (frame-parameter current-frame
                                         'zp/org-agenda-window-config)))
               (zp/create-agenda-view arg)
             (set-window-configuration
              (frame-parameter current-frame
                               'zp/org-agenda-window-config)))
           (set-frame-parameter current-frame
                                'zp/org-agenda-session-p t)))))



(defun zp/switch-to-chronos (add)
  "Switch to and from Chronos’s buffer.

If ADD is non-nil, prompt for a new timer upon switching."
  (interactive "P")
  (cond ((string-match "*chronos*" (buffer-name))
         (zp/chronos-quit))
        ((get-buffer "*chronos*")
         (switch-to-buffer "*chronos*")
         (when add
           (helm-chronos-add-timer)))
        (t
         (chronos-initialize))))

(defun zp/switch-to-chronos-and-add ()
  "Switch to and from Chronos’s buffer.

If switching to Chronos’s buffer, also add a timer."
  (interactive)
  (zp/switch-to-chronos t))

;; (defun zp/switch-to-magit (arg)
;;   (interactive "P")
;;   (if (string-match "magit: .*" (buffer-name))
;;       (magit-mode-bury-buffer arg)
;;     (magit-status arg)))

(defun zp/switch-to-mu4e ()
  (interactive)
  (if (string-match "*mu4e-.*" (buffer-name))
      (progn
        (mu4e-quit)
        (set-window-configuration zp/mu4e-before-config))
    (if (eq (get-buffer "*mu4e-main*") nil)
        (progn
          (setq zp/mu4e-before-config (current-window-configuration))
          (mu4e)))))

(defun zp/notmuch-hello-quit ()
  (interactive)
  (notmuch-bury-or-kill-this-buffer)
  (start-process-shell-command "notmuch-new" nil "systemctl --user start check-mail.service")
  (set-window-configuration zp/notmuch-before-config))

(defun zp/switch-to-notmuch ()
  (interactive)
  (cond ((string-match "\\*notmuch-hello\\*" (buffer-name))
         (zp/notmuch-hello-quit))
        ((string-match "\\*notmuch-.*\\*" (buffer-name))
         (notmuch-bury-or-kill-this-buffer))
        (t
         (setq zp/notmuch-before-config (current-window-configuration))
         (delete-other-windows)
         (notmuch))))

(advice-add #'mu4e-quit :after (lambda ()
                                 (mu4e-update-mail-and-index t)))

(defun zp/mu4e-view-message-with-message-id-save-window-config-before (old-function &rest arguments)
  (interactive)
  (setq zp/mu4e-before-config (current-window-configuration))
  (apply old-function arguments))

(advice-add #'mu4e-view-message-with-message-id :around #'zp/mu4e-view-message-with-message-id-save-window-config-before)

(defun zp/echo-buffer-name ()
  (interactive)
  (message (concat "Current buffer: " (replace-regexp-in-string "%" "%%" (buffer-name)))))

(require 'org-clock)

(defun zp/echo-clock-string ()
  "Echo the tasks being currently clocked in the minibuffer,
along with effort estimates and total time."
  (interactive)
  (if (org-clocking-p)
      (let ((header "Current clock")
            (clocked-time (org-clock-get-clocked-time))
            (org-clock-heading-formatted (replace-regexp-in-string "%" "%%"org-clock-heading)))
        (if org-clock-effort
            (let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
                   (work-done-str
                    (propertize (org-duration-from-minutes clocked-time)
                                'face
                                (if (and org-clock-task-overrun
                                         (not org-clock-task-overrun-text))
                                    'org-mode-line-clock-overrun
                                  'org-meta-line)))
                   (effort-str (org-duration-from-minutes effort-in-minutes)))
              (message (concat
                        header ": "
                        (format (propertize "[%s/%s] (%s)" 'face 'org-meta-line)
                                work-done-str effort-str org-clock-heading-formatted))))
          (message (concat
                    header ": "
                    (format (propertize "[%s] (%s)" 'face 'org-meta-line)
                           (org-duration-from-minutes clocked-time)
                           org-clock-heading-formatted)))))
    (error "Not currently clocking any task.")))



;; Tag skip
;; Not needed at the moment, but might come in handy when trying to
;; write a single skip-function for my org-agenda blocks.
;; (defun zp/org-agenda-skip-tag (tag &optional others)
;;   "Skip all entries that correspond to TAG.

;; If OTHERS is true, skip all entries that do not correspond to TAG."
;;   (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
;;         (current-headline (or (and (org-at-heading-p)
;;                                    (point))
;;                               (save-excursion (org-back-to-heading)))))
;;     (if others
;;         (if (not (member tag (org-get-tags-at current-headline)))
;;             next-headline
;;           nil)
;;       (if (member tag (org-get-tags-at current-headline))
;;           next-headline
;;         nil))))



;; Norang
;; To study in depth to master org-agenda

(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHD NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))


(defun bh/skip-stuck-projects ()
  "Skip trees that are stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun zp/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (if zp/projects-include-waiting
                              (re-search-forward "^\\*+ \\(NEXT\\|STRT\\) " subtree-end t)
                            (re-search-forward "^\\*+ \\(NEXT\\|STRT\\|WAIT\\) " subtree-end t)))
                (unless (member "standby" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defvar zp/projects-include-waiting nil
  "When t, includes stuck projects with a waiting task in the
agenda.")

(defun zp/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (is-waiting (string-match-p "WAIT" (org-get-todo-state)))
                 (has-next))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (if is-waiting
                              (re-search-forward "^\\*+ \\(WAIT\\) " subtree-end t)
                            (re-search-forward "^\\*+ \\(NEXT\\|STRT\\) " subtree-end t)))
                (unless (member "standby" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun zp/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (zp/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun zp/skip-non-unstuck-projects ()
  "Skip trees that are not unstuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (zp/skip-non-stuck-projects))
      (zp/skip-non-projects)
    (save-excursion (org-end-of-subtree t))))

(defun zp/skip-non-unstuck-projects-and-waiting ()
  (or
   (zp/skip-non-projects)
   ;; (zp/skip-non-unstuck-projects)
   (if (not zp/projects-include-waiting)
      (org-agenda-skip-entry-if 'todo '("WAIT")))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun zp/skip-non-tasks-and-scheduled ()
  (or
   (bh/skip-non-tasks)
   (org-agenda-skip-entry-if 'scheduled)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun zp/org-task-in-agenda-groups-p (groups &optional match-groupless pom)
  "Test whether a task is in agenda-group matched by GROUPS.

GROUPS can be a list or a regex.

If MATCH-GROUPLESS is non-nil, returns -1 when a task doesn’t have
a group."
  (let ((groups-regex
         (if (listp groups)
             (zp/org-agenda-groups-format-regex groups)
           groups)))
    (save-restriction
      (widen)
      (let ((task-group (org-entry-get (or pom (point))
                                       "AGENDA_GROUP"
                                       'selective)))
        (cond (task-group
               (string-match-p groups-regex task-group))
              (match-groupless
               -1))))))

(defun zp/skip-tasks-not-belonging-to-agenda-groups (groups &optional exhaustive)
  "Skip tasks if they aren’t part of GROUPS.

GROUPS is a list of AGENDA_GROUPS values to match.

If EXHAUSTIVE is non-nil, the function will not skip groupless
trees."
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion
                            (or (outline-next-heading)
                                (point-max))))
           (groups-regex (zp/org-agenda-groups-format-regex groups))
           (property "AGENDA_GROUP")
           (property-regex (concat "^:" property ":.*"))
           (include-groupless-p (or exhaustive
                                    (member nil groups))))
      (save-excursion
        (cond
          ((zp/org-task-in-agenda-groups-p groups-regex include-groupless-p)
           nil)
          ((and include-groupless-p
                (catch 'found-next
                  (while (re-search-backward (concat property-regex
                                                     ".*$")
                                             nil t)
                    (if (org-entry-get (point) property)
                        (throw 'found-next 't)))))
           (outline-get-next-sibling))
          ((catch 'found-next
             (goto-char next-headline)
             (while (re-search-forward (concat property-regex
                                               "\\("
                                               groups-regex
                                               "\\).*$")
                                       nil t)
               (if (org-entry-get (point) property)
                   (throw 'found-next 't))))
           (outline-previous-heading))
          (t
           (goto-char (point-max))))))))

;; 18.2.1 Narrowing to a subtree with bh/org-todo
;; (global-set-key (kbd "<f5>") 'bh/org-todo)

;; (defun bh/org-todo (arg)
;;   (interactive "p")
;;   (if (equal arg 4)
;;       (save-restriction
;;         (bh/narrow-to-org-subtree)
;;         (org-show-todo-tree nil))
;;     (bh/narrow-to-org-subtree)
;;     (org-show-todo-tree nil)))

;; (global-set-key (kbd "<S-f5>") 'bh/widen)

;; (defun bh/widen ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (progn
;;         (org-agenda-remove-restriction-lock)
;;         (when org-agenda-sticky
;;           (org-agenda-redo)))
;;     (widen)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
;;           'append)

;; (defun bh/restrict-to-file-or-follow (arg)
;;   "Set agenda restriction to 'file or with argument invoke follow mode.
;; I don't use follow mode very often but I restrict to file all the time
;; so change the default 'F' binding in the agenda to allow both"
;;   (interactive "p")
;;   (if (equal arg 4)
;;       (org-agenda-follow-mode)
;;     (widen)
;;     (bh/set-agenda-restriction-lock 4)
;;     (org-agenda-redo)
;;     (beginning-of-buffer)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defnkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
;;           'append)

;; (defun bh/narrow-to-org-subtree ()
;;   (widen)
;;   (org-narrow-to-subtree)
;;   (save-restriction
;;     (org-agenda-set-restriction-lock)))

;; (defun bh/narrow-to-subtree ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (progn
;;         (org-with-point-at (org-get-at-bol 'org-hd-marker)
;;           (bh/narrow-to-org-subtree))
;;         (when org-agenda-sticky
;;           (org-agenda-redo)))
;;     (bh/narrow-to-org-subtree)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
;;           'append)

;; (defun bh/narrow-up-one-org-level ()
;;   (widen)
;;   (save-excursion
;;     (outline-up-heading 1 'invisible-ok)
;;     (bh/narrow-to-org-subtree)))

;; (defun bh/get-pom-from-agenda-restriction-or-point ()
;;   (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
;;       (org-get-at-bol 'org-hd-marker)
;;       (and (equal major-mode 'org-mode) (point))
;;       org-clock-marker))

;; (defun bh/narrow-up-one-level ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (progn
;;         (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
;;           (bh/narrow-up-one-org-level))
;;         (org-agenda-redo))
;;     (bh/narrow-up-one-org-level)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
;;           'append)

;; (defun bh/narrow-to-org-project ()
;;   (widen)
;;   (save-excursion
;;     (bh/find-project-task)
;;     (bh/narrow-to-org-subtree)))

;; (defun bh/narrow-to-project ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (progn
;;         (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
;;           (bh/narrow-to-org-project)
;;           (save-excursion
;;             (bh/find-project-task)
;;             (org-agenda-set-restriction-lock)))
;;         (org-agenda-redo)
;;         (beginning-of-buffer))
;;     (bh/narrow-to-org-project)
;;     (save-restriction
;;       (org-agenda-set-restriction-lock))))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
;;           'append)

;; (defvar bh/project-list nil)

;; (defun bh/view-next-project ()
;;   (interactive)
;;   (let (num-project-left current-project)
;;     (unless (marker-position org-agenda-restrict-begin)
;;       (goto-char (point-min))
;;       ; Clear all of the existing markers on the list
;;       (while bh/project-list
;;         (set-marker (pop bh/project-list) nil))
;;       (re-search-forward "Tasks to Refile")
;;       (forward-visible-line 1))

;;     ; Build a new project marker list
;;     (unless bh/project-list
;;       (while (< (point) (point-max))
;;         (while (and (< (point) (point-max))
;;                     (or (not (org-get-at-bol 'org-hd-marker))
;;                         (org-with-point-at (org-get-at-bol 'org-hd-marker)
;;                           (or (not (bh/is-project-p))
;;                               (bh/is-project-subtree-p)))))
;;           (forward-visible-line 1))
;;         (when (< (point) (point-max))
;;           (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
;;         (forward-visible-line 1)))

;;     ; Pop off the first marker on the list and display
;;     (setq current-project (pop bh/project-list))
;;     (when current-project
;;       (org-with-point-at current-project
;;         (setq bh/hide-scheduled-and-waiting-next-tasks nil)
;;         (bh/narrow-to-project))
;;       ; Remove the marker
;;       (setq current-project nil)
;;       (org-agenda-redo)
;;       (beginning-of-buffer)
;;       (setq num-projects-left (length bh/project-list))
;;       (if (> num-projects-left 0)
;;           (message "%s projects left to view" num-projects-left)
;;         (beginning-of-buffer)
;;         (setq bh/hide-scheduled-and-waiting-next-tasks t)
;;         (error "All projects viewed.")))))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
;;           'append)




;; Modifying faces for bulk-mark in org-agenda.el
;; Can't believe that actually worked

(defun org-agenda-bulk-mark (&optional arg)
  "Mark the entry at point for future bulk action."
  (interactive "p")
  (dotimes (i (or arg 1))
    (unless (org-get-at-bol 'org-agenda-diary-link)
      (let* ((m (org-get-at-bol 'org-hd-marker))
             ov)
        (unless (org-agenda-bulk-marked-p)
          (unless m (user-error "Nothing to mark at point"))
          (push m org-agenda-bulk-marked-entries)
          (setq ov (make-overlay (point-at-bol) (+ 2 (point-at-bol))))
          (org-overlay-display ov (concat org-agenda-bulk-mark-char " ")
                               ;; (org-get-todo-face "TODO")
                               'org-todo                                  ;Modification
                               'evaporate)
          (overlay-put ov 'type 'org-marked-entry-overlay))
        (end-of-line 1)
        (or (ignore-errors
              (goto-char (next-single-property-change (point) 'org-hd-marker)))
            (beginning-of-line 2))
        (while (and (get-char-property (point) 'invisible) (not (eobp)))
          (beginning-of-line 2))
        (message "%d entries marked for bulk action"
                 (length org-agenda-bulk-marked-entries))))))

;; Copy file path

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-08-25"
  (interactive "P")
  (let (($fpath
         (if (equal major-mode 'dired-mode)
             (progn
               (mapconcat 'identity (dired-get-marked-files) "\n"))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))



;; Add ‘CREATED’ property to all captured items

(defvar org-created-property-name "CREATED"
  "The name of the org-mode property that stores the creation date of the entry")

(defun zp/org-set-created-property (&optional active NAME)
  "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
  (interactive)
  (let* ((created (or NAME org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M")))
         (is-capturing (and (boundp 'org-capture-mode) org-capture-mode))
         (add-created (plist-get org-capture-plist :add-created)))
    (unless (or (and is-capturing
                     (not add-created))
                (org-entry-get (point) created nil))
      (when is-capturing
        (goto-char (point-min)))
      (org-set-property created now))))

(add-hook 'org-capture-prepare-finalize-hook #'zp/org-set-created-property)

;; Align tags in templates before finalising
(add-hook 'org-capture-before-finalize-hook #'org-align-all-tags)

;; Conditional APPT_WARNTIME
(defun zp/org-set-appt-warntime-if-timestamp ()
  "Prompt for APPT_WARNTIME if the heading as a timestamp."
  (let ((warntime (org-entry-get (point) "APPT_WARNTIME")))
    (unless warntime
      (save-excursion
        (org-back-to-heading t)
        (let ((end (save-excursion (outline-next-heading) (point))) ts)
          (when (re-search-forward org-stamp-time-of-day-regexp
                                   end t)
            (zp/org-set-appt-warntime)))))))

(defadvice org-insert-time-stamp (after add-appt activate)
  (zp/org-set-appt-warntime-if-timestamp))



;; Ediff in dired
;; https://oremacs.com/2017/03/18/dired-ediff/
;; (require 'dired)
;; (setq dired-dwim-target t)
;; (defun ora-ediff-files ()
;;   (interactive)
;;   (let ((files (dired-get-marked-files))
;;         (wnd (current-window-configuration)))
;;     (if (<= (length files) 2)
;;         (let ((file1 (car files))
;;               (file2 (if (cdr files)
;;                          (cadr files)
;;                        (read-file-name
;;                         "file: "
;;                         (dired-dwim-target-directory)))))
;;           (if (file-newer-than-file-p file1 file2)
;;               (ediff-files file2 file1)
;;             (ediff-files file1 file2))
;;           (add-hook 'ediff-after-quit-hook-internal
;;                     (lambda ()
;;                       (setq ediff-after-quit-hook-internal nil)
;;                       (set-window-configuration wnd))))
;;       (error "no more than 2 files should be marked"))))

;; (define-key dired-mode-map "e" 'ora-ediff-files)



;; ========================================
;; ============ PSYCHOTHERAPY =============
;; ========================================

;;; helm-smbp
;;; Helm function for Setting Multiple Boolean Properties (SMBP)

;; Helper function
(defun zp/make-property-alist (name list)
  "Create property ALIST from LIST designated by SYMBOL.

SYMBOL points to a LIST.

Every ELEM in LIST is formatted as follows:
- Add the name portion of SYMBOL as a prefix
- Make the string uppercase
- Replace spaces and hyphens with underscores"
  (let* ((alist))

    (dolist (var list)
      (add-to-list
       'alist
       ;; Replace spaces and hyphens with underscores
       (cons var (replace-regexp-in-string "-\\| " "_" (upcase (concat name "-" var))))
       t))
    alist))


;; Helm definition
(defun zp/helm-smbp-set-property (alist candidate)
  (save-excursion
    (goto-char (org-end-of-subtree))
    (newline)
    (insert "- " (car (rassoc candidate alist))))
  (org-set-property candidate "t")
  (org-cycle nil)
  (org-cycle nil))

(defun zp/helm-smbp-set-properties (alist)
  (save-excursion
    (org-mark-subtree)
    (forward-line)
    (delete-region (region-beginning) (region-end)))
  (mapc (lambda (arg)
          (zp/helm-smbp-set-property alist arg))
        (helm-marked-candidates)))

(defun zp/helm-smbp (symbol)
  (interactive)
  (let* ((symbol-name (symbol-name symbol))
         (list (symbol-value symbol))

         ;; Separate prefix and name in symbol-name
         (prefix-pos    (string-match-p "/" symbol-name))
         (prefix        (if prefix-pos
                            (substring symbol-name 0 prefix-pos)))
         (name  (if prefix-pos
                    (substring symbol-name (1+ prefix-pos))
                  symbol-name))

         (property-alist (zp/make-property-alist name list))

         (set-property (lambda (arg)
                        (zp/helm-smbp-set-properties `,property-alist))))
    (helm :name "Testing"
          :sources `((name . ,(concat "*helm " name "*"))
                     (candidates . ,property-alist)
                     (action . (("Set property" . ,set-property)))))))



;; Init :Variables
(defvar zp/emotions nil
  "List of emotions.")

(defvar zp/emotions-alist nil
  "Alist of emotions where the car of each item is the human
  name, and the corresponding cdr is the name of the property.")

(defvar zp/cognitive-distortions nil
  "List of cognitive distortions.")

(defvar zp/cognitive-distortions-alist nil
  "Alist of cognitive distortions where the car of each item is
  the human name, and the corresponding cdr is the name of the
  property.")



;; Psychotherapy-mode

;; Helper functions
(defun zp/psychotherapy-thoughts-has-response-p ()
  "Return t if the current thought has a response."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (unless (org-at-heading-p)
        (org-previous-visible-heading 1))
      (re-search-forward "^\\*+ Cognitive Distortions$" subtree-end t))))

(defun zp/psychotherapy-thoughts-clear-response ()
  (save-excursion
    (org-mark-subtree)
    (forward-line)
    (delete-region (region-beginning) (region-end))))

(defun zp/psychotherapy-thoughts-create-response (arg)
  (let ((org-insert-heading-respect-content t)
        (has-response-p (zp/psychotherapy-thoughts-has-response-p)))
    (if has-response-p
        (zp/psychotherapy-thoughts-clear-response))
    (org-insert-heading)
    (org-demote)
    (insert "Cognitive Distortions")

    (zp/helm-smbp 'zp/cognitive-distortions)

    (org-insert-heading)
    (insert "Rational Response")
    (end-of-line)
    (open-line 1)
    (next-line)
    ))

(setq zp/psychotherapy-headings-alist
      '(("Situation" .
         (lambda (arg)
           (if (save-excursion (org-get-next-sibling))
               (progn
                 (org-forward-heading-same-level 1)
                 (zp/psychotherapy-dwim nil)))))

        ("Emotions" .
         (lambda (arg)
           (let ((org-insert-heading-respect-content t))
             (zp/helm-smbp 'zp/emotions)
             (if (save-excursion
                   (org-get-next-sibling))
                 (org-forward-heading-same-level 1))
             (unless (save-excursion
                       (and (org-get-heading "Thoughts")
                            (org-goto-first-child)))
               (org-insert-heading)
               (org-demote)))))

        ("Cognitive Distortions" .
         (lambda (arg)
           (zp/helm-smbp 'zp/cognitive-distortions)
           (if (save-excursion (org-get-next-sibling))
               (org-forward-heading-same-level 1))
           (next-line)))

        ("Rational Response" .
         (lambda (arg)
           (cond ((eq arg 4)
                  (outline-up-heading 1)
                  (zp/psychotherapy-thoughts-create-response arg))
                 ((save-excursion
                    (outline-up-heading 1)
                    (org-get-next-sibling))
                  (outline-up-heading 1)
                  (org-forward-heading-same-level 1)
                  (zp/psychotherapy-dwim nil))
                 (t
                  (if (y-or-n-p "Finalise?")
                      (org-capture-finalize)))))))

      zp/psychotherapy-parent-headings-alist
      '(("Thoughts"     . zp/psychotherapy-thoughts-create-response)))

;; dwim-function
(defun zp/psychotherapy-dwim (arg)
  (interactive "p")
  (let* ((heading (org-get-heading))
         (type (cdr (assoc heading zp/psychotherapy-headings-alist)))
         (parent (car (last (org-get-outline-path))))
         (parent-type (cdr (assoc parent zp/psychotherapy-parent-headings-alist))))
    (cond (type
           (unless (org-at-heading-p)
             (org-previous-visible-heading 1))
           (funcall type arg))

          (parent-type
           (funcall parent-type arg))

          (t
           (if (y-or-n-p "Finalise?")
               (org-capture-finalize))))))



;; mode definition
(defvar zp/psychotherapy-mode-map
  (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") #'zp/psychotherapy-dwim)
        map)
  "Keymap for ‘zp/psychotherapy-mode’.")

(define-minor-mode zp/psychotherapy-mode
  "Provide bindings for filling psychotherapy forms."
  :lighter " Psy"
  :keymap zp/psychotherapy-mode-map)



;; Loading extra minor-modes with org-capture
(defvar zp/org-capture-extra-minor-modes-alist nil
  "Alist of minors modes to load with specific org-capture templates.")

(setq zp/org-capture-extra-minor-modes-alist
      '(("D" . zp/psychotherapy-mode)))

;; org-capture hook
(defun zp/org-capture-load-extra-minor-mode ()
  "Load minor-mode based on based on key."
  (interactive)
  (let* ((key (plist-get org-capture-plist :key))
         (minor-mode (cdr (assoc key zp/org-capture-extra-minor-modes-alist))))
    (if minor-mode
        (funcall minor-mode))))

(add-hook #'org-capture-mode-hook #'zp/org-capture-load-extra-minor-mode)



;; Setting variables
(setq zp/cognitive-distortions
      '("All-or-nothing thinking"
        "Over-generalisation"
        "Mental filter"
        "Disqualifying the positive"
        "Mind-reading"
        "Fortune-Teller error"
        "Magnification or minimisation"
        "Emotional reasoning"
        "Should statements"
        "Labelling and mislabelling"
        "Personalisation")

      zp/emotions
      '("Anger"
        "Anxiety"
        "Boredom"
        "Disgust"
        "Dispirited"
        "Fear"
        "Guilt"
        "Laziness"
        "Loneliness"
        "Sadness"
        "Tiredness"))



;; ========================================
;; ================ MACROS ================
;; ========================================

(fset 'fold-current-drawer
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([21 18 58 46 42 63 58 36 return tab] 0 "%d")) arg)))



;; ========================================
;; ================= KEYS =================
;; ========================================

(global-set-key [M-kanji] 'ignore)

;; (global-set-key (kbd "C-x C-g") 'xah-open-in-external-app)

;; Unset org-mode keys
(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-,") nil)))
;; other-window with neg argument for global-set-key
(defun other-window-reverse ()
    (interactive)
    (other-window -1))

;; Toggle modes
(define-prefix-command 'zp/toggle-map)
(define-key ctl-x-map "t" 'zp/toggle-map)
(define-key zp/toggle-map "d" #'toggle-debug-on-error)
(define-key zp/toggle-map "e" #'toggle-debug-on-error)
(define-key zp/toggle-map "f" #'auto-fill-mode)
(define-key zp/toggle-map "F" #'flycheck-mode)
(define-key zp/toggle-map "l" #'toggle-truncate-lines)
(define-key zp/toggle-map "q" #'electric-quote-local-mode)
(define-key zp/toggle-map "Q" #'toggle-debug-on-quit)
(define-key zp/toggle-map "t" #'zp/switch-theme)
(define-key zp/toggle-map "c" #'zp/helm-select-font-dwim)

(global-set-key (kbd "C-c \\") 'picture-mode)
(global-set-key (kbd "C-c u") 'visual-line-mode)
(global-set-key (kbd "C-c s") 'scroll-bar-mode)
(global-set-key (kbd "C-c H") 'global-hl-line-mode)
(global-set-key (kbd "C-c g") 'display-line-numbers-mode)
(global-set-key (kbd "C-c L") 'org-store-link)
;; (global-set-key (kbd "C-c f") 'switch-main-font)
(global-set-key (kbd "C-c i") 'toggle-truncate-lines)
(global-set-key (kbd "C-c f") 'flyspell-mode)
(global-set-key (kbd "M-U")   'visual-line-mode)
(global-set-key (kbd "M-O")   'olivetti-mode)
(global-set-key (kbd "M-W")   'writeroom-mode)
(global-set-key (kbd "C-c w") 'zp/whitespace-mode-lines-tail)
(global-set-key (kbd "C-c W") 'whitespace-mode)

;; Prototype
;; Doesn't toggle, just turns on
;; (global-set-key (kbd "C-c h") (lambda () (interactive)
;;                              (global-hl-line-mode)
;;                              (global-linum-mode)))


;; Other toggles

;; Actions
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal) ;magnar
(global-set-key (kbd "C-x C-c") 'delete-frame)               ;magnars
(global-set-key (kbd "C-c c") 'calendar)
(global-set-key (kbd "C-c n") 'org-capture)
(global-set-key (kbd "C-c N") 'zp/org-noter)
(global-set-key (kbd "C-c C-=") 'increment-integer-at-point)
(global-set-key (kbd "C-c C--") 'decrement-integer-at-point)
(global-set-key (kbd "C-c d") 'zp/helm-ispell-preselect)
(global-set-key (kbd "C-c y") 'zp/variable-pitch-mode)
(global-set-key (kbd "C-c R") 'org-display-inline-images)
(global-set-key (kbd "C-c P") 'package-list-packages)
(global-set-key (kbd "H-h") 'er/expand-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c T") 'zp/switch-theme)
(global-set-key (kbd "H-.") 'zp/echo-buffer-name)
(global-set-key (kbd "H-M-.") 'herald-the-mode-line)
(global-set-key (kbd "H-/") 'zp/echo-clock-string)
(global-set-key (kbd "H-y") 'zp/helm-bibtex-with-local-bibliography)
(global-set-key (kbd "H-M-y") 'zp/helm-bibtex-select-bib)
(global-set-key (kbd "C-x F") 'zp/unfill-document)
(global-set-key (kbd "M-Q") 'zp/unfill-context)
(global-set-key (kbd "C-c D") 'zp/bibtex-completion-message-key-last)
(global-set-key (kbd "H-<backspace>") 'yas-prev-field)
(global-set-key (kbd "C-c x") 'zp/toggle-org-latex-pdf-process)
;; (global-set-key (kbd "H-g") 'keyboard-quit)
;; (global-set-key (kbd "H-l") 'zp/switch-to-mu4e)
(global-set-key (kbd "H-l") 'zp/switch-to-notmuch)
(global-set-key (kbd "H-M-l") 'mu4e-compose-new)
;; (global-set-key (kbd "H-m") 'zp/switch-to-magit)
(global-set-key (kbd "H-m") 'magit-status)

;; Clocking commands
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-z") 'org-resolve-clocks)

;; Movements
(global-set-key (kbd "H-o") 'zp/switch-to-agenda)
(global-set-key (kbd "H-M-;") 'helm-chronos-add-timer)
(global-set-key (kbd "H-;") 'zp/switch-to-chronos)
(global-set-key (kbd "H-M-;") 'zp/switch-to-chronos-and-add)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "H-j") 'other-window-reverse)
(global-set-key (kbd "H-k") 'other-window)
;; (global-set-key (kbd "C-x t") 'window-toggle-split-direction)
(global-set-key (kbd "C-x 4 1") 'zp/kill-other-buffer-and-window)

;; ace
(global-set-key (kbd "H-b") 'ace-window)
;; (global-set-key (kbd "H-n") 'avy-goto-word-1)
(global-set-key (kbd "H-n") 'avy-goto-char-timer)
;; (global-set-key (kbd "H-m") 'avy-goto-char)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'frame)
(ace-link-setup-default)

;; Usual shortcuts
(defun zp/org-agenda-day (&optional arg)
  (interactive "P")
  (org-agenda arg "N"))
(defun zp/org-agenda-tools (&optional arg)
  (interactive "P")
  (org-agenda arg "l"))
(defun zp/org-agenda-media (&optional arg)
  (interactive "P")
  (org-agenda arg "b"))
(defun zp/org-agenda-reading (&optional arg)
  (interactive "P")
  (org-agenda arg "r"))

(define-prefix-command 'agenda-map)
(global-set-key (kbd "<f9>") 'agenda-map)
(global-set-key (kbd "<f9> <f9>")  'zp/org-agenda-day)
(global-set-key (kbd "<f9> <f10>") 'zp/org-agenda-reading)
(global-set-key (kbd "<f9> <f11>") 'zp/org-agenda-media)
(global-set-key (kbd "<f9> <f12>") 'zp/org-agenda-tools)

;; Winner
(global-set-key (kbd "H-u") 'winner-undo)
(global-set-key (kbd "H-i") 'winner-redo)

;; EXPERIMENTAL
;; key-chord
;; (setq key-chord-two-keys-delay .020
;;       key-chord-one-key-delay .050)
;; (key-chord-define-global "df" 'avy-goto-char-2)

;; Doesn't really work: Clears the previous windows configuration
;; without being able to go back with winner-redo.
;; Better to use C-x z
;; (make-command-repeatable 'winner-undo)

(defun zp/kill-other-buffer-and-window ()
  "Kill the other buffer and window if there is more than one window."
  (interactive)
  (if (not (one-window-p))
      (progn
        (other-window 1)
        (kill-buffer-and-window))
    (error "There is only one window in the frame.")))

(defun zp/org-kill-indirect-buffer ()
  "Kill the current buffer if it is an indirect buffer."
  (interactive)
  (if (not (eq (buffer-base-buffer) nil))
      (progn
        (condition-case nil
            (kill-buffer-and-window)
          (error nil))
        (message "Killed indirect buffer and window."))
    (error "Not in an indirect buffer")))

(defun zp/org-agenda-kill-other-buffer-and-window ()
  "Kill the other buffer and window if there is more than one window in `org-agenda’."
  (interactive)
  (if (not (string-match "*Org Agenda*" (buffer-name)))
      (other-window 1))
  (zp/kill-other-buffer-and-window))

(defun zp/org-agenda-tree-to-indirect-buffer (dedicated)
  "Show the subtree corresponding to the current entry in an indirect buffer.

With a ‘C-u’ prefix, make a separate frame for this tree."
  (interactive "P")
  (let ((last-ibuf org-last-indirect-buffer)
        (buffer)
        (current-prefix-arg nil))
    (when dedicated
      (setq org-last-indirect-buffer nil))
    (org-agenda-tree-to-indirect-buffer nil)
    (when dedicated
      (setq org-last-indirect-buffer last-ibuf))
    (balance-windows)
    (other-window 1)
    (let ((org-startup-folded nil))
      (org-set-startup-visibility))
    (org-overview)
    (org-cycle)))

(defun zp/org-agenda-tree-to-indirect-buffer-without-grabbing-focus (arg)
  (interactive "P")
  (zp/org-agenda-tree-to-indirect-buffer arg)
  (other-window -1))

(defun zp/org-agenda-tree-to-indirect-buffer-maximise (arg)
  (interactive "P")
  (zp/org-agenda-tree-to-indirect-buffer arg)
  (delete-other-windows))

(defun move-beginning-of-line-dwim (arg)
  "Move point back to indentation or beginning of line

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line."
  (interactive "^p")
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= old-point (point))
      (move-beginning-of-line arg))))

(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line-dwim)



;; ========================================
;; ================ FACES =================
;; ========================================

;;; Truncate long buffer names
;; (require 'nadvice)
;; (defun my-truncate-buffer-name (buf-name)
;;   (let ((len (length buf-name)))
;;     (cond ((> len 20)
;;            (concat (substring buf-name 0 19)
;;                    "…"))
;;           (t buf-name))))
;; (advice-add 'powerline-buffer-id :filter-return 'my-truncate-buffer-name)

(defvar zp/powerline-text-height nil
  "Height of the text to be displayed in the modeline.")

(defun zp/pdf-view-midnight-mode-theme ()
  (setq pdf-view-midnight-colors
        `(,(face-attribute 'default :foreground) .
          ,(face-attribute 'default :background))))

(minions-mode 1)
(require 'moody)
(setq x-underline-at-descent-line t)
(setq moody-mode-line-height 40)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)




;; Modeline

(defvar ml-selected-window nil)

(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(defun ml-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'ml-record-selected-window)

(add-hook 'buffer-list-update-hook 'ml-update-all)

(defun zp/propertized-buffer-identification (fmt)
  "Return a list suitable for `mode-line-buffer-identification'.
FMT is a format specifier such as \"%12b\".  This function adds
text properties for face, help-echo, and local-map to it."
  (list (propertize fmt
                    'face (if (eq ml-selected-window (selected-window))
                              'mode-line-buffer-id
                            'mode-line-buffer-id-inactive)
                    'help-echo
                    (purecopy "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer")
                    'mouse-face 'mode-line-highlight
                    'local-map mode-line-buffer-identification-keymap)))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width
          (-
           (window-total-width)
           (+
            (length
             (format-mode-line left))
            (length
             (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)))

;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 ;; (:eval
;;                 ;;  (if (eq ml-selected-window (selected-window))
;;                 ;;      "OK "
;;                 ;;    "NO "))
;;                 ;; (:eval (propertize "%b  " 'face 'org-tag-important
;;                 ;;                    'help-echo (buffer-file-name)))
;;                 ;; (:eval (propertize "" 'font-lock-face '(:foreground "red"
;;                 ;;                                          :background nil)
;;                 ;;         'help-echo (buffer-file-name)))
;;                 "   "
;;                 ;; mode-line-buffer-identification
;;                 (:eval
;;                  (moody-tab
;;                   (format-mode-line
;;                    (zp/propertized-buffer-identification "%b"))
;;                   20 'down))
;;                 ;; (:eval
;;                 ;;  (zp/propertized-buffer-identification "%b"))
;;                 ;; (:eval (propertize "%12b"
;;                 ;;                                 'face (if (eq ml-selected-window (selected-window))
;;                 ;;                                              'mode-line-buffer-id
;;                 ;;                                            'mode-line-buffer-id-inactive)
;;                 ;;                                 'help-echo
;;                 ;;                                 (purecopy "Buffer name
;;                 ;; mouse-1: Previous buffer\nmouse-3: Next buffer")
;;                 ;;                                 'mouse-face 'mode-line-highlight
;;                 ;;                                 'local-map mode-line-buffer-identification-keymap))
;;                 "   "
;;                 mode-line-position
;;                 ;; (vc-mode vc-mode)
;;                 (:eval (moody-tab (substring vc-mode 1) nil 'up))
;;                 "  "
;;                 minions-mode-line-modes
;;                 ;; mode-line-misc-info
;;                 mode-line-end-spaces))

(setq-default mode-line-format
              '((:eval
                 (simple-mode-line-render
                  ;; Left
                  '("%e"
                    mode-line-front-space
                    ;; (:propertize mode-line-mule-info face (:foreground "#777"))
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    mode-line-frame-identification
                    ;; (:eval
                    ;;  (if (eq ml-selected-window (selected-window))
                    ;;      "OK "
                    ;;    "NO "))
                    ;; (:eval (propertize "%b  " 'face 'org-tag-important
                    ;;                    'help-echo (buffer-file-name)))
                    ;; (:eval (propertize "" 'face '(:foreground "red"
                    ;;                                          :background nil)
                    ;;                    'help-echo (buffer-file-name)))
                    "   "
                    ;; mode-line-buffer-identification
                    (:eval (moody-tab
                            (format-mode-line
                             (zp/propertized-buffer-identification "%b"))
                            20 'down))
                    ;; (:propertize " [%*]" face (:foreground "#49B05C"))
                    " [%*]"
                    " "
                    ;; (:eval (propertize "test" 'face '(:foreground "red" :weight 'bold)
                    ;;                    'help-echo "buffer is read-only!!!"))
                    ;; mode-line-buffer-identification
                    ;; (:propertize minions-mode-line-modes face (:foreground "#777"))
                    minions-mode-line-modes
                    ;; minions-mode-line-modes
                    ;; " %l : %c"
                    evil-mode-line-tag)
                  ;; Right
                  '(;; (:propertize "%p" face mode-line-buffer-id)
                    ;; (:propertize " | " face (:foreground "#777"))
                    "%p | %l : %c "
                    ;; mode-line-position
                    ;; (vc-mode vc-mode)
                    (vc-mode moody-vc-mode)
                    " "
                    ;; (:eval (moody-tab (substring vc-mode 1) 20 'up))
                    ;; mode-line-modes
                    mode-line-misc-info
                    "  "
                    mode-line-end-spaces)))))

;; Default
;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification
;;                 "   "
;;                 mode-line-position
;;                 (vc-mode vc-mode)
;;                 "  "
;;                 mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

;;   (set-face-attribute 'mode-line nil :background "#666358" :foreground "black"))

;; END

(defun zp/mode-line-theme (&optional arg)
  (cond ((string= arg "dark")
         (progn
           (set-face-attribute 'mode-line nil
                               :background "#293233"
                               :foreground "#bfe9bd"
                               :weight 'bold)
           (set-face-attribute 'mode-line-inactive nil
                               :background "#1d1d1d"
                               :foreground "#666"
                               :weight 'bold)
           (set-face-attribute 'mode-line-buffer-id nil
                               :foreground "DarkGoldenrod2"
                               :weight 'bold)
           (set-face-attribute 'mode-line-buffer-id-inactive nil
                               :foreground "#888"
                               :weight 'bold)))
        ((string= arg "light")
         (progn
           (set-face-attribute 'mode-line nil
                               :background "#948e76"
                               :foreground "#333"
                               :weight 'bold)
           (set-face-attribute 'mode-line-inactive nil
                               :background "#c7bf9e"
                               :foreground "#666"
                               :weight 'bold)
           (set-face-attribute 'mode-line-buffer-id nil
                               :foreground "#d98e2d"
                               :weight 'bold)
           (set-face-attribute 'mode-line-buffer-id-inactive nil
                               :foreground "#948e76"
                               :weight 'bold)))))

(defface org-todo-todo '((t)) nil)
(defface org-todo-next '((t)) nil)
(defface org-todo-strt '((t)) nil)
(defface org-todo-done '((t)) nil)
(defface org-todo-stby '((t)) nil)
(defface org-todo-wait '((t)) nil)
(defface org-todo-cxld '((t)) nil)

(defun zp/org-todo-format-face (type face colour)
  (cond ((string= type "box")
         (set-face-attribute face nil
                             :box '(:line-width -3 :style released-button)
                             :height 0.8
                             :weight 'bold
                             :foreground "white"
                             :background colour))
        ((string= type "normal")
         (set-face-attribute face nil
                             :box nil
                             :height 0.8
                             :background nil
                             :weight 'bold
                             :foreground colour))))

(defface org-priority-face-a '((t)) nil)
(defface org-priority-face-b '((t)) nil)
(defface org-priority-face-c '((t)) nil)
(defface org-priority-face-d '((t)) nil)
(defface org-priority-face-e '((t)) nil)

(defface org-tag-location    '((t :inherit 'org-tag)) nil)
(defface org-tag-todo        '((t :inherit 'org-tag)) nil)
(defface org-tag-important   '((t :inherit 'org-tag)) nil)
(defface org-tag-reading     '((t :inherit 'org-tag)) nil)
(defface org-tag-french      '((t :inherit 'org-tag)) nil)

(defun zp/org-format-face (face &rest args)
  (let (
        (foreground (plist-get args :foreground))
        (weight     (plist-get args :weight))
        (background (plist-get args :background)))
    (if (bound-and-true-p foreground)
        (set-face-attribute face nil :foreground foreground)
      (set-face-attribute face nil :foreground nil))
    (if (bound-and-true-p background)
        (set-face-attribute face nil :background background)
      (set-face-attribute face nil :background nil))
    (if (bound-and-true-p weight)
        (set-face-attribute face nil :weight weight)
      (set-face-attribute face nil :weight 'normal))))



;; ========================================
;; ================ FONTS =================
;; ========================================

(calendar-set-date-style 'iso)

;; Line spacing
(setq-default line-spacing nil)

(defvar zp/line-spacing line-spacing
  "Default line-spacing.")
(defvar zp/line-spacing-variable nil
  "Default line-spacing for variable-pitch-mode.")

;; Custom variable-pitch-mode
(make-variable-buffer-local
 (defvar zp/variable-pitch-mode-toggle nil
   "State of customised variable-pitch-mode."))

(defun zp/variable-pitch-mode ()
  "Enable variable-pitch-mode and changes line-spacing."
  (interactive)
  (cond (zp/variable-pitch-mode-toggle
         (variable-pitch-mode)
         (setq zp/variable-pitch-mode-toggle nil))
        (t
         (variable-pitch-mode)
         (setq zp/variable-pitch-mode-toggle 1)))
  (zp/update-line-spacing))

(defun zp/update-line-spacing ()
  "Update line-spacing based on font-preset and mode.
Act on all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if zp/variable-pitch-mode-toggle
          (setq line-spacing zp/line-spacing-variable)
        (setq line-spacing zp/line-spacing)
        (setq-default line-spacing zp/line-spacing)))))



;; Font setting
(defvar zp/current-font nil
  "Name of the current default font-preset.")

(defvar zp/current-font-variable nil
  "Name of the current variable font-preset.")

(defun zp/set-font (font)
  "Change default font.
FONT is a preset."
  (let ((current zp/current-font))
    (unless (equal current font)
      (pcase font
        ("sarasa"
         (set-face-attribute 'default nil
                             :font "Sarasa Term Prog J" :height 113)
         (setq zp/line-spacing nil))
        ("operator"
         (set-face-attribute 'default nil
                             :font "Operator Mono Prog" :height 122)
         (setq zp/line-spacing 0.1))
        ("gintronic"
         (set-face-attribute 'default nil
                             :font "Gintronic Prog" :height 113)
         (setq zp/line-spacing 0.1)))
      (setq zp/current-font font)
      (zp/update-line-spacing)
      (message (concat "Font switched to " (capitalize font))))))

(defun zp/set-font-variable (font)
  "Change variable font.
FONT is a preset."
  (let ((current zp/current-font-variable))
    (unless (equal current font)
      (pcase font
        ("equity"
         (set-face-attribute 'variable-pitch nil
                             :font "Equity Text A" :height 158)
         (setq zp/line-spacing-variable nil))
        ("guyot"
         (set-face-attribute 'variable-pitch nil
                             :font "Guyot Text" :height 152)
         (setq zp/line-spacing-variable 0.3))
        ("bliss"
         (set-face-attribute 'variable-pitch nil
                             :font "Bliss Pro Prog" :height 158)
         (setq zp/line-spacing-variable nil))
        ("typewriter"
         (set-face-attribute 'variable-pitch nil
                             :font "ITC American Typewriter Std" :height 158)
         (setq zp/line-spacing-variable 0.3)))
      (setq zp/current-font-variable font)
      (zp/update-line-spacing)
      (message (concat "Variable font switched to " (capitalize font))))))

(zp/set-font "sarasa")
(zp/set-font-variable "equity")



;; Font toggling
(defvar zp/current-font-variable nil
  "Name of the current variable font-preset.")

(defvar zp/list-fonts nil
  "List of default font-presets.")

(defvar zp/list-fonts-variable nil
  "List of variable font-presets.")

(setq zp/list-fonts '("sarasa" "operator" "gintronic"))
(setq zp/list-fonts-variable '("equity" "guyot" "bliss" "typewriter"))

(defun zp/toggle-font (type current list)
  "Toggle between font-presets.

TYPE is the type of fonts to toggle.
CURRENT is the variable holding the current font-preset.
LIST is the variable holding the list of font-presets."
  (interactive)
  (let* ((current current)
         (list list)
         (next-p (car (cdr (member current list))))
         (next (if next-p next-p (car list))))
    (pcase type
      ("default" (zp/set-font next))
      ("variable" (zp/set-font-variable next)))
    (zp/update-line-spacing)))

(defun zp/toggle-font-default ()
  "Toggle between default font-presets.
CURRENT is the variable holding the current default font-preset.
LIST is the variable holding the list of default font-presets."
  (interactive)
  (zp/toggle-font "default" zp/current-font zp/list-fonts))

(defun zp/toggle-font-variable ()
  "Toggle between default font-presets.
CURRENT is the variable holding the current variable font-preset.
LIST is the variable holding the list of variable font-presets."
  (interactive)
  (zp/toggle-font "variable" zp/current-font-variable zp/list-fonts-variable))



;; Select font with Helm
(defun zp/helm-select-font (&optional font)
  "Select the font-preset to use."
  (interactive)
  (let ((current zp/current-font))
    (helm :sources '((name . "*HELM - Font selection*")
                     (candidates . zp/list-fonts)
                     (action . (("Change font" . zp/set-font))))
          :preselect current)))

(defun zp/helm-select-font-variable (&optional font)
  "Select the variable font-preset to use."
  (interactive)
  (let ((current zp/current-font-variable))
    (helm :sources '((name . "*HELM - Font selection*")
                     (candidates . zp/list-fonts-variable)
                     (action . (("Change font" . zp/set-font-variable))))
          :preselect current)))

(defun zp/helm-select-font-dwim ()
  "Select the font-preset to use.

If in variable-pitch-mode, change the variable font-preset."
  (interactive)
  (if zp/variable-pitch-mode-toggle
      (zp/helm-select-font-variable)
    (zp/helm-select-font)))



;; ========================================
;; ================ THEME =================
;; ========================================

(defvar zp/emacs-theme nil
  "Theme currently used by Emacs.")

(defun zp/dark-theme ()
  (interactive)
  (setq zp/emacs-theme "dark")
  (load-theme 'base16-atelier-sulphurpool t)

  (set-face-attribute 'default nil :foreground "#BCAF8E" :background "#141414")
  (set-face-attribute 'org-todo nil :foreground "darkred")
  (set-face-attribute 'org-done nil :foreground "spring green")
  (set-face-attribute 'org-scheduled-today nil :foreground "CadetBlue")
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-hide nil :foreground "#141414")
  (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSlateBlue")
  (set-face-attribute 'region nil :background "RoyalBlue4")
  (set-face-attribute 'helm-selection nil :background "RoyalBlue4") ;Darker Royal Blue
  (set-face-attribute 'org-agenda-clocking nil :background "RoyalBlue4")
  (set-face-attribute 'fringe nil :background "gray10" :foreground "orangered")
  (set-face-attribute 'vertical-border nil :foreground "RoyalBlue1")
  (set-face-attribute 'org-agenda-structure nil :foreground "DodgerBlue1" :weight 'bold)
  (set-face-attribute 'hl-line nil :background "#1F1F3F")
  (set-face-attribute 'org-level-4 nil :foreground "#ed3971")
  (set-face-attribute 'org-meta-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'header-line nil :foreground "#777")
  (set-face-attribute 'mu4e-system-face nil :foreground "SlateBlue")
  (set-face-attribute 'mu4e-modeline-face nil :foreground "LightBlue4")
  (set-face-attribute 'line-number nil :foreground "#969996" :background "#2d2d2d")
  (set-face-attribute 'secondary-selection nil :background "#3B3273")
  (set-face-attribute 'lui-track-bar nil :background "RoyalBlue4")
  (set-face-attribute 'org-column nil :background "#1F1F1F")
  (set-face-attribute 'org-block nil :foreground nil :inherit 'default :background "#1F1F1F")


  (set-face-attribute 'zp/org-agenda-block-info-face nil
                      :foreground "violetred1"
                      :background "violetred4"
                      :height 0.8
                      :weight 'bold)
  (set-face-attribute 'zp/org-agenda-block-warning-face nil :foreground "red" :weight 'bold)

  (zp/org-todo-format-face 'normal 'org-todo-todo "darkred")
  (zp/org-todo-format-face 'normal 'org-todo-next "DodgerBlue1")
  (zp/org-todo-format-face 'normal 'org-todo-strt "gold3")
  (zp/org-todo-format-face 'normal 'org-todo-done "SpringGreen3")
  (zp/org-todo-format-face 'normal 'org-todo-stby "SkyBlue4")
  (zp/org-todo-format-face 'normal 'org-todo-wait "Skyblue4")
  (zp/org-todo-format-face 'normal 'org-todo-cxld "turquoise")

  (zp/org-format-face 'org-priority-face-a :foreground "white" :background "darkred")
  (zp/org-format-face 'org-priority-face-b :foreground "darkred")
  (zp/org-format-face 'org-priority-face-c :foreground "yellow")
  (zp/org-format-face 'org-priority-face-d :foreground "ForestGreen")
  (zp/org-format-face 'org-priority-face-e :foreground "RoyalBlue")

  (zp/org-format-face 'org-tag-location  :weight 'bold :foreground "BlueViolet")
  (zp/org-format-face 'org-tag-todo   :weight 'bold :foreground "Skyblue4")
  (zp/org-format-face 'org-tag-important :weight 'bold :foreground "darkred")
  (zp/org-format-face 'org-tag-reading   :weight 'bold :foreground "DeepPink")
  (zp/org-format-face 'org-tag-french    :weight 'bold :foreground "DodgerBlue1")

  (zp/org-format-face 'magit-tag :foreground "SpringGreen4")

  (zp/mode-line-theme "dark")
  (zp/pdf-view-midnight-mode-theme))

(defun zp/light-theme ()
  (interactive)
  (setq zp/emacs-theme "light")
  (load-theme 'base16-google-light t)

  ;; (set-face-attribute 'org-todo-box nil :inverse-video t :foreground "white" :height 0.8 :weight 'bold :box nil)
  ;; (set-face-attribute 'default nil :background "cornsilk1") ;fff8dc
  (set-face-attribute 'default nil :foreground "#3c3836" :background "#fbf1c7")
  (set-face-attribute 'fringe nil :background "#e6deb8" :foreground "orangered")
  (set-face-attribute 'org-hide nil :foreground "#fbf1c7")
  (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSlateBlue")
  (set-face-attribute 'org-scheduled-today nil :foreground "DodgerBlue4")
  (set-face-attribute 'region nil :background "SkyBlue1")
  (set-face-attribute 'hl-line nil :background "#ffea89")
  (set-face-attribute 'org-level-4 nil :foreground "#ed3971")
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-agenda-clocking nil :background "LightBlue2")
  (set-face-attribute 'org-meta-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'helm-selection nil :background "SteelBlue1")
  (set-face-attribute 'helm-visible-mark nil :background "goldenrod1")
  (set-face-attribute 'header-line nil :foreground "#ccc")
  (set-face-attribute 'mu4e-system-face nil :foreground "SlateBlue3")
  (set-face-attribute 'mu4e-modeline-face nil :foreground "LightBlue3")
  (set-face-attribute 'org-agenda-structure nil :foreground "DodgerBlue1" :weight 'bold)
  (set-face-attribute 'line-number nil :foreground "#636663" :background "#d4cdaa")
  ;; (set-face-attribute 'line-number-current-line nil :foreground "#707370" :background "#ccc6a4")
  (set-face-attribute 'secondary-selection nil :background "#d3ccff")
  (set-face-attribute 'lui-track-bar nil :background "RoyalBlue1")
  (set-face-attribute 'org-column nil :background "#F0E4BE")
  (set-face-attribute 'org-block nil :foreground nil :inherit 'default :background "#F0E6BE")


  (set-face-attribute 'diff-hl-change nil :foreground "#3a81c3" :background "#afcce7")
  (set-face-attribute 'diff-hl-insert nil :foreground "#7ccd7c" :background "#b3e2b3")
  (set-face-attribute 'diff-hl-delete nil :foreground "#ee6363" :background "#f6a8a8")

  (set-face-attribute 'zp/org-agenda-block-info-face nil
                      :foreground "violetred1"
                      :background "thistle2"
                      :height 0.8
                      :weight 'bold)
  (set-face-attribute 'zp/org-agenda-block-warning-face nil :foreground "red" :weight 'bold)

  (zp/org-todo-format-face 'normal 'org-todo-todo "red")
  (zp/org-todo-format-face 'normal 'org-todo-next "DodgerBlue1")
  (zp/org-todo-format-face 'normal 'org-todo-strt "gold3")
  (zp/org-todo-format-face 'normal 'org-todo-done "SpringGreen3")
  (zp/org-todo-format-face 'normal 'org-todo-stby "SkyBlue4")
  (zp/org-todo-format-face 'normal 'org-todo-wait "Skyblue4")
  (zp/org-todo-format-face 'normal 'org-todo-cxld "turquoise")

  (zp/org-format-face 'org-priority-face-a :foreground "white" :background "red")
  (zp/org-format-face 'org-priority-face-b :foreground "red")
  (zp/org-format-face 'org-priority-face-c :foreground "gold3")
  (zp/org-format-face 'org-priority-face-d :foreground "ForestGreen")
  (zp/org-format-face 'org-priority-face-e :foreground "RoyalBlue")

  (zp/org-format-face 'org-tag-location  :weight 'bold :foreground "BlueViolet")
  (zp/org-format-face 'org-tag-todo      :weight 'bold :foreground "Skyblue1")
  (zp/org-format-face 'org-tag-important :weight 'bold :foreground "red")
  (zp/org-format-face 'org-tag-reading   :weight 'bold :foreground "DeepPink")
  (zp/org-format-face 'org-tag-french    :weight 'bold :foreground "DodgerBlue1")

  (zp/org-format-face 'magit-tag :foreground "SpringGreen4")

  (zp/mode-line-theme "light")
  (zp/pdf-view-midnight-mode-theme))

(defun zp/pdf-view-update-midnight ()
  "Update pdf-view’s colour theme."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'pdf-view-mode)
        (pdf-view-midnight-minor-mode)))))

(defun zp/switch-theme ()
  (interactive)
  (cond ((string= zp/emacs-theme "dark")
         (zp/light-theme))
        ((string= zp/emacs-theme "light")
         (zp/dark-theme)))
  (zp/pdf-view-update-midnight))



;; Old stuff
;; Mark 1
;; (load-theme 'base16-atelier-sulphurpool t)

;; SML (Modeline)
;; (setq sml/no-confirm-load-theme t)
;; (setq sml/theme 'dark)
;; (setq sml/name-width '20)
;; (sml/setup)

;; Old theme switch
;; (setq dark-theme 'base16-ateliersulphurpool-dark
;;       light-theme 'base16-solarized-light)
;; (setq theme-mode 'dark-theme)

;; (defun switch-theme-mode ()
;;   "Switch the theme between dark and light mode."
;;   (interactive)
;;   (if (not (eq theme-mode 'dark))
;;       (progn
;;      (load-theme dark-theme t)
;;      (set-face-attribute 'org-done nil :foreground "spring green")
;;      (set-face-attribute 'org-hide nil :foreground "#202746")
;;      (set-face-attribute 'helm-selection nil :background "RoyalBlue4")
;;      (set-face-attribute 'region nil :background "RoyalBlue4")
;;      (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "SkyBlue4")
;;      (set-face-attribute 'org-agenda-clocking nil :background "RoyalBlue4")
;;      (setq theme-mode 'dark))
;;     (progn
;;       (load-theme light-theme t)
;;       (set-face-attribute 'org-hide nil :foreground "#fdf6e3")
;;       (set-face-attribute 'helm-selection nil :background "RoyalBlue4")
;;       (set-face-attribute 'region nil :background "RoyalBlue4")
;;       (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSteelBlue3")
;;       (set-face-attribute 'org-agenda-clocking nil :background "RoyalBlue4")
;;       (setq theme-mode 'light))))

;; (switch-theme-mode)



;; -----------------------------------------------------------------------------
;; Switch theme based on time-of-day
(defvar zp/time-of-day-sections nil
  "List of time specifications STR to split the day into sections.
The sections are inferred based on when they begin, and the time
specifications are processed in order:
- Pre-day
- Day
- Pre-evening
- Evening
- Night")

;;; Parsing time-of-day data
(defun zp/encode-time-of-day (TIME-STR CURRENT-DAY-DECODED NEXT-DAY-DECODED)
  "Encode time specification from STR to (HIGH LOW)."
  (let* ((time-string TIME-STR)
         (current-day-decoded CURRENT-DAY-DECODED)
         (next-day-decoded NEXT-DAY-DECODED))
    ;; The output of ‘parse-time-string’ cannot be encoded by
    ;; ‘encode-time’ because of missing data, so we’re using either
    ;; today’s to fill in the blanks. If ‘time-string’ is between 12am
    ;; and 1am, use tomorrow’s data instead.
    (apply 'encode-time
           (append
            (subseq (parse-time-string time-string) 0 3)
            (subseq (if (string-match "^0*:" time-string)
                        next-day-decoded
                      current-day-decoded)
                    3)))))

(defvar zp/time-of-day-sections-parsed nil
  "List of time specifications (HIGH LOW) to split the day into sections.
Parsed by zp/parse-time-of-day-sections.
See ‘zp/time-of-day-sections’ for more info.")

(defun zp/parse-time-of-day-sections ()
  "Parse time-of-day sections in ‘zp/time-of-day-sections’.
Each string is replaced by the corresponding list of
integers (HIGH LOW) used by Emacs to compute time. See
‘current-time’ for more information on (HIGH LOW).

A new time specification ‘next-day’ is computed from ‘day’ and
appended to the list to handle next-day timers."
  (let* ((now                           (current-time))
         (now-decoded                   (decode-time now))
         (tomorrow                      (time-add now (* 24 60 60)))
         (tomorrow-decoded              (decode-time tomorrow))
         (after-tomorrow                (time-add tomorrow (* 24 60 60)))
         (after-tomorrow-decoded        (decode-time after-tomorrow))
         (tod-sections                  zp/time-of-day-sections)
         (tod-day                       (nth 1 tod-sections)))
    (setq zp/time-of-day-sections-parsed
          (mapcar (lambda (arg)
                    (zp/encode-time-of-day arg
                                           now-decoded
                                           tomorrow-decoded))
                  tod-sections))
    ;; Add next-day to list
    (add-to-list 'zp/time-of-day-sections-parsed
                 (zp/encode-time-of-day tod-day
                                        tomorrow-decoded
                                        after-tomorrow-decoded)
                 t)))

;;; Status
(defun zp/daytimep ()
  "Return t if it’s day-time.
Based on ‘zp/time-of-day-sections’. A time-of-day is considered
as day-time if it’s between pre-day and pre-evening.
See ‘zp/time-of-day-sections’ for more info."
  (let* ((tod-sections zp/time-of-day-sections-parsed)
         (now           (current-time))
         (pre-day       (nth 0 tod-sections))
         (pre-evening   (nth 2 tod-sections)))
    (if (and (time-less-p pre-day now)
             (time-less-p now pre-evening))
        t
      nil)))

;;; Switching themes
(defun zp/switch-theme-dwim (&optional print-message)
  "Switch theme based on time-of-day.
See ‘zp/time-of-day-sections’ and ‘zp/daytimep’ for more info."
  (interactive "p")
  (let* ((daytime (zp/daytimep)))
    (cond ((and daytime
                (or (string= zp/emacs-theme "dark")
                    (not zp/emacs-theme)))
           (zp/light-theme))
          ((and (not daytime)
                (or (string= zp/emacs-theme "light")
                    (not zp/emacs-theme)))
           (zp/dark-theme))
          (t
           (when print-message
             (message "Nothing to do."))))
    (zp/pdf-view-update-midnight)))

(defun zp/switch-theme-auto ()
  "Automatically switch theme based on time-of-day.
See ‘zp/time-of-day-sections’ and ‘zp/daytimep’ for more info."
  (zp/parse-time-of-day-sections)
  (zp/switch-theme-dwim)
  (zp/set-daytime-timer))

;;; Timers
(defvar zp/daytime-timer nil
  "Timer before next daytime event.")

(defun zp/set-daytime-timer ()
  "Set timer for switching theme at ‘day’ and ‘evening’.
See ‘zp/time-of-day-sections’"
  (let* ((tod-sections  zp/time-of-day-sections-parsed)
         (now           (current-time))
         (day           (nth 1 tod-sections))
         (evening       (nth 3 tod-sections))
         (next-day      (nth 5 tod-sections)))
    (unless (not zp/daytime-timer)
          (cancel-timer zp/daytime-timer)
          (setq zp/daytime-timer nil))
    (setq zp/daytime-timer
          (run-at-time (cl-some (lambda (x)
                                  (when (time-less-p now x)
                                    x))
                                (list day evening next-day))
                       nil #'zp/switch-theme-auto))))

;; Init
(setq zp/time-of-day-sections '("06:00" "08:00" "16:00" "20:00" "00:00"))
(zp/switch-theme-auto)
;; -----------------------------------------------------------------------------



(defun zp/terminology-dwim (&optional ARGUMENTS)
  "Run terminology in the CWD.

Trim unnecessary TRAMP information from the path (e.g. /sudo:…),
and forward it to terminology. ARGUMENTS can be any argument
accepted by terminology (e.g. ‘-x command’).

See ‘/home/zaeph/.bin/terminology-dwim’ for more info."
  (interactive)
  (let ((client-buffer (current-buffer))
        (arg ARGUMENTS))
    (with-current-buffer (window-buffer (selected-window))
      (let* ((path-emacs default-directory)
             (tramp-regex "/sudo:root@.*?:")
             (path (replace-regexp-in-string
                    tramp-regex "" path-emacs)))
        (set-buffer client-buffer)
        (call-process-shell-command
         (concat "terminology"
                 (if arg (concat " " arg))
                 " -d \"" path "\""))))))

(defun zp/terminator-dwim (&optional ARGUMENTS)
  "Run terminator in the CWD.

Trim unnecessary TRAMP information from the path (e.g. /sudo:…),
and forward it to terminator. ARGUMENTS can be any argument
accepted by terminator (e.g. ‘-x command’).

See ‘/home/zaeph/.bin/terminator-dwim’ for more info."
  (interactive)
  (let ((client-buffer (current-buffer))
        (arg ARGUMENTS))
    (with-current-buffer (window-buffer (selected-window))
      (let* ((path-emacs default-directory)
             (tramp-regex "/sudo:root@.*?:")
             (path (replace-regexp-in-string
                    tramp-regex "" path-emacs)))
        (set-buffer client-buffer)
        (shell-command
         (concat "terminator --working-dir \"" path "\""
                 (if arg (concat " " arg))))))))



(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s...done (%.3fs)" ,title elapsed))))))



;; DON'T GO THERE
;; YOU'LL LOSE YOUR SANITY



;; ;; Trying font-lock
;; (copy-face 'org-tag 'org-action-word-face)

;; (set-face-attribute 'org-action-word-face nil :foreground nil :weight 'bold :underline nil)n

;; (setq org-action-words "Prepare\\|Email\\|Read\\|Understand\\|Fill")
;; ;; Ideally, should use org-todo-keywords-1 and make a usable string with it rather than just copying everything here
;; (setq org-todo-words "TODO\\|NEXT\\|STRT\\|DONE\\|STBY\\|WAIT\\|CXLD")

;; ;; Can't figure out a way to make the regex work in a variable
;; ;; (setq problematic-regex (concat "\\(?:" org-todo-words "\\).*?\\(" org-action-words "\\)"))
;; ;; (setq problematic-regex "\\(?:TODO\\|NEXT\\|STRT\\|DONE\\|STBY\\|WAIT\\|CXLD\\).*?\\(Prepare\\|Email\\|Read\\|Understand\\|Fill\\)")
;; (defcustom problematic-regex "\\(?:TODO\\|NEXT\\|STRT\\|DONE\\|STBY\\|WAIT\\|CXLD\\).*?\\(Prepare\\|Email\\|Read\\|Understand\\|Fill\\)"
;;   "Just a fucking regex that doesn't want to work")

;; (font-lock-add-keywords
;;  'org-mode
;;  '(("\\(?:NEXT\\|TODO\\).*?\\(Prepare\\|Email\\)" 1 'org-action-word-face prepend))
;;  ;; '(("\\(?:TODO\\|NEXT\\|STRT\\|DONE\\|STBY\\|WAIT\\|CXLD\\).*?\\(Prepare\\|Email\\|Read\\|Understand\\|Fill\\)" 1 'org-action-word-face prepend))
;;  ;; '(((concat "\\(?:" org-todo-words "\\).*?\\(" org-action-words "\\)") 1 'org-action-word-face prepend))
;;  ;; '((regex-keywords 1 'org-action-word-face prepend))
;;  'append)

;; ;; (defun my-font-lock-restart ()
;; ;;   (interactive)
;; ;;   (setq font-lock-mode-major-mode nil)
;; ;;   (font-lock-fontify-buffer))

;; ;; (my-font-lock-restart)



;; ;; Trying text-properties
;; (setq org-finalize-agenda-hook
;;     (lambda ()
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward "(DONE)" nil t)
;;           (add-text-properties (match-beginning 0) (match-end 0)
;;                             '(face org-done)))
;;      (goto-char (point-min))
;;         (while (re-search-forward "(TODO)\\|(NEXT)\\|(STRT)\\|(SCHD)\\|(PREP)" nil t)
;;           (add-text-properties (match-beginning 0) (match-end 0)
;;                             '(face org-todo)))
;;         (while (re-search-forward problematic-regex nil t)
;;           (add-text-properties (match-beginning 1) (match-end 1)
;;                             '(face org-action-word-face)))
;;      ;; Remove mouse highlighting in org-agenda
;;      (remove-text-properties
;;          (point-min) (point-max) '(mouse-face t))
;;      )))

;; ;; (setq org-finalize-agenda-hook nil)

;; ;;   (add-text-properties (match-beginning 0) (point-at-eol)



;; Better archiving
;; Only works without prefix
;; Presumably not too stable

;; (defadvice org-archive-subtree (around fix-hierarchy activate)
;;   (let* ((fix-archive-p (and (not current-prefix-arg)
;;                              (not (use-region-p))))
;;          (afile (org-extract-archive-file (org-get-local-archive-location)))
;;          (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
;;     ad-do-it
;;     (when fix-archive-p
;;       (with-current-buffer buffer
;;         (goto-char (point-max))
;;         (while (org-up-heading-safe))
;;         (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
;;                (path (and olpath (split-string olpath "/")))
;;                (level 1)
;;                tree-text)
;;           (when olpath
;;             (org-mark-subtree)
;;             (setq tree-text (buffer-substring (region-beginning) (region-end)))
;;             (let (this-command) (org-cut-subtree))
;;             (goto-char (point-min))
;;             (save-restriction
;;               (widen)
;;               (-each path
;;                 (lambda (heading)
;;                   (if (re-search-forward
;;                        (rx-to-string
;;                         `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
;;                       (org-narrow-to-subtree)
;;                     (goto-char (point-max))
;;                     (unless (looking-at "^")
;;                       (insert "\n"))
;;                     (insert (make-string level ?*)
;;                             " "
;;                             heading
;;                             "\n"))
;;                   (cl-incf level)))
;;               (widen)
;;               (org-end-of-subtree t t)
;;               (org-paste-subtree level tree-text))))))))



;; Prototype
(add-to-list 'org-export-filter-timestamp-functions
             #'endless/filter-timestamp)
(defun endless/filter-timestamp (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    ((or `jekyll `html)
     (replace-regexp-in-string "&[lg]t;" "" trans))
    (`latex
     (replace-regexp-in-string "[<>]" "" trans))))

;; (setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats
      '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))


;; ========================================
;; ============= LATE MODES ===============
;; ========================================

;; Magnars's codes
;; expand-region causes weird flicker with repeated tasks if it's at the top
(require 'expand-region)
(require 'multiple-cursors)

;; Diminish
;; Allow minor modes to not have modeline display
(require 'diminish)

(diminish 'ivy-mode)
(diminish 'helm-mode)
(diminish 'auto-revert-mode)
(diminish 'anzu-mode)
(diminish 'yas-minor-mode)
(diminish 'which-key-mode)
(diminish 'volatile-highlights-mode)
(diminish 'undo-tree-mode)
(diminish 'whitespace-mode)



;; ========================================
;; =============== CUSTOM =================
;; ========================================

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(gdb-many-windows t)
 '(global-hl-line-mode t)
 '(helm-external-programs-associations
   (quote
    (("xlsx" . "localc")
     ("docx" . "lowriter")
     ("gpg" . "evince")
     ("mp4" . "smplayer")
     ("mkv" . "smplayer")
     ("dvi" . "evince")
     ("svg" . "inkscape")
     ("odt" . "lowriter")
     ("png" . "gimp")
     ("html" . "firefox")
     ("pdf" . "evince"))))
 '(ledger-reports
   (quote
    (("bal-last" "ledger bal ^expenses -p last\\ week and not commons and not swimming")
     ("bal-week" "ledger bal ^expenses -p this\\ week and not commons and not swimming")
     ("bal" "ledger -f /home/zaeph/org/main.ledger bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-submodule-arguments (quote ("--recursive")))
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-code verbatim)
     ("~" org-verbatim verbatim)
     ("+"
      (:strike-through t))
     ("@" org-todo))))
 '(org-file-apps
   (quote
    (("\\.pdf\\'" . "evince %s")
     ("\\.epub\\'" . "ebook-viewer %s")
     ("\\.mobi\\'" . "ebook-viewer %s")
     ("\\.doc\\'" . "lowriter %s")
     (auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default))))
 '(package-selected-packages
   (quote
    (wgrep fountain-mode org-mind-map org org-ref orgalist ws-butler minions moody org-super-agenda backup-walker bug-hunter org-plus-contrib messages-are-flowing notmuch forge go-mode company-anaconda anaconda-mode company realgud ace-link ivy-hydra counsel lispy dumb-jump lua-mode fish-mode exwm el-patch diminish circe-notifications circe ob-async nov which-key eyebrowse diff-hl recentf-ext flycheck-pos-tip helm-projectile projectile clean-aindent-mode volatile-highlights duplicate-thing org-noter magit hydra highlight mu4e-alert ox-hugo writeroom-mode anzu flycheck spaceline helm-chronos chronos olivetti multiple-cursors expand-region ace-window auto-minor-mode ledger-mode sublimity auctex smooth-scrolling yasnippet pdf-tools htmlize helm-bibtex free-keys evil color-theme base16-theme)))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (function org-hugo-export-wim-to-md-after-save)
           :append :local)
     nil
     (org-confirm-babel-evaluate)
     (after-save-hook . org-html-export-to-html))))
 '(send-mail-function (quote mailclient-send-it))
 '(size-indication-mode t)
 '(smtpmail-smtp-server "127.0.0.1")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
