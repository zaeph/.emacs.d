;;; init.el --- Initialisation file for Emacs -*- fill-column: 78; lexical-binding: t; byte-compile-warnings: (not free-vars); auto-compile-display-buffer: nil -*-

;; Copyright © 2013-2024 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; URL: https://github.com/zaeph/.emacs.d
;; Keywords: emacs, init, init.el, dotfiles
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ===============================  INIT FILE  ===============================
;; ==================================== * ====================================
;; ===============================  ~ zaeph ~  ===============================
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; Commentary:
;;
;; This is my custom configuration for Emacs.

;;; Code:

;; Catch the curse
(defvar zp/original-init-finished nil
  "True when init has finished loading.")
(when zp/original-init-finished
  (message "[CURSE TRIGGERED]")
  (debug))

;; Change default face to prevent flashing
(set-face-attribute 'default nil :foreground "#bcaf8e" :background "#141414")

;; EmacsConf
;; (setq org-link-shell-confirm-function
;;       'yes-or-no-p)
(setq org-link-shell-confirm-function
      (lambda (&rest _arg) t))
;; (setq conf-directory "~/projects/emacsconf-wiki/" "Directory where the wiki files are.")

(setq default-directory "~")
(setq inhibit-startup-screen 1)
(setq initial-scratch-message ";; Emacs Scratch\n\n")

;; Adjust GC thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold gc-cons-threshold)	;Default: 800000
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Increase buffer-size subprocesses
(setq read-process-output-max (* 1024 1024))		;Default: 4096

;; Undo parameters
(setq undo-limit 16000000
      undo-strong-limit 24000000
      undo-outer-limit 24000000)

;; (toggle-debug-on-error)
;; (toggle-debug-on-quit)
;; (setq garbage-collection-messages t)

;; Alias the longform of ‘y-or-n-p’
(defalias 'yes-or-no-p 'y-or-n-p)

;; Put customisations in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(add-to-list 'load-path custom-file)
(load-file custom-file)

;; Show current filename in titlebar
(setq frame-title-format "%b")

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set default fill column to 78
(setq-default fill-column 78)

;; Enable `global-hl-line-mode'
(global-hl-line-mode 1)

;; Enable `subword-mode'
(global-subword-mode 1)

;; Suppress cl deprecation warnings
(setq byte-compile-warnings '(cl-functions))

;; Supress some warnings
(setq warning-minimum-level :error)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; Toggle for `warning-minimum-level'
(defun zp/toggle-warning-minimum-level ()
  (interactive)
  (setq warning-minimum-level
        (pcase warning-minimum-level
          (:error
           :warning)
          (:warning
           :error)))
  (message (format "warning-minimum-level set to %s" warning-minimum-level)))

;; Add folders to load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/extra/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'load-path "~/projects/org-mode/lisp/")
;; (add-to-list 'load-path "~/projects/magit/lisp/")
(add-to-list 'load-path "~/projects/crdt.el/")
(add-to-list 'load-path "~/projects/mpv.el/")
(add-to-list 'load-path "~/projects/waveform-el/")

;; Point to my Emacs fork for studying built-in functions
(setq source-directory "~/projects/emacs/")

;; Time
(setq display-time-24hr-format t)

;; Avoid loading older byte-compiled version
(setq load-prefer-newer t)

;; Pixel-accurate resize
(setq frame-resize-pixelwise t)

;; Turn off background when Emacs is run with -nt
(defun on-after-init ()
  "Turn off background."
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Auto-update copyright
(add-hook 'after-save-hook #'copyright-update)

;; Force horizontal splitting
;; (setq split-width-threshold 9999)    ;Default: 160

;; Suppress warning when opening large files
(setq large-file-warning-threshold nil)

;; Configure ‘display-buffer’ behaviour for some special buffers
(setq display-buffer-alist
      `(;; Messages, errors, processes, Calendar in the bottom side window
        (,(rx bos (or "*Apropos"                ; Apropos buffers
                      "*Man"                    ; Man buffers
                      ;; "*Help"                   ; Help buffers
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
        ("." nil (reusable-frames . nil))))

;; Enable disabled commands
(setq disabled-command-function nil)

;; Do not display continuation lines
(set-default 'truncate-lines t)

;; Enable line-restricted horizontal scrolling
(setq auto-hscroll-mode 'current-line)
(setq auto-hscroll-mode 't)
;; (setq auto-hscroll-mode 't)

;; Disable final newline insertion
(setq-default require-final-newline nil)

;; Enforce French spacing when filling paragraphs
(add-to-list 'fill-nobreak-predicate 'fill-french-nobreak-p)

;; Disable mouse focus
(setq focus-follows-mouse nil)
(setq mouse-autoselect-window nil)

;; Use a subtle visible bell
(defun zp/subtle-visible-bell ()
  "A more subtle visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function #'zp/subtle-visible-bell)

;; Suppress bells for reaching beginning and end of buffer
;; Source: https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer/20039
(defun zp/command-error-function (data context signal)
  "Ignore some errors.
Ignore the `buffer-read-only', `beginning-of-buffer',
`end-of-buffer' signals; pass the rest to the default handler.
For details on DATA, CONTEXT, and SIGNAL, see
`command-error-function'."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context signal)))

(setq command-error-function #'zp/command-error-function)

;; Maximise the frame
(toggle-frame-maximized)

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Temporary fix for helm delays
(when (= emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))

;; Enable recursive minibuffers
;; Necessary for some Ivy/Helm commands
(setq enable-recursive-minibuffers t)

;; Make M-U equivalent to C-u
;; Commented out because I can’t remember why it was necessary
;; (global-set-key (kbd "M-U") 'universal-argument)
;; (define-key universal-argument-map "\M-U" 'universal-argument-more)

;; Prevent newlines insertion when moving past the end of the file
(setq next-line-add-newlines nil)

;; Prefer horizontal splits with edif
(setq ediff-split-window-function 'split-window-horizontally)

;; Command history
(setq list-command-history-max 256)

(defun tab-to-tab-stop-prev ()
  "Go to previous tab stop and delete horizontal space."
  (interactive)
  (let ((nexttab (indent-next-tab-stop (current-column) t)))
    (delete-horizontal-space t)
    (indent-to nexttab)))

;; Safe local-variables
;; (setq safe-local-variable-values '((org-confirm-babel-evaluate)
;;                                    (eval require 'org-roam-dev)
;;                                    ;; (org-roam-directory . "~/org/slip-box-testing/")
;;                                    ;; (org-roam-directory . "~/projects/erg-notes/")
;;                                    ;; (org-roam-db-location . "./org-roam.db")
;;                                    ))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------------------------------------
;; Debugging functions
;;----------------------------------------------------------------------------
(require 'cl-lib)
;; Taken from https://alphapapa.github.io/emacs-package-dev-handbook/
;; (cl-defmacro debug-warn (&rest args)
;;   "Display a debug warning showing the runtime value of ARGS.
;; The warning automatically includes the name of the containing
;; function, and it is only displayed if `warning-minimum-log-level'
;; is `:debug' at runtime (which avoids formatting messages that
;; won't be shown).

;; Each of ARGS may be a string, which is displayed as-is, or a
;; symbol, the value of which is displayed prefixed by its name, or
;; a Lisp form, which is displayed prefixed by its first symbol.

;; Before the actual ARGS arguments, you can write keyword
;; arguments, i.e. alternating keywords and values.  The following
;; keywords are supported:

;; :buffer BUFFER   Name of buffer to pass to `display-warning'.
;; :level  LEVEL    Level passed to `display-warning', which see.
;;                  Default is :debug."
;;   (pcase-let* ((fn-name (with-current-buffer
;;                             (or byte-compile-current-buffer (current-buffer))
;;                           ;; This is a hack, but a nifty one.
;;                           (save-excursion
;;                             (beginning-of-defun)
;;                             (cl-second (read (current-buffer))))))
;;                (plist-args (cl-loop while (keywordp (car args))
;;                                     collect (pop args)
;;                                     collect (pop args)))
;;                ((map (:buffer buffer) (:level level)) plist-args)
;;                (level (or level :debug))
;;                (string (cl-loop for arg in args
;;                                 concat (pcase arg
;;                                          ((pred stringp) "%S ")
;;                                          ((pred symbolp)
;;                                           (concat (upcase (symbol-name arg)) ":%S "))
;;                                          ((pred listp)
;;                                           (concat "(" (upcase (symbol-name (car arg)))
;;                                                   (pcase (length arg)
;;                                                     (1 ")")
;;                                                     (_ "...)"))
;;                                                   ":%S "))))))
;;     `(when (eq :debug warning-minimum-log-level)
;;        (display-warning ',fn-name (format ,string ,@args) ,level ,buffer))))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;----------------------------------------------------------------------------
;;; Hotfixes
;;----------------------------------------------------------------------------
;; Cancelling the indent-region curse
(defun zp/advice-noop (&rest _args)
  "Noop for advices.
Catches all ARGS and does nothing with them."
  nil)

(defun zp/hotfix-indent-region ()
  "Fix `indent-region' when it’s been nuked by The Curse™."
  (interactive)
  (advice-add 'indent-according-to-mode :before #'zp/advice-noop)
  (advice-remove 'indent-according-to-mode #'zp/advice-noop))

;;----------------------------------------------------------------------------
;; Keys
;;----------------------------------------------------------------------------
;; Swap some keys on modified Qwerty layout
;; ; <=> _
(global-set-key (kbd "M-;") #'nil)
(global-set-key (kbd "M-_") #'comment-dwim)
(global-set-key (kbd "C-;") #'nil)
(global-set-key (kbd "C-_") #'iedit-mode)
(global-set-key (kbd "C-M-;") #'nil)
(global-set-key (kbd "C-M-_") #'iedit-execute-last-modification)
(global-set-key (kbd "C-x ;") #'nil)
(global-set-key (kbd "C-x _") #'comment-set-column)
(global-set-key (kbd "C-x C-;") #'nil)
(global-set-key (kbd "C-x C-_") #'comment-line)
;; ’ <=> ’
(global-set-key (kbd "M-’") #'abbrev-prefix-mark)
(global-set-key (kbd "M-'") #'nil)
(global-set-key (kbd "C-’") #'nil)
(global-set-key (kbd "C-'") #'undo)
(global-set-key (kbd "C-M-’") #'nil)
(global-set-key (kbd "C-M-'") #'undo-redo)
(global-set-key (kbd "C-x ’") #'expand-abbrev)
(global-set-key (kbd "C-x '") #'nil)

;; Define keymap for minor mode toggles
(define-prefix-command 'zp/toggle-map)
(define-key ctl-x-map "t" 'zp/toggle-map)

(define-key zp/toggle-map (kbd "d") #'toggle-debug-on-error)
(define-key zp/toggle-map (kbd "q") #'toggle-debug-on-quit)
(define-key zp/toggle-map (kbd "Q") #'electric-quote-local-mode)
(define-key zp/toggle-map (kbd "F") #'flyspell-mode)
(define-key zp/toggle-map (kbd "a") #'auto-fill-mode)
(define-key zp/toggle-map (kbd "l") #'display-line-numbers-mode)
(define-key zp/toggle-map (kbd "h") #'global-hl-line-mode)
(define-key zp/toggle-map (kbd "p") #'print-circle-mode)
(define-key zp/toggle-map (kbd "s") #'so-long-mode)
(define-key zp/toggle-map (kbd "S") #'scroll-bar-mode)
(define-key zp/toggle-map (kbd "g") #'glasses-mode)
(define-key zp/toggle-map (kbd "G") #'zp/global-super-glasses-mode)
(define-key zp/toggle-map (kbd "w") #'zp/toggle-warning-minimum-level)
(define-key zp/toggle-map (kbd "v") #'visual-line-mode)

(define-key help-map (kbd "h") #'zp/switch-to-help)

;; Modes
(global-set-key (kbd "C-c H") #'global-hl-line-mode)
(global-set-key (kbd "M-U") #'visual-line-mode)

;; Exit Emacs with ‘C-x r q’, and kill the current frame with ‘C-x C-c’
(global-set-key (kbd "C-x r q") #'save-buffers-kill-terminal)
(global-set-key (kbd "C-x r c") #'delete-frame)
(global-set-key (kbd "C-x C-c") #'zp/delete-frame-ask)

;; Actions

(global-set-key (kbd "M-I") #'tab-to-tab-stop-prev)
(global-set-key (kbd "M-g M-i") #'imenu)
(global-set-key (kbd "C-;") #'undo-redo)
(global-set-key (kbd "M-SPC") #'delete-horizontal-space)
(global-set-key (kbd "M-S-SPC") #'just-one-space)
(global-set-key (kbd "s-.") #'zp/echo-buffer-name)
(global-set-key (kbd "C-x F") #'zp/unfill-document)
(global-set-key (kbd "M-Q") #'zp/unfill-dwim)
(global-set-key (kbd "C-x B") #'rename-buffer)
(global-set-key (kbd "M-o") #'mode-line-other-buffer)
(global-set-key (kbd "s-j") #'other-window)
(global-set-key (kbd "s-k") #'other-window-reverse)
(global-set-key (kbd "s-J") #'toggle-window-split)
(global-set-key (kbd "C-x %") #'map-query-replace-regexp)
(global-set-key (kbd "C-x 4 1") #'zp/kill-other-buffer-and-window)

;; Ignore Kanji key in IME
(global-set-key [M-kanji] 'ignore)

;;----------------------------------------------------------------------------
;; Cosmetics
;;----------------------------------------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(show-paren-mode 1)
(column-number-mode 1)

;; Set fringe sizes
(fringe-mode 20)


;;----------------------------------------------------------------------------
;; Backups
;;----------------------------------------------------------------------------
;; Don’t clobber symlinks
(setq backup-by-copying t)

;; Use versioned backups
(setq version-control t)

;; Number of backups to keep
(setq kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t)

;; Backup directories
(setq backup-directory-alist '(("." . "~/.saves")))

;; Also backup versioned files
(setq vc-make-backup-files t)

;;----------------------------------------------------------------------------
;; Setup package repositories
;;----------------------------------------------------------------------------
(require 'init-elpa)
(require 'init-use-package)

(use-package package
  :bind ("C-c P" . package-list-packages))

(use-package bind-key)
(use-package diminish)

(use-package init-utils
  :commands (zp/get-string-from-file))
(use-package zp-timer-macs)
(use-package init-edit-utils)
(use-package zp-scratch-dir)
(use-package zp-always-centred
  :commands (zp/always-centred-mode)
  :bind ("M-Y" . #'zp/always-centred-mode))

(use-package diff)

;; Libraries
(use-package dash)
(use-package s)

(use-package repeat
  :init
  (repeat-mode 1)
  :config
  (defvar transpose-lines-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-t") #'transpose-lines)
      map))
  (put 'transpose-lines 'repeat-map 'transpose-lines-repeat-map))

(use-package simple
  :bind (([remap just-one-space] . #'cycle-spacing)
         ([remap upcase-word] . #'upcase-dwim)
         ([remap downcase-word] . #'downcase-dwim)
         ([remap capitalize-word] . #'capitalize-dwim)
         ([remap count-words-region] . #'count-words)))

(use-package glasses
  :hook ((prog-mode . glasses-mode))
  :config
  (defun zp/glasses-init ()
    "Initialize `glasses-mode' with default config."
    (setq glasses-face 'bold
          glasses-original-separator ""
          glasses-separator ""
          glasses-separate-parentheses-p nil)
    (glasses-set-overlay-properties))

  (defun zp/super-glasses-init ()
    "Initialize `zp/super-glasses-mode' with default config."
    (setq glasses-original-separator "_"
          glasses-separator "_")
    (glasses-set-overlay-properties))

  (zp/glasses-init)

  (define-minor-mode zp/global-super-glasses-mode
    "Custom `glasses-mode' with stronger emphasis than default settings."
    :global
    :lighter " s^s"
    (cond (zp/global-super-glasses-mode
           (zp/super-glasses-init))
          (t
           (zp/glasses-init)))))

(use-package hs-mode
  :hook ((prog-mode . hs-minor-mode)))

;;----------------------------------------------------------------------------
;;; EmacsConf

;;----------------------------------------------------------------------------
;; (use-package emacsconf
;;   :load-path "~/projects/emacsconf-el/"
;;   :config
;;   (setq emacsconf-org-file "~/projects/emacsconf-2022-private/conf.org")
;;   (setq emacsconf-ansible-directory "~/projects/emacsconf-ansible/"))

(use-package subed
  :load-path "~/projects/subed/subed/")

;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
;; windmove
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

;; desktop
(desktop-save-mode 0)

;; mwheel
(setq mouse-wheel-flip-direction 1
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control)))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Disable side movements
;; (global-set-key (kbd "<mouse-6>") 'ignore)
;; (global-set-key (kbd "<mouse-7>") 'ignore)
;; (global-set-key (kbd "<triple-mouse-7>") 'ignore)
;; (global-set-key (kbd "<triple-mouse-6>") 'ignore)

;; Time
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; EPG
(setq mml2015-use 'epg
      epg-user-id (zp/get-string-from-file "~/org/pp/gpg/gpg-key-id")
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t)

(use-package electric
  :init
  (electric-pair-mode +1))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package zp-presentation
  :bind (:map zp/toggle-map
         ("P" . zp/presentation-mode)))

(use-package crdt
  :commands (crdt-connect
             crdt-share-buffer))

(use-package iso-transl)

(use-package init-auto-compile)

(use-package init-eldoc)

(use-package secret)

(use-package secret-qomon)

(use-package init-cl)

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-jump))
  :config
  (unbind-key "M-o" 'ibuffer-mode-map))

(use-package init-server)

;;----------------------------------------------------------------------------
;; magit
;;----------------------------------------------------------------------------
(use-package init-magit
  :bind (("s-m" . magit-status)
         ("s-b" . magit-blame-addition)
         ("C-c g" . magit-file-dispatch)))

(use-package zp-magit
  :bind (("s-M-m" . zp/magit-stage-file-and-commit)
         :map git-rebase-mode-map
         ("h" . zp/change-commit-author)))

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist '("gitlab.it.imagino.com" "gitlab.it.imagino.com/api/v4" "gitlab.it.imagino.com" forge-gitlab-repository)))

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------
(use-package init-auth-source)

(use-package helpful)

(use-package gif-screencast
  :commands (gif-screencast)
  :bind (("<f8>" . gif-screencast-toggle-pause)
         ("<f9>" . gif-screencast-stop))
  :config
  (setq gif-screencast-output-directory "/home/zaeph/capture/emacs/"))

(use-package init-evil)

(use-package rg
  :commands rg
  :bind (("M-s ," . rg-dwim)
         ("s-SPC" . rg)))

;; For handling encryption
(use-package init-epa-file)

(use-package isearch
  :bind (:map isearch-mode-map
         ("<backspace>" . 'isearch-del-char)))

;; fcitx (IME for CJK)
;; Disabled because of slow-downs in combination with visual-line-mode
;; (fcitx-aggressive-setup)

(use-package ox-hugo)

(use-package edit-indirect)

(use-package package-lint)

(use-package init-duplicate-thing
  :bind (("M-J" . zp/duplicate-thing)))

(use-package init-volatile-highlights)

(use-package init-beacon
  :demand
  :bind (:map zp/toggle-map
         ("b" . beacon-mode)))

;; ;; Removed because of conflict with ‘use-hard-newlines’
;; (use-package clean-aindent-mode
;;   :config
;;   (add-hook 'prog-mode-hook #'clean-aindent-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package whitespace
  :bind (("C-c w" . zp/whitespace-mode-lines-tail)
         ("C-c W" . whitespace-mode))
  :hook (prog-mode . zp/whitespace-mode-lines-tail)
  :config
  (defun zp/whitespace-mode-lines-tail ()
    (interactive)
    (unless (string-match " markdown-code-fontification:" (buffer-name))
      ;; TODO: Clean up
      (if (bound-and-true-p whitespace-mode)
          (progn
            (whitespace-mode -1)
            (message "Whitespace mode disabled in current buffer"))
        (let ((whitespace-style '(face trailing lines-tail))
              (whitespace-line-column nil))
          (whitespace-mode t)
          (message "Whitespace mode enabled in current buffer"))))))

(use-package interaction-log
  :bind (:map zp/toggle-map
         ("I" . interaction-log-mode)))

;; (use-package info+
;;   :load-path "~/.emacs.d/pkg/emacswiki.org/info+.el"
;;   :bind (:map Info-mode-map
;;          ("<mouse-4>" . mwheel-scroll)
;;          ("<mouse-5>" . mwheel-scroll)
;;          ("j" . next-line)
;;          ("k" . previous-line)))

(use-package recentf-ext)

(use-package dired
  :bind ("C-x C-d" . dired)
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :custom
  (dired-dwim-target t))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package eyebrowse)

(use-package which-key
  :config
  (which-key-mode)
  ;; (setq which-key-idle-delay 1) ;Default
  )

(use-package lilypond-mode)

(use-package el-patch
  :config
  (setq el-patch-enable-use-package-integration t))

(use-package ol
  :config
  (global-set-key (kbd "C-c L") #'org-store-link))

(use-package ox
  :requires org
  :config
  (setq org-export-in-background t
        org-export-with-sub-superscripts nil)

  (defun zp/ox-pandoc-convert-region-html-to-org ()
    "Convert region from html to org."
    (interactive)
    (shell-command-on-region (region-beginning) (region-end) "pandoc -f html -t org" nil t))

  (defun zp/ox-pandoc-zotero-convert-citation ()
    "Convert Zotero citation from html to org."
    (interactive)
    (zp/ox-pandoc-convert-region-html-to-org)
    (zp/unfill-region)
    (query-replace-regexp "\\. \\([^\s-]\\)" ".  \\1"
                          nil (region-beginning) (region-end)))

  (defun zp/ox-pandoc-convert-clipboard-html-to-org ()
    "Convert clipboard contents from HTML to Org and then paste (yank)."
    (interactive)
    (kill-new (shell-command-to-string "xclip -o -t text/html | pandoc -f html -t org"))
    (yank)
    (zp/unfill-region)))

(use-package ox-org
  :requires ox)

(use-package ox-texinfo+
  :requires ox
  :load-path "~/projects/ox-texinfo-plus/")

(use-package org-mind-map
  :disabled)

(use-package exwm-config)

(use-package exwm
  :disabled
  :requires exwm-config
  :config
  (exwm-config-default))

;; so-long
(use-package so-long
  :hook (debugger-mode . so-long-minor-mode)
  :config
  (global-so-long-mode 1))

(use-package sh-script
  :mode (("\\zshrc\\'" . shell-script-mode)
         ("\\prompt_.*_setup\\'" . shell-script-mode)))

(use-package neotree)

(use-package fish-mode
  :mode "\\.fish\\'")

;; EmacsConf stuff
;; (use-package mpv)
;; (use-package waveform)

(use-package burly
  :bind (("C-x r W" . burly-bookmark-windows)
         ("C-x r F" . burly-bookmark-frames)))

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
         ([C-tab] . bicycle-cycle)
         ([C-S-tab] . bicycle-cycle-global)))

(use-package prog-mode
  ;; Force fringe indicators
  :hook ((prog-mode . zp/enable-visual-line-fringe-indicators)
         (prog-mode . outline-minor-mode)
         (prog-mode . hs-minor-mode))
  :config
  (defun zp/enable-visual-line-fringe-indicators ()
    "Enablle visual-line fringe-indicators."
    (setq-local visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))))

(use-package hide-mode-line
  :bind (:map zp/toggle-map
         ("m" . hide-mode-line-mode)))

(use-package free-keys
  :config
  (setq free-keys-modifiers '("" "C" "M" "C-M" "H")))

(use-package flycheck
  :hook ((sh-mode . flycheck-mode)
         (cperl-mode . flycheck-mode)
         (lispy-mode . flycheck-mode)
         (typescript-mode . flycheck-mode)
         (haskell-mode . flycheck-mode)
         (rustic-mode . flycheck-mode)
         ;; Enable flycheck everywhere
         ;; Disabled because of slow-downs in large files
         ;; (after-init . global-flycheck-mode)
         )
  :bind (:map zp/toggle-map
         ("f" . flycheck-mode))
  :config
  ;; (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-delay 0.6))

;; Minor-mode to show Flycheck error messages in a popup
(use-package flycheck-pos-tip
  :requires flycheck
  :config
  ;; (flycheck-pos-tip-mode)
  )

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (setq flycheck-posframe-position 'window-bottom-left-corner))

(use-package lisp-mode
  :custom
  ;; (lisp-indent-function #'zp/lisp-indent-function)
  (lisp-indent-function #'zp/lisp-indent-function)
  :config
  ;; Taken from https://github.com/CeleritasCelery/emacs.d/blob/master/emacs.org#indent
  ;; TODO: Use el-patch to catch upstream modifications
  (defun zp/lisp-indent-function (indent-point state)
    "Override `lisp-indent-function' to properly handle plists.
For a full description, see the original function."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state)))))))))

(use-package lispy
  :load-path "~/projects/lispy"
  :bind (:map lispy-mode-map
         ("_" . lispy-comment))
  :hook ((emacs-lisp-mode . lispy-mode)
         (lisp-mode . lispy-mode)
         (lisp-data-mode . lispy-mode)
         (slime-repl-mode . lispy-mode))
  :config
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
        (list (lambda () (not (eq major-mode org-mode))))))

(use-package zp-lispy
  :requires lispy
  :config
  ;; (use-package lispy
  ;;   :bind (:map lispy-mode-map
  ;;          ("M-." . zp/lispy-goto-symbol-ibuf)))
  )

(use-package eros
  :hook (emacs-lisp-mode . eros-mode))

(use-package nov
  :hook (nov-mode . zp/variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

(use-package olivetti
  :bind (:map zp/toggle-map
         ("M-O" . olivetti-mode))
  :config
  (setq-default olivetti-body-width 0.6
                olivetti-minimum-body-width 80))

(use-package fountain-mode
  :config
  (setq fountain-export-font "Courier Prime")
  (setq fountain-mode-hook '(turn-on-visual-line-mode
                             fountain-outline-hide-custom-level
                             olivetti-mode)))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (global-set-key (kbd "s-<backspace>") 'yas-prev-field)

  ;; Helpers
  (defun zp/yasnippet-sh-getopts (text)
    "Snippet for expanding getopts statements."
    (let ((format-string
           (string-join '("%c)\n#TODO: Implement"
                          "echo \"Unimplemented\""
                          "exit 0"
                          ";;")
                        "\n")))
      (mapconcat (lambda (c)
                   (format format-string c))
                 (replace-regexp-in-string ":" "" text)
                 "\n"))))

(use-package winner
  :bind (("s-u" . winner-undo)
         ("s-i" . winner-redo))
  :config
  (winner-mode 1))

(use-package ace-link
  ;; :load-path "~/projects/ace-link/"
  :init
  (ace-link-setup-default))

(use-package ace-window
  :bind ("s-y" . ace-window)
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)

  ;; Source: https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro zp/embark-ace-action (fn)
      `(defun ,(intern (concat "zp/embark-ace-" (symbol-name fn))) ()
         "Dispatch buffer with `ace-window'."
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (with-eval-after-load 'embark
    (define-key embark-file-map (kbd "o") (zp/embark-ace-action find-file))
    (define-key embark-buffer-map (kbd "o") (zp/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (zp/embark-ace-action bookmark-jump))))

(use-package avy
  :bind (;; ("s-n" . avy-goto-goto-word-1)
         ;; ("s-n" . avy-goto-goto-char)
         ("s-n" . avy-goto-char-timer)))

(use-package dumb-jump)

(use-package backup-walker
  :hook (backup-walker-mode . zp/set-diff-backend-git-diff)
  :config
  (defun zp/set-diff-backend-git-diff ()
    "Set diff backend to ‘git diff’.
Modifies ‘diff-command’ and ‘diff-switches’ to use ‘git diff’."
    (setq-local diff-command "git --no-pager diff")
    (setq-local diff-switches "--textconv")))

;; Disabled since Emacs now has a native package for showing
;; line-numbers
(use-package linum
  :disabled
                                        ;Add spaces before and after
  (setq linum-format " %d "))

(use-package vterm
  :ensure t
  :config
  (defun zp/vterm-named (&optional arg)
    "Create a named interactive Vterm buffer.
ARG is to gauge interactivity.
See `vterm' for more info."
    (interactive "P")
    (let* ((name (file-name-directory (buffer-file-name)))
           (vterm-buffer-name (format "*vterm %s*" name)))
      (vterm--internal #'pop-to-buffer-same-window arg))))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode)))

(use-package conf-mode)

(use-package git-link
  :bind (:map prog-mode-map
         ("C-c C-l" . zp/git-link-dwim)
         :map conf-mode-map
         ("C-c C-l" . zp/git-link-dwim))
  :config
  (defun zp/git-link-dwim (arg)
    "Create a URL pointing to current line/region on the branch.
With a C-u argument, point to the commit instead."
    (interactive "P")
    (let ((git-link-use-commit (if arg nil t))
          (current-prefix-arg nil))
      (call-interactively #'git-link)))

  (with-eval-after-load 'magit
    (define-key magit-status-mode-map (kbd "C-c C-l") #'git-link-commit )))

(use-package pdf-tools
  :init
  (pdf-tools-install :no-query)
  :magic ("%PDF" . pdf-view-mode))

(use-package pdf-view
  :config
  (defvar zp/pdf-annot-default-annotation-color "#F1F23B"
    "Default color to use for annotations.")

  (setq zp/pdf-annot-default-annotation-color "#F1F23B")

  (setq pdf-annot-default-annotation-properties
        `((t (label . ,user-full-name))
          (text (icon . "Note") (color . ,zp/pdf-annot-default-annotation-color))
          (highlight (color . "#FFFF4D")) ;Lighter yellow
          (squiggly (color . "orange"))
          (strike-out (color . "red"))
          (underline (color . "blue"))))

  (defun zp/toggle-pdf-view-auto-slice-minor-mode ()
    "Toggle ‘pdf-view-auto-slice-minor-mode’ and reset slice."
    (interactive)
    (call-interactively 'pdf-view-auto-slice-minor-mode)
    (if (not pdf-view-auto-slice-minor-mode)
        (progn
          (pdf-view-reset-slice))))

  ;; Disable continuous view in pdf-view
  ;; I prefer to explicitly turn pages
  (setq pdf-view-continuous nil)

  ;; Automatically activate annotation when they’re created
  (setq pdf-annot-activate-created-annotations t)

  (defvar zp/pdf-view-save-after-annotation nil
    "When non-nil, save the PDF after an annotation is created.")

  ;; Save after creating an annotation
  (defun zp/pdf-view-save-buffer ()
    "Save buffer and preserve midnight state."
    (interactive)
    (call-interactively #'save-buffer)
    (pdf-view-midnight-minor-mode 'toggle)
    (pdf-view-midnight-minor-mode 'toggle))

  (defun zp/pdf-view-save-buffer-maybe ()
    "Save buffer and preserve midnight state."
    (when zp/pdf-view-save-after-annotation
      (zp/pdf-view-save-buffer)))

  (advice-add 'pdf-annot-edit-contents-commit :after 'zp/pdf-view-save-buffer-maybe)

  (defun zp/pdf-view-continuous-toggle ()
    (interactive)
    (cond ((not pdf-view-continuous)
           (setq pdf-view-continuous t)
           (message "Page scrolling: Continous"))
          (t
           (setq pdf-view-continuous nil)
           (message "Page scrolling: Constrained"))))

  (defun zp/pdf-view-open-in-evince ()
    "Open the current PDF with ‘evince’."
    (interactive)
    (save-window-excursion
      (let ((current-file (buffer-file-name))
            (current-page (number-to-string (pdf-view-current-page))))
        (async-shell-command
         (format "evince -i %s \"%s\"" current-page current-file))))
    (message "Sent to Evince"))

  (defun zp/pdf-view-open-in-xournalpp ()
    "Open the current PDF with ‘xournalpp’."
    (interactive)
    (save-window-excursion
      (let ((current-file (buffer-file-name)))
        (async-shell-command
         (format "xournalpp \"%s\"" current-file))))
    (message "Sent to Xournal++"))

  (defun zp/pdf-view-show-current-page ()
    "Show the current page."
    (interactive)
    (message "Page: %s" (pdf-view-current-page)))

  ;;--------------------
  ;; Custom annotations
  ;;--------------------

  (defun zp/pdf-annot-add-custom-annotation (type color &optional icon)
    "Add custom annotation with ICON and COLOR."
    (let* ((icon (or icon "Note"))
           (color (or color zp/pdf-annot-default-annotation-color))
           (pdf-annot-default-annotation-properties
            `((t (label . ,user-full-name))
              ,(pcase type
                 ('text `(text (icon . ,icon) (color . ,color)))
                 ('highlight `(highlight (color . ,color)))))))
      (call-interactively (pcase type
                            ('text #'pdf-annot-add-text-annotation)
                            ('highlight #'pdf-annot-add-highlight-markup-annotation)))))

  (defvar zp/pdf-custom-annot-list nil
    "List of custom annotations and their settings.

Each element in list must be a list with the following elements:
- Name of the function to create
- Key binding
- Name of the icon to use
- Color to use")

  (defun zp/pdf-custom-annot-init ()
    (seq-do
     (lambda (settings)
       (cl-destructuring-bind (name type key icon color) settings
         (let* ((root "zp/pdf-annot-add-text-annotation-")
                (fun (intern (concat root name))))
           (defalias fun
             `(lambda ()
                (interactive)
                (zp/pdf-annot-add-custom-annotation ,type ,color ,icon))
             (format "Insert a note of type ‘%s’." name))
           (when key
             (define-key pdf-view-mode-map
               (kbd key)
               `,fun)))))
     zp/pdf-custom-annot-list))
  (define-prefix-command 'zp/pdf-custom-annot-map)

  (define-key pdf-view-mode-map "a" 'zp/pdf-custom-annot-map)

  (setq zp/pdf-custom-annot-list
        `(("note" 'text "t" "Note" ,zp/pdf-annot-default-annotation-color)
          ("note-blue" 'text "T" "Note" "#389BE6")
          ("insert" 'text "ai" "Insert" "#913BF2")
          ("comment" 'text "c" "Comment" "#389BE6")
          ("comment-red" 'text "ac" "Comment" "#FF483E")
          ("circle" 'text "ay" "Circle" "#38E691")
          ("cross" 'text "an" "Cross" "#FF483E")

          ("hl-red" 'highlight nil nil "#FF7F7F")
          ("hl-blue" 'highlight nil nil "#7FDFFF")
          ("hl-green" 'highlight nil nil "#7FFF7F")
          ("hl-purple" 'highlight nil nil "#967FFF")
          ("hl-orange" 'highlight nil nil "#FFBF7F")))

  (setq pdf-annot-color-history
        '("#FFFF4D" "#FF7F7F" "#7FDFFF" "#7FFF7F" "#967FFF" "#FFBF7F"))

  (defun zp/pdf-annot-add-highlight-markup-annotation (arg &optional activate)
    "Add highlight markup annotation.

This wrapper includes presets which can be accessed with
numerical arguments."
    (interactive "P")
    (let ((pdf-annot-activate-created-annotations (when activate t)))
      (pcase arg
        (1 (zp/pdf-annot-add-text-annotation-hl-red))
        (2 (zp/pdf-annot-add-text-annotation-hl-blue))
        (3 (zp/pdf-annot-add-text-annotation-hl-green))
        (4 (zp/pdf-annot-add-text-annotation-hl-purple))
        (5 (zp/pdf-annot-add-text-annotation-hl-orange))
        (_ (call-interactively #'pdf-annot-add-highlight-markup-annotation))))
    (unless activate
      (zp/pdf-view-save-buffer-maybe)))

  (defun zp/pdf-annot-add-highlight-markup-annotation-and-activate (arg)
    "Add highlight markup annotation and activate it.

This wrapper includes presets which can be accessed with
numerical arguments."
    (interactive "P")
    (zp/pdf-annot-add-highlight-markup-annotation arg t))

  (zp/pdf-custom-annot-init)

  ;;----------
  ;; Bindings
  ;;----------

  (define-key pdf-view-mode-map (kbd "C-x C-s") 'zp/pdf-view-save-buffer)

  (define-key pdf-view-mode-map (kbd "m") 'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map (kbd "P") 'pdf-view-printer-minor-mode)
  (define-key pdf-view-mode-map (kbd "s") 'zp/toggle-pdf-view-auto-slice-minor-mode)
  (define-key pdf-view-mode-map (kbd "M") 'pdf-view-set-slice-using-mouse)
  (define-key pdf-view-mode-map (kbd "C") 'zp/pdf-view-continuous-toggle)
  (define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "f") 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map (kbd "RET") 'zp/pdf-view-open-in-evince)
  (define-key pdf-view-mode-map [(shift return)] 'zp/pdf-view-open-in-xournalpp)
  (define-key pdf-view-mode-map (kbd ".") 'zp/pdf-view-show-current-page)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "h") 'zp/pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "H") 'zp/pdf-annot-add-highlight-markup-annotation-and-activate)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "O") 'org-noter-create-skeleton)

  (define-prefix-command 'slice-map)
  (define-key pdf-view-mode-map (kbd "S") 'slice-map)
  (define-key pdf-view-mode-map (kbd "S b") 'pdf-view-set-slice-from-bounding-box)
  (define-key pdf-view-mode-map (kbd "S m") 'pdf-view-set-slice-using-mouse)
  (define-key pdf-view-mode-map (kbd "S r") 'pdf-view-reset-slice)

  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))

(use-package pdf-annot
  :config
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-c C-k") 'pdf-annot-edit-contents-abort))

(use-package pdf-links
  :config
  (define-key pdf-links-minor-mode-map (kbd "f") 'pdf-view-fit-page-to-window))

;; TODO: Consider deleting this semi-useless minor-mode
(defun zp/save-buffers-kill-terminal-silently ()
  "Save buffer and kill the terminal without printing a message."
  (interactive)
  (save-buffers-kill-terminal t))

(define-minor-mode zp/edit-in-emacs-mode
  "Save buffers silently when exiting."
  :lighter " EiE"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-c") 'zp/save-buffers-kill-terminal-silently)
            (define-key map (kbd "C-c C-k") 'zp/kanji-add-furigana)
            (define-key map (kbd "M-n") 'zp/kanji-add-furigana)
            map)
  (when zp/edit-in-emacs-mode
      (setq-local require-final-newline nil)))

;; Way to enable minor modes based on filenames
;; Added with the package ‘auto-minor-mode-alist’
;; But they can also be added via file-fariables or minor-modes
;; TODO: Adapt this block
(add-to-list 'auto-minor-mode-alist '("^edit-in-emacs.*\\.txt$" . visual-line-mode))
(add-to-list 'auto-minor-mode-alist '("^edit-in-emacs.*\\.txt$" . flyspell-mode))
(add-to-list 'auto-minor-mode-alist '("^edit-in-emacs.*\\.txt$" . zp/edit-in-emacs-mode))

;; (add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . visual-line-mode))
;; (add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . olivetti-mode))
;; (add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . flyspell-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.html" . save-silently-mode))

(defun zp/kanji-add-furigana ()
  "Add furigana to the kanji at point.
If text is selected, adds furigana to the selected kanji instead."
  (interactive)
  (if (not (region-active-p))
      (progn
        (call-interactively 'set-mark-command)
        (call-interactively 'forward-char)))
  (yas-expand-snippet (yas-lookup-snippet "anki-ruby")))

(use-package recentf
  :config
  (setq recentf-max-menu-items 1000)
  (setq recentf-max-saved-items 1000))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-connection-properties '((nil "session-timeout" nil)))
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not
                (let ((method (file-remote-p name 'method)))
                  (when (stringp method)
                    (member method '("su" "sudo")))))))))

(use-package sgml-mode
  :bind (("C-c C-=" . increment-integer-at-point)
         ("C-c C--" . decrement-integer-at-point))
  :bind (:map mhtml-mode-map
         ("M-o" . nil)))

(use-package mgml-mode
  :config
  (unbind-key "M-o" ))

(use-package web-mode
  :mode (("\\.tmpl\\'" . web-mode))
  :config
  (setq web-mode-enable-engine-detection t)
  (setq web-mode-engines-alist nil)
  ;; (setq web-mode-engines-alist
  ;;       '(("go" . "\\.tmpl\\'")))
  )

;; (use-package realgud
;;   :config
;;   (setq realgud-safe-mode nil))

(use-package picture
  :config
  (global-set-key (kbd "C-c \\") #'picture-mode))

(use-package hidpi-fringe-bitmaps)

(use-package strokes
  :config
  ;; Draw strokes with RMB
  (global-set-key (kbd "<down-mouse-3>") 'strokes-do-stroke)
  ;; Don't draw strokes to the screen)
  (setq strokes-use-strokes-buffer t))

(use-package thingatpt
  :bind (("C-c C-=" . increment-integer-at-point)
         ("C-c C--" . decrement-integer-at-point))
  :config
  (defun thing-at-point-goto-end-of-integer ()
    "Go to end of integer at point."
    (save-match-data
      ;; Skip over optional sign
      (when (looking-at "[+-]")
        (forward-char 1))
      ;; Skip over digits
      (skip-chars-forward "[[:digit:]]")
      ;; Check for at least one digit
      (unless (looking-back "[[:digit:]]" nil)
        (error "No integer here"))))
  (put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

  (defun thing-at-point-goto-beginning-of-integer ()
    "Go to end of integer at point."
    (save-match-data
      ;; Skip backward over digits
      (skip-chars-backward "[[:digit:]]")
      ;; Check for digits and optional sign
      (unless (looking-at "[+-]?[[:digit:]]")
        (error "No integer here"))
      ;; Skip backward over optional sign
      (when (looking-back "[+-]" nil)
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
    (increment-integer-at-point (- (or dec 1)))))

(use-package highlight-indent-guides
  :bind (:map zp/toggle-map
         ("c" . highlight-indent-guides-mode))
  :hook ((prog-mode . highlight-indent-guides-mode)
         (hyperlist-mode . highlight-indent-guides-mode))
  :config
  ;; Theme configuration is done handled by theme.el
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc 20)

  ;; (setq highlight-indent-guides-method 'character
  ;;       highlight-indent-guides-responsive 'top
  ;;       highlight-indent-guides-character ?·
  ;;       highlight-indent-guides-auto-character-face-perc 30)
  )

;;----------------------------------------------------------------------------
;; Shortcuts
;;----------------------------------------------------------------------------
;; TODO: Consider optimising this section

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

(setq init-file-user "~/.emacs.d/init.el")

(defun zp/find-init-file ()
  "Find the init-file."
  (interactive)
  (find-file init-file-user))

(global-set-key (kbd "C-c e") #'zp/find-init-file)

;;----------------------------------------------------------------------------
;; Testing
;;----------------------------------------------------------------------------
(use-package buttercup)

;;----------------------------------------------------------------------------
;; ispell
;;----------------------------------------------------------------------------
(use-package ispell
  :bind ("C-c d" . zp/ispell-switch-dict)
  :config
  ;; TODO: Modernise

  ;; Use aspell as the backend
  (setq-default ispell-program-name "aspell")

  ;; Allow curvy quotes to be considered as regular apostrophe
  (setq ispell-local-dictionary-alist
        `(("english" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB-ize-w_accents") nil utf-8)
          ("french" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "fr_FR") nil utf-8))))

(use-package zp-ispell
  :custom
  (ispell-dictionary "english")
  (zp/ispell-completion-data '(("en" . "english")
                               ("fr" . "french"))))

(use-package flyspell
  :bind (("C-c f" . flyspell-mode)
         :map flyspell-mode-map
         ;; Unbind `flyspell-auto-correct-previous-word'
         (("C-;" . nil)))
  :hook ((message-setup . flyspell-mode)
         (org-mode . flyspell-mode)
         (latex-mode . flyspell-mode)
         (fountain-mode . flyspell-mode)))

;;----------------------------------------------------------------------------
;; RSS
;;----------------------------------------------------------------------------
(use-package elfeed
  :bind ("C-x w" . elfeed))

;;----------------------------------------------------------------------------
;; Email
;;----------------------------------------------------------------------------
(use-package message
  :hook ((message-setup . zp/message-flyspell-auto)
         ;; (message-setup . electric-quote-local-mode)
         ;; (message-mode-hook . footnote-mode)
         )
  :custom
  (message-mark-insert-begin
   "--------------------------------[START]--------------------------------\n")
  (message-mark-insert-end
   "\n---------------------------------[END]---------------------------------")
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-sendmail-envelope-from 'header)
  (message-kill-buffer-on-exit t)
  :config
  ;; Enforce f=f in message-mode
  ;; Disabled because it’s bad practice according to the netiquette
  ;; (setq mml-enable-flowed t)
  ;; (defun zp/message-mode-use-hard-newlines ()
  ;;   (use-hard-newlines t 'always))
  ;; (add-hook 'message-mode-hook #'zp/message-mode-use-hard-newlines)
  )

(use-package zp-message
  :custom
  (zp/message-sigs-directory "~/org/sig/")
  :config
  (setq message-signature #'zp/message-get-signature
        message-sendmail-envelope-from 'header))

(use-package sendmail
  :after message
  :config
  (setq send-mail-function 'sendmail-send-it))

(use-package notmuch
  :bind (("s-M-l" . notmuch)
         :map notmuch-search-mode-map
         ("g" . notmuch-refresh-this-buffer)
         :map notmuch-show-mode-map
         ("C-c C-o" . goto-address-at-point))
  :hook ((notmuch-hello-refresh . zp/color-all-inboxes)
         ;; (notmuch-message-mode . electric-quote-local-mode)
         )
  :config
  (use-package zp-notmuch)
  (setq notmuch-always-prompt-for-sender t)

  (setq-default notmuch-search-oldest-first nil)

  (define-key notmuch-search-mode-map "d"
    (lambda (&optional untrash beg end)
      "mark thread as spam"
      (interactive (cons current-prefix-arg (notmuch-interactive-region)))
      (if untrash
          (notmuch-search-tag (list "-deleted"))
        (notmuch-search-tag (list "+deleted" "-inbox")) beg end)
      (notmuch-search-next-thread)))

  (define-key notmuch-show-mode-map "d"
    (lambda ()
      "mark thread as spam"
      (interactive (notmuch-interactive-region))
      (notmuch-show-tag (list "+deleted" "-inbox" "-draft"))
      (notmuch-show-next-thread-show)))

  (setq mail-host-address "hidden")

  ;;------------------------------------------------
  ;; Highlight inbox if it contains unread messages
  ;;------------------------------------------------

  (defun zp/color-inbox-if-unread (inbox &optional search)
    "Color INBOX if SEARCH matches any unread message in inbox.

INBOX is the name of the saved search to highlight.

SEARCH is a string to be interpreted by notmuch-search."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* ((query-base "tag:inbox and tag:unread")
             (query (if search
                        (concat "\("
                                search
                                "\)"
                                " and "
                                "\("
                                query-base
                                "\)")
                      query-base))
             (cnt (car (process-lines "notmuch" "count" query))))
        cnt
        (when (> (string-to-number cnt) 0)
          (when (search-forward inbox (point-max) t)
            (let* ((overlays (overlays-in (match-beginning 0) (match-end 0)))
                   (overlay (car overlays)))
              (when overlay
                (overlay-put overlay 'face '((:inherit bold)
                                             (:foreground "red")
                                             (:underline t))))))))))

  (defun zp/color-all-inboxes ()
    (zp/color-inbox-if-unread "inbox" "not (tag:auto or tag:list")
    (zp/color-inbox-if-unread "pro-inbox" "tag:pro")
    (zp/color-inbox-if-unread "lists-inbox" "tag:list")
    (zp/color-inbox-if-unread "dev-github-inbox" "tag:github")
    (zp/color-inbox-if-unread "dev-forum-inbox" "tag:forum")))

(use-package zp-notmuch
  :after notmuch
  :bind (:map notmuch-show-mode-map
         ("v" . zp/notmuch-view-html)
         :map notmuch-search-mode-map
         ("v" . zp/notmuch-search-view-html)
         :map notmuch-message-mode-map
         (("C-c C-c" . zp/notmuch-confirm-before-sending)
          ("C-c C-s" . zp/notmuch-confirm-before-sending)
          ("C-c C-b" . message-goto-body)
          ("C-c C-." . zp/message-goto-body-end)
          ("M-<" . zp/message-goto-top)
          ("M->" . zp/message-goto-bottom)
          ("C-c C-z" . zp/message-kill-to-signature)))
  :commands (notmuch
             zp/notmuch-identities-get)
  :custom
  (zp/notmuch-fcc-tags-default "-inbox +sent -unread")
  :config
  (setq notmuch-fcc-dirs (zp/notmuch-make-fcc-dirs)
        zp/message-ispell-alist (zp/notmuch-make-ispell-alist)
        zp/message-sigs-alist (zp/notmuch-make-sigs-alist)
        zp/notmuch-saved-queries
        '(("imagino"          . "tag:imagino and not tag:auto")
          ;; ("qomon"              . "tag:qomon and not tag:auto")
          ("dev"              . "tag:dev and not tag:auto")
          ("beta"             . "(tag:list or tag:auto or tag:news)")
          ("lists"            . "tag:list and not tag:auto")
          ("list-emacs-devel" . "(tag:list and tag:emacs-devel) and not tag:auto")
          ("list-emacs-fr" . "(tag:list and tag:emacs-fr) and not tag:auto")
          ("list-org"         . "(tag:list and tag:org) and not tag:auto")
          ("news"             . "tag:news and not tag:auto")
          ("dev-github"       . "tag:dev and tag:github and tag:auto")
          ("dev-forum"        . "tag:dev and tag:forum and tag:auto"))
        notmuch-saved-searches
        `((:name "inbox" :key "i" :query "tag:inbox and not (tag:auto or tag:list or tag:news)")
          (:name "unread" :key "u" :query "tag:unread and not tag:auto")
          (:name "archive-week" :key "a" :query "date:\"7d..today\" and not tag:auto")
          (:name "archive" :key "A" :query "not tag:auto")

          ;; imagino
          ,@(zp/notmuch-format-search "imagino" "w")

          ;; Qomon
          ;; ,@(zp/notmuch-format-search "qomon" "q")

          ;; Dev
          ,@(zp/notmuch-format-search "dev" "d")

          ;; Beta
          ,@(zp/notmuch-format-search "beta" "b")

          ;; News
          ,@(zp/notmuch-format-search "news" "n")

          ;; Lists
          ,@(zp/notmuch-format-search "lists" "la")
          ,@(zp/notmuch-format-search "list-emacs-devel" "le")
          ,@(zp/notmuch-format-search "list-emacs-fr" "lf")
          ,@(zp/notmuch-format-search "list-org" "lo")

          ;; GitHub
          ,@(zp/notmuch-format-search "dev-github" "g")

          ;; Discourse
          ,@(zp/notmuch-format-search "dev-forum" "f")

          (:name "flagged" :key "v" :query "tag:flagged")
          (:name "drafts" :key "x" :query "tag:draft")
          (:name "sent-week" :key "s" :query "tag:sent date:\"7d..today\"")
          (:name "sent" :key "S" :query "tag:sent")
          (:name "trash" :key "t" :query "tag:deleted")))

  (advice-add 'notmuch-mua-prompt-for-sender :override #'zp/notmuch-mua-prompt-for-sender))

(use-package zp-notmuch-fetch
  :bind (:map notmuch-hello-mode-map
         ("f" . zp/notmuch-fetch-new-mail-inbox)
         ("F" . zp/notmuch-fetch-new-mail)
         ("k" . zp/notmuch-fetch-kill)
         ("DEL" . zp/notmuch-fetch-kill)))

(use-package ol-notmuch
  :after (:any org notmuch))

(use-package orgalist
  ;; :disabled
  :after message
  :hook (message-setup . orgalist-mode)
  :config/el-patch
  (define-minor-mode orgalist-mode
  "Toggle Org-like lists and their relative commands.

With a prefix argument ARG, enable Auto Fill mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Orgalist mode is enabled, any line beginning with \"-\",
\"+\", \"1.\" or \"a.\" followed by a space starts a list.  You
can then operate locally on the list, e.g., to insert new items,
move items or sort them.  See below for details.

Moreover, you can add check-boxes to items

  - [ ] A checkbox, toggled with `C-c C-c'

turn an unordered list into a description list

  - term :: description

and control numbering in an ordered list

  4. [@4] a forced numbered item

key             binding
---             -------
M-<RET>         `orgalist-insert-item'
M-<UP>          `orgalist-previous-item'
M-<DOWN>        `orgalist-next-item'
M-S-<UP>        `orgalist-move-item-up'
M-S-<DOWN>      `orgalist-move-item-down'
M-<LEFT>        `orgalist-outdent-item'
M-<RIGHT>       `orgalist-indent-item'
M-S-<LEFT>      `orgalist-outdent-item-tree'
M-S-<RIGHT>     `orgalist-indent-item-tree'
C-c -           `orgalist-cycle-bullet'
C-c ^           `orgalist-sort-items'
C-c C-c         `orgalist-check-item'"
  :lighter " olst"
  (cond
   (orgalist-mode
    (when (derived-mode-p 'org-mode)
      (user-error "Cannot activate Orgalist mode in an Org buffer"))
    (setq-local org-blank-before-new-entry
                `((plain-list-item . ,orgalist-separated-items)))
    (setq-local org-list-allow-alphabetical t)
    (setq-local org-list-automatic-rules nil)
    (setq-local org-list-demote-modify-bullet nil)
    (setq-local org-list-description-max-indent 5)
    (setq-local org-list-indent-offset 0)
    (setq-local org-list-two-spaces-after-bullet-regexp nil)
    (setq-local org-list-use-circular-motion nil)
    (setq-local org-plain-list-ordered-item-terminator ?.)
    (add-function :around (local 'fill-forward-paragraph-function)
                  #'orgalist--fill-forward-wrapper)
    (add-function :around (local 'fill-paragraph-function)
                  #'orgalist--fill-item)
    ;; Unless `indent-line-function' is buffer-local before it is
    ;; advised with `add-function', the workaround for bug#31361 below
    ;; will not work, as (advice--cd*r indent-line-function) will not
    ;; compare `eq' to `indent-relative' in
    ;; `indent-according-to-mode'.
    (make-local-variable 'indent-line-function)
    (add-function :before-until
                  (local 'indent-line-function)
                  #'orgalist--indent-line)
    ;; If Auto fill mode is not enabled when we initialize Orgalist
    ;; mode, `auto-fill-function' is nil and we just advise
    ;; `normal-auto-fill-function'.
    (add-function :around
                  (local 'normal-auto-fill-function)
                  #'orgalist--auto-fill)
    (when auto-fill-function
      (add-function :around (local 'auto-fill-function) #'orgalist--auto-fill))
    ;; Prevent Auto fill mode from creating new items.
    (push 'orgalist--item-nobreak-p fill-nobreak-predicate)
    ;; FIXME: Workaround bug#31361.
    (unless (advice-member-p 'orgalist-fix-bug:31361 'indent-according-to-mode)
      (advice-add 'indent-according-to-mode
                  :around (lambda (old (el-patch-add &rest r))
                            "Workaround bug#31361."
                            (or (orgalist--indent-line)
                                (let ((indent-line-function
                                       (advice--cd*r indent-line-function)))
                                  (funcall old (el-patch-add r)))))
                  '((name . orgalist-fix-bug:31361)))))
   (t
    (remove-function (local 'fill-forward-paragraph-function)
                     #'orgalist--fill-forward-wrapper)
    (remove-function (local 'fill-paragraph-function) #'orgalist--fill-item)
    (remove-function (local 'indent-line-function) #'orgalist--indent-line)
    (setq fill-nobreak-predicate
          (delq 'orgalist--item-nobreak-p fill-nobreak-predicate))
    (remove-function (local 'normal-auto-fill-function) #'orgalist--auto-fill)
    (when auto-fill-function
      (remove-function (local 'auto-fill-function) #'orgalist--auto-fill))
    ;; FIXME: When there is no Orgalist minor mode active in any
    ;; buffer, remove workaround for bug#31361.
    (unless (cl-some (lambda (b) (with-current-buffer b orgalist-mode))
                     (buffer-list))
      (advice-remove 'indent-according-to-mode 'orgalist-fix-bug:31361)))))
  :config
  (message "orgalist was just loaded"))

;; Disabled because not used
;; (use-package footnote
;;   :config
;;   (setq footnote-section-tag "Footnotes: "))

;;----------------------------------------------------------------------------
;; Programming modes
;;----------------------------------------------------------------------------
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp)
         (rustic-mode . lsp)
         (go-mode . lsp)
         (typescriptmode . lsp)
         (js-mode . lsp)
         (js-jsx-mode . lsp))
  :bind-keymap ("s-l" . lsp-command-map)
  :config
  ;; Hack to configure server with .dir-locals.el
  ;; (add-hook 'hack-local-variables-hook
  ;;           (lambda ()
  ;;             (when (or (derived-mode-p 'go-mode))) (lsp)))

  ;; (setq lsp-log-max 100000)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-enabled-clients '(pyright gopls ts-ls rust-analyzer))

  ;; For imagino
  (setq lsp-go-env '((GOFLAGS . "-tags=testing,longTests,needApiServer,devOnly,oracle"))))

(use-package zp-lsp
  :hook (lsp-mode . zp/lsp-before-save-install)
  :config
  (setq zp/lsp-before-save-functions
        '((go-mode . (lsp-format-buffer
                      lsp-organize-imports)))))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package dap-mode
  :after lsp-mode)

(use-package dap-dlv-go
  :after dap-mode)

(use-package dap-firefox
  :config
  (dap-firefox-setup))

;;----------------------------------------------------------------------------
;;; Nix
;;----------------------------------------------------------------------------
(use-package nix-mode)

;;----------------------------------------------------------------------------
;;; Perl
;;----------------------------------------------------------------------------

(use-package cperl-mode
  :bind (:map cperl-mode-map
         ("M-RET" . zp/perl-eval-buffer)
         ("<C-return>" . zp/perl-eval-region))
  :config
  ;; Use ‘cperl-mode’ instead ‘perl-mode’
  (defalias 'perl-mode 'cperl-mode)

  (defun zp/perl-eval-region ()
    "Run selected region as Perl code."
    (interactive)
    (let ((max-mini-window-height nil))
      (call-process (mark) (point) "perl")))

  (defun zp/perl-eval-buffer-in-terminator ()
    "Run selected region as Perl code."
    (interactive)
    (call-process "terminator" (buffer-file-name) nil nil (concat "-x perl"))
    ;; (call-process (concat "terminator -x perl "
    ;;                       (buffer-file-name)))
    )

  (defun zp/perl-eval-buffer (arg)
    "Run current buffer as Perl code."
    (interactive "P")
    (let (max-mini-window-height)
      (unless arg
        (setq max-mini-window-height 999))
      (shell-command-on-region (point-min) (point-max) "perl"))))

;;----------------------------------------------------------------------------
;;; Python
;;----------------------------------------------------------------------------

(use-package python
  :bind (:map python-mode-map
         ("M-RET" . zp/python-eval-buffer))
  :config
  (defun zp/inferior-python-mode-config ()
    "Modify keymaps for ‘inferior-python-mode’."
    (local-set-key (kbd "C-l") #'comint-clear-buffer))

  (setq inferior-python-mode-hook 'zp/inferior-python-mode-config)

  (defun zp/python-eval-buffer (arg)
    "Run current buffer as Python code."
    (interactive "P")
    (let (max-mini-window-height)
      (unless arg
        (setq max-mini-window-height 999))
      (shell-command-on-region (point-min) (point-max) "python")))

  ;; Prototype for something that I’ve now forgotten
  ;; (defun zp/recenter-bottom (arg)
  ;; "Recenter screen at the end of the buffer."
  ;; (interactive "p")
  ;; (let ((inhibit-message t))
  ;;   (goto-char (point-max))
  ;;   (end-of-buffer)
  ;;   (recenter-top-bottom arg)
  ;;   (recenter-top-bottom arg)
  ;;   (scroll-up-line)))
  )

;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; (use-package elpy
;;   :bind (:map elpy-mode-map
;;          ("C-M-n" . elpy-nav-forward-block)
;;          ("C-M-p" . elpy-nav-backward-block))
;;   :hook ((elpy-mode . flycheck-mode)
;;          ;; (pyenv-mode . elpy-rpc-restart)
;;          )
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;;----------------------------------------------------------------------------
;;; C
;;----------------------------------------------------------------------------

(use-package cc-mode)

;;----------------------------------------------------------------------------
;;; Go
;;----------------------------------------------------------------------------

(use-package go-mode)

;;----------------------------------------------------------------------------
;;; Racket
;;----------------------------------------------------------------------------

(use-package racket-mode
  :bind (:map racket-mode-map
         ("M-RET" . zp/racket-eval-buffer))
  :config
  (defun zp/racket-eval-buffer (arg)
    "Run current buffer as Perl code"
    (interactive "P")
    (let (max-mini-window-height)
      (unless arg
        (setq max-mini-window-height 999))
      (let ((inhibit-message t))
        (basic-save-buffer))
      (shell-command (concat "racket " (buffer-file-name))))))

;;----------------------------------------------------------------------------
;; Java
;;----------------------------------------------------------------------------

(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

(use-package helm-lsp)
(use-package lsp-treemacs)

;;----------------------------------------------------------------------------
;; TypeScript
;;----------------------------------------------------------------------------

(use-package js
  :mode (("\\.js\\'" . js-mode)
         ("\\.[tj]sx\\'" . js-jsx-mode)))

(use-package typescript
  :mode (("\\.ts\\'" . typescript-mode)))

(use-package tide
  :disabled
  ;; :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :bind (:map tide-mode-map
         ("M-RET" . zp/typescript-eval-buffer))
  :config
  (tide-setup)
  ;; (defun setup-tide-mode ()
  ;;   (interactive)
  ;;   (tide-setup)
  ;;   (flycheck-mode 1)
  ;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;;   (eldoc-mode 1)
  ;;   (tide-hl-identifier-mode 1)
  ;;   ;; company is an optional dependency. You have to
  ;;   ;; install it separately via package-install
  ;;   ;; `M-x package-install [ret] company`
  ;;   (company-mode 1))

  ;; aligns annotation to the right hand side
  ;; (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (defun zp/typescript-eval-buffer (arg)
    "Run current buffer as Python code."
    (interactive "P")
    (let (max-mini-window-height)
      (save-buffer)
      (unless arg
        (setq max-mini-window-height 999))
      (shell-command-on-region (point-min) (point-max) (format "ts-node %s" (buffer-file-name)))))

  (defun zp/typescript-init-dir-locals ()
    (interactive)
    (let ((content "((nil . ((create-lockfiles . nil))))")
          (target "./.dir-locals.el"))
      (write-region content nil target))))

;;----------------------------------------------------------------------------
;; Haskell
;;----------------------------------------------------------------------------

(use-package haskell
  :bind (:map haskell-mode-map
         ("M-RET" . zp/haskell-eval-buffer))
  :config
  (defun zp/haskell-eval-buffer (arg)
    "Run current buffer as Python code."
    (interactive "P")
    (let (max-mini-window-height)
      (save-buffer)
      (unless arg
        (setq max-mini-window-height 999))
      (shell-command-on-region (point-min) (point-max) (format "ts-node %s" (buffer-file-name))))))

;;----------------------------------------------------------------------------
;; Rust
;;----------------------------------------------------------------------------

(use-package rust-mode)

(use-package rustic
  :bind (:map rustic-mode-map
         (("M-RET" . zp/rust-eval-buffer)))
  :config
  (setq rustic-lsp-server 'rustic-analyzer)

  (defun zp/rust-eval-buffer (arg)
    "Run current buffer as Rust code"
    (interactive "P")
    (let (max-mini-window-height)
      (unless arg
        (setq max-mini-window-height 999))
      (let ((inhibit-message t))
        (basic-save-buffer))
      (shell-command (concat "cargo run --quiet " (buffer-file-name (buffer-base-buffer)))))))

;;----------------------------------------------------------------------------
;; AUCTeX
;;----------------------------------------------------------------------------
(use-package latex
  :bind (:map LaTeX-mode-map
         (("C-x n e" . zp/LaTeX-narrow-to-environment)
          ("C-c DEL" . zp/LaTeX-remove-macro)
          ("C-c <C-backspace>" . zp/LaTeX-remove-macro)
          ("C-c <M-backspace>" . zp/LaTeX-remove-environment)
          ("C-c C-t C-v" . zp/tex-view-program-switch)))
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . TeX-source-correlate-mode))
  :config
  ;; Set default library
  (setq-default TeX-engine 'luatex
                TeX-save-query nil
                TeX-parse-self t
                TeX-auto-save t
                LaTeX-includegraphics-read-file 'LaTeX-includegraphics-read-file-relative)
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

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
      (let* ((pdf (concat file "." TeX-output-extension))
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
    (add-hook 'TeX-after-compilation-finished-functions
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
    (remove-hook 'TeX-after-compilation-finished-functions
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

  (defun zp/LaTeX-narrow-to-environment ()
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
    (message "Narrowing to parent environment")))

;;----------------------------------------------------------------------------
;; org → html/tex export
;;----------------------------------------------------------------------------
(use-package ox-html
  :after (org ox)
  :config
  (setq org-html-postamble nil))

(use-package ox-latex
  :after (org ox)
  :config
  (setq org-latex-default-class "koma-article")
  (setq org-latex-compiler "luatex")

  ;; Redefine default classes
  (setq org-latex-classes
        '(("koma-article"
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

\\author{Leo Vivier}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (setq org-latex-hyperref-template nil)

  ;; Use Minted for src-blocks
  (setq org-latex-src-block-backend 'minted)

  ;; Disable defaut packages
  (setq org-latex-default-packages-alist nil)

  ;; Legacy code for switching between long and short compilation with XeTeX
  ;; Not using it anymore because I’ve moved to LuaTeX
  ;; (defvar zp/org-latex-pdf-process-mode nil
  ;;     "Current mode for processing org-latex files.

  ;; See ‘zp/toggle-org-latex-pdf-process’ for more information.")

  ;;   (defun zp/toggle-org-latex-pdf-process ()
  ;;     "Toggle the number of steps in the XeTeX PDF process."
  ;;     (interactive)
  ;;     (if (or (not (bound-and-true-p zp/org-latex-pdf-process-mode))
  ;;             (string= zp/org-latex-pdf-process-mode "full"))
  ;;         (progn (setq org-latex-pdf-process '("xelatex -shell-escape\
  ;;                                                   -interaction nonstopmode\
  ;;                                                   -output-directory %o %f")
  ;;                      org-export-async-init-file "~/.emacs.d/async/main-short.el"
  ;;                      zp/org-latex-pdf-process-mode 'short)
  ;;                (message "XeLaTeX process mode: Short"))
  ;;       (progn (setq org-latex-pdf-process '("xelatex -shell-escape\
  ;;                                                     -interaction nonstopmode\
  ;;                                                     -output-directory %o %f"
  ;;                                            "biber %b"
  ;;                                            "xelatex -shell-escape\
  ;;                                                     -interaction nonstopmode\
  ;;                                                     -output-directory %o %f"
  ;;                                            "xelatex -shell-escape\
  ;;                                                     -interaction nonstopmode\
  ;;                                                     -output-directory %o %f")
  ;;                    org-export-async-init-file "~/.emacs.d/async/main-full.el"
  ;;                    zp/org-latex-pdf-process-mode 'full)
  ;;              (message "XeLaTeX process mode: Full"))))
  ;;   (zp/toggle-org-latex-pdf-process)

  ;; Suppress creation of labels when converting org→tex
  (defun remove-orgmode-latex-labels ()
    "Remove labels generated by org-mode"
    (interactive)
    (let ((case-fold-search nil))
      (goto-char 1)
      (re-search-forward "\\\\label{sec:org[0-9][^}]*}" nil t)
      (replace-match "")))

  (defun zp/org-latex-remove-section-labels (string backend _info)
    "Remove section labels generated by org-mode"
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\\\\label{sec:.*?}" "" string)))

  (add-to-list 'org-export-filter-final-output-functions
               #'zp/org-latex-remove-section-labels))

(use-package bibtex
  :config
  (setq bibtex-autokey-year-length '4))

(use-package ox-beamer
  :after (org beamer))

(use-package org-src
  :config
  (setq org-src-preserve-indentation t
        org-edit-src-content-indentation 2))

(use-package hyperlist-mode
  :load-path "~/projects/hyperlist-mode/")

;;----------------------------------------------------------------------------
;; org-mode
;;----------------------------------------------------------------------------
(use-package calendar
  :config
  (setq diary-file "~/diary")

  (calendar-set-date-style 'iso)

  ;; Geo-location
  (setq calendar-week-start-day 1
        calendar-latitude 48.8566
        calendar-longitude 2.3522
        calendar-location-name "Paris, France")

  (global-set-key (kbd "C-c c") 'calendar))

(use-package zp-calendar)

(use-package org-id
  :config
  (setq org-id-link-to-org-use-id 'use-existing))

(use-package org-habit
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-preceding-days 6
        org-habit-following-days 7)

  ;; Length of the habit graph
  (setq org-habit-graph-column 65))

(use-package org-appear
  :load-path "~/projects/org-appear/")

(use-package org
  :demand t
  :bind (:map org-mode-map
         ;; ("C-c i" . org-indent-mode)
         ("C-c [" . nil)
         ("C-c ]" . nil)
         ("C-c C-q" . org-set-tags-command)
         ("C-c C-." . org-time-stamp)
         ("C-c C-x r" . zp/org-set-appt-warntime)
         ("C-c C-x l" . zp/org-set-location)
         ("C-c C-x d" . org-delete-property)
         ("C-c C-x D" . org-insert-drawer)
         ("C-c C-x b" . zp/org-tree-to-indirect-buffer-folded)
         ("S-<backspace>" . zp/org-kill-spawned-ibuf)
         ("C-x n o" . zp/org-overview)
         ("C-x n a" . zp/org-show-all)
         ("C-x n u" . zp/org-narrow-up-heading-dwim)
         ("C-x n y" . zp/org-narrow-previous-heading)
         ("C-x n s" . zp/org-narrow-to-subtree)
         ("C-x n f" . zp/org-narrow-forwards)
         ("C-x n b" . zp/org-narrow-backwards)
         ("C-x n w" . zp/org-widen)
         ("C-c ," . zp/hydra-org-priority/body)
         ("M-p" . org-metaup)
         ("M-n" . org-metadown)
         ("M-P" . org-shiftmetaup)
         ("M-N" . org-shiftmetadown)
         ("M-[" . org-metaleft)
         ("M-]" . org-metaright)
         ("M-{" . org-shiftmetaleft)
         ("M-}" . org-shiftmetaright)
         ("C-a" . org-beginning-of-line)
         ("C-e" . org-end-of-line)
         ("M-I" . org-indent-mode)
         ("C-c C-x C-l" . zp/org-latex-preview-dwim)
         ("C-c R" . org-display-inline-images))
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         ;; (org-mode . electric-quote-local-mode)
         (before-save . zp/org-set-last-modified)
         ;; (org-todo-repeat . zp/org-comment-logbook-notes)
         )
  :config
  (setq org-agenda-inhibit-startup nil
        org-log-into-drawer "LOGBOOK-NOTES"
        org-use-property-inheritance '("AGENDA_GROUP")
        org-startup-folded 'fold
        org-attach-preferred-new-method 'ask
        org-log-state-notes-insert-after-drawers nil
        ;; org-special-ctrl-a/e t
        org-special-ctrl-a/e 'reversed
        org-log-done 'time
        org-log-refile nil
        org-enforce-todo-dependencies nil
        org-adapt-indentation nil
        org-loop-over-headlines-in-active-region 'start-level

        org-clock-report-include-clocking-task t
        org-clock-out-remove-zero-time-clocks t

        org-hide-emphasis-markers nil
        org-ellipsis "…"
        org-track-ordered-property-with-tag "ORDERED"
        org-tags-exclude-from-inheritance nil
        org-fold-catch-invisible-edits 'smart

        org-tags-column -77)

  ;; Change the functions used to follow links
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))

  ;; org-refile settings
  (setq org-refile-targets '((nil :maxlevel . 9))
        org-refile-use-cache nil
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path nil)

  ;; Ensure that images can be resized with deferred #+ATTR_ORG:
  (setq org-image-actual-width nil)

  ;; Prevent auto insertion of blank-lines before heading (but not for lists)
  (setq org-blank-before-new-entry (quote ((heading . auto)
                                           (plain-list-item . auto))))

  ;; Prevent blank-lines from being displayed between headings in folded state
  (setq org-cycle-separator-lines 0)

  ;; Add curly quotes to list of pre- and post-matches for emphasis markers
  ;; Otherwise, curly quotes prevent fontification
  (setq org-emphasis-regexp-components '("-       ('‘\"“’{" "-    .,:!?;'’\"”)}\\[" "     
" "." 1))

  ;; Set the default apps to use when opening org-links
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . default))

  ;; Define TODO keywords
  (setq org-todo-keywords
        '(;; Default set
          (sequence "TODO(t)" "NEXT(n)" "STRT(S!)" "|" "DONE(d)")
          ;; Extra sets
          (sequence "STBY(s)" "|" "CXLD(x@/!)")
          (sequence "WAIT(w!)" "|" "CXLD(x@/!)")))

  ;; State triggers
  (setq org-todo-state-tags-triggers
        '(("CXLD" ("cancelled" . t) ("standby") ("waiting"))
          ("STBY" ("standby" . t) ("cancelled") ("waiting"))
          ("WAIT" ("waiting" . t) ("cancelled") ("standby"))
          ("TODO" ("cancelled") ("standby") ("waiting"))
          ("NEXT" ("cancelled") ("standby") ("waiting"))
          ("STRT" ("cancelled") ("standby") ("waiting"))
          ("WAIT" ("cancelled") ("standby") ("waiting"))
          ("DONE" ("cancelled") ("standby") ("waiting"))
          ("" ("cancelled") ("standby") ("waiting")))
        ;; Custom faces for specific tags
        org-tag-faces
        '(("@home" . org-tag-context)
          ("@work" . org-tag-context)
          ("@town" . org-tag-context)
          ("standby" . org-tag-standby)
          ("routine" . org-tag-routine)
          ("boring" . org-tag-special)
          ("cancelled" . org-tag-standby)
          ("waiting" . org-tag-standby)
          ("assignment" . org-tag-important)
          ("exam" . org-tag-important)
          ("important" . org-tag-important)
          ("more" . org-tag-important)
          ("curios" . org-tag-curios)
          ("french" . org-tag-french)))

  ;; Set characters used for priorities
  (setq org-highest-priority ?A
        org-default-priority ?D
        org-lowest-priority ?E)

  ;; Default settings for ‘org-columns’
  (setq org-columns-default-format "%55ITEM(Task) %TODO(State) %Effort(Effort){:} %CLOCKSUM")

  ;; Global values for properties
  (setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 5:00 5:30 6:00 0:00")
                                      ("STYLE_ALL" . "habit")
                                      ("ENERGY_ALL" . "A B C")
                                      ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 35 40 45 50 55 60 none")
                                      ("SESSION_DURATION_ALL" . "0:45 0:15 0:20 0:30 1:00"))))

  ;; Archiving location
  (setq org-archive-location "%s.archive::")

  ;; Load languages with Babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((R . t)
                                 (python . t)
                                 (java . t)
                                 (latex . t)
                                 (ledger . t)
                                 (shell . t)
                                 (sql . t)
                                 (restclient . t)))

  ;; Show images after executing a src-block that generated one
  ;; TODO: Limit the scope of the hook by testing if the block actually
  ;; generated an image
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)

  (use-package zp-org
    :bind (:map org-mode-map
           ("M-*" . zp/org-toggle-fontifications))))

(use-package org-persist
  :config
  (setq org-persist-remote-files nil))

(use-package zp-org
  :after org
  :config
  ;; Formatting options for LaTeX preview-blocks
  (setq org-format-latex-options
        '(:foreground default
          :background default
          :scale zp/org-format-latex-default-scale
          :html-foreground "Black"
          :html-background "Transparent"
          :html-scale 1.0
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(use-package org-contrib)

(use-package org-footnote
  :config
  (setq org-footnote-define-inline 1))

(use-package org-clock
  :bind (("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c C-x C-z" . org-resolve-clocks)
         ("s-/" . zp/echo-clock-string))
  :config
  (setq org-clock-into-drawer "LOGBOOK-CLOCK"
        org-clock-sound t)

  (defun zp/echo-clock-string ()
    "Echo the tasks being currently clocked in the minibuffer, along
with effort estimates and total time."
    (interactive)
    (if (org-clocking-p)
        (let ((header "Current clock")
              (clocked-time (org-clock-get-clocked-time))
              (org-clock-heading-formatted (replace-regexp-in-string "%" "%%" org-clock-heading)))
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
      (error "Not currently clocking any task"))))

;; Enable resetting plain-list checks when marking a repeated tasks DONE
;; To enable that behaviour, set the ‘RESET_CHECK_BOXES’ property to t for the
;; parent
(use-package org-checklist)

(use-package org-faces
  :config
  ;; Assign faces to priorities
  (setq org-priority-faces '((?A . (:inherit org-priority-face-a))
                             (?B . (:inherit org-priority-face-b))
                             (?C . (:inherit org-priority-face-c))
                             (?D . (:inherit org-priority-face-d))
                             (?E . (:inherit org-priority-face-e))))

  ;; Assign faces for TODO keywords
  (setq org-todo-keyword-faces
        '(("TODO" :inherit org-todo-todo)
          ("NEXT" :inherit org-todo-next)
          ("STRT" :inherit org-todo-strt)
          ("DONE" :inherit org-todo-done)

          ("STBY" :inherit org-todo-stby)
          ("WAIT" :inherit org-todo-wait)
          ("CXLD" :inherit org-todo-cxld)))

  ;;-----------------
  ;; Face definition
  ;;-----------------

  ;; TODO: Optimise

  (defface org-todo-todo '((t)) nil :group 'org-faces)
  (defface org-todo-next '((t)) nil :group 'org-faces)
  (defface org-todo-strt '((t)) nil :group 'org-faces)
  (defface org-todo-done '((t)) nil :group 'org-faces)
  (defface org-todo-stby '((t)) nil :group 'org-faces)
  (defface org-todo-wait '((t)) nil :group 'org-faces)
  (defface org-todo-cxld '((t)) nil :group 'org-faces)

  (defface org-priority-face-a '((t)) nil :group 'org-faces)
  (defface org-priority-face-b '((t)) nil :group 'org-faces)
  (defface org-priority-face-c '((t)) nil :group 'org-faces)
  (defface org-priority-face-d '((t)) nil :group 'org-faces)
  (defface org-priority-face-e '((t)) nil :group 'org-faces)

  (defface org-tag-context '((t :inherit 'org-tag)) nil :group 'org-faces)
  (defface org-tag-special '((t :inherit 'org-tag)) nil :group 'org-faces)
  (defface org-tag-standby '((t :inherit 'org-tag)) nil :group 'org-faces)
  (defface org-tag-routine '((t :inherit 'org-tag-standby)) nil :group 'org-faces)
  (defface org-tag-important '((t :inherit 'org-tag)) nil :group 'org-faces)
  (defface org-tag-curios '((t :inherit 'org-tag)) nil :group 'org-faces)
  (defface org-tag-french '((t :inherit 'org-tag)) nil :group 'org-faces))

;; Babel
(use-package ob-sql)

(use-package ob-async
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            (lambda ()
              (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"))))

(use-package ob
  :config
  (setq org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'org-src-lang-modes '("awk" . awk))
  (add-to-list 'org-src-lang-modes '("ditaa" . plantuml))

  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (ditaa .t)
                                                           (awk .t))))

(use-package zp-ob)

(use-package vertico
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("C-l" . vertico-directory-delete-word)
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat)
         :map zp/toggle-map
         (("of" . vertico-flat-mode)
          ("ob" . vertico-buffer-mode)
          ("og" . vertico-grid-mode)))
  :init (vertico-mode 1)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mouse-mode 1)
  (vertico-multiform-mode 1)
  (setq vertico-count 10)
  (setq vertico-multiform-categories '((consult-grep buffer))
        vertico-multiform-commands '((tmm-menubar flat)
                                     (tmm-shortcut flat))))

(use-package orderless
  :after vertico
  :config
  (setq orderless-matching-styles '(orderless-regexp
                                    orderless-initialism
                                    orderless-prefixes)
        orderless-component-separator #'orderless-escapable-split-on-space)

  ;; Use the built-in "partial-completion" style to complete
  ;; file inputs such as "/e/ni/co.nix" into
  ;; "/etc/nixos/configuration.nix".  The "basic" style is
  ;; needed to support the hostname completion in the TRAMP
  ;; inputs such as "/sshx:HOSTNAME".
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))

  (setq completion-styles '(orderless))

  (defun vifon/orderless-without-if-bang (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun vifon/orderless-literal-if-equal (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                      vifon/orderless-literal-if-equal)))

(use-package embark
  :bind (("C-c o" . embark-act)
         ("C-."   . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act)
         :map embark-command-map
         ;; Unbind the dangerous `global-set-key' and `local-set-key'
         ;; actions.  It's far too easy to accidentally bind over some
         ;; `self-insert-command' binding or even over
         ;; \\[keyboard-quit].
         ("g" . nil)
         ("l" . nil))
  :config
  (setq embark-mixed-indicator-delay 0.2)

  ;; Make the eval action editable.  Evaluating code
  ;; in-place is simple enough without Embark, if I invoke
  ;; it with Embark, I almost definitely want to edit the
  ;; expression beforehand.  And even if not, I can
  ;; just confirm.
  (cl-pushnew 'embark--allow-edit
              (alist-get 'pp-eval-expression embark-target-injection-hooks))

  ;; Reload the project list after using
  ;; C-u `embark-act' with `project-forget-project'.
  ;; (cl-pushnew 'embark--restart
  ;;             (alist-get 'project-forget-project embark-post-action-hooks))

  (defun embark-act-with-eval (expression)
    "Evaluate EXPRESSION and call `embark-act' on the result."
    (interactive "sExpression: ")
    (with-temp-buffer
      (let ((expr-value (eval (read expression))))
        (insert (if (stringp expr-value)
                    expr-value
                  (format "%S" expr-value))))
      (embark-act)))

  (dolist (keymap (list embark-variable-map embark-expression-map))
    (define-key keymap (kbd "v") #'embark-act-with-eval))

  ;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
  (autoload 'gnus-dired-attach "gnus-dired" nil t)
  (defun embark-attach-file (file)
    "Attach FILE to an email message."
    (interactive "fAttach: ")
    (gnus-dired-attach (list file)))
  (bind-key "a" #'embark-attach-file embark-file-map)

  ;; Source: https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro zp/embark-split-action (fn split-type)
      `(defun ,(intern (concat "zp/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         ,(format "Split window with `%s'" split-type)
         (interactive)
         (funcall #',split-type)
         (call-interactively #',fn))))

  (define-key embark-file-map (kbd "2") (zp/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map (kbd "2") (zp/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (zp/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map (kbd "3") (zp/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map (kbd "3") (zp/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (zp/embark-split-action bookmark-jump split-window-right)))

(use-package embark-consult
  :after (embark consult))

(use-package marginalia
  :after vertico
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :bind (("M-s f" . consult-line)
         ("M-g M-g" . consult-line)
         ("M-g f" . consult-focus-lines)
         ("M-g o" . consult-outline)
         ("M-g r" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap apropos-command] . consult-apropos)
         ([remap imenu] . consult-imenu)
         ([remap apropos] . consult-apropos)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ([remap previous-matching-history-element] . consult-history))
  :init
  (bind-key "TAB"
            (lambda ()
              (interactive)
              (isearch-exit)
              (let ((query (if isearch-regexp
                               isearch-string
                             (regexp-quote isearch-string))))
                (consult-line query)))
            isearch-mode-map)
  :config
  ;; (setq consult-project-root-function #'vc-root-dir)
  (consult-customize consult-ripgrep
                     consult-grep
                     consult-buffer
                     consult-recent-file
                     consult-bookmark
                     :preview-key "M-."
                     consult-bookmark
                     :sort nil)

  (setq consult-narrow-key "<")

  (defun vifon/orderless-fix-consult-tofu (pattern _index _total)
    "Ignore the last character which is hidden and used only internally."
    (when (string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                    "[\x200000-\x300000]*$"))))

  (dolist (command '(consult-buffer consult-line))
    (advice-add command :around
                (lambda (orig &rest args)
                  (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                           orderless-style-dispatchers)))
                    (apply orig args)))))

  ;; Disable consult-buffer project-related capabilities as
  ;; they are very slow in TRAMP.
  (setq consult-buffer-sources
        (delq 'consult--source-project-buffer
              (delq 'consult--source-project-file consult-buffer-sources)))

  (setq consult--source-hidden-buffer
        (plist-put consult--source-hidden-buffer :narrow ?h)))


;;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
(autoload 'ffap-file-at-point "ffap")
(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table :exclusive 'no))))
          'append)

;;; Add prompt indicator to `completing-read-multiple'.
;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;;
;;; Taken from the Vertico docs.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Use the completing-read UI for the M-tab completion unless
;;; overridden (for example by `corfu').
(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))

;;------------
;; Projectile
;;------------
(use-package project
  :config
  (setq project-switch-commands #'project-dired))

;;----------------------------------------------------------------------------
;; Hydra
;;----------------------------------------------------------------------------
(use-package hydra)

(use-package hydra-org-priority
  :requires (org hydra))

;;----------------------------------------------------------------------------
;; Hydra shortcuts
;;----------------------------------------------------------------------------
(use-package zp-hydra-shortcuts
  :bind ("C-c s" . zp/hydra-shortcuts/body)
  :config
  (zp/hydra-shortcuts-make-hydra
   ;; Misc
   ("e" "~/.emacs.d/init.el")
   ("I" "~/org/info.org.gpg")
   ("p d" "/ssh:asus:~/Downloads/Sharing/dl.org")
   ("p s" "~/org/projects/hacking/snippets.org.gpg")

   ;; Ledger
   ("l l" "~/org/ledger/main.ledger.gpg")
   ("l s" "~/org/ledger/main-schedule.ledger.gpg")
   ;; ("l f" "~/org/ledger/french-house.ledger.gpg")

   ;; Research
   ("p T" "/home/zaeph/org/projects/university/research/monty-python/meef2/1-academic/proposal/proposal.tex")
   ;; ("p T" "~/org/projects/university/research/presentation/presentation.tex")
   ("p b" "~/org/bib/monty-python.bib")
   ("p B" "~/org/projects/university/research/thesis/bibliography/bibliography.tex")
   ;; ("p c" "~/org/projects/university/research/sty/zaeph.sty")
   ;; ("p C" "~/org/projects/university/research/sty/presentation.sty")
   ;; ("p d" "/tmp/asus~/Downloads/Sharing/dl.org")

   ;; Journal
   ("j" "~/org/journal.org")

   ;; Projects
   ("p w" "~/org/projects/writing/writing.org.gpg")
   ;; ("p t" "~/org/projects/tavocat/tavocat.org.gpg")
   ;; ("p k""~/org/projects/kendeskiñ/kendeskiñ.org.gpg")
   ("p t" "~/org/projects/typography/typography.org.gpg")

   ;; University
   ("p u" "~/org/projects/university/university.org.gpg")
   ("p r" "~/org/projects/university/research/research.org.gpg")
   ;; ("p c l"     "~/org/projects/university/classes/university/ling/ling.org.gpg")
   ;; ("p c u"     "~/org/projects/university/classes/university/civ-us/civ-us.org.gpg")
   ;; ("p c g"     "~/org/projects/university/classes/university/civ-gb/civ-gb.org.gpg")
   ;; ("p c s"     "~/org/projects/university/classes/university/space/space.org.gpg")
   ;; ("p c i"     "~/org/projects/university/classes/university/lit/lit.org.gpg")
   ;; ("p c s"     "~/org/projects/university/classes/university/syn/syn.org.gpg")
   ;; ("p c t"     "~/org/projects/university/classes/espe/tronc-commun.org.gpg")

   ;; Languages
   ("p j" "~/org/projects/lang/ja/ja.org.gpg")
   ("p g" "~/org/projects/lang/de/de.org.gpg")

   ;; Activism
   ("p a" "~/org/projects/activism/politics/politics.org.gpg")
   ;; ("p a d"  "[DATA EXPUNGED]")
   ;; ("p a s"  "[DATA EXPUNGED]")
   ;; ("p a c"  "[DATA EXPUNGED]")
   ;; ("p a m"  "[DATA EXPUNGED]")

   ;; Media
   ("p n" "~/org/projects/media/news/news.org.gpg")

   ;; Music
   ("p P" "~/org/piano.org.gpg")

   ;; Awakening
   ("p A" "~/org/projects/awakening/awakening.org.gpg")

   ;; Psychotherapy
   ("p p" "~/org/projects/psychotherapy/psychotherapy.org.gpg")
   ;; Sports
   ("p S" "~/org/sports/swimming/swimming.org")
   ("p D" "~/org/sports/swimming/dryland.org")

   ;; Hacking
   ("p h e" "~/org/projects/hacking/emacs/emacs.org.gpg")
   ("p h l" "~/org/projects/hacking/linux/linux.org.gpg")
   ("p h n" "~/org/projects/hacking/linux/nixos.org")
   ("p h o" "~/org/projects/hacking/opsec/opsec.org.gpg")
   ("p h h" "~/org/projects/hacking/hacking.org.gpg")
   ("p h p" "~/org/projects/hacking/python/python.org.gpg")

   ;; Media
   ("b" "~/org/media.org.gpg")

   ;; Life
   ("o" "~/org/life.org")))

;;----------------------------------------------------------------------------
;; org-super-agenda
;;----------------------------------------------------------------------------
(use-package org-super-agenda
  ;; :load-path "~/projects/org-super-agenda/"
  :after org-agenda
  :config
  (org-super-agenda-mode)

  (setq org-super-agenda-header-separator "")

  (defun zp/org-super-agenda-update-face ()
    (let ((ul-color (internal-get-lisp-face-attribute
                     'font-lock-comment-face :foreground)))
      (set-face-attribute 'org-super-agenda-header nil
                          :slant 'italic
                          :underline `(:color ,ul-color))))

  (defun zp/org-super-agenda-item-in-agenda-groups-p (item groups)
    "Check if ITEM is in agenda GROUPS."
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (org-with-point-at marker
        (apply #'zp/org-task-in-agenda-groups-p groups))))

  (defun zp/org-super-agenda-groups (header groups)
    "Create org-super-agenda section for GROUPS with HEADER."
    `(:name ,header
      :pred (lambda (item)
              (zp/org-super-agenda-item-in-agenda-groups-p item ',groups))))

  (defun zp/org-super-agenda-groups-all ()
    `(,(zp/org-super-agenda-groups "Inbox" '("inbox"))
      ,(zp/org-super-agenda-groups "Life" '("life"))
      ,(zp/org-super-agenda-groups "Maintenance" '("mx"))
      ,(zp/org-super-agenda-groups "Professional" '("pro"))
      ,(zp/org-super-agenda-groups "Research" '("research"))
      ,(zp/org-super-agenda-groups "Activism" '("act"))
      ,(zp/org-super-agenda-groups "Hacking" '("hack"))
      ,(zp/org-super-agenda-groups "Curiosities" '("curios"))
      ,(zp/org-super-agenda-groups "Media" '("media"))))

  (defun zp/org-super-agenda-subtask-p (item)
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (org-with-point-at marker
        (zp/is-subtask-p))))

  (defun zp/org-super-agenda-scheduled ()
    '((:name "Overdue"
       :face (:foreground "red")
       :scheduled past)
      (:name "Waiting"
       :and (:tag "waiting"
             :scheduled nil)
       :and (:tag "waiting"
             :scheduled today))
      (:name "Scheduled"
       :scheduled today)
      (:name "Subtasks"
       :and (:scheduled nil
             :pred (lambda (item)
                     (when zp/org-agenda-split-subtasks
                       (zp/org-super-agenda-subtask-p item)))))
      (:name "Current"
       :and (:not (:scheduled t)
             :not (:tag "waiting")))
      (:name "Later"
       :anything)))

  (defun zp/org-super-agenda-group-heads (item)
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (org-entry-get marker "AGENDA_GROUP" nil)))

  (defun zp/org-super-agenda-stuck-project-p (item)
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (org-with-point-at marker
        (zp/is-stuck-project-p))))

  (defun zp/org-super-agenda-finished-project-p (item)
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (org-with-point-at marker
        (when (zp/is-confused-project-p)
          (zp/org-resolve-confused-project))
        (zp/is-finished-project-p))))

  (defun zp/org-super-agenda-projects ()
    '((:name "Group heads"
       :pred (lambda (item)
               (zp/org-super-agenda-group-heads item)))
      (:name "Finished"
       :face (:foreground "purple")
       :pred (lambda (item)
               (zp/org-super-agenda-finished-project-p item)))
      (:name "Stuck"
       :face (:foreground "red")
       :pred (lambda (item)
               (zp/org-super-agenda-stuck-project-p item)))
      (:name "Waiting"
       :tag "waiting")
      (:name "Current"
       :anything))))

;;----------------------------------------------------------------------------
;; org-agenda
;;----------------------------------------------------------------------------
(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :custom
  (org-agenda-files '("~/org/life.org")))

(use-package zp-org-agenda
  :bind (("s-o" . zp/switch-to-agenda)
         :map org-agenda-mode-map
         (("M-n" . org-agenda-next-date-line)
          ("M-p" . org-agenda-previous-date-line)
          ("C-," . sunrise-sunset)
          ("C-c C-q" . org-agenda-set-tags)
          ("," . zp/hydra-org-priority/body)
          ("M-h" . zp/toggle-org-habit-show-all-today)
          ("M-i" . zp/toggle-org-agenda-category-icons)
          ("M-t" . org-agenda-todo-yesterday)
          ("D" . zp/toggle-org-agenda-include-deadlines)
          ("S" . zp/toggle-org-agenda-include-scheduled)
          ("K" . zp/toggle-org-agenda-include-routine)
          ("H" . zp/toggle-org-agenda-include-habits)
          ("E" . zp/toggle-org-agenda-show-all-dates)
          ("Y" . zp/toggle-org-agenda-include-projects)
          ("C" . zp/org-agenda-mark-as-non-project)
          ("M-d" . zp/toggle-org-deadline-warning-days-range)
          ("r" . zp/org-agenda-benchmark)
          ("R" . zp/org-agenda-garbage-collect)
          ("G" . zp/org-agenda-wipe-local-config)
          ("y" . zp/toggle-org-agenda-split-subtasks)
          ("i" . zp/toggle-org-agenda-sorting-strategy-special-first)
          ("o" . zp/toggle-org-agenda-sort-by-lifo)
          ("F" . zp/toggle-org-agenda-todo-ignore-future)
          ("W" . zp/toggle-org-agenda-projects-include-waiting)
          ("C-c C-x r" . zp/org-agenda-set-appt-warntime)
          ("C-c C-x l" . zp/org-agenda-set-location)
          ("C-c C-x d" . zp/org-agenda-delete-property)
          ("C-c C-x s" . zp/org-agenda-wipe-local-config)
          (">" . zp/org-agenda-date-prompt-and-update-appt)
          ("<" . zp/ivy-org-agenda-set-category-filter)
          ("C-c C-s" . zp/org-agenda-schedule-and-update-appt)
          ("C-c C-S-w" . zp/org-agenda-refile-with-paths)
          ("Z" . zp/org-resolve-clocks)
          ("V" . org-agenda-clockreport-mode)
          ("C-<return>" . org-agenda-switch-to)
          ("<return>" . zp/org-agenda-tree-to-indirect-buffer-without-grabbing-focus)
          ("S-<return>" . zp/org-agenda-tree-to-indirect-buffer)
          ("M-<return>" . zp/org-agenda-tree-to-indirect-buffer-maximise)
          ("<backspace>" . zp/org-kill-spawned-ibuf-and-window)
          ("c" . zp/org-agenda-goto-calendar))
         :map calendar-mode-map
         (("c" . zp/org-calendar-goto-agenda)
          ("<RET>" . zp/org-calendar-goto-agenda)))
  :commands (org-agenda)
  :hook ((org-agenda-finalize . zp/org-font-lock-add-energy-faces))
  :config
  (setq org-agenda-show-future-repeats t
        org-agenda-show-log nil
        org-agenda-skip-scheduled-if-done 1
        org-agenda-skip-timestamp-if-done 1
        org-agenda-skip-deadline-if-done 1
        org-agenda-skip-deadline-prewarning-if-scheduled nil
        org-agenda-tags-todo-honor-ignore-options 1
        org-agenda-todo-ignore-with-date nil
        org-agenda-todo-ignore-deadlines nil
        org-agenda-todo-ignore-timestamp nil
        org-agenda-todo-list-sublevels t
        org-agenda-dim-blocked-tasks nil
        org-agenda-include-deadlines 'all
        org-deadline-warning-days 30
        org-agenda-cmp-user-defined 'zp/org-cmp-created-dwim
        org-agenda-sorting-strategy
        '((agenda habit-down deadline-up time-up scheduled-up priority-down category-keep)
          (tags user-defined-down category-keep)
          (todo user-defined-down category-keep)
          (search category-keep))

        ;; Initialise the list structure for local variables
        zp/org-agenda-local-config
        (zp/org-agenda-local-config-init
         '(
           org-habit-show-all-today nil
           org-agenda-show-all-dates t
           org-agenda-include-deadlines t
           zp/org-agenda-include-scheduled t
           org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp)
           zp/org-agenda-include-category-icons t
           zp/org-agenda-sorting-strategy-special-first nil
           zp/org-agenda-split-subtasks nil
           zp/org-agenda-include-waiting t
           zp/org-agenda-include-projects t
           zp/org-agenda-groups-extra-filters nil
           zp/org-agenda-category-filter nil

           org-habit-show-habits t
           zp/org-agenda-include-routine t

           zp/org-agenda-todo-ignore-future t
           org-agenda-todo-ignore-scheduled 'future

           zp/org-agenda-sort-by-rev-fifo nil))

        zp/org-agenda-extra-local-config
        '(("k" ((zp/org-agenda-include-routine . t)))
          ("K" ((zp/org-agenda-include-routine . t)))
          ("x" ((zp/org-agenda-todo-ignore-future . nil)
                (org-agenda-todo-ignore-scheduled . nil)))
          ("calendar" ((zp/org-agenda-include-routine . nil))))

        ;; View setup
        org-agenda-hide-tags-regexp "recurring\\|waiting\\|standby"
        org-agenda-tags-column -88
        org-agenda-timegrid-use-ampm nil
        org-agenda-window-setup 'current-window
        org-agenda-compact-blocks nil
        org-agenda-entry-text-maxlines 10
        org-agenda-sticky 1
        org-agenda-block-separator 126
        org-agenda-use-time-grid nil
        org-agenda-exporter-settings
        '((ps-print-color-p t)
          (ps-landscape-mode t)
          (ps-print-header nil)
          (ps-default-bg t))
        org-agenda-clockreport-parameter-plist
        '(:link t :narrow 50 :maxlevel 2 :fileskip0 t)
        org-agenda-clock-consistency-checks
        '(:max-duration "10:00"
          :min-duration 0
          :max-gap "0:10"
          :gap-ok-around ("4:00" "7:35" "8:35" "12:05" "17:35")
          :default-face zp/org-agenda-block-info-face
          :gap-face nil
          :no-end-time-face nil
          :long-face nil
          :short-face nil)
        org-agenda-prefix-format '((agenda . " %i %-12:c%(zp/org-agenda-format-energy-level-cookie)%?-12t%s")
                                   (timeline . "  % s")
                                   (todo . " %i %-12:c%(zp/org-agenda-format-energy-level-cookie)")
                                   (tags . " %i %-12:c%(zp/org-agenda-format-energy-level-cookie)")
                                   (search . " %i %-12:c")))

  (setq zp/org-agenda-default-agendas-list '("n" "l"))

  (setq zp/org-agenda-seekable-agendas-list '("n" "N" "k" "K"))

  (setq org-agenda-custom-commands
        `(("n" "Agenda"
           (,(zp/org-agenda-block-agenda-main "Agenda" org-agenda-files)))

          ("N" "Agenda (w/o groups)"
           (,(zp/org-agenda-block-agenda "Agenda (w/o groups)" org-agenda-files)))

          ("k" "Timestamps & Deadlines"
           (,(zp/org-agenda-block-agenda-timestamps-and-deadlines
              "Timestamps & Deadlines")))

          ("K" "Seeking Agenda"
           (,(zp/org-agenda-block-agenda-week-with-group-filter
              "Seeking Agenda" nil)))

          ("A" "Active"
           (,@(zp/org-agenda-blocks-create "Active" nil nil t)))

          ("I" "Inactive"
           (,@(zp/org-agenda-blocks-create "Inactive" nil "/STBY")))

          ("ii" "Inactive (+groups)"
           (,@(zp/org-agenda-blocks-create "Inactive (+groups)" nil "/STBY" t)))

          ("C" "Curiosities"
           (,@(zp/org-agenda-blocks-create "Curiosities" nil "+curios")))

          ("cc" "Curiosities (+groups)"
           (,@(zp/org-agenda-blocks-create "Curiosities (+groups)" nil "+curios" t)))

          ,@(zp/org-agenda-create-all
             '(("l" "Life" ("+life+mx+pro+research+act"))
               ("L" "Life (strict)" ("+life+mx"))
               ("x" "Maintenance" ("mx"))
               ("p" "Professional" ("pro"))
               ("r" "Research" ("research"))
               ("h" "Hacking" ("hack"))
               ("o" "Org" ("org"))
               ("e" "Emacs" ("emacs"))
               ("R" "Org-roam" ("roam"))
               ("O" "OPSEC" ("opsec"))
               ("P" "Activism" ("act"))
               ("M" "Media" ("media"))
               ("mb" "Books" ("books"))
               ("mt" "Theatre" ("theatre"))
               ("mf" "Film" ("film"))
               ("ms" "Series" ("series"))
               ("g" "Groupless" ("nil"))
               ("X" "Testing ground" ("+test"))))

          ("j" "Journal entries"
           (,(zp/org-agenda-block-journal))
           ((org-agenda-files '("~/org/journal.org"))))

          ("J" "Journal entries - Swimming"
           (,(zp/org-agenda-block-journal "Swimming"))
           ((org-agenda-files '("~/org/journal.org"))
            (org-agenda-skip-function
             '(zp/skip-tasks-not-in-agenda-groups-with-extra-filters
              (zp/org-agenda-groups-process-filters '("+swim"))))))

          ("d" "Deadlines"
           (,(zp/org-agenda-block-deadlines)))

          ("w" "Waiting list"
           (,(zp/org-agenda-block-tasks-waiting)))

          ("A" "Meditation records"
           ((agenda ""
                    ((org-agenda-files zp/org-agenda-files-awakening)
                     (org-agenda-log-mode))))
           ((org-agenda-skip-timestamp-if-done nil)))

          ("S" "Swim logs"
           ((agenda ""
                    ((org-agenda-files '("~/org/sports/swimming/swimming.org"
                                         "~/org/sports/swimming/dryland.org")))))
           ((org-agenda-skip-timestamp-if-done nil)
            (org-agenda-show-log t)
            (org-agenda-log-mode-items '(closed clock))
            ))))

  ;; Update ‘org-super-agenda-header-map’
  (use-package org-super-agenda
    :config
    (setq org-super-agenda-header-map org-agenda-mode-map)))

;;----------------------------------------------------------------------------
;; org-capture
;;----------------------------------------------------------------------------
(use-package org-capture
  :bind (("C-c n" . org-capture)
         :map org-agenda-mode-map
         ("k" . zp/org-agenda-capture))
  :hook ((org-capture-mode . zp/org-capture-make-full-frame)
         (org-capture-prepare-finalize . zp/org-capture-set-created-property))
  :config
  (setq org-default-notes-file "~/org/life.org")

  ;;------------------
  ;; Helper functions
  ;;------------------

  (defvar zp/org-capture-journal-file nil
    "Location of the journal file.")

  (setq zp/org-capture-journal-file "~/org/journal.org")

  (defun zp/org-capture-journal-create-template (key name)
    "Create journal template for `org-capture'.

NAME is the name of the template, which will also be used as the
target-headline in `zp/org-capture-journal-file'.

KEY is the key to use to access the template"
    `(,key ,name entry (file+olp ,zp/org-capture-journal-file ,name)
           "* %^{Title|Entry}\n%T\n\n%?"
           :full-frame t))

  ;;-----------
  ;; Templates
  ;;-----------

  (setq org-capture-templates
        `(("n" "Note" entry (file+headline "~/org/life.org" "Notes")
           "* %?" :add-created t)
          ("f" "Todo" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %?" :add-created t)
          ("F" "Todo + Clock" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %?\n" :add-created t :clock-in t)
          ("r" "Todo with Context" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %?\n%a" :add-created t)
          ("R" "Todo with Context + Clock" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %?\n%a" :add-created t :clock-in t)
          ;; ("r" "Todo + Reminder" entry (file+headline "~/org/life.org" "Inbox")
          ;;  "* TODO %?\nSCHEDULED: %^T\n:PROPERTIES:\n:APPT_WARNTIME:  %^{APPT_WARNTIME|5|15|30|60}\n:END:")
          ;; ("T" "Todo (with keyword selection)" entry (file+headline "~/org/life.org" "Inbox")
          ;;  "* %^{State|TODO|NEXT|STBY|WAIT} %?")
          ;; ("e" "Todo + Creation time" entry (file+headline "~/org/life.org" "Inbox")
          ;;  "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
          ;; ("C" "Todo + Clock" entry (file+headline "~/org/life.org" "Inbox")
          ;;  "* TODO %^{Task}%?" :clock-in t)
          ;; ("C" "Todo + Clock (with keyword selection)" entry (file+headline "~/org/life.org" "Inbox")
          ;;  "* %^{State|TODO|NEXT} %?" :clock-in t)
          ("d" "Date" entry (file+headline "~/org/life.org" "Calendar")
           "* %?\n" :add-created t)
          ("e" "Date + Context" entry (file+headline "~/org/life.org" "Calendar")
           "* %?\n%a" :add-created t)

          ("p" "Process notes" entry (file+headline "~/org/life.org" "Notes")
           "* TODO Process notes: %a%?\nDEADLINE: <%(org-read-date nil nil \"+2d\")>" :add-created t)

          ;; ("D" "Date + Reminder" entry (file+headline "~/org/life.org" "Calendar")
          ;;  "* %?\n%^T\n\n%^{APPT_WARNTIME}p")
          ;; ("R" "Reminder" entry (file+headline "~/org/life.org" "Inbox")
          ;;  "* %?\n%^T%^{APPT_WARNTIME}p")

          ("m" "Meeting" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO Meeting with %^{Meeting with}%?" :clock-in t)

          ("s" "Special")
          ("ss" "Code Snippet" entry (file "~/org/projects/hacking/snippets.org.gpg")
           ;; Prompt for tag and language
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ;; ("sf" "Film recommendation" entry (file+olp "~/org/media.org.gpg" "Films" "List")
          ;;  "* %(zp/org-capture-set-media-link-letterboxd)%?%^{MEDIA_DIRECTOR}p%^{MEDIA_YEAR}p%^{MEDIA_DURATION}p")
          ;; ("sf" "Film recommendation" entry (file+olp "~/org/media.org.gpg" "Films" "List")
          ;;  "* %(zp/letterboxd-set-link)%?%^{MEDIA_DIRECTOR}p%^{MEDIA_YEAR}p%(zp/letterboxd-set-duration)")
          ("sf" "Film" entry (file+olp "~/org/media.org.gpg" "Films" "List")
           "* %(zp/letterboxd-capture)")
          ("sF" "Film (insert at top)" entry (file+olp "~/org/media.org.gpg" "Films" "List")
           "* %(zp/letterboxd-capture)" :prepend t)
          ("st" "Swim training" entry (file+weektree+prompt "~/org/sports/swimming/swimming.org")
           ;; "* DONE Training%^{SWIM_DISTANCE}p%^{SWIM_DURATION}p\n%t%(print zp/swimming-workout-default)"
           "* DONE Training%^{SWIM_DISTANCE}p%^{SWIM_DURATION}p\n%t\n%?")
          ("sd" "Dryland training" entry (file+weektree+prompt "~/org/sports/swimming/dryland.org")
           ;; "* DONE Training%^{SWIM_DISTANCE}p%^{SWIM_DURATION}p\n%t%(print zp/swimming-workout-default)"
           "* DONE Training\n:LOGBOOK-CLOCK:\n:END:\n%?" :clock-in t)

          ("j" "Journal")
          ,(zp/org-capture-journal-create-template "jj" "Life")
          ,(zp/org-capture-journal-create-template "jW" "Awakening")
          ,(zp/org-capture-journal-create-template "jp" "Psychotherapy")
          ,(zp/org-capture-journal-create-template "jw" "Writing")
          ,(zp/org-capture-journal-create-template "jr" "Research")
          ,(zp/org-capture-journal-create-template "ju" "University")
          ,(zp/org-capture-journal-create-template "jh" "Hacking")
          ,(zp/org-capture-journal-create-template "jm" "Music")
          ,(zp/org-capture-journal-create-template "js" "Swimming")
          ,(zp/org-capture-journal-create-template "jd" "Dryland")

          ;; Org-protocol templates
          ("OPg" "Capture with guessed action" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %(zp/org-protocol-process \"%:link\" \"%:description\")%? :online:\n%(zp/org-protocol-insert-selection-dwim \"%i\")" :add-created t)

          ("OPc" "Capture with completion" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %(zp/org-protocol-process \"%:link\" \"%:description\" nil t)%? :online:\n%(zp/org-protocol-insert-selection-dwim \"%i\")" :add-created t)

          ("OPq" "Capture Qomon task" entry (file+headline "~/org/life.org" "Qomon")
           "* TODO [Ticket] %:description :clickup:online:\n[[%:link][Link to ClickUp ticket]]\n%?\n%(zp/org-protocol-insert-selection-dwim \"%i\")" :add-created t)

          ("OPe" "Explore" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %(zp/org-protocol-process \"%:link\" \"%:description\" \"Explore\")%? :online:\n%(zp/org-protocol-insert-selection-dwim \"%i\")" :add-created t)

          ;; Daily Record of Dysfunctional Thoughts
          ("D" "Record Dysfunctional Thoughts" entry (file+headline "~/org/journal.org" "Psychotherapy")
           "* Record of Dysfunctional Thoughts\n%T\n** Situation\n%?\n** Emotions\n** Thoughts")

          ;; Pain Diary
          ("P" "Pain Diary" entry (file+olp "~/org/life.org" "Psychotherapy" "Pain Diary")
           "* Entry: %U
** What were you doing or what happened?
%?
** What did you start struggling with psychologically?
** What thoughts came up in association with that struggle?")

          ("w" "Meditation session" entry (file+headline "~/org/projects/awakening/awakening.org.gpg" "Sessions")
           "* DONE Session%^{SESSION_DURATION}p\n%t" :immediate-finish t)

          ("WF" "S: Flat" entry (file+headline "~/org/life.org" "Inbox")
           "* %? :online:%^{PRICE}p%^{LOCATION}p%^{MEUBLÉ}p%^{M²}p
:PROPERTIES:
:LINK: [[%(print zp/org-capture-web-url)][%(print zp/org-capture-web-title)]]
:END:"
           :add-created t)))

  (defvar zp/swimming-workout-default nil
    "Default swimming workout.")

  (setq zp/swimming-workout-default "
|------+--------------------------------------------------|
|  500 | warmup crawl/fly                                 |
|  500 | 100 pull / 100 pull fast                         |
|  500 | 50 fly / 100 crawl × 3 no rest                   |
|  500 | 100 pull / 100 pull fast                         |
| 1500 | 50 fly / 100 crawl × 3 no rest incl. 2 × 100 fly |
|  100 | warmdown                                         |
|------+--------------------------------------------------|")

  ;;---------------------------------
  ;; Templates for ‘org-agenda-mode’
  ;;---------------------------------

  (use-package org-agenda
    :config
    ;; Special set of templates to be used in ‘org-agenda-mode’
    (setq zp/org-agenda-capture-templates
          '(("f" "Todo" entry (file+headline "~/org/life.org" "Inbox")
             "* TODO %?\n%t" :add-created t)
            ("r" "Todo (+time)" entry (file+headline "~/org/life.org" "Inbox")
             "* TODO %?\n%^T" :add-created t :add-warntime t)

            ("d" "Date" entry (file+olp "~/org/life.org" "Life" "Calendar")
             "* %?\n%t" :add-created t )
            ("e" "Date (+time)" entry (file+olp "~/org/life.org" "Life" "Calendar")
             "* %?\n%^T" :add-created t :add-warntime t)

            ("s" "Todo & Scheduled" entry (file+headline "~/org/life.org" "Inbox")
             "* TODO %?\nSCHEDULED: %t" :add-created t)
            ("w" "Todo & Scheduled (+time)" entry (file+headline "~/org/life.org" "Inbox")
             "* TODO %?\nSCHEDULED: %^T" :add-created t :add-warntime t)

            ("g" "Todo + Deadline" entry (file+headline "~/org/life.org" "Inbox")
             "* TODO %?\nDEADLINE: %t" :add-created t)
            ("t" "Todo & Deadline (+time)" entry (file+headline "~/org/life.org" "Inbox")
             "* TODO %?\nDEADLINE: %^T" :add-created t :add-warntime t)))

    (defun zp/org-agenda-capture (&optional arg)
      (interactive "P")
      (let ((org-capture-templates zp/org-agenda-capture-templates))
        (org-agenda-capture arg))))

  ;;------------------------------------------
  ;; Load extra minor modes based on template
  ;;------------------------------------------

  ;; Loading extra minor-modes with org-capture
  (defvar zp/org-capture-extra-minor-modes-alist nil
    "Alist of minors modes to load with specific org-capture templates.")

  (setq zp/org-capture-extra-minor-modes-alist nil)

  (defun zp/org-capture-load-extra-minor-mode ()
    "Load minor-mode based on based on key."
    (interactive)
    (let* ((key (plist-get org-capture-plist :key))
           (minor-mode (cdr (assoc key zp/org-capture-extra-minor-modes-alist))))
      (when (and key
                 minor-mode)
        (if minor-mode
            (funcall minor-mode)))))

  (add-hook 'org-capture-mode-hook #'zp/org-capture-load-extra-minor-mode)

  ;;-------------------------
  ;; Handling extra keywords
  ;;-------------------------

  (defun zp/org-capture-set-created-property ()
    "Conditionally set the CREATED property on captured trees."
    (let ((add-created (plist-get org-capture-plist :add-created))
          (type (plist-get org-capture-current-plist :type)))
      (when (and (not org-note-abort)
                 (eq type 'entry)
                 add-created)
        (unless (buffer-narrowed-p)
          (error "Cannot add CREATED when buffer is not narrowed"))
        (save-excursion
          (goto-char (point-min))
          (zp/org-set-created-property)))))

  (use-package appt
    :hook (org-capture-mode . zp/org-capture-set-appt-warntime-if-timestamp)
    :config
    (defun zp/org-capture-set-appt-warntime-if-timestamp ()
      "Conditionally set the APPT_WARNTIME on capture trees."
      (let ((add-warntime (plist-get org-capture-plist :add-warntime)))
        (when add-warntime
          (zp/org-set-appt-warntime-if-timestamp)))))

  ;;------
  ;; Rest
  ;;------

  (defun zp/org-align-all-tags ()
    "Align tags in current entry."
    (org-align-tags t))

  ;; Align tags in templates before finalising
  (add-hook 'org-capture-before-finalize-hook #'zp/org-align-all-tags)

  ;; Restore the previous window configuration after exiting
  (defvar zp/org-capture-before-config nil
    "Window configuration before ‘org-capture’.")

  (defadvice org-capture (before save-config activate)
    "Save the window configuration before ‘org-capture’."
    (setq zp/org-capture-before-config (current-window-configuration)))

  (defun zp/org-capture-make-full-frame ()
    "Maximise the org-capture frame if :full-frame is non-nil."
    (let ((full-frame (plist-get org-capture-plist :full-frame)))
      (if full-frame
          (delete-other-windows)))))

(use-package zp-org-protocol
  :custom
  (zp/org-protocol-verbs (list "Read"
                               "Explore"
                               "Investigate"
                               "Listen to"
                               "Watch")))

(use-package org-capture-web
  :commands (zp/org-capture-web
             zp/org-capture-web-kill
             zp/org-capture-web-letterboxd)
  :config
  (setq zp/org-capture-web-default-target
        '(file+headline "~/org/life.org" "Inbox")))

;;----------------------------------------------------------------------------
;; bibtex-completion
;;----------------------------------------------------------------------------
(use-package bibtex-completion
  :load-path "~/projects/helm-bibtex/")

(use-package helm-bibtex
  :load-path "~/projects/helm-bibtex/"
  :bind (("s-p" . helm-bibtex)
         ("s-M-p" . helm-bibtex-with-notes)
         ("C-c D" . zp/bibtex-completion-message-key-last))
  :config
  (setq bibtex-completion-notes-path "~/org/lit/")

  (setq bibtex-completion-notes-template-multiple-files
        "#+TITLE: ${=key=}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\nLiterature notes for cite:${=key=}.\n\n")

  ;; TODO: Modernise
  ;; A lot of this code is baby Elisp.

  ;;------------------------
  ;; helm-bibtex-select-bib
  ;;------------------------

  (defvar zp/bibtex-completion-bib-data-alist nil
    "Alist of the bibliography files and their labels.")

  (defvar zp/bibtex-completion-bib-data nil
    "Processed alist of the bibliography files and their labels,
  including an entry with all of them.")

  (defun zp/bibtex-completion-bib-data-format ()
    (interactive)
    (let* ((alist zp/bibtex-completion-bib-data-alist)
           (all-bib (mapcar 'cdr alist)))
      (setq zp/bibtex-completion-bib-data
            (list (cons "All entries" (list all-bib)) alist))))

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

  ;;------------
  ;; Completion
  ;;------------

  (setq zp/bibtex-completion-bib-data-alist
        '(;; ("Monty Python" . "~/org/bib/monty-python.bib")
          ;; ;; ("Monty Python - Extra" . "~/org/bib/monty-python-extra.bib")
          ;; ("FromSoftware" . "~/org/bib/fromsoftware.bib")
          ;; ("Visual" . "~/org/bib/visual.bib")
          ;; ("Film studies" . "~/org/bib/film-studies.bib")
          ;; ("Writing" . "~/org/bib/writing.bib")
          ;; ("Metaphor" . "~/org/bib/metaphor.bib")
          ;; ("Processing" . "~/org/bib/processing.bib")
          ;; ("Emacs" . "~/org/bib/emacs.bib")
          ;; ("Git" . "~/org/bib/git.bib")
          ("Library" . "~/org/bib/library.bib")))

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

        bibtex-completion-additional-search-fields '(subtitle booksubtitle keywords tags library))

  (define-key bibtex-mode-map (kbd "C-c M-o") 'bibtex-Online)

  ;; Define which citation function to use on a buffer basis
  (setq bibtex-completion-format-citation-functions
        '(;; (org-mode      . org-ref-bibtex-completion-format-org)
          (org-mode . org-ref-format-citation)
          (latex-mode . bibtex-completion-format-citation-cite)
          (bibtex-mode . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default . bibtex-completion-format-citation-default)))

  ;; Default citation command
  (setq bibtex-completion-cite-default-command "cite")

  ;; PDF open function
  (setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)

  ;; Helm
  (defun zp/helm-bibtex-with-local-bibliography (&optional arg)
    "Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography
reread."
    (interactive "P")
    (let* ((local-bib (bibtex-completion-find-local-bibliography))
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
    (let ((last-keys zp/bibtex-completion-key-last))
      (if (bound-and-true-p last-keys)
          (insert last-keys)
        (zp/helm-bibtex-solo-action-insert-key))))

  (defun zp/bibtex-completion-message-key-last ()
    (interactive)
    (let ((bib-keys (zp/bibtex-completion-format-citation-comma-space
                 zp/bibtex-completion-key-last)))
      (if (bound-and-true-p bib-keys)
          (message (concat "Last key(s) used: " bib-keys "."))
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
          (previous-actions (helm-get-attr 'action helm-source-bibtex))
          (new-action zp/helm-source-bibtex-insert-key))
      (helm-set-attr 'action new-action helm-source-bibtex)
      (helm-bibtex)
      ;; Wrapping with (progn (foo) nil) suppress the output
      (progn (helm-set-attr 'action previous-actions helm-source-bibtex) nil))))

(use-package ivy-bibtex
  :load-path "~/projects/helm-bibtex/")

;;----------------------------------------------------------------------------
;; org-ref
;;----------------------------------------------------------------------------
(use-package org-ref
  :requires org
  ;; :load-path "~/projects/org-ref/"
  :config
  (setq org-ref-bibliography-notes "~/org/bib/notes.org"
        reftex-default-bibliography '("~/org/bib/monty-python.bib")
        org-ref-pdf-directory "~/org/bib/pdf"
        org-ref-show-broken-links nil)

  ;; Use helm-bibtex data the default bibliography
  (setq org-ref-default-bibliography
        (mapcar #'cdr zp/bibtex-completion-bib-data-alist))


  (defun org-ref-get-bibtex-entry-md (key)
    "Return a md string for the bibliography entry corresponding to KEY."
    ;; We create an anchor to the key that we can jump to, and provide a jump back
    ;; link with the md5 of the key.
    (let ((org-ref-formatted-citation-backend "md"))
      (format "<a id=\"%s\"></a>%s [↩](#%s)"
              key
              (org-ref-format-entry key)
              (md5 key))))

  (setq org-ref-formatted-citation-formats
        '(("text"
           ("article" . "${author}, ${title}, ${journal}, ${volume}(${number}), ${pages} (${year}). ${doi}")
           ("inproceedings" . "${author}, ${title}, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("book" . "${author}, ${title} (${year}), ${address}: ${publisher}.") ("phdthesis"
                                                                                  . "${author}, ${title} (Doctoral dissertation) (${year}). ${school}, ${address}.")
           ("inbook" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("incollection" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("proceedings" . "${editor} (Eds.), ${booktitle} (${year}). ${address}: ${publisher}.")
           ("unpublished" . "${author}, ${title} (${year}). Unpublished manuscript.") (nil
                                                                                       . "${author}, ${title} (${year})."))
          ("org"
           ("article" . "${author}, /${title}/, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
           ("inproceedings" . "${author}, /${title}/, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("book" . "${author}, /${title}/ (${year}), ${address}: ${publisher}.") ("phdthesis"
                                                                                    . "${author}, /${title}/ (Doctoral dissertation) (${year}). ${school}, ${address}.")
           ("inbook" . "${author}, /${title}/, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("incollection" . "${author}, /${title}/, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
           ("unpublished" . "${author}, /${title}/ (${year}). Unpublished manuscript.")
           (nil . "${author}, /${title}/ (${year})."))

          ("md"
           ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
           ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("book" . "${author}, *${title}* (${date}), ${location}: ${publisher}.")
           ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
           ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
           ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
           ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
           (nil . "${author}, *${title}* (${year}).")))))

;;----------------------------------------------------------------------------
;; org-roam
;;----------------------------------------------------------------------------
(use-package org-roam
  :load-path "~/projects/org-roam/"
  ;; :after org
  ;; :commands
  ;; (org-roam-buffer
  ;;  org-roam-setup
  ;;  org-roam-capture
  ;;  org-roam-node-find)
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c m l" . org-roam-buffer-toggle)
         ("C-c m f" . org-roam-node-find)
         ;; ("C-c m r" . org-roam-find-ref)
         ("C-c m ." . zp/org-roam-find-directory)
         ("C-c m >" . zp/org-roam-find-directory-testing)
         ("C-c m j" . org-roam-jump-to-index)
         ;; ("C-c m b" . org-roam-switch-to-buffer)
         ;; ("C-c m g" . org-roam-graph)
         :map org-mode-map
         (("C-c m i" . org-roam-node-insert)))
  :custom
  (org-roam-directory "~/org/slip-box/")
  ;; (org-roam-directory "~/projects/exp2exp.github.io/src/")
  ;; (org-roam-db "~/projects/exp2exp.github.io/src/org-roam.db")
  ;; (org-roam-directory "~/org/slip-box-bristol/")
  (org-roam-db-location (concat org-roam-directory "org-roam.db"))
  :config
  (org-roam-db-autosync-mode)

  (setq org-roam-capture-templates
        `(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("b" "bristol" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+filetags: :bristol:group-%^{Group?||1|2|3|4}:%^{Type?||litany|system|worldview|myth}:group-%\\1-%\\2\n\n")
           :unnarrowed t)
          ("r" "ref" plain
           "%?"
           :target (file+head "refs/${citekey}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("p" "ref + physical" plain
           "%?"
           :target (file+head "refs/${citekey}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n* Notes :physical:")
           :unnarrowed t)
          ("n" "ref + noter" plain
           "%?"
           :target (file+head "refs/${citekey}.org"
                              ,(s-join "\n" (list "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n"
                                                  "* Notes :noter:"
                                                  ":PROPERTIES:"
                                                  ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
                                                  ":NOTER_PAGE:"
                                                  ":END:")))
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain
           "%?"
           :target (file+head "web/${slug}.org"
                              "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n%(zp/org-protocol-insert-selection-dwim \"%i\")")
           :unnarrowed t)
          ("i" "incremental" plain
           "* %?\n%(zp/org-protocol-insert-selection-dwim \"%i\")"
           :target (file+head "web/${slug}.org"
                              "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
           :unnarrowed t
           :empty-lines-before 1)))

  (defun zp/org-roam-find-directory ()
    (interactive)
    (dired org-roam-directory))

  (defcustom org-roam-index-file "index.org"
    "Path to the Org-roam index file.
The path can be a string or a function.  If it is a string, it
should be the path (absolute or relative to `org-roam-directory')
to the index file.  If it is is a function, the function should
return the path to the index file.  Otherwise, the index is
assumed to be a note in `org-roam-directory' whose title is
'Index'."
    :type '(choice
            (string :tag "Path to index" "%s")
            (function :tag "Function to generate the path"))
    :group 'org-roam)

  (defun org-roam--get-index-path ()
    "Return the path to the index in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, it is assumed to be a note in `org-roam-directory'
whose title is 'Index'."
    (let ((path (pcase org-roam-index-file
                  ((pred functionp) (funcall org-roam-index-file))
                  ((pred stringp) org-roam-index-file)
                  ('() (user-error "You need to set `org-roam-index-file' before you can jump to it"))
                  (wrong-type (signal 'wrong-type-argument
                                      `((functionp stringp)
                                        ,wrong-type))))))
      (expand-file-name path org-roam-directory)))

  (defun org-roam-jump-to-index ()
    "Find the index file in `org-roam-directory'.
The path to the index can be defined in `org-roam-index-file'.
Otherwise, the function will look in your `org-roam-directory'
for a note whose title is 'Index'.  If it does not exist, the
command will offer you to create one."
    (interactive)
    (let ((index (org-roam--get-index-path)))
      (if (and index
               (file-exists-p index))
          (find-file index)
        (when (y-or-n-p "Index file does not exist.  Would you like to create it? ")
          (message "Do it yourself; it’s broken")))))

  (defvar zp/org-roam-directory-testing "~/org/slip-box-testing")

  (defun zp/org-roam-find-directory-testing ()
    (interactive)
    (find-file zp/org-roam-directory-testing))

  ;;(setq org-roam-mode-sections
  ;;      (list #'org-roam-backlinks-insert-section
  ;;            #'org-roam-reflinks-insert-section
  ;;            #'org-roam-unlinked-references-insert-section))
  )

(use-package org-roam-dailies
  :load-path "~/projects/org-roam/extensions/"
  :bind (("C-c m d" . org-roam-dailies-map))
  :custom
  (org-roam-dailies-directory "scratch/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n\n")
      :add-created t))))

(use-package org-roam-protocol)

(use-package org-roam-ui
  ;; :load-path "~/projects/org-roam-ui"
  :config
  (setq org-roam-ui-open-on-start nil))

(use-package company
  :bind (("M-/" . company-complete)
         ;; (:map company-mode-map
         ;;  (([remap indent-for-tab-command] . #'company-indent-or-complete-common)))
         )
  :custom
  (company-idle-delay . nil)
  :hook ((after-init . global-company-mode))
  :config
  (add-to-list 'company-backends 'company-capf))

(defvar orb-title-format "${author-or-editor-abbrev} (${date}).  ${title}."
  "Format of the title to use for `orb-templates'.")

(use-package org-roam-bibtex
  :config
  (org-roam-bibtex-mode 1)
  (setq orb-insert-interface 'helm-bibtex)
  (setq orb-attached-file-extensions '("pdf" "epub")))

;; (use-package org-roam-bibtex
;;   :requires bibtex-completion
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :load-path "~/projects/org-roam-bibtex/"
;;   :bind (:map org-roam-bibtex-mode-map
;;          (("C-c m f" . orb-find-non-ref-file))
;;          :map org-mode-map
;;          (("C-c m t" . orb-insert-non-ref)
;;           ("C-c m a" . orb-note-actions)))
;;   :init
;;   :custom
;;   (orb-autokey-format "%a%y")
;;   (orb-templates
;;    `(("r" "ref" plain
;;       (function org-roam-capture--get-point)
;;       ""
;;       :file-name "refs/${citekey}"
;;       :head ,(s-join "\n"
;;                      (list
;;                       (concat "#+title: "
;;                               orb-title-format)
;;                       "#+roam_key: ${ref}"
;;                       "#+created: %U"
;;                       "#+last_modified: %U\n\n"))
;;       :unnarrowed t)
;;      ("p" "ref + physical" plain
;;       (function org-roam-capture--get-point)
;;       ""
;;       :file-name "refs/${citekey}"
;;       :head ,(s-join "\n"
;;                      (list
;;                       (concat "#+title: "
;;                               orb-title-format)
;;                       "#+roam_key: ${ref}"
;;                       ""
;;                       "* Notes :physical:")))
;;      ("n" "ref + noter" plain
;;       (function org-roam-capture--get-point)
;;       ""
;;       :file-name "refs/${citekey}"
;;       :head ,(s-join "\n"
;;                      (list
;;                       (concat "#+title: "
;;                               orb-title-format)
;;                       "#+roam_key: ${ref}"
;;                       ""
;;                       "* Notes :noter:"
;;                       ":PROPERTIES:"
;;                       ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
;;                       ":NOTER_PAGE:"
;;                       ":END:"))))))

;; (use-package org-roam-server
;;   :ensure t
;;   :bind (:map org-roam-mode-map
;;          (("C-c m G" . org-roam-server-mode)))
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

;;----------------------------------------------------------------------------
;; hydra-org-refile
;;----------------------------------------------------------------------------
(use-package hydra-org-refile
  :commands (zp/org-jump-dwim
             zp/org-refile-dwim
             zp/hydra-org-refile)
  :bind ("C-c C-j" . zp/org-jump-dwim)
  ;; :after (:any org org-capture)
  :init
  (setq zp/org-agenda-files-primary "~/org/life.org")
  ;; ‘hydra-org-refile’ needs to modify the keymaps of ‘org-mode’,
  ;; ‘org-agenda-mode’, and ‘org-capture-mode’, but since those packages are
  ;; loaded lazily, we can’t simply add new key-bindings to their keymaps
  ;; because they might have not been initialised.  Instead, we defer the
  ;; feature-related key-binding assignments until their corresponding feature
  ;; has been loaded.
  (use-package org
    :bind (:map org-mode-map
           ("C-c C-j" . zp/org-jump-dwim)
           ("C-c C-w" . zp/org-refile-dwim))
    :hook (org-mode . visual-line-mode))

  (use-package org-agenda
    :bind (:map org-agenda-mode-map
           ("C-c C-w" . zp/hydra-org-refile )))

  (use-package org-capture
    :bind (:map org-capture-mode-map
           ("C-c C-w" . zp/hydra-org-refile )))
  :config
  ;; Exclude separators in all org-refile commands
  (setq org-refile-target-verify-function
        'zp/org-refile-target-verify-exclude-separators))

;;----------------------------------------------------------------------------
;; org-brain
;;----------------------------------------------------------------------------
;; Disabled because I don’t use it
(use-package org-brain
  :disabled
  :config
  (setq org-brain-path "~/org/brain")

  ;; Commented because already the default
  ;; (setq org-id-track-globally t)
  ;; (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  )

;;----------------------------------------------------------------------------
;; appt
;;----------------------------------------------------------------------------
(use-package appt
  :hook (;; Update reminders when…
         ;; Saving org-agenda-files agenda
         (after-save . zp/org-agenda-to-appt-on-save)
         ;; Loading the org-agenda for the first time
         (org-agenda-finalize . zp/org-agenda-to-appt-on-load)
         ;; After marking a task with APPT_WARNTIME as DONE
         (org-after-todo-state-change . zp/org-agenda-to-appt-on-done))
  :config
  (appt-activate t)

  (setq appt-message-warning-time 15
        appt-display-interval 5
        appt-display-mode-line nil)

  (defun zp/org-appt-check-warntime (&optional pom)
    "Check APPT_WARNTIME for current item.

Return nil if APPT_WARNTIME is ‘none’"
    (not (string= "none" (org-entry-get (or pom
                                            (point))
                                        "APPT_WARNTIME"))))

  (defun zp/org-agenda-to-appt-check-warntime (arg)
    "Check APPT_WARNTIME for current item in the agenda.

This is a filter intended to be use with ‘org-agenda-to-appt’."
    (let ((marker (get-text-property (1- (length arg)) 'org-hd-marker arg)))
      (org-with-point-at marker
        (zp/org-appt-check-warntime marker))))

  ;; Use appointment data from org-mode
  (defun zp/org-agenda-to-appt (&optional arg)
    "Update appt-list based on org-agenda items."
    (interactive "p")
    (setq appt-time-msg-list nil)
    (when (eq arg 4)
      (appt-check))
    (with-temp-message (current-message)
      (org-agenda-to-appt nil 'zp/org-agenda-to-appt-check-warntime))
    (when arg
      (message
       (pcase arg
         (4 "appt has been reset and updated.")
         (_ "appt has been updated.")))))

  ;; TODO: Rename variables to more meaningful names
  ;; The name refers to the rôle they’ll have in the hook rather than to what
  ;; they’re actually doing

  (defun zp/org-agenda-to-appt-on-load ()
    "Hook to `org-agenda-finalize-hook' which creates the appt-list
on init and them removes itself."
    (zp/org-agenda-to-appt)
    (remove-hook 'org-agenda-finalize-hook #'zp/org-agenda-to-appt-on-load))

  (defun zp/org-agenda-to-appt-on-save ()
    "Update appt if buffer is visiting a file in ‘org-agenda-files’."
    (let ((file (or (buffer-file-name)
                    (buffer-file-name (buffer-base-buffer))))
          (agenda-files
           (mapcar #'expand-file-name org-agenda-files)))
      (if (member file agenda-files)
          (zp/org-agenda-to-appt))))

  (defun zp/org-agenda-to-appt-on-done ()
    "Update appt if task with APPT_WARNTIME is marked as DONE."
    (when-let* ((done (org-entry-get (point) "TODO" nil))
                (warntime (zp/org-appt-check-warntime)))
      (zp/org-agenda-to-appt)))

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

  ;; ----------------------------------------
  ;; Update reminders when…

  ;; Starting Emacs
  ;; (zp/org-agenda-to-appt)

  ;; Everyday at 12:05am
  ;; (run-at-time "12:05am" (* 24 3600) 'zp/org-agenda-to-appt)
  ;; ----------------------------------------

  ;; Display appointments as a window manager notification
  (setq appt-disp-window-function 'zp/appt-display)

  ;; Prevent appt from deletingg any windows after notifying
  (setq appt-delete-window-function (lambda () t))

  ;; Notification script to handle appt
  (setq zp/appt-notification-app "~/bin/appt-notify")

  (defun zp/appt-display (min-to-app _new-time msg)
    (if (atom min-to-app)
        (start-process "zp/appt-notification-app" nil zp/appt-notification-app min-to-app msg)
      (dolist (i (number-sequence 0 (1- (length min-to-app))))
        (start-process "zp/appt-notification-app" nil zp/appt-notification-app (nth i min-to-app) (nth i msg)))))

  ;; Conditional APPT_WARNTIME
  (defun zp/org-set-appt-warntime-if-timestamp ()
    "Prompt for APPT_WARNTIME if the heading is a timestamp."
    (let ((warntime (org-entry-get (point) "APPT_WARNTIME")))
      (unless warntime
        (save-excursion
          (org-back-to-heading t)
          (let ((end (save-excursion (outline-next-heading) (point))))
            (when (re-search-forward org-stamp-time-of-day-regexp
                                     end t)
              (zp/org-set-appt-warntime)))))))

  (defun zp/org-set-appt-warntime-if-timestamp-advice (&rest _args)
    "Prompt for APPT_WARNTIME if the heading is a timestamp.

This function is intended to be used as an advice.

ARGS is only there to catch the shared arguments between the
advised function and this one."
    (zp/org-set-appt-warntime-if-timestamp))

  ;; Advise timestamp-related commands
  ;; (zp/advise-commands
  ;;  add
  ;;  (org-schedule
  ;;   org-deadline
  ;;   org-time-stamp)
  ;;  after
  ;;  zp/org-set-appt-warntime-if-timestamp-advice)
  )

;;----------------------------------------------------------------------------
;; ledger
;;----------------------------------------------------------------------------
(use-package ledger-mode
  :bind (:map ledger-mode-map
         ("C-c C-d" . ledger-kill-current-transaction))
  :config
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

  (defvar ledger-use-iso-dates nil)
  (defvar ledger-reconcile-default-commodity nil)
  (defvar ledger-post-auto-adjust-amounts nil)
  (setq ledger-use-iso-dates t
        ledger-reconcile-default-commodity "EUR"
        ;; Testing
        ledger-post-auto-adjust-amounts 1
        ledger-schedule-file "~/org/ledger/main-schedule.ledger.gpg")

  (add-hook 'ledger-reconcile-mode-hook #'balance-windows)

  (defun zp/ledger-close-scheduled ()
    "Close the Ledger Scheduled buffer and window."
    (interactive)
    (if (string-match-p (regexp-quote "*Ledger Schedule*") (buffer-name))
        (progn
          (kill-buffer)
          (select-window (previous-window))
          (delete-other-windows))))
  (define-key ledger-mode-map (kbd "S-<backspace>") 'zp/ledger-close-scheduled)

  ;; Patch for inserting an empty line after copied transactions
  (defvar ledger-copy-transaction-insert-blank-line-after nil
    "Non-nil means insert blank line after a transaction inserted
  with ‘ledger-copy-transaction-at-point’.")

  (defun ledger-copy-transaction-at-point (date)
    "Ask for a new DATE and copy the transaction under point to
that date.  Leave point on the first amount."
    (interactive (list
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
      (kill-region (car bounds) (cadr bounds)))))

;;----------------------------------------------------------------------------
;; chronos
;;----------------------------------------------------------------------------
(use-package chronos
  :demand
  :config
  (setq helm-chronos-recent-timers-limit 100
        helm-chronos-standard-timers
        '(
          "Green Tea (Short)            2/Green Tea: Remove tea bag"
          "Green Tea (Medium)      0:2:30/Green Tea: Remove tea bag"
          "Green Tea (Long)             3/Green Tea: Remove tea bag"
          "Black Tea (Medium)           4/Black Tea: Remove tea bag"
          "Black Tea (Long)             5/Black Tea: Remove tea bag"
          "Herbal Tea                  10/Herbal Tea: Remove tea bag"
          "Timebox                     25/Finish and Reflect + 5/Back to it"
          "Break                       30/Back to it"
          "Charge Phone                30/Unplug Phone"
          "Charge Tablet               60/Unplug Tablet"
          "Charge Headphones           15/Unplug Headphones"
          "Charge Battery             120/Unplug Battery"
          "Class: Very Short            1/End"
          "Class: Short                 5/End"
          "Class: Medium               10/End"
          "Class: Long                 15/End"
          ))

  ;;---------------------
  ;; Notification system
  ;;---------------------

  (defun chronos-notify (c)
    "Notify expiration of timer C using custom script."
    (chronos--shell-command "Chronos notification"
                            "chronos-notify"
                            (list (chronos--time-string c)
                                  (chronos--message c))))

  (setq chronos-expiry-functions '(chronos-notify))

  ;;-------------
  ;; Quick edits
  ;;-------------

  (defun zp/chronos-edit-selected-line-time (time prefix)
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
        (quit-window)))))

(use-package helm-chronos
  :load-path "~/projects/helm-chronos/"
  :bind (("s-;" . zp/switch-to-chronos-dwim)
         ("s-M-;" . zp/helm-chronos-add)
         (:map chronos-mode-map
          (("a" . 'helm-chronos-add-timer)
           ("A" . (lambda ()
                    (interactive)
                    (let ((helm-chronos-add-relatively t))
                      (helm-chronos-add-timer))))
           ("q" . zp/chronos-quit)

           ;; Quick keys
           ("U" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "-0:00:05" "5 s")))
           ("I" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "+0:00:05" "5 s")))
           ("u" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "-0:00:15" "15 s")))
           ("i" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "+0:00:15" "15 s")))
           ("j" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "-0:01:00" "1 min")))
           ("k" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "+0:01:00" "1 min")))
           ("J" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "-0:05:00" "5 min")))
           ("K" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "+0:05:00" "5 min")))
           ("m" . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "-0:10:00" "10 min")))
           ("," . (lambda ()
                    (interactive)
                    (zp/chronos-edit-quick "+0:10:00" "10 min"))))))
  :bind
  :after chronos
  :config
  ;; Fix for adding new timers with helm-chronos
  ;; TODO: Check if still necessary with ‘helm-chronos-patched’
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

  (defun zp/switch-to-chronos (&optional print-message)
    "Switch to and from chronos’s main buffer.

Also initialise chronos if it wasn’t live.

Return t when switching to chronos, nil otherwise."
    (interactive "p")
    (cond ((derived-mode-p 'chronos-mode)
           (zp/chronos-quit)
           (when print-message
             (message "Exited chronos."))
           nil)
          (t
           (if-let ((buffer (get-buffer "*chronos*")))
               (switch-to-buffer buffer)
             (chronos-initialize))
           (when print-message
             (message "Switched to chronos."))
           t)))

  (defun zp/helm-chronos-add (&optional arg visit)
    "Add a new chronos timer with ‘helm-chronos-add-timer’.

This wrapper displays the current list of timers in the current
buffer.

With a ‘C-u’ argument or when VISIT is non-nil, stay in chronos
after adding the timer."
    (interactive "p")
    (let ((in-chronos (derived-mode-p 'chronos-mode)))
      (unless in-chronos
        (zp/switch-to-chronos))
      (helm-chronos-add-timer)
      (unless (or visit
                  (eq arg 4))
        (zp/chronos-quit)))
    (when arg
      (message "Timer has been added.")))

  (defun zp/switch-to-chronos-dwim (arg &optional add-only)
    "Conditionally switch to and from chronos’s main buffer.

With a ‘C-u’ argument or when ADD-ONLY is non-nil, only visit
chronos’s main buffer for adding a new timer."
    (interactive "p")
    (if (or add-only
            (eq arg 4))
        (zp/helm-chronos-add (when arg 1))
      (zp/switch-to-chronos arg))))

;;----------------------------------------------------------------------------
;; org-noter
;;----------------------------------------------------------------------------
(use-package org-noter
  :bind (:map org-mode-map
         (("C-c N" . zp/org-noter-dwim))
         :map org-noter-doc-mode-map
         (("M-i" . zp/org-noter-insert-precise-note-dwim)))
  :config
  (setq org-noter-hide-other t
        org-noter-auto-save-last-location t
        org-noter-doc-split-fraction '(0.57 0.43))

  (defun zp/org-noter-visual-line-mode ()
    "Enable visual-line-mode in ‘org-noter’ notes.

Workaround to counter race conditions with the margins."
    (let ((parent (current-buffer))
          (refresh (lambda (parent)
                     (with-current-buffer parent
                       (visual-line-mode 'toggle)
                       (visual-line-mode 'toggle)))))
      (run-at-time "1 sec" nil refresh parent)
      (run-at-time "5 sec" nil refresh parent)))

  (add-hook 'org-noter-notes-mode-hook #'zp/org-noter-visual-line-mode)

  ;; Fix for hiding truncation
  (defun org-noter--set-notes-scroll (_window &rest _ignored)
    nil)

  ;; Fix for visual-line-mode with PDF files
  (defun org-noter--note-after-tipping-point (_point _note-property _view)
    nil)

  (defun zp/org-noter-indirect (arg)
    "Ensure that org-noter starts in an indirect buffer.

Without this wrapper, org-noter creates a direct buffer
restricted to the notes, but this causes problems with the refile
system.  Namely, the notes buffer gets identified as an
agenda-files buffer.

This wrapper addresses it by having org-noter act on an indirect
buffer, thereby propagating the indirectness."
    (interactive "P")
    (if (org-entry-get nil org-noter-property-doc-file)
        (with-selected-window (zp/org-tree-to-indirect-buffer-folded nil t)
          (org-noter arg)
          (kill-buffer))
      (org-noter arg)))

  (defun zp/org-noter-dwim (arg)
    "Run org-noter on the current tree, even if we’re in the agenda."
    (interactive "P")
    (let ((in-agenda (derived-mode-p 'org-agenda-mode))
          (marker))
      (cond (in-agenda
             (setq marker (get-text-property (point) 'org-marker))
             (with-current-buffer (marker-buffer marker)
               (goto-char marker)
               (unless (org-entry-get nil org-noter-property-doc-file)
                 (user-error "No org-noter info on this tree"))
               (zp/org-noter-indirect arg)))
            (t
             (zp/org-noter-indirect arg)
             (setq marker (point-marker))))
      (org-with-point-at marker
        (let ((tags (org-get-tags)))
          (when (and (org-entry-get nil org-noter-property-doc-file)
                     (not (member "noter" tags)))
            (org-set-tags (push "noter" tags)))))
      (unless in-agenda
        (set-marker marker nil))))

  (defun zp/org-noter-insert-precise-note-dwim (force-mouse)
    "Insert note associated with a specific location.

If in nov-mode, use point rather than the mouse to target the
position."
    (interactive "P")
    (if (and (derived-mode-p 'nov-mode)
             (not force-mouse))
        (let ((pos (if (region-active-p)
                       (min (region-beginning) (point))
                     (point))))
          (org-noter-insert-note pos))
      (org-noter-insert-precise-note)))

  (define-key org-noter-doc-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key org-noter-doc-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page))

;;----------------------------------------------------------------------------
;; Psychotherapy
;;----------------------------------------------------------------------------
(use-package psychotherapy
  :requires (org org-capture)
  :config
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

  ;; Load ‘zp/psychotherapy-mode’ with the org-capture-template ‘D’
  ;; (add-to-list 'zp/org-capture-extra-minor-modes-alist
  ;;              '("D" . zp/psychotherapy-mode))
  )

;;----------------------------------------------------------------------------
;; Feedback sounds
;;----------------------------------------------------------------------------
(use-package feedback-sounds
  :hook ((org-clock-in-prepare . zp/play-sound-clock-in)
         (org-clock-out . zp/play-sound-clock-out)
         (org-clock-cancel . zp/play-sound-clock-out)
         (org-after-todo-state-change . zp/play-sound-reward)
         (org-capture-mode . zp/play-sound-start-capture)
         (org-capture-after-finalize . zp/play-sound-after-capture)
         (zp/org-after-view-change . zp/play-sound-turn-page)
         (org-roam-dailies-find-file . zp/play-sound-turn-page)
         (zp/org-after-refile . zp/play-sound-turn-page)
         (org-follow-link . zp/play-sound-turn-page))
  :requires org)

;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
(defun zp/echo-buffer-name ()
  (interactive)
  (message (concat "Current buffer: " (replace-regexp-in-string "%" "%%" (buffer-name)))))

;;----------------------------------------------------------------------------
;; External
;;----------------------------------------------------------------------------
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

(global-set-key (kbd "s-M-.") 'herald-the-mode-line)

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

;;----------------------------------------------------------------------------
;; Mode-line
;;----------------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10))


(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

(use-package minions
  :disabled
  :config
  (minions-mode 1))

(use-package moody
  :disabled
  :config
  (setq moody-mode-line-height 40)

  ;; TODO: Check if really useful
  (setq x-underline-at-descent-line t)

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(use-package theme
  :demand
  :bind (("C-c y" . zp/variable-pitch-mode)
         ("C-c T" . zp/switch-emacs-theme)
         :map zp/toggle-map
         (("t" . zp/switch-emacs-theme)
          ("y" . zp/helm-select-font-dwim)))
  :config
  ;; Fonts
  (zp/set-font "sarasa")
  (zp/set-font-variable "equity")

  ;; Day/night cycle
  (setq zp/time-of-day-sections '("04:00" "05:00" "18:00" "19:00" "00:00"))
  (zp/emacs-dark-theme)
  (zp/switch-theme-auto))

;;----------------------------------------------------------------------------
;; Interaction with terminal emulators
;;----------------------------------------------------------------------------
(defun zp/term-dwim (&optional arg)
  "Run terminator in the CWD.

Trim unnecessary TRAMP information from the path (e.g. /sudo:…),
and forward it to terminator.  ARGUMENTS can be any argument
accepted by terminator (e.g. ‘-x command’).

See ‘~/.bin/terminator-dwim’ for more info."
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    (let* ((path-emacs default-directory)
           (tramp-regex "/sudo:root@.*?:")
           (path (expand-file-name
                  (replace-regexp-in-string
                   tramp-regex "" path-emacs))))
      (start-process-shell-command
       "term"
       nil
       (let (str)
         (setq str (concat "cd \"" path "\" && kitty"
                           (if arg (concat " " arg))))
         (message str))))))

;;----------------------------------------------------------------------------
;; Late packages
;;----------------------------------------------------------------------------
;; Packages which are required to be loaded late
;; TODO: See if I can handle that with use-package

;; Magnars's codes
;; expand-region causes weird flicker with repeated tasks if it's at the top
;; TODO: Confirm if this is still the case
(use-package expand-region
  :config
  (global-set-key (kbd "s-h") 'er/expand-region))

(use-package multiple-cursors-core)

(use-package mc-edit-lines
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package mc-mark-more
  :bind (("M-<mouse-1>" . mc/add-cursor-on-click))
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Disable lighters for some minor-modes
(use-package diminish
  :config
  (diminish 'ivy-mode)
  (diminish 'helm-mode)
  (diminish 'auto-revert-mode)
  (diminish 'anzu-mode)
  (diminish 'yas-minor-mode)
  (diminish 'which-key-mode)
  (diminish 'volatile-highlights-mode)
  (diminish 'undo-tree-mode)
  (diminish 'whitespace-mode)
  (diminish 'magit-wip-mode)
  (diminish 'ws-butler-mode))

;;----------------------------------------------------------------------------
;;; Late hotfixes
;;----------------------------------------------------------------------------
(setq zp/original-init-finished t)

;;----------------------------------------------------------------------------
;; Custom
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)

;;; init.el ends here
