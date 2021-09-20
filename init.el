;;; init.el --- Initialisation file for Emacs -*- fill-column: 78; lexical-binding: t; byte-compile-warnings: (not free-vars) -*-

;; Copyright © 2013-2021 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
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

;; Change default face to prevent flashing
(set-face-attribute 'default nil :foreground "#bcaf8e" :background "#141414")

(setq default-directory "~")
(setq inhibit-startup-screen 1)
(setq initial-scratch-message ";; Emacs Scratch\n\n")

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

;; Set default fill column to 78
(setq-default fill-column 78)

;; Add folders to load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/extra/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; Point to my Emacs fork for studying built-in functions
(setq source-directory "~/projects/emacs/")

;; Time
(setq display-time-24hr-format t)

;; Turn off background when Emacs is run with -nt
(defun on-after-init ()
  "Turn off background."
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
        ("." nil (reusable-frames . visible))))

;; Enable disabled commands
(setq disabled-command-function nil)

;; Do not display continuation lines
(set-default 'truncate-lines t)

;; Enable line-restricted horizontal scrolling
(setq auto-hscroll-mode 'current-line)

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
;; Necessary for for some Ivy/Helm commands
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

;;----------------------------------------------------------------------------
;; Scratch directories
;;----------------------------------------------------------------------------
(require 'vc-git)

(defun scratch-dir-path (name)
  "Format a new scratch dir-path based on NAME and timestamp."
  (concat "~/scratch.d/scratch-"
          (format-time-string "%Y-%m-%d_%s")
          (when (not (string= name ""))
            (concat "--" name))
          "/"))

(defun scratch-dir (&optional use-git name)
  "Create an ad-hoc working directory and open it in dired.

When USE-GIT is non-nil, init git in the created directory with
the name NAME.

Prefix argument initializes the Git repository."
  (interactive "P\nMName: ")
  (let ((directory (expand-file-name (scratch-dir-path name))))
    (make-directory directory t)
    (when (file-symlink-p "~/scratch")
      (delete-file "~/scratch"))
    (make-symbolic-link directory "~/scratch" t)
    (when (car use-git)
      (let ((default-directory directory))
        (vc-git-create-repo)))
    (find-file directory)))

;;----------------------------------------------------------------------------
;; Helper functions & macros
;;----------------------------------------------------------------------------
(defun zp/get-string-from-file (file-path)
  "Read file content from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; TODO: Does it need to be macro?
(defmacro zp/advise-commands (method commands where function)
  "Macro for advising COMMANDS with FUNCTION.

METHOD and WHERE follows the same syntax as `add-advice'.  COMMANDS should
be the list of commands to advice."
  (let ((where-keyword (intern-soft (concat ":" (symbol-name where)))))
    `(progn
       ,@(cond ((string= method 'add)
                (mapcar (lambda (command)
                          `(advice-add ',command ,where-keyword ',function))
                        commands))
               ((string= method 'remove)
                (mapcar (lambda (command)
                          `(advice-remove ',command  ',function))
                        commands))))))

(defmacro zp/add-hooks (method commands function)
  "Add FUNCTION as a hook to COMMANDS with METHOD.
See `add-hooks' for details."
  `(progn
     ,@(cond ((string= method 'add)
              (mapcar (lambda (command)
                        `(add-hook ',command ',function))
                      commands))
             ((string= method 'remove)
              (mapcar (lambda (command)
                        `(remove-hook ',command ',function))
                      commands)))))

(defun other-window-reverse ()
  "Select the previous window."
  (interactive)
  (select-window (previous-window)))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(require 'compile)

(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))

(require 'zp-timer-macs)

;;----------------------------------------------------------------------------
;; Editing commands
;;----------------------------------------------------------------------------
(defun zp/delete-frame-ask ()
  "Ask before deleting FRAME, permanently eliminating it from use.
See `delete-frame' for details."
  (interactive)
  (when (y-or-n-p "Do you want to close the current frame?")
    (call-interactively #'delete-frame)))

(defun zp/unfill-document ()
  "Fill individual paragraphs with large fill column."
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (point-min) (point-max))))

(defun zp/unfill-paragraph ()
  "Unfill current paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun zp/unfill-region ()
  "Unfill current region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun zp/unfill-dwim ()
  "Contextually unfill text.

If region is active, unfill it.  Otherwise, unfill the
surrounding paragraph."
  (interactive)
  (if (region-active-p)
      (zp/unfill-region)
    (zp/unfill-paragraph)))

(defun zp/kill-other-buffer-and-window ()
  "Kill the other buffer and window if there is more than one window."
  (interactive)
  (if (not (one-window-p))
      (progn
        (select-window (next-window))
        (kill-buffer-and-window))
    (user-error "There is only one window in the frame")))

;; Taken from magnars’s config
(defun toggle-window-split ()
  "Change the vertical split direction."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun zp/switch-to-help ()
  "Switch to the help buffer."
  (interactive)
  (switch-to-buffer "*Help*"))

;;----------------------------------------------------------------------------
;; Custom modes
;;----------------------------------------------------------------------------
(define-minor-mode print-circle-mode
  "Mode for toggling ‘print-circle’ globally."
  :lighter " crcl"
  :group 'misc
  :global t
  (if print-circle-mode
      (setq print-circle t)
    (setq print-circle nil)))

(define-minor-mode always-centred-mode
  "Mode for keeping the cursor vertically centred."
  :lighter " ctr"
  (let* ((settings '((scroll-preserve-screen-position nil t)
                     (scroll-conservatively 0 0)
                     (maximum-scroll-margin 0.25 0.5)
                     (scroll-margin 0 99999)))
         (toggle (lambda (mode)
                   (dolist (data settings)
                     (cl-destructuring-bind (setting default new) data
                       (set (make-local-variable setting)
                            (if (eq mode 'on)
                                new
                              default)))))))
    (if always-centred-mode
        (funcall toggle 'on)
      (funcall toggle 'off))))

(global-set-key (kbd "M-Y") #'always-centred-mode)

;;----------------------------------------------------------------------------
;; Keys
;;----------------------------------------------------------------------------
;; Define keymap for minor mode toggles
(define-prefix-command 'zp/toggle-map)
(define-key ctl-x-map "t" 'zp/toggle-map)

(define-key zp/toggle-map (kbd "d") #'toggle-debug-on-error)
(define-key zp/toggle-map (kbd "Q") #'toggle-debug-on-quit)
(define-key zp/toggle-map (kbd "q") #'electric-quote-local-mode)
(define-key zp/toggle-map (kbd "F") #'flyspell-mode)
(define-key zp/toggle-map (kbd "a") #'auto-fill-mode)
(define-key zp/toggle-map (kbd "l") #'display-line-numbers-mode)
(define-key zp/toggle-map (kbd "h") #'global-hl-line-mode)
(define-key zp/toggle-map (kbd "p") #'print-circle-mode)
(define-key zp/toggle-map (kbd "s") #'so-long-mode)
(define-key zp/toggle-map (kbd "S") #'scroll-bar-mode)

(define-key help-map (kbd "h") #'zp/switch-to-help)

;; Modes
(global-set-key (kbd "C-c H") #'global-hl-line-mode)
(global-set-key (kbd "M-U") #'visual-line-mode)

;; Exit Emacs with ‘C-x r q’, and kill the current frame with ‘C-x C-c’
(global-set-key (kbd "C-x r q") #'save-buffers-kill-terminal)
(global-set-key (kbd "C-x r c") #'delete-frame)
(global-set-key (kbd "C-x C-c") #'zp/delete-frame-ask)

;; Actions
(global-set-key (kbd "M-SPC") #'delete-horizontal-space)
(global-set-key (kbd "M-S-SPC") #'just-one-space)
(global-set-key (kbd "s-.") #'zp/echo-buffer-name)
(global-set-key (kbd "C-x F") #'zp/unfill-document)
(global-set-key (kbd "M-Q") #'zp/unfill-dwim)
(global-set-key (kbd "C-x B") #'rename-buffer)
(global-set-key (kbd "M-o") #'mode-line-other-buffer)
(global-set-key (kbd "s-j") #'other-window-reverse)
(global-set-key (kbd "s-k") #'other-window)
(global-set-key (kbd "s-K") #'toggle-window-split)
(global-set-key (kbd "C-x 4 1") #'zp/kill-other-buffer-and-window)

;; Ignore Kanji key in IME
(global-set-key [M-kanji] 'ignore)

;;----------------------------------------------------------------------------
;; Cosmetics
;;----------------------------------------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode 1)

;; Set fringe sizes
(fringe-mode 20)

;;----------------------------------------------------------------------------
;; Electric
;;----------------------------------------------------------------------------
(setq electric-quote-context-sensitive 1)

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
;; diff
;;----------------------------------------------------------------------------
;; Diff backend
(setq diff-command "diff")            ;Default

;; Add ‘-u’ switch for diff
(setq diff-switches "-u")

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

;;----------------------------------------------------------------------------
;; Setup package repositories
;;----------------------------------------------------------------------------
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
                          (org nil)))

;; org-elpa
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Avoid loading older byte-compiled version
(setq load-prefer-newer t)

;; Initialise packages
(package-initialize)

;; ‘use-package’ initialisation
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "~/.emacs.d/")
  (require 'use-package)
  (setq use-package-expand-minimally byte-compile-current-file))

(require 'bind-key)
(require 'diminish)

;; Libraries
(use-package dash)
(use-package s)

(use-package simple
  :bind (([remap just-one-space] . #'cycle-spacing)
         ([remap upcase-word] . #'upcase-dwim)
         ([remap downcase-word] . #'downcase-dwim)
         ([remap capitalize-word] . #'capitalize-dwim)))

(use-package crdt
  ;; :load-path "/home/zaeph/projects/crdt.el/"
  )

(use-package iso-transl)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package eldoc
  :config
  (setq eldoc-idle-delay 0.5))

(use-package secret)

(use-package slime)

(use-package slime-cl-indent
  :config
  ;; (setq lisp-indent-function #'lisp-indent-function)
                                        ;Default

  ;; Change indent style for CL
  (setq common-lisp-style "sbcl")

  ;; Way to override indent for some functions
  ;; (put 'use-package 'common-lisp-indent-function 1)
  )

(use-package package
  :bind ("C-c P" . package-list-packages))

(use-package ibuffer
  :init (global-set-key (kbd "C-x C-b")
                        (if (fboundp #'ibuffer-jump)
                            #'ibuffer-jump
                          #'ibuffer))
  :config
  ;; (global-set-key (kbd "C-x b") #'ibuffer)
  )

;; Start server
(use-package server
  :config
  ;; Start server if it hasn’t been started already
  (if (server-running-p)
      (setq initial-scratch-message
            (concat initial-scratch-message
                    ";; STANDALONE\n\n"))
    (server-start)))

;; (setq use-package-verbose t)

;;----------------------------------------------------------------------------
;; magit
;;----------------------------------------------------------------------------
(use-package magit
  ;; :load-path "~/projects/magit/lisp/"
  :bind (("s-m" . magit-status)
         ("s-b" . magit-blame-addition)
         ("C-c g" . magit-file-dispatch))
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-1" "First parent" "--first-parent"))
  (transient-append-suffix 'magit-log-refresh "-A"
    '("-1" "First parent" "--first-parent"))
  (transient-append-suffix 'magit-log "-f"
    '("-m" "Hide merges" "--no-merges"))
  (transient-append-suffix 'magit-log-refresh "-f"
    '("-m" "Hide merges" "--no-merges"))
  (setq magit-diff-refine-hunk 'all)
  (magit-wip-mode))

(use-package zp-magit
  :bind ("s-M-m" . zp/magit-stage-file-and-commit))

(use-package forge
  :after magit)

;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------
(use-package auth-source
  :custom
  ;; Path to authentication sources
  (auth-sources '("~/.authinfo.gpg" "~/.netrc"))
  (auth-source-save-behavior nil))

(use-package helpful)

(use-package gif-screencast
  :load-path ("~/src/gif-screencast/")
  :commands (gif-screencast)
  :load-path "~/projects/emacs-gif-screencast/"
  :bind (("<f8>" . gif-screencast-toggle-pause)
         ("<f9>" . gif-screencast-stop)))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-enable-undo-in-region nil))

(use-package evil
  :config
  (evil-mode 0))

(use-package rg
  :ensure t
  :commands rg
  :bind (("M-s ," . rg-dwim)
         ("s-SPC" . rg)))

;; For handling encryption
(use-package epa-file
  :config
  (epa-file-enable)
  (setq epg-gpg-program "gpg2"))

(use-package isearch
  :bind (:map isearch-mode-map
         ("<backspace>" . 'isearch-del-char)))

;; fcitx (IME for CJK)
;; Disabled because of slow-downs in combination with visual-line-mode
;; (fcitx-aggressive-setup)

(use-package consult
  ;; :ensure t
  :bind (([remap goto-line] . consult-goto-line)
         ;; ([remap yank-pop] . consult-yank-pop)
         ))

(use-package ox-hugo)

(use-package edit-indirect)

(use-package package-lint)

(use-package duplicate-thing
  :bind (("M-J" . zp/duplicate-thing))
  :config
  (defun zp/duplicate-thing (arg)
    "Wrapper for `duplicate-thing' which restores point."
    (interactive "P")
    (save-excursion
      (duplicate-thing arg))))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode))

(use-package beacon
  :demand
  :bind (:map zp/toggle-map
         ("b" . beacon-mode))
  :custom
  (beacon-push-mark nil)
  (beacon-color "#cc342b")
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode)
  (global-hl-line-mode 1))

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
    (if (bound-and-true-p whitespace-mode)
        (progn
          (whitespace-mode -1)
          (message "Whitespace mode disabled in current buffer"))
      (let ((whitespace-style '(face trailing lines-tail))
            (whitespace-line-column nil))
        (whitespace-mode t)
        (message "Whitespace mode enabled in current buffer")))))

(use-package interaction-log
  :bind (:map zp/toggle-map
         ("i" . interaction-log-mode)))

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

(use-package ox-org)

(use-package ox-texinfo+
  :load-path "~/projects/ox-texinfo-plus/")

(use-package org-mind-map)

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

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

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
         (rust-mode . flycheck-mode)
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
  (flycheck-pos-tip-mode))

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
    "Override `lisp-indent-function' to properly handle plists. See the original function fo full description"
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
  :hook (emacs-lisp-mode . lispy-mode)
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
  :bind ("M-O" . olivetti-mode)
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
  (global-set-key (kbd "s-<backspace>") 'yas-prev-field))

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
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(use-package avy
  :bind (;; ("s-n" . avy-goto-goto-word-1)
         ;; ("s-n" . avy-goto-goto-char)
         ("s-n" . avy-goto-char-timer)))

;; (use-package dumb-jump
;;   :config
;;   (dumb-jump-mode)
;;   (global-visible-mark-mode 1))

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
  :ensure t)

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode)))

(use-package git-link
  :bind (:map prog-mode-map
         ("C-c C-g" . zp/git-link-dwim)
         :map conf-mode-map
         ("C-c C-g" . zp/git-link-dwim))
  :config
  (defun zp/git-link-dwim (arg)
    "Create a URL pointing to current line/region on the branch.
With a C-u argument, point to the commit instead."
    (interactive "P")
    (let ((git-link-use-commit (if arg nil t))
          (current-prefix-arg nil))
      (call-interactively #'git-link))))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

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

  ;; Use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)

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

(define-minor-mode save-silently-mode
  "Save buffers silently when exiting."
  :lighter " SS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-c") 'zp/save-buffers-kill-terminal-silently)
            (define-key map (kbd "C-c C-k") 'zp/kanji-add-furigana)
            (define-key map (kbd "M-n") 'zp/kanji-add-furigana)
            map))

;; Way to enable minor modes based on filenames
;; Added with the package ‘auto-minor-mode-alist’
;; But they can also be added via file-fariables or minor-modes
;; TODO: Adapt this block
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . visual-line-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . flyspell-mode))
(add-to-list 'auto-minor-mode-alist '("edit-in-emacs.txt" . save-silently-mode))

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
  (setq recentf-max-menu-items 1000))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-connection-properties '((nil "session-timeout" nil))))

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
    (let ((inhibit-changing-match-data t))
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
    (let ((inhibit-changing-match-data t))
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
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-auto-character-face-perc 20))

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

  ;; Allow `’` to be part of a word
  ;; Otherwise, apostrophes typed with ‘electric-quote-mode’ are not
  ;; recognised as such
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
          ("french" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "fr_FR") nil utf-8)))))

(use-package zp-ispell
  :custom
  (ispell-dictionary "british")
  (zp/ispell-completion-data '(("en" . "british")
                               ("fr" . "french"))))

(use-package flyspell
  :bind ("C-c f" . flyspell-mode)
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
         (message-setup . electric-quote-local-mode)
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
  :bind (:map notmuch-search-mode-map
         ("g" . notmuch-refresh-this-buffer)
         :map notmuch-show-mode-map
         ("C-c C-o" . goto-address-at-point))
  :hook ((notmuch-hello-refresh . zp/color-all-inboxes)
         (notmuch-message-mode . electric-quote-local-mode))
  :config
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


  (define-key notmuch-hello-mode-map "q" #'zp/notmuch-hello-quit)
  (define-key notmuch-search-mode-map "g" #'notmuch-refresh-this-buffer)

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
    (zp/color-inbox-if-unread "dev-forum-inbox" "tag:forum"))

  ;;----------------------
  ;; Switching to notmuch
  ;;----------------------

  (defun zp/notmuch-hello-quit ()
    (interactive)
    (notmuch-bury-or-kill-this-buffer)
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
           (notmuch)))))

(use-package zp-notmuch
  :bind (("s-l" . zp/switch-to-notmuch)
         :map notmuch-hello-mode-map
         ("q" . zp/notmuch-hello-quit)
         :map notmuch-show-mode-map
         ("v" . zp/notmuch-view-html)
         :map notmuch-search-mode-map
         ("v" . zp/notmuch-search-view-html)
         :map notmuch-message-mode-map
         (("C-c C-c" . zp/notmuch-confirm-before-sending)
          ("C-c C-s" . zp/notmuch-confirm-before-sending)
          ("C-c C-b" . zp/message-goto-body)
          ("C-c C-." . zp/message-goto-body-end)
          ("M-<" . zp/message-goto-top)
          ("M->" . zp/message-goto-bottom)
          ("C-c C-z" . zp/message-kill-to-signature)))
  :custom
  (zp/notmuch-fcc-tags-default "-inbox +sent -unread")
  :config
  (setq notmuch-fcc-dirs (zp/notmuch-make-fcc-dirs)
        zp/message-ispell-alist (zp/notmuch-make-ispell-alist)
        zp/message-sigs-alist (zp/notmuch-make-sigs-alist)
        zp/notmuch-saved-queries
        '(("pro"        . "tag:pro and not tag:auto")
          ("etalab"     . "tag:etalab and not tag:auto")
          ("dev"        . "tag:dev and not tag:auto")
          ("beta"       . "(tag:list or tag:auto)")
          ("lists"      . "tag:list and not tag:auto")
          ("dev-github" . "tag:dev and tag:github and tag:auto")
          ("dev-forum"  . "tag:dev and tag:forum and tag:auto"))
        notmuch-saved-searches
        `((:name "inbox" :key "i" :query "tag:inbox and not (tag:auto or tag:list)")
          (:name "unread" :key "u" :query "tag:unread and not tag:auto")
          (:name "archive-week" :key "a" :query "date:\"7d..today\" and not tag:auto")
          (:name "archive" :key "A" :query "not tag:auto")

          ;; Pro
          ,@(zp/notmuch-format-search "pro" "p")

          ;; Etalab
          ,@(zp/notmuch-format-search "etalab" "e")

          ;; Dev
          ,@(zp/notmuch-format-search "dev" "d")

          ;; Beta
          ,@(zp/notmuch-format-search "beta" "b")

          ;; Lists
          ,@(zp/notmuch-format-search "lists" "l")

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
  :after message
  :hook (message-setup . orgalist-mode))

;; Disabled because not used
;; (use-package footnote
;;   :config
;;   (setq footnote-section-tag "Footnotes: "))

;;----------------------------------------------------------------------------
;; Programming modes
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

(use-package elpy
  :bind (:map elpy-mode-map
         ("C-M-n" . elpy-nav-forward-block)
         ("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode)
         ;; (pyenv-mode . elpy-rpc-restart)
         )
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

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

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java
  :ensure nil)

(use-package helm-lsp)
(use-package lsp-treemacs)

;;----------------------------------------------------------------------------
;; TypeScript
;;----------------------------------------------------------------------------

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :bind (:map tide-mode-map
         ("M-RET" . zp/typescript-eval-buffer))
  :config
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
(use-package tex-buf)

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
  (setq warning-suppress-types nil)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
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
  (setq org-latex-listings 'minted)

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
  (setq org-src-preserve-indentation t))

;;----------------------------------------------------------------------------
;; org-mode
;;----------------------------------------------------------------------------
(use-package calendar
  :config
  (setq diary-file "~/diary")

  (calendar-set-date-style 'iso)

  ;; Geo-location
  (setq calendar-week-start-day 1
        calendar-latitude 48.11198
        calendar-longitude -1.67429
        calendar-location-name "Rennes, France")

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
  :bind (:map org-mode-map
         ("C-c i" . org-indent-mode)
         ("C-c [" . nil)
         ("C-c ]" . nil)
         ("C-c C-q" . counsel-org-tag)
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
         ("M-*" . zp/org-toggle-fontifications)
         ("C-c C-x C-l" . zp/org-latex-preview-dwim)
         ("C-c R" . org-display-inline-images))
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . electric-quote-local-mode)
         (before-save . zp/org-set-last-modified)
         (org-todo-repeat . zp/org-comment-logbook-notes))
  :config
  (setq org-agenda-inhibit-startup nil
        org-log-into-drawer "LOGBOOK-NOTES"
        org-use-property-inheritance '("AGENDA_GROUP")
        org-startup-folded 'fold
        org-attach-preferred-new-method 'ask
        org-log-state-notes-insert-after-drawers nil
        ;; org-special-ctrl-a/e t
        org-special-ctrl-a/e t
        org-log-done 'time
        org-enforce-todo-dependencies nil
        org-adapt-indentation nil
        org-loop-over-headlines-in-active-region 'start-level

        org-clock-report-include-clocking-task t
        org-clock-out-remove-zero-time-clocks t

        org-hide-emphasis-markers nil
        org-ellipsis "…"
        org-track-ordered-property-with-tag "ORDERED"
        org-tags-exclude-from-inheritance nil
        org-catch-invisible-edits 'error

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
                                 (shell . t)))

  ;; Show images after executing a src-block that generated one
  ;; TODO: Limit the scope of the hook by testing if the block actually
  ;; generated an image
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append))

(use-package zp-org
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

(use-package org-footnote
  :config
  (setq org-footnote-define-inline 1))

(use-package org-clock
  :config
  (setq org-clock-into-drawer "LOGBOOK-CLOCK"
        org-clock-sound t)

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
      (error "Not currently clocking any task")))

  ;;------
  ;; Keys
  ;;------

  ;; Clocking commands
  (global-set-key (kbd "C-c C-x C-j") #'org-clock-goto)
  (global-set-key (kbd "C-c C-x C-i") #'org-clock-in)
  (global-set-key (kbd "C-c C-x C-o") #'org-clock-out)
  (global-set-key (kbd "C-c C-x C-z") #'org-resolve-clocks)

  (global-set-key (kbd "s-/") 'zp/echo-clock-string))

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
(use-package ob-async
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            (lambda ()
              (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"))))

(use-package ob
  :config
  ;; PlantUML
  (setq org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  ;; Load library required for PlantUML
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (add-to-list 'org-src-lang-modes '("ditaa" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t))))

;;------------
;; Projectile
;;------------
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/.emacs.d/"
                                    "~/.bin/"
                                    "~/.dotfiles/"
                                    "~/projects/"))
  (projectile-indexing-method 'hybrid)
  (projectile-switch-project-action (lambda () (dired ".")))
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package counsel-projectile
  :ensure t
  ;; :bind (("s-p" . counsel-projectile))
  :custom
  (counsel-projectile-switch-project-action
   '(1
     ("D" counsel-projectile-switch-project-action-dired "open project in dired")
     ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
     ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
     ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
     ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
     ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
     ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
     ("C" counsel-projectile-switch-project-action-configure "run project configure command")
     ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
     ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
     ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
     ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
     ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
     ("xv" counsel-projectile-switch-project-action-run-vterm "invoke vterm from project root")
     ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
     ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda")))
  :init
  (counsel-projectile-mode))

;;----------------------------------------------------------------------------
;; Helm
;;----------------------------------------------------------------------------
(define-prefix-command 'zp/helm-map)
(global-set-key (kbd "C-c h") 'zp/helm-map)
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("<menu>" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ;; ("M-y" . consult-yank-pop)
         ;; ("s-b" . helm-mini)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-s M-s" . helm-occur)
         ("C-x r b" . helm-bookmarks)
         ("C-h C-SPC" . helm-all-mark-rings)
         ("C-h a" . helm-apropos)
         :map helm-map
         ("C-S-o" . helm-previous-source)
         :map zp/helm-map
         (("o" . helm-occur)
          ("f" . helm-find-files)
          ("r" . helm-regexp)
          ("x" . helm-register)
          ("b" . helm-resume)
          ("c" . helm-colors)
          ("M-:" . helm-eval-expression-with-eldoc)
          ("i" . helm-semantic-or-imenu)
          ("/" . helm-find)
          ("<tab>" . helm-lisp-completion-at-point)
          ("p" . helm-projectile)))
  :config
  ;; Increase truncation of buffer names
  (setq helm-buffer-max-length 30       ;Default: 20
        helm-completion-style 'fuzzy)

  ;; Disable helm-mode for some functions
  ;; Used to be necessary, but now it works just fine
  ;; (add-to-list 'helm-completing-read-handlers-alist '(org-set-property)))
  )

(use-package helm-org-rifle)

;;----------------------------------------------------------------------------
;; Selectrum
;;----------------------------------------------------------------------------
(use-package selectrum
  :disabled
  :config
  (selectrum-mode +1))

;;----------------------------------------------------------------------------
;; Ivy
;;----------------------------------------------------------------------------
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-height 10
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")

  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)

  (use-package hydra
    :config
    (setq ivy-read-action-function 'ivy-hydra-read-action))

  ;; Commented because I use Helm for those commands
  ;; (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
  )

(use-package swiper
  :config
  ;; Commented because I now use counsel-grep-or-swiper
  ;; (global-set-key "\C-s" #'swiper)
  )

(use-package counsel
  :after swiper
  :bind (("C-s" . zp/counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper-backward)
         ;; Commented because I use the Helm equivalents
         ;; ("M-x" . counsel-M-x)
         ;; ("<menu>" . counsel-M-x)
         ;; ("C-x C-f" . counsel-find-file)
         ;; ("C-c j" . counsel-git-grep)
         ;; Commented because unused
         ;; ("C-c g" . counsel-git)
         ;; ("C-c k" . counsel-ag)
         ;; ("C-x l" . counsel-locate)
         ;; ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :requires swiper
  :config
  (setq counsel-find-file-at-point t)

  ;; Use rg insted of grep
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never %s %s")

  (defun zp/counsel-grep-or-swiper (&optional arg)
    "Call ‘swiper’ for small buffers and ‘counsel-grep’ for large ones.
Wrapper to always use swiper for gpg-encrypted files and
indirect-buffers."
    (interactive "P")
    (let* ((file (buffer-file-name))
           (ext (if file (file-name-extension file))))
      (if (or (equal arg '(4))                    ;Forcing?
              (not file)                          ;Indirect buffer?
              (string= ext "gpg"))                ;Encrypted buffer?
          (swiper)
        (counsel-grep-or-swiper)))))

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
   ("p S" "~/org/sports/swimming/swimming.org.gpg")
   ("p R" "~/org/sports/running/running.org.gpg")

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
          ("C-c C-q" . counsel-org-tag-agenda)
          (":" . counsel-org-tag-agenda)
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
          ("o" . zp/toggle-org-agenda-sort-by-rev-fifo)
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
        '(("k" ((zp/org-agenda-include-routine . nil)))
          ("K" ((zp/org-agenda-include-routine . nil)))
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

          ("d" "Deadlines"
           (,(zp/org-agenda-block-deadlines)))

          ("w" "Waiting list"
           (,(zp/org-agenda-block-tasks-waiting)))

          ("A" "Meditation records"
           ((agenda ""
                    ((org-agenda-files zp/org-agenda-files-awakening)
                     (org-agenda-log-mode))))
           ((org-agenda-skip-timestamp-if-done nil)))

          ("S" "Swimming records"
           ((agenda ""
                    ((org-agenda-files '("~/org/sports/swimming/swimming.org.gpg")))))
           ((org-agenda-skip-timestamp-if-done nil)))
          ))

  ;; Update ‘org-super-agenda-header-map’
  (use-package org-super-agenda
    :config
    (setq org-super-agenda-header-map org-agenda-mode-map)))

;;----------------------------------------------------------------------------
;; org-capture
;;----------------------------------------------------------------------------
(use-package org-capture
  :commands (zp/org-agenda-capture)
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
          ("sw" "Swimming workout" entry (file+weektree+prompt "~/org/sports/swimming/swimming.org.gpg")
           "* DONE Training%^{SWIM_DISTANCE}p%^{SWIM_DURATION}p\n%t%(print zp/swimming-workout-default)")

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

          ;; Org-protocol templates
          ("OPg" "Capture with guessed action" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %(zp/org-protocol-process \"%:link\" \"%:description\")%? :online:\n%(zp/org-protocol-insert-selection-dwim \"%i\")" :add-created t)

          ("OPc" "Capture with completion" entry (file+headline "~/org/life.org" "Inbox")
           "* TODO %(zp/org-protocol-process \"%:link\" \"%:description\" nil t)%? :online:\n%(zp/org-protocol-insert-selection-dwim \"%i\")" :add-created t)

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
  :load-path "~/projects/org-ref/"
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
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
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

(use-package company
  :bind (("M-/" . company-complete)
         (:map company-mode-map
          (([remap indent-for-tab-command] . #'company-indent-or-complete-common))))
  :custom
  (company-idle-delay . nil)
  :hook ((after-init . global-company-mode))
  :config
  (add-to-list 'company-backends 'company-capf))

(defvar orb-title-format "${author-or-editor-abbrev} (${date}).  ${title}."
  "Format of the title to use for `orb-templates'.")

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
  :after (:any org org-capture)
  :custom
  (zp/org-agenda-files-primary "~/org/life.org")
  :init
  ;; ‘hydra-org-refile’ needs to modify the keymaps of ‘org-mode’,
  ;; ‘org-agenda-mode’, and ‘org-capture-mode’, but since those packages are
  ;; loaded lazily, we can’t simply add new key-bindings to their keymaps
  ;; because they might have not been initialised.  Instead, we defer the
  ;; feature-related key-binding assignments until their corresponding feature
  ;; has been loaded.
  (use-package org
    :bind (:map org-mode-map
           ("C-c C-j" . zp/org-jump-dwim )
           ("C-c C-w" . zp/org-refile-dwim ))
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
  (setq zp/appt-notification-app "~/.bin/appt-notify")

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
  (zp/advise-commands
   add
   (org-schedule
    org-deadline
    org-time-stamp)
   after
   zp/org-set-appt-warntime-if-timestamp-advice))

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
  :init (doom-modeline-mode 1)
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
  (setq zp/time-of-day-sections '("05:00" "08:00" "16:00" "20:00" "00:00"))
  (zp/switch-theme-auto))

;;----------------------------------------------------------------------------
;; Interaction with terminal emulators
;;----------------------------------------------------------------------------
(defun zp/term-dwim (&optional arg)
  "Run terminator in the CWD.

Trim unnecessary TRAMP information from the path (e.g. /sudo:…),
and forward it to terminator. ARGUMENTS can be any argument
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
;; Custom
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
