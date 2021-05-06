;;; zp-lispy.el --- Customisation for Lispy  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; Keywords: lispy
;; Version: 0.1.0
;; Package-Requires: lispy

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

;;; Commentary:
;;
;; Personal customisation for Lispy.

;;; Code:
;;;; Library Requires
(require 'lispy)

(defgroup zp/lispy nil
  "Extra customisation for `lispy'."
  :group 'bindings
  :prefix "zp/lispy-")

(defvar-local zp/lispy-spawn-parent-plist nil
  "Properties from the parent of the lispy-spawn indirect buffer.
The parent of a spawned indirect buffer is the buffer where the
command that spawned the buffer was run.  This is not the same as
the base buffer of an indirect buffer.")

(defvar-local zp/lispy-spawn-children nil
  "List of dependent lispy-spawn indirect buffers.")

(defcustom zp/lispy-spawn-auto-cleanup t
  "When non-nil, automatically cleanup visited buffers.
When looking up symbols with `zp/lispy-goto-symbol-ibuf', it is
sometimes necessary to create buffers.  For instance, when the
definition of a symbol is found in a file that was not already
opened, a base-buffer needs to be created to be the base of the
lispy-spawn indirect buffer.  However, once the latter is killed,
there is little use to keep the base-buffer around, since we only
visited it for `zp/lispy-goto-symbol-ibuf'.

Setting this variable to non-nil enables the tracking and the
killing of those visited base-buffers.  The code tries to be
clever about the definition of a visited buffer, and it ensures
that a base-buffer is not prematurely killed when multiple
lispy-spawn indirect buffers are based on it."
  :type 'boolean
  :group 'zp/lispy)

(defun zp/lispy-spawn--kill (&optional spawn base)
  "Kill the current lispy-spawn indirect buffer.

SPAWN is the lispy-spawn indirect buffer.  If it is not provided,
the current buffer is used.

BASE is the base-buffer of SPAWN.  If it is not provided, it is
read from `zp/lispy-spawn-parent-plist'."
  (let ((spawn (or spawn
                   (current-buffer)))
        (base (or base
                  (plist-get zp/lispy-spawn-parent-plist :created))))
    (kill-buffer spawn)
    ;; Handle base-buffer
    (when base
      (with-current-buffer base
        (setq-local zp/lispy-spawn-children
                    (delq spawn zp/lispy-spawn-children))
        (unless (or (not zp/lispy-spawn-auto-cleanup)
                    zp/lispy-spawn-children)
          (kill-buffer base))))))

(defun zp/lispy-spawn-kill (arg)
  "Kill the current lispy-spawn indirect buffer.
With a `\\[universal-argument]' ARG, restore the previous window
configuration."
  (interactive "P")
  (pcase-let (((map (:win parent-win)
                    (:buf parent-buf)
                    (:pos parent-pos)
                    (:win-conf parent-win-conf)
                    (:win-pos parent-win-pos)
                    (:close close)
                    (:base base))
               zp/lispy-spawn-parent-plist)
              (spawn-win (selected-window))
              (spawn-buf (current-buffer)))
    ;; Resolve windows and focus
    (cond
     ;; With universal-argument, restore previous win-conf
     ((equal arg '(4))
      (set-window-configuration parent-win-conf)
      (goto-char parent-pos)
      (set-window-start (selected-window) parent-win-pos))
     ;; Focus parent if window+buffer is live
     ((and (window-live-p parent-win)
           (eq (window-buffer parent-win) parent-buf))
      (select-window parent-win)))
    (when (and close
               (not (one-window-p)))
      (delete-window spawn-win))
    ;; Defer killing to internal function
    (zp/lispy-spawn--kill spawn-buf base)))

(defun zp/lispy-spawn-visit ()
  "Visit the base-buffer of the current lispy-spawn indirect buffer."
  (interactive)
  (let ((spawn (current-buffer))
        (pos-win-start (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (window-start))
                           ;; Subtract the header-line
                           (forward-line -1)
                           (point))))
        (pos (point)))
    (switch-to-buffer (buffer-base-buffer spawn))
    (setq-local zp/lispy-spawn-children nil)
    (goto-char pos-win-start)
    (let ((recenter-positions '(top)))
      (recenter-top-bottom))
    (goto-char pos)
    (kill-buffer spawn)))

(defun zp/lispy-goto-symbol-ibuf (symbol)
  "Go to definition of SYMBOL in a lispy-spawn indirect buffer.
SYMBOL is a string."
  (interactive (list (or (thing-at-point 'symbol t)
                         (lispy--current-function))))
  (let ((buffers (buffer-list))
        (cur-win (selected-window))
        (cur-buf (current-buffer))
        (cur-pos (point))
        (cur-win-conf (current-window-configuration))
        (cur-win-pos (window-start))
        (close (one-window-p))
        base-pos
        base-buf
        spawn-buf)
    (save-window-excursion
      (save-excursion
        (save-restriction
          (widen)
          (lispy-goto-symbol symbol)
          (save-excursion
            (save-restriction
              ;; Repeat `lispy-goto-symbol' if the base-buf is narrowed
              (when (buffer-narrowed-p)
                (widen)
                (lispy-goto-symbol symbol))
              (setq base-pos (point))
              (setq base-buf (current-buffer))
              (setq spawn-buf
                    (make-indirect-buffer
                     (current-buffer)
                     (generate-new-buffer-name
                      (format "%s / %s" (buffer-name) symbol))
                     t))
              (when (or zp/lispy-spawn-children
                        (not (member base-buf buffers)))
                ;; Bury buffer if it was created
                (bury-buffer base-buf)
                ;; Relate base-buffer and spawn-buffer
                (push spawn-buf zp/lispy-spawn-children)))))))
    (set-window-start (selected-window) cur-win-pos)
    (with-current-buffer spawn-buf
      (goto-char base-pos)
      (narrow-to-defun)
      (zp/lispy-spawn-mode t)
      ;; Store properties from parent in local var
      (setq-local zp/lispy-spawn-parent-plist
                  (list :win cur-win
                        :buf cur-buf
                        :pos cur-pos
                        :win-conf cur-win-conf
                        :win-pos cur-win-pos
                        :close close
                        :base base-buf)))
    (pop-to-buffer spawn-buf)))

(defvar zp/lispy-spawn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'zp/lispy-spawn-kill)
    (define-key map (kbd "C-c C-d") #'zp/lispy-spawn-visit)
    map)
  "Keymap for ‘zp/org-spawned-ibuf-mode’.")

(define-minor-mode zp/lispy-spawn-mode
  "Minor mode for special key-bindings in a lispy-spawn indirect buffer."
  :lighter " Spawn"
  :keymap zp/lispy-spawn-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<zp/lispy-spawn-mode-map>Spawned indirect buffer.  \
Kill `\\[zp/lispy-spawn-kill]', \ + restore \
`\\[universal-argument], \\[zp/lispy-spawn-kill]', visit \
`\\[zp/lispy-spawn-visit]'.")))

(provide 'zp-lispy)
;;; zp-lispy.el ends here
