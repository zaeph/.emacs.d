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

(defvar-local zp/lispy-spawn-parent-plist nil
  "Properties from the parent of the lispy-spawn indirect buffer.
The parent of a spawned indirect buffer is the buffer where the
command that spawned the buffer was run.  This is not the same as
the base buffer of an indirect buffer.")

(defun zp/lispy-spawn-kill ()
  "Kill the current lispy-spawn indirect buffer."
  (interactive)
  (pcase-let (((map (:win parent-win)
                    (:buf parent-buf)
                    (:close close)
                    (:created created))
               zp/lispy-spawn-parent-plist)
              (spawn-win (selected-window))
              (spawn-buf (current-buffer)))
    (when (and (window-live-p parent-win)
               (eq (window-buffer parent-win) parent-buf))
      (select-window parent-win))
    (when close (delete-window spawn-win))
    (when created (kill-buffer created))
    (kill-buffer spawn-buf)))

(defun zp/lispy-spawn-visit ()
  "Visit the base buffer of the current lispy-spawn indirect buffer."
  (interactive)
  (let ((spawn (current-buffer))
        (pos-win-start (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (window-start))
                           ;; Subtracting the header-line
                           (forward-line -1)
                           (point))))
        (pos (point)))
    (switch-to-buffer (buffer-base-buffer spawn))
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
        (cur-filename (buffer-file-name))
        (cur-window (selected-window))
        (cur-buffer (current-buffer))
        (pos-win-start (window-start))
        (close (one-window-p))
        new-pos
        new-filename
        new-buffer
        new-buffer-created
        new-buffer-indirect)
    (save-excursion
      (save-restriction
        (widen)
        (lispy-goto-symbol symbol)
        (setq new-pos (point))
        (setq new-filename (buffer-file-name))
        (setq new-buffer (current-buffer))
        ;; Store whether `lispy-goto-symbol' created a new buffer
        (unless (member new-buffer buffers)
          (setq new-buffer-created new-buffer))
        (setq new-buffer-indirect
              (make-indirect-buffer (current-buffer)
                                    (generate-new-buffer-name
                                     (format "%s / %s" (buffer-name) symbol))
                                    t))))
    (set-window-start (selected-window) pos-win-start)
    (unless (eq cur-filename new-filename)
      ;; (message "Jumping to another buffer")
      (with-current-buffer new-buffer
        (bury-buffer)))
    (with-current-buffer new-buffer-indirect
      (goto-char new-pos)
      (narrow-to-defun)
      (zp/lispy-spawn-mode t)
      ;; Store parent info in local var
      (setq-local zp/lispy-spawn-parent-plist
                  (list
                   :win cur-window
                   :buf cur-buffer
                   :close close
                   :created new-buffer-created)))
    (pop-to-buffer new-buffer-indirect)))

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
Kill `\\[zp/lispy-spawn-kill]', visit \
`\\[zp/lispy-spawn-visit]'.")))

(provide 'zp-lispy)
;;; zp-lispy.el ends here
