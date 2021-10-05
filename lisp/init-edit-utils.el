;;; init-edit-utils.el --- Day-to-day editing helpers  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2013-2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
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

;;; Commentary:

;;; Code:

;;; Windows & Frames

(defun other-window-reverse ()
  "Select the previous window."
  (interactive)
  (select-window (previous-window)))

(defun zp/delete-frame-ask ()
  "Ask before deleting FRAME, permanently eliminating it from use.
See `delete-frame' for details."
  (interactive)
  (when (y-or-n-p "Do you want to close the current frame?")
    (call-interactively #'delete-frame)))

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

;;; Unfill

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

(provide 'init-edit-utils)
;;; init-edit-utils.el ends here
