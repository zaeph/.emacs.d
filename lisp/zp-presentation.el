;;; zp-presentation.el --- Basic presentation mode  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2022 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; Keywords: presentation
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
;;
;; Basic presentation mode.

;;; Code:
;;;; Library Requires
(require 'doom-modeline)

(defvar zp/presentation-height 200
  "Height to use for default face when in `zp/presentation-mode'.")

(defvar zp/presentation-height-default nil
  "Height of default face before entering `zp/presentation-mode'.")

(defvar zp/presentation-mode-line-height-default nil
  "Height of default mode-line before entering `zp/presentation-mode'.")

(define-minor-mode zp/presentation-mode
  "Save buffers silently when exiting."
  :group 'zp/presentation
  :lighter " Pres"
  :global t
  (cond (zp/presentation-mode
         (setq zp/presentation-height-default (face-attribute 'default :height)
               zp/presentation-mode-line-height-default doom-modeline-height)
         (set-face-attribute 'default nil :height zp/presentation-height))
        (t
         (set-face-attribute 'default nil :height zp/presentation-height-default)))
  (doom-modeline-refresh-bars))


(provide 'zp-presentation)
;;; zp-presentation.el ends here
