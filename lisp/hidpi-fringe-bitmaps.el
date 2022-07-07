;;; hidpi-fringe-bitmaps.el --- High resolution fringe bitmaps -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <zaeph@zaeph.net>

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

;;; Commentary:
;;
;; This is a library of bitmaps to be used in the fringes for HiDPI monitors.

;;; Code:

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

(provide 'hidpi-fringe-bitmaps)
;;; hidpi-fringe-bitmaps.el ends here
