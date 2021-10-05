;;; init-undo-tree.el --- Settings and helpers for undo-tree.  -*- fill-column: 78; lexical-binding: t; -*-

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
;;;; Library Requires
(require 'undo-tree)

;; Disable undo-tree for regions since it often corrupts the history
(setq undo-tree-enable-undo-in-region nil)

;; Fire undo-tree's global mode
(global-undo-tree-mode)

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
