;;; init-duplicate-thing.el --- Settings and helpers for dupplicate-thing.  -*- fill-column: 78; lexical-binding: t; -*-

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
(require 'duplicate-thing)

(defun zp/duplicate-thing (arg)
  "`duplicate-thing' with ARG and restore point."
  (interactive "P")
  (save-excursion
    (duplicate-thing arg)))

(provide 'init-duplicate-thing)
;;; init-duplicate-thing.el ends here
