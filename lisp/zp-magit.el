;;; zp-magit.el --- Expansion for Magit  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; Keywords: git, magit
;; Version: 0.1.0
;; Package-Requires: magit

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
;; Personal expansion for Magit.

;;; Code:
;;;; Library Requires
(require 'magit)

(defun zp/magit-stage-file-and-commit (&optional arg)
    "Stage the current file and commit the changes.

With a ‘C-u’ prefix ARG, amend the last commit instead."
    (interactive "p")
    (when (buffer-modified-p)
      (save-buffer))
    (magit-stage-file (magit-file-relative-name))
    (pcase arg
      (4 (magit-commit-amend))
      (_ (magit-commit-create))))

(provide 'zp-magit)
;;; zp-magit.el ends here
