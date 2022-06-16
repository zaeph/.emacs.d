;;; zp-lsp.el --- Custom configuration for LSP  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2022 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; Keywords: lsp
;; Version: 0.1.0
;; Package-Requires: lsp

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
;; Custom configuration for LSP.

;;; Code:
;;;; Library Requires
(require 'lsp)

(defvar zp/lsp-before-save-functions nil
  "Alist of modes and LSP functions to run with `before-save'.
See `zp/lsp-before-save' for more details.")

(defun zp/lsp-before-save ()
  "Run `zp/lsp-before-save-functions' in supported buffer.
Meant to be used with `before-save-hook'."
  (let ((modes (mapcar #'car zp/lsp-before-save-functions)))
    (when-let ((mode (apply #'derived-mode-p modes)))
      (let ((functions (alist-get mode zp/lsp-before-save-functions)))
        (mapc #'funcall functions)))))

(provide 'zp-lsp)
;;; zp-lsp.el ends here
