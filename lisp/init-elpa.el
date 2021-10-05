;;; init-elpa.el --- Settings and helpers for package.el  -*- fill-column: 78; lexical-binding: t; -*-

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
(require 'package)
(require 'cl-lib)


;;; Archives

;;;; MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
)

;;;; org-elpa
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;;;; NonGNU ELPA
(add-to-list 'package-archives '("org" . "https://elpa.nongnu.org/nongnu/") t)

;;; Disable some packages
(setq package-load-list '(all
                          ;; (org nil)
                          ))

;; Fire package.el up
(package-initialize)

(provide 'init-elpa)
;;; init-elpa.el ends here
