;;; zp-always-centred.el --- Mode for keeping the cursor vertically centred.  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; Keywords: centring
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
;;

;;; Code:
(define-minor-mode zp/always-centred-mode
  "Mode for keeping the cursor vertically centred."
  :lighter " ctr"
  (let* ((settings '((scroll-preserve-screen-position nil t)
                     (scroll-conservatively 0 0)
                     (maximum-scroll-margin 0.25 0.5)
                     (scroll-margin 0 99999)))
         (toggle (lambda (mode)
                   (dolist (data settings)
                     (cl-destructuring-bind (setting default new) data
                       (set (make-local-variable setting)
                            (if (eq mode 'on)
                                new
                              default)))))))
    (if zp/always-centred-mode
        (funcall toggle 'on)
      (funcall toggle 'off))))

(provide 'zp-always-centred)
;;; zp-always-centred.el ends here
