;;; zp-hydra-shortcuts.el --- Hydra for accessing shortcuts -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
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
;; This is a hydra for handling shortcuts to frequently used files.

;;; Code:

(defun zp/hydra-shortcuts-make-head (key path &optional name)
  (let ((name (or name
                  (file-name-base path))))
    `(,key (find-file ,path) ,name)))

(defmacro zp/hydra-shortcuts-make-hydra (&rest shortcuts)
  `(defhydra zp/hydra-shortcuts (:color blue)
     ,@(mapcar (lambda (list)
                 (let ((key (pop list))
                       (path (pop list))
                       (name (pop list)))
                   (zp/hydra-shortcuts-make-head key path name)))
               shortcuts)))

(provide 'zp-hydra-shortcuts)
;;; zp-hydra-shortcuts.el ends here
