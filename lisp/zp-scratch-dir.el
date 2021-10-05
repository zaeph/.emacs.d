;;; zp-scratch-dir.el --- Facilities for creating scratch directories.  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020-2021 Wojciech Siewierski
;; Copyright © 2020-2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Wojciech Siewierski, Leo Vivier <zaeph@zaeph.net>
;; Keywords: scratch
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
;; This package provides facilities for creating scratch directories.  This
;; was mostly created to prevent users from working in their *scratch* buffers
;; and losing their code.

;;; Code:
;;;; Library Requires
(require 'vc-git)

(defun scratch-dir-path (name)
  "Format a new scratch dir-path based on NAME and timestamp."
  (concat "~/scratch.d/scratch-"
          (format-time-string "%Y-%m-%d_%s")
          (when (not (string= name ""))
            (concat "--" name))
          "/"))

(defun scratch-dir (&optional use-git name)
  "Create an ad-hoc working directory and open it in dired.

When USE-GIT is non-nil, init git in the created directory with
the name NAME.

Prefix argument initializes the Git repository."
  (interactive "P\nMName: ")
  (let ((directory (expand-file-name (scratch-dir-path name))))
    (make-directory directory t)
    (when (file-symlink-p "~/scratch")
      (delete-file "~/scratch"))
    (make-symbolic-link directory "~/scratch" t)
    (when (car use-git)
      (let ((default-directory directory))
        (vc-git-create-repo)))
    (find-file directory)))

(provide 'zp-scratch-dir)
;;; zp-scratch-dir.el ends here
