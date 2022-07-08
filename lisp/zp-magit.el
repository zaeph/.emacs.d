;;; zp-magit.el --- Expansion for Magit  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020-2022 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
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
(require 'git-rebase)

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

;; Source: https://gist.github.com/danielmartin/34bc36dafd8f900de483394087230f48
(defun zp/change-commit-author (arg)
  "Change the commit author during an interactive rebase in Magit.
With a prefix argument, insert a new change commit author command
even when there is already another rebase command on the current
line.  With empty input, remove the change commit author action
on the current line, if any."
  (interactive "P")
  (let ((author
         (magit-transient-read-person "Select a new author for this commit"
                               nil
                               nil)))
    (git-rebase-set-noncommit-action
     "exec"
     (lambda (_) (if author
                     (format "git commit --amend --author='%s'" author)
                   ""))
     arg)))

(define-key git-rebase-mode-map (kbd "h") #'zp/change-commit-author)

(provide 'zp-magit)
;;; zp-magit.el ends here
