;;; zp-org-journal.el --- Personal expansion for org-journal  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; Keywords: org, org-journal
;; Version: 0.1.0
;; Package-Requires: org, org-journal

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
;; This is a collection of variables and functions I have written to expand
;; org-journal’s behaviour.

;;; Code:
;;;; Library Requires
(require 'org)
(require 'org-journal)

;;----------------------------------------------------------------------------
;; ‘org-capture’ expansion
;;----------------------------------------------------------------------------
(defun org-journal-find-location ()
  (set-buffer
   (save-window-excursion
     (org-journal-new-entry t)
     (zp/org-find-olp (list "Inbox") t)
     (current-buffer))))

(defun zp/org-journal-find-today ()
  "Open today’s journal, and create it if it doesn’t exist."
  (interactive)
  (org-journal-new-entry t))

(defun zp/org-journal-capture (goto)
  (interactive "P")
  (let ((org-capture-templates
         '(("j" "Journal entry" entry #'org-journal-find-location
            "* %(format-time-string org-journal-time-format)%?"
            :add-created t))))
    (org-capture goto "j")))

(provide 'zp-org-journal)
;;; zp-org-journal.el ends here
