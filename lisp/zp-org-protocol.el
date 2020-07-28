;;; zp-org-protocol.el --- Custom configuration for Org-protocol  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; Keywords: org, org-protocol
;; Version: 0.1.0
;; Package-Requires: org, org-protocol

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
;; This is my custom configuration for Org-protocol, along with some helper
;; functions.

;;; Code:
(defvar zp/org-protocol-verbs
  (list "Explore"
        "Investigate"
        "Read"
        "Listen"
        "Watch")
  "List of action verbs to use when capturing with `org-protocol'.

The first element will be considered the default.")

(defun zp/org-protocol-get-verb (link &optional with-completion)
  "Get verb according to LINK."
  (let ((guess (pcase link
                 ((pred (string-match "youtube\\.com/watch"))
                  "Watch")
                 (_ (car zp/org-protocol-verbs)))))
    (if with-completion
        (ivy-read "Action: " zp/org-protocol-verbs :preselect guess)
      guess)))

(defun zp/org-protocol-insert-selection-dwim (selection)
  "Insert SELECTION as quote if it is not non-nil."
  (unless (string= selection "")
    (format "#+begin_quote\n%s\n#+end_quote" selection)))

(provide 'zp-org-protocol)
;;; zp-org-protocol.el ends here
