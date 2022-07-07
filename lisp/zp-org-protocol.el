;;; zp-org-protocol.el --- Custom configuration for Org-protocol  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; URL: https://github.com/zaeph/.emacs.d
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
(defgroup zp/org-protocol nil
  "Personal expansion to Org-protocol."
  :group 'org
  :prefix "zp/org-protocol-")

(defcustom zp/org-protocol-verbs (list "Read")
  "List of action verbs to use when capturing with `org-protocol'.

The first element will be considered the default."
  :group 'zp/org-protocol
  :type 'list)

(defun zp/org-protocol-get-verb (url &optional with-completion)
  "Get verb according to URL.

The function tries to guess which verb to use based on the URL.

When WITH-COMPLETION is non-nil, use `zp/org-protocol-verbs' as
a completion-list and preselect the guess."
  (let ((guess (pcase url
                 ((pred (string-match "youtube\\.com/watch"))
                  "Watch")
                 (_ (if zp/org-protocol-verbs
                        (car zp/org-protocol-verbs)
                      (user-error "No action verb has been defined in `zp/org-protocol-verbs'"))))))
    (if with-completion
        (ivy-read "Verb: " zp/org-protocol-verbs :preselect guess)
      guess)))

(defun zp/org-protocol-process-title-by-url (title url)
  "Clean up TITLE based on URL rules."
  (pcase url
    ((pred (string-match "youtube\\.com"))
     (replace-regexp-in-string "\\(^([0-9]+) \\| - YouTube\\)" "" title))
    (_
     title)))

(defun zp/org-protocol-process (url desc &optional verb with-completion)
  "Get verb from URL and process DESC.

When VERB is non-nil, use it instead of processing it via
`zp/org-protocol-get-verb'.

When WITH-COMPLETION is non-nil, use `zp/org-protocol-verbs' as
a completion-list and preselect the guess."
  (let ((verb (or verb
                  (zp/org-protocol-get-verb url with-completion)))
        (desc-new (zp/org-protocol-process-title-by-url desc url)))
    (format "%s [[%s][%s]]" verb url desc-new)))

(defun zp/org-protocol-insert-selection-dwim (selection)
  "Insert SELECTION as an org blockquote."
  (unless (string= selection "")
    (format "#+begin_quote\n%s\n#+end_quote" selection)))

(provide 'zp-org-protocol)
;;; zp-org-protocol.el ends here
