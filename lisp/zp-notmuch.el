;;; zp-notmuch.el --- Expansion for notmuch  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/zaeph/.emacs.d
;; Keywords: notmuch
;; Version: 0.1.0
;; Package-Requires: notmuch

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
;; Personal expansion for notmuch.

;;; Code:
;;;; Library Requires
(require 'notmuch-mua)
(require 'dash)

(defgroup zp/notmuch nil
  "Expansion for notmuch."
  :group 'notmuch
  :prefix "zp/notmuch-"
  :link '(url-link :tag "Github" "https://github.com/zaeph/.emacs.d"))

(defcustom zp/notmuch-identities nil
  "Alist of identities to emails."
  :group 'zp/notmuch)

(defun zp/notmuch-identities-get (id keyword)
  "Get KEYWORD from ID."
  (when id
    (-> (assoc id zp/notmuch-identities)
        (cdr)
        (plist-get keyword))))

(defun zp/notmuch-make-fcc-dirs ()
  "Populate `notmuch-fcc-dirs' with data from `zp/notmuch-identities'."
  (--map (-let (((_ &plist :email :fcc) it))
           (cons (regexp-quote email)
                 fcc))
         zp/notmuch-identities))

(defun zp/notmuch-mua-prompt-for-sender ()
  "Prompt for a sender from the user's configured identities.

Meant to replace `notmuch-mua-prompt-for-sender'."
  (when zp/notmuch-identities
    (let ((name (notmuch-user-name))
          (address (-> (ivy-read "Send mail from: " zp/notmuch-identities)
                       (assoc zp/notmuch-identities)
                       (cdr)
                       (plist-get :email))))
      (message-make-from name address))))

(provide 'zp-notmuch)
;;; zp-notmuch.el ends here
