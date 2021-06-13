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
(require 'notmuch)
(require 'notmuch-mua)
(require 'dash)

(defgroup zp/notmuch nil
  "Expansion for notmuch."
  :group 'notmuch
  :prefix "zp/notmuch-"
  :link '(url-link :tag "Github" "https://github.com/zaeph/.emacs.d"))

(defcustom zp/notmuch-identities nil
  "Plist of identities based on `notmuch-identities'."
  :group 'zp/notmuch)

(defcustom zp/notmuch-identities-defaults nil
  "Default values for keywords in `zp/notmuch-identities'.
Used as fallback when the keyword is not defined for a given
identity."
  :group 'zp/notmuch)

(defun zp/notmuch-identities-get (id keyword)
  "Get KEYWORD from ID in `zp/notmuch-identities'.
If the value is nil, fallback to KEYWORD’s value in
`zp/notmuch-identities-defaults'."
  (when id
    (or (-> (assoc id zp/notmuch-identities)
            (cdr)
            (plist-get keyword))
        (plist-get zp/notmuch-identities-defaults keyword))))

(defcustom zp/notmuch-fcc-tags-default nil
  "Default tags to apply to outgoing emails when `:dir' is set.
See `zp/notmuch-identities' for details."
  :type 'string
  :group 'zp/notmuch)

(defun zp/notmuch-make-fcc-dirs ()
  "Populate `notmuch-fcc-dirs' with data from `zp/notmuch-identities'."
  (--map (-let (((id &plist :email :fcc) it))
           (cons (regexp-quote email)
                 (or fcc
                     (--if-let (zp/notmuch-identities-get id :dir)
                         (concat it "/sent " zp/notmuch-fcc-tags-default)))))
         zp/notmuch-identities))

(defun zp/notmuch-make-ispell-alist ()
  "Populate `zp/message-ispell-alist' with data from `zp/notmuch-identities'."
  (--map (-let (((id &plist :email) it))
           (cons email
                 (zp/notmuch-identities-get id :lang)))
         zp/notmuch-identities))

(defun zp/notmuch-make-sigs-alist ()
  "Populate `zp/message-sigs-alist' with data from `zp/notmuch-identities'."
  (--map (-let (((id &plist :email :sig) it))
           (cons email
                 (zp/notmuch-identities-get id :sig)))
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

(defun zp/notmuch-confirm-before-sending (&optional arg)
    (interactive "P")
    (if (y-or-n-p "Ready to send? ")
        (notmuch-mua-send-and-exit arg)))

;;; Queries
(defcustom zp/notmuch-saved-queries nil
  "Alist of symbols to queries as strings."
  :type 'alist
  :group 'zp/notmuch)

(defun zp/notmuch-get-query (query)
  "Get string-query from QUERY.
See `zp/notmuch-saved-queries' for details."
  (cdr (assoc query zp/notmuch-saved-queries)))

(defun zp/notmuch-format-search (name key)
  "Format a saved-search from NAME and KEY."
  `((:name ,(concat name "-inbox") :key ,key
     :query ,(concat (zp/notmuch-get-query name)
                     " and tag:inbox"))
    (:name ,(concat name "-archive-week") :key ,(concat (upcase key) "a")
     :query ,(concat (zp/notmuch-get-query name)
                     " and date:\"7d..today\""))
    (:name ,(concat name "-archive") :key ,(concat (upcase key) "A")
     :query ,(concat (zp/notmuch-get-query name)))))

(defun zp/notmuch-view-html ()
  "View the HTML part of the current email in an external viewer."
  (interactive)
  (let ((match (save-excursion
                 (goto-char 0)
                 (re-search-forward "\\[ text/html .*\\]" nil t))))
    (unless match
      (error "No text/html part"))
    (save-excursion
      (goto-char match)
      (notmuch-show-view-part))))

(provide 'zp-notmuch)
;;; zp-notmuch.el ends here
