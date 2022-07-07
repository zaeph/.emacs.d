;;; org-capture-web.el --- Templates for handling URLs -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
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
;; This package works in tandem with the ‘org-capture-web’ Bash
;; script.

;;; Code:

;;----------------------------------------------------------------------------
;; Require other packages
;;----------------------------------------------------------------------------
(require 'org-capture)

;;------------------
;; Helper functions
;;------------------
(defun zp/convert-m-to-hm (min-str)
  (let* ((min (string-to-number min-str))
         (h (/ min 60))
         (m (% min 60)))
    (format "%1s:%02d" h m)))

;;----------------------------------------------------------------------------
;; Creating the templates
;;----------------------------------------------------------------------------
(defvar zp/org-capture-web-default-key "a"
  "Key to use for the dummy org-capture template.")

(defun zp/org-capture-with-dummy-template ()
  "Run org-capture with a dummy template"
  (org-capture nil zp/org-capture-web-default-key))

(defvar zp/org-capture-web-default-target nil
  "Where the org-capture-web items should be placed.

It can be any target specification accepted by
‘org-capture-mode’.  See ‘org-capture-templates’ for more
information.")

(defun zp/org-capture-web-create-template (target &rest template)
  (declare (indent defun))
  (let* ((key zp/org-capture-web-default-key) ;Key for the dummy template
         (target (or target
                     zp/org-capture-web-default-target)))
    `((,key nil entry ,target
            ,(concat (pop template))
            ,@(when template
                template)))))

;;----------------------------
;; Basic templates
;;----------------------------
(defun zp/org-capture-web (title url &optional action todo curios)
  "Capture the website based on the info provided by org-capture-web.sh.

ACTION is the action-verb to use for the task.  When nil, use
a note template instead.

TITLE and URL are those of the captured webpage.

When CURIOS is non-nil, add the :curios: tag to the task."
  (let* ((todo (or todo
                   "TODO"))
         (prefix (if action
                     (format "%s %s" todo action)
                   "Notes on"))
         waiting
         (template (pcase todo
                     ("WAIT" (progn (setq waiting t)
                                    "* %s [[%%?%s][%s]]
:LOGBOOK-NOTES:
- State \"WAIT\"       from              %%U
:END:
"))
                     (_ "* %s [[%%?%s][%s]]")))
         (org-capture-templates
          (zp/org-capture-web-create-template nil
            (format template prefix url title)
            :add-created t)))
    (zp/org-capture-with-dummy-template)
    (org-toggle-tag "online" 'on)
    (when waiting
      (org-toggle-tag "waiting" 'on))
    (when curios
      (org-toggle-tag "curios" 'on))
    (message (concat "Link added to template: " url))))

(defun zp/org-capture-web-notes (title url &optional curios)
  "Capture notes on the website based on the info provided by org-capture-web.sh.

TITLE and URL are those of the captured webpage.

When CURIOS is non-nil, add the :curios: tag to the task."
  (let* ((template "* Notes on [[%%?%s][%s]]")
         (target '(file+headline "~/org/life.org" "Notes"))
         (org-capture-templates
          (zp/org-capture-web-create-template target
            (format template url title)
            :add-created t)))
    (zp/org-capture-with-dummy-template)
    (org-toggle-tag "online" 'on)
    (when curios
      (org-toggle-tag "curios" 'on))
    (message (concat "Link added to template: " url))))

(defun zp/org-capture-web-kill (title url)
  "Make website the latest kill in the kill ring.

Based on the info provided by org-capture-web.sh.

TITLE and URL are those of the webpage."
  (interactive)
  (kill-new (format "[[%s][%s]]" url title))
  (message (concat "Link added to kill-ring: " url)))

;;----------------------------------------------------------------------------
;; Letterboxd
;;----------------------------------------------------------------------------
(defun zp/org-capture-web-letterboxd (title url director year duration)
  "Capture a film based on the info provided by org-capture-web.sh.

TITLE, DIRECTOR, YEAR and DURATION are related to the film.

URL is the url to the Letterboxd page of the film."
  (let* ((template "* %s%%?
:PROPERTIES:
:MEDIA_LINK: [[%s][Letterboxd]]
:MEDIA_DIRECTOR: %s
:MEDIA_YEAR: %s
:MEDIA_DURATION: %s
:END:")
         (duration-str (if (string= duration "")
                           "???"
                         (zp/convert-m-to-hm duration)))
         (org-capture-templates
          (zp/org-capture-web-create-template '(file+olp "~/org/life.org" "Film" "List")
            (format template title url director year duration-str)
            :add-created t
            :prepend t)))
    (zp/org-capture-with-dummy-template)))

;;----------------------------------------------------------------------------
;; Flat
;;----------------------------------------------------------------------------
(defun zp/org-capture-web-flat (title url)
  (let* ((template "* [[%%?%s][%s]]%%^{ADDRESS}p%%^{PRICE}p%%^{MEUBLÉ}p%%^{M²}p\
%%^{DISTANCE_FROM_UPN}p%%^{DISTANCE_FROM_LPO}p")
         (org-capture-templates
          (zp/org-capture-web-create-template nil
            (format template url title)
            :add-create t)))
    (zp/org-capture-with-dummy-template)))

(provide 'org-capture-web)
;;; org-capture-web.el ends here
