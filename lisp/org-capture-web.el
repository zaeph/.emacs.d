;;; org-capture-web.el --- Templates for handling URLs  -*- lexical-binding: t; -*-
;;; Commentary:

;; This package works in tandem with the ‘org-capture-web’ Bash
;; script.

;;; Code:

;;------------------
;; Helper functions
;;------------------
(defun zp/convert-m-to-hm (min-str)
  (let* ((min (string-to-number min-str))
         (h (/ min 60))
         (m (% min 60)))
    (format "%1s:%02d" h m)))

;;----------------------------
;; Functions for basic templates
;;----------------------------
(defvar zp/org-capture-web-action nil
  "Action to be taken on the webpage captured by org-capture-web.sh.")
(defvar zp/org-capture-web-title nil
  "Title of the webpage captured by org-capture-web.sh.")
(defvar zp/org-capture-web-url nil
  "URL of the webpage captured by org-capture-web.sh.")

(defun zp/org-capture-web (action title url &optional template)
  "Capture the website based on the info provided by org-capture-web.sh.

ACTION is the action-verb to use for the task.

TITLE and URL are those of the captured webpage.

TEMPLATE can be another org-capture template to use than the
default one."
  (interactive)
  (setq zp/org-capture-web-action action)
  (setq zp/org-capture-web-title title)
  (setq zp/org-capture-web-url url)
  (org-capture nil (or template
                       "Wa"))
  (message (concat "Link added to template: \n" url)))

(defun zp/org-capture-web-kill-new (title url)
  "Make website the latest kill in the kill ring.

Based on the info provided by org-capture-web.sh.

TITLE and URL are those of the webpage."
  (interactive)
  (kill-new (concat "[[" url "][" title "]]"))
  (message (concat "Link added to kill-ring: \n" url)))

;;----------------------------------------------------------------------------
;; Letterboxd
;;----------------------------------------------------------------------------
(defun zp/org-capture-web-letterboxd (title url director year duration)
  "Capture a film based on the info provided by org-capture-web.sh.

TITLE, DIRECTOR, YEAR and DURATION are related to the film.

URL is the url to the Letterboxd page of the film."
  (let ((template "Wf")
        (duration-str (if (string= duration "")
                          "???"
                        (zp/convert-m-to-hm duration))))
    (org-capture nil template)))

(defvar zp/org-capture-web-letterboxd-template nil
  "Default template for capturing films from Letterboxd with org-capture-web.sh.")

(setq zp/org-capture-web-letterboxd-template
      "* %(print title)%?
:PROPERTIES:
:MEDIA_LINK: [[%(print url)][Letterboxd]]
:MEDIA_DIRECTOR: %(print director)
:MEDIA_YEAR: %(print year)
:MEDIA_DURATION: %(print duration-str)
:END:")

;;----------------------------------------------------------------------------
;; Flat
;;----------------------------------------------------------------------------
(defun zp/org-capture-web-flat (title url))

(provide 'org-capture-web)
;;; org-capture-web.el ends here
