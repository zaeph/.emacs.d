;;; org-capture-web.el --- Templates for handling URLs  -*- lexical-binding: t; -*-
;;; Commentary:

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
;; Template-related functions
;;----------------------------

(defvar zp/org-capture-web-action nil
  "Action to be taken on the webpage captured by org-capture-web.sh.")
(defvar zp/org-capture-web-title nil
  "Title of the webpage captured by org-capture-web.sh.")
(defvar zp/org-capture-web-url nil
  "URL of the webpage captured by org-capture-web.sh.")

(defun zp/org-capture-web (action title url)
  "Capture the website based on the info provided by org-capture-web.sh.

TITLE and URL are those of the webpage.  TEMPLATE is the
subtemplate to use."
  (interactive)
  (setq zp/org-capture-web-action action)
  (setq zp/org-capture-web-title title)
  (setq zp/org-capture-web-url url)
  (org-capture nil (concat "Wa"))
  (message (concat "Link added to template: \n" url)))

(defvar zp/org-capture-web-letterboxd-title nil
  "Title of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-url nil
  "Letterboxd URL of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-director nil
  "Name of the director of the film captured by
  org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-year nil
  "Year of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-duration nil
  "Duration of the film captured by org-capture-web.sh.")
(defvar zp/org-capture-web-letterboxd-template nil
  "Default template for capturing films from Letterboxd with org-capture.web.sh.")

(defun zp/org-capture-web-letterboxd (title url director year duration)
  "Capture a film based on the info provided by org-capture-web.sh.

TITLE, DIRECTOR, YEAR and DURATION are related to the film.

URL is the url to the Letterboxd page of the film."
  (let ((duration-str (if (string= duration "")
                          "???"
                        (zp/convert-m-to-hm duration))))
    (setq zp/org-capture-web-letterboxd-title title)
    (setq zp/org-capture-web-letterboxd-url url)
    (setq zp/org-capture-web-letterboxd-director director)
    (setq zp/org-capture-web-letterboxd-year year)
    (setq zp/org-capture-web-letterboxd-duration duration-str)
    (org-capture nil "Wf")))

(setq zp/org-capture-web-letterboxd-template
      "* %(print zp/org-capture-web-letterboxd-title)%?
:PROPERTIES:
:MEDIA_LINK: [[%(print zp/org-capture-web-letterboxd-url)][Letterboxd]]
:MEDIA_DIRECTOR: %(print zp/org-capture-web-letterboxd-director)
:MEDIA_YEAR: %(print zp/org-capture-web-letterboxd-year)
:MEDIA_DURATION: %(print zp/org-capture-web-letterboxd-duration)
:END:")

(defun zp/org-capture-web-kill-new (title url)
  "Make website the latest kill in the kill ring.

Based on the info provided by org-capture-web.sh.

TITLE and URL are those of the webpage."
  (interactive)
  (kill-new (concat "[["
                    url
                    "]["
                    title
                    "]]"))
  (message (concat "Link added to kill-ring: \n" url)))

(provide 'org-capture-web)
;;; org-capture-web.el ends here
