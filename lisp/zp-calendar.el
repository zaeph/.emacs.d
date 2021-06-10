;;; zp-calendar.el --- Custom code for calendar-mode  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; Keywords: calendar
;; Version: 0.1.0
;; Package-Requires: calendar

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
;; Custom extension for calendar-mode.

;;; Code:
;;;; Library Requires
(require 'calendar)

;; Taken from https://www.emacswiki.org/emacs/CalendarWeekNumbers

(defface calendar-iso-week-face
  '((t :inherit (default)))
  "Face for week numbers."
  :group 'calendar-faces)

(defface calendar-iso-week-header-face
  '((t :inherit (default)))
  "Face for header of week numbers."
  :group 'calendar-faces)

(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'calendar-iso-week-header-face))

(provide 'zp-calendar)
;;; zp-calendar.el ends here
