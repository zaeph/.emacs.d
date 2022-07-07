;;; zp-ispell.el --- Expansion for ispell  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; URL: https://github.com/zaeph/.emacs.d
;; Keywords: ispell
;; Version: 0.1.0
;; Package-Requires: ispell

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
;; Personal expansion for ispell.

;;; Code:
;;;; Library Requires
(require 'ispell)
(require 'dash)

;; Don't send ’ to the subprocess.
(defun endless/replace-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add 'ispell-send-string :filter-args
            #'endless/replace-apostrophe)

;; Convert ' back to ’ from the subprocess.
(defun endless/replace-quote (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add 'ispell-parse-output :filter-args
            #'endless/replace-quote)

;; Helm-Ispell
(defvar zp/ispell-completion-data
  '(("en" . "english")
    ("fr" . "french"))
  "Alist of languages to Ispell dictionaries.")

(defun zp/ispell--lang-to-dict (lang)
  "Get Ispell dictionary to use from LANG.
See `zp/ispell-completion-data' for details."
  (when lang
    (let ((data zp/ispell-completion-data))
      (-> (assoc lang data)
          (cdr)))))

(defun zp/ispell--dict-to-lang (dict)
  "Get language from Ispell DICT.
See `zp/ispell-completion-data' for details."
  (when dict
    (let ((data zp/ispell-completion-data))
      (-> (rassoc dict data)
          (car)))))

(defun zp/ispell--switch-dict (lang-or-dict)
  "Change the Ispell dictionary to LANG-OR-DICT.
LANG-OR-DICT can either be a language or an Ispell dictionary.
See `zp/ispell-completion-data' for details."
  (let* ((data zp/ispell-completion-data)
         (langs (mapcar #'car data))
         (dict (if (member lang-or-dict langs)
                   (zp/ispell--lang-to-dict lang-or-dict)
                 lang-or-dict))
         (inhibit-message t))
    (setq ispell-local-dictionary dict)
    ;; Disable `flyspell-mode' if it is already loaded
    ;; Required for fontification from previous dict
    (when flyspell-mode
      (flyspell-mode -1))
    (flyspell-mode)))

(defun zp/ispell-switch-dict (&optional lang)
  "Change the Ispell dictionary to LANG.
LANG should be the name of an Ispell dictionary.  See
`zp/ispell-completion-data' for details.
If LANG is not provided, query the user."
  (interactive)
  (let* ((data zp/ispell-completion-data)
         (dict-current (when flyspell-mode
                         (or ispell-local-dictionary
                             ispell-dictionary)))
         (lang-current (zp/ispell--dict-to-lang dict-current))
         (lang-preselect (pcase lang-current
                           ("en" "fr")
                           (_ "en")))
         (lang (pcase lang
                 ((or "ask" 'nil)
                  (ivy-read "Choose language: " data
                            :preselect
                    lang-preselect))
                 (_ lang)))
         (dict (zp/ispell--lang-to-dict lang)))
    (when (eq dict dict-current)
      (user-error "Dictionary is already loaded"))
    (zp/ispell--switch-dict dict)
    (message "Changed dictionary to %s" (capitalize dict))))

(defun zp/ispell-query-dict ()
  (interactive)
  (unless (y-or-n-p "Writing in English? ")
    (zp/ispell-switch-dict "fr")))

(provide 'zp-ispell)
;;; zp-ispell.el ends here
