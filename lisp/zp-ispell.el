;;; zp-ispell.el --- Expansion for ispell  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
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
(defvar zp/ispell-completion-data nil)
(setq ispell-dictionary "british"
      zp/ispell-completion-data '(("English" . "british")
                                  ("French" . "french")))

(defun zp/ispell-switch-dictionary (language)
  "Change the Ispell dictionary to LANGUAGE.

LANGUAGE should be the name of an Ispell dictionary."
  (interactive)
  (let ((name (car (rassoc language zp/ispell-completion-data))))
    (if (eq language ispell-local-dictionary)
        (message "Dictionary is already loaded for this language")
      (setq ispell-local-dictionary language)
      (flyspell-mode)
      (message (concat "Local Ispell dictionary set to " name)))
    (when flyspell-mode
      (flyspell-mode -1)
      (flyspell-mode))))

(defun zp/ispell-query-dictionary ()
  (if (not (y-or-n-p "Writing in English? "))
      (ispell-change-dictionary "french")))

(defvar zp/helm-ispell-actions nil)
(setq zp/helm-ispell-actions
      '(("Change dictionary" . zp/ispell-switch-dictionary)))

(defvar zp/helm-source-ispell nil)
(setq zp/helm-source-ispell
      '((name . "*HELM Ispell - Dictionary selection*")
        (candidates . zp/ispell-completion-data)
        (action . zp/helm-ispell-actions)))

(defun zp/helm-ispell-preselect (&optional lang)
  (interactive)
  (let ((current ispell-local-dictionary))
    (helm :sources '(zp/helm-source-ispell)
          :preselect (if (or
                          (eq lang "French")
                          (eq current nil)
                          (string-match-p current "british"))
                         "French"
                       "English"))))

(provide 'zp-ispell)
;;; zp-ispell.el ends here
