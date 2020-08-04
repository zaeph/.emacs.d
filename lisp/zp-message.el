;;; zp-message.el --- Expansion for message-mode  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; URL: https://github.com/zaeph/.emacs.d
;; Keywords: emacs, message-mode
;; Version: 0.1.0
;; Package-Requires: message

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
;; Personal expansion for message-mode.

;;; Code:
;;;; Library Requires
(require 'message)
(require 'dash)

;;----------------------------------------------------------------------------
;; Getting emails
;;----------------------------------------------------------------------------
(defun zp/get-email-with-alias (email alias &optional regex)
  "Create email alias from EMAIL and ALIAS.

If REGEX is non-nil, creates a regex to match the email alias."
  (let* ((email (cond
                 ((equal email "old-pro")
                  zp/email-old-pro)
                 ((equal email "old-private")
                  zp/email-old-private)
                 (t
                  email)))
         (email-alias (replace-regexp-in-string "@"
                                                (concat "+" alias "@")
                                                email)))
    (if regex
        (regexp-quote email-alias)
      email-alias)))

;;----------------------------------------------------------------------------
;; Signature
;;----------------------------------------------------------------------------
(defcustom zp/message-sigs-directory nil
  "Directory where email signatures live.")

(defvar zp/message-sigs-alist nil
  "Alist of emails and the signature to use with them.
It should be a symbol representing the signature-model to use.")

(defun zp/message-get-signature ()
  "Get signature for current buffer."
  (let* ((sig (-> (message-sendmail-envelope-from)
                  (downcase)
                  (assoc zp/message-sigs-alist)
                  (cdr)
                  (symbol-name)))
         (path (concat (file-name-as-directory zp/message-sigs-directory)
                       sig)))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

;;----------------------------------------------------------------------------
;; Extended movements
;;----------------------------------------------------------------------------
;; TODO: Improve
(defun zp/message-goto-bottom-1 ()
  (let ((newline message-signature-insert-empty-line))
    (goto-char (point-max))
    (when (re-search-backward message-signature-separator nil t)
      (end-of-line (if newline -1 0)))
    (point)))

(defun zp/message-goto-bottom ()
  "Go to the end of the message or buffer.
Go to the end of the message (before signature) or, if already there, go to the
end of the buffer."
  (interactive)
  (let ((old-position (point))
        (message-position (save-excursion (message-goto-body) (point)))
        (newline message-signature-insert-empty-line))
    (zp/message-goto-bottom-1)
    (when (equal (point) old-position)
      (goto-char (point-max)))))

(defun zp/message-goto-top-1 ()
  "Go to the beginning of the message."
  (interactive)
  (message-goto-body-1)
  (point))

(defun zp/message-goto-top ()
  "Go to the beginning of the message or buffer.
Go to the beginning of the message or, if already there, go to the
beginning of the buffer."
  (interactive)
  (let ((old-position (point)))
    (zp/message-goto-top-1)
    (when (equal (point) old-position)
      (goto-char (point-min)))))

(defun zp/message-goto-body-1 ()
  "Go to the beginning of the body of the message."
  (zp/message-goto-top-1)
  (forward-line 2)
  (point))

(defun zp/message-goto-body ()
  "Move point to the beginning of the message body."
  (interactive)
  (let ((old-position (point))
        (greeting (save-excursion
                    (zp/message-goto-top-1)
                    (re-search-forward "^[^>]+.*,$" (point-at-eol) t)))
        (modified))
    (zp/message-goto-top-1)
    (cond (greeting
           (forward-line 2))
          ((save-excursion
             (re-search-forward "writes:$" (point-at-eol) t))
           (insert "\n\n")
           (forward-char -2)
           (setq modified t))
          (t
           (insert "\n")
           (forward-char -1)
           (setq modified t)))
    ;; (cond ((re-search-forward "writes:$" (point-at-eol) t)
    ;;        (beginning-of-line)
    ;;        (insert "\n\n")
    ;;        (forward-char -2))
    ;;       ((re-search-forward "^[^>]+.*,$" (line-end-position) t)
    ;;        (zp/message-goto-body-1))
    ;;       (t
    ;;        (insert "\n")
    ;;        (forward-char -1)))
    (when (and (not modified)
               (equal (point) old-position))
      (zp/message-goto-top-1)
      (goto-char (1- (line-end-position))))))

(defun zp/message-goto-body-end-1 ()
  (zp/message-goto-bottom-1)
  (re-search-backward "[^[:space:]]")
  (end-of-line)
  (point))

(defun zp/message-goto-body-end ()
  (interactive)
  (let* ((old-position (point))
         (top-posting (save-excursion
                        (zp/message-goto-top-1)
                        (re-search-forward "writes:$" nil t)
                        (when (< old-position (line-beginning-position 0))
                          (line-beginning-position))))
         (sign-off (save-excursion
                     (or
                      (progn
                        (zp/message-goto-bottom-1)
                        (beginning-of-line)
                        (re-search-forward "^[^>]+.*,$" (line-end-position) t))
                      (and top-posting
                           (progn
                             (goto-char top-posting)
                             (beginning-of-line -1)
                             (re-search-forward "^[^>]+.*,$" (line-end-position) t))))))
         (modified))
    (if sign-off
        (progn
          (goto-char sign-off)
          (beginning-of-line 0)
          (re-search-backward "^[^>[:space:]]+" nil t)
          (end-of-line))
      (cond (top-posting
             (goto-char top-posting)
             (insert "\n\n")
             (forward-char -2)
             (setq modified t))
            (t
             (zp/message-kill-to-signature)
             (unless (bolp) (insert "\n"))
             (insert "\n")
             (setq modified t))))
    (when (and (not modified)
               (equal (point) old-position))
      (goto-char (1- sign-off)))))

(defun zp/message-kill-to-signature (&optional arg)
  "Kill all text up to the signature.
If a numeric argument or prefix arg is given, leave that number
of lines before the signature intact."
  (interactive "P")
  (let ((newline message-signature-insert-empty-line))
    (save-excursion
      (save-restriction
        (let ((point (point)))
	  (narrow-to-region point (point-max))
	  (message-goto-signature)
	  (unless (eobp)
	    (if (and arg (numberp arg))
	        (forward-line (- -1 arg))
	      (end-of-line (if newline -2 -1))))
	  (unless (= point (point))
	    (kill-region point (point))
	    (unless (bolp)
	      (insert "\n"))))))))

(defun zp/message-kill-to-signature (&optional arg)
  (interactive "P")
  (let ((newline message-signature-insert-empty-line)
        (at-end (save-excursion (= (point) (zp/message-goto-bottom-1)))))
    (when at-end
      (error "Already at end"))
    (message-kill-to-signature arg)
    (unless (bolp) (insert "\n"))
    (when newline
      (insert "\n")
      (forward-char -1))))

;;----------------------------------------------------------------------------
;; Automatic language detection
;;----------------------------------------------------------------------------
(defvar zp/message-ispell-alist nil
  "Alist of emails and the language they typically use.
The language should be the name as a string of a valid Ispell
dictionary.")

(defun zp/message-flyspell-auto ()
  "Start Ispell with the language associated with the email.

Looks for the email in the ‘From:’ field and chooses a language
based on ‘zp/message-mode-ispell-alist’."
  (let* ((sender (downcase (message-sendmail-envelope-from)))
         (language (cdr (assoc sender zp/message-ispell-alist))))
    (zp/ispell-switch-dictionary language)))

;;----------------------------------------------------------------------------
;; Unused functions
;;----------------------------------------------------------------------------
;; TODO: Consider possible usage
(defun zp/message-sendmail-envelope-to ()
  "Return the envelope to."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^To: " nil t)
      (substring-no-properties
       (buffer-substring
        (point)
        (point-at-eol))))))

(defun zp/message-retrieve-to ()
  "Create a list of emails from ‘To:’."
  (let ((to-raw (zp/message-sendmail-envelope-to))
        (emails))
    (with-temp-buffer
      (insert to-raw)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((bound (save-excursion
                       (if (re-search-forward "," nil t)
                           (progn (forward-char -1)
                                  (point))
                         (point-max)))))
          (re-search-forward "@")
          (if (re-search-backward " " nil t)
              (forward-char)
            (goto-char (point-min)))
          (setq framed (looking-at-p "<"))
          (push (substring-no-properties
                 (buffer-substring (if framed
                                       (1+ (point))
                                     (point))
                                   (if framed
                                       (1- bound)
                                     bound)))
                emails)
          (goto-char (1+ bound))))
      (setq email-list emails))))

(provide 'zp-message)
;;; zp-message.el ends here
