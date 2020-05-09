;;; ace-link-notmuch-show.el --- Library for opening links in notmuch-show  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:
;;
;; This library provides a ace-link handler for links displayed in
;; a notmuch-show buffer.

;;; Code:
;;;; Library Requires
(require 'ace-link)
(require 'subr-x)
(require 'shr)

;;----------------------------------------------------------------------------
;; Handler for text/plain
;;----------------------------------------------------------------------------
(defun ace-link--notmuch-show-plain-collect ()
  "Collect the positions of visible links in `notmuch-show' buffer.
Only consider the links in 'text/plain'."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (while (re-search-forward "https?://" nil t)
          (setq pt (- (point) (length (match-string 0))))
          (push pt candidates))))
    (nreverse candidates)))

(defun ace-link--notmuch-show-plain-action (pt)
  "Open link at PT in a `notmuch-show' buffer.
Only works in 'text/plain'"
  (goto-char pt)
  (browse-url-at-point))

;;;###autoload
(defun ace-link-notmuch-show-plain ()
  "Open a visible link in a `notmuch-show' buffer.
Only consider the 'text/plain' portion of the buffer."
  (interactive)
  (when-let ((pt (avy-with ace-link-notmuch-show-plain
                   (avy-process
                    (ace-link--notmuch-show-plain-collect)
                    #'avy--overlay-pre))))
    (ace-link--notmuch-show-plain-action pt)))

;;----------------------------------------------------------------------------
;; Handler for text/html
;;----------------------------------------------------------------------------
(defun ace-link--notmuch-show-html-next-link (pos)
  "Find next link from POS in current `notmuch-show' buffer."
  (let* ((shr-link-pos (text-property-not-all pos (point-max) 'shr-url nil))
         (links (seq-filter
                 (lambda (link)
                   (elt link 1))
                 (list
                  (list 'shr-url shr-link-pos)))))

    (if links
        (car
         (sort links (lambda (x y)
                       (< (elt x 1) (elt y 1)))))
      nil)))

(defun ace-link--notmuch-show-html-end-of-link (link)
  "Return end of LINK at point in current `notmuch-show' buffer."
  (or (text-property-any (elt link 1) (point-max) (elt link 0) nil)
      (point-max)))

;;;###autoload
(defun ace-link--notmuch-show-html-collect ()
  "Collect the positions of visible links in current `notmuch-show' buffer."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (window-start)
       (window-end))
      (goto-char (point-min))
      (let (link pos candidates)
        (setq pos (point))
        (while (setq link (ace-link--mu4e-next-link pos))
          (goto-char (elt link 1))
          (setq pos (ace-link--mu4e-end-of-link link))
          (push (cons (buffer-substring-no-properties (elt link 1) pos) (elt link 1)) candidates))
        (nreverse candidates)))))

(defun ace-link--notmuch-show-html-action (pt)
  "Open link at PT in a `notmuch-show' buffer.
Only works in 'text/html'"
  (when (number-or-marker-p pt)
    (when (get-text-property (point) 'shr-url)
      (shr-browse-url))))

(defun ace-link-notmuch-show-html ()
  "Open a visible link in a `notmuch-show' buffer.
Only consider the 'text/html' portion of the buffer."
  (interactive)
  (if (bound-and-true-p mu4e-view-use-gnus)
      (ace-link-gnus)
    (let ((pt (avy-with ace-link-mu4e
                (avy-process
                 (mapcar #'cdr (ace-link--mu4e-collect))
                 (avy--style-fn avy-style)))))
      (ace-link--mu4e-action pt))))

;;----------------------------------------------------------------------------
;; Combined handler for text/plain and text/html
;;----------------------------------------------------------------------------
(defun ace-link--notmuch-collect ()
  "Collect the positions of visible links in `notmuch-show' buffer.
Considers the links in 'text/plain' and 'text/html'.
Returns a list of cons \( fn . pt ) where FN is the function to
call at PT."
  (append
   (mapcar (lambda (x)
             (cons x #'ace-link--notmuch-show-plain-action))
           (ace-link--notmuch-show-plain-collect))
   (mapcar (lambda (x)
             (cons (cdr x) #'ace-link--notmuch-show-html-action))
           (ace-link--notmuch-show-html-collect))))

;;;###autoload
(defun ace-link-notmuch-show ()
  "Open a visible link in `notmuch-show' buffer.
Consider both the links in 'text/plain' and 'text/html'."
  (interactive)
  (when-let ((match (avy-with ace-link-notmuch-show
                      (avy-process
                       (ace-link--notmuch-collect)
                       #'avy--overlay-pre))))
    (let ((pt (car match))
          (fn (cdr match)))
      (funcall fn pt))))

(provide 'ace-link-notmuch-show)
;;; ace-link-notmuch-show.el ends here
