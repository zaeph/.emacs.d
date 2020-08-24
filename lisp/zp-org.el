;;; zp-org.el --- Personal expansion to Org-mode  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2013-2020 Leo Vivier <leo.vivier+dev@gmail.com>

;; Author: Leo Vivier <leo.vivier+dev@gmail.com>
;; Keywords: org
;; Version: 0.1.0
;; Package-Requires: org

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
;; This file contains the expansions I have written for Org-mode.

;;; Code:
;;;; Library Requires
(require 'org)

;;----------------------------------------------------------------------------
;; Archiving
;;----------------------------------------------------------------------------
;; Keep hierarchy when archiving
  ;; Source: https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
  (defadvice org-archive-subtree (around fix-hierarchy activate)
    (let* ((fix-archive-p (and (not current-prefix-arg)
                               (not (use-region-p))))
           (location (org-archive--compute-location
                      (or (org-entry-get nil "ARCHIVE" 'inherit)
                          org-archive-location)))
           (afile (car location))
           (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
      ad-do-it
      (when fix-archive-p
        (with-current-buffer buffer
          (goto-char (point-max))
          (while (org-up-heading-safe))
          (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
                 (path (and olpath (split-string olpath "/")))
                 (level 1)
                 tree-text)
            (when olpath
              (org-mark-subtree)
              (setq tree-text (buffer-substring (region-beginning) (region-end)))
              (let (this-command) (org-cut-subtree))
              (goto-char (point-min))
              (save-restriction
                (widen)
                (-each path
                  (lambda (heading)
                    (if (re-search-forward
                         (rx-to-string
                          `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                        (org-narrow-to-subtree)
                      (goto-char (point-max))
                      (unless (looking-at "^")
                        (insert "\n"))
                      (insert (make-string level ?*)
                              " "
                              heading
                              "\n"))
                    (cl-incf level)))
                (widen)
                (org-end-of-subtree t t)
                (org-paste-subtree level tree-text))))))))

;;----------------------------------------------------------------------------
;; State flow
;;----------------------------------------------------------------------------
;; Heavily inspired from
;; https://emacs.stackexchange.com/questions/9433/how-to-make-org-prompt-for-a-timestamp-when-changing-state-of-a-todo/9451#9451
(defun zp/org-todo-with-date (&optional arg)
  "Like `org-todo' but the time of change will be prompted."
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
             ((symbol-function 'current-time)
              #'(lambda () my-current-time))
             ((symbol-function 'org-today)
              #'(lambda () (time-to-days my-current-time)))
             ((symbol-function 'org-current-effective-time)
              #'(lambda () my-current-time))
             (super (symbol-function 'format-time-string))
             ((symbol-function 'format-time-string)
              #'(lambda (fmt &optional time time-zone)
                  (funcall super fmt my-current-time time-zone))))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-todo arg)
      (org-todo arg))))

;;----------------------------------------------------------------------------
;; LaTeX export
;;----------------------------------------------------------------------------
(defvar zp/org-format-latex-default-scale 3.0
  "Initial value for the scale of LaTeX previews.")

(defun zp/org-latex-preview-dwim (arg)
  "Run org-latex-preview after updating the scale."
  (interactive "P")
  (let* ((default-scale 3)
         (scale-amount (or (and (boundp 'text-scale-mode-amount)
                                text-scale-mode-amount)
                           0))
         (new-scale (+ default-scale scale-amount)))
    (setq-local org-format-latex-options
                (plist-put org-format-latex-options :scale new-scale))
    (org-latex-preview arg)))

;;----------------------------------------------------------------------------
;; Indented quotes
;;----------------------------------------------------------------------------
;; Inspired from https://emacs.stackexchange.com/questions/38570/org-mode-quote-block-indentation-highlighting
(defun zp/org-indent-quotes (limit)
  (let ((case-fold-search t))
    (while (search-forward-regexp "^[ \t]*#\\+begin_quote" limit t)
      (let ((beg (1+ (match-end 0))))
        ;; on purpose, we look further than LIMIT
        (when (search-forward-regexp "^[ \t]*#\\+end_quote" nil t)
          (let ((end (1- (match-beginning 0)))
                (indent (propertize "    " 'face 'org-hide)))
            (add-text-properties beg end (list 'line-prefix indent
                                               'wrap-prefix indent))))))))

;;----------------------------------------------------------------------------
;; ‘CREATED’ property
;;----------------------------------------------------------------------------
(defvar zp/org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")

;; TODO: Find the source for this because I’ve improved something which
;; already existed
(defun zp/org-set-created-property (&optional active name)
  "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given, the ‘NAME’
argument will be used instead. If the property already exists, it
will not be modified.

If the function sets CREATED, it returns its value."
  (interactive)
  (let* ((created (or name zp/org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now)
      now)))

;;----------------------------------------------------------------------------
;; Updating time-based metadata
;;----------------------------------------------------------------------------
(defun zp/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun zp/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (zp/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "LAST_MODIFIED")))

;;----------------------------------------------------------------------------
;; Narrowing and movements
;;----------------------------------------------------------------------------
(defvar zp/org-after-view-change-hook nil
    "Hook run after a significant view change in org-mode.")

  (defun zp/org-overview (&optional arg keep-position keep-restriction)
    "Switch to overview mode, showing only top-level headlines.

With a ‘C-u’ prefix, do not move point.

When KEEP-RESTRICTION is non-nil, do not widen the buffer."
    (interactive "p")
    (let ((pos-before (point))
          (indirect (not (buffer-file-name)))
          (narrowed (buffer-narrowed-p)))
      (setq-local zp/org-narrow-previous-position pos-before)
      ;; Do not widen buffer if in indirect buffer
      (save-excursion
        (goto-char (point-min))
        (widen)
        (when (or (and indirect
                       narrowed)
                  keep-restriction)
          (org-narrow-to-subtree))
        (unless indirect
          (org-display-inline-images)))
      (zp/org-fold (or keep-position
                       (and arg
                            (> arg 1))))
      (when arg
        (message "Showing overview.")
        (run-hooks 'zp/org-after-view-change-hook))))

  (defun zp/org-fold (&optional keep-position)
    (let ((indirectp (not (buffer-file-name)))
          (org-startup-folded 'overview))
      ;; Fold drawers
      (org-set-startup-visibility)
      ;; Fold trees
      (org-overview)
      (unless keep-position
        (goto-char (point-min)))
      (recenter)
      (save-excursion
        (goto-char (point-min))
        (org-show-entry)
        (when (org-at-heading-p)
          (org-show-children)))))

  (defun zp/org-show-all (arg)
    (interactive "p")
    (let ((pos-before (point))
          (indirect (not (buffer-file-name))))
      (setq-local zp/org-narrow-previous-position pos-before)
      ;; Do not widen buffer if in indirect buffer
      (unless indirect
        (widen)
        (org-display-inline-images))
      ;; Unfold everything
      (org-show-all)
      (unless (eq arg 4)
        (goto-char (point-min)))
      (recenter-top-bottom)
      (when arg
        (message "Showing everything.")
        (run-hooks 'zp/org-after-view-change-hook))))

  ;; org-narrow movements

  (defun zp/org-narrow-to-subtree ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (org-narrow-to-subtree)
    (zp/org-fold nil)
    (when (called-interactively-p 'any)
      (message "Narrowing to tree at point.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defun zp/org-widen ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (let ((pos-before (point)))
      (setq-local zp/org-narrow-previous-position pos-before))
    (widen)
    (when (called-interactively-p 'any)
      (message "Removing narrowing.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defvar zp/presentation-mode nil)

  (defun zp/org-narrow-forwards ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (widen)
    (org-forward-heading-same-level 1)
    (org-narrow-to-subtree)
    (unless zp/presentation-mode
      (zp/org-fold nil))
    (when (called-interactively-p 'any)
      (message "Narrowing to next tree.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defun zp/org-narrow-backwards ()
    "Move to the next subtree at same level, and narrow the buffer to it."
    (interactive)
    (widen)
    (org-backward-heading-same-level 1)
    (org-narrow-to-subtree)
    (unless zp/presentation-mode
      (zp/org-fold nil))
    (when (called-interactively-p 'any)
      (message "Narrowing to previous tree.")
      (run-hooks 'zp/org-after-view-change-hook)))

  (defun zp/org-narrow-up-heading (&optional arg keep-position)
    "Move to the upper subtree, and narrow the buffer to it."
    (interactive "p")
    (unless (buffer-narrowed-p)
      (user-error "No narrowing"))
    (let ((pos-before (point)))
      (setq-local zp/org-narrow-previous-position pos-before)
      (widen)
      (org-reveal)
      (outline-up-heading 1)
      (org-narrow-to-subtree)
      (when (or (eq arg 4)
                keep-position)
        (goto-char pos-before)
        (recenter-top-bottom))
      (zp/org-fold (or (eq arg 4)
                       keep-position))
      (when arg
        (message "Narrowing to tree above.")
        (run-hooks 'zp/org-after-view-change-hook))))

  (defun zp/org-narrow-up-heading-dwim (arg)
    "Narrow to the upper subtree, and narrow the buffer to it.

If the buffer is already narrowed to level-1 heading, overview
the entire buffer."
    (interactive "p")
    (if (save-excursion
          ;; Narrowed to a level-1 heading?
          (goto-char (point-min))
          (and (buffer-narrowed-p)
               (equal (org-outline-level) 1)))
        (zp/org-overview arg)
      (zp/org-narrow-up-heading arg)))

  (defun zp/org-narrow-previous-heading (arg)
    "Move to the previously narrowed tree, and narrow the buffer to it."
    (interactive "p")
    (if (bound-and-true-p zp/org-narrow-previous-position)
        (let ((pos-before zp/org-narrow-previous-position))
          (goto-char zp/org-narrow-previous-position)
          (org-reveal)
          (org-cycle)
          (org-narrow-to-subtree)
          (setq zp/org-narrow-previous-position nil)
          (message "Narrowing to previously narrowed tree."))
      (message "Couldn’t find a previous position.")))

  ;; Toggle fontifications
  (defun zp/org-toggle-emphasis-markers (&optional arg)
    "Toggle emphasis markers."
    (interactive "p")
    (let ((markers org-hide-emphasis-markers))
      (if markers
          (setq-local org-hide-emphasis-markers nil)
        (setq-local org-hide-emphasis-markers t))
      (when arg
        (font-lock-fontify-buffer))))

  (defun zp/org-toggle-link-display (&optional arg)
    "Toggle the literal or descriptive display of links in the current buffer."
    (interactive "p")
    (if org-link-descriptive (remove-from-invisibility-spec '(org-link))
      (add-to-invisibility-spec '(org-link)))
    (setq-local org-link-descriptive (not org-link-descriptive))
    (when arg
      (font-lock-fontify-buffer)))

  (defun zp/org-toggle-fontifications (&optional arg)
    "Toggle emphasis markers or the link display.

Without a C-u argument, toggle the emphasis markers.

With a C-u argument, toggle the link display."
    (interactive "P")
    (let ((markers org-hide-emphasis-markers)
          (links org-link-descriptive))
      (if arg
          (zp/org-toggle-link-display)
        (zp/org-toggle-emphasis-markers))
      (font-lock-fontify-buffer)))

  (defun zp/org-find-olp (target &optional move)
    "Find TARGET in the current buffer.

When there is a match and MOVE is nil, return a marker pointing
to the beginning of the matched headline.  When there is a match
and MOVE is non-nil, move point to that match.  Otherwise, return
nil.

TARGET can either be a string or a list.  If it is a string, it
should be the name of the top-headline to find.  If it is a list,
it should be an outline path OLP."
    (let* ((olp (pcase target
                  ((pred stringp) (list target))
                  ((pred listp) target)
                  (wrong-type (signal 'wrong-type-argument
                                      `((stringp listp)
                                        ,wrong-type)))))
           (marker (condition-case msg
                       (org-find-olp olp t)
                     (error))))
      (cond ((and marker
                  (not move))
             marker)
            (marker
             (goto-char marker)
             (set-marker marker nil)
             t))))

;;----------------------------------------------------------------------------
;; Custom exported timestamp
;;----------------------------------------------------------------------------
;; TODO: Check if I still need this.
(add-to-list 'org-export-filter-timestamp-functions
             #'endless/filter-timestamp)

(defun endless/filter-timestamp (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    ((or `jekyll `html)
     (replace-regexp-in-string "&[lg]t;" "" trans))
    (`latex
     (replace-regexp-in-string "[<>]" "" trans))))

(setq org-time-stamp-custom-formats
      '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))

;;----------------------------------------------------------------------------
;; Commenting old ‘LOGBOOK’ notes
;;----------------------------------------------------------------------------
(defcustom zp/org-logbook-notes-current-lines 30
  "Number of lines to keep active in LOGBOOK-NOTES drawers.")

(defun zp/org-comment-logbook-notes ()
  "Only keep a certain amount of lines in LOGBOOK-NOTES drawers.

A large amount of lines in LOGBOOK-NOTES slows down state-changes, which is
why it is a good idea to only keep a set amount of them.

The number of lines to keep is defined in
`zp/org-logbook-notes-current-lines'."
  (let* ((logbook-notes-top-re "^[\s]*:LOGBOOK-NOTES:[\s]*\n")
         (logbook-notes-bottom-re "[\s]*:END:[\s]*$")
         (logbook-notes-re (concat logbook-notes-top-re
                                   "\\(?:.*\n\\)*?"
                                   logbook-notes-bottom-re))
         (lines-to-keep zp/org-logbook-notes-current-lines)
         ;; Prevent org-ref from interfering with our affairs
         (org-ref-labels nil))
    (save-restriction
      (widen)
      (let ((heading-end (save-excursion
                           (forward-line)
                           (or (re-search-forward "^\*+ " nil t)
                               (point-max)))))
        (save-excursion
          (org-back-to-heading)
          (when-let ((drawer-end (re-search-forward logbook-notes-re
                                                    heading-end
                                                    t))
                     (notes (match-string 0))
                     (drawer-beg (re-search-backward logbook-notes-top-re nil t))
                     (notes-beg (save-excursion
                                  (forward-line)
                                  (point)))
                     (notes-end (save-excursion
                                  (goto-char drawer-end)
                                  (forward-line -1)
                                  (point-at-eol))))
            (when (> (count-lines notes-beg notes-end) lines-to-keep)
              (let ((current-notes-end (save-excursion
                                         (forward-line lines-to-keep)
                                         (point-at-eol))))
                (goto-char notes-end)
                (while (re-search-backward "^[^#]" current-notes-end t)
                  (let ((bol (point-at-bol))
                        (eol (point-at-eol)))
                    (unless (comment-only-p bol eol)
                      (comment-region-default bol eol)
                      (goto-char (point-at-bol)))))))))))))

;;----------------------------------------------------------------------------
;; Spawned indirect buffers
;;----------------------------------------------------------------------------
(defun zp/org-tree-to-indirect-buffer-folded (arg &optional dedicated bury)
  "Clone tree to indirect buffer in a folded state.

When called with a ‘C-u’ prefix or when DEDICATED is non-nil,
create a dedicated frame."
  (interactive "p")
  (let* ((in-new-window (and arg
                             (one-window-p)))
         (org-indirect-buffer-display (if in-new-window
                                          'other-window
                                        'current-window))
         (last-ibuf org-last-indirect-buffer)
         (parent (current-buffer))
         (parent-window (selected-window))
         (dedicated (or dedicated
                        (eq arg 4))))
    (when dedicated
      (setq org-last-indirect-buffer nil))
    (when (and arg
               zp/org-spawned-ibuf-mode)
      (zp/org-ibuf-spawned-dedicate))
    (org-tree-to-indirect-buffer)
    (when in-new-window
      (select-window (next-window))
      (setq zp/org-ibuf-spawned-also-kill-window parent-window))
    (if dedicated
        (setq org-last-indirect-buffer last-ibuf)
      (zp/org-spawned-ibuf-mode t))
    (when bury
      (switch-to-buffer parent nil t)
      (bury-buffer))
    (let ((org-startup-folded nil))
      (org-set-startup-visibility))
    (org-overview)
    (org-show-entry)
    (org-show-children)
    (prog1 (selected-window)
      (when arg
        (message "Cloned tree to indirect buffer.")
        (run-hooks 'zp/org-after-view-change-hook)))))

(defun zp/org-kill-spawned-ibuf (&optional arg)
  "Kill the current buffer if it is an indirect buffer."
  (interactive "p")
  (let* ((other (not (one-window-p)))
         (indirect (buffer-base-buffer))
         (spawn zp/org-spawned-ibuf-mode)
         (parent-window zp/org-ibuf-spawned-also-kill-window))
    (unless (and indirect
                 spawn)
      (user-error "Not a spawned buffer"))
    (if (and other
             parent-window)
        (progn (kill-buffer-and-window)
               ;; Select parent when called interactively
               (when arg
                 (select-window parent-window)))
      (kill-buffer))
    (when arg
      (message "Killed indirect buffer."))
    (run-hooks 'zp/org-after-view-change-hook)))

(defun zp/org-ibuf-spawned-dedicate (&optional print-message)
  (unless (and (boundp zp/org-spawned-ibuf-mode) zp/org-spawned-ibuf-mode)
    (user-error "Not in a spawned buffer"))
  (zp/org-spawned-ibuf-mode -1)
  (setq org-last-indirect-buffer nil)
  (setq header-line-format nil)
  (when print-message
    (message "Buffer is now dedicated.")))

(defun zp/org-kill-spawned-ibuf-dwim (&optional dedicate)
  "Kill the current buffer if it is an indirect buffer.

With a ‘C-u’ argument, dedicate the buffer instead."
  (interactive "P")
  (if dedicate
      (zp/org-ibuf-spawned-dedicate t)
    (zp/org-kill-spawned-ibuf t)))

(defvar-local zp/org-ibuf-spawned-also-kill-window nil
  "When t, also kill the window when killing a spawned buffer.

A spawned buffer is an indirect buffer created by
‘org-tree-to-indirect-buffer’ which will be replaced by
subsequent calls.")

(defvar zp/org-spawned-ibuf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'zp/org-kill-spawned-ibuf-dwim)
    map)
  "Keymap for ‘zp/org-spawned-ibuf-mode’.")

(define-minor-mode zp/org-spawned-ibuf-mode
  "Show when the current indirect buffer is a spawned buffer."
  :lighter " Spawn"
  :keymap zp/org-spawned-ibuf-mode-map
  (setq header-line-format
        "Spawned indirect buffer.  Kill with ‘C-c C-k’, dedicate with ‘C-u C-c C-k’."))

(provide 'zp-org)
;;; zp-org.el ends here
