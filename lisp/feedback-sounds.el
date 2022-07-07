;;; feedback-sounds.el --- Library of sounds to play with commands -*- fill-column: 78; lexical-binding: t; -*-

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
;; This is a library of sounds to be played on various actions in Emacs.
;; Please note that that I am using a custom script called ‘play-sound’ to,
;; well, play the sounds.

;;; Code:

;; TODO: Simplify, and use more meaningful names for functions

(defun zp/play-sound-clock-in ()
  (start-process-shell-command "play-sound" nil "notification-sound-org clock-in"))

(defun zp/play-sound-clock-out ()
  (start-process-shell-command "play-sound" nil "notification-sound-org clock-out"))

(defun zp/play-sound-reward ()
  (when (string-equal org-state "DONE")
    (start-process-shell-command "play-sound" nil "notification-sound-org done")))

(defun zp/play-sound-start-capture ()
  (start-process-shell-command "play-sound" nil "notification-sound-org open"))

(defun zp/play-sound-after-capture ()
  (start-process-shell-command "play-sound" nil "notification-sound-org close"))

(defun zp/play-sound-after-refile ()
  (start-process-shell-command "play-sound" nil "notification-sound-org move"))

(defun zp/play-sound-turn-page ()
  (start-process-shell-command "play-sound" nil "notification-sound-org page"))

(provide 'feedback-sounds)
;;; feedback-sounds.el ends here
