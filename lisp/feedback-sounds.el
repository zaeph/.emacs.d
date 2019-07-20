;;; feedback-sounds.el --- Library of sounds to play with commands -*- lexical-binding: t; -*-
;;; Commentary:

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
