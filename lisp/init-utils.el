;;; init-utils.el --- Elisp helpers  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
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

;;; Code:
;;;; Library Requires
(require 'compile)

(defun zp/get-string-from-file (file-path)
  "Read file content from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; TODO: Does it need to be macro?
(defmacro zp/advise-commands (method commands where function)
  "Macro for advising COMMANDS with FUNCTION.

METHOD and WHERE follows the same syntax as `add-advice'.  COMMANDS should
be the list of commands to advice."
  (let ((where-keyword (intern-soft (concat ":" (symbol-name where)))))
    `(progn
       ,@(cond ((string= method 'add)
                (mapcar (lambda (command)
                          `(advice-add ',command ,where-keyword ',function))
                        commands))
               ((string= method 'remove)
                (mapcar (lambda (command)
                          `(advice-remove ',command  ',function))
                        commands))))))

(defmacro zp/add-hooks (method commands function)
  "Add FUNCTION as a hook to COMMANDS with METHOD.
See `add-hooks' for details."
  `(progn
     ,@(cond ((string= method 'add)
              (mapcar (lambda (command)
                        `(add-hook ',command ',function))
                      commands))
             ((string= method 'remove)
              (mapcar (lambda (command)
                        `(remove-hook ',command ',function))
                      commands)))))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))

(provide 'init-utils)
;;; init-utils.el ends here
