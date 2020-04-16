;;; zp-hydra-shortcuts.el --- Hydra for accessing shortcuts  -*- fill-column: 78; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defun zp/hydra-shortcut-make-head (key path &optional name)
  (let ((name (or name
                  (file-name-base path))))
    `(,key (find-file ,path) ,name)))

(defmacro zp/hydra-shortcut-make-hydra (&rest shortcuts)
  `(defhydra zp/hydra-shortcuts (:color blue)
     ,@(mapcar (lambda (list)
                 (let ((key (pop list))
                       (path (pop list))
                       (name (pop list)))
                   (zp/hydra-shortcut-make-head key path name)))
               shortcuts)))

(provide 'zp-hydra-shortcuts)
;;; zp-hydra-shortcuts.el ends here
