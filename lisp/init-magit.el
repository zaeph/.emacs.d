;;; init-magit.el --- Settings and helpers for magit  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2021-2024 Leo Vivier <zaeph@zaeph.net>

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
(require 'magit)

;;; Extra options for transient

(transient-append-suffix 'magit-log "-A"
  '("-1" "First parent" "--first-parent"))
(transient-append-suffix 'magit-log-refresh "-A"
  '("-1" "First parent" "--first-parent"))
(transient-append-suffix 'magit-log "-f"
  '("-m" "Hide merges" "--no-merges"))
(transient-append-suffix 'magit-log-refresh "-f"
  '("-m" "Hide merges" "--no-merges"))
(transient-append-suffix 'magit-push "-u"
  '("=s" "Skip gitlab pipeline" "--push-option=ci.skip"))


;;; Options

;;;; Diff
(setq magit-diff-refine-hunk 'all)

;;; WIP-mode
(magit-wip-mode)

(provide 'init-magit)
;;; init-magit.el ends here
