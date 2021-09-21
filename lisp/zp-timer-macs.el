;;; zp-timer-macs.el --- Timer macros  -*- fill-column: 78; lexical-binding: t; -*-

;; Copyright © 2021 Leo Vivier <zaeph@zaeph.net>

;; Author: Leo Vivier <zaeph@zaeph.net>
;; Keywords: timer macros
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

;;; Code:
(defvar gc-cons-threshold-for-timers 800000000
  "Custom ‘gc-cons-threshold’ to be used by ‘time’.

This prevents garbage-collection from interfering with the
functions being timed.

The value needs to be sufficiently high to prevent
garbage-collection during execution, but not so high as to cause
performance problems.")

(defvar timer-output-format "%.3fs (GC: +%.3fs, Σ: %.3fs)"
  "Default output format for timers.")

(defmacro time-internal (&rest forms)
  "Compute the time taken to run FORMS.

Return a list containing:
- The return value of FORMS
- The time taken to evaluate FORMS
- The time taken to garbage collect
- The total time"
  `(let* ((body '(progn ,@forms))
          (start (current-time))
          (gc-cons-threshold (progn (garbage-collect)
                                    gc-cons-threshold-for-timers))
          (return-value (eval body))
          (end (current-time))
          (elapsed (float-time (time-subtract end start)))
          results)
     (prog1 (setq results (list return-value elapsed))
       (let* ((_beg-gc (prog1 (current-time)
                         (garbage-collect)))
              (end-gc (current-time))
              (elapsed-gc (float-time (time-subtract end-gc end)))
              (elapsed-total (+ elapsed elapsed-gc)))
         (push elapsed-gc (cdr (last results)))
         (push elapsed-total (cdr (last results)))))))

(defmacro time (&rest forms)
  "Return the time taken to run FORMS as a string."
  (message "Processing...")
  `(let* ((results (time-internal ,@forms))
          (return-value (pop results))
          (elapsed (pop results))
          (elapsed-gc (pop results))
          (elapsed-total (pop results)))
     (format timer-output-format elapsed elapsed-gc elapsed-total)))

(defun time-validate-iterations (iterations)
  "Validate ITERATIONS."
  (pcase iterations
    ((pred (natnump))
     (when (zerop iterations)
       (user-error "ITERATIONS needs to be a non-null positive integer")) )
    (wrong-type (signal 'wrong-type-argument
                        `((natnump)
                          ,wrong-type)))))

(defmacro time-stats-internal (iterations multiplier &rest forms)
  "Return statistics on the execution of FORMS.

ITERATIONS is the sample-size to use for the statistics.

MULTIPLIER is an integer to specify how many times to evaluate
FORMS on each iteration."
  (declare (indent 2))
  (time-validate-iterations iterations)
  (let ((multiplier (or multiplier 1)))
    `(let (list)
       (dotimes (i ,iterations)
         (message "Processing...  Iteration: %s" (1+ i))
         (push (nth 1 (time-internal
                       (dotimes (y ,multiplier)
                         ,@forms)))
               list))
       (let ((min (apply #'min list))
             (max (apply #'max list))
             (mean (/ (apply #'+ list) (length list))))
         (list min max mean)))))

(defmacro time-stats (iterations multiplier &rest forms)
  "Return statistics on the execution of FORMS as a string.

ITERATIONS is the sample-size to use for the statistics.

MULTIPLIER is an integer to specify how many times to evaluate
FORMS on each iteration."
  (declare (indent 2))
  (time-validate-iterations iterations)
  `(if (= ,iterations 1)
       (time
        (dotimes (_y ,multiplier)
          ,@forms))
     (pcase-let* ((stats (time-stats-internal ,iterations ,multiplier ,@forms))
                  (`(,min ,max ,mean) stats))
       (message (format "min: %.3fs, max: %.3fs, mean: %.3fs" min max mean)))))

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.

A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (message "%s..." title)
  `(let* ((results (time-internal ,@forms))
          (return-value (pop results))
          (elapsed (pop results))
          (elapsed-gc (pop results))
          (elapsed-total (pop results)))
     (prog1 return-value
       (message (concat "%s...done in " timer-output-format)
                ,title elapsed elapsed-gc elapsed-total))))

(provide 'zp-timer-macs)
;;; zp-timer-macs.el ends here
