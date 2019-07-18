;;; theme.el --- Custom theme with day/night cycle -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Theme definition
;;----------------------------------------------------------------------------
(defvar zp/emacs-theme nil
  "Theme currently used by Emacs.")

(defun zp/dark-theme ()
  (interactive)
  (setq zp/emacs-theme "dark")
  (load-theme 'base16-atelier-sulphurpool t)

  (set-face-attribute 'default nil :foreground "#BCAF8E" :background "#141414")
  (set-face-attribute 'org-todo nil :foreground "darkred")
  (set-face-attribute 'org-done nil :foreground "spring green")
  (set-face-attribute 'org-scheduled-today nil :foreground "CadetBlue")
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-hide nil :foreground "#141414")
  (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSlateBlue")
  (set-face-attribute 'region nil :background "RoyalBlue4")
  (set-face-attribute 'helm-selection nil :background "RoyalBlue4") ;Darker Royal Blue
  (set-face-attribute 'org-agenda-clocking nil :background "RoyalBlue4")
  (set-face-attribute 'fringe nil :background "gray10" :foreground "orangered")
  (set-face-attribute 'vertical-border nil :foreground "RoyalBlue1")
  (set-face-attribute 'org-agenda-structure nil :foreground "DodgerBlue1" :weight 'bold)
  (set-face-attribute 'hl-line nil :background "#1F1F3F")
  (set-face-attribute 'org-level-4 nil :foreground "#ed3971")
  (set-face-attribute 'org-meta-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'header-line nil :foreground "#777")
  (set-face-attribute 'line-number nil :foreground "#969996" :background "#2d2d2d")
  (set-face-attribute 'secondary-selection nil :background "#3B3273")
  (set-face-attribute 'org-column nil :background "#1F1F1F")
  (set-face-attribute 'org-block nil :foreground nil :inherit 'default :background "#1F1F1F")


  (set-face-attribute 'zp/org-agenda-block-info-face nil
                      :foreground "violetred1"
                      :background "violetred4"
                      :height 0.8
                      :weight 'bold)
  (set-face-attribute 'zp/org-agenda-block-warning-face nil :foreground "red" :weight 'bold)

  (zp/org-todo-format-face 'normal 'org-todo-todo "darkred")
  (zp/org-todo-format-face 'normal 'org-todo-next "DodgerBlue1")
  (zp/org-todo-format-face 'normal 'org-todo-strt "gold3")
  (zp/org-todo-format-face 'normal 'org-todo-done "SpringGreen3")
  (zp/org-todo-format-face 'normal 'org-todo-stby "SkyBlue4")
  (zp/org-todo-format-face 'normal 'org-todo-wait "Skyblue4")
  (zp/org-todo-format-face 'normal 'org-todo-cxld "turquoise")

  (zp/org-format-face 'org-priority-face-a :foreground "white" :background "darkred")
  (zp/org-format-face 'org-priority-face-b :foreground "darkred")
  (zp/org-format-face 'org-priority-face-c :foreground "yellow")
  (zp/org-format-face 'org-priority-face-d :foreground "ForestGreen")
  (zp/org-format-face 'org-priority-face-e :foreground "RoyalBlue")

  (zp/org-format-face 'org-tag-location  :weight 'bold :foreground "BlueViolet")
  (zp/org-format-face 'org-tag-todo   :weight 'bold :foreground "Skyblue4")
  (zp/org-format-face 'org-tag-important :weight 'bold :foreground "darkred")
  (zp/org-format-face 'org-tag-curios   :weight 'bold :foreground "DeepPink")
  (zp/org-format-face 'org-tag-french    :weight 'bold :foreground "DodgerBlue1")

  (zp/org-format-face 'magit-tag :foreground "SpringGreen4")

  (zp/org-super-agenda-update-face)

  (zp/mode-line-theme "dark")
  (zp/pdf-view-midnight-mode-theme))

(defun zp/light-theme ()
  (interactive)
  (setq zp/emacs-theme "light")
  (load-theme 'base16-google-light t)

  ;; (set-face-attribute 'org-todo-box nil :inverse-video t :foreground "white" :height 0.8 :weight 'bold :box nil)
  ;; (set-face-attribute 'default nil :background "cornsilk1") ;fff8dc
  (set-face-attribute 'default nil :foreground "#3c3836" :background "#fbf1c7")
  (set-face-attribute 'fringe nil :background "#e6deb8" :foreground "orangered")
  (set-face-attribute 'org-hide nil :foreground "#fbf1c7")
  (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSlateBlue")
  (set-face-attribute 'org-scheduled-today nil :foreground "DodgerBlue4")
  (set-face-attribute 'region nil :background "SkyBlue1")
  (set-face-attribute 'hl-line nil :background "#ffea89")
  (set-face-attribute 'org-level-4 nil :foreground "#ed3971")
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-agenda-clocking nil :background "LightBlue2")
  (set-face-attribute 'org-meta-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'helm-selection nil :background "SteelBlue1")
  (set-face-attribute 'helm-visible-mark nil :background "goldenrod1")
  (set-face-attribute 'header-line nil :foreground "#ccc")
  (set-face-attribute 'org-agenda-structure nil :foreground "DodgerBlue1" :weight 'bold)
  (set-face-attribute 'line-number nil :foreground "#636663" :background "#d4cdaa")
  ;; (set-face-attribute 'line-number-current-line nil :foreground "#707370" :background "#ccc6a4")
  (set-face-attribute 'secondary-selection nil :background "#d3ccff")
  (set-face-attribute 'org-column nil :background "#F0E4BE")
  (set-face-attribute 'org-block nil :foreground nil :inherit 'default :background "#F0E6BE")


  (set-face-attribute 'diff-hl-change nil :foreground "#3a81c3" :background "#afcce7")
  (set-face-attribute 'diff-hl-insert nil :foreground "#7ccd7c" :background "#b3e2b3")
  (set-face-attribute 'diff-hl-delete nil :foreground "#ee6363" :background "#f6a8a8")

  (set-face-attribute 'zp/org-agenda-block-info-face nil
                      :foreground "violetred1"
                      :background "thistle2"
                      :height 0.8
                      :weight 'bold)
  (set-face-attribute 'zp/org-agenda-block-warning-face nil :foreground "red" :weight 'bold)

  (zp/org-todo-format-face 'normal 'org-todo-todo "red")
  (zp/org-todo-format-face 'normal 'org-todo-next "DodgerBlue1")
  (zp/org-todo-format-face 'normal 'org-todo-strt "gold3")
  (zp/org-todo-format-face 'normal 'org-todo-done "SpringGreen3")
  (zp/org-todo-format-face 'normal 'org-todo-stby "SkyBlue4")
  (zp/org-todo-format-face 'normal 'org-todo-wait "Skyblue4")
  (zp/org-todo-format-face 'normal 'org-todo-cxld "turquoise")

  (zp/org-format-face 'org-priority-face-a :foreground "white" :background "red")
  (zp/org-format-face 'org-priority-face-b :foreground "red")
  (zp/org-format-face 'org-priority-face-c :foreground "gold3")
  (zp/org-format-face 'org-priority-face-d :foreground "ForestGreen")
  (zp/org-format-face 'org-priority-face-e :foreground "RoyalBlue")

  (zp/org-format-face 'org-tag-location  :weight 'bold :foreground "BlueViolet")
  (zp/org-format-face 'org-tag-todo      :weight 'bold :foreground "Skyblue1")
  (zp/org-format-face 'org-tag-important :weight 'bold :foreground "red")
  (zp/org-format-face 'org-tag-curios   :weight 'bold :foreground "DeepPink")
  (zp/org-format-face 'org-tag-french    :weight 'bold :foreground "DodgerBlue1")

  (zp/org-format-face 'magit-tag :foreground "SpringGreen4")

  (zp/org-super-agenda-update-face)

  (zp/mode-line-theme "light")
  (zp/pdf-view-midnight-mode-theme))

(defun zp/pdf-view-update-midnight ()
  "Update pdf-view’s colour theme."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'pdf-view-mode)
        (pdf-view-midnight-minor-mode)))))

(defun zp/switch-theme ()
  (interactive)
  (cond ((string= zp/emacs-theme "dark")
         (zp/light-theme))
        ((string= zp/emacs-theme "light")
         (zp/dark-theme)))
  (zp/pdf-view-update-midnight))

;;----------------------------------------------------------------------------
;; Day/night cycle
;;----------------------------------------------------------------------------
;; TODO: I could probably improve the time-comparison functions
(defvar zp/time-of-day-sections nil
  "List of time specifications STR to split the day into sections.
The sections are inferred based on when they begin, and the time
specifications are processed in order:
- Pre-day
- Day
- Pre-evening
- Evening
- Night")

;;; Parsing time-of-day data
(defun zp/encode-time-of-day (TIME-STR CURRENT-DAY-DECODED NEXT-DAY-DECODED)
  "Encode time specification from STR to (HIGH LOW)."
  (let* ((time-string TIME-STR)
         (current-day-decoded CURRENT-DAY-DECODED)
         (next-day-decoded NEXT-DAY-DECODED))
    ;; The output of ‘parse-time-string’ cannot be encoded by
    ;; ‘encode-time’ because of missing data, so we’re using either
    ;; today’s to fill in the blanks. If ‘time-string’ is between 12am
    ;; and 1am, use tomorrow’s data instead.
    (apply 'encode-time
           (append
            (subseq (parse-time-string time-string) 0 3)
            (subseq (if (string-match "^0*:" time-string)
                        next-day-decoded
                      current-day-decoded)
                    3)))))

(defvar zp/time-of-day-sections-parsed nil
  "List of time specifications (HIGH LOW) to split the day into sections.
Parsed by zp/parse-time-of-day-sections.
See ‘zp/time-of-day-sections’ for more info.")

(defun zp/parse-time-of-day-sections ()
  "Parse time-of-day sections in ‘zp/time-of-day-sections’.
Each string is replaced by the corresponding list of
integers (HIGH LOW) used by Emacs to compute time. See
‘current-time’ for more information on (HIGH LOW).

A new time specification ‘next-day’ is computed from ‘day’ and
appended to the list to handle next-day timers."
  (let* ((now                           (current-time))
         (now-decoded                   (decode-time now))
         (tomorrow                      (time-add now (* 24 60 60)))
         (tomorrow-decoded              (decode-time tomorrow))
         (after-tomorrow                (time-add tomorrow (* 24 60 60)))
         (after-tomorrow-decoded        (decode-time after-tomorrow))
         (tod-sections                  zp/time-of-day-sections)
         (tod-day                       (nth 1 tod-sections)))
    (setq zp/time-of-day-sections-parsed
          (mapcar (lambda (arg)
                    (zp/encode-time-of-day arg
                                           now-decoded
                                           tomorrow-decoded))
                  tod-sections))
    ;; Add next-day to list
    (add-to-list 'zp/time-of-day-sections-parsed
                 (zp/encode-time-of-day tod-day
                                        tomorrow-decoded
                                        after-tomorrow-decoded)
                 t)))

;;; Status
(defun zp/daytimep ()
  "Return t if it’s day-time.
Based on ‘zp/time-of-day-sections’. A time-of-day is considered
as day-time if it’s between pre-day and pre-evening.
See ‘zp/time-of-day-sections’ for more info."
  (let* ((tod-sections zp/time-of-day-sections-parsed)
         (now           (current-time))
         (pre-day       (nth 0 tod-sections))
         (pre-evening   (nth 2 tod-sections)))
    (if (and (time-less-p pre-day now)
             (time-less-p now pre-evening))
        t
      nil)))

;;----------------------------------------------------------------------------
;; Switching
;;----------------------------------------------------------------------------
(defun zp/switch-theme-dwim (&optional print-message)
  "Switch theme based on time-of-day.
See ‘zp/time-of-day-sections’ and ‘zp/daytimep’ for more info."
  (interactive "p")
  (let* ((daytime (zp/daytimep)))
    (cond ((and daytime
                (or (string= zp/emacs-theme "dark")
                    (not zp/emacs-theme)))
           (zp/light-theme))
          ((and (not daytime)
                (or (string= zp/emacs-theme "light")
                    (not zp/emacs-theme)))
           (zp/dark-theme))
          (t
           (when print-message
             (message "Nothing to do."))))
    (zp/pdf-view-update-midnight)))

(defun zp/switch-theme-auto ()
  "Automatically switch theme based on time-of-day.
See ‘zp/time-of-day-sections’ and ‘zp/daytimep’ for more info."
  (zp/parse-time-of-day-sections)
  (zp/switch-theme-dwim)
  (zp/set-daytime-timer))

;;----------------------------------------------------------------------------
;; Timer
;;----------------------------------------------------------------------------
(defvar zp/daytime-timer nil
  "Timer before next daytime event.")

(defun zp/set-daytime-timer ()
  "Set timer for switching theme at ‘day’ and ‘evening’.
See ‘zp/time-of-day-sections’"
  (let* ((tod-sections  zp/time-of-day-sections-parsed)
         (now           (current-time))
         (day           (nth 1 tod-sections))
         (evening       (nth 3 tod-sections))
         (next-day      (nth 5 tod-sections)))
    (unless (not zp/daytime-timer)
          (cancel-timer zp/daytime-timer)
          (setq zp/daytime-timer nil))
    (setq zp/daytime-timer
          (run-at-time (cl-some (lambda (x)
                                  (when (time-less-p now x)
                                    x))
                                (list day evening next-day))
                       nil #'zp/switch-theme-auto))))

(provide 'theme)
;;; theme.el ends here
