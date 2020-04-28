;;; theme.el --- Custom theme with day/night cycle  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Helper functions
;;----------------------------------------------------------------------------
(defun zp/org-todo-format-face (type face colour)
    (cond ((string= type "box")
           (set-face-attribute face nil
                               :box '(:line-width -3 :style released-button)
                               :height 0.8
                               :weight 'bold
                               :foreground "white"
                               :background colour))
          ((string= type "normal")
           (set-face-attribute face nil
                               :box nil
                               :height 0.8
                               :background nil
                               :weight 'bold
                               :foreground colour))))

;;----------------------------------------------------------------------------
;; Theme definition
;;----------------------------------------------------------------------------
(defvar zp/emacs-theme nil
  "Theme currently used by Emacs.")

(defun zp/emacs-dark-theme ()
  (interactive)
  (setq zp/emacs-theme "dark")
  (load-theme 'base16-atelier-sulphurpool t)

  (set-face-attribute 'default nil :foreground "#BCAF8E" :background "#141414")
  (set-face-attribute 'cursor nil :background "#c94922")
  (when (bound-and-true-p beacon-color)
    (setq beacon-color "#c94922"))
  (set-face-attribute 'org-todo nil :foreground "darkred")
  (set-face-attribute 'org-done nil :foreground "spring green")
  (set-face-attribute 'org-scheduled nil :foreground "#198844")
  (set-face-attribute 'org-scheduled-today nil :foreground "CadetBlue")
  (set-face-attribute 'org-link nil :underline t)
  (with-eval-after-load 'org-roam
    (set-face-attribute 'org-roam-link nil :foreground "#9449ff")
    (set-face-attribute 'org-roam-link-current nil :foreground "#ae005c"))
  (set-face-attribute 'org-hide nil :foreground "#141414")
  (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSlateBlue")
  (set-face-attribute 'region nil :background "RoyalBlue4")
  (set-face-attribute 'helm-selection nil :background "RoyalBlue4") ;Darker Royal Blue
  (set-face-attribute 'org-agenda-clocking nil :background "RoyalBlue4")
  (set-face-attribute 'fringe nil :background "gray10" :foreground "orangered")
  (set-face-attribute 'vertical-border nil :foreground "RoyalBlue1")
  (set-face-attribute 'hl-line nil :background "#1f1f3f")
  (set-face-attribute 'org-level-4 nil :foreground "#ed3971")
  (set-face-attribute 'org-meta-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'header-line nil :foreground "#777")
  (set-face-attribute 'line-number nil :foreground "#969996" :background "#2d2d2d")
  (set-face-attribute 'secondary-selection nil :background "#3B3273")
  (set-face-attribute 'org-column nil :background "#1F1F1F")
  (set-face-attribute 'org-block-begin-line nil :foreground "DodgerBlue3")
  ;; (set-face-attribute 'org-block nil :foreground nil :background nil)
  (set-face-attribute 'org-block-end-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'org-drawer nil :foreground "#4790C9")

  (set-face-attribute 'org-agenda-structure nil
                      :foreground "DodgerBlue1"
                      :weight 'bold)
  (set-face-attribute 'org-agenda-date nil
                      :foreground "#3971ed")
  (set-face-attribute 'org-agenda-date-today nil
                      :foreground "#2aa198"
                      :slant 'normal)
  (set-face-attribute 'org-agenda-date-weekend nil
                      :foreground "#5b39ed"
                      :slant 'normal
                      :weight 'normal)

  (with-eval-after-load "diff-hl"
    (set-face-attribute 'diff-hl-change nil :foreground "#6679cc")
    (set-face-attribute 'diff-hl-insert nil :foreground "#ac9739")
    (set-face-attribute 'diff-hl-delete nil :foreground "#c94922"))

  (with-eval-after-load "zp-org-agenda"
    (set-face-attribute 'zp/org-agenda-block-info-face nil
                        :foreground "violetred1"
                        :background "violetred4"
                        :height 0.8
                        :weight 'bold)
    (set-face-attribute 'zp/org-agenda-block-warning-face nil
                        :foreground "red"
                        :weight 'bold))

  (zp/org-todo-format-face 'normal 'org-todo-todo "darkred")
  (zp/org-todo-format-face 'normal 'org-todo-next "DodgerBlue1")
  (zp/org-todo-format-face 'normal 'org-todo-strt "gold3")
  (zp/org-todo-format-face 'normal 'org-todo-done "SpringGreen3")
  (zp/org-todo-format-face 'normal 'org-todo-stby "SkyBlue4")
  (zp/org-todo-format-face 'normal 'org-todo-wait "Skyblue4")
  (zp/org-todo-format-face 'normal 'org-todo-cxld "turquoise")

  (set-face-attribute 'org-priority-face-a nil :foreground "purple")
  (set-face-attribute 'org-priority-face-b nil :foreground "darkred")
  (set-face-attribute 'org-priority-face-c nil :foreground "yellow")
  (set-face-attribute 'org-priority-face-d nil :foreground "ForestGreen")
  (set-face-attribute 'org-priority-face-e nil :foreground "RoyalBlue")

  (set-face-attribute 'org-tag-context nil  :weight 'bold :foreground "BlueViolet")
  (set-face-attribute 'org-tag-special nil :weight 'bold :foreground "SpringGreen4")
  (set-face-attribute 'org-tag-standby nil :weight 'bold :foreground "Skyblue4")
  (set-face-attribute 'org-tag-important nil :weight 'bold :foreground "darkred")
  (set-face-attribute 'org-tag-curios nil   :weight 'bold :foreground "DeepPink")
  (set-face-attribute 'org-tag-french nil    :weight 'bold :foreground "DodgerBlue1")

  (with-eval-after-load "magit"
    (set-face-attribute 'magit-tag nil :foreground "SpringGreen4"))

  (with-eval-after-load "highlight-indent-guides"
    (highlight-indent-guides-auto-set-faces))

  (zp/org-super-agenda-update-face)

  (zp/mode-line-dark-theme)
  (zp/pdf-view-midnight-mode-theme))

(defun zp/emacs-light-theme ()
  (interactive)
  (setq zp/emacs-theme "light")
  (load-theme 'base16-google-light t)

  (set-face-attribute 'default nil :foreground "#3c3836" :background "#fbf1c7")
  (set-face-attribute 'cursor nil :background "#ff4136")
  (when (bound-and-true-p beacon-color)
    (setq beacon-color "#ff4136"))
  (set-face-attribute 'fringe nil :background "#e6deb8" :foreground "orangered")
  (set-face-attribute 'org-hide nil :foreground "#fbf1c7")
  (set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "LightSlateBlue")
  (set-face-attribute 'org-scheduled-today nil :foreground "DodgerBlue4")
  (set-face-attribute 'region nil :background "SkyBlue1")
  (set-face-attribute 'hl-line nil :background "#fff989")
  (set-face-attribute 'org-level-4 nil :foreground "#ed3971")
  (set-face-attribute 'org-link nil :underline t)
  (with-eval-after-load 'org-roam
    (set-face-attribute 'org-roam-link nil :foreground "#9449ff")
    (set-face-attribute 'org-roam-link-current nil :foreground "#ae005c"))
  (set-face-attribute 'org-agenda-clocking nil :background "LightBlue2")
  (set-face-attribute 'org-meta-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'helm-selection nil :background "SteelBlue1")
  (set-face-attribute 'helm-visible-mark nil :background "goldenrod1")
  (set-face-attribute 'header-line nil :foreground "#666")
  (set-face-attribute 'line-number nil :foreground "#636663" :background "#d4cdaa")
  ;; (set-face-attribute 'line-number-current-line nil :foreground "#707370" :background "#ccc6a4")
  (set-face-attribute 'secondary-selection nil :background "#d3ccff")
  (set-face-attribute 'org-column nil :background "#F0E4BE")
  (set-face-attribute 'org-block-begin-line nil :foreground "DodgerBlue3")
  ;; (set-face-attribute 'org-block nil :foreground nil :background nil)
  (set-face-attribute 'org-block-end-line nil :foreground "DodgerBlue3")
  (set-face-attribute 'org-drawer nil :foreground "#4790C9")

  (set-face-attribute 'org-agenda-structure nil
                      :foreground "DodgerBlue1"
                      :weight 'bold)
  (set-face-attribute 'org-agenda-date nil
                      :foreground "#3971ed")
  (set-face-attribute 'org-agenda-date-today nil
                      :foreground "#2aa198"
                      :slant 'normal)
  (set-face-attribute 'org-agenda-date-weekend nil
                      :foreground "#5b39ed"
                      :slant 'normal
                      :weight 'normal)

  (with-eval-after-load "diff-hl"
    (set-face-attribute 'diff-hl-change nil :foreground "#3a81c3" :background "#afcce7")
    (set-face-attribute 'diff-hl-insert nil :foreground "#7ccd7c" :background "#b3e2b3")
    (set-face-attribute 'diff-hl-delete nil :foreground "#ee6363" :background "#f6a8a8"))

  (with-eval-after-load "zp-org-agenda"
    (set-face-attribute 'zp/org-agenda-block-info-face nil
                        :foreground "violetred1"
                        :background "thistle2"
                        :height 0.8
                        :weight 'bold)
    (set-face-attribute 'zp/org-agenda-block-warning-face nil
                        :foreground "red"
                        :weight 'bold))

  (zp/org-todo-format-face 'normal 'org-todo-todo "red")
  (zp/org-todo-format-face 'normal 'org-todo-next "DodgerBlue1")
  (zp/org-todo-format-face 'normal 'org-todo-strt "gold3")
  (zp/org-todo-format-face 'normal 'org-todo-done "SpringGreen3")
  (zp/org-todo-format-face 'normal 'org-todo-stby "SkyBlue4")
  (zp/org-todo-format-face 'normal 'org-todo-wait "Skyblue4")
  (zp/org-todo-format-face 'normal 'org-todo-cxld "turquoise")

  (set-face-attribute 'org-priority-face-a nil :foreground "purple")
  (set-face-attribute 'org-priority-face-b nil :foreground "red")
  (set-face-attribute 'org-priority-face-c nil :foreground "gold3")
  (set-face-attribute 'org-priority-face-d nil :foreground "ForestGreen")
  (set-face-attribute 'org-priority-face-e nil :foreground "RoyalBlue")

  (set-face-attribute 'org-tag-context nil :weight 'bold :foreground "BlueViolet")
  (set-face-attribute 'org-tag-special nil :weight 'bold :foreground "SpringGreen4")
  (set-face-attribute 'org-tag-standby nil :weight 'bold :foreground "SkyBlue4")
  (set-face-attribute 'org-tag-important nil :weight 'bold :foreground "red")
  (set-face-attribute 'org-tag-curios nil :weight 'bold :foreground "DeepPink")
  (set-face-attribute 'org-tag-french nil :weight 'bold :foreground "DodgerBlue1")

  (with-eval-after-load "magit"
    (set-face-attribute 'magit-tag nil :foreground "SpringGreen4"))

  (with-eval-after-load "highlight-indent-guides"
    (highlight-indent-guides-auto-set-faces))

  (zp/org-super-agenda-update-face)

  (zp/mode-line-light-theme)
  (zp/pdf-view-midnight-mode-theme))

;;----------------------------------------------------------------------------
;; Mode-line definition
;;----------------------------------------------------------------------------

(defvar zp/mode-line-theme nil
  "Theme currently used by the mode-line")

(defun zp/mode-line-dark-theme ()
  (set-face-attribute 'mode-line nil
                      :background "#293233"
                      :foreground "#bfe9bd"
                      :weight 'bold)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#1d1d1d"
                      :foreground "#666"
                      :weight 'bold)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "DarkGoldenrod2"
                      :weight 'bold)
  (with-eval-after-load 'moody
    (set-face-attribute 'mode-line-buffer-id-inactive nil
                        :foreground "#888"
                        :weight 'bold)))

(defun zp/mode-line-light-theme ()
  (set-face-attribute 'mode-line nil
                      :background "#d4c692"
                      :foreground "#333"
                      :weight 'bold)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#c7bf9e"
                      :foreground "#666"
                      :weight 'bold)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "#3650ff"
                      :weight 'bold)
  (with-eval-after-load 'moody
    (set-face-attribute 'mode-line-buffer-id-inactive nil
                        :foreground "#948e76"
                        :weight 'bold)))

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
;; pdf-view-midnight-mode
;;----------------------------------------------------------------------------
(defun zp/pdf-view-midnight-mode-theme ()
  (setq pdf-view-midnight-colors
        `(,(face-attribute 'default :foreground) .
          ,(face-attribute 'default :background))))

(defun zp/pdf-view-update-midnight-mode ()
  "Update pdf-view’s colour theme."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'pdf-view-mode)
                 (buffer-file-name buffer))
        (pdf-view-midnight-minor-mode)))))

;;----------------------------------------------------------------------------
;; Switching
;;----------------------------------------------------------------------------
(defun zp/switch-emacs-theme ()
  (interactive)
  (cond ((string= zp/emacs-theme "dark")
         (zp/emacs-light-theme))
        ((string= zp/emacs-theme "light")
         (zp/emacs-dark-theme)))
  (zp/pdf-view-update-midnight-mode))

(defun zp/switch-mode-line-theme ()
  (interactive)
  (cond ((string= zp/mode-line-theme "dark")
         (zp/mode-line-light-theme))
        ((string= zp/mode-line-theme "light")
         (zp/mode-line-dark-theme))))

(defun zp/switch-theme-dwim (&optional print-message)
  "Switch theme based on time-of-day.
See ‘zp/time-of-day-sections’ and ‘zp/daytimep’ for more info."
  (interactive "p")
  (let* ((daytime (zp/daytimep)))
    (cond ((and daytime
                (or (string= zp/emacs-theme "dark")
                    (not zp/emacs-theme)))
           (zp/emacs-light-theme))
          ((and (not daytime)
                (or (string= zp/emacs-theme "light")
                    (not zp/emacs-theme)))
           (zp/emacs-dark-theme))
          (t
           (when print-message
             (message "Nothing to do."))))
    (zp/pdf-view-update-midnight-mode)))

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

;;--------------
;; Line spacing
;;--------------

(setq-default line-spacing nil)

(defvar zp/line-spacing line-spacing
  "Default line-spacing.")

(defvar zp/line-spacing-variable nil
  "Default line-spacing for variable-pitch-mode.")

(defvar-local zp/variable-pitch-mode-toggle nil
  "State of customised variable-pitch-mode.")

(defun zp/update-line-spacing ()
  "Update line-spacing based on font-preset and mode.
Act on all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if zp/variable-pitch-mode-toggle
          (setq line-spacing zp/line-spacing-variable)
        (setq line-spacing zp/line-spacing)
        (setq-default line-spacing zp/line-spacing)))))

;; Wrapper for ‘variable-pitch-mode’
;; TODO: Use hook rather than a wrapper
(defun zp/variable-pitch-mode ()
  "Enable variable-pitch-mode and changes line-spacing."
  (interactive)
  (cond (zp/variable-pitch-mode-toggle
         (variable-pitch-mode)
         (setq zp/variable-pitch-mode-toggle nil))
        (t
         (variable-pitch-mode)
         (setq zp/variable-pitch-mode-toggle 1)))
  (zp/update-line-spacing))

;;--------------
;; Font setting
;;--------------

(defvar zp/current-font nil
  "Name of the current default font-preset.")

(defvar zp/current-font-variable nil
  "Name of the current variable font-preset.")

(defun zp/set-font (font)
  "Change default font.
FONT is a preset."
  (let ((current zp/current-font))
    (unless (equal current font)
      (pcase font
        ("sarasa"
         (set-face-attribute 'default nil
                             :font "Sarasa Term Prog J" :height 113)
         (setq zp/line-spacing nil))
        ("operator"
         (set-face-attribute 'default nil
                             :font "Operator Mono Prog" :height 122)
         (setq zp/line-spacing 0.1))
        ("gintronic"
         (set-face-attribute 'default nil
                             :font "Gintronic Prog" :height 113)
         (setq zp/line-spacing 0.1)))
      (setq zp/current-font font)
      (zp/update-line-spacing)
      (message (concat "Font switched to " (capitalize font))))))

(defun zp/set-font-variable (font)
  "Change variable font.
FONT is a preset."
  (let ((current zp/current-font-variable))
    (unless (equal current font)
      (pcase font
        ("equity"
         (set-face-attribute 'variable-pitch nil
                             :font "Equity Text A" :height 187)
         (setq zp/line-spacing-variable 0.2))
        ("guyot"
         (set-face-attribute 'variable-pitch nil
                             :font "Guyot Text" :height 187)
         (setq zp/line-spacing-variable 0.5))
        ("bliss"
         (set-face-attribute 'variable-pitch nil
                             :font "Bliss Pro Prog" :height 195)
         (setq zp/line-spacing-variable 0.2))
        ("typewriter"
         (set-face-attribute 'variable-pitch nil
                             :font "ITC American Typewriter Std" :height 158)
         (setq zp/line-spacing-variable 0.3)))
      (setq zp/current-font-variable font)
      (zp/update-line-spacing)
      (message (concat "Variable font switched to " (capitalize font))))))

;;---------------
;; Font toggling
;;---------------

(defvar zp/current-font-variable nil
  "Name of the current variable font-preset.")

(defvar zp/list-fonts nil
  "List of default font-presets.")

(defvar zp/list-fonts-variable nil
  "List of variable font-presets.")

(setq zp/list-fonts '("sarasa" "operator" "gintronic"))
(setq zp/list-fonts-variable '("equity" "guyot" "bliss" "typewriter"))

(defun zp/toggle-font (type current list)
  "Toggle between font-presets.

TYPE is the type of fonts to toggle.
CURRENT is the variable holding the current font-preset.
LIST is the variable holding the list of font-presets."
  (interactive)
  (let* ((current current)
         (list list)
         (next-p (car (cdr (member current list))))
         (next (if next-p next-p (car list))))
    (pcase type
      ("default" (zp/set-font next))
      ("variable" (zp/set-font-variable next)))
    (zp/update-line-spacing)))

(defun zp/toggle-font-default ()
  "Toggle between default font-presets.
CURRENT is the variable holding the current default font-preset.
LIST is the variable holding the list of default font-presets."
  (interactive)
  (zp/toggle-font "default" zp/current-font zp/list-fonts))

(defun zp/toggle-font-variable ()
  "Toggle between default font-presets.
CURRENT is the variable holding the current variable font-preset.
LIST is the variable holding the list of variable font-presets."
  (interactive)
  (zp/toggle-font "variable" zp/current-font-variable zp/list-fonts-variable))

;; Select font with Helm
(defun zp/helm-select-font (&optional font)
  "Select the font-preset to use."
  (interactive)
  (let ((current zp/current-font))
    (helm :sources '((name . "*HELM - Font selection*")
                     (candidates . zp/list-fonts)
                     (action . (("Change font" . zp/set-font))))
          :preselect current)))

(defun zp/helm-select-font-variable (&optional font)
  "Select the variable font-preset to use."
  (interactive)
  (let ((current zp/current-font-variable))
    (helm :sources '((name . "*HELM - Font selection*")
                     (candidates . zp/list-fonts-variable)
                     (action . (("Change font" . zp/set-font-variable))))
          :preselect current)))

(defun zp/helm-select-font-dwim ()
  "Select the font-preset to use.

If in variable-pitch-mode, change the variable font-preset."
  (interactive)
  (if zp/variable-pitch-mode-toggle
      (zp/helm-select-font-variable)
    (zp/helm-select-font)))

(provide 'theme)
;;; theme.el ends here
