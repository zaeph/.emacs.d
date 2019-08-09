;;; zp-org-agenda.el --- Custom config for org-agenda -*- fill-column: 78; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Require other packages
;;----------------------------------------------------------------------------
(require 'org-agenda)

;;----------------------------------------------------------------------------
;; Debugging
;;----------------------------------------------------------------------------
(setq zp/org-agenda-skip-functions-debug t)
(setq zp/org-agenda-skip-functions-debug nil)

;;----------------------------------------------------------------------------
;; Initialise local config
;;----------------------------------------------------------------------------
(defun zp/org-agenda-get-key ()
  "Return the key of the current org-agenda view."
  (unless (derived-mode-p 'org-agenda-mode)
    (error "Not in an agenda"))
  (let ((name (buffer-name))
        (regex "\\*Org Agenda(\\(.*\\))\\*"))
    (save-match-data
      (string-match regex name)
      (match-string 1 name))))

(defvar zp/org-agenda-local-settings nil
  "Settings to use for local agenda views.")

(defvar zp/org-agenda-load-local-config-post-hook nil
  "Hooks to run after the local org-agenda config has been
  loaded.")

(defun zp/org-agenda-local-config-init (list)
  "Create the data structure for org-agenda local config.

This function takes every variables in
‘zp/org-agenda-local-settings’ and store them with their value in
a data structure, thus defining the global state of those
variables."
  (let ((settings))
    (while (cdr list)
      (let ((var (pop list)))
        (push var settings)
        (set var (eval (pop list)))))
    (list (cons 'default (list (mapcar (lambda (setting)
                                         (cons setting (eval setting)))
                                       settings))))))

;;----------------------------------------------------------------------------
;; Category icons
;;----------------------------------------------------------------------------
(defvar zp/org-agenda-include-category-icons nil
  "When non-nil, show category icons in the agenda")

(defvar zp/org-agenda-category-icon-alist nil
  "Alist of category icon to be displayed in agenda views.

Custom variable to hold the content when the icons are toggled
off.")

;;----------------------------------------------------------------------------
;; Commands
;;----------------------------------------------------------------------------
(defun zp/org-agenda-benchmark (&optional arg)
  "Rebuild the agenda and display the time it took to do so.

With a prefix argument, do so in all agenda buffers."
  (interactive "P")
  (cond ((equal arg '(4))
         (with-timer "Rebuilding all agenda buffers"
           (zp/org-agenda-redo-all)))
        (t
         (with-timer "Rebuilding agenda buffer"
           (org-agenda-redo)))))

(defun zp/update-org-agenda-files ()
  "Initialise ‘org-agenda-files’ and all the shortcuts."
  (interactive)
  (setq org-agenda-files '("~/org/life.org"))
  (zp/set-shortcuts-all))

(zp/update-org-agenda-files)

(defun zp/org-agenda-redo-all ()
  "Redo all the agenda views."
  (interactive)
  (let ((inhibit-message t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo))))))

(run-at-time "06:00" 86400 #'zp/org-agenda-redo-all)

;; Idle timer for rebuilding all the agenda views
;; Disabled for review
;; (run-with-idle-timer 300 t #'zp/org-agenda-redo-all)

(defun zp/org-habit-show-habits-force ()
  "Enable habits in all local agendas, even if they’ve been disabled."
  (interactive)
  (mapcar (lambda (cons)
            (let ((agenda (car cons)))
              (unless (eq agenda 'default)
                (zp/set-agenda-local 'org-habit-show-habits t agenda))))
          zp/org-agenda-local-config))

;; Force habits to be shown if they’ve been disabled the previous day
(run-at-time "06:00" 86400 #'zp/org-habit-show-habits-force)

;; Change face of the arrow for ‘org-agenda-bulk-mark’
(defun org-agenda-bulk-mark (&optional arg)
  "Mark the entry at point for future bulk action."
  (interactive "p")
  (dotimes (i (or arg 1))
    (unless (org-get-at-bol 'org-agenda-diary-link)
      (let* ((m (org-get-at-bol 'org-hd-marker))
             ov)
        (unless (org-agenda-bulk-marked-p)
          (unless m (user-error "Nothing to mark at point"))
          (push m org-agenda-bulk-marked-entries)
          (setq ov (make-overlay (point-at-bol) (+ 2 (point-at-bol))))
          (org-overlay-display ov (concat org-agenda-bulk-mark-char " ")
                               ;; (org-get-todo-face "TODO")
                               'org-todo        ;Modification
                               'evaporate)
          (overlay-put ov 'type 'org-marked-entry-overlay))
        (end-of-line 1)
        (or (ignore-errors
              (goto-char (next-single-property-change (point) 'org-hd-marker)))
            (beginning-of-line 2))
        (while (and (get-char-property (point) 'invisible) (not (eobp)))
          (beginning-of-line 2))
        (message "%d entries marked for bulk action"
                 (length org-agenda-bulk-marked-entries))))))

;;----------------------------------------------------------------------------
;; Group filters
;;----------------------------------------------------------------------------
(defvar zp/org-agenda-groups-property "AGENDA_GROUP"
  "Name of the property used for agenda-groups.")

(defun zp/org-get-agenda-groups (&optional pom)
  "Get agenda-groups from current tree."
  (let* ((pos (or pom
                  (point)))
         (property zp/org-agenda-groups-property)
         (groups (org-entry-get pos property t))
         (category (org-entry-get pos "CATEGORY" t))
         ;; Combine groups and category
         (string (mapconcat #'identity
                            (list groups
                                  category)
                            ", ")))
    (when string
      (delete-dups (split-string string ", ?")))))

(defun zp/org-agenda-groups-is-group-filter-p (object)
  "Return t if OBJECT is a list-formatted org-agenda group-filter.

A list-formatted group-filter should have the following form:
  (inclusion-list exclusion-list)"
  (and (listp object)
       (cl-every #'listp object)
       (eq 2 (length object))
       (cl-every #'stringp (car object))
       (cl-every #'stringp (cadr object))))

(defun zp/org-agenda-groups-is-compound-group-filter-p (object)
  "Return t if OBJECT is a compound org-agenda group-filter.

A compound group-filter should have the following form:
  ((inclusion-list-1 inclusion-list-2 ...) exclusion-list)"
  (and (listp object)
       (cl-every #'listp object)
       (eq 2 (length object))
       (cl-every #'listp (car object))
       (cl-every #'stringp (caar object))
       (cl-every #'stringp (cadr object))))

(defun zp/org-agenda-groups-read-group-filter-string (filter)
  "Read an org-agenda group-FILTER given as a string.

Return a list-formatted FILTER:
  (inclusion-list exclusion-list)

FILTER should be formatted as \"+group1-group2\" where:
- \"group1\" is an org-agenda group to be included.
- \"group2\" is an org-agenda group to be excluded."
  (let* ((prefix-re "[\\+-]")
         ;; Handle special case when 1st group is w/o prefix
         (filter (when-let ((match (substring filter 0 1)))
                   (if (string-match prefix-re match)
                       filter
                     (concat "+" filter))))
         (groups (s-slice-at prefix-re filter))
         include
         exclude)
    (dolist (group groups)
      (let ((type (substring group 0 1))
            (group (substring group 1)))
        (push group
              (pcase type
                ("+" include)
                ("-" exclude)))))
    (list include
          exclude)))

(defun zp/org-agenda-groups-format-filter (filter)
  "Format a list-formatted FILTER into a string.

This function creates human-readable filters to be used in UI
elements."
  (let* ((include (car filter))
         (exclude (cadr filter))
         (format (lambda (symbol list)
                   (mapconcat (lambda (elem)
                                (concat symbol elem))
                              list " ")))
         (include-fmt (when (car include)
                        (funcall format "+" include)))
         (exclude-fmt (when (car exclude)
                        (funcall format "-" exclude)))
         (list (delete nil
                       (list include-fmt
                             exclude-fmt))))
    (concat "{" (mapconcat #'identity
                           list
                           " ")
            "}")))

(defun zp/org-agenda-groups-format-filters (filters)
  "Format a list of list-formatted FILTERS into a string.

For more information, see ‘zp/org-agenda-groups-format-filter’."
  (let ((i 1))
    (mapconcat (lambda (filter)
                 (prog1 (concat "F" (number-to-string i)
                                ":" (zp/org-agenda-groups-format-filter filter))
                   (setq i (1+ i))))
               filters ", ")))

(defun zp/org-agenda-groups-process-filters (filters)
  "Process org-agenda group-FILTERS into a list.

Any member of FILTERS given as a string will first be read with
‘zp/org-agenda-groups-read-group-filter-string’.

Return a list of list-formatted filters.

For more information on list-formatted filters, see
‘zp/org-agenda-groups-is-group-filter-p’."
  (when filters
    (thread-last filters
      ;; Read filters provided as strings
      (mapcar (lambda (filter)
                (cond ((not filter)
                       nil)
                      ((stringp filter)
                       (zp/org-agenda-groups-read-group-filter-string filter))
                      ((zp/org-agenda-groups-is-group-filter-p filter)
                       filter)
                      (t
                       (error "Invalid filter format")))))
      ;; Remove nil filters
      (delete nil))))

(defun zp/org-agenda-groups-create-compound-filter (filters)
  "Process org-agenda group-FILTERS into a compound filter.

For more information on compound filters, see
‘zp/org-agenda-groups-is-compound-group-filter-p’."
  (let (include
        exclude)
    (dolist (filter filters)
      ;; Only push include-filter if non-nil
      (when-let ((this (pop filter)))
        (push this include))
      (when-let ((this (pop filter)))
        (push (car this) exclude)))
    (delete-dups include)
    (delete-dups exclude)
    (list include exclude)))

(defun zp/org-task-in-agenda-groups-p (&rest filters)
  "Test whether a task is in agenda-group matched by FILTERS.

FILTERS can either be provided as string-formatted or
list-formatted filters.

As a special case, return -1 if the task is not part of any
agenda-group."
  (let* ((filters
          (if (zp/org-agenda-groups-is-compound-group-filter-p (car filters))
              (car filters)
            (zp/org-agenda-groups-create-compound-filter
             (zp/org-agenda-groups-process-filters filters))))
         (include (pop filters))
         (exclude (pop filters)))
    (let* ((task-groups (zp/org-get-agenda-groups))
           (test (lambda (list)
                   (if list
                       (cl-some (lambda (group)
                                  (member group list))
                                task-groups)
                     ;; Return t if the filter is nil
                     t))))
      (cond (task-groups
             (let ((matched-pos
                    (and (cl-some #'identity include)
                         ;; Check if all include-filters match
                         (cl-every #'identity
                                   (mapcar (lambda (filter)
                                             (funcall test filter))
                                           include))))
                   (matched-neg
                    (and (cl-some #'identity exclude)
                         (funcall test exclude))))
               (and (or matched-pos
                        ;; Special case: Filter is exclude-only
                        (cl-every 'not include))
                    (not matched-neg))))
            ((cl-some (lambda (filter)
                        (member "nil" filter))
                      include)
             -1)))))

(defun zp/org-agenda-groups-set-extra-filter (arg)
  (interactive "sFilter: ")
  (let* ((filters (if (string= "" arg)
                      nil
                    (split-string arg " ")))
         (processed (zp/org-agenda-groups-process-filters filters)))
    (zp/set-agenda-local 'zp/org-agenda-groups-extra-filters processed)
    (org-agenda-redo-all)
    (message (pcase arg
               ("" "Filter has been reset.")
               (_ (concat "New filter has been set: " arg))))))

;;----------------------------------------------------------------------------
;; Skip functions
;;----------------------------------------------------------------------------
;; Inspired by Bernst Hansen’s helper functions.
;; Source: http://doc.norang.ca/org-mode.html
(defun zp/org-agenda-groups-format-regex (list)
  "Format LIST of agenda groups as a regex"
  (string-join
   (mapcar (lambda (arg)
             (if (not arg)
                 "^$"
               (concat "\\b"
                       arg
                       "\\b")))
           list)
   "\\|"))

(defun zp/skip-tasks-not-in-agenda-groups (filters)
  "Skip tasks which aren’t in an org-agenda group matched by FILTERS.

FILTERS must be a list of either list-formatted or
string-formatted filters.  However, for the sake of efficiency,
the former should be preferred.

For more information on formatting, see
‘zp/org-agenda-groups-is-group-filter-p’ and
‘zp/org-agenda-groups-read-group-filter-string’."
  (when zp/org-agenda-skip-functions-debug
    (message "STNG: %s" (org-entry-get (point) "ITEM")))
  (when filters
    (save-restriction
      (widen)
      (let* (;; Create compound filter
             (compound-filter (zp/org-agenda-groups-create-compound-filter
                               (zp/org-agenda-groups-process-filters filters)))
             (include (apply #'append (car compound-filter)))
             (next-headline (save-excursion
                              (or (outline-next-heading)
                                  (point-max))))
             (groups-regex (zp/org-agenda-groups-format-regex include))
             (property zp/org-agenda-groups-property)
             (properties-regex (concat "^:\\("
                                     property
                                     "\\|CATEGORY\\)"
                                     ":.*")))
        (save-excursion
          (cond
           ((or (not compound-filter)
                (zp/org-task-in-agenda-groups-p compound-filter))
            nil)
           ((catch 'found-next
              (goto-char next-headline)
              (while (re-search-forward
                      (concat properties-regex
                              "\\("
                              groups-regex
                              "\\).*$")
                      nil t)
                (if (zp/org-task-in-agenda-groups-p compound-filter)
                    (throw 'found-next 't))))
            (outline-previous-heading))
           (t
            (goto-char (point-max)))))))))

(defvar zp/org-agenda-groups-extra-filters nil
  "Extra filters to use for filtering org-agenda groups.

This is to allow interactive group-filtering in custom views.")

(defun zp/skip-tasks-not-in-agenda-groups-with-extra-filters (filters)
  "Skip tasks which aren’t in an org-agenda group matched by FILTERS.

This function combines FILTERS with
‘zp/org-agenda-groups-extra-filters’ to allow for interactive
filtering in custom views.

For more information, see ‘zp/skip-tasks-not-in-agenda-groups’."
  (let ((filters (append filters
                         zp/org-agenda-groups-extra-filters)))
    (zp/skip-tasks-not-in-agenda-groups filters)))

(defvar zp/fluid-project-definition t
  "When t, a project with no remaining subtasks become a task.

When nil, a project with no remaining subtasks will be considered
stuck.")

(defun zp/identify-task-type ()
  "Identify the type of the task at point.

- If the task at point has at least one subtask, return 'project.

- If the task at point does not have any subtask, return 'task.

- Return nil if point is not on a task.

When ‘zp/fluid-project-definition’ is non-nil, projects with no
remaining subtasks are considered as tasks.  We say it is ‘fluid’ because
a tree can go back-and-forth between being a task and being a project."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1))
          (fluid zp/fluid-project-definition))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (and (member (org-get-todo-state) org-todo-keywords-1)
                     (if fluid (not (org-entry-is-done-p)) t))
            (setq has-subtask t))))
      (cond ((and is-a-task has-subtask)
             'project)
            (is-a-task
             'task)))))

(defun zp/is-project-p ()
  "Return t if the tree at point is a project."
  (eq (zp/identify-task-type) 'project))

(defun zp/is-task-p ()
  "Return t if the item at point is a task."
  (eq (zp/identify-task-type) 'task))

(defun zp/is-stuck-project-p ()
  "Return t if the project at point is stuck.

A project is considered to be stuck if it any of the following
condition is not true:

- The project (or any of its subprojects) does not have a task
  with a NEXT todo-keyword.  In GTD lingo, this means that the
  project has no ‘next action’.

- The project (or any of its subprojects) does not have a task
  with a STRT todo-keyword.  In GTD lingo, this means that an
  action is underway.

- The project is waiting (i.e. it has a WAIT todo-keyword) but
  none of its subtasks (direct or indirect) is waiting..  This is
  to ensure that a project marked as waiting is actually waiting
  for something."
  (save-restriction
    (widen)
    (when zp/org-agenda-skip-functions-debug
      (message "SNSP: %s" (org-entry-get (point) "ITEM")))
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (is-waiting (string= (nth 2 (org-heading-components)) "WAIT"))
          (has-next))
      (save-excursion
        (forward-line 1)
        (while (and (not has-next)
                    (< (point) subtree-end)
                    (if is-waiting
                        (re-search-forward "^\\*+ \\(WAIT\\) " subtree-end t)
                      (re-search-forward "^\\*+ \\(NEXT\\|STRT\\) " subtree-end t)))
          (setq has-next t)))
      (if has-next
          nil
        t))))

(defun zp/is-waiting-p ()
  "Return t if the item/tree at point is waiting for something."
  (member "waiting" (org-get-tags-at)))

(defvar zp/org-agenda-include-waiting nil
  "When t, include waiting item/trees in the agenda.")

(defun zp/skip-waiting ()
  "Skip items/trees which are waiting for something.

An item or a tree are considered to be waiting for something when they
have the :waiting: tag.

Note that this function only checks the tag of those items/trees.
This means that waiting project will *not* be validated by
checking whether they have a waiting subtask (i.e. with a WAIT
todo-keyword)."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (if (and (not zp/org-agenda-include-waiting)
               (zp/is-waiting-p))
          next-headline
        nil))))

(defun zp/skip-future-non-waiting-timestamped-tasks ()
  "Skip non-waiting tasks with a future timestamp."
  (save-restriction
    (widen)
    (let ((ts (org-entry-get (point) "TIMESTAMP" nil)))
      (if (and ts
               (not (zp/is-waiting-p)))
          (save-excursion
            (org-end-of-subtree))
        nil))))

(defun zp/skip-future-non-waiting-timestamped-tasks-cond ()
  "Cond. skip non-waiting tasks with a future timestamp."
  (when zp/org-agenda-todo-ignore-future
    (zp/skip-future-non-waiting-timestamped-tasks)))

(defun zp/skip-non-projects ()
  "Skip trees which aren’t projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((zp/is-project-p)
        nil)
       ((not (zp/is-task-p))
        nil)
       (t
        subtree-end)))))

(defun zp/skip-non-projects-cond ()
  "Conditionally skip trees which aren’ts projects.

If ‘zp/org-agenda-include-projects’ is nil, skip to the end of
the file."
  (if zp/org-agenda-include-projects
      (zp/skip-non-projects)
    (point-max)))

(defun zp/skip-non-tasks (&optional subtasks)
  "Skip items which aren’t tasks.

When SUBTASKS is non-nil, also skip project subtasks."
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (and subtasks
                                    (org-goto-sibling)
                                    (point))
                               (outline-next-heading)
                               (point-max)))))
      (cond
       ((and subtasks
             (zp/is-subtask-p))
        next-headline)
       ((and (zp/is-task-p)
             (not (org-is-habit-p)))
        nil)
       (t
        next-headline)))))

(defun zp/is-group-head-p ()
  "Return t when the tree at point is the head of an agenda-group."
  (org-entry-get (point) "AGENDA_GROUP"))

(defun zp/is-subtask-p ()
  "Return t when the item at point is a project subtask.

As a special case, if the item is a subtask of an agenda-group’s
head (i.e. it has an ‘AGENDA_GROUP’ property), it will *not* be
considered as a subtask.  This is to avoid inactive
group-heads (i.e. with a STBY todo-keyword) from being considered
as regular projects."
  (save-restriction
    (widen)
    (save-excursion
      (and (zp/is-task-p)
           (org-up-heading-safe)
           (zp/is-project-p)
           (not (zp/is-group-head-p))))))

(defun zp/skip-routine ()
  "Skip items which have a :routine: tag."
  (when (member "routine" (org-get-tags (point) t))
    (org-end-of-subtree)))

(defun zp/skip-routine-cond ()
  "Conditionally skip items which have a :routine: tag."
  (unless zp/org-agenda-include-routine
    (zp/skip-routine)))

;;----------------------------------------------------------------------------
;; Sorting functions
;;----------------------------------------------------------------------------
(defun zp/org-cmp-test-todo (todo a b)
  "Compare the todo states of strings A and B."
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (fa (and ma (marker-buffer ma)))
         (fb (and mb (marker-buffer mb)))
         (todo-kwds
          (or (and fa (with-current-buffer fa org-todo-keywords-1))
              (and fb (with-current-buffer fb org-todo-keywords-1))))
         (ta (or (get-text-property 1 'todo-state a) ""))
         (tb (or (get-text-property 1 'todo-state b) ""))
         (la (- (length (member ta todo-kwds))))
         (lb (- (length (member tb todo-kwds))))
         (donepa (member ta org-done-keywords-for-agenda))
         (donepb (member tb org-done-keywords-for-agenda)))
    (cond ((and (string-match-p ta todo) (not (string-match-p tb todo))) +1)
          ((and (string-match-p tb todo) (not (string-match-p ta todo))) -1))))

(defun zp/org-cmp-todo-special (a b)
  (when zp/org-agenda-sorting-strategy-special-first
    (or (zp/org-cmp-test-todo "STRT" a b)
        (zp/org-cmp-test-todo "NEXT" a b))))

(defun zp/org-agenda-sort-wait (a b)
  (cond
   ((zp/org-cmp-test-todo "WAIT|STBY" a b))))

(defvar zp/org-cmp-time-debug nil
  "When non-nil, print debug messages when running
  ‘zp/org-cmp-time’.")

(defun zp/org-cmp-time (a b property &optional set-created)
  "Sort items by time property.

A and B are the objects to compare (taken from the org-agenda
building function).

When SET-CREATED is non-nil, create the ‘CREATED’ property and
set it to the current time.

PROPERTY can either be:

- the name of the property to match as a string,
  e.g. ‘TIMESTAMP’, ‘SCHEDULED’, ‘DEADLINE’, or any user-defined
  property like ‘CREATED’

- a list containing the names of the properties to match as
  strings"
  (let* ((debug zp/org-cmp-time-debug)
         (properties (when (and (symbolp property)
                                (eq property 'all))
                       '("SCHEDULED" "TIMESTAMP")))
         (a-pos (get-text-property 0 'org-marker a))
         (b-pos (get-text-property 0 'org-marker b))
         (prop property)
         (get-property (lambda (pos)
                         (if properties
                             (let* ((data (mapcar (lambda (prop)
                                                    (org-entry-get pos prop))
                                                  properties))
                                    (scheduled-str (pop data))
                                    (timestamp-str (pop data))
                                    (scheduled (and scheduled-str
                                                    (org-time-string-to-seconds
                                                     scheduled-str)))
                                    (timestamp (and timestamp-str
                                                    (org-time-string-to-seconds
                                                     timestamp-str))))
                               (prog1 (or scheduled
                                          timestamp)
                                 (when debug
                                   (cond (scheduled-str
                                          (message "  Scheduled: %s" scheduled-str))
                                         (timestamp-str
                                          (message "  Timestamp: %s" timestamp-str))
                                         (t
                                          (message "No time info"))))))
                           (when-let ((data (org-entry-get pos prop)))
                             (org-time-string-to-seconds data)))))
         (ta (progn (when debug
                      (message "\nComparing ‘%s’"
                               (org-entry-get a-pos "ITEM")))
                    (funcall get-property a-pos)))
         (tb (progn (when debug
                      (message "With ‘%s’"
                               (org-entry-get b-pos "ITEM")))
                    (funcall get-property b-pos))))
    (when set-created
      (mapc (lambda (pos)
              (unless (org-entry-get pos "CREATED")
                (org-with-point-at pos
                  (zp/org-set-created-property))))
            (list a-pos b-pos)))
    (when-let ((result (cond ((if ta (and tb (< ta tb)) tb) 1)
                             ((if tb (and ta (< tb ta)) ta) -1))))
      (when debug
        (message "Result: %s\n"
                 (pcase result
                   (1  "UP: A goes up")
                   (-1 "DN:A goes down")
                   (_  "EQ: A and B are equal"))))
      result)))

(defun zp/org-cmp-created (a b)
  (zp/org-cmp-time a b "CREATED" t))

(defun zp/org-cmp-scheduled (a b)
  (zp/org-cmp-time a b "SCHEDULED"))

(defun zp/org-cmp-timestamp (a b)
  (zp/org-cmp-time a b "TIMESTAMP"))

(defun zp/org-cmp-time-all (a b)
  "Sort objects according to their time data.

The objects will be sorted in that order:

- Earlier ‘SCHEDULED’ or ‘TIMESTAMP’ first.

- If ‘SCHEDULED’ and ‘TIMESTAMP’, favour ‘SCHEDULED’."
  (zp/org-cmp-time a b 'all))

(defun zp/org-cmp-created-dwim (a b)
  "Sort items by creation time, priority and specialness conditionally.

If ‘zp/org-agenda-sorting-strategy-special-first’ is non-nil,
first sort by specialness.

This function also checks for priority because only one
‘org-agenda-cmp-user-defined’ can be specified at a time.  When
sorting with this function, make sure not to use use ‘priority’
afterwards."
  (let ((reverse zp/org-agenda-sort-by-rev-fifo))
    (or (zp/org-cmp-time-all (if reverse b a)
                             (if reverse a b))
        (when zp/org-agenda-sorting-strategy-special-first
          (zp/org-cmp-todo-special a b))
        (unless reverse
          (org-cmp-values a b 'priority))
        (zp/org-cmp-created (if reverse b a)
                            (if reverse a b)))))

;;----------------------------------------------------------------------------
;; Headers
;;----------------------------------------------------------------------------
(defun zp/org-agenda-format-header-align (header)
  (let ((tags-column org-agenda-tags-column))
    (format
     (concat
      "%"
      (number-to-string
       (/ (+ (- tags-column) (length header)) 2))
      "s")
     header)))

(defun zp/org-agenda-format-word-list (word-list)
  (let ((word-list-linked (s-join ";" word-list)))
    (if (not (eq word-list nil))
        (concat " " "(" word-list-linked ")"))))

(defun zp/org-agenda-format-header-main (header)
  "Format the main header block in org-agenda."
  (let* ((tags-column org-agenda-tags-column)
         (flanking-symbol "~")
         (header-formatted
          (zp/org-agenda-format-header-align
           (concat flanking-symbol " " header " " flanking-symbol)))
         (extra-filters zp/org-agenda-groups-extra-filters)
         word-list)
    (unless org-agenda-include-deadlines
      (add-to-list 'word-list "-deadlines" t))
    (unless org-agenda-show-all-dates
      (add-to-list 'word-list "-empty" t))
    (unless org-habit-show-habits
      (add-to-list 'word-list "-habits" t))
    (unless zp/org-agenda-include-category-icons
      (add-to-list 'word-list "-icons" t))
    (unless zp/org-agenda-include-projects
      (add-to-list 'word-list "-projects" t))
    (unless zp/org-agenda-include-routine
      (add-to-list 'word-list "-routine" t))
    (unless zp/org-agenda-include-scheduled
      (add-to-list 'word-list "-scheduled" t))
    (let ((word-list-formatted (s-join ";" word-list)))
      (if (not (eq word-list nil))
          (setq word-list-formatted (concat " " "(" word-list-formatted ")")))
      (concat
       header-formatted word-list-formatted "\n"
       (when extra-filters
         (concat "Extra filters: "
                 (zp/org-agenda-groups-format-filters extra-filters)
                 "\n"))))))

(defun zp/org-agenda-format-header-block (header)
  "Format header blocks in org-agenda."
  (let* ((tags-column org-agenda-tags-column)
         (header-formatted (zp/org-agenda-format-header-align header)))
    (concat
     header-formatted "\n")))

(defun zp/org-agenda-format-header-block-with-settings (header)
  "Format header blocks in org-agenda, and display important
agenda settings after them."
  (let ((word-list ()))
    (when zp/org-agenda-sorting-strategy-special-first
      (add-to-list 'word-list "+S↓" t))
    ;; (if (eq org-agenda-dim-blocked-tasks nil)
    ;;     (add-to-list 'word-list "-dimmed" t))
    (when zp/org-agenda-todo-ignore-future
      (add-to-list 'word-list "-future" t))
    (when zp/org-agenda-sort-by-rev-fifo
      (add-to-list 'word-list "+rev-fifo" t))
    (unless zp/org-agenda-include-routine
      (add-to-list 'word-list "-routine" t))
    (when zp/org-agenda-split-subtasks
      (add-to-list 'word-list "+split" t))
    (unless zp/org-agenda-include-waiting
      (add-to-list 'word-list "-waiting" t))
    (let ((header-formatted (zp/org-agenda-format-header-align header))
          (word-list-formatted (zp/org-agenda-format-word-list word-list)))
      (concat header-formatted word-list-formatted))))

;; Special blocks
(defun zp/org-agenda-format-header-projects ()
  "Format header blocks in org-agenda, and display important
agenda settings after them."
  (let* ((tags-column org-agenda-tags-column)
         (header "Projects")
         (word-list ()))
    (when zp/org-agenda-sorting-strategy-special-first
      (add-to-list 'word-list "+S↓" t))
    (when (eq zp/org-agenda-include-waiting nil)
      (add-to-list 'word-list "-waiting" t))
    (let ((header-formatted (zp/org-agenda-format-header-align header))
          (word-list-formatted (zp/org-agenda-format-word-list word-list)))
      (concat header-formatted word-list-formatted))))

;;----------------------------------------------------------------------------
;; Show modes in headers
;;----------------------------------------------------------------------------
(defface zp/org-agenda-block-info-face nil
  "Info for blocked faces in org-agenda.")

(defface zp/org-agenda-block-warning-face nil
  "Warning for blocked faces in org-agenda.")

(defun zp/org-agenda-hi-lock ()
  (highlight-regexp "([-+].*?)" 'zp/org-agenda-block-info-face)
  (highlight-regexp "F.:{.*?}" 'zp/org-agenda-block-info-face)
  ;; (highlight-regexp "^[[:space:]]*? \\[ Stuck Projects \\]" 'zp/org-agenda-block-warning-face)
  (highlight-regexp "^~~.*~~$" 'font-lock-comment-face))

;;----------------------------------------------------------------------------
;; Blocks
;;----------------------------------------------------------------------------
(defun zp/org-agenda-block-agenda-main (header &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-span 'day)
            (org-agenda-skip-function
             '(or (zp/skip-tasks-not-in-agenda-groups-with-extra-filters nil)
                  (zp/skip-routine-cond)))
            (org-super-agenda-groups
             '((:name "Grid"
                      :time-grid t)
               ,@(zp/org-super-agenda-groups-all))))))

(defun zp/org-agenda-block-agenda (header &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-skip-function
             '(zp/skip-routine-cond))
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-header (header)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            (org-agenda-files nil)
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-header-with-deadlines (header filters &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if file
                  `((org-agenda-files ',file)))
            (org-agenda-entry-types
             '(:deadline))
            (zp/org-agenda-groups-filters
             (zp/set-agenda-local 'zp/org-agenda-groups-filters
                                  ',(copy-tree filters)))
            (org-agenda-skip-function
             '(zp/skip-tasks-not-in-agenda-groups-with-extra-filters
               ',filters))
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-agenda-with-group-filter (header filters &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-skip-function
             '(or (zp/skip-tasks-not-in-agenda-groups
                   ',filters)
                  (zp/skip-routine-cond)))
            (org-agenda-span 'day))))

(defun zp/org-agenda-block-agenda-week-with-group-filter (header filters &optional file)
  `(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            ,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-span 'week)
            (org-habit-show-habits nil)
            (org-agenda-skip-function
             '(or (zp/skip-tasks-not-in-agenda-groups-with-extra-filters
                   ',filters)
                  (zp/skip-routine-cond)))
            (org-agenda-dim-blocked-tasks 'dimmed)
            (org-deadline-warning-days 0))))

(defun zp/org-agenda-block-agenda-timestamps-and-deadlines (header &optional file)
  `(agenda ""
           (,@(if (bound-and-true-p file)
                  `((org-agenda-files ',file)))
            (org-agenda-overriding-header
             (zp/org-agenda-format-header-main ,header))
            (org-agenda-span 'week)
            (org-deadline-warning-days 0)
            (org-agenda-skip-function
             '(or (zp/skip-routine-cond)
                  (org-agenda-skip-entry-if 'todo '("CXLD"))))
            (org-agenda-entry-types '(:deadline :timestamp :sexp))
            (org-agenda-dim-blocked-tasks 'dimmed))))

(defun zp/org-agenda-block-tasks-with-group-filter (&optional filters tags by-groups file)
  `(tags-todo ,(or tags
                   "-standby-cancelled-recurring-curios")
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block-with-settings "Tasks"))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-hide-tags-regexp
                (concat org-agenda-hide-tags-regexp "\\|curios"))
               (org-agenda-sorting-strategy
                '(user-defined-down
                  category-keep))
               (org-agenda-skip-function
                '(or (zp/skip-tasks-not-in-agenda-groups-with-extra-filters
                      ',filters)
                     (zp/skip-routine-cond)
                     (zp/skip-non-tasks)
                     (zp/skip-waiting)
                     (zp/skip-future-non-waiting-timestamped-tasks-cond)))
               (org-super-agenda-groups
                ',(cond (by-groups
                         (zp/org-super-agenda-groups-all))
                        (t
                         (zp/org-super-agenda-scheduled)))))))

(defun zp/org-agenda-block-projects-with-group-filter (&optional filters tags file)
  `(tags-todo ,(or tags
                   "-standby-cancelled-curios")
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-projects))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               (org-agenda-skip-function
                '(or (zp/skip-tasks-not-in-agenda-groups-with-extra-filters
                      ',filters)
                     (zp/skip-non-projects-cond)
                     (zp/skip-waiting)
                     (zp/skip-future-non-waiting-timestamped-tasks-cond)))
               (org-agenda-sorting-strategy
                '(user-defined-down
                  category-keep))
               (org-agenda-todo-ignore-scheduled nil)
               (org-agenda-dim-blocked-tasks nil)
               (org-super-agenda-groups
                (zp/org-super-agenda-projects)))))

(defun zp/org-agenda-blocks-create (header &optional groups tags by-groups file)
  "Format the main agenda blocks.

HEADER is the string to be used as the header of the the agenda
view.

GROUPS should be a list of strings of AGENDA_GROUPS to
match (‘or’ is implied).

It creates 4 blocks:
- An ‘agenda’ block displaying the HEADER and the date
- A ‘tags-todo’ block displaying the non-stuck projects
- A ‘tags-todo’ block displaying the stuck projects
- A ‘tags-todo’ block displaying the tasks"
  (let ((filters (zp/org-agenda-groups-process-filters groups)))
    `(,(zp/org-agenda-block-header-with-deadlines
        header filters file)
      ,(zp/org-agenda-block-projects-with-group-filter
        filters tags file)
      ,(zp/org-agenda-block-tasks-with-group-filter
        filters tags by-groups file))))

(defun zp/org-agenda-variant-create (prefix-key key prefix-header header groups tags by-groups file)
  (let ((variant-key (concat prefix-key key))
        (variant-header (concat prefix-header ": " header)))
    `(,variant-key
      ,variant-header
      ,(zp/org-agenda-blocks-create variant-header groups tags by-groups file))))

(defun zp/org-agenda-variants-create (key header &optional groups tags by-groups file)
  `(;; Active
    (,key ,header
          ,(zp/org-agenda-blocks-create header groups tags by-groups file))
    ;; Inactive
    ,(zp/org-agenda-variant-create
      "i" key "Inactive"
      header groups (concat tags "-cancelled/STBY") by-groups file)
    ;; Curiosities
    ,(zp/org-agenda-variant-create
      "c" key "Curiosities"
      header groups (concat tags "+curios-cancelled") by-groups file)))

(defun zp/org-agenda-create-all (list)
  (mapcan (lambda (params)
            (apply #'zp/org-agenda-variants-create params))
          list))

(defun zp/org-agenda-block-tasks-waiting (&optional file)
  `(tags-todo "-recurring-reading/!WAIT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-main "Waiting & Stand-by Tasks"))
               ,@(if (bound-and-true-p file)
                     `((org-agenda-files ',file)))
               ;; (org-agenda-skip-function
               ;;       '(org-agenda-skip-entry-if 'notscheduled))
               (org-agenda-dim-blocked-tasks 'dimmed)
               (org-agenda-sorting-strategy '(timestamp-up user-defined-down priority-down)))))

(defun zp/org-agenda-block-deadlines ()
  '(agenda ""
           ((org-agenda-span 'day)
            (org-agenda-overriding-header
             (zp/org-agenda-format-header-main "Deadlines"))
            (org-agenda-entry-types '(:deadline))
            (org-agenda-include-deadlines t)
            (org-agenda-skip-deadline-prewarning-if-scheduled nil)
            (org-agenda-dim-blocked-tasks 'dimmed)
            (org-agenda-sorting-strategy
             '((agenda habit-down time-up deadline-up priority-down category-keep)
               (tags priority-down category-keep)
               (todo priority-down category-keep)
               (search category-keep))))))

(defun zp/org-agenda-block-reading-next-and-started ()
  '(tags-todo "+reading-recurring-standby/!NEXT|STRT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block "Next & Started")))))

(defun zp/org-agenda-block-reading-list ()
  '(tags-todo "+reading-recurring-standby/!-NEXT-STRT"
              ((org-agenda-overriding-header
                (zp/org-agenda-format-header-block "Next & Started")))))

(defun zp/org-agenda-block-journal ()
  '(agenda ""
           ((org-agenda-overriding-header
             (zp/org-agenda-format-header-main "Journal")))))

(defun zp/org-agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-delete-empty-blocks)

;;----------------------------------------------------------------------------
;; Toggles
;;----------------------------------------------------------------------------
(defun zp/toggle-org-agenda-include-habits ()
  "Toggle habits."
  (interactive)
  (if (prog1 (zp/set-agenda-local 'org-habit-show-habits
                                  (not (zp/get-agenda-local
                                        'org-habit-show-habits)))
        (org-agenda-redo))
      (message "Habits turned on.")
    (message "Habits turned off.")))

(defvar zp/org-agenda-include-routine t
  "When non-nil, include habits and items with a :routine: tag.")

(defun zp/toggle-org-agenda-include-routine ()
  "Toggle the visibility of items with a :routine: tag."
  (interactive)
  (if (prog1 (zp/set-agenda-local 'zp/org-agenda-include-routine
                                  (not (zp/get-agenda-local
                                        'zp/org-agenda-include-routine)))
        (org-agenda-redo))
      (message "Displaying habits & routine tasks.")
    (org-agenda-redo)
    (message "Hiding habits & routine tasks.")))

(defun zp/toggle-org-habit-show-all-today ()
  "Toggle the display of habits between showing only the habits
due today, and showing all of them."
  (interactive)
  (cond ((zp/get-agenda-local 'org-habit-show-all-today)
         (zp/set-agenda-local 'org-habit-show-all-today nil)
         (org-agenda-redo)
         (message "Habits: Showing today"))
        (t
         (zp/set-agenda-local 'org-habit-show-all-today t)
         (org-agenda-redo)
         (message "Habits: Showing all"))))

(defun zp/toggle-org-agenda-include-deadlines ()
  "Toggle the inclusion of deadlines in the agenda."
  (interactive)
  (cond ((zp/get-agenda-local 'org-agenda-include-deadlines)
         (zp/set-agenda-local 'org-agenda-include-deadlines nil)
         (org-agenda-redo)
         (message "Deadlines: Hidden"))
        (t
         (zp/set-agenda-local 'org-agenda-include-deadlines t)
         (org-agenda-redo)
         (message "Deadlines: Visible"))))

(defvar zp/org-agenda-include-scheduled t
  "Toggle the inclusion of scheduled items in the agenda.")

(defun zp/toggle-org-agenda-include-scheduled ()
  "Toggle the inclusion of scheduled items in the agenda."
  (interactive)
  (cond ((zp/set-agenda-local 'zp/org-agenda-include-scheduled
                              (not (zp/get-agenda-local
                                    'zp/org-agenda-include-scheduled)))
         (zp/set-agenda-local 'org-agenda-entry-types
                              '(:deadline :scheduled :timestamp :sexp))
         (org-agenda-redo)
         (message "Scheduled: Visible"))
        (t
         (zp/set-agenda-local 'org-agenda-entry-types
                              '(:deadline :timestamp :sexp))
         (org-agenda-redo)
         (message "Scheduled: Hidden"))))

(defun zp/toggle-org-agenda-category-icons ()
  "Toggle the inclusion of category icons in the agenda."
  (interactive)
  (if (prog1 (zp/set-agenda-local
              'zp/org-agenda-include-category-icons
              (not (zp/get-agenda-local
                    'zp/org-agenda-include-category-icons)))
        (org-agenda-redo))
      (message "Showing category icons.")
    (message "Hiding category icons.")))

(defvar zp/org-agenda-sorting-strategy-special-first nil
  "When non-nil, sort special TODOs first (STRT & NEXT).")

(defun zp/toggle-org-agenda-sorting-strategy-special-first ()
  "Toggle the skip function used by the agenda."
  (interactive)
  (if (prog1 (zp/set-agenda-local
              'zp/org-agenda-sorting-strategy-special-first
              (not (zp/get-agenda-local
                    'zp/org-agenda-sorting-strategy-special-first)))
        (org-agenda-redo))
      (message "Sorting: Special first.")
    (message "Sorting: Normal.")))

(defvar zp/org-agenda-split-subtasks nil
  "When non-nil, split subtasks and lone tasks.")

(defun zp/toggle-org-agenda-split-subtasks ()
  (interactive)
  (if (prog1 (zp/set-agenda-local
              'zp/org-agenda-split-subtasks
              (not (zp/get-agenda-local 'zp/org-agenda-split-subtasks)))
        (org-agenda-redo))
      (message "Splitting subtasks.")
    (message "Merging subtasks.")))

(defun zp/toggle-org-deadline-warning-days-range ()
  "Toggle the range of days for deadline to show up on the agenda."
  (interactive)
  (cond ((eq org-deadline-warning-days 7)
         (setq org-deadline-warning-days 31)
         (org-agenda-redo)
         (message "Deadline range: 1 month"))
        ((eq org-deadline-warning-days 31)
         (setq org-deadline-warning-days 7)
         (org-agenda-redo)
         (message "Deadline range: 1 week"))))

(defvar zp/org-agenda-todo-ignore-future nil
  "When non-nil, ignore future SCHEDULED and timestamps.")

(defun zp/toggle-org-agenda-todo-ignore-future ()
  "Toggle whether to include future SCHEDULED and timestamps."
  (interactive)
  (cond ((eq (zp/get-agenda-local 'zp/org-agenda-todo-ignore-future) t)
         (zp/set-agenda-local 'zp/org-agenda-todo-ignore-future nil)
         (zp/set-agenda-local 'org-agenda-todo-ignore-scheduled nil)
         (org-agenda-redo)
         (message "Show items in the future."))
        (t
         (zp/set-agenda-local 'zp/org-agenda-todo-ignore-future t)
         (zp/set-agenda-local 'org-agenda-todo-ignore-scheduled 'future)
         (org-agenda-redo)
         (message "Ignore items in the future."))))

(defvar zp/org-agenda-sort-by-rev-fifo nil
  "When non-nil, sort by reverse FIFO order.")

(defun zp/toggle-org-agenda-sort-by-rev-fifo ()
  (interactive)
  (if (prog1 (zp/set-agenda-local 'zp/org-agenda-sort-by-rev-fifo
                                  (not (zp/get-agenda-local
                                        'zp/org-agenda-sort-by-rev-fifo)))
        (org-agenda-redo))
      (message "Show items in reverse FIFO order.")
    (message "Show items in FIFO order.")))

(defvar zp/org-agenda-include-projects t
  "When non-nil, include projects in the org-agenda task view.")

(defun zp/toggle-org-agenda-include-projects ()
  (interactive)
  (if (prog1 (zp/set-agenda-local 'zp/org-agenda-include-projects
                                  (not (zp/get-agenda-local
                                        'zp/org-agenda-include-projects)))
        (org-agenda-redo))
      (message "Showing projects.")
    (message "Hiding projects.")))

(defun zp/toggle-org-agenda-projects-include-waiting ()
  "Toggle whether to include projects with a waiting task."
  (interactive)
  (if (prog1 (zp/set-agenda-local 'zp/org-agenda-include-waiting
                                  (not (zp/get-agenda-local
                                        'zp/org-agenda-include-waiting)))
        (org-agenda-redo))
      (message "Waiting: Visible")
    (message "Waiting: Hidden")))

(defun zp/toggle-org-agenda-dim-blocked-tasks ()
  "Toggle the dimming of blocked tags in the agenda."
  (interactive)
  (cond ((or (eq org-agenda-dim-blocked-tasks nil)
             (eq org-agenda-dim-blocked-tasks 'invisible))
         (setq org-agenda-dim-blocked-tasks t)
         (org-agenda-redo)
         (message "Blocked tasks: Dimmed"))
        (t
         (setq org-agenda-dim-blocked-tasks nil)
         (org-agenda-redo)
         (message "Blocked tasks: Plain"))))

(defun zp/toggle-org-agenda-hide-blocked-tasks ()
  "Toggle the visibility of blocked tags in the agenda."
  (interactive)
  (cond ((or (eq org-agenda-dim-blocked-tasks nil)
             (eq org-agenda-dim-blocked-tasks t))
         (setq org-agenda-dim-blocked-tasks 'invisible)
         (org-agenda-redo)
         (message "Blocked tasks: Invisible"))
        (t
         (setq org-agenda-dim-blocked-tasks t)
         (org-agenda-redo)
         (message "Blocked tasks: Dimmed"))))

(defun zp/toggle-org-agenda-show-all-dates ()
  "Toggle the inclusion of days without entries in the agenda."
  (interactive)
  (if (prog1 (zp/set-agenda-local
              'org-agenda-show-all-dates
              (not (zp/get-agenda-local 'org-agenda-show-all-dates)))
        (org-agenda-redo))
      (message "Showing all dates.")
    (message "Hiding empty dates.")))

;;----------------------------------------------------------------------------
;; Prepare agendas
;;----------------------------------------------------------------------------
(defun zp/org-agenda-set-category-icons ()
  "Set category icons.

If ‘zp/org-agenda-include-category-icons’ is non-nil, the
function populate ‘org-agenda-category-icon-alist’.

Meant to be run with ‘org-agenda-mode-hook’."
  (setq org-agenda-category-icon-alist
        (when zp/org-agenda-include-category-icons
          zp/org-agenda-category-icon-alist)))

(add-hook 'zp/org-agenda-load-local-config-post-hook
          #'zp/org-agenda-set-category-icons)

(defun zp/org-agenda-create-local-config (&optional agenda)
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    (setf (alist-get agenda
                     zp/org-agenda-local-config
                     nil nil 'equal)
          ;; Copying the list is necessary to have different
          ;; references to the same values.  Otherwise, we’d also
          ;; modify the global config.
          (list (copy-tree (car (alist-get 'default
                                           zp/org-agenda-local-config)))))))

(defun zp/org-agenda-wipe-current-local-config (print-message &optional agenda)
  "Reset the org-agenda local config to the default values."
  (interactive "p")
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    (setq zp/org-agenda-local-config
          (delq (assoc agenda zp/org-agenda-local-config)
                zp/org-agenda-local-config)))
  (when print-message
    (org-agenda-redo-all)
    (message "Local org-agenda config has been reset.")))

(defun zp/org-agenda-wipe-all-local-configs (print-message)
  "Reset all local org-agenda configs to their default value."
  (interactive "p")
  (setq zp/org-agenda-local-config (list (assoc 'default zp/org-agenda-local-config)))
  (zp/org-agenda-redo-all)
  (when print-message
    (message "All local org-agenda configs have been reset.")))

(defun zp/org-agenda-wipe-local-config (&optional print-message agenda)
  "Reset the org-agenda local config.

With a ‘C-u’ prefix argument, reset all the local configs."
  (interactive "p")
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    (pcase print-message
      (4 (zp/org-agenda-wipe-all-local-configs print-message))
      (_ (zp/org-agenda-wipe-current-local-config print-message agenda)))))

(defvar zp/org-agenda-extra-local-config nil
  "Extra settings to use for local agenda views.

Those settings will be loaded *after* the ones in
‘zp/org-agenda-local-config’.  This is to allow some of your
local agendas to have a different default state for some
variables.

Note that those variables will also be loaded after
a wipe (‘zp/org-agenda-wipe-local-config’).")

(defun zp/org-agenda-load-extra-local-config (&optional agenda)
  "Load the extra local config for the current view."
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key)))
        (config zp/org-agenda-extra-local-config))
    (mapcar (lambda (params)
              (let ((var (car params))
                    (val (cdr params)))
                (zp/set-agenda-local var val agenda)))
            (car (alist-get agenda config nil nil 'equal)))))

(defun zp/org-agenda-load-local-config (&optional agenda)
  "Load the org-agenda local config for the current view."
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    ;; Create local config if it doesn’t exist for current agenda
    (unless (assoc agenda zp/org-agenda-local-config)
      (zp/org-agenda-create-local-config agenda)
      (zp/org-agenda-load-extra-local-config agenda))
    ;; Load all settings
    (mapcar (lambda (cons)
              (let ((variable (car cons))
                    (value (cdr cons)))
                (set variable value
                     ;; (message "Setting ‘%s’ to ‘%s’." variable value)
                     )))
            ;; alist-get returns its match within a list, but we only
            ;; need its car
            (car (alist-get agenda
                            zp/org-agenda-local-config
                            nil nil 'equal)))
    ;; Refresh mode-name based on modifications
    (org-agenda-set-mode-name)
    (run-hooks 'zp/org-agenda-load-local-config-post-hook)))

(add-hook 'org-agenda-mode-hook #'zp/org-agenda-load-local-config)

;;----------------------------------------------------------------------------
;; Prepare org-agenda files
;;----------------------------------------------------------------------------
(defun zp/org-agenda-prepare-main-file ()
  "Prepare the main file for creating an org-agenda view.

Save the file it has been modified, and reveal everything in the
file (trees, drawers, etc.)."
  (let ((buffer (get-file-buffer "~/org/life.org"))
        (inhibit-message t))
    (when buffer
      (with-current-buffer buffer
        (when (buffer-modified-p (get-file-buffer "~/org/life.org"))
          (save-buffer))
        (org-show-all)))))

(add-hook 'org-agenda-mode-hook #'zp/org-agenda-prepare-main-file)

;;----------------------------------------------------------------------------
;; Local variables in org-agenda
;;----------------------------------------------------------------------------
(defun zp/org-agenda-local-has-config-p (&optional agenda)
  "Return t when the agenda has a local config."
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    (alist-get agenda zp/org-agenda-local-config nil nil 'equal)))

(defun zp/get-agenda-local (symbol &optional agenda)
  "Get value of SYMBOL for the current org-agenda view.

If AGENDA is an agenda key (as a string), set SYMBOL to VALUE in
that agenda.

If AGENDA is 'default, set SYMBOL to VALUE globally."
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    (unless (zp/org-agenda-local-has-config-p agenda)
      (zp/org-agenda-load-local-config))
    (alist-get symbol (car (alist-get agenda
                                      zp/org-agenda-local-config
                                      nil nil 'equal)))))

(defun zp/set-agenda-local (symbol value &optional agenda)
  "Set SYMBOL to VALUE locally for the current org-agenda view.

If AGENDA is an agenda key (as a string), set SYMBOL to VALUE in
that agenda.

If AGENDA is 'default, set SYMBOL to VALUE globally."
  (let ((agenda (or agenda
                    (zp/org-agenda-get-key))))
    (unless (zp/org-agenda-local-has-config-p agenda)
      (zp/org-agenda-load-local-config))
    (setf (alist-get symbol
                     (car (alist-get agenda
                                     zp/org-agenda-local-config
                                     nil nil 'equal)))
          value)
    value))

;;----------------------------------------------------------------------------
;; Garbage collection
;;----------------------------------------------------------------------------
(defvar zp/org-agenda-default-agendas-list nil
  "List of agendas to consider as defaults.

Any agenda not in this list will be considered special, thereby
marking it for deletion upon garbage collection.")

(defun zp/org-agenda-kill-special-agendas ()
  "Kill all special agendas.

An agenda is considered special if its key isn’t listed in
‘zp/org-agenda-default-agendas-list’.

This will also wipe the local config of the special agendas."
  (interactive)
  (let ((kill-count 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (let ((agenda (zp/org-agenda-get-key)))
            (unless (member agenda zp/org-agenda-default-agendas-list)
              (zp/org-agenda-wipe-local-config)
              (kill-buffer)
              (setq kill-count (1+ kill-count)))))))
    kill-count))

(defun zp/org-agenda-garbage-collect (arg)
  "Garbage collect all special agendas and open main view.

With a ‘C-u’ prefix argument, also kill the main Org buffer."
  (interactive "p")
  (let ((kill-file (eq arg 4))
        (kill-count (zp/org-agenda-kill-special-agendas)))
    (zp/create-agenda-view nil)
    (when kill-file
      (when-let ((file (find-buffer-visiting "~/org/life.org")))
        (with-current-buffer file
          (when (buffer-modified-p)
            (save-buffer))
          (kill-buffer))))
    (org-agenda-redo-all)
    (with-selected-window (window-left (selected-window))
      (org-agenda-redo-all))
    (when arg
      (message (concat "Garbage collection complete: "
                       (when kill-file
                         "Org file was killed, and ")
                       (pcase kill-count
                         (0 "no agenda-buffers were killed.")
                         (1 "1 agenda-buffer was killed.")
                         (_ (concat (number-to-string kill-count)
                                    " agenda-buffers were killed."))))))))

;;----------------------------------------------------------------------------
;; Creating & toggling org-agenda windows
;;----------------------------------------------------------------------------
;; TODO: Improve

(defun zp/create-agenda-view (arg)
  "Create the default agenda view."
  (interactive "P")
  (if (get-buffer "*Org Agenda(l)*")
      (switch-to-buffer "*Org Agenda(l)*")
    (org-agenda arg "l"))
  (delete-other-windows)
  (split-window-right)
  (if (get-buffer "*Org Agenda(n)*")
      (switch-to-buffer "*Org Agenda(n)*")
    (org-agenda arg "n"))
  (select-window (next-window))
  (balance-windows))

(defun zp/switch-to-agenda (arg)
  "Toggles a custom org-agenda context, or creates it if it doesn’t
  exist.

The layout of the org-agenda context is stored as a frame
parameter, which implies that each frame can have its own
independent org-agenda context.

With a ‘C-u’ prefix argument, forces the re-creation of the
org-agenda context."

  (interactive "P")
  (let* ((current-config (current-window-configuration))
         (current-frame (window-configuration-frame current-config)))

    (cond ((and
            (frame-parameter current-frame
                             'zp/org-agenda-session-p)
            (not arg))
           (set-frame-parameter current-frame
                                'zp/org-agenda-window-config
                                current-config)
           (set-window-configuration
            (frame-parameter current-frame
                             'zp/org-agenda-window-config-before))
           (set-frame-parameter current-frame
                                'zp/org-agenda-session-p nil))

          (t
           (set-frame-parameter current-frame
                                'zp/org-agenda-window-config-before
                                current-config)
           (if (or arg
                   (not (frame-parameter current-frame
                                         'zp/org-agenda-window-config)))
               (zp/create-agenda-view arg)
             (set-window-configuration
              (frame-parameter current-frame
                               'zp/org-agenda-window-config)))
           (set-frame-parameter current-frame
                                'zp/org-agenda-session-p t)))))

;;----------------------------------------------------------------------------
;; Rest
;;----------------------------------------------------------------------------
;; Commented because unused
;; (defmacro zp/org-agenda-run-on-current-entry (&rest body)
;;   "Run BODY on current org-agenda entry.

;; Functions in BODY can make use of the following local
;; variables:
;; - ‘hdmarker’: Marker on the entry.
;; - ‘buffer’: Buffer of the entry."
;;   (interactive)
;;   `(progn
;;      (org-agenda-check-no-diary)
;;      (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
;;                           (org-agenda-error)))
;;             (buffer (marker-buffer hdmarker))
;;             (inhibit-read-only t))
;;        (with-current-buffer buffer
;;          ,@body))))

(defun zp/org-agenda-set-property (property-function)
  "Set a property for the current headline in the agenda.
Based on `org-agenda-set-property'."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (call-interactively property-function)))))

(defun zp/org-agenda-delete-property ()
  "Delete a property for the current headline in the agenda."
  (interactive)
  (zp/org-agenda-set-property 'org-delete-property))

(defun zp/org-agenda-remove-mouse-face ()
  "Remove mouse-face from org-agenda."
  (remove-text-properties(point-min) (point-max) '(mouse-face t)))

(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-hi-lock)
(add-hook 'org-agenda-finalize-hook #'zp/org-agenda-remove-mouse-face)

;; Variables used for debugging
(defvar zp/org-agenda-skip-functions-debug nil
  "When t, print helpful debugging messages for skips.")

(setq zp/org-agenda-skip-functions-debug nil)

(define-key mode-specific-map (kbd "a") 'org-agenda)

;;----------------------------------------------------------------------------
;; Spawned indirect buffers
;;----------------------------------------------------------------------------
(defun zp/org-agenda-tree-to-indirect-buffer (dedicated)
  "Show the subtree corresponding to the current entry in an indirect buffer.

With a ‘C-u’ prefix, make a separate frame for this tree."
  (interactive "P")
  (let* ((last-ibuf org-last-indirect-buffer)
         (buffer (if dedicated
                     (save-window-excursion
                       (setq org-last-indirect-buffer nil)
                       (org-agenda-tree-to-indirect-buffer nil)
                       (select-window (previous-window))
                       (current-buffer))
                   (org-agenda-tree-to-indirect-buffer nil)))
         (parent-window (selected-window))
         subtask)
    (with-selected-window (if dedicated
                              (and (split-window-below)
                                   (next-window))
                            (next-window))
      (cond (dedicated
             (setq org-last-indirect-buffer last-ibuf)
             (switch-to-buffer buffer))
            (t
             (zp/org-spawned-ibuf-mode t)
             (setq zp/org-ibuf-spawned-also-kill-window parent-window)))
      (when (setq subtask (zp/is-subtask-p))
        (zp/org-narrow-up-heading nil t))
      (zp/org-overview nil subtask t)
      (when subtask
        (org-show-entry))
      (org-back-to-heading)
      (org-beginning-of-line))
    (balance-windows)
    (select-window (next-window))
    (message "Visiting tree in indirect buffer.")
    (run-hooks 'zp/org-after-view-change-hook)))

(defun zp/org-agenda-tree-to-indirect-buffer-without-grabbing-focus (arg)
  (interactive "P")
  (zp/org-agenda-tree-to-indirect-buffer arg)
  (select-window (previous-window)))

(defun zp/org-agenda-tree-to-indirect-buffer-maximise (arg)
  (interactive "P")
  (switch-to-buffer
   (save-window-excursion
     (zp/org-agenda-tree-to-indirect-buffer arg)
     (prog1 (current-buffer)
       (setq zp/org-ibuf-spawned-also-kill-window nil)))))

(defun zp/org-kill-spawned-ibuf-and-window ()
  "Kill the other buffer and window if there is more than one window."
  (interactive)
  (let ((other (and (not (one-window-p))
                    (save-excursion
                      (select-window (next-window))
                      (prog1 (current-buffer)
                        (select-window (previous-window)))))))
    (with-current-buffer other
      (zp/org-kill-spawned-ibuf))))

(provide 'zp-org-agenda)
;;; zp-org-agenda.el ends here
