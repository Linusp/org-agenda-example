(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-frame-font "Dejavu Sans Mono 12" nil t)
(modify-all-frames-parameters (list (cons 'font "Dejavu Sans Mono 12")))
(set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 15");
(setq face-font-rescale-alist '(("WenQuanYi Micro Hei Mono" . 1.3)))

;; org-agenda 基础设置
(require 'org)
(require 'org-clock)
(setq org-agenda-span 'day
      org-agenda-files '("agenda-files-dir")
      org-agenda-block-separator " "
      )

(defun z/get-weekday ()
  (let* ((today (calendar-current-date))
         (weekday (org-day-of-week (nth 1 today) (nth 0 today) (nth 2 today))))
    (if (> weekday 0) weekday 7)))

(defun z/begin-date-of-current-week ()
  (let* ((weekday (z/get-weekday))
         (offset (- 1 weekday)))
    (org-read-date nil nil (format "+%dd" offset))))

(defun z/end-date-of-current-week ()
  (let* ((weekday (z/get-weekday))
         (offset (- 7 weekday)))
    (org-read-date nil nil (format "+%dd" offset))))

(defun z/get-day (property)
  (let ((val (org-entry-get nil property)))
    (if val (time-to-days (org-time-string-to-time val)) nil)))

(defun z/get-subtasks-num ()
  (save-excursion
    (save-restriction
      (progn
        (org-back-to-heading)
        (org-narrow-to-subtree)
        (org-show-children)
        (let* ((level (org-current-level))
               (subheading-level (+ 1 level))
               (todo-keywords '("TODO" "NEXT" "DONE" "ABORT")))
          (length (delq nil
                        (org-map-entries
                         (lambda ()
                           (let ((todo-keyword (org-get-todo-state)))
                             (when (and (= (org-current-level) subheading-level)
                                        todo-keyword
                                        (member (substring-no-properties todo-keyword) todo-keywords))
                               (org-heading-components))))
                         nil
                         'tree))))))))

(defun z/get-ancestor-tasks-num ()
  (save-excursion
    (save-restriction
      (let* ((level (org-current-level))
             (cur-level level)
             (todo-keywords '("TODO" "NEXT" "DONE" "ABORT"))
             (ancestors '()))
        (while (> cur-level 1)
          (outline-up-heading 1)
          (let ((todo-keyword (org-get-todo-state)))
            (when (and todo-keyword (member (substring-no-properties todo-keyword) todo-keywords))
              (push (org-heading-components) ancestors)))
          (setq cur-level (org-current-level)))
        (length ancestors)))))


(defun z/org-agenda-select (&rest args)
  (let* ((no-subtasks (plist-get args :no-subtasks))
         (no-ancestors (plist-get args :no-ancestors))
         (todo-keywords (plist-get args :todo-keywords))
         (begin-scheduled (plist-get args :begin-scheduled))
         (end-scheduled (plist-get args :end-scheduled))
         (begin-deadline (plist-get args :begin-deadline))
         (end-deadline (plist-get args :end-deadline))
         (begin-scheduled-or-deadline (plist-get args :begin-scheduled-or-deadline))
         (end-scheduled-or-deadline (plist-get args :end-scheduled-or-deadline))
         (begin-scheduled-date (cond ((null begin-scheduled) nil)
                                     ((string= begin-scheduled "ws") (z/begin-date-of-current-week))
                                     ((string= begin-scheduled "we") (z/end-date-of-current-week))
                                     (t (org-read-date nil nil begin-scheduled))))
         (end-scheduled-date (cond ((null end-scheduled) nil)
                                   ((string= end-scheduled "ws") (z/begin-date-of-current-week))
                                   ((string= end-scheduled "we") (z/end-date-of-current-week))
                                   (t (org-read-date nil nil end-scheduled))))
         (begin-deadline-date (cond ((null begin-deadline) nil)
                                    ((string= begin-deadline "ws") (z/begin-date-of-current-week))
                                    ((string= begin-deadline "we") (z/end-date-of-current-week))
                                    (t (org-read-date nil nil begin-deadline))))
         (end-deadline-date (cond ((null end-deadline) nil)
                                  ((string= end-deadline "ws") (z/begin-date-of-current-week))
                                  ((string= end-deadline "we") (z/end-date-of-current-week))
                                  (t (org-read-date nil nil end-deadline))))
         (begin-s-or-d (cond ((null begin-scheduled-or-deadline) nil)
                             ((string= begin-scheduled-or-deadline "ws") (z/begin-date-of-current-week))
                             ((string= begin-scheduled-or-deadline "we") (z/end-date-of-current-week))
                             (t (org-read-date nil nil begin-scheduled-or-deadline))))
         (end-s-or-d (cond ((null end-scheduled-or-deadline) nil)
                           ((string= end-scheduled-or-deadline "ws") (z/begin-date-of-current-week))
                           ((string= end-scheduled-or-deadline "we") (z/end-date-of-current-week))
                           (t (org-read-date nil nil end-scheduled-or-deadline))))
         (next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (todo-keyword (org-get-todo-state))
         (deadline-day (z/get-day "DEADLINE"))
         (scheduled-day (z/get-day "SCHEDULED"))
         (now (time-to-days (current-time)))
         (subtasks-num (z/get-subtasks-num))
         (ancestors-num (z/get-ancestor-tasks-num))
         (subtree-valid
          (and (or (not no-subtasks) (= subtasks-num 0))
               (or (not no-ancestors) (= ancestors-num 0))
               (or (not todo-keywords) (and todo-keyword
                                            (member (substring-no-properties todo-keyword) todo-keywords)))
               (or (not begin-scheduled-date)
                   (and scheduled-day
                        (>= scheduled-day (org-time-string-to-absolute begin-scheduled-date))))
               (or (not end-scheduled-date)
                   (and scheduled-day
                        (<= scheduled-day (org-time-string-to-absolute end-scheduled-date))))
               (or (not begin-deadline)
                   (and deadline-day
                        (>= deadline-day (org-time-string-to-absolute begin-deadline-date))))
               (or (not end-deadline-date)
                   (and deadline-day
                        (<= deadline-day (org-time-string-to-absolute end-deadline-date))))
               (or (not begin-s-or-d)
                   (or (and scheduled-day
                            (>= scheduled-day (org-time-string-to-absolute begin-s-or-d)))
                       (and deadline-day
                            (>= deadline-day (org-time-string-to-absolute begin-s-or-d)))))
               (or (not end-s-or-d)
                   (or (and scheduled-day
                            (<= scheduled-day (org-time-string-to-absolute end-s-or-d)))
                       (and deadline-day
                            (<= deadline-day (org-time-string-to-absolute end-s-or-d))))))))
    (unless subtree-valid (min next-headline subtree-end))))


;; agenda 视图定义
(setq org-agenda-custom-commands
      '(("a" "Customized Agenda View"
         ((agenda "" ((org-agenda-overriding-header "今日事项")
                      (org-agenda-format-date "")
                      (org-scheduled-past-days 0)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-todo-ignore-deadlines 'far)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-log-mode-add-notes nil)
                      (org-agenda-sorting-strategy '(time-up priority-down category-keep))
                      (org-agenda-skip-function
                       '(z/org-agenda-select :no-subtasks t
                                           :todo-keywords '("TODO" "NEXT")
                                           :begin-scheduled-or-deadline "today"
                                           :end-scheduled-or-deadline "today"))))
          (todo "" ((org-agenda-time-grid nil)
                    (org-agenda-files '("agenda-files-dir/work.org"))
                    (org-agenda-overriding-header "本周工作项目")
                    (org-agenda-sorting-strategy '(time-up priority-down category-keep))
                    (org-agenda-skip-function
                     '(z/org-agenda-select :no-ancestors t
                                         :todo-keywords '("TODO" "NEXT" "DONE" "ABORT")
                                         :begin-scheduled-or-deadline "ws"
                                         :end-scheduled-or-deadline "we"))))
          (todo "" ((org-agenda-overriding-header "本周个人项目")
                    (org-agenda-files '("agenda-files-dir/personal.org"))
                    (org-agenda-sorting-strategy '(time-up priority-down category-keep))
                    (org-agenda-skip-function
                     '(z/org-agenda-select :no-ancestors t
                                         :todo-keywords '("TODO" "NEXT" "DONE" "ABORT")
                                         :begin-scheduled-or-deadline "ws"
                                         :end-scheduled-or-deadline "we"))))
          (todo "SOMETIME" ((org-agenda-overriding-header "纯计时任务")))))
        ("l" "Daily Agenda Logs"
         ((agenda "" ((org-agenda-overriding-header "今日记录")
                      (org-agenda-span 'day)
                      (org-agenda-show-log 'clockcheck)
                      (org-agenda-start-with-log-mode nil)
                      (org-agenda-archives-mode t)
                      (org-agenda-log-mode-items '(closed clock))
                      (org-agenda-clockreport-parameter-plist
                       '(:link t :maxlevel 2 :fileskip0 t :scope agenda-with-archives))
                      (org-agenda-clockreport-mode t)
                      (org-agenda-log-mode-add-notes nil)
                      ))))
        ("k" "Kanban View"
         ((agenda "" ((org-agenda-overriding-header "TODO")
                      (org-agenda-format-date "")
                      (org-agenda-span 'day)
                      (org-agenda-time-grid nil)
                      (org-agenda-use-time-grid nil)
                      (org-scheduled-past-days 0)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-skip-function
                       '(z/org-agenda-select :todo-keywords '("TODO")
                                           :begin-scheduled-or-deadline "today"
                                           :end-scheduled-or-deadline "today"))))
          (agenda "" ((org-agenda-overriding-header "NEXT")
                      (org-agenda-format-date "")
                      (org-agenda-span 'day)
                      (org-agenda-time-grid nil)
                      (org-agenda-use-time-grid nil)
                      (org-scheduled-past-days 0)
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-todo-ignore-scheduled 'past)
                      (org-agenda-skip-function
                       '(z/org-agenda-select :todo-keywords '("NEXT")
                                           :begin-scheduled-or-deadline "today"
                                           :end-scheduled-or-deadline "today"))))
          (agenda "" ((org-agenda-overriding-header "DONE")
                      (org-agenda-format-date "")
                      (org-agenda-span 'day)
                      (org-agenda-time-grid nil)
                      (org-agenda-use-time-grid nil)
                      (org-scheduled-past-days 0)
                      (org-deadline-warning-days 0)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-skip-function
                       '(z/org-agenda-select :todo-keywords '("DONE" "ABORT")
                                           :begin-scheduled-or-deadline "today"
                                           :end-scheduled-or-deadline "today"))))
         ))
        ))


(org-agenda "" "k")
(delete-other-windows)
