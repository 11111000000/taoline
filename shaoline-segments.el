;;; shaoline-segments.el --- Standard segments for shaoline -*- lexical-binding: t -*-

(require 'shaoline-msg-engine)
(require 'all-the-icons nil t)
(require 'projectile nil t)
(require 'calendar)
(require 'battery nil t)

;;; Buffer icon & name
(shaoline-define-segment shaoline-segment-icon-and-buffer (buffer)
  "Цветная иконка буфера (по режиму, как во вкладках) + имя буфера."
  (let* ((icon (when (featurep 'all-the-icons)
                 (let* ((mode (buffer-local-value 'major-mode buffer))
                        (raw (all-the-icons-icon-for-mode mode :height 0.9)))
                   (cond
                    ((stringp raw) raw)
                    ((buffer-file-name buffer)
                     (all-the-icons-icon-for-file
                      (buffer-file-name buffer) :height 0.9))
                    (t (all-the-icons-faicon "file-o" :height 0.9))))))
         (name (buffer-name buffer))
         (text (if icon (concat icon " " name) name)))
    (add-face-text-property
     (if icon (length icon) 0) (length text)
     'shaoline-buffer-face 'append
     text)
    text))

;;; Project name
(shaoline-define-simple-segment shaoline-segment-project-name
  "Project name, if available."
  (let* ((project
          (cond
           ((and (featurep 'projectile) (projectile-project-name))
            (projectile-project-name))
           ((fboundp 'project-current)
            (when-let ((pr (project-current)))
              (file-name-nondirectory (directory-file-name (car (project-roots pr))))))
           (t nil))))
    (when (and project (not (string= "-" project)))
      (propertize project 'face 'shaoline-project-face))))

;;; Git branch
(shaoline-define-simple-segment shaoline-segment-git-branch
  "Current Git branch."
  (when (and (featurep 'vc-git) (buffer-file-name))
    (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
      (when branch
        (concat
         (when (featurep 'all-the-icons)
           (all-the-icons-octicon "git-branch" :v-adjust 0 :height 1.0 :face 'shaoline-git-face))
         " "
         (propertize branch 'face 'shaoline-git-face))))))

;;; Echo-area message
(shaoline-define-simple-segment shaoline-segment-echo-message
  "Show the most recent user `message' for a fixed duration.
The segment itself no longer worries about width – `shaoline-compose-modeline'
will truncate it if necessary."
  (let ((show (and (shaoline-msg-active-p shaoline-message-timeout)
                   (shaoline-msg-current))))
    (if show
        (propertize show 'face 'shaoline-echo-face)
      "")))

;;; Battery
(shaoline-define-simple-segment
 shaoline-segment-battery
 "Show battery percentage and charging status."
 (if (and (fboundp 'battery) battery-status-function)
     (let* ((data (funcall battery-status-function)))
       (cond
        ((and (listp data) (cl-every #'consp data))
         (let* ((percent (or (cdr (assoc 112 data))
                             (cdr (assoc "percentage" data))
                             (cdr (assoc "perc" data))
                             (cdr (assoc "capacity" data))))
                (status (or (cdr (assoc 66 data))
                            (cdr (assoc "status" data))
                            (cdr (assoc "charging" data))
                            (cdr (assoc "state" data))))
                (icon (cond
                       ((not (featurep 'all-the-icons)) "")
                       ((and percent (string-match "\\([0-9]+\\)" percent))
                        (let* ((n (string-to-number (match-string 1 percent))))
                          (cond ((>= n 90) (all-the-icons-faicon "battery-full"
                                                                 :face 'shaoline-battery-face))
                                ((>= n 70) (all-the-icons-faicon "battery-three-quarters"
                                                                 :face 'shaoline-battery-face))
                                ((>= n 40) (all-the-icons-faicon "battery-half"
                                                                 :face 'shaoline-battery-face))
                                ((>= n 10) (all-the-icons-faicon "battery-quarter"
                                                                 :face 'shaoline-battery-face))
                                (t (all-the-icons-faicon "battery-empty"
                                                        :face 'shaoline-battery-face)))))
                       ((and status (string-match-p "full" status)) (all-the-icons-faicon "battery-full" :face 'shaoline-battery-face))
                       ((and status (string-match-p "\\<ac\\>" status)) (all-the-icons-octicon "plug" :face 'shaoline-battery-face))
                       ((and status (string-match-p "charging" status)) (all-the-icons-faicon "bolt" :face 'shaoline-battery-face))
                       ((and status (string-match-p "discharging" status)) "")
                       (t ""))))
           (if percent
               (concat
                (if (and (stringp icon) (not (string-empty-p icon))) (concat icon " "))
                (propertize (concat (replace-regexp-in-string "%" "" percent) "%")
                            'face 'shaoline-battery-face))
             (propertize
              (if (featurep 'all-the-icons)
                  (concat (all-the-icons-faicon "battery-empty" :face 'shaoline-battery-face)
                          " No battery")
                "No battery")
              'face '(:inherit shaoline-battery-face :slant italic)))))
        ((and (stringp data) (not (string-empty-p data)))
         (propertize data 'face 'shaoline-battery-face))
        (t (propertize
            (if (featurep 'all-the-icons)
                (all-the-icons-faicon "battery-empty" :face 'shaoline-battery-face)
              "N/A")
            'face '(:inherit shaoline-battery-face :slant italic)))))
   ""))

;;; Major-mode with icon
(shaoline-define-simple-segment shaoline-segment-major-mode
  "Major mode segment with icon."
  (let ((icon (when (and (featurep 'all-the-icons) major-mode)
                (all-the-icons-icon-for-mode major-mode :height 0.9))))
    (concat
     (when (and icon (stringp icon))
       (concat icon " "))
     (propertize (format-mode-line mode-name)
                 'face 'shaoline-mode-face))))

;;; Time + Moon Phase
(defun shaoline--moon-phase-idx (&optional date)
  "Return moon phase index 0..7 for DATE."
  (let* ((d        (or date (calendar-current-date)))
         (abs-day  (float (calendar-absolute-from-gregorian d)))
         (synodic  29.530588853)
         (age      (- abs-day (* (floor (/ abs-day synodic)) synodic)))
         (idx      (mod (floor (* age (/ 8.0 synodic))) 8)))
    idx))

(shaoline-define-simple-segment shaoline-segment-time
  "Current time with moon phase."
  (let* ((time (propertize (format-time-string "%H:%M") 'face 'shaoline-time-face))
         (phase-number (shaoline--moon-phase-idx))
         (phases ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
         (moon (propertize (aref phases phase-number) 'face 'shaoline-time-face)))
    (concat time " " moon "  ")))

(provide 'shaoline-segments)
;;; shaoline-segments.el ends here
