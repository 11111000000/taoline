;;; taoline.el --- Functional minimalist echo-area modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Peter
;;
;; Author: Peter <11111000000@email.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mode-line, minimal, functional, echo-area
;; URL: https://github.com/11111000000/taoline
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: MIT

;;; Commentary:

;; taoline-echo is a trimmed-down edition of the original “taoline”.
;; There is no child-frame and no in-window modeline — the only rendering
;; target is the echo area (the minibuffer).  The architecture remains
;; functional: every segment is a pure function, the string composition
;; is a pure function as well, and the only side effects are writing to
;; the echo area and (optionally) hiding the regular `mode-line`.
;;
;; Key features:
;;   • Minimalist, declarative segment configuration.
;;   • No polling timers — the modeline is refreshed only from hooks
;;     (`post-command-hook`, etc.).
;;   • Global minor mode `taoline-mode` to toggle everything on/off.
;;   • Optional hiding of the traditional mode line.
;;   • A unit-test-friendly “core” — the compose function is side-effect-free.
;;
;; Quick start:
;;
;;   (require 'taoline-echo)
;;   (taoline-mode 1)
;;
;; See the README on GitHub for examples of writing custom segments.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'rx))
(require 'projectile nil t)
(require 'all-the-icons nil t)

;; ----------------------------------------------------------------------------
;; Customisation group and variables

(defgroup taoline nil
  "Functional minimalist echo-area modeline."
  :group 'convenience
  :prefix "taoline-")

(defcustom taoline-debug nil
  "If non-nil, log taoline activity into `taoline--log-buffer'."
  :type 'boolean
  :group 'taoline)

(defvar taoline--log-buffer "*taoline-logs*"
  "Buffer name used for debug log.")

(defun taoline--log (fmt &rest args)
  "Write formatted FMT/ARGS message to `taoline--log-buffer' if `taoline-debug' is non-nil."
  (when taoline-debug
    (let ((msg (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create taoline--log-buffer)
        (goto-char (point-max))
        (insert msg "\n")))))

(defcustom taoline-segments
  '((:left taoline-segment-icon-and-buffer taoline-segment-git-branch)
    (:center taoline-segment-echo-message)
    (:right taoline-segment-project-name taoline-segment-battery taoline-segment-time))
  "Alist describing segments for :left, :center and :right.
Each value is a list of segment function symbols."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'taoline)

(defcustom taoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "Hooks that trigger Taoline to recompute and display the modeline."
  :type '(repeat symbol)
  :group 'taoline)

(defcustom taoline-autohide-modeline t
  "When non-nil, hide the usual mode-line while `taoline-mode' is active."
  :type 'boolean
  :group 'taoline)

(defcustom taoline-right-padding 13
  "Spaces appended to the rightmost edge of taoline."
  :type 'integer
  :group 'taoline)

;; ----------------------------------------------------------------------------
;; Faces

(defface taoline-base-face
  '((t :inherit default
       :height 1.0
       :box nil
       :underline nil
       :overline nil
       :inverse-video nil
       :extend t))
  "Base face for taoline."
  :group 'taoline)

(defface taoline-echo-face
  '((t :inherit (font-lock-comment-face taoline-base-face) :height 1.0))
  "Face for echo message segment (slightly smaller)."
  :group 'taoline)

(defface taoline-time-face
  '((t :background "#002200" :foreground "#00aa00" :height 1.0 :bold nil :family "Digital Display"))
  "Face for time segment."
  :group 'taoline)

(defface taoline-battery-face
  '((t :inherit (success taoline-base-face) :height 0.8))
  "Face for time segment."
  :group 'taoline)

(defface taoline-buffer-face
  '((t :inherit (taoline-base-face) :height 1.0))
  "Face for buffer name segment."
  :group 'taoline)

(defface taoline-project-face
  '((t :inherit (font-lock-keyword-face taoline-base-face) :height 1.0))
  "Face for project name segment."
  :group 'taoline)

(defface taoline-modified-face
  '((t :inherit (warning taoline-base-face) :height 1.0))
  "Face for modified indicator segment."
  :group 'taoline)

(defface taoline-git-face
  '((t :inherit (font-lock-type-face taoline-base-face) :height 1.0))
  "Face for git branch segment."
  :group 'taoline)

(defface taoline-mode-face
  '((t :inherit (font-lock-type-face taoline-base-face) :height 1.0))
  "Face for major mode segment."
  :group 'taoline)

;; ----------------------------------------------------------------------------
;; Internal state

(defvar taoline--segment-table (make-hash-table :test 'eq)
  "Hash-table storing registered segment functions.")

(defvar taoline--last-str ""
  "Last string that was rendered, used to avoid redundant redisplay.")

(defvar taoline--default-mode-line-format-backup nil
  "Backup of `default-mode-line-format' when modeline is hidden.")

(defvar taoline--resize-mini-windows-backup nil
  "Backup of `resize-mini-windows' when `taoline-mode` toggles.")

(defvar-local taoline--saved-mode-line-format nil
  "Buffer-local backup of `mode-line-format` used while `taoline-mode` hides the classical modeline.")

;; ----------------------------------------------------------------------------
;; Helpers for (auto)hiding the classical modeline

(defun taoline--set-modeline-format-globally (value &optional backup-restore)
  "Set `mode-line-format' to VALUE for all buffers / windows.
If BACKUP-RESTORE is non-nil, take/restore backup of default."
  (when backup-restore
    (if (eq value :default)
        (when taoline--default-mode-line-format-backup
          (setq-default mode-line-format taoline--default-mode-line-format-backup)
          (setq taoline--default-mode-line-format-backup nil))
      (unless taoline--default-mode-line-format-backup
        (setq taoline--default-mode-line-format-backup (default-value 'mode-line-format)))))
  (let ((final (if (eq value :default)
                   (default-value 'mode-line-format)
                 value)))
    (setq-default mode-line-format final)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq-local mode-line-format final))))
  (force-mode-line-update t))

(defun taoline--autohide-modeline-globally ()
  "Hide the classic mode-line in all existing and future buffers.

The current value of `mode-line-format` is stored in the buffer-local
variable `taoline--saved-mode-line-format` so that it can be restored
verbatim when `taoline-mode` is turned off."
  ;; Save the global default (only once) and blank it so *new* buffers
  ;; inherit the hidden state.
  (unless taoline--default-mode-line-format-backup
    (setq taoline--default-mode-line-format-backup
          (default-value 'mode-line-format)))
  (setq-default mode-line-format nil)
  ;; Hide mode-line in all currently existing buffers while remembering
  ;; their individual value.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (unless (local-variable-p 'taoline--saved-mode-line-format)
        (setq-local taoline--saved-mode-line-format mode-line-format))
      (setq-local mode-line-format nil)))
  (force-mode-line-update t))

(defun taoline--unhide-modeline-globally ()
  "Restore the classic mode-line in every buffer.

The value saved in `taoline--saved-mode-line-format` is put back and the
helper variable is cleaned up."
  ;; Restore global default.
  (when taoline--default-mode-line-format-backup
    (setq-default mode-line-format taoline--default-mode-line-format-backup)
    (setq taoline--default-mode-line-format-backup nil))
  ;; Restore per-buffer values.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (local-variable-p 'taoline--saved-mode-line-format)
        (setq-local mode-line-format taoline--saved-mode-line-format)
        (kill-local-variable 'taoline--saved-mode-line-format))))
  (force-mode-line-update t))

;; ----------------------------------------------------------------------------
;; Segment definition macros

(defmacro taoline-define-segment (name args &rest body)
  "Define segment NAME taking ARGS, register it, and return its symbol."
  (declare (indent defun))
  (let ((func `(defun ,name ,args ,@body)))
    `(progn
       ,func
       (puthash ',name #',name taoline--segment-table)
       ',name)))

(defmacro taoline-define-simple-segment (name docstring &rest body)
  "Define segment NAME with no args; BODY is executed each render."
  `(taoline-define-segment ,name ()
     ,docstring
     ,@body))

;; ----------------------------------------------------------------------------
;; Segment helper functions

(defun taoline--apply-segment (fn buffer)
  "Call segment FN with BUFFER or without – return string, never nil."
  (condition-case err
      (let* ((arity (help-function-arglist fn t))
             (res (if (and (consp arity) (not (null arity)))
                      (funcall fn buffer)
                    (funcall fn))))
        (if (stringp res) res (or (and res (format "%s" res)) "")))
    (error (propertize (format "[SEGMENT ERROR: %s]" err) 'face 'error))))

(defun taoline--collect-segments (side buffer)
  "Render all segments for SIDE (:left/:center/:right) inside BUFFER."
  (cl-loop for fn in (cdr (assq side taoline-segments))
           for str = (taoline--apply-segment fn buffer)
           unless (or (null str) (string-empty-p str))
           collect str))

;; ----------------------------------------------------------------------------
;; Core: available center width (pure)

(defun taoline-available-center-width (&optional buffer window width)
  "Return the maximum possible width for the center segment (excluding right-padding).
Takes into account current left and right segments, including needed separator spaces."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         (left   (mapconcat #'identity (taoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (taoline--collect-segments :right buffer) " "))
         (left-w (string-width left))
         (right-w (string-width right))
         (space-left  (if (and (not (string-empty-p left))
                               (not (string-empty-p right)))
                          1 0))
         ;; If center segment is empty, no space needed before or after
         ;; If both exist, at least one space before center
         (space-center-left (if (and (not (string-empty-p left))
                                     ;; center will be inserted anyway, always leave space
                                     )
                                1 0))
         (space-center-right (if (and (not (string-empty-p right)))
                                 1 0)))
    (max 0 (- width
              left-w
              right-w
              ;; NB! We only leave spaces where segments exist
              (if (not (string-empty-p left)) 1 0)
              (if (not (string-empty-p right)) 1 0)
              taoline-right-padding))))

;; ----------------------------------------------------------------------------
;; Core: compose function (pure)

(defun taoline-compose-modeline (&optional buffer window width)
  "Compose a Taoline string that is guaranteed to fit within WIDTH.
This function is pure: it merely returns the string and causes no side effects."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         (left   (mapconcat #'identity (taoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (taoline--collect-segments :right buffer) " "))
         (center (mapconcat #'identity (taoline--collect-segments :center buffer) " "))
         (left-w (string-width left))
         (right-w (string-width right))
         ;; Always put one space before center (echo) segment
         (space-left " ")
         (space-right (if (not (string-empty-p right)) " " ""))
         ;; The maximum room for center segment
         (available
          (max 0 (- width
                    left-w
                    right-w
                    (string-width space-left)
                    (string-width space-right)
                    taoline-right-padding)))
         ;; Строка-центр растягивается НА ВСЮ доступную ширину,
         ;; с обрезанием только если не помещается.
         (center-str
          (if (> (string-width center) available)
              (truncate-string-to-width center available 0 ?\s)
            (concat center (make-string (- available (string-width center)) ?\s)))))
    (let ((final (concat
                  left
                  space-left
                  center-str
                  space-right
                  right
                  (make-string taoline-right-padding ?\s))))
      ;; ensure we never exceed the echo-area width
      (truncate-string-to-width final width 0 ?\s))))

;; ----------------------------------------------------------------------------
;; Display/update

(defun taoline--display (str)
  "Show STR in echo-area if different from last output or when echo-area is empty."
  (let ((cur-msg (current-message)))
    (when (or (not (string-equal str taoline--last-str))
              (null cur-msg))
      (setq taoline--last-str str)
        (let ((message-log-max nil))
        (message "%s" str)))))

(defun taoline--update (&rest _)
  "Recompute modeline for `selected-window' and display.
Skips update (and clears) during isearch or minibuffer input."
  (taoline--log "taoline--update")
  (if (or (active-minibuffer-window)
          (bound-and-true-p isearch-mode))
      (taoline--clear-display)
    (taoline--display
     (taoline-compose-modeline))))

(defun taoline--clear-display ()
  "Clear echo-area if last output belongs to taoline."
  (when (and (stringp taoline--last-str)
             (string= (current-message) taoline--last-str))
    (message nil))
  (setq taoline--last-str ""))

;; ----------------------------------------------------------------------------
;; Minor mode

(defcustom taoline-timer-interval 10
  "Interval in seconds for updating time and battery segments.
The timer will not run more often than this interval."
  :type 'number
  :group 'taoline)

;; --------------------------------------------------------------------------
;;  NEW message-timeout logic (config, timer & helpers)

(defcustom taoline-message-timeout 10
  "Seconds to keep a regular `message' in the echo area before Taoline
re-draws its modeline.  A value of 0 disables the timeout (Taoline will
stay hidden until some other update is triggered)."
  :type 'number
  :group 'taoline)

(defvar taoline--redisplay-timer nil
  "Idle timer started after an ordinary `message'; forces a Taoline
refresh once `taoline-message-timeout' seconds elapsed.")

(defun taoline--cancel-redisplay-timer ()
  "Cancel and clear `taoline--redisplay-timer' if it is active."
  (when (timerp taoline--redisplay-timer)
    (cancel-timer taoline--redisplay-timer)
    (setq taoline--redisplay-timer nil)))

(defun taoline--schedule-redisplay ()
  "Start (or restart) `taoline--redisplay-timer'."
  (taoline--cancel-redisplay-timer)
  (when (and taoline-message-timeout
             (> taoline-message-timeout 0))
    (setq taoline--redisplay-timer
          (run-at-time taoline-message-timeout
                       nil
                       #'taoline--update))))

(defun taoline--message-filter-return (result &rest _args)
  "Make Taoline coexist politely with ordinary `message' output.

When `taoline-mode' is active we keep the user's message in the echo
area for at least `taoline-message-timeout' seconds.  A non-empty RESULT
coming from `message' restarts that countdown.  Empty RESULTS neither
erase the previous text nor force an immediate Taoline redraw – the
pending timer (if any) will handle that."
  (when taoline-mode
    (cond
     ;; New, non-empty message → (re)start the timer.
     ((and (stringp result) (not (string-empty-p result)))
      (taoline--schedule-redisplay))
     ;; RESULT is empty – if no timer is running, schedule one so that
     ;; Taoline will eventually return.
     ((null taoline--redisplay-timer)
      (taoline--schedule-redisplay))))
  result)
  

;;  Ensure the timer is cleaned up whenever the global minor-mode is toggled.
(add-hook 'taoline-mode-hook #'taoline--cancel-redisplay-timer)

(defvar taoline--timer nil
  "Internal timer used by `taoline-mode' for periodic updates.")

;;;###autoload
(define-minor-mode taoline-mode
  "Global minor mode that displays a functional minimalist modeline in echo-area."
  :global t
  :lighter ""
  (if taoline-mode
      (progn
        (when taoline-autohide-modeline
          (taoline--autohide-modeline-globally))
        ;; Backup and disable vertical resizing of the echo area
        (setq taoline--resize-mini-windows-backup resize-mini-windows)
        (setq resize-mini-windows nil)
        (dolist (hook taoline-update-hooks)
          (add-hook hook #'taoline--update))
        (advice-add 'message :filter-return #'taoline--message-filter-return)
        ;; Hide taoline when echo-area is used for input, then restore afterwards
        (add-hook 'minibuffer-setup-hook    #'taoline--clear-display)
        (add-hook 'minibuffer-exit-hook     #'taoline--update)
        (add-hook 'isearch-mode-hook        #'taoline--clear-display)
        (add-hook 'isearch-mode-end-hook    #'taoline--update)
        (taoline--update)
        (setq taoline--timer
              (run-with-timer taoline-timer-interval
                              taoline-timer-interval
                              #'taoline--update)))
    ;; turn off
    (dolist (hook taoline-update-hooks)
      (remove-hook hook #'taoline--update))
    (advice-remove 'message #'taoline--message-filter-return)
    (taoline--clear-display)
    ;; Remove our input hooks
    (remove-hook 'minibuffer-setup-hook    #'taoline--clear-display)
    (remove-hook 'minibuffer-exit-hook     #'taoline--update)
    (remove-hook 'isearch-mode-hook        #'taoline--clear-display)
    (remove-hook 'isearch-mode-end-hook    #'taoline--update)
    (when taoline-autohide-modeline
      (taoline--unhide-modeline-globally))
    ;; Cancel the periodic timer
    (when (timerp taoline--timer)
      (cancel-timer taoline--timer)
      (setq taoline--timer nil))
    ;; Restore minibuffer resize behavior
    (when taoline--resize-mini-windows-backup
      (setq resize-mini-windows taoline--resize-mini-windows-backup)
      (setq taoline--resize-mini-windows-backup nil))))

;; ----------------------------------------------------------------------------
;; Segments
;; ---------------------------------------------------------------------------

(defun taoline--get-buffer-icon (buffer)
  "Return a buffer icon, possibly using `all-the-icons'.
Chooses color/iconation similar to tabs: prefers icon-for-mode (colored), else
icon-for-file, else default faicon."
  (when (require 'all-the-icons nil t)
    (let* ((mode (buffer-local-value 'major-mode buffer))
           (raw-icon (all-the-icons-icon-for-mode mode :height 0.9))
           (icon
            (cond
             ((stringp raw-icon) raw-icon)
             ((buffer-file-name buffer)
              (all-the-icons-icon-for-file
               (buffer-file-name buffer) :height 0.9))
             (t
              (all-the-icons-faicon "file-o" :height 0.9)))))
      icon)))

(defconst taoline--icon-width 3
  "Fixed icon width (in display columns) for taoline buffer icons.
You may need to adapt this for your font & setup.")

(taoline-define-segment taoline-segment-icon-and-buffer (buffer)
  "Цветная иконка буфера (по режиму, как во вкладках) + имя буфера.
➤ Например:  README.org — иконка будет цветной и соответствовать файлу/режиму."
  (let* ((icon (taoline--get-buffer-icon buffer)) ;; Теперь — цветная иконка, та же логика.
         (name (buffer-name buffer))
         (text (if icon (concat icon " " name) name)))
    ;; Face только к имени (иконка останется цветной/оформленной, не затрагивается face).
    (add-face-text-property
     (if icon (length icon) 0) (length text)
     'taoline-buffer-face 'append
     text)
    text))

;; Project name (for example, Projectile)
(taoline-define-simple-segment taoline-segment-project-name
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
      (propertize project 'face 'taoline-project-face))))

;; Git branch (projectile / vc-git)
(taoline-define-simple-segment taoline-segment-git-branch
  "Current Git branch."
  (when (and (featurep 'vc-git) (buffer-file-name))
    (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
      (when branch
        (concat
         (all-the-icons-octicon "git-branch" :v-adjust 0 :height 1.0 :face 'taoline-git-face)
         " "
         (propertize branch 'face 'taoline-git-face))))))

;; Project name (projectile)
(taoline-define-simple-segment taoline-segment-project-name
  "Project name via projectile."
  (when (and (featurep 'projectile) (projectile-project-p))
    (concat
     (all-the-icons-faicon "folder-o" :v-adjust 0 :height 1.0 :face 'taoline-base-face)
     " "
     (projectile-project-name))))

;; Echo-area message segment (for demonstration – shows `current-message`)
(taoline-define-simple-segment taoline-segment-echo-message
  "Current echo message excluding taoline itself, растянутый на всю ширину между левым и правым сегментами."
  (let* ((msg  (current-message))
         (max-width (taoline-available-center-width)) ;; автоширина!
         ;; Show the message only if it's not the last taoline string
         (show (unless (or (null msg)
                           (string-equal msg taoline--last-str))
                 msg))
         (shown-str (or show ""))
         (padded-str
          ;; Растягиваем, если надо — дополнительно пробелы вправо!
          (if (>= (string-width shown-str) max-width)
              (truncate-string-to-width shown-str max-width 0 ?\s)
            (concat shown-str (make-string (- max-width (string-width shown-str)) ?\s)))))
    (propertize padded-str 'face 'taoline-echo-face)))

;; Battery segment (if `battery` is available)
(taoline-define-simple-segment taoline-segment-battery
  "Battery status with icon."
  (when (and (fboundp 'battery) battery-status-function)
    (let* ((data (and battery-status-function (funcall battery-status-function)))
           (percent (cdr (assoc ?p data)))
           (status  (cdr (assoc ?B data))) ;; charging, discharging, etc.
           (icon
            (cond
             ((not (featurep 'all-the-icons)) "")
             ((and status (string-match-p "AC" status))
              (all-the-icons-octicon "plug" :face 'taoline-battery-face :height 1.0 :v-adjust 0))
             ((and status (string-match-p "Charging" status))
              (all-the-icons-faicon "bolt" :face 'taoline-battery-face :height 1.0 :v-adjust 0))
             ((and percent (string-match "\\([0-9]+\\)" percent))
              (let* ((n (string-to-number (match-string 1 percent))))
                (cond
                 ((>= n 95) (all-the-icons-faicon "battery-full" :face 'taoline-battery-face :height 1.0 :v-adjust 0))
                 ((>= n 75) (all-the-icons-faicon "battery-three-quarters" :face 'taoline-battery-face :height 1.0 :v-adjust -0.1))
                 ((>= n 50) (all-the-icons-faicon "battery-half" :face 'taoline-battery-face :height 1.0 :v-adjust 0))
                 ((>= n 25) (all-the-icons-faicon "battery-quarter" :face 'taoline-battery-face :height 1.0 :v-adjust 0))
                 (t (all-the-icons-faicon "battery-empty" :face 'taoline-battery-face :height 1.0 :v-adjust 0)))))
             (t ""))))
      (when percent
        (concat
         (when (and (stringp icon) (not (string-empty-p icon)))
           (concat icon " "))
         (propertize
          (concat (replace-regexp-in-string "%" "" percent) "%")
          'face 'taoline-battery-face))))))

;; Major-mode
(taoline-define-simple-segment taoline-segment-major-mode
  "Major mode segment with icon."
  (let ((icon (when (and (featurep 'all-the-icons) major-mode)
                (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust 0))))
    (concat
     ;; if `icon` is a string (not a symbol placeholder), show it
     (when (and icon (stringp icon))
       (concat icon " "))
     (propertize
      (format-mode-line mode-name)
      'face 'taoline-mode-face))))

;; Time segment
(taoline-define-simple-segment taoline-segment-time
  "Current time."
  (propertize (format-time-string "%H:%M") 'face 'taoline-time-face))

;; ---------------------------------------------------------------------------

(provide 'taoline)
;;; taoline.el ends here
