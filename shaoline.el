;;; shaoline.el --- Functional minimalist echo-area modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Peter
;;
;; Author: Peter <11111000000@email.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mode-line, minimal, functional, echo-area
;; URL: https://github.com/11111000000/SHAOLINE
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: MIT

;;; Commentary:

;; In the land of the rising buffer, modelines wandered the world in excess—
;; until Shaoline arrived, shaved its segments, and sat quietly in the echo area.
;; The true path has neither timers nor distractions.
;;
;; Shaoline-echo is a trimmed-down edition of the original: a Shaolin monk
;; among modelines. There is no child-frame and no in-window modeline —
;; the only rendering target is the echo area (the minibuffer). The architecture
;; remains functional: each segment is a pure function, unaffected by worldly
;; state, and the only side effects are writing to the echo area and (optionally)
;; hiding the regular `mode-line` (so your modeline too may attain Nirvana).
;;
;; Key features (as written upon a bamboo leaf):
;;   • Minimalist, declarative segment configuration. Add, remove, or swap
;;     segments like stones in the Zen garden.
;;   • No polling timers — the modeline is refreshed only by the wind
;;     (or `post-command-hook`, etc.).
;;   • Global minor mode `shaoline-mode` toggles everything with serenity.
;;   • Optional hiding of the traditional mode line, for those seeking emptiness.
;;   • A unit-test-friendly “core” — the composing function is a silent
;;     observer, causing no side effects (Wu Wei by design).
;;
;; Quick start:
;;
;;   (require 'shaoline-echo)
;;   (shaoline-mode 1)
;;
;; See the README on GitHub for scrolls and stories about writing custom segments.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'rx))
(require 'projectile nil t)
(require 'all-the-icons nil t)
(require 'battery nil t)
(require 'calendar)

;; Helper: compute moon phase index 0..7 for a given date.
;; 0 = new moon, 4 = full moon, etc.
(defun calendar-phase-of-moon (&optional date)
  "Return moon phase index 0..7 for DATE (Gregorian list). Defaults to today.
0 = new moon, 4 = full moon."
  (let* ((d        (or date (calendar-current-date)))
         (abs-day  (float (calendar-absolute-from-gregorian d)))
         (synodic  29.530588853)  ; length of synodic month
         ;; age in days since last new moon:
         (age      (- abs-day
                       (* (floor (/ abs-day synodic)) synodic)))
         ;; scale age to 0..8, then floor to 0..7
         (idx      (mod (floor (* age (/ 8.0 synodic))) 8)))
    idx))

;; Helper: compute moon phase index 0..7 for a given date.
;; 0 = new moon, 4 = full moon, etc.
(defun calendar-phase-of-moon (&optional date)
  "Return moon phase index 0..7 for DATE (Gregorian list). Defaults to today.
0 = new moon, 4 = full moon."
  (let* ((d        (or date (calendar-current-date)))
         (abs-day  (float (calendar-absolute-from-gregorian d)))
         (synodic  29.530588853)  ; length of synodic month
         ;; age in days since last new moon:
         (age      (- abs-day
                       (* (floor (/ abs-day synodic)) synodic)))
         ;; scale age to 0..8, then floor to 0..7
         (idx      (mod (floor (* age (/ 8.0 synodic))) 8)))
    idx))

;; ----------------------------------------------------------------------------
;; Customization group and variables
;;
;; If you seek change, like the autumn leaf, customize here.

(defgroup shaoline nil
  "Functional minimalist echo-area modeline."
  :group 'convenience
  :prefix "shaoline-")

(defcustom shaoline-icon-width 2
  "Width in characters to pad or truncate all-the-icons icons for consistent spacing.
Increase if your favorite icons are wider."
  :type 'integer
  :group 'shaoline)


(defcustom shaoline-debug t
  "If non-nil, log shaoline activity into `shaoline--log-buffer'. Seek inner debugness."
  :type 'boolean
  :group 'shaoline)

(defvar shaoline--log-buffer "*shaoline-logs*"
  "Buffer name used for debug log. A quiet scroll for Shaoline whispers.")

(defun shaoline--log (fmt &rest args)
  "Write formatted FMT/ARGS message to `shaoline--log-buffer' if `shaoline-debug' is non-nil.
Zen masters say: A log unread is a tree falling in a silent forest."
  (when shaoline-debug
    (let ((msg (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create shaoline--log-buffer)
        (goto-char (point-max))
        (insert msg "\n")))))

(defcustom shaoline-segments
  '((:left shaoline-segment-icon-and-buffer shaoline-segment-git-branch)
    (:center shaoline-segment-echo-message)
    (:right shaoline-segment-project-name shaoline-segment-battery shaoline-segment-time))
  "Alist describing segments for :left, :center and :right.
Each entry is a list of segment function symbols for that side.
Tweak segments as calmly as rearranging stones: simply, purposefully."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'shaoline)

(defcustom shaoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "Hooks that tell Shaoline to recompute and display the modeline."
  :type '(repeat symbol)
  :group 'shaoline)

(defcustom shaoline-autohide-modeline t
  "When non-nil, the traditional mode-line is hidden while `shaoline-mode' is active."
  :type 'boolean
  :group 'shaoline)

(defcustom shaoline-right-padding 16
  "Extra spaces appended to the right edge of the shaoline. Sometimes, a little emptiness is all you need."
  :type 'integer
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; Faces

(defface shaoline-base-face
  '((t :inherit default
       :height 1.0
       :box nil
       :underline nil
       :overline nil
       :inverse-video nil
       :extend t))
  "Base face for shaoline. The foundation for all modeline elements."
  :group 'shaoline)

(defface shaoline-echo-face
  '((t :inherit (font-lock-comment-face shaoline-base-face) :height 1.0))
  "Face for the echo message segment (slightly smaller)."
  :group 'shaoline)

(defface shaoline-time-face
  '((t :background "#002200" :foreground "#00aa00" :height 1.0 :bold nil :family "Digital Display"))
  "Face for the time segment."
  :group 'shaoline)

(defface shaoline-battery-face
  '((t :inherit (success shaoline-base-face) :height 0.8))
  "Face for battery level segment."
  :group 'shaoline)

(defface shaoline-buffer-face
  '((t :inherit (shaoline-base-face) :height 1.0))
  "Face for the buffer name segment."
  :group 'shaoline)

(defface shaoline-project-face
  '((t :inherit (font-lock-keyword-face shaoline-base-face) :height 1.0))
  "Face for the project name segment."
  :group 'shaoline)

(defface shaoline-modified-face
  '((t :inherit (warning shaoline-base-face) :height 1.0))
  "Face for the modified indicator segment."
  :group 'shaoline)

(defface shaoline-git-face
  '((t :inherit (font-lock-type-face shaoline-base-face) :height 1.0))
  "Face for the git branch segment."
  :group 'shaoline)

(defface shaoline-mode-face
  '((t :inherit (font-lock-type-face shaoline-base-face) :height 1.0))
  "Face for the major mode segment."
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; Internal state

(defvar shaoline--segment-table (make-hash-table :test 'eq)
  "Hash-table storing registered segment functions.")

(defvar shaoline--last-str ""
  "Last rendered string, used to avoid unnecessary redisplay. Repetition is not enlightenment.")

(defvar shaoline--default-mode-line-format-backup nil
  "Backup of `default-mode-line-format' when the modeline is hidden (restored when Shaoline is disabled).")

(defvar shaoline--resize-mini-windows-backup nil
  "Backup of `resize-mini-windows' when `shaoline-mode` toggles. Settings are always restored on exit.")

(defvar-local shaoline--saved-mode-line-format nil
  "Buffer-local backup of `mode-line-format` for use while `shaoline-mode` is hiding the classic modeline.
Restored exactly as it was when the mode is toggled off.")

;; ----------------------------------------------------------------------------
;; Helpers for (auto)hiding the classic mode-line
;;
;; Sometimes, seeing less helps you perceive more.

(defun shaoline--set-modeline-format-globally (value &optional backup-restore)
  "Set `mode-line-format' to VALUE for all buffers and windows.
If BACKUP-RESTORE is non-nil, take or restore backup of the default setting."
  (when backup-restore
    (if (eq value :default)
        (when shaoline--default-mode-line-format-backup
          (setq-default mode-line-format shaoline--default-mode-line-format-backup)
          (setq shaoline--default-mode-line-format-backup nil))
      (unless shaoline--default-mode-line-format-backup
        (setq shaoline--default-mode-line-format-backup (default-value 'mode-line-format)))))
  (let ((final (if (eq value :default)
                   (default-value 'mode-line-format)
                 value)))
    (setq-default mode-line-format final)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq-local mode-line-format final))))
  (force-mode-line-update t))

(defun shaoline--autohide-modeline-globally ()
  "Hide the classic mode-line in all current and future buffers.

The current value of `mode-line-format` is stored buffer-locally as `shaoline--saved-mode-line-format`,
allowing exact restoration when `shaoline-mode` is disabled."
  ;; Save the global default (only once) and blank it so *new* buffers
  ;; inherit the hidden state.
  (unless shaoline--default-mode-line-format-backup
    (setq shaoline--default-mode-line-format-backup
          (default-value 'mode-line-format)))
  (setq-default mode-line-format nil)
  ;; Hide the mode-line in all currently existing buffers while remembering
  ;; their individual values.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (unless (local-variable-p 'shaoline--saved-mode-line-format)
        (setq-local shaoline--saved-mode-line-format mode-line-format))
      (setq-local mode-line-format nil)))
  (force-mode-line-update t))

(defun shaoline--unhide-modeline-globally ()
  "Restore the classic mode-line in every buffer.

The value saved in `shaoline--saved-mode-line-format` is restored and the marker removed."
  ;; Restore the global default.
  (when shaoline--default-mode-line-format-backup
    (setq-default mode-line-format shaoline--default-mode-line-format-backup)
    (setq shaoline--default-mode-line-format-backup nil))
  ;; Restore per-buffer values.
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (local-variable-p 'shaoline--saved-mode-line-format)
        (setq-local mode-line-format shaoline--saved-mode-line-format)
        (kill-local-variable 'shaoline--saved-mode-line-format))))
  (force-mode-line-update t))

;; ----------------------------------------------------------------------------
;; Segment definition macros


(defmacro shaoline-define-segment (name args &rest body)
  "Define segment NAME taking ARGS, register it, and return its symbol.
Every segment is independent; every function a ripple in the pond."
  (declare (indent defun))
  (let ((func `(defun ,name ,args ,@body)))
    `(progn
       ,func
       (puthash ',name #',name shaoline--segment-table)
       ',name)))

(defmacro shaoline-define-simple-segment (name docstring &rest body)
  "Define a segment NAME with no arguments. BODY is run on each render.
True elegance: nothing more than necessary."
  `(shaoline-define-segment ,name ()
     ,docstring
     ,@body))

;; ----------------------------------------------------------------------------
;; Segment helper functions

(defun shaoline--apply-segment (fn buffer)
  "Call segment FN with BUFFER (or with no args if not needed) and return its string.
Errors are returned as a diagnostic string, not signalled."
  (condition-case err
      (let* ((arity (help-function-arglist fn t))
             (res (if (and (consp arity) (not (null arity)))
                      (funcall fn buffer)
                    (funcall fn))))
        (if (stringp res) res (or (and res (format "%s" res)) "")))
    (error (propertize (format "[SEGMENT ERROR: %s]" err) 'face 'error))))

(defun shaoline--collect-segments (side buffer)
  "Render all segments for SIDE (:left, :center, :right) within BUFFER.
Only non-empty results are included."
  (cl-loop for fn in (cdr (assq side shaoline-segments))
           for str = (shaoline--apply-segment fn buffer)
           unless (or (null str) (string-empty-p str))
           collect str))

;; ----------------------------------------------------------------------------
;; Core: calculating available center width (pure)

(defun shaoline-available-center-width (&optional buffer window width)
  "Return the maximum width available for the center segment (excluding right-padding).
Calculates based on the length of left and right segments, including any necessary spacing."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         (left   (mapconcat #'identity (shaoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (shaoline--collect-segments :right buffer) " "))
         (left-w (string-width left))
         (right-w (string-width right))
         (space-left  (if (and (not (string-empty-p left))
                               (not (string-empty-p right)))
                          1 0))
         ;; If the center segment is empty, no space is needed before/after.
         ;; If both sides exist, one space is left before the center.
         (space-center-left (if (and (not (string-empty-p left)))
                                1 0))
         (space-center-right (if (not (string-empty-p right))
                                 1 0)))
    (max 0 (- width
              left-w
              right-w
              ;; NOTE: We only leave spaces where segments exist
              (if (not (string-empty-p left)) 1 0)
              (if (not (string-empty-p right)) 1 0)
              shaoline-right-padding))))

;; ----------------------------------------------------------------------------
;; Core: compose the modeline (pure version)

(defun shaoline-compose-modeline (&optional buffer window width)
  "Compose a Shaoline string that fits within WIDTH.
This function is pure and returns a string with no side effects.
What appears is simply what fits."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         (left   (mapconcat #'identity (shaoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (shaoline--collect-segments :right buffer) " "))
         (center (mapconcat #'identity (shaoline--collect-segments :center buffer) " "))
         (left-w (string-width left))
         (right-w (string-width right))
         ;; Always insert a space before the center (echo) segment
         (space-left " ")
         (space-right (if (not (string-empty-p right)) " " ""))
         ;; Calculate maximum available space for the center
         (available
          (max 0 (- width
                    left-w
                    right-w
                    (string-width space-left)
                    (string-width space-right)
                    shaoline-right-padding)))
         ;; Pad or truncate the center string as needed
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
                  (make-string shaoline-right-padding ?\s))))
      ;; Ensure we never exceed the echo area width
      (truncate-string-to-width final width 0 ?\s))))

;; ----------------------------------------------------------------------------
;; Display/update

(defun shaoline--display (str)
  "Show STR in the echo area, only if it's different from the previous output or if the echo area is empty."
  (let ((cur-msg (current-message)))
    (when (or (not (string-equal str shaoline--last-str))
              (null cur-msg))
      (setq shaoline--last-str str)
      (let ((message-log-max nil))
        (message "%s" str)))))

(defun shaoline--update (&rest _)
  "Recompute the modeline for the `selected-window` and display.
Skips update (and clears) during isearch or minibuffer input.
The best display is sometimes none at all."
  (shaoline--log "shaoline--update")
  (if (or (active-minibuffer-window)
          (bound-and-true-p isearch-mode))
      (shaoline--clear-display)
    (shaoline--display
     (shaoline-compose-modeline))))

(defun shaoline--clear-display ()
  "Clear the echo area if the last output was from Shaoline.
Sometimes, true clarity is emptiness."
  (when (and (stringp shaoline--last-str)
             (string= (current-message) shaoline--last-str))
    (message nil))
  (setq shaoline--last-str ""))

;; ----------------------------------------------------------------------------
;; Minor mode

(defcustom shaoline-timer-interval 15
  "Interval in seconds for updating time and battery segments.
Even time updates only when it ought to—not a second sooner."
  :type 'number
  :group 'shaoline)

;; --------------------------------------------------------------------------
;;  Message-timeout logic (config, timer & helpers)

(defcustom shaoline-message-timeout 10
  "Seconds to keep a regular `message` in the echo area before Shaoline
redraws its modeline. If set to 0, the modeline is not redrawn automatically
(until something else triggers it)."
  :type 'number
  :group 'shaoline)

(defvar shaoline--redisplay-timer nil
  "Idle timer started after an ordinary `message`; forces a Shaoline
refresh when `shaoline-message-timeout` seconds have elapsed.")

(defun shaoline--cancel-redisplay-timer ()
  "Cancel and clear `shaoline--redisplay-timer` if active."
  (when (timerp shaoline--redisplay-timer)
    (cancel-timer shaoline--redisplay-timer)
    (setq shaoline--redisplay-timer nil)))

(defun shaoline--schedule-redisplay ()
  "Start (or restart) `shaoline--redisplay-timer'."
  (shaoline--cancel-redisplay-timer)
  (when (and shaoline-message-timeout
             (> shaoline-message-timeout 0))
    (setq shaoline--redisplay-timer
          (run-at-time shaoline-message-timeout
                       nil
                       #'shaoline--update))))

(defun shaoline--message-filter-return (result &rest _args)
  "Let Shaoline play nicely with regular `message' output.

When `shaoline-mode' is active, retain the user's message in the echo area
for at least `shaoline-message-timeout` seconds. A non-empty RESULT (from `message`)
restarts the timer; empty results neither erase text nor force a redraw.
A pending timer will eventually bring back Shaoline's modeline."
  (when shaoline-mode
    (cond
     ;; New, non-empty message – restart the timer.
     ((and (stringp result) (not (string-empty-p result)))
      (shaoline--schedule-redisplay))
     ;; Empty result – if no timer is running, start one so Shaoline returns.
     ((null shaoline--redisplay-timer)
      (shaoline--schedule-redisplay))))
  result)


;;  Ensure the timer is cleaned up whenever the global minor-mode is toggled.
(add-hook 'shaoline-mode-hook #'shaoline--cancel-redisplay-timer)

(defvar shaoline--timer nil
  "Internal timer used by `shaoline-mode' for periodic updates.")

(require 'cl-lib)

;;;###autoload
(define-minor-mode shaoline-mode
  "Global minor mode that displays a functional minimalist modeline in echo-area."
  :global t
  :lighter ""
  (if shaoline-mode
      (progn
        (when shaoline-autohide-modeline
          (shaoline--autohide-modeline-globally))
        ;; Backup and disable vertical resizing of the echo area
        (setq shaoline--resize-mini-windows-backup resize-mini-windows)
        (setq resize-mini-windows nil)
        (dolist (hook shaoline-update-hooks)
          (add-hook hook #'shaoline--update))
        (advice-add 'message :filter-return #'shaoline--message-filter-return)
        ;; Hide shaoline when echo-area is used for input, then restore afterwards
        (add-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
        (add-hook 'minibuffer-exit-hook     #'shaoline--update)
        (add-hook 'isearch-mode-hook        #'shaoline--clear-display)
        (add-hook 'isearch-mode-end-hook    #'shaoline--update)
        (shaoline--update)
        (setq shaoline--timer
              (run-with-timer shaoline-timer-interval
                              shaoline-timer-interval
                              #'shaoline--update)))
    ;; turn off
    (dolist (hook shaoline-update-hooks)
      (remove-hook hook #'shaoline--update))
    (advice-remove 'message #'shaoline--message-filter-return)
    (shaoline--clear-display)
    ;; Remove our input hooks
    (remove-hook 'minibuffer-setup-hook    #'shaoline--clear-display)
    (remove-hook 'minibuffer-exit-hook     #'shaoline--update)
    (remove-hook 'isearch-mode-hook        #'shaoline--clear-display)
    (remove-hook 'isearch-mode-end-hook    #'shaoline--update)
    (when shaoline-autohide-modeline
      (shaoline--unhide-modeline-globally))
    ;; Cancel the periodic timer
    (when (timerp shaoline--timer)
      (cancel-timer shaoline--timer)
      (setq shaoline--timer nil))
    ;; Restore minibuffer resize behavior
    (when shaoline--resize-mini-windows-backup
      (setq resize-mini-windows shaoline--resize-mini-windows-backup)
      (setq shaoline--resize-mini-windows-backup nil))))

;; ----------------------------------------------------------------------------
;; Segments
;; ---------------------------------------------------------------------------

(defun shaoline--get-buffer-icon (buffer)
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

(defconst shaoline--icon-width 3
  "Fixed icon width (in display columns) for shaoline buffer icons.
You may need to adapt this for your font & setup.")

(shaoline-define-segment shaoline-segment-icon-and-buffer (buffer)
  "Цветная иконка буфера (по режиму, как во вкладках) + имя буфера.
➤ Например:  README.org — иконка будет цветной и соответствовать файлу/режиму."
  (let* ((icon (shaoline--get-buffer-icon buffer)) ;; Теперь — цветная иконка, та же логика.
         (name (buffer-name buffer))
         (text (if icon (concat icon " " name) name)))
    ;; Face только к имени (иконка останется цветной/оформленной, не затрагивается face).
    (add-face-text-property
     (if icon (length icon) 0) (length text)
     'shaoline-buffer-face 'append
     text)
    text))

(shaoline-define-segment shaoline-segment-icon-and-buffer (buffer)
  "Icon for BUFFER (padded/truncated to `shaoline-icon-width`) and its name."
  (let* ((raw-icon (and (fboundp 'all-the-icons-icon-for-buffer)
                        (with-current-buffer buffer
                          (all-the-icons-icon-for-buffer))))
         (icon      (or raw-icon ""))
         (width     (max 0 (or (and (boundp 'shaoline-icon-width)
                                    shaoline-icon-width)
                               2)))
         (icon-str
          ;; pad or truncate the icon string to `width`
          (let ((w (string-width icon)))
            (cond
             ((< w width)    (concat icon (make-string (- width w) ?\s)))
             ((> w width)    (truncate-string icon width))
             (t               icon))))
         (buf-name  (buffer-name buffer)))
    (concat icon-str " " buf-name)))

;; Project name (for example, Projectile)
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

;; Git branch (projectile / vc-git)
(shaoline-define-simple-segment shaoline-segment-git-branch
  "Current Git branch."
  (when (and (featurep 'vc-git) (buffer-file-name))
    (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
      (when branch
        (concat
         (all-the-icons-octicon "git-branch" :v-adjust 0 :height 1.0 :face 'shaoline-git-face)
         " "
         (propertize branch 'face 'shaoline-git-face))))))

;; Project name (projectile)
(shaoline-define-simple-segment shaoline-segment-project-name
  "Project name via projectile."
  (when (and (featurep 'projectile) (projectile-project-p))
    (concat
     (all-the-icons-faicon "folder-o" :v-adjust 0 :height 1.0 :face 'shaoline-base-face)
     " "
     (projectile-project-name))))

;; Echo-area message segment (for demonstration – shows `current-message`)
(shaoline-define-simple-segment shaoline-segment-echo-message
  "Current echo message excluding shaoline itself, растянутый на всю ширину между левым и правым сегментами."
  (let* ((msg  (current-message))
         (max-width (shaoline-available-center-width)) ;; автоширина!
         ;; Show the message only if it's not the last shaoline string
         (show (unless (or (null msg)
                           (string-equal msg shaoline--last-str))
                 msg))
         (shown-str (or show ""))
         (padded-str
          ;; Растягиваем, если надо — дополнительно пробелы вправо!
          (if (>= (string-width shown-str) max-width)
              (truncate-string-to-width shown-str max-width 0 ?\s)
            (concat shown-str (make-string (- max-width (string-width shown-str)) ?\s)))))
    (propertize padded-str 'face 'shaoline-echo-face)))

;; Battery segment (if `battery` is available)
(shaoline-define-simple-segment
 shaoline-segment-battery
 "Show battery percentage and charging status (icon, optional).
Requires `all-the-icons` and a working `battery-status-function`."
 (if (and (fboundp 'battery)
          battery-status-function)
     (let* ((data (and battery-status-function (funcall battery-status-function))))
       (cond
        ;; (A) If data is an alist (list of cons cells), use the old logic 
        ((and (listp data)
              (not (null data))
              (cl-every #'consp data))
         (let* ((percent (or (cdr (assoc 112 data))           ; char code ?p
                             (cdr (assoc "percentage" data))  ; string key
                             (cdr (assoc "perc" data))        ; alt string key
                             (cdr (assoc "capacity" data))))  ; another possible key
                (status (or (cdr (assoc 66 data))             ; char code ?B
                            (cdr (assoc "status" data))
                            (cdr (assoc "charging" data))
                            (cdr (assoc "state" data))))
                (icon (cond
                       ((not (featurep 'all-the-icons)) "")
                       ((and status
                             (let ((case-fold-search t))
                               (string-match-p "ac" status)))
                        (all-the-icons-octicon "plug" :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                       ((and status
                             (let ((case-fold-search t))
                               (string-match-p "charging" status)))
                        (all-the-icons-faicon "bolt" :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                       ((and status
                             (let ((case-fold-search t))
                               (string-match-p "discharging" status)))
                        "")  ;; Could use a separate icon for discharging if desired
                       ((and status
                             (let ((case-fold-search t))
                               (string-match-p "full" status)))
                        (all-the-icons-faicon "battery-full" :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                       ((and percent (string-match "\\([0-9]+\\)" percent))
                        (let* ((n (string-to-number (match-string 1 percent))))
                          (cond ((>= n 90) (all-the-icons-faicon "battery-full"
                                                                  :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                                ((>= n 70) (all-the-icons-faicon "battery-three-quarters"
                                                                :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                                ((>= n 40) (all-the-icons-faicon "battery-half"
                                                                :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                                ((>= n 10) (all-the-icons-faicon "battery-quarter"
                                                                :face 'shaoline-battery-face :height 1.0 :v-adjust 0))
                                (t (all-the-icons-faicon "battery-empty"
                                                         :face 'shaoline-battery-face :height 1.0 :v-adjust 0)))))
                       (t ""))))
           (if percent
               (concat
                (if (and (stringp icon) (not (string-empty-p icon)))
                    (concat icon " "))
                (propertize (concat (replace-regexp-in-string "%" "" percent) "%")
                            'face 'shaoline-battery-face))
             ;; If no percent, show a fallback "no battery" string with an icon if possible.
             (propertize
              (if (featurep 'all-the-icons)
                  (concat
                   (all-the-icons-faicon "battery-empty" :height 1.0 :v-adjust 0 :face 'shaoline-battery-face)
                   " No battery")
                "No battery")
              'face '(:inherit shaoline-battery-face :slant italic)))))
        ;; (B) If data is a string (your case), just display it
        ((and (stringp data)
              (not (string-empty-p data)))
         (propertize data 'face 'shaoline-battery-face))
        ;; (C) Fallback ("No battery" symbol)
        (t
         (propertize
          (if (featurep 'all-the-icons)
              (all-the-icons-faicon "battery-empty" :height 1.0 :v-adjust 0 :face 'shaoline-battery-face)
            "N/A")
          'face '(:inherit shaoline-battery-face :slant italic)))))
   ;; battery functionality not available
   ""))

;; Major-mode
(shaoline-define-simple-segment shaoline-segment-major-mode
  "Major mode segment with icon."
  (let ((icon (when (and (featurep 'all-the-icons) major-mode)
                (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust 0))))
    (concat
     ;; if `icon` is a string (not a symbol placeholder), show it
     (when (and icon (stringp icon))
       (concat icon " "))
     (propertize
      (format-mode-line mode-name)
      'face 'shaoline-mode-face))))

;; Time + Moon Phase segment
;; We need calendar for `calendar-phase-of-moon` and `calendar-current-date`.

(shaoline-define-simple-segment shaoline-segment-time
  "Current time with moon phase."
  (let* ((time (propertize (format-time-string "%H:%M")
                           'face 'shaoline-time-face))
         ;; 0 = new, 4 = full, etc. 0–7 index into our icons.
         (phase-number (calendar-phase-of-moon
                        (calendar-current-date)))
         (phases ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
         (moon (propertize (aref phases phase-number)
                           'face 'shaoline-time-face)))
    (concat time " " moon)))

;; ---------------------------------------------------------------------------

(provide 'shaoline)
;;; shaoline.el ends here
