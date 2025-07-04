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

(defcustom shaoline-right-padding 0
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
  '((t :inherit (shaoline-base-face)))
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

;; Message state now handled by shaoline-msg-engine.el
(require 'shaoline-msg-engine)

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
Errors are returned as a diagnostic string, not signalled; stacktrace is logged if `shaoline-debug`."
  (condition-case err
      (let* ((arity (help-function-arglist fn t))
             (res (if (and (consp arity) (not (null arity)))
                      (funcall fn buffer)
                    (funcall fn))))
        (if (stringp res) res (or (and res (format "%s" res)) "")))
    (error
     (let ((msg (format "[SEGMENT ERROR: %s in %s]" err fn)))
       (when shaoline-debug
         (shaoline--log "%s" msg)
         (shaoline--log "Traceback: %s" (with-output-to-string (backtrace))))
       (propertize msg 'face 'error)))))

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
;; Core: compose the modeline (pure version, NO side effects)
;;-----------------------------------------------------------------------------
;; This function is pure: it only computes a string with properties, no UI,
;; no global state. It is safe for use in tests and non-UI code.

(defun shaoline-compose-modeline (&optional buffer window _width)
  "Return the Shaoline string for the echo area.

Pure function: collects, arranges, and truncates segments for rendering 
in the echo area. This function is entirely side-effect free.
The right segment is *pinned* to the right edge by means of the
`space :align-to' display property, while the central segment is
truncated when necessary so that the whole string never exceeds
the window width."
  (let* ((buffer  (or buffer (current-buffer)))
         (window  (or window  (selected-window)))
         (left    (mapconcat #'identity
                             (shaoline--collect-segments :left buffer) " "))
         (center0 (mapconcat #'identity
                             (shaoline--collect-segments :center buffer) " "))
         (right   (mapconcat #'identity
                             (shaoline--collect-segments :right buffer) " "))
         (gap-left (if (string-empty-p left) "" " "))
         (gap-mid  (if (and (not (string-empty-p center0))
                            (not (string-empty-p right)))
                       " " ""))
         (avail (max 0 (- (window-width window)
                          (string-width left)
                          (string-width gap-left)
                          (string-width right)
                          shaoline-right-padding
                          (string-width gap-mid))))
         (center (if (> (string-width center0) avail)
                     (truncate-string-to-width center0 avail 0 ?\s)
                   center0)))
    (concat
     left
     gap-left
     center
     gap-mid
     (propertize
      " "
      'display `(space :align-to (- right ,(+ (string-width right)
                                              shaoline-right-padding
                                              1))))
     right
     (make-string shaoline-right-padding ?\s))))

;; ----------------------------------------------------------------------------
;; Display/update

(defun shaoline--display (str)
  "Show STR in the echo area.  The string is tagged with the text-property
'shaoline so later we can reliably distinguish our own output."
  (let ((cur-msg (current-message)))
    (when (or (not (string-equal str shaoline--last-str))
              (null cur-msg))
      ;; Remember *unmodified* string for width-comparison / clearing,
      ;; but send the *tagged* one to `message'.
      (setq shaoline--last-str str)
      (let* ((msg-with-prop (propertize str 'shaoline t))
             (message-log-max nil))
        (message "%s" msg-with-prop)))))

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

(defcustom shaoline-timer-interval 1
  "Interval (seconds) for Shaoline’s periodic refresh.

A low value (default 1 s) lets echo-messages expire precisely while
CPU impact stays negligible."
  :type 'number
  :group 'shaoline)

;; --------------------------------------------------------------------------
;;  Message-timeout logic (config, timer & helpers)

(defcustom shaoline-message-timeout 10
  "Seconds to keep a regular `message` in the echo area before Shaoline
redraws its modeline.

If set to 0 (or a negative number), Shaoline **immediately** redraws the
modeline right after the `message` call, so the text never appears in the
echo area by itself; it is shown only inside the
`shaoline-segment-echo-message` segment."
  :type 'number
  :group 'shaoline)

(defvar shaoline--redisplay-timer nil
  "Idle timer started after an ordinary `message`; forces a Shaoline
refresh when `shaoline-message-timeout` seconds have elapsed.")

(defun shaoline--cancel-redisplay-timer ()
  "Compatibility stub – the separate idle-redisplay timer is no longer used."
  nil)

(defun shaoline--schedule-redisplay ()
  "Compatibility stub – idle redisplay timer removed in the simplified design."
  nil)

(defun shaoline--message-filter-return (result &rest _args)
  "Intercept `message' for Shaoline: stores last user message & time, manages timer.

Only reacts if message from user (not Shaoline's own)."
  (when (and shaoline-mode
             (not (and (stringp result)
                       (get-text-property 0 'shaoline result))))
    (cond
     ;; New, non-empty message
     ((and (stringp result) (not (string-empty-p result)))
      (shaoline-msg-save result)
      (run-at-time 0 nil #'shaoline--update)
      (shaoline--maybe-start-timer)) ; нужен таймер

     ;; Пустое сообщение — сброс
     ((or (null result) (string-empty-p result))
      (shaoline-msg-clear)
      (run-at-time 0 nil #'shaoline--update)
      (shaoline--maybe-cancel-timer))))
  result)


;;  Ensure the timer is cleaned up whenever the global minor-mode is toggled.
(add-hook 'shaoline-mode-hook #'shaoline--cancel-redisplay-timer)

(defvar shaoline--timer nil
  "Internal timer used by `shaoline-mode' for periodic updates (only when needed).")

(defun shaoline--maybe-start-timer ()
  "Запускать ленивый таймер только если требуется обновлять динамические сегменты (например, время, батарея)."
  (when (and (null shaoline--timer) shaoline-mode)
    (setq shaoline--timer
          (run-with-timer shaoline-timer-interval
                          shaoline-timer-interval
                          #'shaoline--lazy-update))))

(defun shaoline--maybe-cancel-timer ()
  "Убить таймер, если он не нужен."
  (when (timerp shaoline--timer)
    (cancel-timer shaoline--timer)
    (setq shaoline--timer nil)))

(defun shaoline--lazy-update ()
  "Ленивая обновляющая функция: если нет показа echo-message и нет динамического сегмента — убираем себя."
  (shaoline--log "shaoline--lazy-update")
  (let ((should-keep
         (or
          ;; Центр может включать временной сегмент; присутствует ли он?
          (cl-member 'shaoline-segment-time (cdr (assq :right shaoline-segments)))
          (cl-member 'shaoline-segment-time (cdr (assq :center shaoline-segments)))
          ;; Пользовательское сообщение ещё "живое"
          (shaoline-msg-active-p shaoline-message-timeout))))
    (shaoline--update)
    (unless should-keep
      (shaoline--maybe-cancel-timer))))

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
        (setq shaoline--timer nil) ;; Таймер больше не запускаем автоматически, только лениво.
        )
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
;; Segments are now located in a separate file.
(require 'shaoline-segments)

;; --- Auto-load user-defined segments if available ---
(let ((user-file
       (or
        (expand-file-name "shaoline-user-segments.el" user-emacs-directory)
        (and load-file-name
             (expand-file-name "shaoline-user-segments.el" (file-name-directory load-file-name))))))
  (when (and user-file (file-exists-p user-file))
    (condition-case err
        (load user-file nil t)
      (error (shaoline--log "Could not load user segments: %s" err)))))


(provide 'shaoline)
;;; shaoline.el ends here
