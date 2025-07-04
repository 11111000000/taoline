;;; shaoline-infra.el --- Infrastructure: echo display, hooks, advice, UI for Shaoline -*- lexical-binding: t; -*-

;; This file hosts *impure* infrastructure that complements the pure core.
;; It is loaded *from* shaoline.el, so we purposely do *not* `require' it
;; back to avoid a circular dependency.

(require 'cl-lib)                    ; for cl-member etc.

;; ----------------------------------------------------------------------------
;; Customisation (side-effect related)

(defcustom shaoline-timer-interval 1
  "Interval (seconds) for Shaoline’s optional periodic refresh.
Only used when dynamic segments such as time/battery are present."
  :type 'number
  :group 'shaoline)

(defcustom shaoline-message-timeout 10
  "How many seconds a normal `message' remains before Shaoline redraws.
If non-positive, Shaoline repaints immediately after the `message'."
  :type 'number
  :group 'shaoline)

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

(defun shaoline--cancel-redisplay-timer ()
  "Compatibility stub – the separate idle-redisplay timer is no longer used."
  nil)

(defun shaoline--schedule-redisplay ()
  "Compatibility stub – idle redisplay timer removed in the simplified design."
  nil)

;;  Ensure the timer is cleaned up whenever the global minor-mode is toggled.
(add-hook 'shaoline-mode-hook #'shaoline--cancel-redisplay-timer)

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

(provide 'shaoline-infra)
;;; shaoline-infra.el ends here
