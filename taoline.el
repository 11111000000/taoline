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

;; taoline-echo – упрощённая редакция оригинального taoline: без child-frame
;; и без in-window modeline.  Единственный способ отображения – echo-area
;; (мини-буфер).  Архитектура при этом остаётся функциональной: сегменты
;; – чистые функции, композиция строки – чистая функция, реальные побочные
;; эффекты ограничены выводом в echo-area и, опционально, скрытием штатного
;; `mode-line`.
;;
;; Основные фичи:
;;   • минималистичная, декларативно настраиваемая конфигурация сегментов;
;;   • отсутствие периодических таймеров – обновление только по хукам
;;     (`post-command-hook` и др.);
;;   • глобальный minor-mode `taoline-mode` для включения/выключения;
;;   • автоскрытие штатной строки состояния (опционально);
;;   • unit-testable “core” (compose-функция не имеет сайд-эффектов).
;;
;; Пример включения:
;;
;;   (require 'taoline-echo)
;;   (taoline-mode 1)
;;
;; Смотрите README на GitHub для примеров расширения сегментов.

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
  '((:left taoline-segment-project-name taoline-segment-git-branch taoline-segment-icon-and-buffer)
    (:center taoline-segment-echo-message)
    (:right taoline-segment-battery taoline-segment-time))
  "Alist describing segments for :left, :center и :right.
Каждое значение – список *символов-функций* сегмента."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'taoline)

(defcustom taoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "Hooks, по которым taoline пересчитывает и выводит строку."
  :type '(repeat symbol)
  :group 'taoline)

(defcustom taoline-autohide-modeline t
  "When non-nil, hide the usual mode-line while `taoline-mode' is active."
  :type 'boolean
  :group 'taoline)

(defcustom taoline-right-padding 15
  "Spaces appended to the rightmost edge of taoline."
  :type 'integer
  :group 'taoline)

;; ----------------------------------------------------------------------------
;; Faces

(defface taoline-base-face
  '((t :inherit mode-line
       :height 1.0
       :box nil
       :underline nil
       :overline nil
       :inverse-video nil
       :extend t))
  "Base face for taoline."
  :group 'taoline)

(defface taoline-echo-face
  '((t :inherit (shadow taoline-base-face) :height 1.0))
  "Face for echo message segment."
  :group 'taoline)

(defface taoline-time-face
  '((t :inherit (success taoline-base-face) :height 1.0))
  "Face for time segment."
  :group 'taoline)

(defface taoline-buffer-face
  '((t :inherit (mode-line-buffer-id taoline-base-face) :height 1.0))
  "Face for buffer name segment."
  :group 'taoline)

(defface taoline-modified-face
  '((t :inherit (warning taoline-base-face) :height 1.0))
  "Face for modified indicator segment."
  :group 'taoline)

(defface taoline-git-face
  '((t :inherit (font-lock-keyword-face taoline-base-face) :height 1.0))
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
  "Last string rendered – чтобы не выводить лишний раз.")

(defvar taoline--default-mode-line-format-backup nil
  "Backup of `default-mode-line-format' when modeline is hidden.")

(defvar taoline--resize-mini-windows-backup nil
  "Backup of `resize-mini-windows' when `taoline-mode` toggles.")

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
  "Hide mode-line everywhere."
  (taoline--set-modeline-format-globally nil 'backup))

(defun taoline--unhide-modeline-globally ()
  "Restore previously hidden mode-lines."
  (taoline--set-modeline-format-globally :default 'restore))

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
;; Core: compose function (pure)

(defun taoline-compose-modeline (&optional buffer window width)
  "Собрать строку taoline, гарантированно укладывающуюся в ширину WIDTH.
Функция чистая: она лишь возвращает строку, не вызывая побочных эффектов."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         ;; Доступная ширина (display columns) мини-буфера / кадра
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         ;; ------------------------------------------------------------------
         ;; Сегменты
         (left   (mapconcat #'identity (taoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (taoline--collect-segments :right buffer) " "))
         (center (mapconcat #'identity (taoline--collect-segments :center buffer) " "))
         ;; Ширины сегментов
         (left-w   (string-width left))
         (right-w  (string-width right))
         ;; Разделительные пробелы вставляются ТОЛЬКО при необходимости
         (space-left  (if (and (not (string-empty-p left))
                               (or (not (string-empty-p center))
                                   (not (string-empty-p right))))
                          " " ""))
         (space-right (if (and (not (string-empty-p right))
                               (not (string-empty-p center)))
                          " " ""))
         ;; Сколько колонок остаётся под центральную часть (центр + пэддинг)
         (available (- width
                       left-w
                       right-w
                       (string-width space-left)
                       (string-width space-right)
                       taoline-right-padding))
         ;; Центральная строка обрезается по `available`
         (center-str (truncate-string-to-width center (max 0 available) 0 ?\s))
         (center-w  (string-width center-str))
         ;; Оставшееся место равномерно делится на левый/правый пэддинг
         (pad-total   (max 0 (- available center-w)))
         (pad-left-w  (/ pad-total 2))
         (pad-right-w (- pad-total pad-left-w))
         (pad-left  (make-string pad-left-w ?\s))
         (pad-right (make-string pad-right-w ?\s)))
    ;; ------------------------------------------------------------------
    ;; Итоговая строка
    (let ((final (concat
                  (propertize left 'face 'taoline-base-face)
                  space-left
                  pad-left
                  (propertize center-str 'face 'taoline-base-face)
                  pad-right
                  space-right
                  (propertize right 'face 'taoline-base-face)
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

(defun taoline--message-filter-return (result &rest args)
  "Filter return of `message'. If RESULT is empty and `taoline-mode' active, redisplay taoline."
  (when (and taoline-mode (string-empty-p (or result "")))
    (taoline--update))
  result)

;; ----------------------------------------------------------------------------
;; Minor mode

(defcustom taoline-timer-interval 10
  "Interval in seconds for updating time and battery segments.
The timer will not run more often than this interval."
  :type 'number
  :group 'taoline)

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
;; ---------------------------------------------------------------------------
;; Default segments (примерные реализации)
;; ---------------------------------------------------------------------------

;; Имя буфера + ‘*’ если modified
(taoline-define-simple-segment taoline-segment-icon-and-buffer
  "Buffer name with optional icon and modified flag."
  (let* ((icon (when (featurep 'all-the-icons)
                 (if (buffer-file-name)
                     (all-the-icons-icon-for-file
                      (file-name-nondirectory (buffer-file-name))
                      :height 1.0 :v-adjust -0.1 :face 'taoline-base-face)
                   (all-the-icons-icon-for-mode
                    major-mode
                    :height 1.0 :v-adjust -0.1 :face 'taoline-base-face))))
         (name (propertize (buffer-name) 'face 'taoline-buffer-face))
         (mod  (when (buffer-modified-p)
                 (propertize " *" 'face 'taoline-modified-face))))
    (concat
     (or icon "")
     " "
     name
     (or mod ""))))

;; Git-ветка (projectile / vc-git)
(taoline-define-simple-segment taoline-segment-git-branch
  "Current Git branch."
  (when (and (featurep 'vc-git) (buffer-file-name))
    (let ((branch (vc-git--symbolic-ref (buffer-file-name))))
      (when branch
        (concat
         (all-the-icons-octicon "git-branch" :v-adjust -0.1 :height 1.0 :face 'taoline-git-face)
         " "
         (propertize branch 'face 'taoline-git-face))))))

;; Имя проекта (projectile)
(taoline-define-simple-segment taoline-segment-project-name
  "Project name via projectile."
  (when (and (featurep 'projectile) (projectile-project-p))
    (concat
     (all-the-icons-octicon "briefcase" :v-adjust -0.1 :height 1.0 :face 'taoline-base-face)
     " "
     (projectile-project-name))))

;; Сообщение из echo-area (для демонстрации – текущее `current-message`)
(taoline-define-simple-segment taoline-segment-echo-message
  "Current echo message excluding taoline itself."
  (let* ((msg  (current-message))
         (show (unless (or (null msg)
                           (string-equal msg taoline--last-str))
                 msg)))
    (propertize (or show "") 'face 'taoline-echo-face)))

;; Сегмент батареи (если доступна `battery`)
(taoline-define-simple-segment taoline-segment-battery
  "Battery status."
  (when (and (fboundp 'battery) battery-status-function)
    (let ((data (and battery-status-function (funcall battery-status-function))))
      (when data
        (propertize (cdr (assoc ?p data)) 'face 'taoline-echo-face)))))

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


;; Время
(taoline-define-simple-segment taoline-segment-time
  "Current time."
  (propertize (format-time-string "%H:%M") 'face 'taoline-time-face))

;; ---------------------------------------------------------------------------

(provide 'taoline)
;;; taoline.el ends here
