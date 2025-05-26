;;; taoline.el --- Functional minimalist modeline for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Peter

;; Author: Peter <11111000000@email.com>
;; Version: 2.0
;; Package-Requires: ((emacs "27.1") (posframe "1.1.0"))
;; Keywords: mode-line, minimal, functional, faces
;; URL: https://github.com/11111000000/taoline

;; License: MIT

;;; Commentary:

;; taoline.el provides a minimal modeline for Emacs with modern,
;; functional-programming-based architecture.  All logic is split into pure
;; functions, and all side-effects are isolated in backend modules.
;;
;; Features:
;; - Minimalist, highly customizable segments.
;; - Functional style: pure segment functions, stateless composition.
;; - Multiple backends: echo area and posframe.
;; - Easy extension via macros for new segments.
;; - No periodic timers; only updates on user commands (post-command-hook, etc).
;; - Unit-testable core compose logic.
;;
;; See README on GitHub for usage and extension details.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(eval-when-compile (require 'rx))

;; Optional: Only if posframe is available and user wants it
(eval-and-compile
  (ignore-errors (require 'posframe)))

(defgroup taoline nil
  "Functional minimalist mode-line replacement."
  :group 'convenience
  :prefix "taoline-")

;; ----------------------------------------------------------------
;; Debug logging helpers
(defcustom taoline-debug-log t
  "If non-nil, taoline emits verbose debug messages to *Messages*."
  :type 'boolean
  :group 'taoline)

(defun taoline--log (fmt &rest args)
  "Log a formatted debug message when `taoline-debug-log' is non-nil."
  (when taoline-debug-log
    (apply #'message (concat "[taoline] " fmt) args)))

;; -- Posframe backend
;;
;; When the posframe backend is enabled we want to completely hide the
;; conventional mode-lines in all windows across all frames.  When the
;; backend is torn down (either because another Taoline backend is
;; activated or because `taoline-mode' is disabled) we need to restore the
;; original `mode-line-format' in every window.
;;
;; The implementation below is backend-agnostic: we simply walk all windows
;; and stash the current value of the `mode-line-format' window-parameter
;; under a private key.  The value is then replaced with `nil', which hides
;; the mode-line.  On teardown we do the inverse operation.
;;
;; Two small helpers are defined and then hooked to the lifecycle of the
;; posframe backend via `advice-add'.  Wrapping the advice in
;; `with-eval-after-load' ensures the target functions are defined.

(defvar taoline--saved-modeline-param 'taoline--saved-modeline
  "Window parameter key used to store the original mode-line value.")

(defun taoline--hide-all-modelines ()
  "Hide mode-lines in every window on every frame, remembering originals."
  (taoline--log ">> hide-all-modelines")
  (when (and taoline-autohide-modeline
             (memq taoline--active-backend '(echo posframe)))
    (walk-windows
     (lambda (win)
       (with-selected-window win
         ;; Save only once per window
         (unless (window-parameter win taoline--saved-modeline-param)
           (set-window-parameter
            win taoline--saved-modeline-param
            (window-parameter win 'mode-line-format)))
         ;; Hide the mode-line in this window
         (set-window-parameter win 'mode-line-format nil)
         (setq mode-line-format nil)))
     nil t)))                            ; t ⇒ include all frames

(defun taoline--maybe-hide-new-window-modeline (&rest _)
  "Auto-hide modeline for all windows after window/buffer changes, if autohide is enabled.
This guarantees that modeline remains hidden for all windows, including new ones, when taoline is active with autohide.
Intended for use in hooks."
  (when (and taoline-autohide-modeline
             (memq taoline--active-backend '(echo posframe)))
    ;; Hide for all windows, including new ones
    (walk-windows
     (lambda (win)
       (with-selected-window win
         (unless (window-parameter win taoline--saved-modeline-param)
           (set-window-parameter
            win taoline--saved-modeline-param
            (window-parameter win 'mode-line-format))
           (set-window-parameter win 'mode-line-format nil)
           (setq mode-line-format nil))))
     nil t)))

(defun taoline--restore-all-modelines ()
  "Restore previously hidden mode-lines in all windows."
  (taoline--log ">> restore-all-modelines")
  (walk-windows
   (lambda (win)
     (with-selected-window win
       (let ((old (window-parameter win taoline--saved-modeline-param)))
         (when old
           (set-window-parameter win 'mode-line-format old)
           (setq mode-line-format old)
           (set-window-parameter win taoline--saved-modeline-param nil)
           (force-mode-line-update t)))))
   nil t))

(with-eval-after-load 'taoline
  ;; ------------------------------------------------------------
  ;; Сохраняем оригинальные mode-line’ы для windows ещё до того,
  ;; как бек-энд modeline их перезапишет.
  (defun taoline--save-all-modelines (&rest _)
    "Сохранить текущий `mode-line-format` каждого окна,
если он ещё не был сохранён."
    (taoline--log ">> save-all-modelines")
    (walk-windows
     (lambda (win)
       (unless (window-parameter win taoline--saved-modeline-param)
         (set-window-parameter
          win taoline--saved-modeline-param
          (window-parameter win 'mode-line-format))))
     nil t))

  ;; ------------------------------------------------------------
  ;; Hide on backend activation (both posframe and echo)
  (advice-add 'taoline--posframe-setup :after #'taoline--hide-all-modelines)
  (advice-add 'taoline--echo-setup   :after #'taoline--hide-all-modelines)

  ;; Для in-window бек-энда: сперва сохраняем исходное состояние.
  (advice-add 'taoline--modeline-setup :before #'taoline--save-all-modelines)

  ;; ------------------------------------------------------------
  ;; Restore on backend deactivation
  (advice-add 'taoline--posframe-teardown :before #'taoline--restore-all-modelines)
  (advice-add 'taoline--echo-teardown     :before #'taoline--restore-all-modelines)
  (advice-add 'taoline--modeline-teardown :before #'taoline--restore-all-modelines))

;; --- Automatically hide modeline in any new window/buffer while taoline is on and autohide is enabled
(defvar taoline--window-config-hide-hooked nil
  "Whether taoline is currently hiding modelines for new windows.")

(defun taoline--enable-hide-modeline-hooks ()
  "Enable hooks that keep hiding modelines in new windows when taoline is active."
  (unless taoline--window-config-hide-hooked
    (add-hook 'window-configuration-change-hook #'taoline--maybe-hide-new-window-modeline)
    (add-hook 'buffer-list-update-hook #'taoline--maybe-hide-new-window-modeline)
    (setq taoline--window-config-hide-hooked t)))

(defun taoline--disable-hide-modeline-hooks ()
  "Remove hooks set by `taoline--enable-hide-modeline-hooks'."
  (when taoline--window-config-hide-hooked
    (remove-hook 'window-configuration-change-hook #'taoline--maybe-hide-new-window-modeline)
    (remove-hook 'buffer-list-update-hook #'taoline--maybe-hide-new-window-modeline)
    (setq taoline--window-config-hide-hooked nil)))


(defvar taoline--posframe-table (make-hash-table :test 'eq)
  "Таблица posframe'ов по окнам.")

(defun taoline--posframe-setup ()
  "Показать posframe taoline во всех окнах. Скрыть modeline если нужно."
  (when taoline-autohide-modeline
    (taoline--hide-all-modelines))
  (walk-windows
   (lambda (win)
     (taoline--posframe-show win))
   nil t))

(defun taoline--posframe-update (_str)
  "Обновить posframe taoline для всех окон."
  (walk-windows
   (lambda (win)
     (taoline--posframe-show win))
   nil t))

(defun taoline--posframe-teardown ()
  "Убрать posframe taoline во всех окнах. Восстановить modeline если нужно."
  ;; 1. Удаляем все posframe, которые были созданы в taoline--posframe-table
  (maphash (lambda (_win pfname)
             (ignore-errors (posframe-delete pfname)))
           taoline--posframe-table)
  (clrhash taoline--posframe-table)
  ;; 2. Также дополнительно удаляем все posframe, которые соответствуют шаблону " taoline-posframe-*" (на случай несинхронизации хеша)
  (when (boundp 'posframe--frame-list)
    (dolist (entry posframe--frame-list)
      (let ((name (car entry)))
        (when (and (stringp (symbol-name name))
                   (string-prefix-p " taoline-posframe-" (symbol-name name)))
          (ignore-errors (posframe-delete name))))))
  ;; 3. Гарантированно восстанавливаем modeline даже если autohide отключён уже после инициализации
  (taoline--restore-all-modelines))

(defun taoline--posframe-show (win)
  "Показать/обновить posframe в окне WIN."
  (let* ((buffer (window-buffer win))
         ;; Явно всегда используем frame-width!
         (fwidth (frame-width (window-frame win)))
         (str (taoline-compose-modeline buffer win fwidth))
         (pfname (intern (format " taoline-posframe-%s" (window-parameter win 'window-id)))))
    (puthash win pfname taoline--posframe-table)
    (posframe-show
     pfname
     :string str
     :position (window-point win)
     :parent-window win
     :width fwidth
     :border-width 0
     :background-color (face-attribute 'mode-line :background nil t))))

;; ------------------------------------------------------


;; Customizable variables

(defcustom taoline-display-backend 'modeline
  "Backend to show the modeline.
Supported values: 'modeline, 'echo, 'posframe"
  :type '(choice (const :tag "In-window modeline" modeline)
                 (const :tag "Echo area" echo)
                 (const :tag "Child frame (posframe)" posframe))
  :group 'taoline)

(defcustom taoline-segments
  '((:left taoline-segment-git-branch taoline-segment-buffer-name)
    (:right taoline-segment-battery taoline-segment-major-mode taoline-segment-time))
  "List of segments to display in the modeline.
:LEFT and :RIGHT are lists of segment *function symbols*."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'taoline)

(defcustom taoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "A list of hooks triggering taoline update."
  :type '(repeat symbol)
  :group 'taoline)

(defcustom taoline-autohide-modeline t
  "If non-nil, auto-hide the usual modeline in all windows when taoline is active in 'posframe or 'echo backends.
When enabled and taoline is using the echo area or posframe backend, all modelines are hidden in every window.
When taoline is switched back to the in-window backend, original modelines are restored.
This variable does not currently affect the 'modeline backend (in-window modeline)."
  :type 'boolean
  :group 'taoline)

;; ----------------------------------------------------------------------------
;; Faces

(defface taoline-base-face
  '((t :inherit mode-line))
  "Base face for taoline."
  :group 'taoline)

(defface taoline-time-face
  '((t :inherit success))
  "Face for time segment."
  :group 'taoline)

(defface taoline-buffer-face
  '((t :inherit mode-line-buffer-id))
  "Face for buffer name segment."
  :group 'taoline)

(defface taoline-modified-face
  '((t :inherit warning))
  "Face for modified indicator segment."
  :group 'taoline)

(defface taoline-git-face
  '((t :inherit font-lock-keyword-face))
  "Face for git branch segment."
  :group 'taoline)

(defface taoline-mode-face
  '((t :inherit font-lock-type-face))
  "Face for major mode segment."
  :group 'taoline)

;; ----------------------------------------------------------------------------
;; Internal tables and state

(defvar taoline--segment-table (make-hash-table :test 'eq)
  "Table for registered taoline segment functions.")

(defvar taoline--last-str ""
  "Cache for the last rendered modeline string.")

(defvar taoline--active-backend nil
  "Currently active backend symbol.")

(defvar taoline--backend-data nil
  "Backend data for teardown or cleanup.")

(defun taoline--switch-backend (new-backend)
  "Switch to NEW-BACKEND. Teardown previous, setup new."
  (taoline--log ">> switch-backend %s -> %s" taoline--active-backend new-backend)
  (when taoline--active-backend
    (let* ((teardown (cdr (assq 'teardown (cdr (assq taoline--active-backend taoline--backend-table))))))
      (when teardown (funcall teardown))))
  (setq taoline--active-backend new-backend)
  (let* ((setup (cdr (assq 'setup (cdr (assq taoline--active-backend taoline--backend-table)))))
         (update (cdr (assq 'update (cdr (assq taoline--active-backend taoline--backend-table))))))
    (when setup (funcall setup))
    (setq taoline--backend-data (cons setup update)))
  ;; Восстанавливаем modeline и убираем autohide-хуки всегда при уходе с posframe/echo на modeline
  (if (memq new-backend '(echo posframe))
      (when taoline-autohide-modeline
        (taoline--enable-hide-modeline-hooks))
    ;; После любого выключения posframe/echo – modeline должны быть восстановлены
    (unless new-backend  ; При полном выключении (new-backend nil)
      (taoline--modeline-teardown))
    (taoline--restore-all-modelines)
    (taoline--disable-hide-modeline-hooks)))

;; ----------------------------------------------------------------------------
;; Segment definition and segment API

(defmacro taoline-define-segment (name args &rest body)
  "Define a segment function NAME accepting ARGS, with BODY.
Registers the NAME in the segment table."
  (declare (indent defun))
  `(progn
     (defun ,name ,args ,@body)
     (puthash ',name #',name taoline--segment-table)))

(defun taoline--apply-segment (fn buffer)
  "Call segment FN with BUFFER. Always return a string (empty if nil, or error)."
  (condition-case err
      (let ((res (funcall fn buffer)))
        (if (stringp res) res (or (and res (format "%s" res)) "")))
    (error (propertize (format "[SEGMENT ERROR: %s]" err) 'face 'error))))

(defun taoline--collect-segments (side buffer)
  "Return list of rendered segment strings for SIDE (:left/:right) in BUFFER.
Функциональный стиль: без явных мутаций и обратных проходов."
  (cl-loop for fn in (cdr (assq side taoline-segments))
           collect (taoline--apply-segment fn buffer)))

;; ----------------------------------------------------------------------------
;; Core: Compose functionally the modeline string

(defun taoline-compose-modeline (&optional buffer window width)
  "Pure function: compose modeline string for BUFFER and WINDOW.
WIDTH если задан, используется для ограничения ширины.
Если WINDOW не задан — используется выбранное окно.

Для backend’ов posframe и echo по умолчанию используется ширина frame, иначе window."
  (let* ((buffer (or buffer (current-buffer)))
         (window (or window (selected-window)))
         ;; Определяем ширину: если echo или posframe — frame-width
         (width (or width
                    (cond
                     ((eq taoline--active-backend 'posframe)
                      (frame-width (window-frame window)))
                     ((eq taoline--active-backend 'echo)
                      (frame-width (window-frame window)))
                     (t (window-width window)))))
         (left (string-join (taoline--collect-segments :left buffer) " "))
         (right (string-join (taoline--collect-segments :right buffer) " "))
         (mid-space (max 1 (- width (string-width left) (string-width right)))))
    (truncate-string-to-width
     (concat
      (propertize left 'face 'taoline-base-face)
      (make-string mid-space ?\s)
      (propertize right 'face 'taoline-base-face))
     width)))

;; ----------------------------------------------------------------------------
;; Built-in segments

(taoline-define-segment taoline-segment-time (buffer)
  "Display current time."
  (propertize (format-time-string "%H:%M")
              'face 'taoline-time-face))

(taoline-define-segment taoline-segment-buffer-name (buffer)
  "Display current buffer name, with * if modified."
  (let ((name (buffer-name buffer)))
    (concat
     (propertize name 'face 'taoline-buffer-face)
     (when (buffer-modified-p buffer)
       (propertize "*" 'face 'taoline-modified-face)))))

(taoline-define-segment taoline-segment-major-mode (buffer)
  "Display the major mode."
  (propertize (format-mode-line mode-name)
              'face 'taoline-mode-face))

;; Simple git branch detector; can be redefined by user
(taoline-define-segment taoline-segment-git-branch (buffer)
  "Display current git branch, if any."
  (let* ((default-directory (or (buffer-local-value 'default-directory buffer) default-directory))
         (branch (when (and default-directory (fboundp 'vc-git-root))
                   (let ((root (ignore-errors (vc-git-root default-directory))))
                     (when root
                       (with-temp-buffer
                         (let* ((git-dir (expand-file-name ".git/HEAD" root)))
                           (when (file-readable-p git-dir)
                             (insert-file-contents git-dir)
                             (when (re-search-forward "ref: refs/heads/\\(.*\\)" nil t)
                               (match-string 1))))))))))
    (if branch
        (propertize (concat " " branch) 'face 'taoline-git-face)
      "")))

(taoline-define-segment taoline-segment-battery (buffer)
  "Display battery icon and percentage."
  (if (functionp battery-status-function)
      (let* ((data (ignore-errors (funcall battery-status-function)))
             (percent-str (or (cdr (assoc "percentage" data))
                              (cdr (assoc 112 data))
                              (cdr (assoc ?p data)))))
        (if (and percent-str (string-match-p "^[0-9]+$" percent-str))
            (let* ((percent (string-to-number percent-str))
                   (icon (cond
                          ((>= percent 95) (all-the-icons-faicon "battery-full" :height 0.95 :v-adjust -0.05))
                          ((>= percent 70) (all-the-icons-faicon "battery-three-quarters" :height 0.95 :v-adjust -0.05))
                          ((>= percent 45) (all-the-icons-faicon "battery-half" :height 0.95 :v-adjust -0.05))
                          ((>= percent 20) (all-the-icons-faicon "battery-quarter" :height 0.95 :v-adjust -0.05))
                          (t (all-the-icons-faicon "battery-empty" :height 0.95 :v-adjust -0.05)))))
              (format "%s %s%%" icon percent))
          ""))
    ""))

;; ----------------------------------------------------------------------------
;; Backend registry

(defvar taoline--backend-table
  '((modeline . ((setup . taoline--modeline-setup)
                 (update . taoline--modeline-update)
                 (teardown . taoline--modeline-teardown)))
    (echo    . ((setup . taoline--echo-setup)
                (update . taoline--echo-update)
                (teardown . taoline--echo-teardown)))

    (posframe . ((setup . taoline--posframe-setup)
                 (update . taoline--posframe-update)
                 (teardown . taoline--posframe-teardown))))
  "Backend function registry.")

;; -- Echo backend (minibuffer)

(defun taoline--echo-update (_str)
  "Обновить таолайн в эхо-области (minibuffer)."
  ;; Echo area всегда шириной frame-width
  (let* ((fwidth (frame-width (selected-frame)))
         (str (taoline-compose-modeline (current-buffer) (selected-window) fwidth)))
    (message "%s" str)))

(defun taoline--echo-setup ()
  "Echo backend: Скрыть modeline если нужно."
  (when taoline-autohide-modeline
    (taoline--hide-all-modelines))
  nil)

(defun taoline--echo-teardown ()
  "Очистить эхо и восстановить modeline если нужно."
  (when taoline-autohide-modeline
    (taoline--restore-all-modelines))
  (message ""))


;; -- Modeline backend (in-window modeline)

(defvar-local taoline--modeline-old nil)
(defvar-local taoline--modeline-expression nil)

(defun taoline--modeline-setup ()
  "Setup taoline modeline so that only the selected window shows taoline."
  (taoline--log ">> modeline-setup (selected-window %s)" (selected-window))
  ;; Хук для ресайза оставляем, но он теперь просто обновляет выбранное окно (перерисовка)
  (unless (bound-and-true-p taoline--size-change-hook-installed)
    (add-hook 'window-size-change-functions #'taoline--window-size-change-handler)
    (setq taoline--size-change-hook-installed t))
  ;; При активации сохраняем modeline только для selected-window
  (let ((win (selected-window))
        (expr '(:eval (taoline-compose-modeline nil (selected-window)))))
    ;; Сохранять только оригинальный (не уже подменённый taoline) mode-line
    (unless (window-parameter win 'taoline--modeline-old)
      ;; Только если в параметре всё ещё nil, сохраняем текущее (оригинальное) значение.
      (set-window-parameter win 'taoline--modeline-old (window-parameter win 'mode-line-format)))
    (set-window-parameter win 'taoline--modeline-expression expr)
    (set-window-parameter win 'mode-line-format expr)
    (setq mode-line-format expr)
    (force-mode-line-update t))
  ;; Ставим хук на переключение окон — чтобы taoline показывался только в активном
  (unless (bound-and-true-p taoline--window-selection-change-hook-installed)
    (add-hook 'window-selection-change-functions #'taoline--modeline-window-selection-change)
    (setq taoline--window-selection-change-hook-installed t)))

;; Централизованный обработчик для любого изменения размера окна для modeline
(defun taoline--window-size-change-handler (&rest _)
  "Перерисовать taoline в текущем окне при изменении размера окна."
  (when (eq taoline--active-backend 'modeline)
    (with-selected-window (selected-window)
      (force-mode-line-update t))))

(defun taoline--modeline-update (_str)
  "Update taoline modeline only in selected window; restore defaults (or hide) in others.
Если `taoline-autohide-modeline`, то скрывает modeline во всех окнах, кроме активного."
  (taoline--log ">> modeline-update")
  (let* ((win (selected-window))
         (expr '(:eval (taoline-compose-modeline nil (selected-window)))))
    (dolist (other (window-list))
      (if (eq other win)
          (with-selected-window other
            (unless (window-parameter other 'taoline--modeline-old)
              (set-window-parameter other 'taoline--modeline-old (window-parameter other 'mode-line-format)))
            (set-window-parameter other 'mode-line-format expr)
            (setq mode-line-format expr)
            (force-mode-line-update t))
        ;; невыбранное окно:
        (if (bound-and-true-p taoline-autohide-modeline)
            (progn
              (unless (window-parameter other 'taoline--modeline-old)
                (set-window-parameter other 'taoline--modeline-old (window-parameter other 'mode-line-format)))
              (set-window-parameter other 'mode-line-format nil)
              (with-selected-window other
                (setq mode-line-format nil)
                (force-mode-line-update t)))
          ;; вернуть оригинальный modeline если был
          (let ((old (window-parameter other 'taoline--modeline-old)))
            (when old
              (set-window-parameter other 'mode-line-format old)
              (set-window-parameter other 'taoline--modeline-old nil)
              (set-window-parameter other 'taoline--modeline-expression nil)
              (setq mode-line-format old)
              (force-mode-line-update t))))))))

(defun taoline--modeline-teardown ()
  "Restore all modelines in all windows (including hidden)."
  (taoline--log ">> modeline-teardown")
  (remove-hook 'post-command-hook #'taoline--modeline-window-selection-change)
  (remove-hook 'window-configuration-change-hook #'taoline--modeline-window-configuration-change)
  (remove-hook 'window-size-change-functions #'taoline--window-size-change-handler)
  (dolist (win (window-list (selected-frame) t))
    (let ((old (or (window-parameter win 'taoline--modeline-old)
                   (default-value 'mode-line-format))))
      (set-window-parameter win 'mode-line-format old)
      (with-selected-window win
        (setq mode-line-format old)
        (force-mode-line-update))
      (set-window-parameter win 'taoline--modeline-old nil)
      (set-window-parameter win 'taoline--modeline-active nil)
      (set-window-parameter win 'taoline--modeline-expression nil)))
  (setq taoline--windows-with-taoline nil)
  (setq taoline--last-selected-window nil))


;; Window selection change: обновлять taoline на лету

(defun taoline--modeline-window-selection-change (_frame)
  "Хук на переключение выбранного окна — обновить taoline только в выбранном окне."
  (when (eq taoline--active-backend 'modeline)
    (taoline--modeline-update nil)))  ;; Просто пересоздать modeline/restore везде

;; -- Echo area backend

(defgroup taoline nil
  "Slim, extensible, functional mode-line for Emacs."
  :group 'convenience)

(defcustom taoline-debug t
  "If non-nil, taoline logs to the *taoline-logs* buffer instead of/in addition to *Messages*."
  :type 'boolean
  :group 'taoline)

(defvar taoline--log-buffer "*taoline-logs*"
  "Buffer name for taoline debug log.")

(defun taoline--log (fmt &rest args)
  "Log message FMT with ARGS to `taoline--log-buffer' if `taoline-debug' is non-nil.
Lines are appended at the end of the buffer, which is auto-created.
Never writes debug to *Messages*."
  (when taoline-debug
    (let ((msg (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create taoline--log-buffer)
        (goto-char (point-max))
        (insert msg "\n")))))

(defvar taoline--echo-postcmd-hooked nil
  "Internal: Whether taoline echo post-command refresh is enabled.")

(defvar taoline--echo-minibuf-hooked nil
  "Internal: Whether taoline echo minibuffer exit refresh is enabled.")

(defvar taoline--echo-clear-hooked nil
  "Internal: Whether taoline echo `echo-area-clear-hook' refresh is enabled (Emacs 29+).")

(defun taoline--echo-refresh-if-empty ()
  "Show taoline in the echo area if minibuffer is gone and echo area is empty or contains only whitespace.
Hide taoline only when something meaningful (not just whitespace) is in the echo area,
or when minibuffer is active or minibuffer input is ongoing.
Adds debug output to *taoline-logs* if debugging is enabled."
  (let* ((taoline-debug-info
          (list
           :taoline-mode taoline-mode
           :active-backend taoline--active-backend
           :display-backend taoline-display-backend
           :active-minibuffer (active-minibuffer-window)
           :current-message (current-message))))
    (taoline--log "[taoline echo-debug] refresh: %S" taoline-debug-info)
    (when (and taoline-mode
               (eq taoline--active-backend 'echo)
               (not (active-minibuffer-window)))
      (let ((msg (current-message)))
        (taoline--log "[taoline echo-debug] mini='%s'" msg)
        (if (or (null msg)
                (string-match-p "\\`[ \t\n\r]*\\'" msg))
            (progn
              (taoline--log "[taoline echo-debug] minibuffer is empty, refreshing taoline (run-at-time)")
              (run-at-time 0 nil
                           (lambda ()
                             (taoline--log "[taoline echo-debug] (delayed) calling taoline--maybe-update")
                             (taoline--maybe-update))))
          (taoline--log "[taoline echo-debug] minibuffer not empty, taoline not shown"))))))

(defun taoline--echo-minibuffer-exit-refresh ()
  "Hook for minibuffer-exit to restore taoline if needed."
  (run-at-time 0.05 nil #'taoline--echo-refresh-if-empty))

(defun taoline--echo-clear-refresh ()
  "Hook for `echo-area-clear-hook' (Emacs 29+) to restore Taoline
немедленно, как только echo-area становится пустой."
  (taoline--log "[taoline echo-debug] echo-area-clear-hook triggered")
  (when (and taoline-mode
             (eq taoline--active-backend 'echo)
             (not (active-minibuffer-window)))
    ;; Прямой вызов без таймера, чтобы не было заметной задержки.
    (taoline--maybe-update)))

(defun taoline--echo-setup ()
  ;; Ensure echo restoring logic is installed
  (unless taoline--echo-postcmd-hooked
    (add-hook 'post-command-hook #'taoline--echo-refresh-if-empty))
  (setq taoline--echo-postcmd-hooked t)

  (unless taoline--echo-minibuf-hooked
    (add-hook 'minibuffer-exit-hook #'taoline--echo-minibuffer-exit-refresh))
  (setq taoline--echo-minibuf-hooked t)

  ;; Emacs 29+: используем echo-area-clear-hook для мгновенного восстановления
  (when (and (boundp 'echo-area-clear-hook)
             (not taoline--echo-clear-hooked))
    (add-hook 'echo-area-clear-hook #'taoline--echo-clear-refresh)
    (setq taoline--echo-clear-hooked t))

  (taoline--log "[taoline echo-debug] echo-setup: run initial refresh")
  ;; Небольшая задержка для первого показа, чтобы не мешать стартовым сообщениям
  (run-at-time 0.01 nil #'taoline--echo-refresh-if-empty))

(defun taoline--echo-update (str)
  "Update echo area with STR if minibuffer is not active.
No debug/trace output goes to the minibuffer or *Messages*; debug stays in *taoline-logs* only.
НИКОГДА не писать taoline в echo если buffer спецбуфер ИЛИ str пустой."
  (let ((active-minibuffer (active-minibuffer-window)))
    (taoline--log "[taoline echo-debug] echo-update: active-minibuffer=%S str='%s'"
                   active-minibuffer str)
    (when (and (not active-minibuffer)
               (stringp str)
               (not (string-prefix-p "*" (buffer-name (current-buffer))))
               (not (equal str "")))
      (let ((message-log-max nil)) (message "%s" str)))))

(defun taoline--echo-teardown ()
  (when taoline--echo-postcmd-hooked
    (remove-hook 'post-command-hook #'taoline--echo-refresh-if-empty)
    (setq taoline--echo-postcmd-hooked nil))
  (when taoline--echo-minibuf-hooked
    (remove-hook 'minibuffer-exit-hook #'taoline--echo-minibuffer-exit-refresh)
    (setq taoline--echo-minibuf-hooked nil))
  (when (and taoline--echo-clear-hooked (boundp 'echo-area-clear-hook))
    (remove-hook 'echo-area-clear-hook #'taoline--echo-clear-refresh)
    (setq taoline--echo-clear-hooked nil))
  (let ((msg (current-message)))
    (when (or (null msg)
              (string-match-p "\\`[ \t\n\r]*\\'" msg))
      (message ""))))

;; -- Posframe backend

(defvar taoline--posframe-buffer " *taoline-posframe*")
(defvar taoline--posframe-name "taoline-posframe")

(defface taoline-posframe-face
  '((t :inherit default :family "monospace" :height 1.0))
  "Face for taoline posframe panel (forced monospace).")

(defun taoline--posframe-setup ()
  (require 'posframe)
  (posframe-delete taoline--posframe-buffer))

(defun taoline--posframe-update (str)
  (require 'posframe)
  ;; Обеспечиваем что строка имеет наш шрифт
  (let* ((width (frame-width))
         (height 1)
         (mono-str (propertize str 'face 'taoline-posframe-face)))
    (posframe-show
     taoline--posframe-buffer
     :string mono-str
     :name taoline--posframe-name
     :poshandler 'posframe-poshandler-frame-bottom-left-corner
     :min-width width
     :max-width width
     :min-height height
     :border-width 0
     ;; Важно: font для child-frame explicit
     :override-parameters
     '((font . "monospace") (left-fringe . 0) (right-fringe . 0)
       (vertical-scroll-bars . nil)))))

(defun taoline--posframe-teardown ()
  (ignore-errors
    (require 'posframe)
    ;; Сначала попробуем скрыть, затем удалить, на всех версиях posframe.
    (when (fboundp 'posframe-hide)
      (posframe-hide taoline--posframe-buffer))
    (posframe-delete taoline--posframe-buffer)))

;; ----------------------------------------------------------------------------
;; Backend dispatcher

(defun taoline--get-backend (backend)
  (alist-get backend taoline--backend-table))

(defun taoline--backend-setup (backend)
  (taoline--log "[taoline debug] backend-setup: %S" backend)
  (let ((b (taoline--get-backend backend)))
    (when b (funcall (alist-get 'setup b)))))

(defun taoline--backend-update (backend str)
  (taoline--log "[taoline debug] backend-update: backend=%s, str='%s'" backend str)
  (let ((b (taoline--get-backend backend)))
    (when b (funcall (alist-get 'update b) str))))

(defun taoline--backend-teardown (backend)
  (taoline--log "[taoline debug] backend-teardown: %S" backend)
  (let ((b (taoline--get-backend backend)))
    (when b (funcall (alist-get 'teardown b)))))

(defun taoline--set-backend (backend)
  "Switch to BACKEND, handling setup/teardown, правильное восстановление обычного modeline, и общий window-size-change-хук."
  (taoline--log "[taoline debug] set-backend called: %S" backend)
  (setq taoline-display-backend backend)
  (unless (eq backend taoline--active-backend)
    (when taoline--active-backend
      (taoline--log "[taoline debug] backend-teardown: %S" taoline--active-backend)
      (taoline--backend-teardown taoline--active-backend))
    (taoline--backend-setup backend)
    (setq taoline--active-backend backend)
    (setq taoline--last-str ""))
  ;; Если taoline выключен и не остался ни один оконный backend — убрать size-change-хук
  (unless (memq backend '(modeline))
    (when (bound-and-true-p taoline--size-change-hook-installed)
      (remove-hook 'window-size-change-functions #'taoline--window-size-change-handler)
      (setq taoline--size-change-hook-installed nil))))

;; ----------------------------------------------------------------------------
;; Core update logic

(defun taoline--maybe-update ()
  "Compose and update modeline if changed, or if echo area is empty (for echo backend).
Debugs all observable modeline update events to *taoline-logs* if debugging is enabled.
SKIP taoline update for internal/special buffers (starting with *).
In echo mode: ALWAYS refresh into echo area if it is empty or whitespace (even if string does not change).
"
  (unless (or (string-prefix-p "*" (buffer-name (current-buffer)))
              (minibufferp (current-buffer)))
    (when taoline--active-backend
      (let ((str (taoline-compose-modeline)))
        (cond
         ((eq taoline--active-backend 'echo)
          (let ((msg (current-message))
                (active-mini (active-minibuffer-window)))
            (cond
             (active-mini
              (taoline--log "[taoline debug] maybe-update: echo area NOT updated (active minibuffer) '%s'" str))
             ((or (not (equal str taoline--last-str))
                  (null msg)
                  (string-match-p "\\`[ \t\n\r]*\\'" (or msg "")))
              (taoline--log "[taoline debug] maybe-update: backend=echo will update (changed or echo empty, and minibuffer not active): '%s'" str)
              (taoline--backend-update taoline--active-backend str)
              (setq taoline--last-str str))
             (t
              (taoline--log "[taoline debug] maybe-update: not updating (echo in use, unchanged) '%s'" str)))))
         (t
          (unless (equal str taoline--last-str)
            (taoline--log "[taoline debug] maybe-update: backend=%s will update: '%s'"
                           taoline--active-backend str)
            (taoline--backend-update taoline--active-backend str)
            (setq taoline--last-str str))
          (when (equal str taoline--last-str)
            (taoline--log "[taoline debug] maybe-update: not updating (string unchanged) '%s'" str))))))))

(defun taoline--window-size-change-handler (_frame)
  "Force taoline update on any window size change in current frame."
  (when (memq taoline-display-backend '(modeline))
    (walk-windows
     (lambda (win)
       (with-selected-window win
         (force-mode-line-update t)))
     nil t)))

(defun taoline--add-hooks ()
  (dolist (hk taoline-update-hooks)
    (add-hook hk #'taoline--maybe-update))
  (when (eq taoline-display-backend 'echo)
    (add-hook 'post-command-hook #'taoline--echo-refresh-if-empty))
  ;; Add window-size-change hook only for relevant backends
  (when (memq taoline-display-backend '(modeline))
    (add-hook 'window-size-change-functions #'taoline--window-size-change-handler))
  (taoline--log "[taoline debug] hooks enabled, backend=%s hooks=%S"
           taoline-display-backend taoline-update-hooks))

(defun taoline--remove-hooks ()
  (dolist (hk taoline-update-hooks)
    (remove-hook hk #'taoline--maybe-update))
  ;; Extra cleanup for echo backend edge-case
  (remove-hook 'post-command-hook #'taoline--echo-refresh-if-empty)
  ;; Remove window-size-change hook for taoline display
  (remove-hook 'window-size-change-functions #'taoline--window-size-change-handler)
  (taoline--log "[taoline debug] hooks removed, backend=%s" taoline-display-backend))

;; ----------------------------------------------------------------------------
;; minor mode

;;;###autoload
(define-minor-mode taoline-mode
  "Global taoline minor mode."
  :global t
  (when (called-interactively-p 'any)
    (taoline--log "[taoline debug] taoline-mode state: %s backend: %S"
                   (if taoline-mode "ENABLED" "DISABLED")
                   taoline-display-backend))
  (if taoline-mode
      (progn
        (unless (eq taoline--active-backend taoline-display-backend)
          (taoline--log "[taoline debug] SYNCING backend! was %S now %S"
                         taoline--active-backend taoline-display-backend)
          (setq taoline--active-backend nil))
        (taoline--set-backend taoline-display-backend)
        (taoline--add-hooks)
        (taoline--maybe-update))
    (taoline--remove-hooks)
    ;; Сначала восстановим всем окнам их исходный mode-line (важно для backend=modeline)
    (taoline-reset-all-lines)
    ;; После восстановления вызываем teardown только для тех backend’ов,
    ;; которым это действительно нужно (modeline уже приведён в порядок).
    (when (and taoline--active-backend
               (not (eq taoline--active-backend 'modeline)))
      (taoline--backend-teardown taoline--active-backend))
    (setq taoline--active-backend nil)))

;;;###autoload
(defun taoline-set-backend (bk)
  "Select BK as the Taoline backend.

If `taoline-mode' is currently active, switch immediately
(handling teardown/setup).  If the mode is disabled, just remember
the choice so that the same backend is used the next time the mode
is enabled."
  (interactive
   (list
    (intern
     (completing-read
      "Backend: "
      (mapcar (lambda (cell) (symbol-name (car cell))) taoline--backend-table)
      nil t nil nil (symbol-name taoline-display-backend)))))
  (setq taoline-display-backend bk)
  (if taoline-mode
      (progn
        (unless (eq taoline--active-backend bk)
          (taoline--set-backend bk)
          (taoline--maybe-update)))
    (message "Taoline backend set to %s (will activate when taoline-mode is enabled)."
             bk)))

;;;###autoload
(defun taoline-add-segment (side fn)
  "Add FN to SIDE (:left/:right) segments."
  (let* ((cell (assq side taoline-segments))
         (lst (if cell (cdr cell) ())))
    (setf (alist-get side taoline-segments) (append lst (list fn)))))

;;;###autoload
(defun taoline-reset-all-lines ()
  "Forcefully restore all mode-line format in every window to
their original (pre-Taoline) values.

Useful when Taoline visuals get corrupted or you simply want to return to the
standard Emacs look without disabling the package entirely.  The function
ignores the currently active backend and just re-applies the values saved by
Taoline when it first replaced them.

It is safe to call at any time; if Taoline was not active in a window, nothing
is changed there."
  (interactive)
  ;; Walk through every live window
  (walk-windows
   (lambda (win)
     (with-selected-window win
       ;; ---------- mode-line ----------
       (let ((saved (window-parameter win 'taoline--modeline-old)))
         (when saved
           (set-window-parameter win 'mode-line-format saved)
           (set-window-parameter win 'taoline--modeline-old nil)
           (setq mode-line-format saved)))
       ;; Immediately redraw the window after restoring
       (force-mode-line-update t)))
   nil t)
  ;; Forget backend so Taoline doesn't try to update again until re-enabled
  (setq taoline--active-backend nil)
  (message "Taoline: mode-line were reset to defaults."))

(provide 'taoline)

;;; taoline.el ends here
