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
(eval-when-compile (require 'rx))
(require 'projectile nil t)
(require 'all-the-icons nil t)

;; Optional: Only if posframe is available and user wants it
(eval-and-compile
  (ignore-errors (require 'posframe)))

(defgroup taoline nil
  "Functional minimalist mode-line replacement."
  :group 'convenience
  :prefix "taoline-")

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

;; -- Posframe/echo modeline global autohide logic
;;
;; Для posframe/echo-бэкендов используем глобальный дефолт для mode-line,
;; чтобы новые буферы открывались уже без modeline. При отключении этих
;; бэкендов -- откатываем к стандартному поведению.
(defvar taoline--default-mode-line-format-backup nil
  "Сохраняет предыдущее значение `default-mode-line-format' для возврата.")

(defun taoline--set-modeline-format-globally (value &optional backup-restore)
  "Set `mode-line-format' to VALUE globally: in all buffers and windows.
If BACKUP-RESTORE is non-nil, backup or restore `default-mode-line-format'."
  (when backup-restore
    (if (eq value :default) ; This implies 'restore'
        (when taoline--default-mode-line-format-backup ; Only restore if we have a backup
          (setq-default mode-line-format taoline--default-mode-line-format-backup)
          (setq taoline--default-mode-line-format-backup nil)) ; Clear backup after restoring
      (unless taoline--default-mode-line-format-backup ; This implies 'backup' (when value is nil)
        (setq taoline--default-mode-line-format-backup (default-value 'mode-line-format)))))

  (let ((final-value
         (cond
          ((eq value :default) (default-value 'mode-line-format)) ; If :default, use Emacs's default
          (t value)))) ; Otherwise, use the provided value (nil or something else)

    (setq-default mode-line-format final-value) ; Apply to default (global)

    (dolist (win (window-list nil 'all))
      (with-selected-window win
        (setq mode-line-format final-value) ; Apply to window-local
        (force-mode-line-update t)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format final-value)))) ; Apply to buffer-local
  (force-mode-line-update t))

(defun taoline--autohide-modeline-globally ()
  "Hide modeline in all buffers and windows globally."
  (taoline--set-modeline-format-globally nil 'backup))

(defun taoline--unhide-modeline-globally ()
  "Restore modeline in all buffers and windows globally."
  (taoline--set-modeline-format-globally :default 'restore))


(defvar taoline--posframe-table (make-hash-table :test 'eq)
  "Hash table of posframes keyed by windows.")
(defun taoline--posframe--delete-all ()
  "Delete all taoline-related posframes and forcibly clean up."
  ;; Удаляем явно добавленные posframes из hash-table
  (maphash (lambda (_win pfname)
             (ignore-errors (posframe-delete pfname)))
           taoline--posframe-table)
  (clrhash taoline--posframe-table)
  ;; Пробуем удалить любой posframe с именем, связанным с taoline
  (when (boundp 'posframe--frame-list)
    (dolist (entry posframe--frame-list)
      (let ((name (car entry)))
        (when (and (stringp (symbol-name name))
                   (string-match-p "taoline" (symbol-name name)))
          (ignore-errors (posframe-delete name))))))
  ;; Также можно попытаться убить все posframe, если posframe.el >= 1.1 содержит posframe-delete-all
  (when (fboundp 'posframe-delete-all)
    (ignore-errors (posframe-delete-all))))

(defun taoline--posframe-setup ()
  "Show taoline posframes in all windows, including minibuffer. Hide modelines if necessary."
  (when taoline-autohide-modeline
    (taoline--autohide-modeline-globally))
  (walk-windows
   #'taoline--posframe-show
   nil 'all))

(defun taoline--posframe-update (_str)
  "Update taoline posframes for all windows, including minibuffer and buffers with no window."
  (walk-windows
   #'taoline--posframe-show
   nil 'all)
  ;; Убираем mode-line с буферов без окон, если нужно:
  (when taoline-autohide-modeline
    (dolist (buf (buffer-list))
      (unless (get-buffer-window buf 'visible)
        (with-current-buffer buf
          (setq mode-line-format nil))))))

(defun taoline--posframe-teardown ()
  "Remove taoline posframes in all windows and everywhere. Restore modelines if necessary."
  (taoline--posframe--delete-all)
  (taoline--unhide-modeline-globally))

(defun taoline--posframe-show (win)
  "Show or update the posframe in window WIN, including minibuffer window."
  (when (windowp win)
    (let* ((buffer (window-buffer win))
           (frame-w (frame-width (window-frame win)))
           (width   (max 1 (1- frame-w)))
           (str     (taoline-compose-modeline buffer win width))
           (pfname  (intern (format " taoline-posframe-%s"
                                    (or (window-parameter win 'window-id)
                                        (window-buffer win))))))
      (puthash win pfname taoline--posframe-table)
      (posframe-show
       pfname
       :string               str
       :position             (window-point win)
       :parent-window        win
       :width                width
       :left-fringe          0
       :right-fringe         0
       :internal-border-width 0
       :border-width         0
       :background-color     (face-attribute 'mode-line :background nil t)))))
(unless (fboundp 'taoline--posframe-show)
  (defalias 'taoline--posframe-show #'taoline--posframe-show))

;; ------------------------------------------------------

;; Customizable variables

(defcustom taoline-display-backend 'echo
  "Backend to show the modeline.
Supported values: modeline, echo, posframe"
  :type '(choice (const :tag "In-window modeline" modeline)
                 (const :tag "Echo area" echo)
                 (const :tag "Child frame (posframe)" posframe))
  :group 'taoline
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'taoline--switch-backend)
           (taoline--switch-backend val))))

(defcustom taoline-segments
  '((:left taoline-segment-icon-and-buffer taoline-segment-git-branch taoline-segment-project-name)
    (:center taoline-segment-echo-message)
    (:right taoline-segment-battery taoline-segment-major-mode taoline-segment-time))
  "List of segments to display in the modeline.
:LEFT, :CENTER and :RIGHT are lists of segment *function symbols*.
:CENTER will expand to fill all available space and be truncated to fit."
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

(defcustom taoline-right-padding 0
  "Number of spaces to pad on the right edge of the taoline."
  :type 'integer
  :group 'taoline)

;; ----------------------------------------------------------------------------
;; Faces

(defface taoline-base-face
  '((t :inherit mode-line
       :height 1.0
       :family "default"
       :box nil
       :underline nil
       :overline nil
       :inverse-video nil
       :extend t
       :align-to baseline)) ; align pretty text and icons
  "Base face for taoline with fixed height and baseline alignment to prevent jumping."
  :group 'taoline)

(defface taoline-echo-face
  '((t :inherit shadow))
  "Face for echo message segment."
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
  ;; Всегда сначала возвращаем глобальный mode-line-формат, чтобы избежать замусоривания состояния
  (taoline--unhide-modeline-globally)
  (setq taoline--active-backend new-backend)
  (let* ((setup (cdr (assq 'setup (cdr (assq taoline--active-backend taoline--backend-table)))))
         (update (cdr (assq 'update (cdr (assq taoline--active-backend taoline--backend-table))))))
    (when setup (funcall setup))
    (setq taoline--backend-data (cons setup update)))
  (cond
   ((and taoline-autohide-modeline (memq new-backend '(posframe echo)))
    (taoline--autohide-modeline-globally))
   ((eq new-backend 'modeline)
    (taoline--single-window-modeline-setup))))


;; ----------------------------------------------------------------------------
;; Segment definition and segment API

;;;###autoload
(defun taoline-set-backend (backend)
  "Set the taoline backend to BACKEND (`modeline`, `echo`, or `posframe').
Choice persists across Emacs sessions."
  (interactive
   (list (intern (completing-read "Taoline backend: "
                                  '(modeline echo posframe)
                                  nil t (symbol-name taoline-display-backend)))))
  (customize-set-variable 'taoline-display-backend backend)
  (message "Taoline backend set to: %s" backend))

(defmacro taoline-define-segment (name args &rest body)
  "Define a segment function NAME accepting ARGS, with BODY.
Registers the NAME in the segment table.
If ARGS is a single symbol, function will always receive the buffer.
If ARGS is an empty list, function will be called with no arguments."
  (declare (indent defun))
  (let ((func `(defun ,name ,args ,@body)))
    `(progn
       ,func
       (puthash ',name #',name taoline--segment-table))))

(defmacro taoline-define-simple-segment (name docstring &rest body)
  "Define a simple segment NAME (no buffer arg), register it, with DOCSTRING."
  `(taoline-define-segment ,name ()
     ,docstring
     ,@body))

(defun taoline--apply-segment (fn buffer)
  "Call segment FN with BUFFER or none. Always return a string (empty if nil, or error)."
  (condition-case err
      (let* ((arity (help-function-arglist fn t))
             (res (if (and (consp arity) (not (null arity))) ; arity = (buffer), call with buffer else no args
                      (funcall fn buffer)
                    (funcall fn))))
        (if (stringp res) res (or (and res (format "%s" res)) "")))
    (error (propertize (format "[SEGMENT ERROR: %s]" err) 'face 'error))))

(defun taoline--collect-segments (side buffer)
  "Return list of rendered segment strings for SIDE (:left/:right/:center) in BUFFER, skipping empty segments."
  (cl-loop for fn in (cdr (assq side taoline-segments))
           for str = (taoline--apply-segment fn buffer)
           unless (or (null str) (string= str ""))
           collect str))

;; ----------------------------------------------------------------------------
;; Core: Compose functionally the modeline string

(defun taoline-compose-modeline (&optional buffer window width)
  "Pure function: compose modeline string for BUFFER and WINDOW.
WIDTH, when supplied, is used as an upper bound.
If WINDOW is nil, the selected window is used.

Для posframe/echo backends учитываем ширину фрейма/минибуфера, иначе окна.
Для backend 'modeline' отрисовываем всегда, даже для служебных буферов (никаких исключений)."
  ;; Всегда работаем именно с окном.
  ;; 1. Если явно передали окно — используем его.
  ;; 2. Если передали фрейм — берём выбранное в нём окно.
  ;; 3. Если ничего не передали — берём (selected-window).
  (let* ((window
          (cond
           ((windowp (or window (selected-window)))
            (or window (selected-window)))
           ((framep (or window (selected-window)))
            (frame-selected-window (or window (selected-window))))
           (t (selected-window))))
         ;; Буфер по умолчанию — буфер этого окна, а не текущий,
         ;; что надёжнее при вычислении mode-line разных окон.
         (buffer (or buffer (window-buffer window))))
    ;; Если по ошибке передан фрейм, берём выбранное в нём окно.
    (when (framep window)
      (setq window (frame-selected-window window)))
    ;; Продолжаем только для «живых» окон
    (when (and (windowp window) (window-live-p window))
      (let* ((width (or width
                        (cond
                         ((eq taoline--active-backend 'posframe)
                          (frame-width (window-frame window)))
                         ((eq taoline--active-backend 'echo)
                          (let ((mbw (ignore-errors (window-width (minibuffer-window)))))
                            (if (and mbw (numberp mbw) (> mbw 0))
                                (max 0 (1- mbw))
                              (frame-width (window-frame window)))))
                         (t (window-width window)))))
             (left (mapconcat #'identity (taoline--collect-segments :left buffer) " "))
             (right (mapconcat #'identity (taoline--collect-segments :right buffer) " "))
             (center (mapconcat #'identity (taoline--collect-segments :center buffer) " "))
             ;; Compute width for center (all free space between left and right)
             (center-width (max 0 (- width (string-width left) (string-width right) taoline-right-padding 2))) ; -2 cushion
             (center-trunc (if (> (string-width center) center-width)
                               (truncate-string-to-width center center-width nil nil "…")
                             (progn
                               (concat center (make-string (max 0 (- center-width (string-width center))) ?\s)))))
             (sep-left (if (string= left "") "" " "))
             (sep-right (if (string= right "") "" " ")))
        (taoline--log "[compose-modeline] buffer=%S, major-mode=%S, window=%S, left='%s', center='%s', right='%s', width=%s, center-width=%s taoline-right-padding=%s"
                       (buffer-name buffer)
                       (with-current-buffer buffer major-mode)
                       window left center right width center-width taoline-right-padding)
        (concat
         (propertize
          (concat
           left
           sep-left
           center-trunc
           sep-right
           right
           (make-string (max 0 taoline-right-padding) ?\s))
          'face 'taoline-base-face))))))


;; ----------------------------------------------------------------------------
;; --- Show modeline only in selected window (backend 'modeline') ---

(defvar taoline--mode-line-format-backup nil
  "Backup of the initial global `mode-line-format` before taoline modified it.")

;; ----------------------------------------------------------------------
(defun taoline--single-window-modeline-setup ()
  "Set up taoline to render in the usual window mode-line.
Гарантирует передачу window, а не frame, в `taoline-compose-modeline`."
  (taoline--log "[single-window-modeline-setup] (safe redefinition)")
  (setq-default
   mode-line-format
   '((:eval
      ;; BUFFER — current-buffer; WIDTH определяется самой функции.
      (taoline-compose-modeline nil (selected-window))))))
;; Немедленно обновляем mode-line, чтобы исправление вступило в силу.

(defun taoline--single-window-modeline-update (&rest _)
  "Показывать taoline только в «логически активном» окне, скрывать modeline у остальных (per-window, не трогая локально buffer)."
  (taoline--log "[swmu] --- taoline--single-window-modeline-update called. Backend: %S. Selected win: %S / buffer: %s"
                taoline--active-backend
                (selected-window)
                (buffer-name (window-buffer (selected-window))))
  (when (eq taoline--active-backend 'modeline)
    ;; Находим истинное NORMAL окно: игнорируем minibuffer, или если Emacs вдруг решил его выбрать
    (let* ((all-wins (window-list nil 'no-minibuffer (selected-frame)))
           (main-win
            (car (or all-wins
                     (list (selected-window))))))
      (walk-windows
       (lambda (win)
         (let* ((buf (window-buffer win))
                (is-main (eq win main-win))
                (is-minibuf (minibufferp buf))
                (local-ml (with-current-buffer buf (and (local-variable-p 'mode-line-format) mode-line-format))))
           (taoline--log "[swmu] win=%s buffer=%s is-main=%S is-minibuf=%S BEFORE: window-param=%S, local mode-line=%S"
                         win (buffer-name buf) is-main is-minibuf
                         (window-parameter win 'mode-line-format) local-ml)
           (when (window-parameter win 'mode-line-format)
             (taoline--log "[swmu] win=%s: Clearing previous window-parameter 'mode-line-format'" win)
             (set-window-parameter win 'mode-line-format nil))
           (cond
            (is-main
             (set-window-parameter win 'mode-line-format
                                   `((:eval (taoline-compose-modeline (window-buffer ,win) ,win))))
             (taoline--log "[swmu] SET taoline modeline in win=%s buffer=%s" win (buffer-name buf)))
            (t
             (set-window-parameter win 'mode-line-format nil)
             (taoline--log "[swmu] UNSET modeline (set window-param nil) in win=%s buffer=%s" win (buffer-name buf))))
           (with-current-buffer buf
             (let ((post-ml (and (local-variable-p 'mode-line-format) mode-line-format)))
               (taoline--log "[swmu] win=%s, buffer=%s: AFTER set. local mode-line=%S, window-param=%S"
                             win (buffer-name buf)
                             post-ml (window-parameter win 'mode-line-format)))
             (force-mode-line-update t))))
       nil 'visible))))

(defun taoline--mod-hooks (action hooks fn)
  "Call ACTION ('add-hook or 'remove-hook) for FN in HOOKS."
  (dolist (hook hooks)
    (funcall action hook fn)))

(defconst taoline--modeline-update-hooks
  ;; window-selection-change-functions — только Emacs 27+, но window-configuration-change-hook — всегда.
  '(window-selection-change-functions window-configuration-change-hook window-size-change-functions)
  "Hooks relevant for single-window modeline change.")


(defun taoline--single-window-modeline-teardown ()
  "Восстановить обычные mode-line, отключить taoline в modeline."
  (taoline--log "[teardown] called: restoring mode-line globally and unsetting per-window param")
  ;; Удаляем только window-parameter, НЕ трогаем buffer local!
  (walk-windows
   (lambda (win)
     (let* ((buf (window-buffer win))
            (pre-window-param (window-parameter win 'mode-line-format))
            (pre-local-mode-line
             (with-current-buffer buf (and (local-variable-p 'mode-line-format) mode-line-format))))
       (taoline--log "[teardown] before win=%s buffer=%s  window-param=%S, local mode-line=%S"
                     win (buffer-name buf) pre-window-param pre-local-mode-line)
       (set-window-parameter win 'mode-line-format nil)
       (with-current-buffer buf
         (force-mode-line-update t)
         (let ((post-local-mode-line
                (and (local-variable-p 'mode-line-format) mode-line-format)))
           (taoline--log "[teardown] after  win=%s buffer=%s  window-param=%S  local mode-line=%S"
                         win (buffer-name buf)
                         (window-parameter win 'mode-line-format)
                         post-local-mode-line))))))
  (when taoline--mode-line-format-backup
    (taoline--log "[teardown] restoring default mode-line-format globally: %S" taoline--mode-line-format-backup)
    (setq-default mode-line-format taoline--mode-line-format-backup))
  (taoline--mod-hooks #'remove-hook taoline--modeline-update-hooks #'taoline--single-window-modeline-update)
  (remove-hook 'buffer-list-update-hook #'taoline--single-window-modeline-update)
  (remove-hook 'window-state-change-hook #'taoline--single-window-modeline-update)
  (remove-hook 'post-command-hook #'taoline--single-window-modeline-update)
  (setq taoline--mode-line-format-backup nil))

;; ----------------------------------------------------------------------------
;; Built-in segments

(taoline-define-segment taoline-segment-project-name (buffer)
  "Show the current Projectile project name, or empty if none."
  (let ((special (string-match-p "^\\*" (buffer-name buffer))))
    (taoline--log "[segment-project-name] buffer=%S, special=%S, major-mode=%S"
                  (buffer-name buffer) special (with-current-buffer buffer major-mode)))
  (when (and (featurep 'projectile) (buffer-file-name buffer))
    (let ((prj (projectile-project-name)))
      (when (and prj (not (string= prj "-")))
        (propertize (concat "(" prj ")") 'face 'font-lock-constant-face)))))

(taoline-define-segment taoline-segment-icon-and-buffer (buffer)
  "Display major mode icon (all-the-icons) and buffer/file-name (* if modified), optimized."
  (let ((special (string-match-p "^\\*" (buffer-name buffer))))
    (taoline--log "[segment-icon-and-buffer] buffer=%S, special=%S, major-mode=%S"
                  (buffer-name buffer) special (with-current-buffer buffer major-mode)))
  (let* ((icon (and (featurep 'all-the-icons)
                    (let ((ic (all-the-icons-icon-for-mode
                               (buffer-local-value 'major-mode buffer))))
                      (and (stringp ic) ic))))
         (name (buffer-name buffer)))
    (concat
     (if icon (concat icon " ") "")
     (propertize name 'face 'taoline-buffer-face)
     (when (buffer-modified-p buffer)
       (propertize "*" 'face 'taoline-modified-face)))))

(taoline-define-simple-segment taoline-segment-time
  "Display current time."
  (propertize (format-time-string "%H:%M") 'face 'taoline-time-face))

(taoline-define-simple-segment taoline-segment-echo-message
  "Show last message from echo area, truncated to available width."
  (let* ((msg
          (or
           (with-current-buffer (get-buffer "*Messages*")
             (save-excursion
               (goto-char (point-max))
               (if (bobp) "" (progn (forward-line -1) (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
           ""))
         (clean (replace-regexp-in-string "[\n\t]" " " msg)))
    (propertize (concat "|" clean "|") 'face 'taoline-echo-face)))

(taoline-define-segment taoline-segment-buffer-name (buffer)
  "Display current buffer name, with * if modified."
  (let ((special (string-match-p "^\\*" (buffer-name buffer))))
    (taoline--log "[segment-buffer-name] buffer=%S, special=%S, major-mode=%S"
                  (buffer-name buffer) special (with-current-buffer buffer major-mode)))
  (let ((name (buffer-name buffer)))
    (concat
     (propertize name 'face 'taoline-buffer-face)
     (when (buffer-modified-p buffer)
       (propertize "*" 'face 'taoline-modified-face)))))

(taoline-define-simple-segment taoline-segment-major-mode
  "Display the major mode (optimized)."
  (propertize (format-mode-line mode-name) 'face 'taoline-mode-face))

(taoline-define-segment taoline-segment-git-branch (buffer)
  "Display current git branch, if any."
  (let* ((default-directory (or (buffer-local-value 'default-directory buffer) default-directory))
         (branch (when (and default-directory (fboundp 'vc-git-root))
                   (let ((root (ignore-errors (vc-git-root default-directory))))
                     (when root
                       (with-temp-buffer
                         (let ((git-head (expand-file-name ".git/HEAD" root)))
                           (when (file-readable-p git-head)
                             (insert-file-contents git-head)
                             (when (re-search-forward "ref: refs/heads/\\(.*\\)" nil t)
                               (match-string 1))))))))))
    (if branch
        (propertize (format " %s" branch) 'face 'taoline-git-face)
      "")))

(taoline-define-simple-segment taoline-segment-battery
  "Display battery icon and percentage, optimized."
  (when (functionp battery-status-function)
    (let* ((data (ignore-errors (funcall battery-status-function)))
           (percent-str (or (cdr (assoc "percentage" data))
                            (cdr (assoc 112 data))
                            (cdr (assoc ?p data)))))
      (when (and percent-str (string-match-p "^[0-9]+$" percent-str))
        (let* ((percent (string-to-number percent-str)))
          (if (featurep 'all-the-icons)
              (let ((icon (cond
                           ((>= percent 95) (all-the-icons-faicon "battery-full" :height 0.95 :v-adjust -0.05))
                           ((>= percent 70) (all-the-icons-faicon "battery-three-quarters" :height 0.95 :v-adjust -0.05))
                           ((>= percent 45) (all-the-icons-faicon "battery-half" :height 0.95 :v-adjust -0.05))
                           ((>= percent 20) (all-the-icons-faicon "battery-quarter" :height 0.95 :v-adjust -0.05))
                           (t (all-the-icons-faicon "battery-empty" :height 0.95 :v-adjust -0.05)))))
                (format "%s %s%%" icon percent))
            (format "%s%%" percent)))))))

;; ----------------------------------------------------------------------------
;; Backend registry

(defvar taoline--backend-table
  '((echo    . ((setup . taoline--echo-setup)
                (update . taoline--echo-update)
                (teardown . taoline--echo-teardown)))
    (posframe . ((setup . taoline--posframe-setup)
                 (update . taoline--posframe-update)
                 (teardown . taoline--posframe-teardown)))
    (modeline . ((setup . taoline--single-window-modeline-setup)
                 (teardown . taoline--single-window-modeline-teardown))))
  "Backend function registry.")

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
    (taoline--maybe-update)))

(defconst taoline--echo-hooks
  '(post-command-hook minibuffer-exit-hook)
  "Echo backend required hooks list.")

(defun taoline--echo-setup ()
  (when taoline-autohide-modeline
    (taoline--autohide-modeline-globally))
  (taoline--mod-hooks #'add-hook '(post-command-hook) #'taoline--echo-refresh-if-empty)
  (taoline--mod-hooks #'add-hook '(minibuffer-exit-hook) #'taoline--echo-minibuffer-exit-refresh)
  (when (and (boundp 'echo-area-clear-hook)
             (not taoline--echo-clear-hooked))
    (add-hook 'echo-area-clear-hook #'taoline--echo-clear-refresh)
    (setq taoline--echo-clear-hooked t))
  (setq taoline--echo-postcmd-hooked t taoline--echo-minibuf-hooked t)
  (taoline--log "[taoline echo-debug] echo-setup: run initial refresh")
  (run-at-time 0.01 nil #'taoline--echo-refresh-if-empty))

(defun taoline--echo-update (_str)
  (when (and (not (active-minibuffer-window)) taoline-mode)
    (let* ((buf (window-buffer (selected-window)))
           (win (minibuffer-window))
           (win-width (window-width win))
           (str (taoline-compose-modeline buf (selected-window))))
      (when (stringp str)
        ;; Trim string if it exceeds echo area width minus padding (2 chars is typical)
        (let* ((maxlen (max 1 (- win-width 2)))
               (shown-str (if (> (string-width str) maxlen)
                              (truncate-string-to-width str maxlen)
                            str))
               (message-log-max nil))
          (message "%s" shown-str))))))

(defun taoline--echo-teardown ()
  (taoline--mod-hooks #'remove-hook '(post-command-hook) #'taoline--echo-refresh-if-empty)
  (taoline--mod-hooks #'remove-hook '(minibuffer-exit-hook) #'taoline--echo-minibuffer-exit-refresh)
  (when (and taoline--echo-clear-hooked (boundp 'echo-area-clear-hook))
    (remove-hook 'echo-area-clear-hook #'taoline--echo-clear-refresh)
    (setq taoline--echo-clear-hooked nil))
  (setq taoline--echo-postcmd-hooked nil taoline--echo-minibuf-hooked nil)
  (taoline--unhide-modeline-globally)
  (let ((msg (current-message)))
    (when (or (null msg) (string-match-p "\\`[ \t\n\r]*\\'" msg)) (message ""))))

;; -- Modeline backend (shows taoline only in selected window, hides in others)

;; For a clean teardown we must be able to restore the exact modeline each
;; window had before taoline touched it.  We therefore remember the original
;; value in a dedicated window-parameter called
;; `taoline--saved-mode-line-format'.

(defun taoline--modeline--post-command ()
  "Update taoline modeline so that it is shown only in the selected window."
  (taoline--modeline-update nil))

(defun taoline--modeline-setup ()
  "Show taoline in the current window's modeline, hide in all other windows."
  (taoline--modeline-update nil)
  (add-hook 'post-command-hook #'taoline--modeline--post-command)
  (add-hook 'window-configuration-change-hook #'taoline--modeline--post-command)
  (when (boundp 'window-state-change-hook)
    (add-hook 'window-state-change-hook #'taoline--modeline--post-command))
  (add-hook 'buffer-list-update-hook #'taoline--modeline--post-command)
  (force-mode-line-update t))

(defun taoline--modeline-update (_str)
  "Show taoline only in the selected window, hide it in all others.
Before the first override of any window we store its original value of
`mode-line-format' in the helper parameter
`taoline--saved-mode-line-format' so it can be restored later."
  (let ((selected (selected-window)))
    (walk-windows
     (lambda (win)
       (with-selected-window win
         ;; Save the original setting once.
         (unless (window-parameter win 'taoline--saved-mode-line-format)
           (set-window-parameter
            win 'taoline--saved-mode-line-format
            (window-parameter win 'mode-line-format)))
         (if (eq win selected)
             ;; Selected window → show taoline.
             (set-window-parameter win 'mode-line-format
                                   '(:eval (taoline-compose-modeline)))
           ;; Other windows → hide modeline.
           (set-window-parameter win 'mode-line-format nil))))
     nil t))
  (force-mode-line-update t))

(defun taoline--modeline-teardown ()
  "Restore each window's original modeline and remove hooks."
  (remove-hook 'post-command-hook #'taoline--modeline--post-command)
  (remove-hook 'window-configuration-change-hook #'taoline--modeline--post-command)
  (when (boundp 'window-state-change-hook)
    (remove-hook 'window-state-change-hook #'taoline--modeline--post-command))
  (remove-hook 'buffer-list-update-hook #'taoline--modeline--post-command)

  ;; Walk through all windows, restore the saved modeline, and clean up
  ;; helper parameters.
  (walk-windows
   (lambda (win)
     (with-selected-window win
       (let ((saved (window-parameter win 'taoline--saved-mode-line-format)))
         (set-window-parameter win 'mode-line-format saved)
         (set-window-parameter win 'taoline--saved-mode-line-format nil))))
   nil t)
  (force-mode-line-update t))

;; -- Posframe backend

(defvar taoline--posframe-buffer " *taoline-posframe*")
(defvar taoline--posframe-name "taoline-posframe")

(defun taoline--posframe-setup ()
  (require 'posframe)
  (when taoline-autohide-modeline
    (taoline--autohide-modeline-globally))
  (ignore-errors (posframe-delete taoline--posframe-buffer)))

(defun taoline--posframe-update (str)
  (require 'posframe)
  ;; Гарантированно одна строка, убираем \n (если по ошибке попал):
  (let* ((clean-str (replace-regexp-in-string "\n" " " str))
         ;; Вычисляем ширину текущего окна, чтобы posframe занимал ровно столько символов
         (width (window-width (frame-selected-window)))
         (mono-str (propertize clean-str 'face 'taoline-posframe-face)))
    (posframe-show
     taoline--posframe-buffer
     :string mono-str
     :name taoline--posframe-name
     :poshandler 'posframe-poshandler-frame-bottom-left-corner
     :min-width width
     :max-width width
     :min-height 1
     :max-height 1
     :border-width 0
     :override-parameters
     '((font . "monospace")
       (left-fringe . 0)
       (right-fringe . 0)
       (vertical-scroll-bars . nil)
       (line-spacing . 0))) ;; Явно убираем доп. высоту строк
    ))

(defun taoline--posframe-teardown ()
  "Полностью удалить и скрыть posframe и его буфер, даже если был сбой."
  (ignore-errors
    (require 'posframe)
    (when (fboundp 'posframe-hide)
      (posframe-hide taoline--posframe-buffer))
    (when (fboundp 'posframe-delete)
      (posframe-delete taoline--posframe-buffer))
    ;; Дополнительно удалим сам буфер, если он вдруг остался
    (let ((buf (get-buffer taoline--posframe-buffer)))
      (when buf (kill-buffer buf))))
  (taoline--unhide-modeline-globally))

;; ----------------------------------------------------------------------------
;; Backend dispatcher

(defvar taoline--backend-table
  '((echo    (setup . taoline--echo-setup)
             (update . taoline--echo-update)
             (teardown . taoline--echo-teardown))
    (modeline (setup . taoline--modeline-setup)
              (update . taoline--modeline-update)
              (teardown . taoline--modeline-teardown))
    (posframe (setup . taoline--posframe-setup)
              (update . taoline--posframe-update)
              (teardown . taoline--posframe-teardown))))

(defun taoline--backend-action (backend key &optional arg)
  "Call backend handler for BACKEND and action KEY. Pass ARG if needed."
  (let ((fn (alist-get key (alist-get backend taoline--backend-table))))
    (when fn (if arg (funcall fn arg) (funcall fn)))))

(defun taoline--backend-setup   (backend) (taoline--log "[taoline debug] backend-setup: %S" backend)
       (taoline--backend-action backend 'setup))
(defun taoline--backend-update  (backend str) (taoline--log "[taoline debug] backend-update: backend=%s, str='%s'" backend str)
       (taoline--backend-action backend 'update str))
(defun taoline--backend-teardown(backend) (taoline--log "[taoline debug] backend-teardown: %S" backend)
       (taoline--backend-action backend 'teardown))

(defun taoline--set-backend (backend)
  "Switch to BACKEND, handling setup/teardown and cleanup."
  (unless (eq backend taoline--active-backend)
    (when taoline--active-backend
      (taoline--backend-teardown taoline--active-backend)
      (taoline-reset-all-lines)) ;; Сброс спецэффектов всех окон обязательно!
    (setq taoline--active-backend backend)
    (setq taoline--last-str "")
    (taoline--backend-setup backend)))

;; ----------------------------------------------------------------------------
;; Core update logic

(defun taoline--maybe-update ()
  "Compose and update modeline if changed, or if echo area is empty (for echo backend).
Debugs all observable modeline update events to *taoline-logs* if debugging is enabled.
SKIP taoline update for internal/special buffers (starting with *).
In echo mode: ALWAYS refresh into echo area if it is empty or whitespace (even if string does not change).
"
  (unless (minibufferp (current-buffer))
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
              (taoline--log "[taoline debug] maybe-update: backend=echo will update: '%s'" str)
              (taoline--backend-update taoline--active-backend str)
              (setq taoline--last-str str))
             (t
              (taoline--log "[taoline debug] maybe-update: not updating (echo in use, unchanged) '%s'" str)))))
         ((eq (selected-window) (get-buffer-window (current-buffer)))
          (if (not (equal str taoline--last-str))
              (progn
                (taoline--log "[taoline debug] maybe-update: backend=%s will update: '%s'"
                              taoline--active-backend str)
                (taoline--backend-update taoline--active-backend str)
                (setq taoline--last-str str))
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
  "Установить все хуки, нужные текущему backend."
  (dolist (hk taoline-update-hooks)
    (add-hook hk #'taoline--maybe-update))
  (when (eq taoline-display-backend 'echo)
    (add-hook 'post-command-hook #'taoline--echo-refresh-if-empty))
  (when (eq taoline-display-backend 'modeline)
    (add-hook 'window-size-change-functions #'taoline--window-size-change-handler)))

(defun taoline--remove-hooks ()
  "Удалить все хуки, связанные с backend."
  (dolist (hk taoline-update-hooks)
    (remove-hook hk #'taoline--maybe-update))
  (remove-hook 'post-command-hook #'taoline--echo-refresh-if-empty)
  (remove-hook 'window-size-change-functions #'taoline--window-size-change-handler))

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
        (taoline--set-backend taoline-display-backend)
        (taoline--add-hooks)
        (taoline--maybe-update))
    (taoline--remove-hooks)
    (when taoline--active-backend
      (taoline--backend-teardown taoline--active-backend))
    (taoline-reset-all-lines)
    ;; Всегда корректно скрываем posframe, если он остался
    (taoline--posframe-teardown)
    (setq taoline--active-backend nil)))

;;;###autoload
(defun taoline-add-segment (side fn)
  "Add FN to SIDE (:left/:right) segments."
  (let* ((cell (assq side taoline-segments))
         (lst (if cell (cdr cell) ())))
    (setf (alist-get side taoline-segments) (append lst (list fn)))))

;;;###autoload
(defun taoline-reset-all-lines ()
  "Restore all default modelines and remove window-parameter shadowing for mode-line-format."
  (interactive)
  (dolist (win (window-list nil 'nomini))
    (with-selected-window win
      (set-window-parameter win 'mode-line-format nil)))
  (setq taoline--last-taoline-window nil)
  (force-mode-line-update t)
  (message "Taoline: all mode-lines forcibly reset to global mode-line-format."))

(defface taoline-posframe-face
  '((t :inherit default))
  "Face for the Taoline posframe text.")

(force-mode-line-update t)

;; ---------------------------------------------------------------------------
;; Patch: make sure the modeline backend is completely torn down
;; ---------------------------------------------------------------------------

(defun taoline--modeline-clear-all-params ()
  "Remove any taoline‐related `mode-line-format' overrides in every window."
  (walk-windows
   (lambda (win)
     (set-window-parameter win 'mode-line-format nil)
     (set-window-parameter win 'taoline--saved-mode-line-format nil))
   nil t)
  (force-mode-line-update t))

(defun taoline--modeline-teardown ()
  "Completely disable taoline modeline backend.
Restores original mode-lines, removes every hook we installed and clears
internal caches so no further calls to `taoline-compose-modeline' (and
therefore no log messages) are made."
  ;; Remove every hook we added
  (remove-hook 'post-command-hook #'taoline--modeline--post-command)
  (remove-hook 'window-configuration-change-hook #'taoline--modeline--post-command)
  (when (boundp 'window-state-change-hook)
    (remove-hook 'window-state-change-hook #'taoline--modeline--post-command))
  (remove-hook 'buffer-list-update-hook #'taoline--modeline--post-command)

  ;; Clear all window parameters we touched
  (taoline--modeline-clear-all-params)

  ;; Reset cached rendered string so nothing stale is reused
  (setq taoline--last-str ""))

;; ---------------------------------------------------------------------------
(provide 'taoline)

;;; taoline.el ends here
