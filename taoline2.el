;;; taoline2.el --- Functional minimalist modeline for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Your Name

;; Author: Your Name <your@email.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (posframe "1.1.0"))
;; Keywords: mode-line, minimal, functional, faces
;; URL: https://github.com/your/taoline2

;; License: MIT

;;; Commentary:

;; taoline2.el provides a minimal modeline for Emacs with modern,
;; functional-programming-based architecture. All logic is split into pure
;; functions, and all side-effects are isolated in backend modules.
;;
;; Features:
;; - Minimalist, highly customizable segments.
;; - Functional style: pure segment functions, stateless composition.
;; - Multiple backends: echo area, header-line, and posframe.
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

(defgroup taoline2 nil
  "Functional minimalist mode-line replacement."
  :group 'convenience
  :prefix "taoline2-")

;; ----------------------------------------------------------------------------
;; Customizable variables

(defcustom taoline2-display-backend 'modeline
  "Backend to show the modeline.
Supported values: 'modeline, 'echo, 'header, 'posframe"
  :type '(choice (const :tag "In-window modeline" modeline)
                 (const :tag "Echo area" echo)
                 (const :tag "Header line" header)
                 (const :tag "Child frame (posframe)" posframe))
  :group 'taoline2)

(defcustom taoline2-segments
  '((:left taoline2-segment-git-branch taoline2-segment-buffer-name)
    (:right taoline2-segment-major-mode taoline2-segment-time))
  "List of segments to display in the modeline.
:LEFT and :RIGHT are lists of segment *function symbols*."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'taoline2)

(defcustom taoline2-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "A list of hooks triggering taoline2 update."
  :type '(repeat symbol)
  :group 'taoline2)

;; ----------------------------------------------------------------------------
;; Faces

(defface taoline2-base-face
  '((t :inherit mode-line))
  "Base face for taoline2."
  :group 'taoline2)

(defface taoline2-time-face
  '((t :inherit success))
  "Face for time segment."
  :group 'taoline2)

(defface taoline2-buffer-face
  '((t :inherit mode-line-buffer-id))
  "Face for buffer name segment."
  :group 'taoline2)

(defface taoline2-modified-face
  '((t :inherit warning))
  "Face for modified indicator segment."
  :group 'taoline2)

(defface taoline2-git-face
  '((t :inherit font-lock-keyword-face))
  "Face for git branch segment."
  :group 'taoline2)

(defface taoline2-mode-face
  '((t :inherit font-lock-type-face))
  "Face for major mode segment."
  :group 'taoline2)

;; ----------------------------------------------------------------------------
;; Internal tables and state

(defvar taoline2--segment-table (make-hash-table :test 'eq)
  "Table for registered taoline2 segment functions.")

(defvar taoline2--last-str ""
  "Cache for the last rendered modeline string.")

(defvar taoline2--active-backend nil
  "Currently active backend symbol.")

(defvar taoline2--backend-data nil
  "Backend data for teardown or cleanup.")

;; ----------------------------------------------------------------------------
;; Segment definition and segment API

(defmacro taoline2-define-segment (name args &rest body)
  "Define a segment function NAME accepting ARGS, with BODY.
Registers the NAME in the segment table."
  (declare (indent defun))
  `(progn
     (defun ,name ,args ,@body)
     (puthash ',name #',name taoline2--segment-table)))

(defun taoline2--apply-segment (fn buffer)
  "Call segment FN with BUFFER."
  (condition-case err
      (funcall fn buffer)
    (error (propertize (format "[SEGMENT ERROR: %s]" err) 'face 'error))))

(defun taoline2--collect-segments (side buffer)
  "For SIDE (:left/:right), collect and evaluate segment functions for BUFFER."
  (let ((fns (cdr (or (assq side taoline2-segments) ())))
        (results '()))
    (dolist (fn fns)
      (push (taoline2--apply-segment fn buffer) results))
    (nreverse results)))

;; ----------------------------------------------------------------------------
;; Core: Compose functionally the modeline string

(defun taoline2-compose-modeline (&optional buffer frame)
  "Pure function: compose modeline string for BUFFER and FRAME."
  (let* ((buffer (or buffer (current-buffer)))
         (frame (or frame (selected-frame)))
         (width (frame-width frame))
         (left (string-join (taoline2--collect-segments :left buffer) " "))
         (right (string-join (taoline2--collect-segments :right buffer) " "))
         (mid-space (max 1 (- width (string-width left) (string-width right)))))
    (concat
     (propertize left 'face 'taoline2-base-face)
     (make-string mid-space ?\s)
     (propertize right 'face 'taoline2-base-face))))

;; ----------------------------------------------------------------------------
;; Built-in segments

(taoline2-define-segment taoline2-segment-time (buffer)
  "Display current time."
  (propertize (format-time-string "%H:%M")
              'face 'taoline2-time-face))

(taoline2-define-segment taoline2-segment-buffer-name (buffer)
  "Display current buffer name, with * if modified."
  (let* ((name (buffer-name buffer))
         (mod (if (buffer-modified-p buffer) "*" "")))
    (concat
     (propertize name 'face 'taoline2-buffer-face)
     (when (buffer-modified-p buffer)
       (propertize "*" 'face 'taoline2-modified-face)))))

(taoline2-define-segment taoline2-segment-major-mode (buffer)
  "Display the major mode."
  (propertize (format-mode-line mode-name)
              'face 'taoline2-mode-face))

;; Simple git branch detector; can be redefined by user
(taoline2-define-segment taoline2-segment-git-branch (buffer)
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
        (propertize (concat " " branch) 'face 'taoline2-git-face)
      "")))

;; ----------------------------------------------------------------------------
;; Backend registry

(defvar taoline2--backend-table
  '((modeline . ((setup . taoline2--modeline-setup)
                 (update . taoline2--modeline-update)
                 (teardown . taoline2--modeline-teardown)))
    (echo    . ((setup . taoline2--echo-setup)
                (update . taoline2--echo-update)
                (teardown . taoline2--echo-teardown)))
    (header  . ((setup . taoline2--header-setup)
                (update . taoline2--header-update)
                (teardown . taoline2--header-teardown)))
    (posframe . ((setup . taoline2--posframe-setup)
                 (update . taoline2--posframe-update)
                 (teardown . taoline2--posframe-teardown))))
  "Backend function registry.")

;; -- Modeline backend (in-window modeline)

(defvar-local taoline2--modeline-old nil)
(defvar-local taoline2--modeline-expression nil)

(defun taoline2--modeline-setup ()
  "Setup for `modeline' backend.
Прячет обычный `mode-line-format' (сохранив в
`taoline2--saved-mode-line-format') и устанавливает собственную
Taoline-строку."
  (taoline2--hide-default-mode-line)
  "Setup taoline2 as the current window's modeline."
  (unless taoline2--modeline-old
    (setq taoline2--modeline-old (default-value 'mode-line-format)))
  (setq taoline2--modeline-expression
        '(:eval (taoline2-compose-modeline)))
  (setq mode-line-format taoline2--modeline-expression))

(defun taoline2--modeline-update (_str)
  "Force update of the modeline."
  (force-mode-line-update t))

(defun taoline2--modeline-teardown ()
  "Teardown for `modeline' backend.
Убирает Taoline из `mode-line-format' и восстанавливает сохранённый
пользовательский mode-line."
  (taoline2--restore-default-mode-line)
  "Restore previous mode-line-format."
  (when taoline2--modeline-old
    (setq mode-line-format taoline2--modeline-old)
    (setq taoline2--modeline-old nil)
    (setq taoline2--modeline-expression nil)))

;; -- Echo area backend

(defgroup taoline2 nil
  "Slim, extensible, functional mode-line for Emacs."
  :group 'convenience)

(defcustom taoline2-debug t
  "If non-nil, taoline2 logs to the *taoline-logs* buffer instead of/in addition to *Messages*."
  :type 'boolean
  :group 'taoline2)

(defvar taoline2--log-buffer "*taoline-logs*"
  "Buffer name for taoline2 debug log.")

(defun taoline2--log (fmt &rest args)
  "Log message FMT with ARGS to `taoline2--log-buffer' if `taoline2-debug' is non-nil.
Lines are appended at the end of the buffer, which is auto-created.
Never writes debug to *Messages*."
  (when taoline2-debug
    (let ((msg (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create taoline2--log-buffer)
        (goto-char (point-max))
        (insert msg "\n")))))

(defvar taoline2--echo-postcmd-hooked nil
  "Internal: Whether taoline2 echo post-command refresh is enabled.")

(defvar taoline2--echo-minibuf-hooked nil
  "Internal: Whether taoline2 echo minibuffer exit refresh is enabled.")

(defvar taoline2--echo-clear-hooked nil
  "Internal: Whether taoline2 echo `echo-area-clear-hook' refresh is enabled (Emacs 29+).")

(defun taoline2--echo-refresh-if-empty ()
  "Show taoline2 in the echo area if minibuffer is gone and echo area is empty or contains only whitespace.
Hide taoline2 only when something meaningful (not just whitespace) is in the echo area,
or when minibuffer is active or minibuffer input is ongoing.
Adds debug output to *taoline-logs* if debugging is enabled."
  (let* ((taoline-debug-info
          (list
           :taoline2-mode taoline2-mode
           :active-backend taoline2--active-backend
           :display-backend taoline2-display-backend
           :active-minibuffer (active-minibuffer-window)
           :current-message (current-message))))
    (taoline2--log "[taoline2 echo-debug] refresh: %S" taoline-debug-info)
    (when (and taoline2-mode
               (eq taoline2--active-backend 'echo)
               (not (active-minibuffer-window)))
      (let ((msg (current-message)))
        (taoline2--log "[taoline2 echo-debug] mini='%s'" msg)
        (if (or (null msg)
                (string-match-p "\\`[ \t\n\r]*\\'" msg))
            (progn
              (taoline2--log "[taoline2 echo-debug] minibuffer is empty, refreshing taoline2 (run-at-time)")
              (run-at-time 0 nil
                           (lambda ()
                             (taoline2--log "[taoline2 echo-debug] (delayed) calling taoline2--maybe-update")
                             (taoline2--maybe-update))))
          (taoline2--log "[taoline2 echo-debug] minibuffer not empty, taoline2 not shown"))))))

(defun taoline2--echo-minibuffer-exit-refresh ()
  "Hook for minibuffer-exit to restore taoline2 if needed."
  (run-at-time 0.05 nil #'taoline2--echo-refresh-if-empty))

(defun taoline2--echo-clear-refresh ()
  "Hook for `echo-area-clear-hook' (Emacs 29+) to restore Taoline
немедленно, как только echo-area становится пустой."
  (taoline2--log "[taoline2 echo-debug] echo-area-clear-hook triggered")
  (when (and taoline2-mode
             (eq taoline2--active-backend 'echo)
             (not (active-minibuffer-window)))
    ;; Прямой вызов без таймера, чтобы не было заметной задержки.
    (taoline2--maybe-update)))

(defun taoline2--echo-setup ()
  ;; Ensure echo restoring logic is installed
  (unless taoline2--echo-postcmd-hooked
    (add-hook 'post-command-hook #'taoline2--echo-refresh-if-empty))
  (setq taoline2--echo-postcmd-hooked t)

  (unless taoline2--echo-minibuf-hooked
    (add-hook 'minibuffer-exit-hook #'taoline2--echo-minibuffer-exit-refresh))
  (setq taoline2--echo-minibuf-hooked t)

  ;; Emacs 29+: используем echo-area-clear-hook для мгновенного восстановления
  (when (and (boundp 'echo-area-clear-hook)
             (not taoline2--echo-clear-hooked))
    (add-hook 'echo-area-clear-hook #'taoline2--echo-clear-refresh)
    (setq taoline2--echo-clear-hooked t))

  (taoline2--log "[taoline2 echo-debug] echo-setup: run initial refresh")
  ;; Небольшая задержка для первого показа, чтобы не мешать стартовым сообщениям
  (run-at-time 0.01 nil #'taoline2--echo-refresh-if-empty))

(defun taoline2--echo-update (str)
  "Update echo area with STR if minibuffer is not active.
No debug/trace output goes to the minibuffer or *Messages*; debug stays in *taoline-logs* only.
НИКОГДА не писать taoline в echo если buffer спецбуфер ИЛИ str пустой."
  (let ((active-minibuffer (active-minibuffer-window)))
    (taoline2--log "[taoline2 echo-debug] echo-update: active-minibuffer=%S str='%s'"
                   active-minibuffer str)
    (when (and (not active-minibuffer)
               (stringp str)
               (not (string-prefix-p "*" (buffer-name (current-buffer))))
               (not (equal str "")))
      (let ((message-log-max nil)) (message "%s" str)))))

(defun taoline2--echo-teardown ()
  (when taoline2--echo-postcmd-hooked
    (remove-hook 'post-command-hook #'taoline2--echo-refresh-if-empty)
    (setq taoline2--echo-postcmd-hooked nil))
  (when taoline2--echo-minibuf-hooked
    (remove-hook 'minibuffer-exit-hook #'taoline2--echo-minibuffer-exit-refresh)
    (setq taoline2--echo-minibuf-hooked nil))
  (when (and taoline2--echo-clear-hooked (boundp 'echo-area-clear-hook))
    (remove-hook 'echo-area-clear-hook #'taoline2--echo-clear-refresh)
    (setq taoline2--echo-clear-hooked nil))
  (let ((msg (current-message)))
    (when (or (null msg)
              (string-match-p "\\`[ \t\n\r]*\\'" msg))
      (message ""))))

;; -- Header line backend

(defvar-local taoline2--header-old nil)
(defvar-local taoline2--header-expression nil)

(defun taoline2--header-setup ()
  (unless taoline2--header-old
    (setq taoline2--header-old (default-value 'header-line-format)))
  (setq taoline2--header-expression
        '(:eval (taoline2-compose-modeline)))
  (setq header-line-format taoline2--header-expression))

(defun taoline2--header-update (_str)
  (force-mode-line-update t))

(defun taoline2--header-teardown ()
  (when taoline2--header-old
    (setq header-line-format taoline2--header-old)
    (setq taoline2--header-old nil)
    (setq taoline2--header-expression nil)))

;; -- Posframe backend

(defvar taoline2--posframe-buffer " *taoline2-posframe*")
(defvar taoline2--posframe-name "taoline2-posframe")

(defun taoline2--posframe-setup ()
  (require 'posframe)
  (posframe-delete taoline2--posframe-buffer))

(defun taoline2--posframe-update (str)
  (require 'posframe)
  (let* ((width (frame-width))
         (height 1)
         (params '((left-fringe . 8) (right-fringe . 8))))
    (posframe-show
     taoline2--posframe-buffer
     :string str
     :name taoline2--posframe-name
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width width
     :min-height height
     :border-width 0
     :override-parameters params)))

(defun taoline2--posframe-teardown ()
  (ignore-errors
    (require 'posframe)
    (posframe-delete taoline2--posframe-buffer)))

;; ----------------------------------------------------------------------------
;; Backend dispatcher

(defun taoline2--get-backend (backend)
  (alist-get backend taoline2--backend-table))

(defun taoline2--backend-setup (backend)
  (taoline2--log "[taoline2 debug] backend-setup: %S" backend)
  (let ((b (taoline2--get-backend backend)))
    (when b (funcall (alist-get 'setup b)))))

(defun taoline2--backend-update (backend str)
  (taoline2--log "[taoline2 debug] backend-update: backend=%s, str='%s'" backend str)
  (let ((b (taoline2--get-backend backend)))
    (when b (funcall (alist-get 'update b) str))))

(defun taoline2--backend-teardown (backend)
  (taoline2--log "[taoline2 debug] backend-teardown: %S" backend)
  (let ((b (taoline2--get-backend backend)))
    (when b (funcall (alist-get 'teardown b)))))

(defun taoline2--set-backend (backend)
  "Switch to BACKEND, handling setup/teardown **и** правильное
скрытие / восстановление обычного mode-line через backend-функции."
  (taoline2--log "[taoline2 debug] set-backend called: %S" backend)
  (setq taoline2-display-backend backend)
  (unless (eq backend taoline2--active-backend)
    (when taoline2--active-backend
      (taoline2--log "[taoline2 debug] backend-teardown: %S" taoline2--active-backend)
      (taoline2--backend-teardown taoline2--active-backend))
    (taoline2--backend-setup backend)
    (setq taoline2--active-backend backend)
    (setq taoline2--last-str ""))) ;; force refresh

;; ----------------------------------------------------------------------------
;; Core update logic

(defun taoline2--maybe-update ()
  "Compose and update modeline if changed, or if echo area is empty (for echo backend).
Debugs all observable modeline update events to *taoline-logs* if debugging is enabled.
SKIP taoline update for internal/special buffers (starting with *).
In echo mode: ALWAYS refresh into echo area if it is empty or whitespace (even if string does not change).
"
  (unless (or (string-prefix-p "*" (buffer-name (current-buffer)))
              (minibufferp (current-buffer)))
    (when taoline2--active-backend
      (let ((str (taoline2-compose-modeline)))
        (cond
         ((eq taoline2--active-backend 'echo)
          (let ((msg (current-message))
                (active-mini (active-minibuffer-window)))
            (cond
             (active-mini
              (taoline2--log "[taoline2 debug] maybe-update: echo area NOT updated (active minibuffer) '%s'" str))
             ((or (not (equal str taoline2--last-str))
                  (null msg)
                  (string-match-p "\\`[ \t\n\r]*\\'" (or msg "")))
              (taoline2--log "[taoline2 debug] maybe-update: backend=echo will update (changed or echo empty, and minibuffer not active): '%s'" str)
              (taoline2--backend-update taoline2--active-backend str)
              (setq taoline2--last-str str))
             (t
              (taoline2--log "[taoline2 debug] maybe-update: not updating (echo in use, unchanged) '%s'" str)))))
         (t
          (unless (equal str taoline2--last-str)
            (taoline2--log "[taoline2 debug] maybe-update: backend=%s will update: '%s'"
                           taoline2--active-backend str)
            (taoline2--backend-update taoline2--active-backend str)
            (setq taoline2--last-str str))
          (when (equal str taoline2--last-str)
            (taoline2--log "[taoline2 debug] maybe-update: not updating (string unchanged) '%s'" str))))))))

(defun taoline2--add-hooks ()
  (dolist (hk taoline2-update-hooks)
    (add-hook hk #'taoline2--maybe-update))
  (when (eq taoline2-display-backend 'echo)
    (add-hook 'post-command-hook #'taoline2--echo-refresh-if-empty))
  (taoline2--log "[taoline2 debug] hooks enabled, backend=%s hooks=%S"
           taoline2-display-backend taoline2-update-hooks))

(defun taoline2--remove-hooks ()
  (dolist (hk taoline2-update-hooks)
    (remove-hook hk #'taoline2--maybe-update))
  ;; Extra cleanup for echo backend edge-case
  (remove-hook 'post-command-hook #'taoline2--echo-refresh-if-empty)
  (taoline2--log "[taoline2 debug] hooks removed, backend=%s" taoline2-display-backend))

;; ----------------------------------------------------------------------------
;; Mode-line hiding / restoration helpers

(defvar taoline2--saved-mode-line-format nil
  "Backup of the original `mode-line-format` before `taoline2-mode` hides it.")

(defun taoline2--hide-default-mode-line ()
  "Hide the regular mode-line, saving its original value.
The original value is stored in `taoline2--saved-mode-line-format' and
applied to all existing buffers as well as the default for future ones."
  (unless taoline2--saved-mode-line-format
    (setq taoline2--saved-mode-line-format (default-value 'mode-line-format))
    ;; Hide for future buffers
    (setq-default mode-line-format nil)
    ;; Hide for every currently live buffer
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format nil)))))

(defun taoline2--restore-default-mode-line ()
  "Restore the regular mode-line that was saved by `taoline2--hide-default-mode-line'."
  (when taoline2--saved-mode-line-format
    ;; Restore for future buffers
    (setq-default mode-line-format taoline2--saved-mode-line-format)
    ;; Restore for every existing buffer
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format taoline2--saved-mode-line-format)))
    (setq taoline2--saved-mode-line-format nil)))

;; ----------------------------------------------------------------------------
;; Minor mode

;;;###autoload
(define-minor-mode taoline2-mode
  "Global taoline2 minor mode."
  :global t
  (when (called-interactively-p 'any)
    (taoline2--log "[taoline2 debug] taoline2-mode state: %s backend: %S"
                   (if taoline2-mode "ENABLED" "DISABLED")
                   taoline2-display-backend))
  (if taoline2-mode
      (progn
        (unless (eq taoline2--active-backend taoline2-display-backend)
          (taoline2--log "[taoline2 debug] SYNCING backend! was %S now %S"
                         taoline2--active-backend taoline2-display-backend)
          (setq taoline2--active-backend nil))
        (taoline2--set-backend taoline2-display-backend)
        (taoline2--add-hooks)
        (taoline2--maybe-update))
    (taoline2--remove-hooks)
    (when taoline2--active-backend
      (taoline2--backend-teardown taoline2--active-backend)
      (setq taoline2--active-backend nil))
    ;; Restore the original mode-line for the user
    (taoline2--restore-default-mode-line)))

;;;###autoload
(defun taoline2-set-backend (bk)
  "Select BK as the Taoline backend.

If `taoline2-mode' is currently active, switch immediately
(handling teardown/setup).  If the mode is disabled, just remember
the choice so that the same backend is used the next time the mode
is enabled."
  (interactive
   (list
    (intern
     (completing-read
      "Backend: "
      (mapcar (lambda (cell) (symbol-name (car cell))) taoline2--backend-table)
      nil t nil nil (symbol-name taoline2-display-backend)))))
  (setq taoline2-display-backend bk)
  (if taoline2-mode
      (progn
        (unless (eq taoline2--active-backend bk)
          (taoline2--set-backend bk)
          (taoline2--maybe-update)))
    (message "Taoline backend set to %s (will activate when taoline2-mode is enabled)."
             bk)))

;;;###autoload
(defun taoline2-add-segment (side fn)
  "Add FN to SIDE (:left/:right) segments."
  (let* ((cell (assq side taoline2-segments))
         (lst (if cell (cdr cell) ())))
    (setf (alist-get side taoline2-segments) (append lst (list fn)))))

(provide 'taoline2)

;;; taoline2.el ends here
