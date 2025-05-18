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

(defgroup taoline nil
  "Functional minimalist mode-line replacement."
  :group 'convenience
  :prefix "taoline-")

;; ----------------------------------------------------------------------------
;; Customizable variables

(defcustom taoline-display-backend 'modeline
  "Backend to show the modeline.
Supported values: 'modeline, 'echo, 'header, 'posframe"
  :type '(choice (const :tag "In-window modeline" modeline)
                 (const :tag "Echo area" echo)
                 (const :tag "Header line" header)
                 (const :tag "Child frame (posframe)" posframe))
  :group 'taoline)

(defcustom taoline-segments
  '((:left taoline-segment-git-branch taoline-segment-buffer-name)
    (:right taoline-segment-major-mode taoline-segment-time))
  "List of segments to display in the modeline.
:LEFT and :RIGHT are lists of segment *function symbols*."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'taoline)

(defcustom taoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "A list of hooks triggering taoline update."
  :type '(repeat symbol)
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
  "Call segment FN with BUFFER."
  (condition-case err
      (funcall fn buffer)
    (error (propertize (format "[SEGMENT ERROR: %s]" err) 'face 'error))))

(defun taoline--collect-segments (side buffer)
  "For SIDE (:left/:right), collect and evaluate segment functions for BUFFER."
  (let ((fns (cdr (or (assq side taoline-segments) ())))
        (results '()))
    (dolist (fn fns)
      (push (taoline--apply-segment fn buffer) results))
    (nreverse results)))

;; ----------------------------------------------------------------------------
;; Core: Compose functionally the modeline string

(defun taoline-compose-modeline (&optional buffer frame)
  "Pure function: compose modeline string for BUFFER and FRAME."
  (let* ((buffer (or buffer (current-buffer)))
         (frame (or frame (selected-frame)))
         (width (frame-width frame))
         (left (string-join (taoline--collect-segments :left buffer) " "))
         (right (string-join (taoline--collect-segments :right buffer) " "))
         (mid-space (max 1 (- width (string-width left) (string-width right)))))
    (concat
     (propertize left 'face 'taoline-base-face)
     (make-string mid-space ?\s)
     (propertize right 'face 'taoline-base-face))))

;; ----------------------------------------------------------------------------
;; Built-in segments

(taoline-define-segment taoline-segment-time (buffer)
  "Display current time."
  (propertize (format-time-string "%H:%M")
              'face 'taoline-time-face))

(taoline-define-segment taoline-segment-buffer-name (buffer)
  "Display current buffer name, with * if modified."
  (let* ((name (buffer-name buffer))
         (mod (if (buffer-modified-p buffer) "*" "")))
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

;; ----------------------------------------------------------------------------
;; Backend registry

(defvar taoline--backend-table
  '((modeline . ((setup . taoline--modeline-setup)
                 (update . taoline--modeline-update)
                 (teardown . taoline--modeline-teardown)))
    (echo    . ((setup . taoline--echo-setup)
                (update . taoline--echo-update)
                (teardown . taoline--echo-teardown)))
    (header  . ((setup . taoline--header-setup)
                (update . taoline--header-update)
                (teardown . taoline--header-teardown)))
    (posframe . ((setup . taoline--posframe-setup)
                 (update . taoline--posframe-update)
                 (teardown . taoline--posframe-teardown))))
  "Backend function registry.")

;; -- Modeline backend (in-window modeline)

(defvar-local taoline--modeline-old nil)
(defvar-local taoline--modeline-expression nil)

(defun taoline--modeline-setup ()
  "Setup for `modeline' backend.
Прячет обычный `mode-line-format' (сохранив в
`taoline--saved-mode-line-format') и устанавливает собственную
Taoline-строку."
  (taoline--hide-default-mode-line)
  "Setup taoline as the current window's modeline."
  (unless taoline--modeline-old
    (setq taoline--modeline-old (default-value 'mode-line-format)))
  (setq taoline--modeline-expression
        '(:eval (taoline-compose-modeline)))
  (setq mode-line-format taoline--modeline-expression))

(defun taoline--modeline-update (_str)
  "Force update of the modeline."
  (force-mode-line-update t))

(defun taoline--modeline-teardown ()
  "Teardown for `modeline' backend.
Убирает Taoline из `mode-line-format' и восстанавливает сохранённый
пользовательский mode-line."
  (taoline--restore-default-mode-line)
  "Restore previous mode-line-format."
  (when taoline--modeline-old
    (setq mode-line-format taoline--modeline-old)
    (setq taoline--modeline-old nil)
    (setq taoline--modeline-expression nil)))

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

;; -- Header line backend

(defvar-local taoline--header-old nil)
(defvar-local taoline--header-expression nil)

(defun taoline--header-setup ()
  (unless taoline--header-old
    (setq taoline--header-old (default-value 'header-line-format)))
  (setq taoline--header-expression
        '(:eval (taoline-compose-modeline)))
  (setq header-line-format taoline--header-expression))

(defun taoline--header-update (_str)
  (force-mode-line-update t))

(defun taoline--header-teardown ()
  (when taoline--header-old
    (setq header-line-format taoline--header-old)
    (setq taoline--header-old nil)
    (setq taoline--header-expression nil)))

;; -- Posframe backend

(defvar taoline--posframe-buffer " *taoline-posframe*")
(defvar taoline--posframe-name "taoline-posframe")

(defun taoline--posframe-setup ()
  (require 'posframe)
  (posframe-delete taoline--posframe-buffer))

(defun taoline--posframe-update (str)
  (require 'posframe)
  (let* ((width (frame-width))
         (height 1)
         (params '((left-fringe . 8) (right-fringe . 8))))
    (posframe-show
     taoline--posframe-buffer
     :string str
     :name taoline--posframe-name
     :poshandler 'posframe-poshandler-frame-bottom-center
     :min-width width
     :min-height height
     :border-width 0
     :override-parameters params)))

(defun taoline--posframe-teardown ()
  (ignore-errors
    (require 'posframe)
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
  "Switch to BACKEND, handling setup/teardown **и** правильное
скрытие / восстановление обычного mode-line через backend-функции."
  (taoline--log "[taoline debug] set-backend called: %S" backend)
  (setq taoline-display-backend backend)
  (unless (eq backend taoline--active-backend)
    (when taoline--active-backend
      (taoline--log "[taoline debug] backend-teardown: %S" taoline--active-backend)
      (taoline--backend-teardown taoline--active-backend))
    (taoline--backend-setup backend)
    (setq taoline--active-backend backend)
    (setq taoline--last-str ""))) ;; force refresh

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

(defun taoline--add-hooks ()
  (dolist (hk taoline-update-hooks)
    (add-hook hk #'taoline--maybe-update))
  (when (eq taoline-display-backend 'echo)
    (add-hook 'post-command-hook #'taoline--echo-refresh-if-empty))
  (taoline--log "[taoline debug] hooks enabled, backend=%s hooks=%S"
           taoline-display-backend taoline-update-hooks))

(defun taoline--remove-hooks ()
  (dolist (hk taoline-update-hooks)
    (remove-hook hk #'taoline--maybe-update))
  ;; Extra cleanup for echo backend edge-case
  (remove-hook 'post-command-hook #'taoline--echo-refresh-if-empty)
  (taoline--log "[taoline debug] hooks removed, backend=%s" taoline-display-backend))

;; ----------------------------------------------------------------------------
;; Mode-line hiding / restoration helpers

(defvar taoline--saved-mode-line-format nil
  "Backup of the original `mode-line-format` before `taoline-mode` hides it.")

(defun taoline--hide-default-mode-line ()
  "Hide the regular mode-line, saving its original value.
The original value is stored in `taoline--saved-mode-line-format' and
applied to all existing buffers as well as the default for future ones."
  (unless taoline--saved-mode-line-format
    (setq taoline--saved-mode-line-format (default-value 'mode-line-format))
    ;; Hide for future buffers
    (setq-default mode-line-format nil)
    ;; Hide for every currently live buffer
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format nil)))))

(defun taoline--restore-default-mode-line ()
  "Restore the regular mode-line that was saved by `taoline--hide-default-mode-line'."
  (when taoline--saved-mode-line-format
    ;; Restore for future buffers
    (setq-default mode-line-format taoline--saved-mode-line-format)
    ;; Restore for every existing buffer
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (setq mode-line-format taoline--saved-mode-line-format)))
    (setq taoline--saved-mode-line-format nil)))

;; ----------------------------------------------------------------------------
;; Minor mode

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
    (when taoline--active-backend
      (taoline--backend-teardown taoline--active-backend)
      (setq taoline--active-backend nil))
    ;; Restore the original mode-line for the user
    (taoline--restore-default-mode-line)))

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

(provide 'taoline)

;;; taoline.el ends here
