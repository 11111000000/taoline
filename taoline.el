;;; taoline.el --- Slim modeline proxy without timers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Taoline contributors
;;
;; Author: Peter <11111000000@email.com>
;; URL:   https://github.com/11111000000/taoline
;; Version: 0.2
;; Package-Requires: ((emacs "26.1") (posframe "1.4.1") (all-the-icons "4.0.1"))
;; Keywords: convenience, mode-line

;; This file is NOT part of GNU Emacs.

;; GNU General Public License v3 or later.

;;; Commentary:

;; “taoline” прячет стандартный mode-line и рендерит его упрощённую
;; версию в:
;;
;;   • echo-area (message),
;;   • child-frame (posframe) приклеенный к нижней кромке,
;;   • header-line активного окна.
;;
;; Обновление выполняется без таймеров: после каждой команды и нескольких
;; системных событий, так что минибуфер остаётся свободным, а Emacs не
;; будит интерпретатор каждые N-миллисекунд.

;;; Code:

(require 'subr-x)          ; string-helpers
(require 'posframe nil t)  ; мягкая зависимость — нужен только для child-frame
(require 'all-the-icons nil t)

(defface taoline-time-face
  '((t :background "#002200" :foreground "#00aa00" :weight normal))
  "Face for time segment (green on black, как в прежней taoline)."
  :group 'taoline)



(defcustom taoline-time-align-right t
  "When non-nil, the time segment is right-aligned.
The algorithm calculates the width of the currently selected
frame (in character cells) and pads the taoline string with
spaces so that the clock is stuck to the right edge (see
`taoline-time-right-margin').  If nil, the clock appears
wherever it is placed in `taoline-segments'."
  :type 'boolean
  :group 'taoline)

(defcustom taoline-time-right-margin 2
  "Extra *character* gap between the right edge of the frame
and the clock when `taoline-time-align-right' is non-nil.

16 px ≃ 2 standard characters on typical 8-px fonts, therefore
the default is 2."
  :type 'integer
  :group 'taoline)

(defface taoline-filename-face
  '((t :inherit mode-line-buffer-id))
  "Face for file/buffer name."
  :group 'taoline)

(defface taoline-modified-face
  '((t :inherit default :foreground "orange"))
  "Face for ‘modified’ asterisk."
  :group 'taoline)

(defface taoline-battery-face
  '((t :background "#002200" :foreground "#00aa00"))
  "Face for battery segment (green on black)."
  :group 'taoline)
 
(defgroup taoline nil
  "Slim modeline proxy."
  :group 'convenience)

(defcustom taoline-display-backend
  (if (and (featurep 'posframe) (display-graphic-p)) 'child-frame 'echo-area)
  "Where to render taoline.  One of: echo-area, child-frame, header-line."
  :type '(choice (const echo-area) (const child-frame) (const header-line))
  :group 'taoline)

(defcustom taoline-segments
  '((time . "HH:MM")
    (battery . "BAT")
    (filename . "FILENAME")
    (mode . "MODE"))
  "List of segments to display in the modeline.
Each element is a cons cell (NAME . FORMAT) where NAME is a symbol
and FORMAT is a string. The following names are supported:
`time', `battery', `filename', `mode'."
  :type '(alist :key-type symbol :value-type string)
  :group 'taoline)


(defface taoline-posframe-face
  '((t :family "monospace" :height 1.0))
  "Face for taoline child-frame line (used to uniformalize height & vertical alignment)."
  :group 'taoline)

(defcustom taoline-child-frame-parameters
  '((left-fringe . 0) (right-fringe . 0)
    (internal-border-width . 0)
    (undecorated . t) (no-accept-focus . t)
    (minibuffer . nil) (visibility . nil)
    (skip-taskbar . t) (border-width . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0) (tool-bar-lines . 0)
    (line-spacing . 0.08))  ;; чуть меньше пространства (около 2px)
  "Parameters passed to ‘posframe’ child frame."
  :type 'alist
  :group 'taoline)

(defvar taoline--last ""           "Последняя отображённая строка.")
(defvar taoline--saved-ml nil      "Backup of the original mode-line.")
(defvar taoline--posframe-name " *taoline-posframe*" "Имя posframe буфера.")

(defvar taoline--clock-timer nil   "Таймер тиканья часов.")
(defvar taoline--last-clock ""     "Последняя отрисованная строка времени.")

;;;; helpers --------------------------------------------------------

(defun taoline--git-branch ()
  "Return \":branch\" or \"\" if not in git repo."
  (when (and (executable-find "git")
             (locate-dominating-file default-directory ".git"))
    (let* ((cmd "git symbolic-ref --quiet --short HEAD 2>/dev/null")
           (branch (string-trim (shell-command-to-string cmd))))
      (if (string-empty-p branch) ""
        (propertize (concat ":" branch) 'face 'italic)))))

(defun taoline--battery-string ()
  "Return battery icon and percentage, or empty string if unavailable."
  (when (and (featurep 'all-the-icons)
             (functionp battery-status-function))
    (let* ((data (ignore-errors (funcall battery-status-function)))
           (percent-str (or (cdr (assoc "percentage" data))
                            (cdr (assoc 112 data))       ; ?p
                            (cdr (assoc ?p data)))))
      (when (and percent-str (string-match-p "^[0-9]+$" percent-str))
        (let* ((percent (string-to-number percent-str))
               (icon
                (cond
                 ((>= percent 95) (all-the-icons-faicon "battery-full"         :height 0.95 :v-adjust -0.05))
                 ((>= percent 70) (all-the-icons-faicon "battery-three-quarters" :height 0.95 :v-adjust -0.05))
                 ((>= percent 45) (all-the-icons-faicon "battery-half"         :height 0.95 :v-adjust -0.05))
                 ((>= percent 20) (all-the-icons-faicon "battery-quarter"      :height 0.95 :v-adjust -0.05))
                 (t                (all-the-icons-faicon "battery-empty"       :height 0.95 :v-adjust -0.05))))
               (text (format "%s %s%%" icon percent)))
          text)))))

(defun taoline--segment->plist (seg)
  "Преобразовать SEG в внутренний plist.

SEG может быть:
1. В «простом» формате (NAME . FORMAT), к примеру
     (time . \"HH:MM\")
2. В «подробном» формате (FORMAT ARGS PROPS &optional EXTRA)
   – исторически использовавшийся в taoline.

Возвращаемый plist содержит ключи
 :format, :args, :props, :float, :predicate."
  (cond
   ;; ────────────────────────────────────────────────────────────────
   ;; Новый/простой формат: (NAME . FORMAT)
   ((and (consp seg) (stringp (cdr seg)))
    (let* ((name       (car seg))
           (raw-fmt    (cdr seg))
           ;; Подставляем разумные значения для известных сегментов ― вместо
           ;; статичных «BAT», «FILENAME», «MODE», «HH:MM» получаем живые данные.
           (spec       (pcase name
                         ('battery  (list "%s"
                                          '((taoline--battery-string))
                                          '(:face taoline-battery-face)))
                         ('filename (list "%s"
                                          '((taoline--filename-string))
                                          '(:face taoline-filename-face)))
                         ('mode     (list "%s"
                                          '((format-mode-line mode-name))
                                          '(:face mode-line-emphasis)))
                         ('time     (list "%s"
                                          '((format-time-string "%H:%M"))
                                          '(:face taoline-time-face)))
                         (_         nil)))
           (fmt        (or (car  spec) raw-fmt))
           (args       (or (cadr spec) nil))
           ;; По умолчанию все сегменты располагаются слева.
           (float nil))
      (list :format fmt
            :args   args
            :props  nil
            :float  float
            :predicate nil)))
   ;; ────────────────────────────────────────────────────────────────
   ;; Старый/подробный формат
   (t
    (let* ((fmt   (nth 0 seg))
           (args  (nth 1 seg))
           (props (nth 2 seg))
           (extra (and (> (length seg) 3) (nth 3 seg))))
      (list :format fmt
            :args   args
            :props  props
            :float  (when (and (listp extra) (plist-get extra :float))
                      (plist-get extra :float))
            :predicate (when (functionp extra) extra))))))

(defun taoline--segment-eval (segment)
  "Сформировать текст сегмента SEGMENT (plist).
Вернёт plist (:string, :float), либо nil если сегмент не должен отображаться."
  (let* ((fmt   (plist-get segment :format))
         (args  (plist-get segment :args))
         (props (plist-get segment :props))
         (float (or (plist-get segment :float) 'left))
         (pred  (plist-get segment :predicate))
         (show? (or (null pred) (funcall pred)))
         (txt (and show? (apply #'format fmt (mapcar #'eval args)))))
    (when (and txt (not (string-empty-p txt)))
      (when props (add-text-properties 0 (length txt) props txt))
      (list :string txt :float float))))

(defun taoline--compose ()
  "Собрать итоговую строку taoline: все сегменты аккумулируются, правая часть подгоняется по ширине.
Гарантирует однострочность (без перевода строки и табов)."
  (let* ((segments (mapcar #'taoline--segment->plist taoline-segments))
         (plists   (delq nil (mapcar #'taoline--segment-eval segments)))
         (lefts    (seq-filter (lambda (x) (eq (plist-get x :float) 'left)) plists))
         (rights   (seq-filter (lambda (x) (eq (plist-get x :float) 'right)) plists))
         (left-str  (mapconcat (lambda (x) (plist-get x :string)) lefts ""))
         (right-str (mapconcat (lambda (x) (plist-get x :string)) rights ""))
         (fw (frame-width (selected-frame)))
         (padding (max 1 (- fw
                            (string-width left-str)
                            (string-width right-str)
                            taoline-time-right-margin)))
         (line (if (string-empty-p right-str)
                   left-str
                 (concat left-str (make-string padding ?\s) right-str))))
    (replace-regexp-in-string "[\n\r\t]+" " " line)))



(defun taoline--ready-p ()
  "Whether echo-area is currently free for our message."
  (and (not (active-minibuffer-window))
       (null (current-message))))

;;;; backend: echo-area ---------------------------------------------

(defun taoline-echo-setup () nil)

(defun taoline-echo-display (str)
  (let* ((oneline (taoline--pad-to-frame-width str))
         (message-log-max nil))
    (message "%s" oneline)))

(defun taoline-echo-teardown ()
  (message nil))

;;;; backend: header-line -------------------------------------------

(defvar taoline--header-cache "")

(defun taoline-header-setup ()
  (setq taoline--saved-ml mode-line-format)
  (set (make-local-variable 'header-line-format)
       '(:eval taoline--header-cache))
  (setq-default mode-line-format nil))

(defun taoline--truncate-to-frame-width (str)
  "Truncate STR so its display width fits exactly in current frame,
never exceeding one screen line (avoiding soft-wraps and multi-line header-line)."
  (let* ((fw (frame-width (selected-frame))))
    (truncate-string-to-width str fw 0 ?\s)))

(defun taoline-header-display (str)
  (let ((oneline (taoline--pad-to-frame-width str)))
    (unless (equal oneline taoline--header-cache)
      (setq taoline--header-cache oneline)
      (force-mode-line-update t))))

(defun taoline-header-teardown ()
  (setq-default mode-line-format taoline--saved-ml)
  (setq header-line-format nil)
  (force-mode-line-update t))

;;;; backend: child-frame (posframe) --------------------------------

(defun taoline--pad-to-frame-width (str)
  "Collapse STR to a single visual line and pad it on the right so
its display width is *at least* the current frame width.

The function NEVER removes faces or other text properties and it
NEVER TRUNCATES the string; if STR is already wider than the
frame it is returned unchanged.  Newlines, carriage returns and
tabs are replaced with a single space."
  (let* ((fw (frame-width (selected-frame)))
         (clean (replace-regexp-in-string "[\n\r\t]+" " " (or str "")))
         (w (string-width clean)))
    (when (< w fw)
      (setq clean (concat clean (make-string (- fw w) ?\s))))
    clean))

(defun taoline--posframe-show (str)
  "Show STR in a stable child-frame stuck to the bottom edge of PARENT.
Faces, icons, single-line guarantee, no right-edge gap or clipping."
  (let* ((parent  (selected-frame))
         (fw      (frame-width parent))
         (oneline (taoline--pad-to-frame-width str))
         ;; Apply a face to everything for unified vertical alignment in posframe
         (_ (add-face-text-property 0 (length oneline) 'taoline-posframe-face nil oneline))
         (w       (string-width oneline))           ; real width
         (w*      (max fw w)))                      ; never smaller than frame
    (with-current-buffer (get-buffer-create taoline--posframe-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert oneline)))
    (posframe-show taoline--posframe-name
                   :string nil                      ;; use buffer instead!
                   
                   :poshandler #'posframe-poshandler-frame-bottom-left-corner
                   :min-width w*
                   :max-width w*
                   :width     w*
                   :min-height 1
                   :max-height 1
                   :internal-border-width 0
                   ;; NO :no-properties t!
                   :background-color (face-attribute 'mode-line :background nil t)
                   :override-parameters (copy-sequence taoline-child-frame-parameters))))

(defun taoline-posframe-setup ()
  (unless (require 'posframe nil t)
    (user-error "posframe backend requested but package ‘posframe’ not found"))
  ;; hide original mode-line
  (setq taoline--saved-ml mode-line-format)
  (setq-default mode-line-format nil)
  ;; initial invisible frame
  (taoline--posframe-show "")
  (posframe-hide taoline--posframe-name))

(defun taoline-posframe-display (str)
  (if (active-minibuffer-window)             ; не мешать вводу
      (posframe-hide taoline--posframe-name)
    (taoline--posframe-show str)))

(defun taoline-posframe-teardown ()
  (posframe-delete taoline--posframe-name)
  (setq-default mode-line-format taoline--saved-ml)
  (force-mode-line-update t))

;;;; backend dispatch table -----------------------------------------

(cl-defun taoline--dispatch (action)
  (pcase taoline-display-backend
    ('echo-area   (pcase action
                    ('setup    (taoline-echo-setup))
                    ('display  #'taoline-echo-display)
                    ('teardown (taoline-echo-teardown))))
    ('header-line (pcase action
                    ('setup    (taoline-header-setup))
                    ('display  #'taoline-header-display)
                    ('teardown (taoline-header-teardown))))
    ('child-frame (pcase action
                    ('setup    (taoline-posframe-setup))
                    ('display  #'taoline-posframe-display)
                    ('teardown (taoline-posframe-teardown))))
    (_ (error "Unknown backend %s" taoline-display-backend))))

;;;; core update ----------------------------------------------------

(defun taoline--update ()
  "Main update entry point hooked into post-command or timer.
Всегда обновляет строку, если она изменилась по содержимому (не только по действию пользователя)."
  (let* ((raw (taoline--compose))
         (new (taoline--apply-time-alignment raw)))
    (unless (equal new taoline--last)
      (setq taoline--last new)
      (funcall (taoline--dispatch 'display) new))))




(defun taoline--clock-tick ()
  "Фоновое обновление для тикающих часов. Проверяет, изменилась ли секунда - если да, обновляет строку."
  (let* ((raw (taoline--compose))
         (new (taoline--apply-time-alignment raw)))
    (unless (equal new taoline--last-clock)
      (setq taoline--last-clock new)
      (taoline--update))))

;; -------------------------------------------------------------------
;; helpers ------------------------------------------------------------
;; (removed duplicate taoline--battery-string to allow the icon-based version above to be used)

(defun taoline--filename-string ()
  "Return a short name for the current buffer.
Prefer `buffer-file-name' stripped of its directory, fall back to `buffer-name'."
  (if buffer-file-name
      (file-name-nondirectory buffer-file-name)
    (buffer-name)))
(defun taoline--cancel-all-clock-timers ()
  "Cancel every active timer that calls `taoline--clock-tick'.
Also reset `taoline--clock-timer' to nil."
  (dolist (tm (copy-sequence timer-list)) ; `timer-list' is Emacs-internal
    (when (and (timerp tm)
               (eq (timer--function tm) #'taoline--clock-tick))
      (cancel-timer tm)))
  (when (timerp taoline--clock-timer)
    (cancel-timer taoline--clock-timer))
  (setq taoline--clock-timer nil))

(defconst taoline--hooks
  '(post-command-hook
    window-configuration-change-hook
    focus-in-hook
    frame-size-change-functions))

(defun taoline--add-hooks ()
  (mapc (lambda (hook)
          (add-hook hook #'taoline--update))
        taoline--hooks))

(defun taoline--remove-hooks ()
  (mapc (lambda (hook)
          (remove-hook hook #'taoline--update))
        taoline--hooks))

(defun taoline--restore-mode-line ()
  "Восстановить mode-line во всех буферах функционально."
  (mapc (lambda (buf)
          (with-current-buffer buf
            (cond
             ((and (local-variable-p 'mode-line-format) taoline--saved-ml)
              (setq-local mode-line-format taoline--saved-ml))
             ((and (local-variable-p 'mode-line-format)
                   (or (null mode-line-format)
                       (equal mode-line-format "")))
              (kill-local-variable 'mode-line-format)))))
        (buffer-list))
  (setq-default mode-line-format
                (or taoline--saved-ml
                    (get 'mode-line-format 'standard-value)))
  (setq taoline--saved-ml nil))

(defun taoline--wipe-cache ()
  "Сброс кэшированных значений строки."
  (setq taoline--last "")
  (setq taoline--last-clock ""))

;;;###autoload
(define-minor-mode taoline-mode
  "Global minor-mode providing slim modeline proxy."
  :global t
  :lighter ""
  (if taoline-mode
      (progn
        (unless taoline--saved-ml
          (setq taoline--saved-ml mode-line-format))
        (mapc (lambda (win)
                (with-selected-window win
                  (setq-local mode-line-format nil)))
              (window-list))
        (taoline--dispatch 'setup)
        (taoline--add-hooks)
        ;; Убедимся, что от прежних версий не осталось "висячих" таймеров
        (taoline--cancel-all-clock-timers)
        (setq taoline--clock-timer
              (run-with-timer 0 1 #'taoline--clock-tick)))
    (progn
      (taoline--remove-hooks)
      (taoline--dispatch 'teardown)
      (taoline--restore-mode-line)
      (taoline--wipe-cache)
      (taoline--cancel-all-clock-timers)
      (message nil)
      (force-mode-line-update t)
      (redraw-display))))


;;;###autoload
(defun taoline-set-backend (backend)
  "Interactively switch BACKEND (echo-area, child-frame, header-line)."
  (interactive
   (list (intern (completing-read "Backend: "
                                  '(echo-area child-frame header-line)
                                  nil t))))
  (unless (eq backend taoline-display-backend)
    (setq taoline-display-backend backend)
    (when taoline-mode
      ;; reinit backend on the fly
      (taoline--dispatch 'teardown)
      (setq taoline--last "")
      (taoline--dispatch 'setup)
      (taoline--update))))

(defun taoline--apply-time-alignment (str)
  "Return STR with the clock (HH:MM:SS) aligned flush right.

If `taoline-time-align-right' is nil or if STR does not contain the
currently formatted time, return STR unchanged.

Otherwise the current time is removed from its original place and
padded so that it ends at the right edge of the current frame,
leaving `taoline-time-right-margin' spaces between the clock and
the frame edge."
  (if (not taoline-time-align-right)
      str
    (let* ((clock-str (format-time-string "%H:%M:%S"))
           (pos (string-match (regexp-quote clock-str) str)))
      (if (not pos)
          str
        (let* ((left        (concat (substring str 0 pos)
                                    (substring str (+ pos (length clock-str)))))
               (frame-width (frame-width (selected-frame)))
               (padding     (max 1 (- frame-width
                                      (string-width left)
                                      (string-width clock-str)
                                      taoline-time-right-margin))))
          (concat left (make-string padding ?\s) clock-str))))))
(provide 'taoline)
;;; taoline.el ends here
