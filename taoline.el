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

(defcustom taoline-right-padding 6
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
  '((t :inherit (font-lock-comment-face taoline-base-face) :height 0.8))
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
;; Core: compose function (pure)

(defun taoline-compose-modeline (&optional buffer window width)
  "Compose a Taoline string that is guaranteed to fit within WIDTH.
This function is pure: it merely returns the string and causes no side effects."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         ;; Available width (display columns) of the minibuffer / frame
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         ;; ------------------------------------------------------------------
         ;; Segments
         (left   (mapconcat #'identity (taoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (taoline--collect-segments :right buffer) " "))
         (center (mapconcat #'identity (taoline--collect-segments :center buffer) " "))
         ;; Segment widths
         (left-w   (string-width left))
         (right-w  (string-width right))
         ;; Insert separator spaces ONLY when needed
         (space-left  (if (and (not (string-empty-p left))
                               (or (not (string-empty-p center))
                                   (not (string-empty-p right))))
                          " " ""))
         (space-right (if (and (not (string-empty-p right))
                               (not (string-empty-p center)))
                          " " ""))
         ;; How many columns remain for the central part (center + padding)
         (available (- width
                       left-w
                       right-w
                       (string-width space-left)
                       (string-width space-right)
                       taoline-right-padding))
         ;; Center string is truncated to `available`
         (center-str (truncate-string-to-width center (max 0 available) 0 ?\s))
         (center-w  (string-width center-str))
         ;; The remaining space is divided evenly into left/right padding
         (pad-total   (max 0 (- available center-w)))
         (pad-left-w  (/ pad-total 2))
         (pad-right-w (- pad-total pad-left-w))
         (pad-left  (make-string pad-left-w ?\s))
         (pad-right (make-string pad-right-w ?\s)))

    (let ((final (concat
                  left
                  space-left
                  pad-left
                  center-str
                  pad-right
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
;; Segments
;; ---------------------------------------------------------------------------

(defun taoline--get-buffer-icon (&optional buffer)
  "Return a suitable icon string for BUFFER using all-the-icons, universal and concise."
  (let* ((default-icon (propertize "●" 'face 'taoline-base-face))
         (buf (or buffer (current-buffer))))
    (when (featurep 'all-the-icons)
      (ignore-errors
        (with-current-buffer buf
          (let* ((file (buffer-file-name buf)))
            (or
             ;; If there is a file — by file type
             (when file
               (let ((ic (all-the-icons-icon-for-file
                          (file-name-nondirectory file)
                          :height 1.0 :v-adjust 0 :face 'taoline-base-face)))
                 (when (and (stringp ic) (> (string-width ic) 0)) ic)))
             ;; By major-mode (universal, works for most cases)
             (let ((ic (all-the-icons-icon-for-mode
                        major-mode :height 1.0 :v-adjust 0 :face 'taoline-base-face)))
               (when (and (stringp ic) (> (string-width ic) 0)) ic))
             ;; Fallback
             default-icon)))))))

(defconst taoline--icon-width 3
  "Fixed icon width (in display columns) for taoline buffer icons.
You may need to adapt this for your font & setup.")

(taoline-define-simple-segment taoline-segment-icon-and-buffer
  "Buffer name with universal icon selection and modified flag."
  (let* ((icon (or (taoline--get-buffer-icon) (propertize "●" 'face 'taoline-base-face)))
         (icon-str (let* ((w (string-width icon))
                          (pad (max 0 (- taoline--icon-width w))))
                     (concat icon (make-string pad ?\s))))
         (name (propertize (buffer-name) 'face 'taoline-buffer-face))
         (mod  (when (buffer-modified-p)
                 (propertize " *" 'face 'taoline-modified-face))))
    (concat
     icon-str
     name
     (or mod ""))))

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
  "Current echo message excluding taoline itself, padded to fixed width to prevent jumping."
  (let* ((msg  (current-message))
         (max-width 32) ;; Adjust if needed
         ;; Show the message only if it's not the last taoline string
         (show (unless (or (null msg)
                           (string-equal msg taoline--last-str))
                 msg))
         (shown-str (or show ""))
         (padded-str
          (truncate-string-to-width
           (concat shown-str
                   (make-string max-width ?\s))
           max-width)))
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
