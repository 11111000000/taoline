;;; taoline-legacy.el --- Replace modeline with a slimmer proxy -*- lexical-binding: t; -*-

;; taoline-legacy © 2018-∞ Peter 11111000000
;; feebleline © 2018 Benjamin Lindqvist

;; Author of taoline-legacy: Peter <11111000000@email.com>
;; Author of feebleline: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; URL: https://github.com/11111000000/taoline-legacy
;; Package-Version: 0.1
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For editing!

;; Taoline-Legacy removes the modeline and replaces it with a slimmer proxy
;; version, which displays some basic information in the echo area
;; instead.  This information is only displayed if the echo area is not used
;; for anything else (but if you switch frame/window, it will replace whatever
;; message is currently displayed).

;; To customize taoline-legacy's format, modify `taoline-legacy-mode-line-text'.

;; NOTE:
;; taoline-legacy.el will look considerably better with the following
;; settings:

;;   (window-divider-mode t)
;;   (setq window-divider-default-bottom-width 1)
;;   (setq window-divider-default-places (quote bottom-only))

;; But this mode does not work for all EMACS versions and may not work with
;; terminal EMACS (but I haven't checked).  If you're on GUI EMACS and your
;; version supports it, just place the following in your init file:

;;   (taoline-legacy-default-settings)

;; Otherwise, do (taoline-legacy-mode t) instead, but be warned that I'm not sure
;; if it will look good.

;;; Code:

(require 'advice)

(defvar taoline-legacy-use-legacy-settings nil)
(when (< emacs-major-version 25)
  (setq taoline-legacy-use-legacy-settings t))

(setq taoline-legacy-use-legacy-settings nil)

(defface taoline-legacy-time-face '((t :background "#002200" :foreground "#00aa00" :height 0.96 :bold nil :family "Digital Display"))
  "Taoline-Legacy time face (with improved vertical centering for minibuffer proxy)."
  :group 'taoline-legacy)
(defface taoline-legacy-input-face '((t :inherit 'fixed-pitch :height 0.92))
  "Taoline-Legacy input face."
  :group 'taoline-legacy)
(defface taoline-legacy-bufname-face '((t :inherit 'fixed-pitch :height 0.96))
  "Taoline-Legacy filename face."
  :group 'taoline-legacy)
(defface taoline-legacy-linum-face '((t :inherit 'fixed-pitch :height 0.90))
  "Taoline-Legacy linum face."
  :group 'taoline-legacy)
(defface taoline-legacy-asterisk-face '((t :foreground "yellow"  :inherit 'fixed-pitch :height 0.92))
  "Taoline-Legacy file modified asterisk face."
  :group 'taoline-legacy)
(defface taoline-legacy-previous-buffer-face '((t :foreground "#7e7e7e" :height 0.96))
  "Taoline-Legacy filename face."
  :group 'taoline-legacy)
(defface taoline-legacy-dir-face '((t :inherit 'font-lock-variable-name-face :height 0.96))
  "Taoline-Legacy filename face."
  :group 'taoline-legacy)
(defface taoline-legacy-git-branch-face '((t :inherit 'font-lock-comment-face :bold nil :italic t :height 0.96))
  "Taoline-Legacy filename face."
  :group 'taoline-legacy)

;; Customizations
(defcustom taoline-legacy-show-previous-buffer nil
  "Set this if you want to show the previous 'buffer-name' in the modeline proxy."
  :group 'taoline-legacy)
(defcustom taoline-legacy-show-directory t
  "Set this if you want to show the direcory path as well as the file-name in the modeline proxy."
  :group 'taoline-legacy)

(defcustom taoline-legacy-disable-eldoc t
  "If non-nil, temporarily silence Eldoc while `taoline-legacy-mode' is active.
Many users keep Eldoc enabled globally; its continuous messages
occupy the echo area and prevent taoline-legacy from displaying its
proxy.  When this option is non-nil, `taoline-legacy-mode' saves
`eldoc-message-function', sets it to `ignore' on activation, and
restores the original value on deactivation."
  :type 'boolean
  :group 'taoline-legacy)

(defvar taoline-legacy--eldoc-message-function-prev nil
  "Backup of `eldoc-message-function' taken when `taoline-legacy-mode' is activated.")

(defun taoline-legacy-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defvar taoline-legacy-mode-line-text nil
  "Each element is a list with the following format:

    (FORMAT-STRING FORMAT-ARGS PROPS)

FORMAT-STRING will be used as the first argument to `format', and
FORMAT-ARGS (a list) will be expanded as the rest of `format'
arguments.  If PROPS is given, it should be a list which will be
sent to `add-text-properties'.")

(defun taoline-legacy--git-branch-string ()
  "Return current git branch as a string, or the empty string if pwd is not in a git repo (or the git command is not found)."
  (interactive)
  (require 'esh-ext)
  (when (and (eshell-search-path "git")
             (locate-dominating-file default-directory ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " default-directory " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
          (substring git-output 0 -1)
          ;; (concat " :" (substring git-output 0 -1))
        ""))))

;; Always keep the user’s home directory as a non-nil string so that
;; `replace-regexp-in-string` never gets a nil PATTERN argument.
(defvar taoline-legacy--home-dir (expand-file-name "~"))

(require 'all-the-icons)

(setq taoline-legacy-default-icon (all-the-icons-octicon "browser" :height 1  :v-adjust 0))

(setq taoline-legacy-mode-line-text
 '(
   ("%s " ((cond ((equal current-input-method "russian-computer") "") (t "EN")))
    ( '(face taoline-legacy-input-face)))
   (" %s " ((format-time-string "%H:%M:%S"))
    ( '(face taoline-legacy-time-face)))
   ;; Батарея: иконка + процент
   (" %s " ((taoline-legacy--battery-string))
    nil)
   (" %s" ((let ((icon (all-the-icons-icon-for-mode major-mode)))
             (cond
              ((or (not icon) (symbolp icon)) (or taoline-legacy-default-icon ""))
              (t icon))))
    ((let ((icon (all-the-icons-icon-for-mode major-mode :height 0.8)))
       (and icon (not (symbolp icon)) (text-properties-at 0 icon)))))
   (" %s" ((if (and taoline-legacy-show-directory (buffer-file-name))
              (replace-regexp-in-string
               taoline-legacy--home-dir "~"
               (file-name-directory (buffer-file-name)))
            ""))
    ( '(face taoline-legacy-dir-face)))
   ("%s" ((if (buffer-file-name)
              (substring-no-properties (file-name-nondirectory (buffer-file-name)))
            (substring-no-properties (buffer-name))))
    nil) ;; Очистка text properties с имени буфера!
   ("%s" ((let ((branch (taoline-legacy--git-branch-string))) (if (> (length branch) 0) (concat ":" branch) "")))
    ( '(face taoline-legacy-git-branch-face)))
   ("%s" ((if (buffer-modified-p) " * " 
            "" ))
    ( '(face taoline-legacy-asterisk-face)))
   (" %s " ((format "%s:%s" (format-mode-line "%l") (current-column)))
    ( '(face taoline-legacy-linum-face)))
   ;; ("%s" ((concat " | " (taoline-legacy-previous-buffer-name)))
   ;; (face taoline-legacy-previous-buffer-face))
   ))

(defun taoline-legacy-default-settings-on ()
  "Some default settings that works well with taoline-legacy."
  (setq window-divider-default-bottom-width 1
        window-divider-default-places (quote bottom-only))
  (window-divider-mode t)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil))

(defun taoline-legacy-legacy-settings-on ()
  "Some default settings for EMACS < 25."
  (set-face-attribute 'mode-line nil :height 0.1))

(defvar taoline-legacy/timer)
(defvar taoline-legacy/mode-line-format-previous)

(defvar taoline-legacy--last-placeholder "")

;;;###autoload
(define-minor-mode taoline-legacy-mode
  "Replace modeline with a slimmer proxy."
  :require 'taoline-legacy
  :global t
  (if taoline-legacy-mode
      ;; Activation:
      (progn
        (setq taoline-legacy--home-dir (expand-file-name "~"))
        (setq taoline-legacy/mode-line-format-previous mode-line-format)
        ;; Не допускаем накопления нескольких таймеров при повторном
        ;; включении режима: старый таймер сначала отменяем.
        (when (timerp taoline-legacy/timer)
          (when (timerp taoline-legacy/timer)
      (cancel-timer taoline-legacy/timer)

    ;; Restore Eldoc if we silenced it during activation.
    (when taoline-legacy-disable-eldoc
      (setq eldoc-message-function taoline-legacy--eldoc-message-function-prev))
      (setq taoline-legacy/timer nil)))
        (setq taoline-legacy/timer
              (run-with-timer 0 0.2 #'taoline-legacy-mode-line-proxy-fn)) ;; Reduced timer frequency for better performance.

        ;; Optionally silence Eldoc so that it does not monopolise the echo area.
        (when taoline-legacy-disable-eldoc
          (setq taoline-legacy--eldoc-message-function-prev eldoc-message-function)
          (setq eldoc-message-function #'ignore))
        (if taoline-legacy-use-legacy-settings (taoline-legacy-legacy-settings-on)
          (taoline-legacy-default-settings-on))
        ;; (ad-activate 'handle-switch-frame)
        (add-hook 'focus-in-hook 'taoline-legacy-mode-line-proxy-fn))

    ;; Deactivation:
    (set-face-attribute 'mode-line nil :height 1.0)
    (setq-default mode-line-format taoline-legacy/mode-line-format-previous)
    (setq mode-line-format taoline-legacy/mode-line-format-previous)
    (cancel-timer taoline-legacy/timer)
    ;; (ad-deactivate 'handle-switch-frame)
    (remove-hook 'focus-in-hook 'taoline-legacy-mode-line-proxy-fn)
    (force-mode-line-update)
    (redraw-display)
    (with-current-buffer " *Minibuf-1*"
      (erase-buffer))))

(defun taoline-legacy--mode-line-part (part)
  "Return a PART (an element) of `taoline-legacy-mode-line-text` as a propertized string."
  (let* ((args (mapcar #'eval (cadr part)))
         ;; Ensure first arg to format is a string even if nil
         (fmt (or (car part) ""))
         (text (apply #'format (append (list fmt) args)))
         (props  (eval (car (elt part 2)))))
    (when props
      (add-text-properties 0 (length text) props text))
    text))

(defvar taoline-legacy-placeholder)
(defvar taoline-legacy--last-placeholder "")

(defun taoline-legacy-write-buffer-name-maybe ()
  "Replace echo-area message with mode-line proxy (unless minibuffer is active, minibuf contains overlays, or text is unchanged).
If eldoc-box or another overlay is currently displaying something, taoline-legacy will not overwrite the echo area."
  (when (and (not (active-minibuffer-window))
             (get-buffer " *Minibuf-0*"))
    (with-current-buffer " *Minibuf-0*"
      (let* ((currentbuf (window-buffer (selected-window))) ;; выбираем активный буфер
             (placeholder
              (with-current-buffer currentbuf
                (mapconcat
                 (lambda (x) (substring-no-properties x)) ; Стираем все text properties с каждой части!
                 (mapcar #'taoline-legacy--mode-line-part taoline-legacy-mode-line-text)
                 ""))))
        (let ((has-ol (overlays-in (point-min) (point-max))))
          (when (and (not has-ol))
            (unless (equal placeholder taoline-legacy--last-placeholder)
              (setq taoline-legacy--last-placeholder placeholder)
              (let ((inhibit-read-only t))   ; позволяем писать в read-only echo-area
                (erase-buffer)
                (insert placeholder))))))))

(defun taoline-legacy-mode-line-proxy-fn ()
  "Put a mode-line proxy in the echo area *if* echo area is empty."
  (unless (or (current-message)
              (active-minibuffer-window)) ; Do not update if minibuffer is used.
    (taoline-legacy-write-buffer-name-maybe)))

;; (defadvice handle-switch-frame (after switch-frame-message-name)
;;   "Get the modeline proxy to work with i3 switch focus."
;;   (taoline-legacy-write-buffer-name-maybe)
;;   ad-do-it
;;   (taoline-legacy-write-buffer-name-maybe))

;; === Батарея: функция получения иконки и процента ===
(defun taoline-legacy--battery-string ()
  "Return battery icon and percentage based on battery-status-function format."
  (if (functionp battery-status-function)
      (let* ((data (ignore-errors (funcall battery-status-function)))
             ;; Попробуем оба варианта ключей: строкой и ASCII-кодом ?
             (percent-str (or (cdr (assoc "percentage" data))
                              (cdr (assoc 112 data))     ; 112 = ?p
                              (cdr (assoc ?p data)))))
        (if (and percent-str (string-match-p "^[0-9]+$" percent-str))
            (let* ((percent (string-to-number percent-str))
                   (icon
                    (cond
                     ((>= percent 95) (all-the-icons-faicon "battery-full" :height 0.95 :v-adjust -0.05))
                     ((>= percent 70) (all-the-icons-faicon "battery-three-quarters" :height 0.95 :v-adjust -0.05))
                     ((>= percent 45) (all-the-icons-faicon "battery-half" :height 0.95 :v-adjust -0.05))
                     ((>= percent 20) (all-the-icons-faicon "battery-quarter" :height 0.95 :v-adjust -0.05))
                     (t (all-the-icons-faicon "battery-empty" :height 0.95 :v-adjust -0.05)))))
              (format "%s %s%%" icon percent))
          ""))
    ""))

(provide 'taoline-legacy)
;;; taoline-legacy.el ends here
