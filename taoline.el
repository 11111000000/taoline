;;; taoline.el --- Replace modeline with a slimmer proxy -*- lexical-binding: t; -*-

;; taoline © 2018-∞ Peter 11111000000
;; feebleline © 2018 Benjamin Lindqvist

;; Author of taoline: Peter <11111000000@email.com>
;; Author of feebleline: Benjamin Lindqvist <benjamin.lindqvist@gmail.com>
;; URL: https://github.com/11111000000/taoline
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

;; Taoline removes the modeline and replaces it with a slimmer proxy
;; version, which displays some basic information in the echo area
;; instead.  This information is only displayed if the echo area is not used
;; for anything else (but if you switch frame/window, it will replace whatever
;; message is currently displayed).

;; To customize taoline's format, modify `taoline-mode-line-text'.

;; NOTE:
;; taoline.el will look considerably better with the following
;; settings:

;;   (window-divider-mode t)
;;   (setq window-divider-default-bottom-width 1)
;;   (setq window-divider-default-places (quote bottom-only))

;; But this mode does not work for all EMACS versions and may not work with
;; terminal EMACS (but I haven't checked).  If you're on GUI EMACS and your
;; version supports it, just place the following in your init file:

;;   (taoline-default-settings)

;; Otherwise, do (taoline-mode t) instead, but be warned that I'm not sure
;; if it will look good.

;;; Code:

(require 'advice)

(defvar taoline-use-legacy-settings nil)
(when (< emacs-major-version 25)
  (setq taoline-use-legacy-settings t))

(setq taoline-use-legacy-settings nil)

(defface taoline-time-face '((t :background "#002200" :foreground "#00aa00" :height 0.96 :bold nil :family "Digital Display"))
  "Taoline time face (with improved vertical centering for minibuffer proxy)."
  :group 'taoline)
(defface taoline-input-face '((t :inherit 'fixed-pitch :height 0.92))
  "Taoline input face."
  :group 'taoline)
(defface taoline-bufname-face '((t :inherit 'fixed-pitch :height 0.96))
  "Taoline filename face."
  :group 'taoline)
(defface taoline-linum-face '((t :inherit 'fixed-pitch :height 0.90))
  "Taoline linum face."
  :group 'taoline)
(defface taoline-asterisk-face '((t :foreground "yellow"  :inherit 'fixed-pitch :height 0.92))
  "Taoline file modified asterisk face."
  :group 'taoline)
(defface taoline-previous-buffer-face '((t :foreground "#7e7e7e" :height 0.96))
  "Taoline filename face."
  :group 'taoline)
(defface taoline-dir-face '((t :inherit 'font-lock-variable-name-face :height 0.96))
  "Taoline filename face."
  :group 'taoline)
(defface taoline-git-branch-face '((t :inherit 'font-lock-comment-face :bold nil :italic t :height 0.96))
  "Taoline filename face."
  :group 'taoline)

;; Customizations
(defcustom taoline-show-previous-buffer nil
  "Set this if you want to show the previous 'buffer-name' in the modeline proxy."
  :group 'taoline)
(defcustom taoline-show-directory t
  "Set this if you want to show the direcory path as well as the file-name in the modeline proxy."
  :group 'taoline)

(defun taoline-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defvar taoline-mode-line-text nil
  "Each element is a list with the following format:

    (FORMAT-STRING FORMAT-ARGS PROPS)

FORMAT-STRING will be used as the first argument to `format', and
FORMAT-ARGS (a list) will be expanded as the rest of `format'
arguments.  If PROPS is given, it should be a list which will be
sent to `add-text-properties'.")

(defun taoline--git-branch-string ()
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

(defvar taoline--home-dir nil)

(require 'all-the-icons)

(setq taoline-default-icon (all-the-icons-octicon "browser" :height 1  :v-adjust 0))

(setq taoline-mode-line-text
 '(
   ("%s " ((cond ((equal current-input-method "russian-computer") "") (t "EN")))
    ( '(face taoline-input-face)))
   (" %s " ((format-time-string "%H:%M:%S"))
    ( '(face taoline-time-face)))
   (" %s" ((let ((icon (all-the-icons-icon-for-mode major-mode)))
             (cond
              ((or (not icon) (symbolp icon)) (or taoline-default-icon ""))
              (t icon))))
    ((let ((icon (all-the-icons-icon-for-mode major-mode :height 0.8)))
       (and icon (not (symbolp icon)) (text-properties-at 0 icon)))))
   (" %s" ((if (and taoline-show-directory (buffer-file-name))
              (replace-regexp-in-string
               taoline--home-dir "~"
               (file-name-directory (buffer-file-name)))
            ""))
    ( '(face taoline-dir-face)))
   ("%s" ((if (buffer-file-name)
              (substring-no-properties (file-name-nondirectory (buffer-file-name)))
            (substring-no-properties (buffer-name))))
    nil) ;; Очистка text properties с имени буфера!
   ("%s" ((let ((branch (taoline--git-branch-string))) (if (> (length branch) 0) (concat ":" branch) "")))
    ( '(face taoline-git-branch-face)))
   ("%s" ((if (buffer-modified-p) " * " 
            "" ))
    ( '(face taoline-asterisk-face)))
   (" %s " ((format "%s:%s" (format-mode-line "%l") (current-column)))
    ( '(face taoline-linum-face)))
   ;; ("%s" ((concat " | " (taoline-previous-buffer-name)))
   ;; (face taoline-previous-buffer-face))
   ))

(defun taoline-default-settings-on ()
  "Some default settings that works well with taoline."
  (setq window-divider-default-bottom-width 1
        window-divider-default-places (quote bottom-only))
  (window-divider-mode t)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil))

(defun taoline-legacy-settings-on ()
  "Some default settings for EMACS < 25."
  (set-face-attribute 'mode-line nil :height 0.1))

(defvar taoline/timer)
(defvar taoline/mode-line-format-previous)

(defvar taoline--last-placeholder "")

;;;###autoload
(define-minor-mode taoline-mode
  "Replace modeline with a slimmer proxy."
  :require 'taoline
  :global t
  (if taoline-mode
      ;; Activation:
      (progn
        (setq taoline--home-dir (expand-file-name "~"))
        (setq taoline/mode-line-format-previous mode-line-format)
        (setq taoline/timer
               (run-with-timer 0 0.2 'taoline-mode-line-proxy-fn)) ;; Reduced timer frequency for better performance.
        (if taoline-use-legacy-settings (taoline-legacy-settings-on)
          (taoline-default-settings-on))
        ;; (ad-activate 'handle-switch-frame)
        (add-hook 'focus-in-hook 'taoline-mode-line-proxy-fn))

    ;; Deactivation:
    (set-face-attribute 'mode-line nil :height 1.0)
    (setq-default mode-line-format taoline/mode-line-format-previous)
    (setq mode-line-format taoline/mode-line-format-previous)
    (cancel-timer taoline/timer)
    ;; (ad-deactivate 'handle-switch-frame)
    (remove-hook 'focus-in-hook 'taoline-mode-line-proxy-fn)
    (force-mode-line-update)
    (redraw-display)
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer))))

(defun taoline--mode-line-part (part)
  "Return a PART (an element) of `taoline-mode-line-text` as a propertized string."
  (let* ((args (mapcar #'eval (cadr part)))
         ;; Ensure first arg to format is a string even if nil
         (fmt (or (car part) ""))
         (text (apply #'format (append (list fmt) args)))
         (props  (eval (car (elt part 2)))))
    (when props
      (add-text-properties 0 (length text) props text))
    text))

(defvar taoline-placeholder)
(defvar taoline--last-placeholder "")

(defun taoline-write-buffer-name-maybe ()
  "Replace echo-area message with mode-line proxy (unless minibuffer is active, minibuf contains overlays, or text is unchanged).
If eldoc-box or another overlay is currently displaying something, taoline will not overwrite the echo area."
  (when (and (not (active-minibuffer-window))
             (get-buffer " *Minibuf-0*"))
    (with-current-buffer " *Minibuf-0*"
      (let* ((currentbuf (window-buffer (selected-window))) ;; выбираем активный буфер
             (placeholder
              (with-current-buffer currentbuf
                (mapconcat
                 (lambda (x) (substring-no-properties x)) ; Стираем все text properties с каждой части!
                 (mapcar #'taoline--mode-line-part taoline-mode-line-text)
                 ""))))
        (let ((has-ol (overlays-in (point-min) (point-max))))
          (when (and (not has-ol))
            (unless (equal placeholder taoline--last-placeholder)
              (setq taoline--last-placeholder placeholder)
              (erase-buffer)
              (insert placeholder))))))))

(defun taoline-mode-line-proxy-fn ()
  "Put a mode-line proxy in the echo area *if* echo area is empty."
  (unless (or (current-message)
              (active-minibuffer-window)) ; Do not update if minibuffer is used.
    (taoline-write-buffer-name-maybe)))

;; (defadvice handle-switch-frame (after switch-frame-message-name)
;;   "Get the modeline proxy to work with i3 switch focus."
;;   (taoline-write-buffer-name-maybe)
;;   ad-do-it
;;   (taoline-write-buffer-name-maybe))

(provide 'taoline)
;;; taoline.el ends here
