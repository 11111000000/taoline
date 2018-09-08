;;; taoline.el --- Replace modeline with a slimmer proxy

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

(defface taoline-time-face '((t :inherit 'font-lock-comment-face))
  "Taoline timestamp face."
  :group 'taoline)
(defface taoline-linum-face '((t :inherit 'default))
  "Taoline linum face."
  :group 'taoline)
(defface taoline-bufname-face '((t :inherit 'font-lock-function-name-face))
  "Taoline filename face."
  :group 'taoline)
(defface taoline-asterisk-face '((t :foreground "salmon"))
  "Taoline file modified asterisk face."
  :group 'taoline)
(defface taoline-previous-buffer-face '((t :foreground "#7e7e7e"))
  "Taoline filename face."
  :group 'taoline)
(defface taoline-dir-face '((t :inherit 'font-lock-variable-name-face))
  "Taoline filename face."
  :group 'taoline)
(defface taoline-git-branch-face '((t :inherit 'font-lock-comment-face :bold nil :italic t))
  "Taoline filename face."
  :group 'taoline)

;; Customizations
(defcustom taoline-show-time nil
  "Set this if you want to show the time in the modeline proxy."
  :group 'taoline)
(defcustom taoline-show-git-branch nil
  "Set this if you want to show the git branch in the modeline proxy."
  :group 'taoline)
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
        "(no branch)"))))

(defvar taoline--home-dir nil)

(setq
 taoline-mode-line-text
 '(
   ("%s" ((if taoline-show-time (format-time-string "[%H:%M:%S] ") ""))
    (face taoline-time-face))
   ("%6s" ((format "%s:%s" (format-mode-line "%l") (current-column)))
    (face taoline-linum-face))
   (" %s" ((if (and taoline-show-directory (buffer-file-name))
               (replace-regexp-in-string
                taoline--home-dir "~"
                (file-name-directory (buffer-file-name)))
             ""))
    (face taoline-dir-face))
   ("%s" ((if (buffer-file-name) (file-name-nondirectory (buffer-file-name))
            (buffer-name)))
    (face taoline-bufname-face))
   ("%s" ((if (and (buffer-file-name) (buffer-modified-p)) "*"
            "" ))
    (face taoline-asterisk-face))
   ("%s" ((if taoline-show-git-branch (concat " : " (taoline--git-branch-string))
            ""))
    (face taoline-git-branch-face))
   ("%s" ((if taoline-show-previous-buffer (concat " | " (taoline-previous-buffer-name))
            ""))
   (face taoline-previous-buffer-face)))
 )


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
              (run-with-timer 0 0.5 'taoline-mode-line-proxy-fn))
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
  (let ((text (apply #'format (append (list (car part))
                                      (mapcar #'eval (cadr part)))))
        (props (elt part 2)))
    (when props
      (add-text-properties 0 (length text) props text))
    text))

(defvar taoline-placeholder)
(defun taoline-write-buffer-name-maybe ()
  "Replace echo-area message with mode-line proxy."
  (progn (setq taoline-placeholder (mapconcat #'taoline--mode-line-part
                                                 taoline-mode-line-text ""))
         (with-current-buffer " *Minibuf-0*"
           (erase-buffer)
           (insert taoline-placeholder))))

(defun taoline-mode-line-proxy-fn ()
  "Put a mode-line proxy in the echo area *if* echo area is empty."
  (unless (current-message)
    (taoline-write-buffer-name-maybe)))

;; (defadvice handle-switch-frame (after switch-frame-message-name)
;;   "Get the modeline proxy to work with i3 switch focus."
;;   (taoline-write-buffer-name-maybe)
;;   ad-do-it
;;   (taoline-write-buffer-name-maybe))

(provide 'taoline)
;;; taoline.el ends here
