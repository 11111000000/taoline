;;; shaoline.el --- Functional minimalist echo-area modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Peter
;;
;; Author: Peter <11111000000@email.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mode-line, minimal, functional, echo-area
;; URL: https://github.com/11111000000/SHAOLINE
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: MIT

;;; Commentary:

;; In the land of the rising buffer, modelines wandered the world in excess—
;; until Shaoline arrived, shaved its segments, and sat quietly in the echo area.
;; The true path has neither timers nor distractions.
;;
;; Shaoline-echo is a trimmed-down edition of the original: a Shaolin monk
;; among modelines. There is no child-frame and no in-window modeline —
;; the only rendering target is the echo area (the minibuffer). The architecture
;; remains functional: each segment is a pure function, unaffected by worldly
;; state, and the only side effects are writing to the echo area and (optionally)
;; hiding the regular `mode-line` (so your modeline too may attain Nirvana).
;;
;; Key features (as written upon a bamboo leaf):
;;   • Minimalist, declarative segment configuration. Add, remove, or swap
;;     segments like stones in the Zen garden.
;;   • No polling timers — the modeline is refreshed only by the wind
;;     (or `post-command-hook`, etc.).
;;   • Global minor mode `shaoline-mode` toggles everything with serenity.
;;   • Optional hiding of the traditional mode line, for those seeking emptiness.
;;   • A unit-test-friendly “core” — the composing function is a silent
;;     observer, causing no side effects (Wu Wei by design).
;;
;; Quick start:
;;
;;   (require 'shaoline-echo)
;;   (shaoline-mode 1)
;;
;; See the README on GitHub for scrolls and stories about writing custom segments.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'rx))
;; project-specific libraries are only needed by segments (see shaoline-segments.el);
;; keeping the core truly “pure”.


;; ----------------------------------------------------------------------------
;; Customization group and variables
;;
;; If you seek change, like the autumn leaf, customize here.

(defgroup shaoline nil
  "Functional minimalist echo-area modeline."
  :group 'convenience
  :prefix "shaoline-")

(defcustom shaoline-icon-width 2
  "Width in characters to pad or truncate all-the-icons icons for consistent spacing.
Increase if your favorite icons are wider."
  :type 'integer
  :group 'shaoline)


(defcustom shaoline-debug t
  "If non-nil, log shaoline activity into `shaoline--log-buffer'. Seek inner debugness."
  :type 'boolean
  :group 'shaoline)

(defvar shaoline--log-buffer "*shaoline-logs*"
  "Buffer name used for debug log. A quiet scroll for Shaoline whispers.")

(defun shaoline--log (fmt &rest args)
  "Write formatted FMT/ARGS message to `shaoline--log-buffer' if `shaoline-debug' is non-nil.
Zen masters say: A log unread is a tree falling in a silent forest."
  (when shaoline-debug
    (let ((msg (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create shaoline--log-buffer)
        (goto-char (point-max))
        (insert msg "\n")))))

(defcustom shaoline-segments
  '((:left shaoline-segment-icon-and-buffer shaoline-segment-git-branch)
    (:center shaoline-segment-echo-message)
    (:right shaoline-segment-project-name shaoline-segment-battery shaoline-segment-time))
  "Alist describing segments for :left, :center and :right.
Each entry is a list of segment function symbols for that side.
Tweak segments as calmly as rearranging stones: simply, purposefully."
  :type '(alist :key-type symbol :value-type (repeat function))
  :group 'shaoline)

(defcustom shaoline-update-hooks
  '(post-command-hook find-file-hook after-save-hook)
  "Hooks that tell Shaoline to recompute and display the modeline."
  :type '(repeat symbol)
  :group 'shaoline)

(defcustom shaoline-autohide-modeline t
  "When non-nil, the traditional mode-line is hidden while `shaoline-mode' is active."
  :type 'boolean
  :group 'shaoline)

(defcustom shaoline-right-padding 0
  "Extra spaces appended to the right edge of the shaoline. Sometimes, a little emptiness is all you need."
  :type 'integer
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; Faces

(defface shaoline-base-face
  '((t :inherit default
       :height 1.0
       :box nil
       :underline nil
       :overline nil
       :inverse-video nil
       :extend t))
  "Base face for shaoline. The foundation for all modeline elements."
  :group 'shaoline)

(defface shaoline-echo-face
  '((t :inherit (font-lock-comment-face shaoline-base-face) :height 1.0))
  "Face for the echo message segment (slightly smaller)."
  :group 'shaoline)

(defface shaoline-time-face
  '((t :background "#002200" :foreground "#00aa00" :height 1.0 :bold nil :family "Digital Display"))
  "Face for the time segment."
  :group 'shaoline)

(defface shaoline-battery-face
  '((t :inherit (shaoline-base-face)))
  "Face for battery level segment."
  :group 'shaoline)

(defface shaoline-buffer-face
  '((t :inherit (shaoline-base-face) :height 1.0))
  "Face for the buffer name segment."
  :group 'shaoline)

(defface shaoline-project-face
  '((t :inherit (font-lock-keyword-face shaoline-base-face) :height 1.0))
  "Face for the project name segment."
  :group 'shaoline)

(defface shaoline-modified-face
  '((t :inherit (warning shaoline-base-face) :height 1.0))
  "Face for the modified indicator segment."
  :group 'shaoline)

(defface shaoline-git-face
  '((t :inherit (font-lock-type-face shaoline-base-face) :height 1.0))
  "Face for the git branch segment."
  :group 'shaoline)

(defface shaoline-mode-face
  '((t :inherit (font-lock-type-face shaoline-base-face) :height 1.0))
  "Face for the major mode segment."
  :group 'shaoline)

;; ----------------------------------------------------------------------------
;; Internal state

(defvar shaoline--segment-table (make-hash-table :test 'eq)
  "Hash-table storing registered segment functions.")

(defvar shaoline--last-str ""
  "Last rendered string, used to avoid unnecessary redisplay. Repetition is not enlightenment.")

;; Message state now handled by shaoline-msg-engine.el
(require 'shaoline-msg-engine)

(defvar shaoline--default-mode-line-format-backup nil
  "Backup of `default-mode-line-format' when the modeline is hidden (restored when Shaoline is disabled).")

(defvar shaoline--resize-mini-windows-backup nil
  "Backup of `resize-mini-windows' when `shaoline-mode` toggles. Settings are always restored on exit.")

(defvar-local shaoline--saved-mode-line-format nil
  "Buffer-local backup of `mode-line-format` for use while `shaoline-mode` is hiding the classic modeline.
Restored exactly as it was when the mode is toggled off.")

;; ----------------------------------------------------------------------------
;; Infrastructure (UI, advice, minor mode, etc.) now in shaoline-infra.el.
(require 'shaoline-infra)

;; ----------------------------------------------------------------------------
;; Segment definition macros


(defmacro shaoline-define-segment (name args &rest body)
  "Define segment NAME taking ARGS, register it, and return its symbol.
Every segment is independent; every function a ripple in the pond."
  (declare (indent defun))
  (let ((func `(defun ,name ,args ,@body)))
    `(progn
       ,func
       (puthash ',name #',name shaoline--segment-table)
       ',name)))

(defmacro shaoline-define-simple-segment (name docstring &rest body)
  "Define a segment NAME with no arguments. BODY is run on each render.
True elegance: nothing more than necessary."
  `(shaoline-define-segment ,name ()
     ,docstring
     ,@body))

;; ----------------------------------------------------------------------------
;; Segment helper functions

(defun shaoline--apply-segment (fn buffer)
  "Call segment FN with BUFFER (or with no args if not needed) and return its string.
Errors are returned as a diagnostic string, not signalled; stacktrace is logged if `shaoline-debug`."
  (condition-case err
      (let* ((arity (help-function-arglist fn t))
             (res (if (and (consp arity) (not (null arity)))
                      (funcall fn buffer)
                    (funcall fn))))
        (if (stringp res) res (or (and res (format "%s" res)) "")))
    (error
     (let ((msg (format "[SEGMENT ERROR: %s in %s]" err fn)))
       (when shaoline-debug
         (shaoline--log "%s" msg)
         (shaoline--log "Traceback: %s" (with-output-to-string (backtrace))))
       (propertize msg 'face 'error)))))

(defun shaoline--collect-segments (side buffer)
  "Render all segments for SIDE (:left, :center, :right) within BUFFER.
Only non-empty results are included."
  (cl-loop for fn in (cdr (assq side shaoline-segments))
           for str = (shaoline--apply-segment fn buffer)
           unless (or (null str) (string-empty-p str))
           collect str))

;; ----------------------------------------------------------------------------
;; Core: calculating available center width (pure)

(defun shaoline-available-center-width (&optional buffer window width)
  "Return the maximum width available for the center segment (excluding right-padding).
Calculates based on the length of left and right segments, including any necessary spacing."
  (let* ((window (or (and (windowp window) window)
                     (selected-window)))
         (buffer (or buffer (window-buffer window)))
         (width  (or width
                     (let* ((mini (minibuffer-window))
                            (mini-width (and (window-live-p mini)
                                             (window-width mini))))
                       (or mini-width (frame-width)))))
         (left   (mapconcat #'identity (shaoline--collect-segments :left buffer) " "))
         (right  (mapconcat #'identity (shaoline--collect-segments :right buffer) " "))
         (left-w (string-width left))
         (right-w (string-width right))
         (space-left  (if (and (not (string-empty-p left))
                               (not (string-empty-p right)))
                          1 0))
         ;; If the center segment is empty, no space is needed before/after.
         ;; If both sides exist, one space is left before the center.
         (space-center-left (if (and (not (string-empty-p left)))
                                1 0))
         (space-center-right (if (not (string-empty-p right))
                                 1 0)))
    (max 0 (- width
              left-w
              right-w
              ;; NOTE: We only leave spaces where segments exist
              (if (not (string-empty-p left)) 1 0)
              (if (not (string-empty-p right)) 1 0)
              shaoline-right-padding))))

;; ----------------------------------------------------------------------------
;; Core: compose the modeline (pure version, NO side effects)
;;-----------------------------------------------------------------------------
;; This function is pure: it only computes a string with properties, no UI,
;; no global state. It is safe for use in tests and non-UI code.

(defun shaoline-compose-modeline (&optional buffer window _width)
  "Return the Shaoline string for the echo area.

Pure function: collects, arranges, and truncates segments for rendering 
in the echo area. This function is entirely side-effect free.
The right segment is *pinned* to the right edge by means of the
`space :align-to' display property, while the central segment is
truncated when necessary so that the whole string never exceeds
the window width."
  (let* ((buffer  (or buffer (current-buffer)))
         (window  (or window  (selected-window)))
         (left    (mapconcat #'identity
                             (shaoline--collect-segments :left buffer) " "))
         (center0 (mapconcat #'identity
                             (shaoline--collect-segments :center buffer) " "))
         (right   (mapconcat #'identity
                             (shaoline--collect-segments :right buffer) " "))
         (gap-left (if (string-empty-p left) "" " "))
         (gap-mid  (if (and (not (string-empty-p center0))
                            (not (string-empty-p right)))
                       " " ""))
         (avail (max 0 (- (window-width window)
                          (string-width left)
                          (string-width gap-left)
                          (string-width right)
                          shaoline-right-padding
                          (string-width gap-mid))))
         (center (if (> (string-width center0) avail)
                     (truncate-string-to-width center0 avail 0 ?\s)
                   center0)))
    (concat
     left
     gap-left
     center
     gap-mid
     (propertize
      " "
      'display `(space :align-to (- right ,(+ (string-width right)
                                              shaoline-right-padding
                                              1))))
     right
     (make-string shaoline-right-padding ?\s))))





;; ----------------------------------------------------------------------------
;; Segments are now located in a separate file.
(require 'shaoline-segments)

;; --- Auto-load user-defined segments if available ---
(let ((user-file
       (or
        (expand-file-name "shaoline-user-segments.el" user-emacs-directory)
        (and load-file-name
             (expand-file-name "shaoline-user-segments.el" (file-name-directory load-file-name))))))
  (when (and user-file (file-exists-p user-file))
    (condition-case err
        (load user-file nil t)
      (error (shaoline--log "Could not load user segments: %s" err)))))


(provide 'shaoline)
;;; shaoline.el ends here
