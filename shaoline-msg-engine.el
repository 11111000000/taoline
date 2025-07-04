;;; shaoline-msg-engine.el --- Shaoline message management (timeout, state, helpers) -*- lexical-binding: t -*-

;; Internal – not for user loading.

(defvar shaoline-msg--last-user-message nil
  "Last *user* message captured by Shaoline (string or nil).")

(defvar shaoline-msg--last-user-message-ts 0
  "Timestamp (float-time) when last user message appeared.")

(defun shaoline-msg-clear ()
  "Erase last user message & timestamp."
  (setq shaoline-msg--last-user-message nil
        shaoline-msg--last-user-message-ts 0))

(defun shaoline-msg-save (str)
  "Save STR as last user message and record current timestamp."
  (setq shaoline-msg--last-user-message str
        shaoline-msg--last-user-message-ts (float-time)))

(defun shaoline-msg-active-p (timeout)
  "Return non-nil if current user message is still to be displayed (timeout not expired)."
  (and shaoline-msg--last-user-message
       (< (float-time (time-since shaoline-msg--last-user-message-ts)) (max 0 timeout))))

(defun shaoline-msg-current ()
  "Return string of current user message, or nil."
  shaoline-msg--last-user-message)

(defun shaoline-msg-age ()
  "Return age (seconds, float) of current user message, or 0."
  (if shaoline-msg--last-user-message-ts
      (float-time (time-since shaoline-msg--last-user-message-ts))
    0))

(provide 'shaoline-msg-engine)
