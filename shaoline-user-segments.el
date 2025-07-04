;;; shaoline-user-segments.el --- User-defined segments for Shaoline -*- lexical-binding: t -*-

;; This file is optional! Define any custom segments you desire here.
;; It will be loaded automatically by Shaoline (if present).

(shaoline-define-segment shaoline-segment-zen-quote ()
  "Shows a random Zen quote."
  (let ((quotes '("Sitting quietly, doing nothing, spring comes and the grass grows by itself."
                  "If you want to climb a mountain, begin at the top."
                  "Before enlightenment: chop wood, carry water. After enlightenment: chop wood, carry water."
                  "The ten thousand things return to the one. Where does the one return to?")))
    (propertize (nth (random (length quotes)) quotes)
                'face 'shaoline-echo-face)))

;; To use, add to :center or :right, for example:
;; (push 'shaoline-segment-zen-quote (alist-get :center shaoline-segments))

(provide 'shaoline-user-segments)
