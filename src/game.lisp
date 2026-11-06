(in-package #:rogue-company)

(defun draw ()
  (raylib:clear-background :raywhite)
  (raylib:draw-rectangle 100 100 50 50 :red))
