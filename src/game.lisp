(in-package #:rogue-company)

(defun draw ()
  (raylib:clear-background :raywhite)
  (raylib:draw-rectangle (+ 100 (mod *frame* 600)) 100 50 50 :red))
