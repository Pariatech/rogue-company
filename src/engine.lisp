(in-package #:rogue-company)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defun main ()
  (unless (sb-thread:main-thread-p)
    (error "main doit tourner sur le thread principal (exigence de Cocoa)."))
  (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
    (raylib:with-window (*window-width* *window-height* "rogue-company")
      (raylib:set-target-fps 60)
      (loop until (raylib:window-should-close)
            do (raylib:with-drawing
                 (draw))))))
