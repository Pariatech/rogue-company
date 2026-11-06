(in-package #:rogue-company)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defvar *frame* 0 "Numéro de l'image courante.")

(defun main ()
  (unless (sb-thread:main-thread-p)
    (error "main doit tourner sur le thread principal (exigence de Cocoa)."))
  (start-mcp-server)
  (unwind-protect
       (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
         (raylib:with-window (*window-width* *window-height* "rogue-company")
           (raylib:set-target-fps 60)
           (loop until (raylib:window-should-close)
                 do (raylib:with-drawing
                      (draw))
                    (incf *frame*))))
    (stop-mcp-server)))
