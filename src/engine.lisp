(in-package #:rogue-company)

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *fixed-dt* (/ 1.0 60)
  "Pas de temps utilisé en mode pas à pas.")
(defparameter *max-dt* 0.1
  "Plafond du pas de temps, pour éviter les sauts après un ralentissement.")

(defvar *state*)
(defvar *frame* 0 "Numéro de l'image courante.")
(defvar *paused* nil
  "NIL si le jeu tourne, :USER si mis en pause à la demande, :ERROR après une erreur.")
(defvar *step-frames* 0
  "Nombre d'images à simuler pendant la pause, avec un pas fixe.")
(defvar *last-error* nil
  "Dernière erreur capturée : plist (:where :message :backtrace :frame).")

;;; Erreurs

(defun record-error (where condition)
  (setf *last-error*
        (list :where where
              :message (handler-case (princ-to-string condition)
                         (error () (format nil "~s" (type-of condition))))
              :backtrace (with-output-to-string (s)
                           (sb-debug:print-backtrace :count 25 :stream s))
              :frame *frame*)
        *paused* :error
        *step-frames* 0)
  (format *error-output* "~&[~a] ~a~%" where (getf *last-error* :message)))

(defmacro guarded (where &body body)
  "Exécute BODY. En cas d'erreur, l'enregistre dans *LAST-ERROR* avec la pile
d'appels, met le jeu en pause et renvoie NIL."
  (let ((block (gensym "GUARDED")))
    `(block ,block
       (handler-bind ((error (lambda (condition)
                               (record-error ,where condition)
                               (return-from ,block nil))))
         ,@body))))

(defun clear-error ()
  "Efface la dernière erreur et reprend le jeu s'il était en pause à cause d'elle."
  (setf *last-error* nil)
  (when (eq *paused* :error)
    (setf *paused* nil)))

(defun draw-error-overlay ()
  (when *last-error*
    (let ((message (getf *last-error* :message)))
      (raylib:draw-rectangle 0 0 *window-width* 90 '(40 0 0 220))
      (raylib:draw-text (format nil "Erreur dans ~(~a~), image ~d : jeu en pause"
                                (getf *last-error* :where) (getf *last-error* :frame))
                        10 10 20 :white)
      (raylib:draw-text (subseq message 0 (min 70 (length message)))
                        10 40 20 :white)
      (raylib:draw-text "Corriger puis game_reload pour reprendre." 10 65 16 :lightgray))))

;;; Boucle

(defun next-dt ()
  "Renvoie le pas de temps de l'image, ou NIL si update ne doit pas tourner."
  (cond ((not *paused*) (min (raylib:get-frame-time) *max-dt*))
        ((plusp *step-frames*) (decf *step-frames*) *fixed-dt*)))

(defun draw-error-p ()
  (eq (getf *last-error* :where) :draw))

(defun run-frame ()
  (run-pending-jobs :max 8)
  (let ((dt (next-dt)))
    (when dt
      (guarded :update
        (update *state* dt))
      (advance-input)))
  (raylib:with-drawing
    (if (draw-error-p)
        (raylib:clear-background :black)
        (guarded :draw (draw *state*)))
    (draw-error-overlay))
  (incf *frame*))

(defun main ()
  (unless (sb-thread:main-thread-p)
    (error "main doit tourner sur le thread principal (exigence de Cocoa)."))
  (start-mcp-server)
  (unwind-protect
       (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
         (raylib:with-window (*window-width* *window-height* "rogue-company")
           (raylib:set-target-fps 60)
           (loop until (raylib:window-should-close)
                 do (run-frame))))
    (stop-mcp-server)))
