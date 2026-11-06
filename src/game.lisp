(in-package #:rogue-company)

;;; État, update et draw : le code modifié à chaud par game_reload.
;;; DEFCLASS plutôt que DEFSTRUCT pour que les instances existantes suivent
;;; les changements de classe ; DEFVAR pour que *STATE* survive au rechargement.

(defclass game-state ()
  ((x :initarg :x :initform 100.0 :accessor x)
   (y :initarg :y :initform 100.0 :accessor y)
   (size :initarg :size :initform 50 :accessor size)
   (move-speed :initarg :move-speed :initform 240.0 :accessor move-speed
               :documentation "Pixels par seconde.")))

(defvar *state* (make-instance 'game-state))

(defun update (state dt)
  (let ((dx 0) (dy 0))
    (when (key-down-p :key-right) (incf dx))
    (when (key-down-p :key-left) (decf dx))
    (when (key-down-p :key-down) (incf dy))
    (when (key-down-p :key-up) (decf dy))
    (incf (x state) (* dx (move-speed state) dt))
    (incf (y state) (* dy (move-speed state) dt))))

(defun draw (state)
  (raylib:clear-background :black)
  (raylib:draw-rectangle (round (x state)) (round (y state))
                         (size state) (size state) :blue)
  (raylib:draw-text "Fleches : deplacer le carre" 10 (- *window-height* 30) 20 :darkgray))
