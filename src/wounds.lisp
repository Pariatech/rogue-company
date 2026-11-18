(in-package #:rogue-company)

;;; Journal des blessures d'une bataille : chaque coup qui porte (mêlée ou tir),
;;; mortel ou non, pour dessiner le sang qui gicle puis tache le sol
;;; (blood-view.lisp). Position du blessé et direction du coup (de l'attaquant
;;; vers lui, vecteur unitaire), tick et dégâts.
;;;
;;; Tampon circulaire de taille fixe, alloué une fois : au-delà, les blessures
;;; les plus anciennes, et leurs taches, sont oubliées. Écrit par STRIKE
;;; (combat.lisp) et FIRE-SHOTS (shooting.lisp), sans allocation ni tirage
;;; aléatoire : il ne change rien à la simulation.

(defparameter *wound-log-size* 4096
  "Blessures gardées par bataille : au-delà, les plus anciennes disparaissent.")

(defclass wound-log ()
  ((x :initarg :x :reader wound-x :type float-array)
   (y :initarg :y :reader wound-y :type float-array)
   (dx :initarg :dx :reader wound-dx :type float-array
       :documentation "Direction du coup, de l'attaquant vers le blessé : x.")
   (dy :initarg :dy :reader wound-dy :type float-array)
   (damage :initarg :damage :reader wound-damage :type float-array)
   (tick :initarg :tick :reader wound-tick :type fixnum-array)
   (count :initform 0 :accessor wound-count :type fixnum
          :documentation "Blessures enregistrées depuis le début, oubliées comprises.")))

(defun make-wound-log (&optional (size *wound-log-size*))
  (flet ((floats () (make-array size :element-type 'single-float :initial-element 0f0)))
    (make-instance 'wound-log :x (floats) :y (floats) :dx (floats) :dy (floats)
                              :damage (floats)
                              :tick (make-array size :element-type 'fixnum :initial-element 0))))

(defun wound-log-capacity (log)
  (length (the float-array (wound-x log))))

(defun record-wound (log x y from-x from-y damage tick)
  "Ajoute au journal un coup reçu en X Y, porté depuis FROM-X FROM-Y, à la
place du plus ancien s'il est plein."
  (declare (type single-float x y from-x from-y damage) (type fixnum tick)
           (optimize (speed 3) (safety 0)))
  (let* ((slot (mod (the fixnum (wound-count log)) (the fixnum (wound-log-capacity log))))
         (ex (- x from-x)) (ey (- y from-y))
         (length (sqrt (+ (* ex ex) (* ey ey)))))
    (declare (type fixnum slot) (type single-float ex ey length))
    (setf (aref (the float-array (wound-x log)) slot) x
          (aref (the float-array (wound-y log)) slot) y
          (aref (the float-array (wound-dx log)) slot) (if (plusp length) (/ ex length) 1f0)
          (aref (the float-array (wound-dy log)) slot) (if (plusp length) (/ ey length) 0f0)
          (aref (the float-array (wound-damage log)) slot) damage
          (aref (the fixnum-array (wound-tick log)) slot) tick)
    (incf (the fixnum (wound-count log)))
    log))
