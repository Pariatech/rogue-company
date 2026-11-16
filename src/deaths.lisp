(in-package #:rogue-company)

;;; Journal des morts d'une bataille : un mort est retiré des tableaux de
;;; soldats par échange (remove-soldier), son indice resservant aussitôt. Le
;;; journal garde ce qu'il faut pour le dessiner tomber puis gisant : position,
;;; orientation, camp, tick de la mort et rang qu'il occupait.
;;;
;;; Tampon circulaire de taille fixe, alloué une fois : au-delà, les morts les
;;; plus anciens sont oubliés. Écrit par apply-damage (combat.lisp), sans
;;; allocation ni tirage aléatoire : il ne change rien à la simulation.

(defparameter *death-log-size* 4096
  "Morts gardés par bataille : au-delà, les plus anciens disparaissent.")

(defclass death-log ()
  ((x :initarg :x :reader death-x :type float-array)
   (y :initarg :y :reader death-y :type float-array)
   (facing :initarg :facing :reader death-facing :type float-array)
   (side :initarg :side :reader death-side :type fixnum-array)
   (tick :initarg :tick :reader death-tick :type fixnum-array)
   (rank :initarg :rank :reader death-rank :type fixnum-array
         :documentation "Rang du mort dans sa formation, depuis 0 au front.")
   (count :initform 0 :accessor death-count :type fixnum
          :documentation "Morts enregistrés depuis le début, oubliés compris.")))

(defun make-death-log (&optional (size *death-log-size*))
  (flet ((floats () (make-array size :element-type 'single-float :initial-element 0f0))
         (fixnums () (make-array size :element-type 'fixnum :initial-element 0)))
    (make-instance 'death-log :x (floats) :y (floats) :facing (floats)
                              :side (fixnums) :tick (fixnums) :rank (fixnums))))

(defun death-log-capacity (log)
  (length (the float-array (death-x log))))

(defun record-death (log x y facing side tick rank)
  "Ajoute un mort au journal, à la place du plus ancien s'il est plein."
  (declare (type single-float x y facing) (type fixnum side tick rank))
  (let ((slot (mod (death-count log) (death-log-capacity log))))
    (setf (aref (the float-array (death-x log)) slot) x
          (aref (the float-array (death-y log)) slot) y
          (aref (the float-array (death-facing log)) slot) facing
          (aref (the fixnum-array (death-side log)) slot) side
          (aref (the fixnum-array (death-tick log)) slot) tick
          (aref (the fixnum-array (death-rank log)) slot) rank)
    (incf (death-count log))
    log))

(defun record-soldier-death (battle regiment i)
  "Inscrit au journal le soldat I de REGIMENT, avant son retrait."
  (let* ((soldiers (battle-soldiers battle))
         (alive (regiment-alive regiment))
         (files (max 1 (formation-files (regiment-formation regiment) alive))))
    (record-death (battle-deaths battle)
                  (aref (soldiers-x soldiers) i) (aref (soldiers-y soldiers) i)
                  (aref (soldiers-facing soldiers) i)
                  (regiment-side regiment) (battle-tick-count battle)
                  (floor (- i (regiment-start regiment)) files))))
