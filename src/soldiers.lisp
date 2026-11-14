(in-package #:rogue-company)

;;; Soldats en tableaux typés : un soldat est un indice, chaque attribut un
;;; tableau. Chaque régiment possède une plage contiguë d'indices (regiment.lisp).
;;;
;;; La capacité ne grandit qu'à l'ajout d'un régiment, jamais pendant un tick :
;;; les systèmes n'allouent rien. Les tableaux de combat (arme en main, recharge,
;;; arme chargée, cible) s'ajouteront avec le combat.

(deftype float-array () '(simple-array single-float (*)))
(deftype fixnum-array () '(simple-array fixnum (*)))

(defparameter *soldier-float-arrays* '(x y prev-x prev-y slot-x slot-y hp)
  "Slots de SOLDIERS qui sont des tableaux de single-float.")

(defclass soldiers ()
  ((count :initform 0 :accessor soldiers-count :type fixnum
          :documentation "Indices réservés : la somme des capacités des régiments.")
   (x :initform (make-float-array 0) :reader soldiers-x :type float-array
      :documentation "Position, en mètres.")
   (y :initform (make-float-array 0) :reader soldiers-y :type float-array)
   (prev-x :initform (make-float-array 0) :reader soldiers-prev-x :type float-array
           :documentation "Position au tick précédent, pour interpoler le dessin.")
   (prev-y :initform (make-float-array 0) :reader soldiers-prev-y :type float-array)
   (slot-x :initform (make-float-array 0) :reader soldiers-slot-x :type float-array
           :documentation "Emplacement cible dans la formation.")
   (slot-y :initform (make-float-array 0) :reader soldiers-slot-y :type float-array)
   (hp :initform (make-float-array 0) :reader soldiers-hp :type float-array)
   (regiment :initform (make-array 0 :element-type 'fixnum) :reader soldiers-regiment
             :type fixnum-array :documentation "Indice du régiment dans la bataille.")))

(defun make-float-array (size)
  (make-array size :element-type 'single-float :initial-element 0f0))

(defun make-soldiers ()
  (make-instance 'soldiers))

(defun soldiers-capacity (soldiers)
  (length (soldiers-x soldiers)))

(defun grow-array (array size)
  (let ((new (make-array size :element-type (array-element-type array)
                              :initial-element (coerce 0 (array-element-type array)))))
    (replace new array)))

(defun reserve-soldiers (soldiers n)
  "Réserve N indices et renvoie le premier. Agrandit les tableaux au besoin, en
doublant la capacité pour que des ajouts successifs restent bon marché."
  (let* ((start (soldiers-count soldiers))
         (needed (+ start n)))
    (when (> needed (soldiers-capacity soldiers))
      (let ((size (max needed (* 2 (soldiers-capacity soldiers)))))
        (dolist (slot (cons 'regiment *soldier-float-arrays*))
          (setf (slot-value soldiers slot) (grow-array (slot-value soldiers slot) size)))))
    (setf (soldiers-count soldiers) needed)
    start))

;;; Systèmes

(defun copy-previous-positions (soldiers)
  (declare (optimize (speed 3) (safety 1)))
  (let ((count (soldiers-count soldiers)))
    (replace (the float-array (soldiers-prev-x soldiers)) (the float-array (soldiers-x soldiers))
             :end2 count)
    (replace (the float-array (soldiers-prev-y soldiers)) (the float-array (soldiers-y soldiers))
             :end2 count)))

(defun move-toward-slots (soldiers start end step)
  "Avance les soldats de START à END d'au plus STEP mètres vers leur emplacement."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum start end)
           (type single-float step))
  (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
        (slot-xs (soldiers-slot-x soldiers)) (slot-ys (soldiers-slot-y soldiers)))
    (declare (type float-array xs ys slot-xs slot-ys))
    (loop for i of-type fixnum from start below end
          do (let* ((dx (- (aref slot-xs i) (aref xs i)))
                    (dy (- (aref slot-ys i) (aref ys i)))
                    (distance (sqrt (the (single-float 0f0) (+ (* dx dx) (* dy dy))))))
               (declare (type single-float dx dy distance))
               (if (<= distance step)
                   (setf (aref xs i) (aref slot-xs i)
                         (aref ys i) (aref slot-ys i))
                   (let ((ratio (/ step distance)))
                     (incf (aref xs i) (* dx ratio))
                     (incf (aref ys i) (* dy ratio))))))))

(defun slot-distance (soldiers i)
  (let ((dx (- (aref (soldiers-slot-x soldiers) i) (aref (soldiers-x soldiers) i)))
        (dy (- (aref (soldiers-slot-y soldiers) i) (aref (soldiers-y soldiers) i))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun soldier-snapshot (soldiers i)
  (ht "index" i
      "x" (json-float (aref (soldiers-x soldiers) i))
      "y" (json-float (aref (soldiers-y soldiers) i))
      "slot_x" (json-float (aref (soldiers-slot-x soldiers) i))
      "slot_y" (json-float (aref (soldiers-slot-y soldiers) i))
      "hp" (json-float (aref (soldiers-hp soldiers) i))))
