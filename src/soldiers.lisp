(in-package #:rogue-company)

;;; Soldats en tableaux typés : un soldat est un indice, chaque attribut un
;;; tableau. Chaque régiment possède une plage contiguë d'indices (regiment.lisp).
;;;
;;; La capacité ne grandit qu'à l'ajout d'un régiment, jamais pendant un tick :
;;; les systèmes n'allouent rien.
;;;
;;; Un soldat mort a des points de vie nuls : il est retiré de sa plage à la fin
;;; du tick (combat.lisp), mais son indice reste réservé. La grille spatiale,
;;; les collisions et le ciblage l'ignorent.

(deftype float-array () '(simple-array single-float (*)))
(deftype fixnum-array () '(simple-array fixnum (*)))

(defparameter *soldier-float-arrays*
  '(x y prev-x prev-y facing prev-facing slot-x slot-y hp radius mass push-x push-y damage)
  "Slots de SOLDIERS qui sont des tableaux de single-float.")

(defparameter *soldier-fixnum-arrays*
  '(regiment side walking target weapon switch-timer cooldown calm last-attacker)
  "Slots de SOLDIERS qui sont des tableaux de fixnums.")

(defparameter *walk-start-distance* 2f0
  "Au-delà de cette distance à son emplacement, le soldat se tourne vers lui pour
marcher.")

(defparameter *walk-stop-distance* 1f0
  "En deçà, il fait face au front de sa formation, et recule ou se décale sans se
retourner. L'écart entre les deux seuils évite qu'il alterne à la limite.")

(defclass soldiers ()
  ((count :initform 0 :accessor soldiers-count :type fixnum
          :documentation "Indices réservés : la somme des capacités des régiments.")
   (x :initform (make-float-array 0) :reader soldiers-x :type float-array
      :documentation "Position, en mètres.")
   (y :initform (make-float-array 0) :reader soldiers-y :type float-array)
   (prev-x :initform (make-float-array 0) :reader soldiers-prev-x :type float-array
           :documentation "Position au tick précédent, pour interpoler le dessin.")
   (prev-y :initform (make-float-array 0) :reader soldiers-prev-y :type float-array)
   (facing :initform (make-float-array 0) :reader soldiers-facing :type float-array
           :documentation "Orientation, en radians dans [-pi, pi] ; 0 regarde vers +x.")
   (prev-facing :initform (make-float-array 0) :reader soldiers-prev-facing :type float-array
                :documentation "Orientation au tick précédent, pour interpoler le dessin.")
   (slot-x :initform (make-float-array 0) :reader soldiers-slot-x :type float-array
           :documentation "Emplacement cible dans la formation.")
   (slot-y :initform (make-float-array 0) :reader soldiers-slot-y :type float-array)
   (hp :initform (make-float-array 0) :reader soldiers-hp :type float-array)
   (radius :initform (make-float-array 0) :reader soldiers-radius :type float-array
           :documentation "Rayon de collision, copié des stats du régiment.")
   (mass :initform (make-float-array 0) :reader soldiers-mass :type float-array
         :documentation "Masse, copiée des stats : partage la séparation entre deux soldats.")
   (push-x :initform (make-float-array 0) :reader soldiers-push-x :type float-array
           :documentation "Déplacement de séparation accumulé pendant les collisions du tick.")
   (push-y :initform (make-float-array 0) :reader soldiers-push-y :type float-array)
   (regiment :initform (make-array 0 :element-type 'fixnum) :reader soldiers-regiment
             :type fixnum-array :documentation "Indice du régiment dans la bataille.")
   (walking :initform (make-array 0 :element-type 'fixnum) :reader soldiers-walking
            :type fixnum-array
            :documentation "1 si le soldat marche face à sa destination, 0 s'il fait face au front.")
   ;; Combat (combat.lisp)
   (side :initform (make-array 0 :element-type 'fixnum) :reader soldiers-side
         :type fixnum-array :documentation "Camp, copié du régiment.")
   (target :initform (make-array 0 :element-type 'fixnum) :reader soldiers-target
           :type fixnum-array :documentation "Ennemi visé, ou -1.")
   (weapon :initform (make-array 0 :element-type 'fixnum) :reader soldiers-weapon
           :type fixnum-array :documentation "Arme en main : 0 principale, 1 secondaire.")
   (switch-timer :initform (make-array 0 :element-type 'fixnum) :reader soldiers-switch-timer
                 :type fixnum-array :documentation "Ticks avant d'avoir l'arme en main.")
   (cooldown :initform (make-array 0 :element-type 'fixnum) :reader soldiers-cooldown
             :type fixnum-array
             :documentation "Ticks avant le prochain coup ; -1 hors combat, le premier
coup est alors décalé au hasard.")
   (calm :initform (make-array 0 :element-type 'fixnum) :reader soldiers-calm
         :type fixnum-array
         :documentation "Ticks depuis qu'un ennemi était à portée de l'arme secondaire.")
   (last-attacker :initform (make-array 0 :element-type 'fixnum) :reader soldiers-last-attacker
                  :type fixnum-array
                  :documentation "Régiment du dernier soldat qui l'a blessé, ou -1.")
   (damage :initform (make-float-array 0) :reader soldiers-damage :type float-array
           :documentation "Dégâts reçus pendant le tick, appliqués ensemble.")))

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
        (dolist (slot (append *soldier-fixnum-arrays* *soldier-float-arrays*))
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
             :end2 count)
    (replace (the float-array (soldiers-prev-facing soldiers))
             (the float-array (soldiers-facing soldiers))
             :end2 count)))

(declaim (inline wrap-angle))
(defun wrap-angle (angle)
  "ANGLE ramené dans [-pi, pi], pour un ANGLE dans [-3 pi, 3 pi]."
  (declare (type single-float angle))
  (cond ((> angle #.(float pi 1f0)) (- angle #.(float (* 2 pi) 1f0)))
        ((< angle #.(float (- pi) 1f0)) (+ angle #.(float (* 2 pi) 1f0)))
        (t angle)))

(defun move-toward-slots (soldiers start end step front turn-step hold-distance)
  "Avance les soldats de START à END d'au plus STEP mètres vers leur emplacement,
et les tourne d'au plus TURN-STEP radians : vers leur destination s'ils en sont
loin, vers FRONT, l'orientation de la formation, s'ils en sont près. Un soldat
dont la cible est à moins de HOLD-DISTANCE (de bord à bord) ne bouge plus : il
se tourne vers elle."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum start end)
           (type single-float step front turn-step hold-distance))
  (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
        (slot-xs (soldiers-slot-x soldiers)) (slot-ys (soldiers-slot-y soldiers))
        (facings (soldiers-facing soldiers))
        (walking (soldiers-walking soldiers))
        (targets (soldiers-target soldiers))
        (hps (soldiers-hp soldiers))
        (radii (soldiers-radius soldiers))
        (start-distance *walk-start-distance*)
        (stop-distance *walk-stop-distance*))
    (declare (type float-array xs ys slot-xs slot-ys facings hps radii)
             (type fixnum-array walking targets)
             (type single-float start-distance stop-distance))
    (flet ((turn-toward (i angle)
             (declare (type fixnum i) (type single-float angle))
             (let ((turn (wrap-angle (- angle (aref facings i)))))
               (setf (aref facings i)
                     (wrap-angle (+ (aref facings i) (max (- turn-step) (min turn-step turn))))))))
      (declare (inline turn-toward))
      (loop for i of-type fixnum from start below end
            do (let ((target (aref targets i)))
                 (if (and (>= target 0) (plusp (aref hps target))
                          (let ((tx (- (aref xs target) (aref xs i)))
                                (ty (- (aref ys target) (aref ys i))))
                            (<= (- (sqrt (the (single-float 0f0) (+ (* tx tx) (* ty ty))))
                                   (aref radii i) (aref radii target))
                                hold-distance)))
                     (progn
                       (setf (aref walking i) 0)
                       (turn-toward i (atan (- (aref ys target) (aref ys i))
                                            (- (aref xs target) (aref xs i)))))
                     (let* ((dx (- (aref slot-xs i) (aref xs i)))
                            (dy (- (aref slot-ys i) (aref ys i)))
                            (distance (sqrt (the (single-float 0f0) (+ (* dx dx) (* dy dy))))))
                       (declare (type single-float dx dy distance))
                       ;; Orientation : face à la destination en marche, face au front sinon.
                       (setf (aref walking i)
                             (if (zerop (aref walking i))
                                 (if (> distance start-distance) 1 0)
                                 (if (< distance stop-distance) 0 1)))
                       (turn-toward i (if (zerop (aref walking i)) front (atan dy dx)))
                       (if (<= distance step)
                           (setf (aref xs i) (aref slot-xs i)
                                 (aref ys i) (aref slot-ys i))
                           (let ((ratio (/ step distance)))
                             (incf (aref xs i) (* dx ratio))
                             (incf (aref ys i) (* dy ratio)))))))))))

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
      "facing_degrees" (json-float (* (aref (soldiers-facing soldiers) i) (/ 180 pi)))
      "walking" (if (plusp (aref (soldiers-walking soldiers) i)) t 'yason:false)
      "hp" (json-float (aref (soldiers-hp soldiers) i))
      "target" (aref (soldiers-target soldiers) i)
      "weapon" (if (zerop (aref (soldiers-weapon soldiers) i)) "primary" "secondary")
      "cooldown" (aref (soldiers-cooldown soldiers) i)))
