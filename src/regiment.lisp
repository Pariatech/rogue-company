(in-package #:rogue-company)

;;; Régiment de bataille : ses stats effectives (stats.lisp), sa formation, ses
;;; ordres et sa plage de soldats, de START à START + ALIVE. Il ne vit que le
;;; temps d'une bataille ; la fiche persistante viendra avec la partie.
;;;
;;; Les systèmes travaillent régiment par régiment : ils lisent les stats dans
;;; des variables locales, puis parcourent la plage. Ils ne connaissent pas la
;;; bataille, seulement le régiment et les tableaux de soldats.

(defclass regiment ()
  ((index :initarg :index :reader regiment-index :type fixnum
          :documentation "Position dans BATTLE-REGIMENTS, rangée dans SOLDIERS-REGIMENT.")
   (name :initarg :name :reader regiment-name)
   (side :initarg :side :reader regiment-side :type fixnum :documentation "Camp : 0 ou 1.")
   (stats :initarg :stats :reader regiment-stats :type regiment-stats)
   (formation :initarg :formation :reader regiment-formation :type formation)
   (morale :initarg :morale :accessor regiment-morale :type single-float)
   (fatigue :initform 0f0 :accessor regiment-fatigue :type single-float
            :documentation "0 frais, 1 épuisé. Reste à 0 jusqu'à la course et au combat.")
   (orders :initform nil :accessor regiment-orders
           :documentation "Ordre en cours : (:move x y facing), (:attack régiment) ou NIL.")
   (engaged :initform 0 :accessor regiment-engaged :type fixnum
            :documentation "Soldats qui avaient une cible au dernier tick.")
   (closed :initform 0 :accessor regiment-closed :type fixnum
           :documentation "Soldats dont la cible était à la distance d'arrêt.")
   (holding :initform nil :accessor regiment-holding-p
            :documentation "Au contact : la formation ne bouge plus.")
   (contact-lost :initform 0 :accessor regiment-contact-lost :type fixnum
                 :documentation "Ticks sans aucun soldat engagé, pendant un ordre d'attaque.")
   (min-x :initform 0f0 :accessor regiment-min-x :type single-float
          :documentation "Boîte englobante des soldats vivants, recalculée à chaque tick.")
   (min-y :initform 0f0 :accessor regiment-min-y :type single-float)
   (max-x :initform 0f0 :accessor regiment-max-x :type single-float)
   (max-y :initform 0f0 :accessor regiment-max-y :type single-float)
   (start :initarg :start :reader regiment-start :type fixnum
          :documentation "Premier indice dans les tableaux de soldats.")
   (capacity :initarg :capacity :reader regiment-capacity :type fixnum)
   (alive :initarg :alive :accessor regiment-alive :type fixnum)
   (kills :initform 0 :accessor regiment-kills :type fixnum
          :documentation "Ennemis tués, convertis en expérience après la bataille.")
   (slots-dirty :initform t :accessor regiment-slots-dirty-p
                :documentation "Les emplacements sont à recalculer au prochain tick.")))

(defun regiment-traits (regiment)
  (stats-traits (regiment-stats regiment)))

(defun regiment-end (regiment)
  (+ (regiment-start regiment) (regiment-alive regiment)))

(defun order-move (regiment x y facing)
  "Envoie REGIMENT en X, Y, front tourné vers FACING (radians). Les emplacements
se recalculent au tick suivant."
  (let ((formation (regiment-formation regiment)))
    (setf (formation-x formation) (float x 1f0)
          (formation-y formation) (float y 1f0)
          (formation-facing formation) (float facing 1f0)
          (regiment-orders regiment) (list :move (float x 1f0) (float y 1f0) (float facing 1f0))
          (regiment-holding-p regiment) nil
          (regiment-slots-dirty-p regiment) t))
  regiment)

;;; Systèmes

(defun update-regiment-slots (regiment soldiers)
  "Recalcule les emplacements si la formation a changé."
  (when (regiment-slots-dirty-p regiment)
    (place-formation (regiment-formation regiment) (regiment-alive regiment)
                     (soldiers-slot-x soldiers) (soldiers-slot-y soldiers)
                     (regiment-start regiment))
    (setf (regiment-slots-dirty-p regiment) nil)))

(defparameter *hold-margin* 0.25f0
  "Marge ajoutée à la portée minimale de l'arme principale pour la distance
d'arrêt au contact.")

(defun regiment-hold-distance (regiment)
  "Distance de bord à bord où un soldat s'arrête devant sa cible : juste au-delà
de la portée minimale de son arme principale de mêlée. Avec une pique (1,5 m),
les trois ou quatre premiers rangs frappent ensemble."
  (let ((primary (stats-primary (regiment-stats regiment))))
    (+ *hold-margin*
       (if (eq (attack-kind primary) :melee) (attack-min-reach primary) 0f0))))

(defun move-regiment (regiment soldiers)
  "Avance les soldats de REGIMENT vers leur emplacement, au pas de marche."
  (let ((stats (regiment-stats regiment)))
    (move-toward-slots soldiers (regiment-start regiment) (regiment-end regiment)
                       (/ (stats-speed stats) #.(float +ticks-per-second+ 1f0))
                       (formation-facing (regiment-formation regiment))
                       (/ (stats-turn-rate stats) #.(float +ticks-per-second+ 1f0))
                       (regiment-hold-distance regiment))))

;;; Inspection

(defun regiment-slot-distances (regiment soldiers)
  "Distance moyenne et maximale des soldats à leur emplacement, en mètres."
  (let ((total 0f0) (worst 0f0) (alive (regiment-alive regiment)))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          for distance = (slot-distance soldiers i)
          do (incf total distance)
             (setf worst (max worst distance)))
    (values (if (plusp alive) (/ total alive) 0f0) worst)))

(defun regiment-snapshot (regiment soldiers &key (sample 3))
  (let ((stats (regiment-stats regiment))
        (orders (regiment-orders regiment)))
    (multiple-value-bind (mean worst) (regiment-slot-distances regiment soldiers)
      (ht "index" (regiment-index regiment)
          "name" (regiment-name regiment)
          "side" (regiment-side regiment)
          "alive" (regiment-alive regiment)
          "capacity" (regiment-capacity regiment)
          "start" (regiment-start regiment)
          "orders" (case (first orders)
                     (:move (destructuring-bind (x y facing) (rest orders)
                              (ht "kind" "move" "x" (json-float x) "y" (json-float y)
                                  "facing_degrees" (json-float (* facing (/ 180 pi))))))
                     (:attack (ht "kind" "attack" "regiment" (second orders)))
                     (t nil))
          "engaged" (regiment-engaged regiment)
          "holding" (if (regiment-holding-p regiment) t 'yason:false)
          "formation" (formation-snapshot (regiment-formation regiment) (regiment-alive regiment))
          "slot_distance_mean" (json-float mean)
          "slot_distance_max" (json-float worst)
          "morale" (json-float (regiment-morale regiment))
          "fatigue" (json-float (regiment-fatigue regiment))
          "kills" (regiment-kills regiment)
          "stats" (ht "speed" (json-float (stats-speed stats))
                      "spacing" (json-float (stats-spacing stats))
                      "ranks" (stats-ranks stats)
                      "primary" (string-downcase (attack-kind (stats-primary stats)))
                      "traits" (map 'vector #'string-downcase (stats-traits stats)))
          "soldiers" (loop for i from (regiment-start regiment)
                             below (min (regiment-end regiment) (+ (regiment-start regiment) sample))
                           collect (soldier-snapshot soldiers i))))))
