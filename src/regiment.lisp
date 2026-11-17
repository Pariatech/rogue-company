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
           :documentation "Ordre en cours : (:move x y facing), (:attack régiment),
(:retreat x y facing) pour un repli de tirailleurs (stances.lisp), (:rout) pour
un régiment débandé, ou NIL.")
   (engaged :initform 0 :accessor regiment-engaged :type fixnum
            :documentation "Soldats qui avaient une cible au dernier tick.")
   (closed :initform 0 :accessor regiment-closed :type fixnum
           :documentation "Soldats du premier rang dont la cible était à la distance d'arrêt.")
   (flank-attackers :initform 0 :accessor regiment-flank-attackers :type fixnum
                    :documentation "Ennemis qui le visaient de flanc au dernier tick (flanks.lisp).")
   (rear-attackers :initform 0 :accessor regiment-rear-attackers :type fixnum
                   :documentation "Ennemis qui le visaient de dos au dernier tick.")
   (holding :initform nil :accessor regiment-holding-p
            :documentation "Au contact : la formation ne bouge plus.")
   (drill :initarg :drill :initform nil :accessor regiment-drill
          :documentation "Exercice de tir des salves (shooting.lisp) : :COUNTERMARCH,
:FIRE-BY-RANK, ou NIL (feu à volonté).")
   (stance :initarg :stance :initform :hold :accessor regiment-stance
           :documentation "Posture (stances.lisp) : :HOLD s'arrête quand l'ennemi se
débande, :CHASE le poursuit, :SKIRMISH se replie devant une mêlée qui approche.")
   (resume-orders :initform nil :accessor regiment-resume-orders
                  :documentation "Ordre d'attaque repris à la fin d'un repli (:skirmish).")
   (retreat-ticks :initform 0 :accessor regiment-retreat-ticks :type fixnum
                  :documentation "Ticks depuis le début du repli en cours.")
   (chasing :initform nil :accessor regiment-chasing-p
            :documentation "Poursuit un ennemi débandé (:chase).")
   (chase-x :initform 0f0 :accessor regiment-chase-x :type single-float
            :documentation "Centre du régiment au début de la poursuite.")
   (chase-y :initform 0f0 :accessor regiment-chase-y :type single-float)
   (depth :initform 0 :accessor regiment-depth :type fixnum
          :documentation "Rangs de la grille : le rang du dernier soldat, plus un
(REFRESH-LAYOUT).")
   (bearer :initform -1 :accessor regiment-bearer :type fixnum
           :documentation "Indice du porteur d'étendard, ou -1 (REFRESH-LAYOUT).")
   (busy-tick :initform 0 :accessor regiment-busy-tick :type fixnum
              :documentation "Dernier tick en mêlée ou sous le feu : le régiment se
reforme après *REFORM-SECONDS* de calme (UPDATE-REFORM).")
   (reform-pending :initform nil :accessor regiment-reform-pending-p
                   :documentation "Un ordre vient d'être donné : le régiment se reforme
au prochain tick.")
   (steady :initform nil :accessor regiment-steady-p
           :documentation "Arrêté en formation, pour les salves (shooting.lisp, UPDATE-STEADY).")
   (alert :initform nil :accessor regiment-alert-p
          :documentation "Ennemi proche (combat.lisp, UPDATE-ALERT) : en garde, piques
basses et arquebuses en joue, même sans ordre d'attaque.")
   (open-until :initform -1 :accessor regiment-open-until :type fixnum
               :documentation "Files ouvertes pour un ami jusqu'à ce tick (open-files.lisp) ;
-1 en files normales.")
   (volley-rank :initform 0 :accessor regiment-volley-rank :type fixnum
                :documentation "Feu par rang : rang qui tire la prochaine salve.")
   (crouch :initform 0 :accessor regiment-crouch :type fixnum
           :documentation "Feu par rang : premiers rangs accroupis après avoir tiré.")
   (volley-timer :initform 0 :accessor regiment-volley-timer :type fixnum
                 :documentation "Ticks avant que le premier rang puisse tirer sa salve
(contremarche, shooting.lisp).")
   (countermarch-timer :initform -1 :accessor regiment-countermarch-timer :type fixnum
                       :documentation "Ticks avant la fin de la salve : le rang qui vient de
tirer passe à l'arrière (contremarche), ou tous se relèvent (feu par rang) ; -1
sans salve en cours.")
   (fire-tick :initform -1000000 :accessor regiment-fire-tick :type fixnum
              :documentation "Dernier tick où il a tiré ou essuyé un tir (shooting.lisp) :
le moral ne remonte pas sous le feu.")
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
          (regiment-chasing-p regiment) nil
          (regiment-reform-pending-p regiment) t
          (regiment-slots-dirty-p regiment) t))
  regiment)

;;; Systèmes

(defun update-regiment-slots (regiment soldiers)
  "Recalcule les emplacements si la formation a changé : chaque soldat à son
rang et sa file."
  (when (regiment-slots-dirty-p regiment)
    (place-formation (regiment-formation regiment) (regiment-alive regiment)
                     (soldiers-slot-x soldiers) (soldiers-slot-y soldiers)
                     (regiment-start regiment)
                     (soldiers-rank soldiers) (soldiers-file soldiers))
    (when (eq (first (regiment-orders regiment)) :rout)
      (scatter-slots regiment soldiers))
    (setf (regiment-slots-dirty-p regiment) nil)))

(defparameter *hold-margin* 0.25f0
  "Marge ajoutée à la portée minimale de l'arme principale : on ne s'arrête jamais
plus près.")

(defparameter *hold-short-of-reach* 0.5f0
  "Le soldat s'arrête à cette distance en deçà de la portée de son arme principale.")

(defun regiment-hold-distance (regiment)
  "Distance de bord à bord où un soldat s'arrête devant sa cible : 0,5 m en
deçà de la portée de son arme principale de mêlée, et jamais plus près que sa
portée minimale. Avec une pique (portée 4,5 m), les premiers rangs s'arrêtent
à 4 m l'un de l'autre : le premier rang frappe, le deuxième, un mètre plus
loin, est hors de portée."
  (let ((primary (stats-primary (regiment-stats regiment))))
    (if (eq (attack-kind primary) :melee)
        (max (+ *hold-margin* (attack-min-reach primary))
             (- (attack-reach primary) *hold-short-of-reach*))
        *hold-margin*)))

(defparameter *bearer-rank* 3
  "Rang du porteur d'étendard, compté depuis 0 au front : le quatrième, hors de
portée des piques ennemies.")

;;; Rangs et files
;;;
;;; Chaque soldat a un rang et une file (SOLDIERS-RANK, SOLDIERS-FILE). La
;;; plage du régiment reste rangée par rang puis par file : le premier rang est
;;; en tête de plage. Au combat, un soldat change de rang (sa file avance quand
;;; un homme tombe devant, ou tourne en contremarche) mais jamais de file ; une
;;; file vide laisse un trou. Le régiment se reforme en rangs pleins, le dernier
;;; centré, quand il reçoit un ordre ou après *REFORM-SECONDS* de calme.

(defparameter *reform-seconds* 2
  "Secondes sans mêlée ni tir avant qu'un régiment bouche ses trous.")

(defun dense-place (k alive files)
  "Rang et file du soldat n° K d'une formation de ALIVE soldats en rangs pleins
sur FILES files, le dernier rang centré."
  (let* ((depth (ceiling alive files))
         (last-size (- alive (* (1- depth) files))))
    (multiple-value-bind (rank file) (floor k files)
      (values rank (if (= rank (1- depth))
                       (+ file (floor (- files last-size) 2))
                       file)))))

(defun refresh-layout (regiment soldiers)
  "Recalcule la profondeur et le porteur d'étendard de REGIMENT après un
changement de rangs : le porteur est au milieu du rang *BEARER-RANK*, ou du
dernier s'il en reste moins ; -1 pour un homme seul, qui se bat. Il ne frappe
pas ; quand il meurt ou change de rang, l'homme du milieu prend l'étendard."
  (let ((alive (regiment-alive regiment))
        (start (regiment-start regiment))
        (ranks (soldiers-rank soldiers)))
    (setf (regiment-depth regiment) (if (plusp alive) (1+ (aref ranks (+ start alive -1))) 0)
          (regiment-bearer regiment)
          (if (> alive 1)
              (let* ((rank (min *bearer-rank* (1- (regiment-depth regiment))))
                     (first (loop for i from start below (+ start alive)
                                  when (>= (aref ranks i) rank) return i))
                     (count (loop for i from first below (+ start alive)
                                  while (= (aref ranks i) rank) count t)))
                (+ first (floor count 2)))
              -1)
          (regiment-slots-dirty-p regiment) t)))

(defun front-rank-end (regiment soldiers)
  "Fin (exclue) du premier rang dans la plage de REGIMENT : il est en tête."
  (let ((ranks (soldiers-rank soldiers)))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          while (zerop (aref ranks i))
          finally (return i))))

(defun rank-count (regiment soldiers low high)
  "Soldats de REGIMENT dont le rang est entre LOW (compris) et HIGH (exclu)."
  (let ((ranks (soldiers-rank soldiers)))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          count (<= low (aref ranks i) (1- high)))))

(defun layout-regiment (regiment soldiers)
  "Reforme REGIMENT en rangs pleins, dans l'ordre de sa plage : files fixées (ou
moins s'il manque d'hommes), dernier rang centré. Les soldats peuvent changer
de file."
  (let* ((formation (regiment-formation regiment))
         (alive (regiment-alive regiment))
         (start (regiment-start regiment))
         (files (formation-dense-files formation (max 1 alive))))
    (setf (formation-layout-files formation) files)
    (dotimes (k alive)
      (multiple-value-bind (rank file) (dense-place k alive files)
        (setf (aref (soldiers-rank soldiers) (+ start k)) rank
              (aref (soldiers-file soldiers) (+ start k)) file)))
    (refresh-layout regiment soldiers)))

(defun layout-dense-p (regiment soldiers)
  "Vrai si REGIMENT est déjà en rangs pleins, sans trou."
  (let* ((formation (regiment-formation regiment))
         (alive (regiment-alive regiment))
         (start (regiment-start regiment))
         (files (formation-dense-files formation (max 1 alive))))
    (and (eql files (formation-layout-files formation))
         (loop for k below alive
               always (multiple-value-bind (rank file) (dense-place k alive files)
                        (and (= rank (aref (soldiers-rank soldiers) (+ start k)))
                             (= file (aref (soldiers-file soldiers) (+ start k)))))))))

(defun update-reform (regiment soldiers tick)
  "Reforme REGIMENT, en début de tick, après un ordre ou *REFORM-SECONDS* sans
mêlée, sans tir et sans ennemi proche (REGIMENT-ALERT-P) ; pas en débandade, ni
pendant une contremarche."
  ;; Occupé : en mêlée, sous le feu ou tirant, ou l'ennemi proche (à portée
  ;; de tir, ou à moins de *ALERT-DISTANCE* : entre deux salves aussi).
  (when (or (plusp (regiment-engaged regiment))
            (regiment-alert-p regiment)
            (< (- tick (regiment-fire-tick regiment)) (* 2 +ticks-per-second+)))
    (setf (regiment-busy-tick regiment) tick))
  (when (and (plusp (regiment-alive regiment))
             (not (eq (first (regiment-orders regiment)) :rout))
             (or (regiment-reform-pending-p regiment)
                 (and (zerop (mod (+ tick (regiment-index regiment)) 15))
                      (>= (- tick (regiment-busy-tick regiment))
                          (* *reform-seconds* +ticks-per-second+))))
             (loop for i from (regiment-start regiment) below (regiment-end regiment)
                   always (zerop (aref (soldiers-march soldiers) i))))
    (let ((ordered (regiment-reform-pending-p regiment)))
      (setf (regiment-reform-pending-p regiment) nil)
      (unless (layout-dense-p regiment soldiers)
        (let ((depth (regiment-depth regiment)))
          (layout-regiment regiment soldiers)
          ;; Au calme, le premier rang reste où il est ; un ordre, lui, vient
          ;; de placer la formation.
          (unless ordered (keep-front regiment depth)))))))

(defun move-regiment (regiment soldiers)
  "Avance les soldats de REGIMENT vers leur emplacement, au pas de marche."
  (let ((stats (regiment-stats regiment)))
    (move-toward-slots soldiers (regiment-start regiment) (regiment-end regiment)
                       ;; Un régiment débandé court, et qui le poursuit ou se
                       ;; replie en tirailleurs aussi (stances.lisp).
                       (/ (if (or (member (first (regiment-orders regiment)) '(:rout :retreat))
                                  (regiment-chasing-p regiment))
                              (stats-run-speed stats)
                              (stats-speed stats))
                          #.(float +ticks-per-second+ 1f0))
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
                     (:rout (ht "kind" "rout"))
                     (:retreat (ht "kind" "retreat" "x" (json-float (second orders))
                                   "y" (json-float (third orders))))
                     (t nil))
          "stance" (string-downcase (regiment-stance regiment))
          "chasing" (if (regiment-chasing-p regiment) t 'yason:false)
          "engaged" (regiment-engaged regiment)
          "flank_attackers" (regiment-flank-attackers regiment)
          "rear_attackers" (regiment-rear-attackers regiment)
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
