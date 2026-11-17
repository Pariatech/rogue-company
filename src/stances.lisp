(in-package #:rogue-company)

;;; Postures des régiments (REGIMENT-STANCE) : une conduite que le régiment
;;; garde d'un ordre à l'autre, à côté de ses ordres.
;;;
;;; :HOLD, par défaut : il charge, et s'arrête quand son ennemi se débande.
;;; :CHASE : quand son ennemi se débande, il le poursuit au pas de course, rangs
;;;   rompus (chacun court après un fuyard, LOOSEN-FORMATION), jusqu'à
;;;   *CHASE-DISTANCE* de là où la poursuite a commencé ou jusqu'au dernier
;;;   fuyard, puis se reforme sur place. Seulement pour un régiment de mêlée :
;;;   des tireurs s'arrêtent.
;;; :SKIRMISH : quand un régiment ennemi de mêlée, ni débandé ni déjà au contact
;;;   d'un autre, arrive à moins de *SKIRMISH-TRIGGER* mètres, le régiment se
;;;   replie de *SKIRMISH-DISTANCE* mètres à l'opposé de lui (ordre :RETREAT),
;;;   en courant, dos à lui, puis lui refait face et reprend son ordre
;;;   d'attaque ; plus tôt si aucune mêlée ennemie libre n'est plus à moins de
;;;   *SKIRMISH-CLEAR*, une fois sorti des rangs amis qu'il traverse. Rattrapé au contact, il cesse de se replier et se bat.
;;;   Pour passer derrière ses piques, les deux régiments ouvrent leurs files
;;;   (open-files.lisp).

(defparameter *chase-distance* 100f0
  "Distance maximale d'une poursuite, en mètres, depuis le centre du régiment
au début de la poursuite.")

(defparameter *skirmish-trigger* 20f0
  "Distance, de soldat à soldat, sous laquelle une mêlée ennemie qui approche
fait replier un régiment en tirailleurs.")

(defparameter *skirmish-distance* 30f0 "Longueur d'un repli, en mètres.")

(defparameter *skirmish-clear* 40f0
  "Le repli s'arrête avant sa fin quand plus aucune mêlée ennemie libre (ni
débandée ni au contact) n'est à moins de cette distance : l'ennemi s'est
accroché aux piques, les tireurs se retournent et tirent sur la mêlée.")

(defparameter *skirmish-arrived* 1f0
  "Le repli est fini quand les soldats sont en moyenne à moins de cette distance
de leur emplacement, en mètres.")

(defparameter *skirmish-timeout* 1.5f0
  "Un repli bloqué s'arrête après ce multiple du temps qu'il devrait prendre.")

;;; Poursuite

(defun update-chase (battle regiment enemy)
  "Poursuite de ENEMY, débandé, par REGIMENT sous la posture :CHASE."
  (let ((soldiers (battle-soldiers battle))
        (formation (regiment-formation regiment)))
    (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
      (unless (regiment-chasing-p regiment)
        (setf (regiment-chasing-p regiment) t
              (regiment-holding-p regiment) nil
              (regiment-chase-x regiment) x
              (regiment-chase-y regiment) y))
      (if (> (+ (expt (- x (regiment-chase-x regiment)) 2)
                (expt (- y (regiment-chase-y regiment)) 2))
             (expt *chase-distance* 2))
          (end-chase regiment soldiers)
          (when (zerop (mod (+ (battle-tick-count battle) (regiment-index regiment))
                            *advance-period*))
            ;; La formation suit les fuyards, front tourné vers eux : les soldats
            ;; sans fuyard proche courent vers leur boîte (LOOSEN-FORMATION).
            (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
              (setf (formation-x formation) ex
                    (formation-y formation) ey
                    (formation-facing formation) (float (atan (- ey y) (- ex x)) 1f0)
                    (regiment-slots-dirty-p regiment) t)))))))

(defun end-chase (regiment soldiers)
  "Fin de l'attaque de REGIMENT, ennemi détruit ou débandé : il reste où il est.
Après une poursuite, rangs rompus, il se reforme autour de son centre."
  (setf (regiment-orders regiment) nil
        (regiment-holding-p regiment) nil)
  (if (regiment-chasing-p regiment)
      (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
        (let ((formation (regiment-formation regiment)))
          (setf (regiment-chasing-p regiment) nil
                (formation-x formation) x
                (formation-y formation) y
                (regiment-slots-dirty-p regiment) t)))
      (hold-in-place regiment soldiers)))

;;; Repli des tirailleurs

(defun regiment-gap (a b soldiers limit)
  "Plus petite distance entre un soldat de A et un soldat de B, ou LIMIT si
leurs boîtes sont à LIMIT ou plus."
  (if (>= (box-gap a b) limit)
      limit
      (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
            (best (* limit limit)))
        (loop for i from (regiment-start a) below (regiment-end a)
              do (let ((xi (aref xs i)) (yi (aref ys i)))
                   (loop for j from (regiment-start b) below (regiment-end b)
                         do (let ((dx (- (aref xs j) xi)) (dy (- (aref ys j) yi)))
                              (setf best (min best (+ (* dx dx) (* dy dy))))))))
        (sqrt best))))

(defun skirmish-threat (battle regiment &optional (distance *skirmish-trigger*))
  "Régiment ennemi de mêlée qui menace REGIMENT : vivant, ni débandé ni au
contact, le plus proche à moins de DISTANCE. NIL sinon."
  (let ((soldiers (battle-soldiers battle))
        (threat nil) (best (float distance 1f0)))
    (loop for other across (battle-regiments battle)
          when (and (/= (regiment-side other) (regiment-side regiment))
                    (plusp (regiment-alive other))
                    (not (regiment-routed-p other))
                    (zerop (regiment-engaged other))
                    (eq (attack-kind (stats-primary (regiment-stats other))) :melee))
            do (let ((gap (regiment-gap regiment other soldiers best)))
                 (when (< gap best) (setf best gap threat other))))
    threat))

(defun start-retreat (regiment soldiers threat)
  "Replie REGIMENT de *SKIRMISH-DISTANCE* à l'opposé de THREAT, front vers lui."
  (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
    (multiple-value-bind (ex ey) (regiment-centroid threat soldiers)
      (let* ((facing (float (atan (- ey y) (- ex x)) 1f0))
             (tx (- x (* *skirmish-distance* (cos facing))))
             (ty (- y (* *skirmish-distance* (sin facing))))
             (formation (regiment-formation regiment))
             (orders (regiment-orders regiment)))
        (setf (regiment-resume-orders regiment) (and (eq (first orders) :attack) orders)
              (regiment-orders regiment) (list :retreat tx ty facing)
              (regiment-retreat-ticks regiment) 0
              (regiment-holding-p regiment) nil
              (formation-x formation) tx
              (formation-y formation) ty
              (formation-facing formation) facing
              (regiment-slots-dirty-p regiment) t)))))

(defun end-retreat (regiment)
  "Fin du repli : REGIMENT reprend son ordre d'attaque, s'il en avait un."
  (setf (regiment-orders regiment) (regiment-resume-orders regiment)
        (regiment-resume-orders regiment) nil
        (regiment-holding-p regiment) nil
        (regiment-contact-lost regiment) 0))

(defun update-skirmish (battle regiment)
  "Posture :SKIRMISH : déclenche, suit et termine le repli de REGIMENT."
  (when (and (eq (regiment-stance regiment) :skirmish)
             (plusp (regiment-alive regiment))
             (not (regiment-routed-p regiment)))
    (let ((soldiers (battle-soldiers battle)))
      (if (eq (first (regiment-orders regiment)) :retreat)
          (let* ((files (formation-files (regiment-formation regiment) (regiment-alive regiment)))
                 (period (zerop (mod (+ (battle-tick-count battle) (regiment-index regiment))
                                     *advance-period*)))
                 (limit (* *skirmish-timeout* +ticks-per-second+
                           (/ *skirmish-distance* (stats-run-speed (regiment-stats regiment))))))
            (when (or (>= (regiment-engaged regiment) (max 1 (floor (* *hold-fraction* files))))
                      (> (incf (regiment-retreat-ticks regiment)) limit)
                      (< (regiment-slot-distances regiment soldiers) *skirmish-arrived*)
                      ;; Jamais au milieu d'un régiment ami qu'il traverse.
                      (and period (null (skirmish-threat battle regiment *skirmish-clear*))
                           (not (friend-near-p battle regiment))))
              (end-retreat regiment)))
          (when (and (zerop (regiment-engaged regiment))
                     (zerop (mod (+ (battle-tick-count battle) (regiment-index regiment))
                                 *advance-period*)))
            (let ((threat (skirmish-threat battle regiment)))
              (when threat (start-retreat regiment soldiers threat))))))))
