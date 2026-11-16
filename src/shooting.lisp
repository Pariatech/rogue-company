(in-package #:rogue-company)

;;; Tir : arquebuses et mousquets, sans projectiles simulés. À chaque tick,
;;; chaque soldat d'un régiment de tireurs décide s'il tire : arme de tir en
;;; main et chargée, dans les *FIRING-RANKS* premiers rangs (à genoux, courbé,
;;; debout), un régiment ennemi à portée devant sa formation. Le coup vise un
;;; soldat tiré au hasard dans les deux premiers rangs de ce régiment, les plus
;;; exposés ; la chance de toucher baisse avec la distance et la couverture de
;;; la cible. Les tireurs partent chargés : la première décharge est une salve,
;;; et les suivantes, au rechargement près, aussi.
;;;
;;; Au contact, le soldat prend son arme secondaire (ACQUIRE-TARGETS,
;;; combat.lisp) et ne tire plus ; il ne recharge pas non plus. Il reprend son
;;; arme de tir, toujours chargée s'il l'était, après un moment sans ennemi.
;;;
;;; Sous un ordre d'attaque, un régiment de tireurs avance jusqu'à
;;; *FIRE-DISTANCE* de sa portée, s'arrête, fait face à l'ennemi et tire. Il
;;; reprend son avance si l'ennemi s'éloigne au-delà de *FIRE-RESUME*.
;;;
;;; Pas encore : contremarche, ligne de tir (alliés ou relief entre les tireurs
;;; et leur cible), tir ami, fumée.

(defparameter *firing-ranks* 3 "Rangs qui tirent, depuis le premier.")

(defparameter *fire-distance* 0.5f0
  "Part de sa portée à laquelle un régiment de tireurs s'arrête pour tirer.")

(defparameter *fire-resume* 0.9f0
  "Part de sa portée au-delà de laquelle un régiment de tireurs arrêté reprend son avance.")

(defparameter *fire-cos* 0.5f0
  "Cosinus de l'angle, depuis le front de la formation, sous lequel les tireurs
voient un régiment ennemi (60°).")

(defparameter *range-falloff* 0.3f0
  "Chance de toucher perdue à portée maximale ; proportionnelle en deçà.")

(defparameter *reload-jitter* 0.2f0
  "Écart du rechargement, en part de sa durée : les salves se défont peu à peu.")

(defparameter *fire-turn-cos* 0.94f0
  "Un régiment de tireurs arrêté se tourne vers l'ennemi quand celui-ci sort de
cet angle autour de son front (20°).")

(defun ranged-regiment-p (regiment)
  (eq (attack-kind (stats-primary (regiment-stats regiment))) :ranged))

(defun box-gap (a b)
  "Distance entre les boîtes de deux régiments, 0 si elles se chevauchent."
  (let ((dx (max 0f0 (- (regiment-min-x b) (regiment-max-x a))
                 (- (regiment-min-x a) (regiment-max-x b))))
        (dy (max 0f0 (- (regiment-min-y b) (regiment-max-y a))
                 (- (regiment-min-y a) (regiment-max-y b)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun update-fire-position (battle regiment)
  "Engagement d'un régiment de tireurs sous un ordre d'attaque : avance jusqu'à
portée de tir, puis arrêt face à l'ennemi. Renvoie vrai s'il s'en est chargé,
NIL pour un régiment de mêlée (UPDATE-ENGAGEMENT fait alors le reste)."
  (when (ranged-regiment-p regiment)
    (let* ((soldiers (battle-soldiers battle))
           (enemy (aref (battle-regiments battle) (second (regiment-orders regiment))))
           (formation (regiment-formation regiment))
           (range (attack-reach (stats-primary (regiment-stats regiment))))
           (gap (box-gap regiment enemy))
           (period (zerop (mod (+ (battle-tick-count battle) (regiment-index regiment))
                               *advance-period*))))
      (cond ((or (zerop (regiment-alive enemy)) (regiment-routed-p enemy))
             (setf (regiment-orders regiment) nil
                   (regiment-holding-p regiment) nil)
             (hold-in-place regiment soldiers))
            ((regiment-holding-p regiment)
             (cond ((> gap (* *fire-resume* range))
                    (setf (regiment-holding-p regiment) nil))
                   (period
                    ;; Face à l'ennemi, sans bouger son centre.
                    (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
                      (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
                        (let* ((facing (formation-facing formation))
                               (dx (- ex x)) (dy (- ey y))
                               (length (max 1f-3 (sqrt (+ (* dx dx) (* dy dy))))))
                          (when (< (/ (+ (* dx (cos facing)) (* dy (sin facing))) length)
                                   *fire-turn-cos*)
                            (setf (formation-facing formation) (float (atan dy dx) 1f0)
                                  (regiment-slots-dirty-p regiment) t))))))))
            ((<= gap (* *fire-distance* range))
             (setf (regiment-holding-p regiment) t)
             (hold-in-place regiment soldiers))
            (period
             (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
               (setf (formation-x formation) ex
                     (formation-y formation) ey
                     (formation-facing formation) (attack-facing regiment enemy soldiers)
                     (regiment-slots-dirty-p regiment) t))))
      t)))

(defun fire-target (battle regiment range)
  "Régiment ennemi le plus proche que REGIMENT voit devant lui à portée, ou NIL."
  (let* ((soldiers (battle-soldiers battle))
         (facing (formation-facing (regiment-formation regiment)))
         (best nil) (best-gap range))
    (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
      (loop for other across (battle-regiments battle)
            do (when (and (/= (regiment-side other) (regiment-side regiment))
                          (plusp (regiment-alive other))
                          (not (regiment-routed-p other)))
                 (let ((gap (box-gap regiment other)))
                   (when (<= gap best-gap)
                     (multiple-value-bind (ex ey) (regiment-centroid other soldiers)
                       (let* ((dx (- ex x)) (dy (- ey y))
                              (length (max 1f-3 (sqrt (+ (* dx dx) (* dy dy))))))
                         (when (>= (/ (+ (* dx (cos facing)) (* dy (sin facing))) length)
                                   *fire-cos*)
                           (setf best other best-gap gap)))))))))
    best))

(defun fire-shots (battle)
  "Tirs du tick, régiment par régiment, dans l'ordre des indices, avec le
générateur de la bataille : la bataille se rejoue. Écrit les dégâts comme
STRIKE, appliqués ensuite par APPLY-DAMAGE."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((soldiers (battle-soldiers battle))
         (regiments (battle-regiments battle))
         (rng (battle-rng battle))
         (tick (battle-tick-count battle))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (weapons (soldiers-weapon soldiers))
         (switch-timers (soldiers-switch-timer soldiers))
         (reloads (soldiers-reload soldiers))
         (stuns (soldiers-stun soldiers))
         (strike-ticks (soldiers-strike-tick soldiers))
         (damages (soldiers-damage soldiers))
         (last-attackers (soldiers-last-attacker soldiers))
         (firing-ranks *firing-ranks*)
         (falloff *range-falloff*)
         (jitter *reload-jitter*)
         (min-chance *min-hit-chance*)
         (max-chance *max-hit-chance*)
         (stun-rate (* (the single-float *stun-seconds-per-damage*)
                       #.(float +ticks-per-second+ 1f0))))
    (declare (type vector regiments)
             (type fixnum tick firing-ranks)
             (type float-array xs ys damages)
             (type fixnum-array weapons switch-timers reloads stuns strike-ticks last-attackers)
             (type single-float falloff jitter min-chance max-chance stun-rate))
    (loop for regiment across regiments
          do (when (and (ranged-regiment-p regiment)
                        (plusp (regiment-alive regiment))
                        (not (regiment-routed-p regiment)))
               (let* ((stats (regiment-stats regiment))
                      (attack (stats-primary stats))
                      (range (attack-reach attack))
                      (reload (attack-cooldown attack))
                      (skill (+ (the single-float (stats-marksmanship stats))
                                (the single-float (attack-accuracy attack))))
                      (start (regiment-start regiment))
                      (files (max 1 (formation-files (regiment-formation regiment)
                                                     (regiment-alive regiment))))
                      (bearer (regiment-bearer regiment))
                      (index (regiment-index regiment))
                      (enemy (fire-target battle regiment range)))
                 (declare (type single-float range skill) (type fixnum reload start files bearer index))
                 (loop for i of-type fixnum from start below (regiment-end regiment)
                       do (when (= 0 (aref weapons i))
                            (when (plusp (aref reloads i))
                              (decf (aref reloads i)))
                            (when (and enemy
                                       (< (floor (- i start) files) firing-ranks)
                                       (/= i bearer)
                                       (zerop (aref reloads i))
                                       (zerop (aref switch-timers i))
                                       (zerop (aref stuns i)))
                              (let* ((enemy-start (regiment-start enemy))
                                     (enemy-files (max 1 (formation-files
                                                          (regiment-formation enemy)
                                                          (regiment-alive enemy))))
                                     (exposed (min (regiment-alive enemy) (* 2 enemy-files)))
                                     (j (+ enemy-start (rng-below rng exposed)))
                                     (dx (- (aref xs j) (aref xs i)))
                                     (dy (- (aref ys j) (aref ys i)))
                                     (distance (sqrt (+ (* dx dx) (* dy dy)))))
                                (declare (type fixnum enemy-start enemy-files exposed j)
                                         (type single-float dx dy distance))
                                ;; Hors de portée : il garde sa charge.
                                (when (<= distance range)
                                  (let* ((defender (regiment-stats enemy))
                                         (chance (max min-chance
                                                      (min max-chance
                                                           (- skill
                                                              (* falloff (/ distance range))
                                                              (the single-float
                                                                   (stats-cover defender)))))))
                                    (declare (type single-float chance))
                                    (setf (aref reloads i)
                                          (+ reload (rng-below rng (max 1 (round (* jitter reload)))))
                                          (aref strike-ticks i) tick
                                          (regiment-fire-tick regiment) tick
                                          (regiment-fire-tick enemy) tick)
                                    (when (< (rng-float rng) chance)
                                      (let ((damage (+ (max 0f0 (- (the single-float (attack-damage attack))
                                                                   (the single-float (stats-armor defender))))
                                                       (the single-float (attack-piercing attack)))))
                                        (incf (aref damages j) damage)
                                        (setf (aref last-attackers j) index
                                              (aref stuns j)
                                              (max (aref stuns j)
                                                   (the fixnum (round (* damage stun-rate))))))))))))))))))
