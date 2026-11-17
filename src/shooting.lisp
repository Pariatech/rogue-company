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
;;; *FIRE-DISTANCE* de sa portée, s'arrête, fait face à l'ennemi et tire ; il ne
;;; tire pas en marche, et chaque soldat ne tire qu'immobile (*STILL-STEP*). Il
;;; reprend son avance si l'ennemi s'éloigne au-delà de *FIRE-RESUME*.
;;;
;;; Contremarche : un régiment armé pour la salve (:volley, arquebuse ou
;;; mousquet), arrêté et sans soldat en mêlée, ne tire que de son premier rang,
;;; en salve, quand au moins *VOLLEY-READY* de ce rang est chargé. Après
;;; *COUNTERMARCH-DELAY*, ce rang passe à l'arrière (COUNTERMARCH : les autres
;;; avancent d'un rang) et recharge pendant que les suivants tirent à leur tour,
;;; *VOLLEY-INTERVAL* plus tard. Chaque salve ébranle le moral de sa cible
;;; (*VOLLEY-SHOCK*). En marche ou au contact, feu à volonté des
;;; *FIRING-RANKS* premiers rangs.
;;;
;;; Feu par rang (:FIRE-BY-RANK), l'autre exercice : le premier rang tire puis
;;; s'accroupit, le deuxième tire par-dessus puis s'accroupit, le troisième tire
;;; debout, à *RANK-FIRE-INTERVAL* d'écart, chacun quand il est prêt. Puis tous
;;; se relèvent et rechargent sur place. Trois salves rapprochées, puis le
;;; silence du rechargement ; les rangs au-delà du troisième ne tirent pas.
;;;
;;; Feu à volonté (:AT-WILL), l'exercice de base : le premier rang tire, chacun
;;; dès qu'il est chargé et immobile, sans attendre ses voisins. C'est aussi le
;;; feu de tout régiment de tireurs en marche ou au contact.
;;;
;;; Salve (:VOLLEY) : les *VOLLEY-RANKS* premiers rangs
;;; tirent ensemble, le premier accroupi, le deuxième debout par-dessus, puis
;;; rechargent sur place. Le régiment se range sur ces seuls rangs (ADD-REGIMENT).
;;;
;;; Salvée (:SALVO), la salve suédoise : les *RANK-FIRE-RANKS* (3) premiers
;;; rangs tirent ensemble, le premier à genoux, le deuxième courbé, le troisième
;;; debout, puis rechargent sur place. Le choc d'une salve sur le moral de sa
;;; cible croît avec le nombre de balles : la salvée frappe trois fois plus fort
;;; qu'un rang seul, puis laisse un long silence.
;;;
;;; L'exercice d'un régiment est REGIMENT-DRILL (ADD-REGIMENT, :DRILL).
;;;
;;; Pas encore : ligne de tir (alliés ou relief entre les tireurs et leur
;;; cible), tir ami, fumée qui gêne la visée.

(defparameter *firing-ranks* 1
  "Feu à volonté (exercice de base, et au contact pour tous) : rangs qui tirent,
depuis le premier ; chacun dès qu'il est chargé, sans attendre ses voisins.")

(defparameter *volley-ready* 0.9f0
  "Contremarche : part du premier rang qui doit être chargée et en place
(immobile) pour tirer la salve : le rang tire ensemble.")

(defparameter *volley-interval* 1.5f0
  "Contremarche : secondes entre le passage d'un rang à l'arrière et la salve du suivant.")

(defparameter *rank-fire-interval* 0.8f0
  "Feu par rang : secondes entre la salve d'un rang et celle du suivant, et entre
la dernière salve et le moment où tous se relèvent.")

(defparameter *crouch-height* 0.35f0
  "Feu par rang : un soldat accroupi est plus bas d'autant, en mètres (dessin).")

(defparameter *countermarch-delay* 0.4f0
  "Contremarche : secondes entre la salve et le départ du rang vers l'arrière.")

(defparameter *march-reached* 0.25f0
  "Contremarche : distance, en mètres, à laquelle un soldat a atteint une étape
de son chemin.")

(defparameter *volley-shock* 2f0
  "Moral perdu par la cible d'une salve, par rang complet de balles tirées
(proportionnel au nombre de balles : deux rangs ensemble, le double).")

(defparameter *fire-distance* 0.75f0
  "Part de sa portée à laquelle un régiment de tireurs s'arrête pour tirer.")

(defparameter *fire-resume* 0.95f0
  "Part de sa portée au-delà de laquelle un régiment de tireurs arrêté reprend son avance.")

(defparameter *fire-cos* 0.5f0
  "Cosinus de l'angle, depuis le front de la formation, sous lequel les tireurs
voient un régiment ennemi (60°).")

(defparameter *still-step* 0.02f0
  "Un soldat qui a bougé de plus de cette distance, en mètres, pendant le tick ne
tire pas : on tire immobile.")

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
  "Distance entre les boîtes de deux régiments, 0 si elles se chevauchent. Les
boîtes suivent les axes : en diagonale, elles débordent largement des
formations, et cette distance sous-estime l'écart réel. Sert seulement de
premier tri (FIRE-DISTANCE ne peut pas être plus petite)."
  (let ((dx (max 0f0 (- (regiment-min-x b) (regiment-max-x a))
                 (- (regiment-min-x a) (regiment-max-x b))))
        (dy (max 0f0 (- (regiment-min-y b) (regiment-max-y a))
                 (- (regiment-min-y a) (regiment-max-y b)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun swap-soldiers (soldiers i j)
  "Échange les soldats I et J dans tous les tableaux."
  (dolist (slot *soldier-float-arrays*)
    (let ((array (slot-value soldiers slot)))
      (rotatef (aref array i) (aref array j))))
  (dolist (slot *soldier-fixnum-arrays*)
    (let ((array (slot-value soldiers slot)))
      (rotatef (aref array i) (aref array j)))))

(defun countermarch (regiment soldiers)
  "Dans chaque file de REGIMENT, l'homme du premier rang passe en queue de sa
file, les autres avancent d'un rang : chaque colonne d'indices (F, F + FILES,
F + 2 FILES…) tourne d'un cran (le soldat n° k occupe l'emplacement n° k).
Chacun reste dans sa file, même avec un dernier rang incomplet. Une file dont
l'homme de tête n'a pas tiré (encore chargé : il vient de remplacer un mort et
n'était pas en place pour la salve) ne tourne pas : il tirera à la suivante. Le porteur
d'étendard garde sa place. À faire en début de tick, avant le ciblage : les
cibles et les dégâts du tick ne visent pas encore d'indice."
  (let* ((start (regiment-start regiment))
         (end (regiment-end regiment))
         (alive (regiment-alive regiment))
         (files (formation-files (regiment-formation regiment) (max 1 alive)))
         (march (soldiers-march soldiers))
         (reloads (soldiers-reload soldiers)))
    (when (> alive files)
      (let ((bearer (regiment-bearer regiment))
            ;; Files dont l'homme de tête a tiré.
            (fired (make-array files :element-type 'bit)))
        (loop for f from 0 below files
              do (setf (sbit fired f) (if (plusp (aref reloads (+ start f))) 1 0)))
        (dolist (slot (append *soldier-float-arrays* *soldier-fixnum-arrays*))
          (let ((array (slot-value soldiers slot)))
            (loop for f from 0 below files
                  for first = (+ start f)
                  when (= 1 (sbit fired f))
                    do (let ((front (aref array first))
                             (k first))
                         (loop while (< (+ k files) end)
                               do (setf (aref array k) (aref array (+ k files)))
                                  (incf k files))
                         (setf (aref array k) front)))))
        ;; La queue de chaque file qui a tourné : l'homme qui vient de tirer,
        ;; qui gagne l'arrière par l'intervalle à droite de sa file
        ;; (STEER-COUNTERMARCH).
        (loop for f from 0 below files
              for first = (+ start f)
              when (= 1 (sbit fired f))
                do (let ((k first))
                     (loop while (< (+ k files) end) do (incf k files))
                     (when (> k first) (setf (aref march k) 1))))
        ;; Le porteur a avancé d'un rang avec sa file : il reprend sa place.
        (when (and (>= bearer 0) (>= (- bearer files) start)
                   (= 1 (sbit fired (mod (- bearer start) files))))
          (swap-soldiers soldiers bearer (- bearer files))))
      (setf (regiment-slots-dirty-p regiment) t))))

(defun steer-countermarch (regiment soldiers)
  "Chemin des soldats en contremarche, à chaque tick après les emplacements :
un pas de côté dans l'intervalle à droite de leur file, à hauteur du premier
rang ; la descente de l'intervalle jusqu'à hauteur de leur place ; puis leur
place. Leur emplacement devient l'étape en cours."
  (let ((march (soldiers-march soldiers))
        (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
        (slot-xs (soldiers-slot-x soldiers)) (slot-ys (soldiers-slot-y soldiers))
        (start (regiment-start regiment))
        (alive (regiment-alive regiment))
        (formation (regiment-formation regiment)))
    (loop for i from start below (regiment-end regiment)
          do (when (plusp (aref march i))
               (let* ((k (- i start))
                      (files (formation-files formation alive))
                      (half (* 0.5f0 (formation-lateral-spacing formation))))
                 (multiple-value-bind (fx fy right-x right-y) (formation-slot formation alive k)
                   (multiple-value-bind (ax ay)
                       (if (= 1 (aref march i))
                           ;; Hauteur du premier rang, dans la même file.
                           (formation-slot formation alive (mod k files))
                           (values fx fy))
                     (let ((wx (+ ax (* half right-x)))
                           (wy (+ ay (* half right-y))))
                       (if (< (+ (expt (- wx (aref xs i)) 2) (expt (- wy (aref ys i)) 2))
                              (expt *march-reached* 2))
                           ;; Étape atteinte : la suivante, ou sa place.
                           (if (= 1 (aref march i))
                               (setf (aref march i) 2)
                               (setf (aref march i) 0
                                     (aref slot-xs i) fx
                                     (aref slot-ys i) fy))
                           (setf (aref slot-xs i) wx
                                 (aref slot-ys i) wy))))))))))

(defun regiment-stoop (regiment)
  "Rang courbé (dessiné à mi-hauteur) : le deuxième à la salvée, ou -1."
  (if (and (eq (regiment-drill regiment) :salvo) (plusp (regiment-crouch regiment)))
      1
      -1))

(defun volley-fire-p (regiment)
  "Vrai si REGIMENT tire en salves, selon son exercice (REGIMENT-DRILL) ; faux au
feu à volonté (:AT-WILL)."
  (and (member (regiment-drill regiment) '(:volley :countermarch :fire-by-rank :salvo))
       (attack-volley-p (stats-primary (regiment-stats regiment)))
       (regiment-holding-p regiment)
       (zerop (regiment-engaged regiment))))

(defun update-countermarch (regiment soldiers)
  "Minuteries des salves, en début de tick : fin de salve (contremarche, ou tous
debout au feu par rang). Hors des salves (en marche, au contact), tous debout."
  (when (plusp (regiment-volley-timer regiment))
    (decf (regiment-volley-timer regiment)))
  ;; Rang à tirer disparu (pertes) : le cycle reprend au premier.
  (when (>= (regiment-volley-rank regiment)
            (ceiling (max 1 (regiment-alive regiment))
                     (formation-files (regiment-formation regiment)
                                      (max 1 (regiment-alive regiment)))))
    (setf (regiment-volley-rank regiment) 0
          (regiment-crouch regiment) 0))
  (if (volley-fire-p regiment)
      ;; Salve et salvée : le premier rang reste accroupi, les suivants tirent
      ;; par-dessus (à la salvée, le deuxième courbé : REGIMENT-STOOP).
      (when (member (regiment-drill regiment) '(:volley :salvo))
        (setf (regiment-crouch regiment) 1))
      (setf (regiment-crouch regiment) 0
            (regiment-volley-rank regiment) 0))
  (let ((timer (regiment-countermarch-timer regiment)))
    (cond ((> timer 0) (decf (regiment-countermarch-timer regiment)))
          ((= timer 0)
           (setf (regiment-countermarch-timer regiment) -1)
           (when (volley-fire-p regiment)
             (ecase (regiment-drill regiment)
               ((:volley :salvo))
               (:countermarch (countermarch regiment soldiers))
               (:fire-by-rank (setf (regiment-crouch regiment) 0
                                    (regiment-volley-rank regiment) 0))))))))

(defun fire-distance (regiment enemy soldiers)
  "Distance, en mètres, du centre du premier rang de REGIMENT au soldat vivant
le plus proche de ENEMY : l'écart que voient les tireurs, quelle que soit
l'orientation des deux formations."
  (let* ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (start (regiment-start regiment))
         (front (min (regiment-end regiment)
                     (+ start (formation-files (regiment-formation regiment)
                                               (max 1 (regiment-alive regiment))))))
         (n (max 1 (- front start)))
         (cx (/ (loop for i from start below front sum (aref xs i)) n))
         (cy (/ (loop for i from start below front sum (aref ys i)) n)))
    (sqrt (loop for j from (regiment-start enemy) below (regiment-end enemy)
                minimize (+ (expt (- (aref xs j) cx) 2) (expt (- (aref ys j) cy) 2))))))

(defun update-fire-position (battle regiment)
  "Engagement d'un régiment de tireurs sous un ordre d'attaque : avance jusqu'à
portée de tir, puis arrêt face à l'ennemi. Renvoie vrai s'il s'en est chargé,
NIL pour un régiment de mêlée (UPDATE-ENGAGEMENT fait alors le reste)."
  (when (ranged-regiment-p regiment)
    (update-countermarch regiment (battle-soldiers battle))
    (let* ((soldiers (battle-soldiers battle))
           (enemy (aref (battle-regiments battle) (second (regiment-orders regiment))))
           (formation (regiment-formation regiment))
           (range (attack-reach (stats-primary (regiment-stats regiment))))
           (gap (if (plusp (regiment-alive enemy))
                    (fire-distance regiment enemy soldiers)
                    0f0))
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
                 (let ((gap (if (<= (box-gap regiment other) best-gap)
                                (fire-distance regiment other soldiers)
                                most-positive-single-float)))
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
         (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
         (still2 (expt (the single-float *still-step*) 2))
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
             (type float-array xs ys prev-xs prev-ys damages)
             (type single-float still2)
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
                      ;; Sous un ordre d'attaque, on ne tire qu'arrêté à portée.
                      (enemy (and (or (not (eq (first (regiment-orders regiment)) :attack))
                                      (regiment-holding-p regiment))
                                  (fire-target battle regiment range)))
                      ;; En salves, un seul rang tire, ensemble, quand assez des
                      ;; siens sont chargés et en place : le premier (contremarche)
                      ;; ou celui dont c'est le tour (feu par rang).
                      (volley (volley-fire-p regiment))
                      (volley-rank (if (eq (regiment-drill regiment) :fire-by-rank)
                                       (regiment-volley-rank regiment)
                                       0))
                      ;; Rangs qui tirent ensemble : un, ou deux pour la salve.
                      (volley-depth (case (regiment-drill regiment)
                                      (:volley (the fixnum *volley-ranks*))
                                      (:salvo (the fixnum *rank-fire-ranks*))
                                      (t 1)))
                      (rank-start (+ start (* volley-rank files)))
                      (rank-end (min (+ rank-start (* volley-depth files)) (regiment-end regiment)))
                      (ready (and volley enemy
                                  (< rank-start rank-end)
                                  (zerop (regiment-volley-timer regiment))
                                  (minusp (regiment-countermarch-timer regiment))
                                  (>= (loop for k of-type fixnum from rank-start below rank-end
                                            count (and (zerop (aref reloads k))
                                                       (= 0 (aref weapons k))
                                                       (< (+ (expt (- (aref xs k) (aref prev-xs k)) 2)
                                                             (expt (- (aref ys k) (aref prev-ys k)) 2))
                                                          still2)))
                                      (* (the single-float *volley-ready*)
                                         (float (- rank-end rank-start) 1f0)))))
                      ;; Rangs qui tirent : de LOW à HIGH exclu.
                      (low (if volley volley-rank 0))
                      (high (cond ((not volley) firing-ranks)
                                  (ready (+ volley-rank volley-depth))
                                  (t volley-rank)))
                      (shots 0))
                 (declare (type single-float range skill)
                          (type fixnum reload start files bearer index volley-rank volley-depth rank-start
                                rank-end low high shots))
                 (loop for i of-type fixnum from start below (regiment-end regiment)
                       do (when (= 0 (aref weapons i))
                            (when (plusp (aref reloads i))
                              (decf (aref reloads i)))
                            (when (and enemy
                                       (<= low (floor (- i start) files))
                                       (< (floor (- i start) files) high)
                                       (/= i bearer)
                                       (zerop (aref reloads i))
                                       (zerop (aref switch-timers i))
                                       (zerop (aref stuns i))
                                       ;; Immobile.
                                       (< (+ (expt (- (aref xs i) (aref prev-xs i)) 2)
                                             (expt (- (aref ys i) (aref prev-ys i)) 2))
                                          still2))
                              (let* ((enemy-start (regiment-start enemy))
                                     (enemy-files (max 1 (formation-files
                                                          (regiment-formation enemy)
                                                          (regiment-alive enemy))))
                                     (exposed (min (regiment-alive enemy) (* 2 enemy-files)))
                                     (j enemy-start)
                                     (distance most-positive-single-float))
                                (declare (type fixnum enemy-start enemy-files exposed j)
                                         (type single-float distance))
                                ;; Un ennemi exposé à portée : jusqu'à trois tirages
                                ;; (de biais, les plus éloignés sont hors de portée).
                                (loop repeat 3
                                      do (let* ((k (+ enemy-start (rng-below rng exposed)))
                                                (dx (- (aref xs k) (aref xs i)))
                                                (dy (- (aref ys k) (aref ys i)))
                                                (d (sqrt (+ (* dx dx) (* dy dy)))))
                                           (declare (type fixnum k) (type single-float dx dy d))
                                           (when (< d distance) (setf j k distance d)))
                                      until (<= distance range))
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
                                    (incf shots)
                                    (when (< (rng-float rng) chance)
                                      (let ((damage (+ (max 0f0 (- (the single-float (attack-damage attack))
                                                                   (the single-float (stats-armor defender))))
                                                       (the single-float (attack-piercing attack)))))
                                        (incf (aref damages j) damage)
                                        (setf (aref last-attackers j) index
                                              (aref stuns j)
                                              (max (aref stuns j)
                                                   (the fixnum (round (* damage stun-rate)))))))))))))
                 ;; Salve tirée : la cible est ébranlée ; puis l'exercice.
                 (when (and volley (plusp shots))
                   (lose-morale enemy (* (the single-float *volley-shock*)
                                         (/ (float shots 1f0) (float files 1f0))))
                   (case (regiment-drill regiment)
                     ;; Salve et salvée : rien à faire, chacun recharge sur place.
                     ((:volley :salvo))
                     (:fire-by-rank
                       (let ((interval (round (* (the single-float *rank-fire-interval*)
                                                 +ticks-per-second+)))
                             (ranks (ceiling (regiment-alive regiment) files)))
                         (if (< (1+ volley-rank) (min (the fixnum *rank-fire-ranks*) ranks))
                             ;; Le rang s'accroupit, le suivant tirera par-dessus.
                             (setf (regiment-crouch regiment) (1+ volley-rank)
                                   (regiment-volley-rank regiment) (1+ volley-rank)
                                   (regiment-volley-timer regiment) interval)
                             ;; Dernier rang : tous se relèvent et rechargent.
                             (setf (regiment-countermarch-timer regiment) interval
                                   (regiment-volley-timer regiment) interval))))
                     ;; Contremarche : le rang passera à l'arrière.
                     (t
                       (setf (regiment-countermarch-timer regiment)
                             (round (* (the single-float *countermarch-delay*) +ticks-per-second+))
                             (regiment-volley-timer regiment)
                             (round (* (+ (the single-float *countermarch-delay*)
                                          (the single-float *volley-interval*))
                                       +ticks-per-second+)))))))))))
