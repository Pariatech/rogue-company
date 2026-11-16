(in-package #:rogue-company)

;;; Combat de mêlée, en trois temps par tick (voir BATTLE-TICK) :
;;;
;;; 1. Ciblage, régiment par régiment : chaque soldat cherche, par la grille
;;;    spatiale, l'ennemi le plus proche à portée de chacune de ses armes, choisit
;;;    son arme et sa cible. Il n'écrit que dans ses propres cases.
;;; 2. Coups, en séquence : chaque soldat prêt tire son toucher avec le générateur
;;;    de la bataille, et ses dégâts s'ajoutent à ceux de sa cible.
;;; 3. Dégâts appliqués ensemble ; les morts sont retirés de leur plage par
;;;    échange avec le dernier vivant du régiment, et crédités à leur tueur.
;;;
;;; Les distances de portée se mesurent de bord à bord : distance des centres
;;; moins les deux rayons. Seules les armes de mêlée frappent pour l'instant.
;;;
;;; L'engagement est au niveau du régiment : un ordre d'attaque avance la
;;; formation vers l'ennemi, puis la fige quand assez de soldats sont à leur
;;; distance d'arrêt (REGIMENT-HOLD-DISTANCE) : juste au-delà de la portée
;;; minimale de l'arme, pour que plusieurs rangs de piques frappent.

(defparameter *hold-fraction* 0.25f0
  "Part des files qui doivent être à la distance d'arrêt pour que le régiment s'arrête.")

(defparameter *contact-lost-ticks* 30
  "Ticks avec trop peu de soldats au contact (moins de *HOLD-FRACTION* des
files) avant qu'un régiment figé reprenne son avance.")

(defparameter *advance-period* 15
  "Ticks entre deux recalculs de la direction d'avance d'un régiment qui attaque.")

(defparameter *primary-return-ticks* 30
  "Ticks sans ennemi à portée de l'arme secondaire avant de reprendre l'arme
principale : évite d'alterner quand un ennemi oscille à la limite.")

(defparameter *press-distance* 1.0f0
  "Sous les piques : une arme à portée minimale (la pique) ne s'abaisse pas, depuis
le deuxième rang ou plus loin, sur un ennemi à moins de cette distance (de bord
à bord) de son propre premier rang. Collé au premier rang, l'ennemi n'a plus en
face que lui : des rodeleros passés sous les pointes ne prennent plus les coups
de quatre rangs de piques.")

(defparameter *stun-seconds-per-damage* 0.04f0
  "Étourdissement d'un soldat touché au corps à corps, en secondes par point de
dégât reçu (après l'armure) : une pique à 30 contre un corselet étourdit
environ 1 s, une dague presque pas. L'étourdi ne bouge plus et pèse lourd dans
les collisions (*STUN-MASS*) : ceux qui le suivent butent sur lui. Même règle
pour toutes les armes ; elle compte surtout face aux armes longues, dont
l'ennemi doit traverser la portée sous les coups.")

(defparameter *min-hit-chance* 0.05f0)
(defparameter *max-hit-chance* 0.95f0)

;;; Ordres et engagement

(defun order-attack (regiment target)
  "Lance REGIMENT sur le régiment TARGET."
  (setf (regiment-orders regiment) (list :attack (regiment-index target))
        (regiment-holding-p regiment) nil
        (regiment-contact-lost regiment) 0)
  regiment)

(defun regiment-centroid (regiment soldiers)
  (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
        (n (regiment-alive regiment)) (sx 0f0) (sy 0f0))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          do (incf sx (aref xs i)) (incf sy (aref ys i)))
    (if (plusp n)
        (values (/ sx n) (/ sy n))
        (values 0f0 0f0))))

(defun hold-in-place (regiment soldiers &optional enemy)
  "Fige la formation sur la position de son premier rang, sans changer son
orientation : le premier rang reste au contact, les autres se rangent derrière.
Avec ENEMY, la formation se recentre de côté sur lui : arrivée en diagonale,
elle ne laisse pas une partie de son front dans le vide."
  (let* ((formation (regiment-formation regiment))
         (alive (regiment-alive regiment))
         (files (formation-files formation alive))
         (ranks (ceiling alive files))
         (start (regiment-start regiment))
         (front-count (min files alive))
         (facing (formation-facing formation))
         ;; Le premier rang est à cette distance devant le centre (place-formation).
         (front (* 0.5f0 (formation-spacing formation) (float (1- ranks) 1f0)))
         (sx 0f0) (sy 0f0))
    (when (plusp alive)
      ;; Le soldat n° k de la plage occupe l'emplacement n° k : les FILES premiers
      ;; forment le premier rang.
      (loop for i from start below (+ start front-count)
            do (incf sx (aref (soldiers-x soldiers) i))
               (incf sy (aref (soldiers-y soldiers) i)))
      (let ((x (- (/ sx front-count) (* front (cos facing))))
            (y (- (/ sy front-count) (* front (sin facing)))))
        (when (and enemy (plusp (regiment-alive enemy)))
          ;; Décalage de côté (perpendiculaire au front) jusqu'au centre de l'ennemi.
          (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
            (let ((side (- (* (- ey y) (cos facing)) (* (- ex x) (sin facing)))))
              (setf x (- x (* side (sin facing)))
                    y (+ y (* side (cos facing)))))))
        (setf (formation-x formation) x
              (formation-y formation) y
              (regiment-slots-dirty-p regiment) t)))))

(defun attack-facing (regiment enemy soldiers)
  "Orientation de REGIMENT qui charge ENEMY : parallèle au côté de l'ennemi
qu'il aborde (front, flanc ou dos, d'après sa position par rapport au
rectangle de la formation ennemie), pour que tout son premier rang touche. Si
l'ennemi le charge aussi, chacun se tourne vers le centre de l'autre : leurs
fronts sont déjà parallèles, et se régler chacun sur l'autre oscillerait."
  (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
    (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
      (let ((orders (regiment-orders enemy)))
        (if (and (eq (first orders) :attack) (= (second orders) (regiment-index regiment)))
            (float (atan (- ey y) (- ex x)) 1f0)
            (let* ((formation (regiment-formation enemy))
                   (alive (max 1 (regiment-alive enemy)))
                   (files (formation-files formation alive))
                   (ranks (ceiling alive files))
                   (spacing (formation-spacing formation))
                   (radius (stats-radius (regiment-stats enemy)))
                   (half-width (+ radius (* 0.5f0 spacing (float (1- files) 1f0))))
                   (half-depth (+ radius (* 0.5f0 spacing (float (1- ranks) 1f0))))
                   (facing (formation-facing formation))
                   (fx (cos facing)) (fy (sin facing))
                   (dx (- x ex)) (dy (- y ey))
                   ;; Position de REGIMENT dans le repère de l'ennemi.
                   (along (+ (* dx fx) (* dy fy)))
                   (across (- (* dy fx) (* dx fy))))
              ;; Le côté abordé est celui que la droite vers REGIMENT traverse ;
              ;; REGIMENT lui fait face, à l'opposé de sa normale.
              (if (>= (/ (abs along) half-depth) (/ (abs across) half-width))
                  (wrap-angle (if (plusp along) (+ facing (float pi 1f0)) facing))
                  (wrap-angle (if (plusp across)
                                  (- facing (* 0.5f0 (float pi 1f0)))
                                  (+ facing (* 0.5f0 (float pi 1f0))))))))))))

(defun regiment-contact (regiment)
  "Soldats au contact, pour l'arrêt et la poussée : le premier rang à sa distance
d'arrêt ; en ordre lâche, dont le premier rang se disperse, les soldats engagés."
  (if (loose-order-p regiment)
      (regiment-engaged regiment)
      (regiment-closed regiment)))

(defun update-engagement (battle regiment)
  "Fait avancer un régiment qui attaque, ou le fige au contact."
  (let ((orders (regiment-orders regiment))
        (soldiers (battle-soldiers battle)))
    (when (and (eq (first orders) :attack) (plusp (regiment-alive regiment))
               ;; Des tireurs s'arrêtent à portée de tir (shooting.lisp).
               (not (update-fire-position battle regiment)))
      (let* ((enemy (aref (battle-regiments battle) (second orders)))
             (formation (regiment-formation regiment))
             (files (formation-files formation (regiment-alive regiment)))
             (needed (max 1 (floor (* *hold-fraction* files))))
             (closed (regiment-contact regiment)))
        (cond ((or (zerop (regiment-alive enemy)) (regiment-routed-p enemy))
               ;; Ennemi détruit ou débandé : le régiment reste où il est.
               (setf (regiment-orders regiment) nil
                     (regiment-holding-p regiment) nil)
               (hold-in-place regiment soldiers))
              ((>= closed needed)
               (setf (regiment-contact-lost regiment) 0)
               (unless (regiment-holding-p regiment)
                 (setf (regiment-holding-p regiment) t)
                 (hold-in-place regiment soldiers enemy)))
              ((regiment-holding-p regiment)
               ;; Repoussé ou séparé de l'ennemi : sans assez de soldats au contact
               ;; pendant *CONTACT-LOST-TICKS*, il reprend son avance, même si des
               ;; soldats ont encore une cible à portée.
               (when (> (incf (regiment-contact-lost regiment)) *contact-lost-ticks*)
                 (setf (regiment-holding-p regiment) nil)))
              ((zerop (mod (+ (battle-tick-count battle) (regiment-index regiment))
                           *advance-period*))
               ;; Avance : la formation vise le centre de l'ennemi, front parallèle
               ;; au côté qu'elle aborde (ATTACK-FACING).
               (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
                 (setf (formation-x formation) ex
                       (formation-y formation) ey
                       (formation-facing formation) (attack-facing regiment enemy soldiers)
                       (regiment-slots-dirty-p regiment) t))))))))

(defun update-regiment-bounds (regiment soldiers)
  (declare (optimize (speed 3) (safety 1)))
  (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
        (min-x most-positive-single-float) (min-y most-positive-single-float)
        (max-x most-negative-single-float) (max-y most-negative-single-float))
    (declare (type float-array xs ys) (type single-float min-x min-y max-x max-y))
    (loop for i of-type fixnum from (regiment-start regiment) below (regiment-end regiment)
          do (let ((x (aref xs i)) (y (aref ys i)))
               (setf min-x (min min-x x) max-x (max max-x x)
                     min-y (min min-y y) max-y (max max-y y))))
    (setf (regiment-min-x regiment) min-x (regiment-max-x regiment) max-x
          (regiment-min-y regiment) min-y (regiment-max-y regiment) max-y)))

(defun enemy-within-p (battle regiment distance)
  "Vrai si la boîte d'un régiment ennemi vivant est à moins de DISTANCE de celle
de REGIMENT : sinon, aucun de ses soldats ne peut avoir de cible."
  (loop for other across (battle-regiments battle)
        thereis (and (/= (regiment-side other) (regiment-side regiment))
                     (plusp (regiment-alive other))
                     (< (regiment-min-x other) (+ (regiment-max-x regiment) distance))
                     (< (regiment-min-x regiment) (+ (regiment-max-x other) distance))
                     (< (regiment-min-y other) (+ (regiment-max-y regiment) distance))
                     (< (regiment-min-y regiment) (+ (regiment-max-y other) distance)))))

;;; 1. Ciblage

(defun melee-reaches (attack)
  "Portée minimale et maximale de ATTACK, ou NIL si elle ne frappe pas en mêlée."
  (when (eq (attack-kind attack) :melee)
    (values (attack-min-reach attack) (attack-reach attack))))

(defun acquire-targets (battle regiment)
  "Phase 1 pour les soldats de REGIMENT : arme en main, cible et minuteries.
N'écrit que dans les cases de ces soldats, et compte les engagés, et ceux du
premier rang qui sont au contact (cible à la distance d'arrêt)."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((soldiers (battle-soldiers battle))
         (grid (battle-grid battle))
         (stats (regiment-stats regiment))
         (primary (stats-primary stats))
         (secondary (stats-secondary stats))
         (side (regiment-side regiment))
         (start (regiment-start regiment))
         (end (regiment-end regiment))
         (targets (soldiers-target soldiers))
         (weapons (soldiers-weapon soldiers))
         (switch-timers (soldiers-switch-timer soldiers))
         (cooldowns (soldiers-cooldown soldiers))
         (calms (soldiers-calm soldiers))
         (max-radius (battle-max-radius battle))
         (hold-distance (regiment-hold-distance regiment))
         ;; Le premier rang : seul lui compte au contact (poussée, arrêt).
         (files (max 1 (formation-files (regiment-formation regiment)
                                        (max 1 (regiment-alive regiment)))))
         (front-end (+ start files))
         (spacing (formation-spacing (regiment-formation regiment)))
         (press (the single-float *press-distance*))
         ;; Une arme longue ne vise que devant sa formation (flanks.lisp).
         (front-x (cos (formation-facing (regiment-formation regiment))))
         (front-y (sin (formation-facing (regiment-formation regiment))))
         (front-cos (the single-float *front-cos*))
         (engaged 0)
         (closed 0))
    (declare (type fixnum side start end files front-end engaged closed)
             (type single-float spacing press front-x front-y front-cos)
             (type single-float hold-distance)
             (type fixnum-array targets weapons switch-timers cooldowns calms)
             (type single-float max-radius))
    (multiple-value-bind (p-min p-reach) (melee-reaches primary)
      (multiple-value-bind (s-min s-reach) (melee-reaches secondary)
        (declare (ignore s-min))
        (let* ((p-usable (and p-reach t))
               ;; Arme principale à reprendre après le contact : de mêlée, ou de
               ;; tir (sans cible de mêlée, elle ne vise rien ici : shooting.lisp).
               (p-returns (or p-usable (eq (attack-kind primary) :ranged)))
               (s-usable (and s-reach t))
               (p-min (if p-usable (the single-float p-min) 0f0))
               (p-reach (if p-usable (the single-float p-reach) -1f0))
               (s-reach (if s-usable (the single-float s-reach) -1f0))
               (search (max p-reach s-reach)))
          (declare (type single-float p-min p-reach s-reach search))
          ;; Minuteries : elles avancent même sans ennemi.
          (loop for i of-type fixnum from start below end
                do (when (plusp (aref switch-timers i)) (decf (aref switch-timers i)))
                   (when (plusp (aref cooldowns i)) (decf (aref cooldowns i))))
          (if (or (< search 0f0)
                  (regiment-routed-p regiment)
                  (not (enemy-within-p battle regiment (+ search (* 2f0 max-radius)))))
              ;; Aucun ennemi à portée du régiment, ou débandade : personne n'a de cible.
              (loop for i of-type fixnum from start below end
                    do (setf (aref targets i) -1 (aref cooldowns i) -1)
                       (setf (aref calms i) (min 1000000 (1+ (aref calms i))))
                       (when (and (= 1 (aref weapons i)) p-returns
                                  (>= (aref calms i) (the fixnum *primary-return-ticks*)))
                         (setf (aref weapons i) 0
                               (aref switch-timers i) (attack-ready-time primary))))
              (let* ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
                     (radii (soldiers-radius soldiers))
                     (sides (soldiers-side soldiers))
                     (columns (grid-columns grid))
                     (cell-size (grid-cell-size grid))
                     (cell-start (grid-cell-start grid))
                     (sorted (grid-sorted grid))
                     (soldier-cell (grid-soldier-cell grid))
                     (return-ticks *primary-return-ticks*))
                (declare (type float-array xs ys radii)
                         (type fixnum-array sides cell-start sorted soldier-cell)
                         (type fixnum columns return-ticks)
                         (type single-float cell-size))
                (loop for i of-type fixnum from start below end
                      do (let* ((xi (aref xs i)) (yi (aref ys i)) (ri (aref radii i))
                                (cell (aref soldier-cell i))
                                (row (floor cell columns))
                                (column (- cell (* row columns)))
                                (span (min 8 (the fixnum (ceiling (+ search ri max-radius) cell-size))))
                                (best-p -1) (best-p-d most-positive-single-float)
                                (best-s -1) (best-s-d most-positive-single-float)
                                ;; Arme à portée minimale, hors du premier rang : un
                                ;; ennemi doit être à PRESS au moins de notre premier
                                ;; rang, à BEHIND mètres devant ce soldat.
                                (behind (if (and (plusp p-min) (>= i front-end))
                                            (+ (* spacing (float (floor (- i start) files) 1f0))
                                               press)
                                            0f0))
                                (p-floor (max p-min behind)))
                           (declare (type single-float xi yi ri best-p-d best-s-d behind p-floor)
                                    (type fixnum cell row column span best-p best-s))
                           (loop for r of-type fixnum from (max 0 (- row span))
                                   to (min (1- columns) (+ row span))
                                 do (loop for c of-type fixnum from (max 0 (- column span))
                                            to (min (1- columns) (+ column span))
                                          do (let ((neighbour (+ (* r columns) c)))
                                               (declare (type fixnum neighbour))
                                               (loop for k of-type fixnum from (aref cell-start neighbour)
                                                       below (aref cell-start (1+ neighbour))
                                                     do (let ((j (aref sorted k)))
                                                          (declare (type fixnum j))
                                                          (unless (= (aref sides j) side)
                                                            (let* ((dx (- (aref xs j) xi))
                                                                   (dy (- (aref ys j) yi))
                                                                   (center (sqrt (the (single-float 0f0)
                                                                                      (+ (* dx dx) (* dy dy)))))
                                                                   (d (- center ri (aref radii j))))
                                                              (declare (type single-float dx dy center d))
                                                              (when (and (<= p-floor d p-reach) (< d best-p-d)
                                                                         (or (zerop p-min)
                                                                             (>= (+ (* dx front-x) (* dy front-y))
                                                                                 (* front-cos center))))
                                                                (setf best-p j best-p-d d))
                                                              (when (and (<= d s-reach) (< d best-s-d))
                                                                (setf best-s j best-s-d d)))))))))
                           (setf (aref calms i)
                                 (if (>= best-s 0) 0 (min 1000000 (1+ (aref calms i)))))
                           ;; Arme voulue : la secondaire contre un ennemi trop près
                           ;; pour la principale ; la principale sinon, reprise
                           ;; seulement après un moment sans ennemi au contact.
                           (let* ((weapon (aref weapons i))
                                  (wanted (cond ((and (>= best-s 0) (< best-s-d p-min)) 1)
                                                ((>= best-p 0)
                                                 (if (and (= weapon 1)
                                                          (< (aref calms i) return-ticks))
                                                     1 0))
                                                ((>= best-s 0) 1)
                                                ((and (= weapon 1) p-returns
                                                      (>= (aref calms i) return-ticks))
                                                 0)
                                                (t weapon)))
                                  (target (if (= wanted 0) best-p best-s)))
                             (declare (type fixnum weapon wanted target))
                             (unless (= wanted weapon)
                               (setf (aref weapons i) wanted
                                     (aref switch-timers i)
                                     (attack-ready-time (if (= wanted 0) primary secondary))))
                             (setf (aref targets i) target)
                             (if (< target 0)
                                 (setf (aref cooldowns i) -1)
                                 (progn
                                   (incf engaged)
                                   (when (and (< i front-end)
                                              (<= (if (= wanted 0) best-p-d best-s-d)
                                                  hold-distance))
                                     (incf closed)))))))))
          (setf (regiment-engaged regiment) engaged
                (regiment-closed regiment) closed))))))

;;; Ordre lâche : un régiment au trait :LOOSE-ORDER (les rodeleros, par leur
;;; rondache) rompt sa formation au contact. Chacun de ses soldats sans ennemi à
;;; portée va chercher le plus proche, dans *LOOSE-ENGAGE-RADIUS* : les files
;;; qui dépassent du front ennemi en contournent le coin et le prennent de
;;; flanc. Un régiment discipliné (les piquiers) garde son rang.

(defparameter *loose-crowd-penalty* 1.5f0
  "Ordre lâche : mètres ajoutés à la distance d'un ennemi pour chaque soldat qui
le vise déjà. Les soldats en surplus s'étalent le long de la formation ennemie,
et en contournent le coin, au lieu de s'entasser sur le plus proche.")

(defparameter *loose-engage-radius* 6f0
  "Ordre lâche : distance, de bord à bord, jusqu'où un soldat sans ennemi à
portée quitte son rang pour aller chercher le plus proche.")

(defun loose-order-p (regiment)
  (member :loose-order (regiment-traits regiment)))

(defun loosen-formation (battle regiment)
  "Pour un régiment en ordre lâche au contact : l'emplacement de chaque soldat
sans cible devient un point à sa distance d'arrêt de l'ennemi le plus proche,
compté plus loin d'autant qu'il est déjà visé (*LOOSE-CROWD-PENALTY*). Sans
ennemi dans *LOOSE-ENGAGE-RADIUS*, le point le plus proche de la boîte du régiment attaqué :
les soldats au bout du front ne restent pas à l'écart.
La formation reprend ses emplacements quand elle ne tient plus le contact
(UPDATE-ENGAGEMENT les recalcule). Appelé après UPDATE-REGIMENT-SLOTS, avant le
déplacement : cibles, compte des attaquants et grille sont ceux du tick
précédent, et un indice de la grille peut désigner un soldat mort ou échangé
depuis (il est ignoré s'il est mort)."
  (declare (optimize (speed 3) (safety 1)))
  (when (and (loose-order-p regiment)
             (regiment-holding-p regiment)
             (eq (first (regiment-orders regiment)) :attack))
    (let* ((soldiers (battle-soldiers battle))
           (grid (battle-grid battle))
           (side (regiment-side regiment))
           (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
           (slot-xs (soldiers-slot-x soldiers)) (slot-ys (soldiers-slot-y soldiers))
           (radii (soldiers-radius soldiers))
           (hps (soldiers-hp soldiers))
           (sides (soldiers-side soldiers))
           (targets (soldiers-target soldiers))
           (attackers (soldiers-attackers soldiers))
           (crowd (the single-float *loose-crowd-penalty*))
           (columns (grid-columns grid))
           (cell-size (grid-cell-size grid))
           (cell-start (grid-cell-start grid))
           (sorted (grid-sorted grid))
           (soldier-cell (grid-soldier-cell grid))
           (radius (the single-float *loose-engage-radius*))
           (max-radius (battle-max-radius battle))
           (hold (regiment-hold-distance regiment))
           (span (min 8 (the fixnum (ceiling (+ radius (* 2f0 max-radius)) cell-size))))
           ;; Boîte du régiment attaqué : sans ennemi dans le rayon, on y marche.
           (enemy (aref (battle-regiments battle) (second (regiment-orders regiment))))
           (box-min-x (regiment-min-x enemy)) (box-max-x (regiment-max-x enemy))
           (box-min-y (regiment-min-y enemy)) (box-max-y (regiment-max-y enemy)))
      (declare (type float-array xs ys slot-xs slot-ys radii hps)
               (type single-float box-min-x box-max-x box-min-y box-max-y)
               (type fixnum-array sides targets attackers cell-start sorted soldier-cell)
               (type fixnum side columns span)
               (type single-float cell-size radius max-radius hold crowd))
      (loop for i of-type fixnum from (regiment-start regiment) below (regiment-end regiment)
            when (< (aref targets i) 0)
              do (let* ((xi (aref xs i)) (yi (aref ys i)) (ri (aref radii i))
                        (cell (aref soldier-cell i))
                        (row (floor cell columns))
                        (column (- cell (* row columns)))
                        (best -1) (best-score most-positive-single-float) (best-center 0f0))
                   (declare (type single-float xi yi ri best-score best-center)
                            (type fixnum cell row column best))
                   (loop for r of-type fixnum from (max 0 (- row span)) to (min (1- columns) (+ row span))
                         do (loop for c of-type fixnum from (max 0 (- column span))
                                    to (min (1- columns) (+ column span))
                                  do (let ((neighbour (+ (* r columns) c)))
                                       (declare (type fixnum neighbour))
                                       (loop for k of-type fixnum from (aref cell-start neighbour)
                                               below (aref cell-start (1+ neighbour))
                                             do (let ((j (aref sorted k)))
                                                  (declare (type fixnum j))
                                                  (unless (or (= (aref sides j) side)
                                                              (not (plusp (aref hps j))))
                                                    (let* ((dx (- (aref xs j) xi))
                                                           (dy (- (aref ys j) yi))
                                                           (center (sqrt (the (single-float 0f0)
                                                                              (+ (* dx dx) (* dy dy)))))
                                                           (d (- center ri (aref radii j))))
                                                      (declare (type single-float dx dy center d))
                                                      (when (< d radius)
                                                        (let ((score (+ d (* crowd (float (aref attackers j)
                                                                                         1f0)))))
                                                          (declare (type single-float score))
                                                          (when (< score best-score)
                                                            (setf best j best-score score
                                                                  best-center center)))))))))))
                   (when (and (< best 0) (plusp (regiment-alive enemy)))
                     ;; Personne dans le rayon : vers le point le plus proche du
                     ;; régiment attaqué, jusqu'à trouver un ennemi.
                     (setf (aref slot-xs i) (max box-min-x (min box-max-x xi))
                           (aref slot-ys i) (max box-min-y (min box-max-y yi))))
                   (when (and (>= best 0) (plusp best-center))
                     ;; Les suivants le voient déjà visé.
                     (incf (aref attackers best))
                     ;; Sur la droite vers l'ennemi, à la distance d'arrêt de lui.
                     (let* ((keep (+ hold ri (aref radii best)))
                            (ratio (/ keep best-center)))
                       (declare (type single-float keep ratio))
                       (setf (aref slot-xs i) (- (aref xs best) (* ratio (- (aref xs best) xi)))
                             (aref slot-ys i) (- (aref ys best) (* ratio (- (aref ys best) yi)))))))))))

;;; 2. Coups

(defun strike (battle)
  "Phase 2 : chaque soldat prêt frappe sa cible. Séquentielle, dans l'ordre des
indices, avec le générateur de la bataille : la bataille se rejoue."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((soldiers (battle-soldiers battle))
         (regiments (battle-regiments battle))
         (rng (battle-rng battle))
         (targets (soldiers-target soldiers))
         (weapons (soldiers-weapon soldiers))
         (switch-timers (soldiers-switch-timer soldiers))
         (cooldowns (soldiers-cooldown soldiers))
         (hps (soldiers-hp soldiers))
         (damages (soldiers-damage soldiers))
         (last-attackers (soldiers-last-attacker soldiers))
         (strike-ticks (soldiers-strike-tick soldiers))
         (stuns (soldiers-stun soldiers))
         (stun-rate (* (the single-float *stun-seconds-per-damage*)
                       #.(float +ticks-per-second+ 1f0)))
         (tick (battle-tick-count battle))
         (soldier-regiments (soldiers-regiment soldiers))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (min-chance *min-hit-chance*)
         (max-chance *max-hit-chance*))
    (declare (type fixnum-array targets weapons switch-timers cooldowns last-attackers
                   strike-ticks soldier-regiments stuns)
             (type float-array xs ys)
             (type fixnum tick)
             (type single-float stun-rate)
             (type float-array hps damages)
             (type vector regiments)
             (type single-float min-chance max-chance))
    (loop for regiment across regiments
          do (let* ((stats (regiment-stats regiment))
                    (skill (stats-melee-skill stats))
                    (index (regiment-index regiment))
                    (bearer (regiment-bearer regiment)))
               (declare (type single-float skill) (type fixnum index bearer))
               (loop for i of-type fixnum from (regiment-start regiment) below (regiment-end regiment)
                     do (let ((target (aref targets i)))
                          ;; Le porteur d'étendard ne frappe pas.
                          (when (and (>= target 0) (/= i bearer) (zerop (aref switch-timers i))
                                     (plusp (aref hps target)))
                            (let* ((attack (if (zerop (aref weapons i))
                                               (stats-primary stats)
                                               (stats-secondary stats)))
                                   (cooldown (attack-cooldown attack)))
                              (declare (type fixnum cooldown))
                              (case (aref cooldowns i)
                                ;; Premier contact : le premier coup part au hasard dans
                                ;; la période, pour que tout le rang ne frappe pas ensemble.
                                (-1 (setf (aref cooldowns i) (rng-below rng cooldown)))
                                (0
                                 (setf (aref cooldowns i) cooldown
                                       (aref strike-ticks i) tick)
                                 (let* ((defending (aref regiments (aref soldier-regiments target)))
                                        (defender (regiment-stats defending))
                                        ;; De flanc ou de dos, la défense tombe (flanks.lisp).
                                        (facing (formation-facing (regiment-formation defending)))
                                        (sector (attack-sector (cos facing) (sin facing)
                                                               (- (aref xs i) (aref xs target))
                                                               (- (aref ys i) (aref ys target))))
                                        (chance (max min-chance
                                                     (min max-chance
                                                          (- (+ skill (the single-float
                                                                           (attack-accuracy attack)))
                                                             (* (sector-defense sector)
                                                                (the single-float
                                                                     (stats-defense defender))))))))
                                   (declare (type single-float facing chance) (type fixnum sector))
                                   (when (< (rng-float rng) chance)
                                     ;; L'armure réduit les dégâts ordinaires, jamais les
                                     ;; perforants.
                                     (let ((damage (+ (max 0f0 (- (the single-float (attack-damage attack))
                                                                  (the single-float (stats-armor defender))))
                                                      (the single-float (attack-piercing attack)))))
                                       (progn
                                         (incf (aref damages target) damage)
                                         (setf (aref last-attackers target) index
                                               (aref stuns target)
                                               (max (aref stuns target)
                                                    (the fixnum (round (* damage stun-rate)))))))))))))))))))

;;; 3. Dégâts et morts

(defun move-soldier (soldiers from to)
  "Copie le soldat FROM à l'indice TO, dans tous les tableaux."
  (dolist (slot *soldier-float-arrays*)
    (let ((array (slot-value soldiers slot)))
      (setf (aref array to) (aref array from))))
  (dolist (slot *soldier-fixnum-arrays*)
    (let ((array (slot-value soldiers slot)))
      (setf (aref array to) (aref array from)))))

(defun remove-soldier (regiment soldiers i)
  "Retire le soldat I de REGIMENT en comblant le trou au plus court :
1. le soldat derrière lui avance d'un rang, et ainsi de suite dans sa file
   jusqu'à l'avant-dernier rang ;
2. le trou arrive dans l'avant-dernier rang : le soldat du dernier rang le plus
   proche le comble ;
3. le dernier rang se resserre d'une place vers le trou qu'il a laissé.
Chaque déplacement va d'un indice plus grand vers un plus petit : un parcours
croissant de la plage, qui revérifie l'indice I, voit tous les morts."
  (let* ((start (regiment-start regiment))
         (alive (regiment-alive regiment))
         (formation (regiment-formation regiment))
         (files (formation-files formation alive))
         (ranks (ceiling alive files))
         (last-rank (1- ranks))
         (last-size (- alive (* last-rank files)))
         (slot (- i start)))
    (multiple-value-bind (rank file) (floor slot files)
      ;; 1. La file avance, jusqu'à l'avant-dernier rang.
      (loop while (< (1+ rank) last-rank)
            do (move-soldier soldiers (+ start (* (1+ rank) files) file)
                             (+ start (* rank files) file))
               (incf rank))
      ;; 2. Trou dans un rang complet : le soldat du dernier rang le plus proche
      ;;    latéralement le comble (le dernier rang, incomplet, est centré).
      (let ((hole-in-last
              (if (< rank last-rank)
                  (let ((nearest (max 0 (min (1- last-size)
                                             (round (+ (- file (/ (1- files) 2))
                                                       (/ (1- last-size) 2)))))))
                    (move-soldier soldiers (+ start (* last-rank files) nearest)
                                  (+ start (* rank files) file))
                    nearest)
                  file)))
        ;; 3. Le dernier rang se resserre vers le trou.
        (loop for j from hole-in-last below (1- last-size)
              do (move-soldier soldiers (+ start (* last-rank files) j 1)
                               (+ start (* last-rank files) j)))))
    ;; La dernière case de la plage se libère : réservée, marquée morte.
    (let ((freed (+ start alive -1)))
      (setf (aref (soldiers-hp soldiers) freed) 0f0
            (aref (soldiers-target soldiers) freed) -1))
    (decf (regiment-alive regiment))
    (lose-morale regiment (casualty-morale regiment))
    (setf (regiment-slots-dirty-p regiment) t)))

(defun apply-damage (battle)
  "Applique les dégâts du tick, puis retire les morts de chaque régiment."
  (let* ((soldiers (battle-soldiers battle))
         (regiments (battle-regiments battle))
         (hps (soldiers-hp soldiers))
         (damages (soldiers-damage soldiers))
         (count (soldiers-count soldiers)))
    (declare (type float-array hps damages) (type fixnum count))
    (dotimes (i count)
      (when (plusp (aref damages i))
        (decf (aref hps i) (aref damages i))
        (setf (aref damages i) 0f0)))
    (loop for regiment across regiments
          do (let ((i (regiment-start regiment))
                   (ranks (regiment-depth regiment)))
               (loop while (< i (regiment-end regiment))
                     do (if (plusp (aref hps i))
                            (incf i)
                            (let ((killer (aref (soldiers-last-attacker soldiers) i)))
                              (when (>= killer 0)
                                (incf (regiment-kills (aref regiments killer))))
                              (record-soldier-death battle regiment i)
                              (remove-soldier regiment soldiers i))))
               (keep-front regiment ranks)))))

(defun regiment-depth (regiment)
  "Nombre de rangs de REGIMENT, le dernier compris même incomplet."
  (let ((alive (regiment-alive regiment)))
    (if (plusp alive)
        (ceiling alive (formation-files (regiment-formation regiment) alive))
        0)))

(defun keep-front (regiment ranks-before)
  "Le centre de la formation est au milieu de sa profondeur : quand REGIMENT
perd des rangs, il avance d'un demi-espacement par rang perdu, pour que le
premier rang reste au contact au lieu de reculer."
  (let ((lost (- ranks-before (regiment-depth regiment))))
    (when (and (plusp lost) (plusp (regiment-alive regiment)))
      (let* ((formation (regiment-formation regiment))
             (step (* 0.5f0 (formation-spacing formation) lost))
             (facing (formation-facing formation)))
        (incf (formation-x formation) (* step (cos facing)))
        (incf (formation-y formation) (* step (sin facing)))
        (setf (regiment-slots-dirty-p regiment) t)))))
