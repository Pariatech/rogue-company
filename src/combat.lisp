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

(defparameter *contact-lost-ticks* 60
  "Ticks sans aucun soldat engagé avant qu'un régiment figé reprenne son avance.")

(defparameter *advance-period* 15
  "Ticks entre deux recalculs de la direction d'avance d'un régiment qui attaque.")

(defparameter *primary-return-ticks* 30
  "Ticks sans ennemi à portée de l'arme secondaire avant de reprendre l'arme
principale : évite d'alterner quand un ennemi oscille à la limite.")

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

(defun hold-in-place (regiment soldiers)
  "Fige la formation sur la position de son premier rang, sans changer son
orientation : le premier rang reste au contact, les autres se rangent derrière."
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
      (setf (formation-x formation) (- (/ sx front-count) (* front (cos facing)))
            (formation-y formation) (- (/ sy front-count) (* front (sin facing)))
            (regiment-slots-dirty-p regiment) t))))

(defun update-engagement (battle regiment)
  "Fait avancer un régiment qui attaque, ou le fige au contact."
  (let ((orders (regiment-orders regiment))
        (soldiers (battle-soldiers battle)))
    (when (and (eq (first orders) :attack) (plusp (regiment-alive regiment)))
      (let* ((enemy (aref (battle-regiments battle) (second orders)))
             (formation (regiment-formation regiment))
             (files (formation-files formation (regiment-alive regiment)))
             (needed (max 1 (floor (* *hold-fraction* files))))
             (engaged (regiment-engaged regiment))
             (closed (regiment-closed regiment)))
        (cond ((or (zerop (regiment-alive enemy)) (regiment-routed-p enemy))
               ;; Ennemi détruit ou débandé : le régiment reste où il est.
               (setf (regiment-orders regiment) nil
                     (regiment-holding-p regiment) nil)
               (hold-in-place regiment soldiers))
              ((>= closed needed)
               (setf (regiment-contact-lost regiment) 0)
               (unless (regiment-holding-p regiment)
                 (setf (regiment-holding-p regiment) t)
                 (hold-in-place regiment soldiers)))
              ((regiment-holding-p regiment)
               (if (plusp engaged)
                   (setf (regiment-contact-lost regiment) 0)
                   (when (> (incf (regiment-contact-lost regiment)) *contact-lost-ticks*)
                     (setf (regiment-holding-p regiment) nil))))
              ((zerop (mod (+ (battle-tick-count battle) (regiment-index regiment))
                           *advance-period*))
               ;; Avance : la formation vise le centre de l'ennemi, front tourné vers lui.
               (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
                 (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
                   (setf (formation-x formation) ex
                         (formation-y formation) ey
                         (formation-facing formation) (float (atan (- ey y) (- ex x)) 1f0)
                         (regiment-slots-dirty-p regiment) t)))))))))

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
N'écrit que dans les cases de ces soldats, et compte les engagés."
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
         (engaged 0)
         (closed 0))
    (declare (type fixnum side start end engaged closed)
             (type single-float hold-distance)
             (type fixnum-array targets weapons switch-timers cooldowns calms)
             (type single-float max-radius))
    (multiple-value-bind (p-min p-reach) (melee-reaches primary)
      (multiple-value-bind (s-min s-reach) (melee-reaches secondary)
        (declare (ignore s-min))
        (let* ((p-usable (and p-reach t))
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
                       (when (and (= 1 (aref weapons i)) p-usable
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
                                (best-s -1) (best-s-d most-positive-single-float))
                           (declare (type single-float xi yi ri best-p-d best-s-d)
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
                                                                   (d (- (sqrt (the (single-float 0f0)
                                                                                    (+ (* dx dx) (* dy dy))))
                                                                         ri (aref radii j))))
                                                              (declare (type single-float dx dy d))
                                                              (when (and (<= p-min d p-reach) (< d best-p-d))
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
                                                ((and (= weapon 1) p-usable
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
                                   (when (<= (if (= wanted 0) best-p-d best-s-d) hold-distance)
                                     (incf closed)))))))))
          (setf (regiment-engaged regiment) engaged
                (regiment-closed regiment) closed))))))

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
         (tick (battle-tick-count battle))
         (soldier-regiments (soldiers-regiment soldiers))
         (min-chance *min-hit-chance*)
         (max-chance *max-hit-chance*))
    (declare (type fixnum-array targets weapons switch-timers cooldowns last-attackers
                   strike-ticks soldier-regiments)
             (type fixnum tick)
             (type float-array hps damages)
             (type vector regiments)
             (type single-float min-chance max-chance))
    (loop for regiment across regiments
          do (let* ((stats (regiment-stats regiment))
                    (skill (stats-melee-skill stats))
                    (index (regiment-index regiment)))
               (declare (type single-float skill) (type fixnum index))
               (loop for i of-type fixnum from (regiment-start regiment) below (regiment-end regiment)
                     do (let ((target (aref targets i)))
                          (when (and (>= target 0) (zerop (aref switch-timers i))
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
                                 (let* ((defender (regiment-stats
                                                   (aref regiments (aref soldier-regiments target))))
                                        (chance (max min-chance
                                                     (min max-chance
                                                          (- (+ skill (the single-float
                                                                           (attack-accuracy attack)))
                                                             (the single-float
                                                                  (stats-defense defender)))))))
                                   (declare (type single-float chance))
                                   (when (< (rng-float rng) chance)
                                     (let ((armor (max 0f0 (- (the single-float (stats-armor defender))
                                                              (the single-float
                                                                   (attack-penetration attack))))))
                                       (incf (aref damages target)
                                             (max 0f0 (- (the single-float (attack-damage attack))
                                                         armor)))
                                       (setf (aref last-attackers target) index))))))))))))))

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
