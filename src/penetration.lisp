(in-package #:rogue-company)

;;; Percée de la charge : un régiment lourd (masse d'au moins *PENETRATION-MASS*,
;;; la cavalerie) qui charge ne s'arrête pas net à sa distance d'arrêt. Au
;;; premier contact, chaque cavalier prend un élan (MOMENTUM, en m/s : sa vitesse
;;; de course) et continue droit sur l'ennemi (DRIVE-CHARGES, après le
;;; déplacement, avant les collisions). Chaque ennemi qu'il heurte est renversé
;;; (étourdi *KNOCKDOWN-SECONDS*, écarté de sa route) et lui coûte de l'élan,
;;; d'autant plus qu'il est lourd ; l'élan s'use aussi de lui-même
;;; (*DRIVE-DRAG*). Des piques arrêtent net : pas d'élan contre des piques de
;;; face (PIKE-WALL-P), et un coup de pique sur un cavalier lancé le stoppe
;;; (STRIKE). Un élan de -1 est dépensé : il ne repart qu'à la charge suivante.
;;;
;;; Chaque homme renversé met son régiment en désordre (REGIMENT-DISORDER, de
;;; 0 à 1) : défense, poussée et retour aux emplacements diminués, moral qui
;;; s'use, pas de reformation ; le désordre retombe au calme.

(defparameter *penetration-mass* 1.5f0
  "Masse minimale d'un régiment pour percer à la charge : la cavalerie.")

(defparameter *drive-drag* 3f0
  "Élan perdu par seconde, sans rien heurter, en m/s par seconde.")

(defparameter *knock-cost* 8f0
  "Élan perdu pour chaque ennemi renversé, en m/s, multiplié par sa masse et
divisé par celle du cavalier : un lancier (9) perd 0,9 m/s par fantassin, un
chevau-léger (6), 1,3 m/s.")

(defparameter *knockdown-seconds* 1.5f0
  "Étourdissement d'un homme renversé par un cavalier lancé, en secondes.")

(defparameter *disorder-per-knockdown* 4f0
  "Désordre ajouté par homme renversé, divisé par l'effectif vivant : un dixième
du régiment renversé le met à 0,4.")

(defparameter *disorder-recovery* 0.08f0 "Désordre résorbé par seconde.")

(defparameter *disorder-morale* 4f0 "Moral perdu par seconde au désordre maximal.")

(defparameter *disorder-defense* 0.5f0 "Part de la défense perdue au désordre maximal.")

(defparameter *disorder-slowdown* 0.6f0
  "Part de la vitesse de retour aux emplacements perdue au désordre maximal.")

(defun penetrating-p (regiment)
  "Vrai si REGIMENT perce à la charge : il charge et il est lourd."
  (and (charge-capable-p regiment)
       (>= (stats-mass (regiment-stats regiment)) *penetration-mass*)))

(defun reset-momentum (regiment soldiers)
  "Élan de tous les soldats de REGIMENT à 0 : prêt pour la charge suivante."
  (fill (soldiers-momentum soldiers) 0f0
        :start (regiment-start regiment) :end (regiment-end regiment)))

(defun add-disorder (regiment amount)
  (setf (regiment-disorder regiment)
        (min 1f0 (+ (regiment-disorder regiment) (float amount 1f0)))))

(defun update-disorder (regiment)
  "Une fois par tick : le désordre use le moral, puis se résorbe."
  (let ((disorder (regiment-disorder regiment)))
    (when (plusp disorder)
      (lose-morale regiment (/ (* *disorder-morale* disorder) +ticks-per-second+))
      (setf (regiment-disorder regiment)
            (max 0f0 (- disorder (/ *disorder-recovery* +ticks-per-second+)))))))

(defun start-momentum (battle regiment)
  "Donne son élan à chaque soldat de REGIMENT, à la charge, dès qu'il a une cible
(un ennemi à portée de son arme) : il enchaîne le galop et la percée sans
s'arrêter. Contre des piques de face, l'élan est dépensé sur place."
  (let* ((soldiers (battle-soldiers battle))
         (regiments (battle-regiments battle))
         (momentums (soldiers-momentum soldiers))
         (targets (soldiers-target soldiers))
         (owners (soldiers-regiment soldiers))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (speed (stats-run-speed (regiment-stats regiment))))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          do (let ((target (aref targets i)))
               (when (and (zerop (aref momentums i)) (>= target 0))
                 (let* ((dx (- (aref xs i) (aref xs target)))
                        (dy (- (aref ys i) (aref ys target)))
                        (defending (aref regiments (aref owners target)))
                        (facing (formation-facing (regiment-formation defending)))
                        (sector (if (regiment-square-p defending)
                                    0
                                    (attack-sector (cos facing) (sin facing) dx dy))))
                   (setf (aref momentums i)
                         (if (and (zerop sector) (pike-wall-p defending))
                             -1f0
                             speed))
                   ;; Il fonce droit sur sa cible, et garde ce cap.
                   (setf (aref (soldiers-facing soldiers) i) (float (atan (- dy) (- dx)) 1f0))))))))

(defun drive-charges (battle)
  "Après le déplacement, avant les collisions : chaque soldat lancé avance de
son élan vers sa cible (ou droit devant), renverse les ennemis qu'il chevauche,
et perd de l'élan. Séquentiel, sans tirage."
  (let* ((soldiers (battle-soldiers battle))
         (regiments (battle-regiments battle))
         (grid (battle-grid battle))
         (momentums (soldiers-momentum soldiers))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (facings (soldiers-facing soldiers))
         (radii (soldiers-radius soldiers)) (masses (soldiers-mass soldiers))
         (hps (soldiers-hp soldiers)) (sides (soldiers-side soldiers))
         (stuns (soldiers-stun soldiers))
         (owners (soldiers-regiment soldiers))
         (columns (grid-columns grid))
         (cell-size (grid-cell-size grid))
         (origin (grid-origin grid))
         (cell-start (grid-cell-start grid))
         (sorted (grid-sorted grid))
         (drag (/ *drive-drag* +ticks-per-second+))
         (knock-ticks (round (* *knockdown-seconds* +ticks-per-second+))))
    (loop for regiment across regiments
          when (and (penetrating-p regiment) (plusp (regiment-alive regiment)))
            do (when (or (regiment-charging-p regiment)
                         (charge-bonus-active-p regiment (battle-tick-count battle)))
                 (start-momentum battle regiment))
               (loop for i from (regiment-start regiment) below (regiment-end regiment)
                     when (plusp (aref momentums i))
                       do (let* ((angle (aref facings i))
                                 (step (/ (aref momentums i) +ticks-per-second+))
                                 (ri (aref radii i)) (mi (aref masses i))
                                 (side (aref sides i)))
                            (setf (aref facings i) angle)
                            (incf (aref xs i) (* step (cos angle)))
                            (incf (aref ys i) (* step (sin angle)))
                            (decf (aref momentums i) drag)
                            ;; Ennemis heurtés : renversés, écartés, ils coûtent de l'élan.
                            (let* ((xi (aref xs i)) (yi (aref ys i))
                                   (column (grid-coordinate xi origin cell-size columns))
                                   (row (grid-coordinate yi origin cell-size columns))
                                   (span (ceiling (+ ri 1f0) cell-size)))
                              (loop for r from (max 0 (- row span)) to (min (1- columns) (+ row span))
                                    do (loop for c from (max 0 (- column span))
                                               to (min (1- columns) (+ column span))
                                             do (let ((cell (+ (* r columns) c)))
                                                  (loop for k from (aref cell-start cell)
                                                          below (aref cell-start (1+ cell))
                                                        do (let ((j (aref sorted k)))
                                                             (when (and (/= (aref sides j) side)
                                                                        (plusp (aref hps j))
                                                                        (plusp (aref momentums i)))
                                                               (let* ((dx (- (aref xs j) xi))
                                                                      (dy (- (aref ys j) yi))
                                                                      (d (sqrt (+ (* dx dx) (* dy dy))))
                                                                      (contact (+ ri (aref radii j))))
                                                                 (when (< d contact)
                                                                   ;; Projeté sur le côté de la route du
                                                                   ;; cheval : il lui ouvre le passage.
                                                                   (let* ((fx (cos angle)) (fy (sin angle))
                                                                          (across (- (* dy fx) (* dx fy)))
                                                                          (sign (if (minusp across) -1f0 1f0))
                                                                          (along (+ (* dx fx) (* dy fy))))
                                                                     (setf (aref xs j) (+ xi (* fx (max 0f0 along))
                                                                                          (* sign (- fy) (+ contact 0.05f0)))
                                                                           (aref ys j) (+ yi (* fy (max 0f0 along))
                                                                                          (* sign fx (+ contact 0.05f0)))))
                                                                   ;; Renversé pour la première fois par un
                                                                   ;; cheval (un coup qui l'a étourdi compte
                                                                   ;; quand même) : désordre et élan perdu.
                                                                   (when (< (aref stuns j) (- knock-ticks 3))
                                                                     (let ((victims (aref regiments (aref owners j))))
                                                                       (add-disorder victims
                                                                                     (/ *disorder-per-knockdown*
                                                                                        (max 1 (regiment-alive victims)))))
                                                                     (decf (aref momentums i)
                                                                           (/ (* *knock-cost* (aref masses j)) mi)))
                                                                   (setf (aref stuns j) (max (aref stuns j) knock-ticks))))))))))))
                            (when (<= (aref momentums i) 0f0)
                              (setf (aref momentums i) -1f0))))))
