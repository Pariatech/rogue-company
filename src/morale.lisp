(in-package #:rogue-company)

;;; Poussée et moral, au niveau du régiment, une fois par tick après le combat.
;;;
;;; Poussée : deux régiments au contact comparent leur force, soldats au contact
;;; (à leur distance d'arrêt) × masse × moral. Le plus faible recule (ses soldats sont repoussés, sa
;;; formation aussi) et perd du moral ; le plus fort avance sa formation, et ses
;;; soldats suivent dès que l'ennemi n'est plus à leur distance d'arrêt.
;;;
;;; Moral : il baisse avec les pertes, d'autant plus que le régiment est peu
;;; discipliné, et quand il recule. Hors du contact, il remonte lentement vers
;;; sa valeur de départ. Sous *ROUT-MORALE*, le régiment se débande : il fuit en
;;; courant à l'opposé de l'ennemi et ne frappe plus. Les flancs exposés et les
;;; charges viendront plus tard.

(defparameter *loss-morale* 1f0
  "Moral perdu pour chaque pour cent de l'effectif de départ tué, multiplié par
1,5 − discipline : 1 point par pour cent pour une discipline de 0,5.")

(defparameter *rout-morale* 20f0 "Sous ce moral, le régiment se débande.")

(defparameter *fire-calm-seconds* 3
  "Secondes sans tirer ni essuyer de tir avant que le moral remonte.")

(defparameter *morale-recovery* 0.5f0
  "Moral regagné par seconde sans aucun soldat engagé, jusqu'à la valeur de départ.")

(defparameter *push-speed* 0.5f0
  "Recul, en mètres par seconde, d'un régiment qui n'a aucune force face à l'autre.
Proportionnel à l'écart des forces.")

(defparameter *push-morale-loss* 2f0
  "Moral perdu par seconde en reculant, à écart des forces maximal.")

(defparameter *rout-distance* 150f0
  "Distance de fuite d'un régiment débandé, en mètres.")

(defparameter *rout-scatter* 6f0
  "Écart maximal, sur chaque axe, des emplacements d'un régiment débandé autour
de sa formation : il fuit en désordre, pas en rangs.")

(defun regiment-routed-p (regiment)
  (eq (first (regiment-orders regiment)) :rout))

(defun lose-morale (regiment amount)
  (setf (regiment-morale regiment) (max 0f0 (- (regiment-morale regiment) (float amount 1f0)))))

(defun casualty-morale (regiment)
  "Moral perdu pour un mort de REGIMENT."
  (let ((stats (regiment-stats regiment)))
    (* *loss-morale* (/ 100f0 (regiment-capacity regiment))
       (- 1.5f0 (stats-discipline stats)))))

(defun regiment-push-force (regiment enemy)
  "Soldats au contact (REGIMENT-CONTACT) × masse × moral. Seuls ceux au contact
poussent : les rangs de piques qui frappent par-dessus le premier n'y ajoutent
rien. Leur nombre ne dépasse pas les files d'ENEMY, la largeur de son front :
des soldats en ordre lâche qui débordent sur les flancs pèsent par le moral de
flanc (flanks.lisp), pas par la poussée."
  (* (min (regiment-contact regiment)
          (formation-files (regiment-formation enemy) (max 1 (regiment-alive enemy))))
     (stats-mass (regiment-stats regiment))
     (max 0f0 (regiment-morale regiment))))

(defun opponent (battle regiment)
  "Régiment que REGIMENT attaque, ou NIL."
  (let ((orders (regiment-orders regiment)))
    (when (eq (first orders) :attack)
      (aref (battle-regiments battle) (second orders)))))

(defun shift-regiment (regiment soldiers dx dy &key (soldiers-too t))
  "Déplace la formation de REGIMENT, et ses soldats avec SOLDIERS-TOO."
  (let ((formation (regiment-formation regiment)))
    (incf (formation-x formation) dx)
    (incf (formation-y formation) dy)
    (setf (regiment-slots-dirty-p regiment) t)
    (when soldiers-too
      (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers)))
        (loop for i from (regiment-start regiment) below (regiment-end regiment)
              do (incf (aref xs i) dx)
                 (incf (aref ys i) dy))))))

(defparameter *push-max-lag* 0.25f0
  "Le vainqueur d'une poussée n'avance sa formation que si son premier rang est
en moyenne à moins de cette distance derrière ses emplacements, en mètres :
sinon les rangs de derrière, qui suivent leurs emplacements, s'entassent sur le
premier, resté au contact.")

(defun front-lag (regiment soldiers)
  "Retard moyen, le long du front, du premier rang de REGIMENT sur ses emplacements."
  (let* ((formation (regiment-formation regiment))
         (alive (regiment-alive regiment))
         (files (min alive (formation-files formation (max 1 alive))))
         (fx (cos (formation-facing formation)))
         (fy (sin (formation-facing formation)))
         (start (regiment-start regiment))
         (lag 0f0))
    (if (plusp files)
        (progn
          (loop for i from start below (+ start files)
                do (incf lag (+ (* fx (- (aref (soldiers-slot-x soldiers) i) (aref (soldiers-x soldiers) i)))
                                (* fy (- (aref (soldiers-slot-y soldiers) i) (aref (soldiers-y soldiers) i))))))
          (/ lag files))
        0f0)))

(defun push-regiment (battle regiment)
  "Compare la force de REGIMENT à celle de l'ennemi qu'il attaque, s'ils sont au
contact, et le fait reculer ou avancer d'autant."
  (let ((enemy (opponent battle regiment)))
    (when (and enemy (regiment-holding-p regiment)
               (plusp (regiment-contact regiment)) (plusp (regiment-contact enemy))
               (not (regiment-routed-p enemy)))
      (let* ((own (regiment-push-force regiment enemy))
             (other (regiment-push-force enemy regiment))
             (total (+ own other)))
        (when (plusp total)
          (let* ((balance (/ (- own other) total)) ; -1 écrasé, 1 écrasant
                 (facing (formation-facing (regiment-formation regiment)))
                 (step (/ (* *push-speed* balance) +ticks-per-second+))
                 (dx (* step (cos facing)))
                 (dy (* step (sin facing))))
            (cond ((minusp balance)
                   (shift-regiment regiment (battle-soldiers battle) dx dy)
                   (lose-morale regiment (/ (* *push-morale-loss* (- balance))
                                            +ticks-per-second+)))
                  ((and (plusp balance)
                        (< (front-lag regiment (battle-soldiers battle)) *push-max-lag*))
                   (shift-regiment regiment (battle-soldiers battle) dx dy
                                   :soldiers-too nil)))))))))

(defun nearest-enemy-regiment (battle regiment)
  (let ((soldiers (battle-soldiers battle))
        (nearest nil) (best most-positive-single-float))
    (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
      (loop for other across (battle-regiments battle)
            when (and (/= (regiment-side other) (regiment-side regiment))
                      (plusp (regiment-alive other)))
              do (multiple-value-bind (ox oy) (regiment-centroid other soldiers)
                   (let ((d (+ (expt (- ox x) 2) (expt (- oy y) 2))))
                     (when (< d best) (setf best d nearest other))))))
    nearest))

(defun rout (battle regiment)
  "Débande REGIMENT : il fuit à l'opposé de l'ennemi le plus proche."
  (let* ((soldiers (battle-soldiers battle))
         (enemy (or (opponent battle regiment) (nearest-enemy-regiment battle regiment))))
    (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
      (multiple-value-bind (ex ey) (if enemy
                                       (regiment-centroid enemy soldiers)
                                       (values (- x 1f0) y))
        (let* ((away (float (atan (- y ey) (- x ex)) 1f0))
               (formation (regiment-formation regiment)))
          (setf (formation-x formation) (+ x (* *rout-distance* (cos away)))
                (formation-y formation) (+ y (* *rout-distance* (sin away)))
                (formation-facing formation) away
                (regiment-orders regiment) (list :rout)
                (regiment-holding-p regiment) nil
                (regiment-slots-dirty-p regiment) t))))))

(defun update-morale (battle regiment)
  "Poussée, remontée du moral hors contact, et débandade sous le seuil."
  (when (and (plusp (regiment-alive regiment)) (not (regiment-routed-p regiment)))
    (push-regiment battle regiment)
    ;; Attaqué de flanc ou de dos (flanks.lisp).
    (let ((loss (flank-morale-loss regiment)))
      (when (plusp loss)
        (lose-morale regiment (/ loss +ticks-per-second+))))
    (when (and (zerop (regiment-engaged regiment))
               (> (- (battle-tick-count battle) (regiment-fire-tick regiment))
                  (* *fire-calm-seconds* +ticks-per-second+)))
      (setf (regiment-morale regiment)
            (min (stats-morale (regiment-stats regiment))
                 (+ (regiment-morale regiment) (/ *morale-recovery* +ticks-per-second+)))))
    (when (< (regiment-morale regiment) *rout-morale*)
      (rout battle regiment))))

(defun scatter-slots (regiment soldiers)
  "Disperse les emplacements de REGIMENT d'au plus *ROUT-SCATTER* mètres par
axe. Le décalage de chaque soldat vient d'un hachage de son indice : il ne
consomme pas le générateur de la bataille et ne change pas d'un recalcul à
l'autre."
  (let ((slot-xs (soldiers-slot-x soldiers))
        (slot-ys (soldiers-slot-y soldiers))
        (scatter *rout-scatter*))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          do (let ((h (logand (* (1+ i) 2654435761) #xffffffff)))
               (incf (aref slot-xs i) (* scatter (1- (/ (ldb (byte 16 0) h) 32767.5f0))))
               (incf (aref slot-ys i) (* scatter (1- (/ (ldb (byte 16 16) h) 32767.5f0))))))))
