(in-package #:rogue-company)

;;; Bataille de démonstration, en attendant la partie : deux régiments de
;;; piquiers en formation, face à face. Seul le piquier a un sprite pour
;;; l'instant. Les tests dispersent les soldats au départ (SCATTER) pour
;;; éprouver le regroupement et les collisions.

(defparameter *demo-scatter* 0
  "Dispersion initiale des soldats, en mètres : 0, ils partent à leur emplacement.")

(defun make-demo-battle (&key (seed *battle-seed*) (scatter *demo-scatter*))
  (let ((battle (make-battle :seed seed)))
    (add-regiment battle (dotation-stats :pikemen) 160
                  :name "Piquiers rouges" :side 0 :x -25 :y 0 :facing 0 :scatter scatter)
    (add-regiment battle (dotation-stats :pikemen) 160
                  :name "Piquiers bleus" :side 1 :x 25 :y 0 :facing pi :scatter scatter)
    battle))

(defun order-random-moves (battle rng &key (half-width 30) (half-height 20))
  "Envoie chaque régiment vers un point et une orientation tirés avec RNG, dans
un rectangle centré sur l'origine. RNG n'est pas celui de la bataille : un ordre
est une commande du joueur, pas un tirage de la simulation."
  (let ((half-width (float half-width 1f0))
        (half-height (float half-height 1f0)))
    (loop for regiment across (battle-regiments battle)
          do (order-move regiment
                         (rng-range rng (- half-width) half-width)
                         (rng-range rng (- half-height) half-height)
                         (rng-range rng 0f0 (float (* 2 pi) 1f0))))))

(defun order-charge (battle)
  "Lance chaque régiment sur le régiment ennemi vivant le plus proche."
  (let ((regiments (battle-regiments battle))
        (soldiers (battle-soldiers battle)))
    (loop for regiment across regiments
          when (and (plusp (regiment-alive regiment)) (not (regiment-routed-p regiment)))
            do (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
                 (let ((nearest nil) (best most-positive-single-float))
                   (loop for other across regiments
                         when (and (/= (regiment-side other) (regiment-side regiment))
                                   (plusp (regiment-alive other))
                                   (not (regiment-routed-p other)))
                           do (multiple-value-bind (ox oy) (regiment-centroid other soldiers)
                                (let ((d (+ (expt (- ox x) 2) (expt (- oy y) 2))))
                                  (when (< d best) (setf best d nearest other)))))
                   (when nearest (order-attack regiment nearest)))))))
