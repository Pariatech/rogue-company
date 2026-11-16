(in-package #:rogue-company)

;;; Bataille de démonstration, en attendant la partie. Par défaut, les piquiers
;;; des deux camps se font face au centre ; les rodeleros de chaque camp
;;; attendent sur une aile, assez près des piquiers ennemis pour que C les lance
;;; sur leur flanc pendant que les piques s'affrontent de face. Les tests
;;; prennent leurs propres régiments (REGIMENTS) et dispersent les soldats au
;;; départ (SCATTER) pour éprouver le regroupement et les collisions.

(defparameter *demo-scatter* 0
  "Dispersion initiale des soldats, en mètres : 0, ils partent à leur emplacement.")

(defparameter *demo-regiments*
  '((0 :pikemen 160 -25 0 0)
    (1 :pikemen 160 25 0 180)
    (0 :rodeleros 120 0 -45 90)
    (1 :rodeleros 120 0 45 -90))
  "Régiments de la démonstration : (camp dotation effectif x y orientation en degrés).")

(defparameter *demo-names* '((:pikemen . "Piquiers") (:rodeleros . "Rodeleros")
                              (:musketeers . "Mousquetaires"))
  "Nom affiché de chaque dotation.")

(defun make-demo-battle (&key (seed *battle-seed*) (scatter *demo-scatter*)
                              (regiments *demo-regiments*))
  (let ((battle (make-battle :seed seed)))
    (loop for (side dotation count x y degrees) in regiments
          do (add-regiment battle (dotation-stats dotation) count
                           :name (format nil "~a ~a"
                                         (or (cdr (assoc dotation *demo-names*))
                                             (string-capitalize dotation))
                                         (if (zerop side) "rouges" "bleus"))
                           :side side :x x :y y :facing (* degrees (/ pi 180))
                           :scatter scatter))
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
