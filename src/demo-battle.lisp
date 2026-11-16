(in-package #:rogue-company)

;;; Bataille de démonstration, en attendant la partie : celle que lance « Start
;;; Battle », et que C fait charger. Par défaut, le combat du test
;;; OUTNUMBERED-RODELEROS-STILL-LOSE, 160 piquiers contre 120 rodeleros, avec sa
;;; graine, pour le revoir à l'identique. *FLANK-DEMO-REGIMENTS* garde la
;;; démonstration des flancs. Les tests prennent leurs propres régiments
;;; (REGIMENTS) et dispersent les soldats au départ (SCATTER) pour éprouver le
;;; regroupement et les collisions.

(defparameter *demo-scatter* 0
  "Dispersion initiale des soldats, en mètres : 0, ils partent à leur emplacement.")

(defparameter *demo-seed* 1
  "Graine de la bataille de démonstration lancée par la scène.")

(defparameter *flank-demo-regiments*
  '((0 :pikemen 160 -25 -12 0)
    (0 :pikemen 160 -25 12 0)
    (1 :pikemen 160 25 -12 180)
    (1 :pikemen 160 25 12 180)
    (0 :rodeleros 120 0 -60 90)
    (1 :rodeleros 120 0 60 -90))
  "Démonstration des flancs : deux régiments de piquiers de chaque camp face à
face au centre, et les rodeleros de chaque camp sur une aile, plus près des
piquiers ennemis que de tout autre régiment, pour que C les lance sur leur flanc
pendant que les piques s'affrontent de face.")

(defparameter *melee-demo-regiments*
  '((0 :pikemen 160 -25 -100 0)
    (1 :rodeleros 120 25 -100 180)
    (0 :pikemen 160 -25 -60 0)
    (1 :pikemen 160 25 -60 180)
    (0 :halberdiers 120 -25 -20 0)
    (1 :rodeleros 120 25 -20 180)
    (0 :halberdiers 160 -25 20 0)
    (1 :pikemen 160 25 20 180))
  "Duels de mêlée côte à côte : piques contre rodeleros, piques contre piques,
hallebardes contre rodeleros, hallebardes contre piques.")

(defparameter *demo-regiments*
  '((0 :arquebusiers 120 -40 -75 0)
    (1 :arquebusiers 120 40 -75 180 :fire-by-rank)
    (0 :arquebusiers 120 -40 -25 0)
    (1 :rodeleros 120 40 -25 180)
    (0 :arquebusiers 120 -40 25 0)
    (1 :pikemen 160 40 25 180)
    (0 :arquebusiers 120 -40 75 0)
    (1 :halberdiers 120 40 75 180))
  "Régiments de la démonstration : (camp dotation effectif x y orientation en degrés).
Quatre duels d'arquebusiers côte à côte, du nord au sud, hors de portée de tir
au départ, que C lance chacun sur l'ennemi le plus proche : contre des
arquebusiers (contremarche contre feu par rang), des rodeleros, des piquiers et
des hallebardiers. Un septième élément choisit l'exercice de tir.")

(defparameter *demo-names* '((:pikemen . "Piquiers") (:rodeleros . "Rodeleros")
                              (:halberdiers . "Hallebardiers")
                              (:arquebusiers . "Arquebusiers")
                              (:musketeers . "Mousquetaires"))
  "Nom affiché de chaque dotation.")

(defun make-demo-battle (&key (seed *battle-seed*) (scatter *demo-scatter*)
                              (regiments *demo-regiments*))
  (let ((battle (make-battle :seed seed)))
    (loop for (side dotation count x y degrees . options) in regiments
          do (add-regiment battle (dotation-stats dotation) count
                           :name (format nil "~a ~a"
                                         (or (cdr (assoc dotation *demo-names*))
                                             (string-capitalize dotation))
                                         (if (zerop side) "rouges" "bleus"))
                           :side side :x x :y y :facing (* degrees (/ pi 180))
                           :scatter scatter
                           :drill (if options
                                      (first options)
                                      (and (attack-volley-p (stats-primary (dotation-stats dotation)))
                                           :countermarch))))
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
