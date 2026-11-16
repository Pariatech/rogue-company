(in-package #:rogue-company)

;;; Bataille de démonstration, en attendant la partie : chaque camp aligne ses
;;; régiments du nord au sud, face à face. Par défaut, piquiers et rodeleros
;;; (épée et rondache) croisés : les piquiers rouges font face aux rodeleros
;;; bleus, et l'inverse, pour que C lance chaque arme contre l'autre. Les tests
;;; dispersent les soldats au départ (SCATTER) pour éprouver le regroupement et
;;; les collisions, et prennent leurs propres régiments (RED, BLUE).

(defparameter *demo-scatter* 0
  "Dispersion initiale des soldats, en mètres : 0, ils partent à leur emplacement.")

(defparameter *demo-red* '((:pikemen 160) (:rodeleros 120))
  "Régiments rouges de la démonstration, du nord au sud : (dotation effectif).")

(defparameter *demo-blue* '((:rodeleros 120) (:pikemen 160))
  "Régiments bleus de la démonstration, du nord au sud.")

(defparameter *demo-names* '((:pikemen . "Piquiers") (:rodeleros . "Rodeleros")
                              (:musketeers . "Mousquetaires"))
  "Nom affiché de chaque dotation.")

(defparameter *demo-gap* 44
  "Écart nord-sud entre deux régiments d'un même camp, en mètres.")

(defun make-demo-battle (&key (seed *battle-seed*) (scatter *demo-scatter*)
                              (red *demo-red*) (blue *demo-blue*))
  (let ((battle (make-battle :seed seed)))
    (loop for (regiments side x facing name) in `((,red 0 -25 0 "rouges") (,blue 1 25 ,pi "bleus"))
          do (loop for (dotation count) in regiments
                   for k from 0
                   do (add-regiment battle (dotation-stats dotation) count
                                    :name (format nil "~a ~a"
                                                  (or (cdr (assoc dotation *demo-names*))
                                                      (string-capitalize dotation))
                                                  name)
                                    :side side :x x
                                    :y (* *demo-gap* (- k (/ (1- (length regiments)) 2)))
                                    :facing facing :scatter scatter)))
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
