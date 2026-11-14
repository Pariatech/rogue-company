(in-package #:rogue-company)

;;; Bataille de démonstration, en attendant la partie : des piquiers et des
;;; mousquetaires dispersés au hasard, qui rejoignent leur formation face à face.

(defparameter *demo-scatter* 12 "Dispersion initiale des soldats, en mètres.")

(defun make-demo-battle (&key (seed *battle-seed*))
  (let ((battle (make-battle :seed seed)))
    (add-regiment battle (dotation-stats :pikemen) 160
                  :name "Piquiers" :side 0 :x -15 :y 0 :facing 0 :scatter *demo-scatter*)
    (add-regiment battle (dotation-stats :musketeers) 120
                  :name "Mousquetaires" :side 1 :x 15 :y 0 :facing pi :scatter *demo-scatter*)
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
