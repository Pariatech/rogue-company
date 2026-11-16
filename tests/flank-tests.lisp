(in-package #:rogue-company/tests)

;;; Flancs : côté d'une attaque, armes longues tournées vers l'avant, et moral
;;; d'un régiment pris de flanc.

(def-suite flanks :description "Côté des attaques, armes longues et moral de flanc.")
(in-suite flanks)

(test attack-sector-by-angle
  "Une formation tournée vers l'est : de face à l'est, de flanc au nord et au
sud, de dos à l'ouest."
  (is (= 0 (rogue-company::attack-sector 1f0 0f0 3f0 0.5f0)))
  (is (= 1 (rogue-company::attack-sector 1f0 0f0 0f0 3f0)))
  (is (= 1 (rogue-company::attack-sector 1f0 0f0 0.5f0 -3f0)))
  (is (= 2 (rogue-company::attack-sector 1f0 0f0 -3f0 1f0))))

(defun lone-pikeman-against (x y)
  "Un piquier seul tourné vers l'est, un rodelero immobile en X, Y : la cible
du piquier après un tick."
  (let ((b (rogue-company::make-battle :seed 1)))
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 1
                                 :side 0 :x 0 :y 0 :facing 0)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :rodeleros) 1
                                 :side 1 :x x :y y :facing pi)
    (rogue-company::battle-tick b)
    (aref (rogue-company::soldiers-target (rogue-company::battle-soldiers b)) 0)))

(test pikes-only-point-forward
  "Un piquier abaisse sa pique sur un ennemi devant lui, pas sur un ennemi à
la même distance sur son flanc."
  (is (= 1 (lone-pikeman-against 3 0)))
  (is (= -1 (lone-pikeman-against 0 3))))

(defun pike-duel (&key flank)
  "Deux régiments de piquiers chargés l'un contre l'autre, avec FLANK, 120
rodeleros rouges lancés sur le flanc des bleus. Renvoie la seconde où un camp
se débande et les régiments."
  (let ((b (rogue-company::make-battle :seed 1)))
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 160
                                 :side 0 :x -25 :y 0 :facing 0)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 160
                                 :side 1 :x 25 :y 0 :facing pi)
    (when flank
      (rogue-company::add-regiment b (rogue-company::dotation-stats :rodeleros) 120
                                   :side 0 :x 0 :y -45 :facing (/ pi 2)))
    (rogue-company::order-charge b)
    (loop repeat (* 30 240)
          until (some #'rogue-company::regiment-routed-p (rogue-company::battle-regiments b))
          do (rogue-company::battle-tick b))
    (values (floor (rogue-company::battle-tick-count b) 30) (regiments b))))

(test flanked-pikes-break-sooner
  "Des piquiers pris de flanc par des rodeleros pendant qu'ils combattent de
face se débandent bien avant ceux d'un duel de front, et avec moins de pertes."
  (multiple-value-bind (frontal frontal-regiments) (pike-duel)
    (multiple-value-bind (flanked flanked-regiments) (pike-duel :flank t)
      (is (< flanked (* 0.6 frontal)))
      (let ((blue (second flanked-regiments)))
        (is-true (rogue-company::regiment-routed-p blue))
        (is (> (rogue-company::regiment-alive blue)
               (rogue-company::regiment-alive (find-if #'rogue-company::regiment-routed-p
                                                       frontal-regiments))))))))
