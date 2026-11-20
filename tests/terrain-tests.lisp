(in-package #:rogue-company/tests)

;;; Terrain : carte d'élévation et collines tirées de la graine.

(def-suite terrain :description "Carte d'élévation, collines, hauteur interpolée.")
(in-suite terrain)

(test flat-without-terrain
  (is (= 0 (rogue-company::terrain-height nil 12f0 -40f0))))

(test height-interpolates-between-points
  "Sur un plan incliné, l'interpolation retombe sur le plan, même entre deux
points de la carte ; au-delà du bord, la hauteur du bord."
  (let ((terrain (rogue-company::make-terrain 64 (lambda (x y) (+ (* 0.5 x) (* 0.25 y) 10)))))
    (is (near 10 (rogue-company::terrain-height terrain 0f0 0f0) 1e-4))
    (is (near (+ 10 (* 0.5 3.3) (* 0.25 -7.1))
              (rogue-company::terrain-height terrain 3.3f0 -7.1f0) 1e-4))
    (is (near (+ 10 (* 0.5 32)) (rogue-company::terrain-height terrain 500f0 0f0) 1e-4))
    (multiple-value-bind (hx hy) (rogue-company::terrain-slope terrain 5f0 5f0)
      (is (near 0.5 hx 1e-4))
      (is (near 0.25 hy 1e-4)))))

(test hills-come-from-the-seed
  "Mêmes collines pour la même graine, d'autres pour une autre ; des bosses
positives, de quelques mètres."
  (let ((a (rogue-company::make-hills :seed 3 :size 256))
        (b (rogue-company::make-hills :seed 3 :size 256))
        (c (rogue-company::make-hills :seed 4 :size 256)))
    (is (equalp (rogue-company::terrain-heights a) (rogue-company::terrain-heights b)))
    (is (not (equalp (rogue-company::terrain-heights a) (rogue-company::terrain-heights c))))
    (let ((heights (rogue-company::terrain-heights a)))
      (is (<= 0 (reduce #'min heights)))
      (is (< 3 (reduce #'max heights) 40)))))

(test ai-battle-has-hills
  "La bataille contre l'IA a des collines, les mêmes quand elle se rejoue ; le
tirage des collines ne change pas la simulation."
  (let ((a (rogue-company::make-ai-battle :seed 2))
        (b (rogue-company::make-ai-battle :seed 2)))
    (is (rogue-company::battle-terrain a))
    (is (equalp (rogue-company::terrain-heights (rogue-company::battle-terrain a))
                (rogue-company::terrain-heights (rogue-company::battle-terrain b))))
    (let ((flat (let ((rogue-company::*ai-battle-hills* nil))
                  (rogue-company::make-ai-battle :seed 2))))
      (is (null (rogue-company::battle-terrain flat)))
      (loop repeat 60 do (rogue-company::battle-tick a) (rogue-company::battle-tick flat))
      (is (string= (rogue-company::battle-digest a) (rogue-company::battle-digest flat))))))
