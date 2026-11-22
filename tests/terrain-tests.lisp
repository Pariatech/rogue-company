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
  "La bataille contre l'IA a des collines, les mêmes quand elle se rejoue ; elle
se rejoue à l'identique, et la pente change la marche."
  (let ((a (rogue-company::make-ai-battle :seed 2))
        (b (rogue-company::make-ai-battle :seed 2)))
    (is (rogue-company::battle-terrain a))
    (is (equalp (rogue-company::terrain-heights (rogue-company::battle-terrain a))
                (rogue-company::terrain-heights (rogue-company::battle-terrain b))))
    (let ((flat (let ((rogue-company::*ai-battle-hills* nil))
                  (rogue-company::make-ai-battle :seed 2))))
      (is (null (rogue-company::battle-terrain flat)))
      (loop repeat 150 do (rogue-company::battle-tick a)
                          (rogue-company::battle-tick b)
                          (rogue-company::battle-tick flat))
      (is (string= (rogue-company::battle-digest a) (rogue-company::battle-digest b)))
      (is (string/= (rogue-company::battle-digest a) (rogue-company::battle-digest flat))))))

(test slope-slows-the-climb
  "Sur une pente de 20 % (h = 0,2 x), monter est plus lent que sur le plat,
descendre doucement un peu plus rapide, et la pente de travers ne change rien."
  (let ((terrain (rogue-company::make-terrain 100 (lambda (x y) (declare (ignore y)) (* 0.2 (+ x 50)))))
        (gentle (rogue-company::make-terrain 100 (lambda (x y) (declare (ignore y)) (* 0.04 (+ x 50))))))
    (multiple-value-bind (hx hy) (rogue-company::terrain-gradient terrain 3.3 -7.1)
      (is (< (abs (- hx 0.2)) 1e-4))
      (is (< (abs hy) 1e-4)))
    (is (= 1f0 (rogue-company::slope-speed-factor nil 0f0 0f0 1f0 0f0 1f0)))
    (let ((up (rogue-company::slope-speed-factor terrain 0f0 0f0 1f0 0f0 1f0))
          (down-gentle (rogue-company::slope-speed-factor gentle 0f0 0f0 -1f0 0f0 1f0))
          (across (rogue-company::slope-speed-factor terrain 0f0 0f0 0f0 1f0 1f0)))
      (is (< 0.4 up 0.6))
      (is (< 1f0 down-gentle 1.2))
      (is (< (abs (- across 1f0)) 1e-4)))))

(test contours-follow-the-levels
  "Sur une pente régulière (h = x + 20), les courbes de niveau sont des droites
x = niveau - 20, tous les 2 m, maîtresses tous les 10 m."
  (let* ((terrain (rogue-company::make-terrain 40 (lambda (x y) (declare (ignore y)) (+ x 20))))
         (segments (rogue-company::contour-segments terrain :interval 2f0 :major 10f0))
         (levels '()))
    (is (plusp (length segments)))
    (loop for k from 0 below (length segments) by 5
          do (let ((x0 (aref segments k)) (x1 (aref segments (+ k 2)))
                   (major (aref segments (+ k 4))))
               (is (< (abs (- x0 x1)) 1f-3))
               (let ((level (+ x0 20)))
                 (is (< (abs (- level (* 2 (round level 2)))) 1f-3))
                 (is (eql (if (zerop (mod (round level) 10)) 1f0 0f0) major))
                 (pushnew (round level) levels))))
    (is (equal (loop for l from 2 to 40 by 2 collect l) (sort levels #'<)))))

(test grass-tufts-are-drawn
  "Chaque image de touffe a des brins, et les touffes reviennent les mêmes."
  (let ((pixels (rogue-company::grass-pixels))
        (w rogue-company::*grass-width*))
    (is (equal (list rogue-company::*grass-height* (* w rogue-company::*grass-variants*))
               (array-dimensions pixels)))
    (dotimes (v rogue-company::*grass-variants*)
      (is (loop for y below (array-dimension pixels 0)
                thereis (loop for x from (* v w) below (* (1+ v) w)
                              thereis (aref pixels y x)))))
    (is (equalp pixels (rogue-company::grass-pixels)))
    (is (= (rogue-company::cell-hash 12 34 1) (rogue-company::cell-hash 12 34 1)))))
