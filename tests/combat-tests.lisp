(in-package #:rogue-company/tests)

(def-suite combat :description "Ciblage, coups, morts et engagement.")
(in-suite combat)

(defun duel (distance &key (dotation :pikemen))
  "Deux soldats de camps opposés, à DISTANCE mètres l'un de l'autre sur l'axe x."
  (let ((b (rogue-company::make-battle))
        (stats (rogue-company::dotation-stats dotation)))
    (rogue-company::add-regiment b stats 1 :side 0 :x 0 :y 0)
    (rogue-company::add-regiment b stats 1 :side 1 :x distance :y 0 :facing pi)
    b))

(defun soldier-value (battle reader i)
  (aref (funcall reader (rogue-company::battle-soldiers battle)) i))

(test pike-reach-targets
  "À 3 m (2,4 m de bord à bord), chacun vise l'autre avec sa pique."
  (let ((b (run-ticks (duel 3) 1)))
    (is (= 1 (soldier-value b #'rogue-company::soldiers-target 0)))
    (is (= 0 (soldier-value b #'rogue-company::soldiers-target 1)))
    (is (= 0 (soldier-value b #'rogue-company::soldiers-weapon 0)))))

(test too-close-for-the-pike
  "À 0,9 m, trop près pour la pique : la dague, après son temps de mise en main."
  (let ((b (run-ticks (duel 0.9) 1)))
    (is (= 1 (soldier-value b #'rogue-company::soldiers-target 0)))
    (is (= 1 (soldier-value b #'rogue-company::soldiers-weapon 0)))
    (is (= 15 (soldier-value b #'rogue-company::soldiers-switch-timer 0)) "0,5 s")))

(test out-of-reach
  (let ((b (run-ticks (duel 10) 1)))
    (is (= -1 (soldier-value b #'rogue-company::soldiers-target 0)))
    (is (= 0 (rogue-company::regiment-engaged (first (regiments b)))))))

(test duel-ends-with-a-death
  "Deux piquiers à portée finissent par s'entretuer : un mort, crédité à l'autre."
  (let* ((b (run-ticks (duel 3) (* 30 120)))
         (alive (mapcar #'rogue-company::regiment-alive (regiments b)))
         (kills (mapcar #'rogue-company::regiment-kills (regiments b))))
    (is (= 1 (count 0 alive)) "exactement un mort")
    (is (equal (reverse kills) (mapcar (lambda (a) (- 1 a)) alive)) "le tueur est crédité")))

(defun charged-demo (&key (seed rogue-company::*battle-seed*) (seconds 60))
  (let ((b (run-ticks (demo :seed seed) (* 30 30))))
    (rogue-company::order-charge b)
    (run-ticks b (* 30 seconds))))

(test charge-makes-casualties
  (let* ((b (charged-demo))
         (rs (regiments b))
         (deaths (loop for r in rs sum (- (rogue-company::regiment-capacity r)
                                           (rogue-company::regiment-alive r)))))
    (is (plusp deaths))
    (is (= deaths (loop for r in rs sum (rogue-company::regiment-kills r))) "chaque mort a un tueur")
    (is-true (every #'rogue-company::regiment-holding-p rs) "les deux régiments se sont arrêtés")))

(test charged-regiments-do-not-interpenetrate
  "Au contact, les deux blocs restent séparés : leurs centres sont à plus de 8 m."
  (let* ((b (charged-demo :seconds 30))
         (soldiers (rogue-company::battle-soldiers b)))
    (destructuring-bind (red blue) (regiments b)
      (let ((rx (rogue-company::regiment-centroid red soldiers))
            (bx (rogue-company::regiment-centroid blue soldiers)))
        (is (> (abs (- bx rx)) 8))))))

(test dead-are-out-of-the-grid
  (let* ((b (charged-demo))
         (grid (rogue-company::battle-grid b)))
    (is (= (loop for r in (regiments b) sum (rogue-company::regiment-alive r))
           (rogue-company::grid-size grid)))))

(test combat-replays-from-its-seed
  (is (string= (digest (charged-demo :seed 5 :seconds 20))
               (digest (charged-demo :seed 5 :seconds 20)))))

(test combat-does-not-allocate
  (let ((b (charged-demo :seconds 10)))
    (let ((before (sb-ext:get-bytes-consed)))
      (run-ticks b 300)
      (is (< (- (sb-ext:get-bytes-consed) before) 65536)))))

(test casualty-replaced-by-the-man-behind
  "12 soldats en 3 rangs de 4. Le n° 1 (premier rang, 2e file) meurt : le n° 5,
derrière lui, avance ; le n° 9, derrière encore, le remplace ; le dernier rang
se resserre (10 et 11 glissent d'une place). Le front garde ses 4 files."
  (let* ((b (rogue-company::make-battle))
         (r (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 12 :ranks 3))
         (soldiers (rogue-company::battle-soldiers b))
         (xs (rogue-company::soldiers-x soldiers)))
    ;; Chaque soldat porte son numéro d'origine dans x.
    (dotimes (i 12) (setf (aref xs i) (float i 1f0)))
    (setf (aref (rogue-company::soldiers-hp soldiers) 1) 0f0)
    (rogue-company::apply-damage b)
    (is (= 11 (rogue-company::regiment-alive r)))
    (is (equal '(0 5 2 3  4 9 6 7  8 10 11)
               (loop for i below 11 collect (round (aref xs i)))))
    (is (= 4 (rogue-company::formation-files (rogue-company::regiment-formation r) 11)))))
