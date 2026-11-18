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

(test strike-records-its-tick
  "Un coup note son tick, pour l'animation d'attaque ; un soldat qui n'a jamais
frappé reste très loin dans le passé."
  (let ((b (duel 3)))
    (is (< (soldier-value b #'rogue-company::soldiers-strike-tick 0) -1000))
    (loop repeat (* 30 5)
          until (>= (soldier-value b #'rogue-company::soldiers-strike-tick 0) 0)
          do (rogue-company::battle-tick b))
    (let ((tick (soldier-value b #'rogue-company::soldiers-strike-tick 0)))
      (is (<= 0 tick (rogue-company::battle-tick-count b)) "il a frappé en moins de 5 s"))))

(test bearer-is-in-the-middle-of-the-fourth-rank
  "225 piquiers sur 15 files : le porteur est le 8e du quatrième rang. Avec
moins de rangs, il est au milieu du dernier ; un homme seul n'en a pas."
  (let* ((b (demo))
         (r (first (regiments b)))
         (start (rogue-company::regiment-start r)))
    (flet ((alive (n)
             (setf (rogue-company::regiment-alive r) n)
             (rogue-company::refresh-layout r (rogue-company::battle-soldiers b))))
      (is (= (+ start 45 7) (rogue-company::regiment-bearer r)))
      (alive 35)
      (is (= (+ start 30 2) (rogue-company::regiment-bearer r)) "3e rang, 5 hommes")
      (alive 5)
      (is (= (+ start 2) (rogue-company::regiment-bearer r)))
      (alive 1)
      (is (= -1 (rogue-company::regiment-bearer r))))))

(defun charged-demo (&key (seed rogue-company::*battle-seed*) (seconds 60))
  (let ((b (run-ticks (demo :seed seed) (* 30 30))))
    (rogue-company::order-charge b)
    (run-ticks b (* 30 seconds))))

(test front-ranks-stay-in-contact-as-depth-is-lost
  "Après 45 s de mêlée, chaque régiment a perdu des rangs, mais ses
premiers rangs restent au contact : les fronts, de centre à centre, restent à
moins d'un mètre de plus que la distance d'arrêt (4 m de bord à bord, 4,6 m
de centre à centre)."
  (let* ((b (charged-demo :seconds 45))
         (s (rogue-company::battle-soldiers b))
         (fronts (loop for r in (regiments b)
                       collect (let ((files (rogue-company::formation-files
                                             (rogue-company::regiment-formation r)
                                             (rogue-company::regiment-alive r))))
                                 (/ (loop for i from (rogue-company::regiment-start r)
                                            below (+ (rogue-company::regiment-start r) files)
                                          sum (aref (rogue-company::soldiers-x s) i))
                                    files)))))
    (is-true (every (lambda (r) (< (rogue-company::regiment-depth r) 15)) (regiments b))
             "des rangs perdus")
    (is (< (abs (- (second fronts) (first fronts))) 5.6))))

(test three-ranks-strike
  "Les premiers rangs se serrent à 1,75 m l'un de l'autre : les trois premiers
rangs, à 1,75, 2,75 et 3,75 m, sont à portée de pique (4,5 m) et frappent ; le
quatrième, à 4,75 m, beaucoup moins (seulement quand les fronts se serrent)."
  (let* ((b (charged-demo :seconds 40))
         (tick (rogue-company::battle-tick-count b))
         (r (first (regiments b)))
         (start (rogue-company::regiment-start r))
         (files (rogue-company::formation-files (rogue-company::regiment-formation r)
                                                (rogue-company::regiment-alive r))))
    (flet ((strikers (rank)
             (loop for i from (+ start (* rank files)) below (+ start (* (1+ rank) files))
                   count (> (soldier-value b #'rogue-company::soldiers-strike-tick i)
                            (- tick 90)))))
      (is (> (strikers 0) (floor files 2)) "le premier rang frappe")
      (is (> (strikers 2) (floor files 2)) "le troisième rang frappe")
      (is (< (strikers 3) (floor (strikers 2) 2)) "le quatrième rang frappe peu"))))

(test bearer-never-strikes
  "Pendant toute une mêlée, le porteur d'étendard ne frappe jamais. Vérifié aux
ticks sans mort dans son régiment : une mort décale les soldats, et celui qui
vient de frapper peut glisser à la place du porteur."
  (let* ((b (run-ticks (demo) (* 30 30)))
         (checked 0)
         (struck 0))
    (rogue-company::order-charge b)
    (loop repeat (* 30 60)
          do (let ((before (loop for r in (regiments b)
                                 collect (cons (rogue-company::regiment-bearer r)
                                               (rogue-company::regiment-alive r)))))
               (rogue-company::battle-tick b)
               (loop for r in (regiments b)
                     for (bearer . alive) in before
                     when (and (>= bearer 0) (= alive (rogue-company::regiment-alive r)))
                       do (incf checked)
                          (when (= (soldier-value b #'rogue-company::soldiers-strike-tick bearer)
                                   (1- (rogue-company::battle-tick-count b)))
                            (incf struck)))))
    (is (> checked 1000))
    (is (= 0 struck))))

(test charge-makes-casualties
  (let* ((b (charged-demo :seconds 40))
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

;;; Poussée et moral

(test losses-lower-morale
  "100 hommes, discipline 0,5 : chaque mort coûte *LOSS-MORALE* points de moral."
  (let* ((b (rogue-company::make-battle))
         (r (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 100))
         (hps (rogue-company::soldiers-hp (rogue-company::battle-soldiers b))))
    (dotimes (i 10) (setf (aref hps (* 10 i)) 0f0))
    (rogue-company::apply-damage b)
    (is (= 90 (rogue-company::regiment-alive r)))
    (is (near (- 60 (* 10 rogue-company::*loss-morale*)) (rogue-company::regiment-morale r) 1f-3))))

(test low-morale-routs-away-from-the-enemy
  (let* ((b (run-ticks (demo) (* 30 30)))
         (red (first (regiments b)))
         (soldiers (rogue-company::battle-soldiers b))
         (x0 (rogue-company::regiment-centroid red soldiers)))
    (setf (rogue-company::regiment-morale red) 10f0)
    (run-ticks b 1)
    (is-true (rogue-company::regiment-routed-p red))
    (run-ticks b (* 30 5))
    (is (< (rogue-company::regiment-centroid red soldiers) (- x0 10))
        "les rouges, à gauche des bleus, fuient vers la gauche en courant")))

(test weaker-regiment-is-pushed-back
  "Au contact, des bleus au moral bas reculent et les rouges avancent."
  (let* ((b (charged-demo :seconds 25))
         (soldiers (rogue-company::battle-soldiers b)))
    (destructuring-bind (red blue) (regiments b)
      (setf (rogue-company::regiment-morale red) 60f0
            (rogue-company::regiment-morale blue) 25f0)
      (let ((red-x (rogue-company::regiment-centroid red soldiers))
            (blue-x (rogue-company::regiment-centroid blue soldiers)))
        (run-ticks b (* 30 5))
        (is (> (rogue-company::regiment-centroid blue soldiers) (+ blue-x 0.5)) "les bleus reculent")
        (is (> (rogue-company::regiment-centroid red soldiers) (+ red-x 0.5)) "les rouges suivent")))))

(test charge-ends-in-a-rout
  "Sans renfort, un des deux régiments finit par se débander, et le vainqueur
s'arrête au lieu de le poursuivre."
  (let* ((b (charged-demo :seconds 240))
         (rs (regiments b)))
    (is (= 1 (count-if #'rogue-company::regiment-routed-p rs)))
    (is (null (rogue-company::regiment-orders
               (find-if-not #'rogue-company::regiment-routed-p rs))))))

(test deaths-are-logged
  "Chaque mort entre au journal, au front : son camp, son tick passé, un rang
de mêlée, et une position entre les deux régiments."
  (let* ((b (charged-demo :seconds 60))
         (log (rogue-company::battle-deaths b))
         (lost (loop for r in (regiments b)
                     sum (- (rogue-company::regiment-capacity r) (rogue-company::regiment-alive r)))))
    (is (plusp lost))
    (is (= lost (rogue-company::death-count log)))
    (dotimes (k lost)
      (is (<= 0 (aref (rogue-company::death-side log) k) 1))
      (is (<= (aref (rogue-company::death-tick log) k) (rogue-company::battle-tick-count b)))
      (is (zerop (aref (rogue-company::death-rank log) k)) "seul le premier rang est à portée")
      (is (< (abs (aref (rogue-company::death-x log) k)) 10)))))

(test death-log-wraps
  "Plein, le journal remplace ses plus anciens morts."
  (let ((log (rogue-company::make-death-log 4)))
    (dotimes (k 6)
      (rogue-company::record-death log (float k 1f0) 0f0 0f0 0 k 0))
    (is (= 6 (rogue-company::death-count log)))
    (is (equalp #(4f0 5f0 2f0 3f0) (rogue-company::death-x log)))))

(defun pikes-against-swords (rodeleros &key (seconds 240))
  "225 piquiers chargés par RODELEROS rodeleros, jusqu'à la débandade d'un camp."
  (let ((b (rogue-company::make-battle :seed 1)))
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 225
                                 :side 0 :x -25 :y 0 :facing 0)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :rodeleros) rodeleros
                                 :side 1 :x 25 :y 0 :facing pi)
    (rogue-company::order-charge b)
    (loop repeat (* 30 seconds)
          until (some #'rogue-company::regiment-routed-p (rogue-company::battle-regiments b))
          do (rogue-company::battle-tick b))
    (regiments b)))

(test rodeleros-get-under-the-pikes
  "À nombre égal, les rodeleros passés sous les piques l'emportent : collés au
premier rang, ils n'ont plus en face que des dagues, les rangs de derrière ne
pouvant plus abaisser leur pique sur eux."
  (destructuring-bind (pikes swords) (pikes-against-swords 225)
    (is-true (rogue-company::regiment-routed-p pikes))
    (is-false (rogue-company::regiment-routed-p swords))))

(test outnumbered-rodeleros-still-lose
  "À 120 contre 225, les piquiers tiennent."
  (destructuring-bind (pikes swords) (pikes-against-swords 120)
    (is-false (rogue-company::regiment-routed-p pikes))
    (is-true (rogue-company::regiment-routed-p swords))))

(test loss-keeps-files
  "Un mort au premier rang : sa file avance d'un rang derrière lui et le trou
reste en queue de file. Aucun soldat ne change de file."
  (let* ((b (rogue-company::make-battle))
         (r (rogue-company::add-regiment b (rogue-company::dotation-stats :arquebusiers) 28
                                         :drill :countermarch :ranks 6))
         (so (rogue-company::battle-soldiers b))
         (hp (rogue-company::soldiers-hp so))
         (ranks (rogue-company::soldiers-rank so))
         (files (rogue-company::soldiers-file so))
         (start (rogue-company::regiment-start r))
         (before (make-hash-table)))
    ;; Étiquette de chaque soldat : son indice de départ + 1, dans ses points
    ;; de vie ; 28 soldats sur 5 files, dernier rang de 3 centré (files 1 à 3).
    (dotimes (k 28)
      (setf (aref hp (+ start k)) (float (1+ k))
            (gethash (1+ k) before) (aref files (+ start k))))
    (is (equal '(1 2 3) (loop for k from 25 below 28 collect (aref files (+ start k)))))
    ;; Mort en tête de la file 4 (n° 5) : 10, 15, 20, 25 avancent d'un rang.
    (rogue-company::remove-soldier r so (+ start 4))
    (is (= 27 (rogue-company::regiment-alive r)))
    (is (equal '((0 10) (1 15) (2 20) (3 25))
               (loop for i from start below (+ start 27)
                     when (= 4 (aref files i))
                       collect (list (aref ranks i) (round (aref hp i))))))
    (is-true (loop for i from start below (+ start 27)
                   always (= (aref files i) (gethash (round (aref hp i)) before)))
             "personne ne change de file")
    (is (= 6 (rogue-company::regiment-depth r)) "le dernier rang reste")))
