(in-package #:rogue-company/tests)

(def-suite battle :description "Simulation de bataille, sans fenêtre.")
(in-suite battle)

(defun battle (&rest args)
  (apply #'rogue-company::make-battle args))

(defun run-ticks (battle ticks)
  (rogue-company::battle-run battle ticks))

(defun digest (battle)
  (rogue-company::battle-digest battle))

(test ticks-advance-the-count
  (let ((b (battle :seed 1)))
    (is (= 0 (rogue-company::battle-tick-count b)))
    (run-ticks b 37)
    (is (= 37 (rogue-company::battle-tick-count b)))))

(test same-seed-same-digest
  "Deux batailles de même graine ont la même empreinte après 1 000 ticks."
  (is (string= (digest (run-ticks (battle :seed 9) 1000))
               (digest (run-ticks (battle :seed 9) 1000))))
  (is (string/= (digest (run-ticks (battle :seed 9) 1000))
                (digest (run-ticks (battle :seed 10) 1000))))
  (is (string/= (digest (run-ticks (battle :seed 9) 1000))
                (digest (run-ticks (battle :seed 9) 999)))))

(test default-seed
  (is (= rogue-company::*battle-seed* (rogue-company::battle-seed (battle)))))

(test ticks-do-not-allocate
  (let ((b (battle)))
    (run-ticks b 10)
    (let ((before (sb-ext:get-bytes-consed)))
      (run-ticks b 1000)
      (is (< (- (sb-ext:get-bytes-consed) before) 65536)))))

;;; Bataille de démonstration : deux régiments dispersés qui rejoignent leur formation.

(defun demo (&rest args)
  (apply #'rogue-company::make-demo-battle args))

(defun regiments (battle)
  (coerce (rogue-company::battle-regiments battle) 'list))

(defun worst-slot-distance (battle)
  (loop for regiment in (regiments battle)
        maximize (nth-value 1 (rogue-company::regiment-slot-distances
                               regiment (rogue-company::battle-soldiers battle)))))

(test demo-has-two-regiments
  (let ((b (demo)))
    (is (= 2 (length (regiments b))))
    (is (= 280 (rogue-company::soldiers-count (rogue-company::battle-soldiers b))))
    (is (equal '(0 1) (mapcar #'rogue-company::regiment-side (regiments b))))
    (is (> (worst-slot-distance b) 1) "les soldats partent dispersés")))

(test regiments-reach-their-formation
  "Dispersés d'au plus 12 m par axe, à 1,3 m/s : 30 s suffisent."
  (let ((b (run-ticks (demo) (* 30 30))))
    (is (< (worst-slot-distance b) 1f-3))))

(test new-order-reforms-the-regiment
  (let* ((b (run-ticks (demo) (* 30 30)))
         (pikes (first (regiments b)))
         (soldiers (rogue-company::battle-soldiers b)))
    (rogue-company::order-move pikes 10 10 (/ pi 2))
    (run-ticks b 1)
    (is (> (worst-slot-distance b) 1) "les emplacements ont bougé")
    (run-ticks b (* 30 40))
    (is (< (worst-slot-distance b) 1f-3))
    (let ((n (rogue-company::regiment-alive pikes))
          (start (rogue-company::regiment-start pikes)))
      (flet ((mean (array)
               (/ (loop for i from start below (+ start n) sum (aref array i)) n)))
        (is (near 10 (mean (rogue-company::soldiers-x soldiers)) 1f-3))
        (is (near 10 (mean (rogue-company::soldiers-y soldiers)) 1f-3))))))

(test previous-positions-lag-one-tick
  (let* ((b (demo))
         (soldiers (rogue-company::battle-soldiers b))
         (before (copy-seq (rogue-company::soldiers-x soldiers))))
    (run-ticks b 1)
    (is (equalp before (rogue-company::soldiers-prev-x soldiers)))
    (is (not (equalp before (rogue-company::soldiers-x soldiers))))))

(test demo-replays-from-its-seed
  (is (string= (digest (run-ticks (demo :seed 3) 500))
               (digest (run-ticks (demo :seed 3) 500))))
  (is (string/= (digest (run-ticks (demo :seed 3) 500))
                (digest (run-ticks (demo :seed 4) 500)))))

(test soldier-ticks-do-not-allocate
  (let ((b (demo)))
    (run-ticks b 10)
    (let ((before (sb-ext:get-bytes-consed)))
      (run-ticks b 1000)
      (is (< (- (sb-ext:get-bytes-consed) before) 65536)))))

(test random-orders-move-every-regiment
  (let ((b (run-ticks (demo) (* 30 30))))
    (rogue-company::order-random-moves b (rogue-company::make-rng 7 99))
    (is (every (lambda (regiment) (rogue-company::regiment-slots-dirty-p regiment)) (regiments b)))
    (run-ticks b (* 30 90))
    (is (< (worst-slot-distance b) 1f-3))))
