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
