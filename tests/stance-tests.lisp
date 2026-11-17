(in-package #:rogue-company/tests)

(def-suite stances :description "Postures : poursuite et repli des tirailleurs.")
(in-suite stances)

(defun stance-battle (&rest entries)
  "Bataille de démonstration aux régiments ENTRIES, tous lancés sur l'ennemi le
plus proche."
  (let ((b (rogue-company::make-demo-battle :seed 1 :regiments entries)))
    (rogue-company::order-charge b)
    b))

(test skirmishers-fall-back-then-fire
  "Des arquebusiers en tirailleurs se replient devant des rodeleros, puis
reprennent leur ordre d'attaque quand ceux-ci s'accrochent aux piques."
  (let* ((b (stance-battle '(0 :arquebusiers 120 -40 0 0 nil :skirmish)
                           '(0 :pikemen 160 -56 0 0 nil :chase)
                           '(1 :rodeleros 120 40 0 180)))
         (arquebusiers (first (regiments b)))
         (retreated nil))
    (dotimes (i (* 30 40))
      (rogue-company::battle-tick b)
      (when (eq :retreat (first (rogue-company::regiment-orders arquebusiers)))
        (setf retreated t)))
    (is-true retreated)
    (is (eq :attack (first (rogue-company::regiment-orders arquebusiers))))
    (is (= 120 (rogue-company::regiment-alive arquebusiers)) "aucun rattrapé")))

(test hold-stops-chase-pursues
  "Contre des rodeleros débandés, des piques en :HOLD s'arrêtent ; en :CHASE,
elles les poursuivent et en tuent davantage, sur au plus *CHASE-DISTANCE*."
  (flet ((kills-after-rout (stance)
           (let* ((b (stance-battle `(0 :pikemen 160 -10 0 0 nil ,stance)
                                    '(1 :rodeleros 120 10 0 180)))
                  (pikes (first (regiments b)))
                  (rodeleros (second (regiments b))))
             ;; Les rodeleros se débandent vite : moral au plancher.
             (setf (rogue-company::regiment-morale rodeleros) 21f0)
             (loop repeat (* 30 20) until (rogue-company::regiment-routed-p rodeleros)
                   do (rogue-company::battle-tick b))
             (let ((before (rogue-company::regiment-kills pikes))
                   (x0 (rogue-company::regiment-centroid pikes (rogue-company::battle-soldiers b))))
               (run-ticks b (* 30 90))
               (values (- (rogue-company::regiment-kills pikes) before)
                       (- (rogue-company::regiment-centroid pikes (rogue-company::battle-soldiers b)) x0)
                       (rogue-company::regiment-chasing-p pikes))))))
    (multiple-value-bind (hold-kills hold-moved) (kills-after-rout :hold)
      (multiple-value-bind (chase-kills chase-moved chasing) (kills-after-rout :chase)
        (is (< hold-moved 5))
        (is (> chase-kills hold-kills))
        (is (< 20 chase-moved (+ rogue-company::*chase-distance* 10)))
        (is-false chasing "reformé après la poursuite")))))
