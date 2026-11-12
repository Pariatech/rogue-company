(in-package #:rogue-company/tests)

(def-suite fixed-clock :description "Horloge à pas fixe et vitesse de simulation.")
(in-suite fixed-clock)

;;; Les durées sont des multiples ou des moitiés exactes de TICK-DT, pour que
;;; les sommes de flottants tombent juste.

(defun clock (&rest args)
  (apply #'rogue-company::make-fixed-clock args))

(defun consume (clock dt)
  (rogue-company::clock-consume clock dt))

(defun tick-dt (clock)
  (rogue-company::clock-tick-dt clock))

(defun near (a b &optional (tolerance 1f-5))
  (< (abs (- a b)) tolerance))

(defun total-ticks (clock dt frames)
  (loop repeat frames sum (consume clock dt)))

(test half-ticks-accumulate
  (let ((c (clock)))
    (is (= 30 (total-ticks c (/ (tick-dt c) 2) 60)))
    (is (zerop (rogue-company::clock-accumulator c)))))

(test sixty-frames-per-second
  "60 images de 1/60 s font une seconde : 30 ticks, à l'arrondi des flottants près."
  (let ((c (clock)))
    (is (<= 29 (total-ticks c (/ 1f0 60) 60) 30))))

(test several-ticks-in-one-frame
  (let ((c (clock)))
    (is (= 3 (consume c (* 3 (tick-dt c)))))
    (is (= 0 (consume c (/ (tick-dt c) 4))))))

(test time-scale-speeds-up-and-slows-down
  (let ((fast (clock :time-scale 2))
        (slow (clock :time-scale 0.5)))
    (is (= 60 (total-ticks fast (/ (tick-dt fast) 2) 60)))
    (is (= 15 (total-ticks slow (/ (tick-dt slow) 2) 60)))))

(test time-scale-zero-pauses
  (let ((c (clock)))
    (consume c (/ (tick-dt c) 2))
    (let ((alpha (rogue-company::clock-alpha c)))
      (setf (rogue-company::clock-time-scale c) 0)
      (is-true (rogue-company::clock-paused-p c))
      (is (= 0 (total-ticks c 0.1 100)))
      (is (= alpha (rogue-company::clock-alpha c))))
    (setf (rogue-company::clock-time-scale c) 1)
    (is (= 1 (consume c (/ (tick-dt c) 2))))))

(test time-scale-is-validated
  (let ((c (clock)))
    (signals error (setf (rogue-company::clock-time-scale c) -1))
    (signals error (setf (rogue-company::clock-time-scale c) 1000))
    (signals error (setf (rogue-company::clock-time-scale c) "2"))
    (is (= 1f0 (rogue-company::clock-time-scale c)))))

(defun unconsumed (clock)
  "Temps abandonné plus temps accumulé : ce que l'horloge n'a pas rendu en ticks."
  (+ (rogue-company::clock-dropped clock) (rogue-company::clock-accumulator clock)))

;;; Les soustractions répétées de TICK-DT laissent un reste d'arrondi : le dernier
;;; tick peut rester dans l'accumulateur au lieu d'être abandonné. Le total
;;; abandonné + accumulé, lui, est conservé.

(test cap-drops-backlog
  (let ((c (clock :max-ticks 5)))
    (is (= 5 (consume c (* 10 (tick-dt c)))))
    (is (near (* 5 (tick-dt c)) (unconsumed c)))
    (is (< (rogue-company::clock-accumulator c) (tick-dt c)))))

(test cap-follows-time-scale
  (let ((c (clock :max-ticks 5 :time-scale 3)))
    (is (= 15 (consume c (* 10 (tick-dt c)))))
    (is (near (* 15 (tick-dt c)) (unconsumed c)))))

(test alpha-stays-in-unit-interval
  (let ((c (clock)))
    (is (loop repeat 1000
              do (consume c 0.0123)
              always (<= 0 (rogue-company::clock-alpha c))
              always (< (rogue-company::clock-alpha c) 1)))))

(test reset-forgets-time-but-keeps-scale
  (let ((c (clock :time-scale 2)))
    (consume c (* 100 (tick-dt c)))
    (rogue-company::clock-reset c)
    (is (zerop (rogue-company::clock-accumulator c)))
    (is (zerop (rogue-company::clock-dropped c)))
    (is (= 2f0 (rogue-company::clock-time-scale c)))))

(test no-allocation
  (let ((c (clock))
        (before (sb-ext:get-bytes-consed)))
    (dotimes (i 1000000)
      (consume c (/ 1f0 60))
      (rogue-company::clock-alpha c))
    (is (< (- (sb-ext:get-bytes-consed) before) 131072))))
