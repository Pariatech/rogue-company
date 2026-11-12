(in-package #:rogue-company/tests)

(def-suite rng :description "Générateur PCG32 à graine.")
(in-suite rng)

(defun take (rng count &optional (function #'rogue-company::rng-next))
  (loop repeat count collect (funcall function rng)))

(test reference-sequence
  "Mêmes sorties que pcg32-demo pour la graine 42 et la suite 54."
  (is (equal '(#xa15c02b7 #x7b47f409 #xba1d3330 #x83d2f293 #xbfa4784b #xcbed606e)
             (take (rogue-company::make-rng 42 54) 6))))

(test same-seed-same-sequence
  (is (equal (take (rogue-company::make-rng 7) 100)
             (take (rogue-company::make-rng 7) 100)))
  (is (not (equal (take (rogue-company::make-rng 7) 10)
                  (take (rogue-company::make-rng 8) 10))))
  (is (not (equal (take (rogue-company::make-rng 7 1) 10)
                  (take (rogue-company::make-rng 7 2) 10)))))

(test copy-continues-identically
  (let ((rng (rogue-company::make-rng 3)))
    (take rng 17)
    (let ((copy (rogue-company::copy-rng rng)))
      (is (equal (take rng 50) (take copy 50))))))

(test below-is-in-range-and-uniform
  (let ((rng (rogue-company::make-rng 11))
        (counts (make-array 6 :initial-element 0)))
    (is (loop repeat 60000
              for value = (rogue-company::rng-below rng 6)
              always (<= 0 value 5)
              do (incf (aref counts value))))
    ;; 10 000 attendus par face ; un écart de 5 % serait anormal.
    (is (every (lambda (count) (< 9500 count 10500)) counts)))
  (let ((rng (rogue-company::make-rng 12)))
    (is (every #'zerop (take rng 20 (lambda (r) (rogue-company::rng-below r 1)))))
    (is (every (lambda (v) (< v (expt 2 32)))
               (take rng 20 (lambda (r) (rogue-company::rng-below r (expt 2 32))))))))

(test floats-are-in-unit-interval
  (let ((rng (rogue-company::make-rng 5)))
    (is (loop repeat 10000
              for x = (rogue-company::rng-float rng)
              always (and (typep x 'single-float) (<= 0f0 x) (< x 1f0))))
    (is (loop repeat 10000
              for y = (rogue-company::rng-range rng -2f0 3f0)
              always (and (<= -2f0 y) (< y 3f0))))))

(test no-allocation
  "Un million de tirages n'alloue rien (à une zone de 64 ko près)."
  (let ((rng (rogue-company::make-rng 1))
        (before (sb-ext:get-bytes-consed)))
    (dotimes (i 1000000)
      (rogue-company::rng-next rng)
      (rogue-company::rng-below rng 1000)
      (rogue-company::rng-float rng)
      (rogue-company::rng-range rng 1f0 2f0))
    (is (< (- (sb-ext:get-bytes-consed) before) 131072))))
