(in-package #:rogue-company/tests)

(def-suite formation :description "Emplacements de formation et tableaux de soldats.")
(in-suite formation)

(defun slots (count &key (ranks 2) (facing 0) (x 0) (y 0) (start 0))
  "Emplacements de COUNT soldats, en liste de (x y), à partir de START."
  (let ((formation (rogue-company::make-formation :x x :y y :facing facing :ranks ranks :spacing 1))
        (xs (rogue-company::make-float-array (+ start count)))
        (ys (rogue-company::make-float-array (+ start count))))
    (rogue-company::place-formation formation count xs ys start)
    (loop for i from start below (+ start count)
          collect (list (aref xs i) (aref ys i)))))

(defun same-points-p (expected actual)
  (and (= (length expected) (length actual))
       (every (lambda (e a) (every (lambda (u v) (near u v 1f-5)) e a)) expected actual)))

(test front-rank-ahead-files-across
  "Face à +x : le premier rang devant, en x ; les files de gauche à droite, en y."
  (is (same-points-p '((0.5 -1) (0.5 0) (0.5 1) (-0.5 -1) (-0.5 0) (-0.5 1))
                     (slots 6))))

(test partial-last-rank-is-centred
  (is (same-points-p '((0.5 -1) (0.5 0) (0.5 1) (-0.5 -0.5) (-0.5 0.5))
                     (slots 5))))

(test rotation-and-centre
  "Face au bas de l'écran (+y), centrée en (10, 20) : la droite du front est vers -x."
  (is (same-points-p '((11 20.5) (10 20.5) (9 20.5) (11 19.5) (10 19.5) (9 19.5))
                     (slots 6 :facing (/ pi 2) :x 10 :y 20))))

(test single-rank-and-start-offset
  (is (same-points-p '((0 -1) (0 0) (0 1)) (slots 3 :ranks 1 :start 4))))

(test reserve-grows-and-keeps-data
  (let ((soldiers (rogue-company::make-soldiers)))
    (is (= 0 (rogue-company::reserve-soldiers soldiers 3)))
    (setf (aref (rogue-company::soldiers-x soldiers) 2) 7f0)
    (is (= 3 (rogue-company::reserve-soldiers soldiers 100)))
    (is (= 103 (rogue-company::soldiers-count soldiers)))
    (is (<= 103 (rogue-company::soldiers-capacity soldiers)))
    (is (= 7f0 (aref (rogue-company::soldiers-x soldiers) 2)))))
