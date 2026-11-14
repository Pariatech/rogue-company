(in-package #:rogue-company/tests)

(def-suite collision :description "Grille spatiale et collisions entre soldats.")
(in-suite collision)

(defun lone-soldiers (&rest specs)
  "Bataille d'un soldat par régiment. Chaque SPEC est (dotation x y)."
  (let ((b (rogue-company::make-battle)))
    (loop for (dotation x y) in specs
          do (rogue-company::add-regiment b (rogue-company::dotation-stats dotation) 1 :x x :y y))
    b))

(defun collide (battle &optional (passes 1))
  "Collisions seules, sans marche vers l'emplacement."
  (let ((soldiers (rogue-company::battle-soldiers battle))
        (grid (rogue-company::battle-grid battle)))
    (dotimes (i passes battle)
      (rogue-company::rebuild-grid grid soldiers)
      (rogue-company::resolve-collisions soldiers grid (rogue-company::battle-max-radius battle)))))

(defun position-of (battle i)
  (let ((soldiers (rogue-company::battle-soldiers battle)))
    (list (aref (rogue-company::soldiers-x soldiers) i)
          (aref (rogue-company::soldiers-y soldiers) i))))

(defun gap (battle i j)
  (destructuring-bind ((xi yi) (xj yj)) (list (position-of battle i) (position-of battle j))
    (sqrt (+ (expt (- xj xi) 2) (expt (- yj yi) 2)))))

(defun smallest-gap (battle)
  "Plus petite distance entre deux soldats, par force brute."
  (let ((n (rogue-company::soldiers-count (rogue-company::battle-soldiers battle))))
    (loop for i below (1- n)
          minimize (loop for j from (1+ i) below n minimize (gap battle i j)))))

(test grid-sorts-soldiers-by-cell
  (let* ((b (lone-soldiers '(:pikemen 0.5 0.5) '(:pikemen 10.5 0.5) '(:pikemen 0.6 0.4)
                           '(:pikemen 5000 -5000)))
         (grid (rogue-company::battle-grid b)))
    (rogue-company::rebuild-grid grid (rogue-company::battle-soldiers b))
    (let* ((cells (rogue-company::grid-soldier-cell grid))
           (start (rogue-company::grid-cell-start grid))
           (sorted (rogue-company::grid-sorted grid))
           (columns (rogue-company::grid-columns grid))
           (cell (aref cells 0)))
      (is (= cell (aref cells 2)) "0,5 et 0,6 sont dans la même case")
      (is (/= cell (aref cells 1)))
      (is (equal '(0 2) (sort (loop for k from (aref start cell) below (aref start (1+ cell))
                                    collect (aref sorted k))
                              #'<)))
      (is (= (1- columns) (mod (aref cells 3) columns)) "hors de la carte : colonne du bord")
      (is (= 0 (floor (aref cells 3) columns)) "hors de la carte : ligne du bord"))))

(test overlapping-soldiers-separate
  (let ((b (lone-soldiers '(:pikemen 0 0) '(:pikemen 0.2 0))))
    (collide b 40)
    (is (near 0.6 (gap b 0 1) 1f-3))
    (is (near 0.2 (+ (first (position-of b 0)) (first (position-of b 1))) 1f-4)
        "même masse : chacun s'écarte d'autant, le milieu ne bouge pas")))

(test soldiers-on-the-same-point-separate
  (let ((b (lone-soldiers '(:pikemen 3 3) '(:pikemen 3 3))))
    (collide b 40)
    (is (near 0.6 (gap b 0 1) 1f-3))))

(test heavier-soldier-moves-less
  (let ((b (lone-soldiers '(:lancers 0 0) '(:pikemen 1.0 0))))
    (collide b 40)
    (is (near 1.3 (gap b 0 1) 1f-3) "rayons 1,0 + 0,3")
    (is (< (abs (first (position-of b 0))) (abs (- (first (position-of b 1)) 1.0))))))

(test distant-soldiers-do-not-move
  (let ((b (lone-soldiers '(:pikemen 0 0) '(:pikemen 0.61 0) '(:lancers 5 5))))
    (collide b 5)
    (is (equal '(0.0 0.0) (position-of b 0)))
    (is (equal '(0.61 0.0) (position-of b 1)))))

(test demo-ends-without-overlap
  "Partis dispersés et superposés, les soldats finissent en formation sans se chevaucher."
  (let ((b (run-ticks (demo) (* 30 30))))
    (is (>= (smallest-gap b) 0.59))))

(test collisions-do-not-allocate
  (let ((b (demo)))
    (run-ticks b 10)
    (let ((before (sb-ext:get-bytes-consed)))
      (collide b 1000)
      (is (< (- (sb-ext:get-bytes-consed) before) 65536)))))
