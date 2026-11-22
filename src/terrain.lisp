(in-package #:rogue-company)

;;; Terrain d'une bataille : une carte d'élévation, une hauteur en mètres tous
;;; les *TERRAIN-CELL* mètres, sur la carte carrée de la bataille centrée sur
;;; l'origine. Fixe pendant la bataille. TERRAIN-HEIGHT interpole entre les
;;; points ; une bataille sans terrain (BATTLE-TERRAIN NIL) est plate, à 0 m.
;;;
;;; Les collines (MAKE-HILLS) se tirent d'un générateur à part, de la graine de
;;; la bataille : le même terrain revient avec elle, sans toucher aux tirages de
;;; la simulation.
;;;
;;; Le relief se voit au dessin (terrain-view.lisp) et change la marche des
;;; soldats (SLOPE-SPEED-FACTOR, lu par MOVE-TOWARD-SLOTS) ; ni les charges, ni
;;; le tir, ni le moral ne le lisent encore.

(defparameter *terrain-cell* 2f0 "Écart entre deux points de la carte d'élévation, en mètres.")

(defparameter *terrain-hills* 7 "Nombre de collines d'un terrain tiré au hasard.")

(defparameter *hill-radius* '(25f0 . 70f0)
  "Plus petit et plus grand rayon d'une colline, en mètres (le long de son grand axe).")

(defparameter *hill-height* '(4f0 . 14f0) "Plus petite et plus grande hauteur d'une colline, en mètres.")

(defparameter *hill-elongation* '(1f0 . 2.5f0)
  "Rapport du grand axe au petit axe d'une colline : ronde à 1, crête allongée au-delà.")

(defparameter *hill-spread* 0.42f0
  "Les centres des collines tombent à moins de cette part du côté de la carte de
son centre.")

(defparameter *terrain-rng-stream* 7919
  "Flux du générateur des collines (MAKE-RNG), distinct de celui de la bataille.")

(defclass terrain ()
  ((size :initarg :size :reader terrain-size :type single-float
         :documentation "Côté de la carte, en mètres.")
   (points :initarg :points :reader terrain-points :type fixnum
           :documentation "Points par côté : SIZE / *TERRAIN-CELL* + 1.")
   (heights :initarg :heights :reader terrain-heights :type float-array
            :documentation "Hauteurs, ligne par ligne (y puis x), du coin nord-ouest."))
  (:documentation "Carte d'élévation d'une bataille."))

(defun make-terrain (size function)
  "Terrain de SIZE mètres de côté, la hauteur de chaque point donnée par
FUNCTION (x y) en mètres."
  (let* ((size (float size 1f0))
         (points (1+ (ceiling size *terrain-cell*)))
         (heights (make-array (* points points) :element-type 'single-float))
         (half (* 0.5f0 size)))
    (dotimes (j points)
      (dotimes (i points)
        (setf (aref heights (+ i (* j points)))
              (float (funcall function
                              (- (* i *terrain-cell*) half)
                              (- (* j *terrain-cell*) half))
                     1f0))))
    (make-instance 'terrain :size size :points points :heights heights)))

(defun hill-list (seed size)
  "Collines tirées de SEED pour une carte de SIZE mètres : liste de
(x y hauteur grand-rayon petit-rayon angle)."
  (let ((rng (make-rng seed *terrain-rng-stream*))
        (spread (* *hill-spread* size)))
    (loop repeat *terrain-hills*
          collect (let* ((x (rng-range rng (- spread) spread))
                         (y (rng-range rng (- spread) spread))
                         (height (rng-range rng (car *hill-height*) (cdr *hill-height*)))
                         (radius (rng-range rng (car *hill-radius*) (cdr *hill-radius*)))
                         (elongation (rng-range rng (car *hill-elongation*) (cdr *hill-elongation*)))
                         (angle (rng-range rng 0f0 (float pi 1f0))))
                    (list x y height radius (/ radius elongation) angle)))))

(defun make-hills (&key (seed 1) (size *battlefield-size*))
  "Terrain de collines tirées de SEED : chacune une bosse en cloche, ronde ou
allongée en crête ; les collines voisines s'additionnent."
  (let ((hills (hill-list seed size)))
    (make-terrain size
                  (lambda (x y)
                    (loop for (hx hy height long short angle) in hills
                          sum (let* ((dx (- x hx)) (dy (- y hy))
                                     (c (cos angle)) (s (sin angle))
                                     (u (/ (+ (* dx c) (* dy s)) long))
                                     (v (/ (- (* dy c) (* dx s)) short)))
                                (* height (exp (- (+ (* u u) (* v v)))))))))))

(defun terrain-height (terrain x y)
  "Hauteur du sol en X, Y (mètres), interpolée entre les quatre points voisins ;
0 sans terrain, et au bord de la carte au-delà."
  (if (null terrain)
      0f0
      (let* ((points (terrain-points terrain))
             (half (* 0.5f0 (terrain-size terrain)))
             (last (float (1- points) 1f0))
             (gx (max 0f0 (min last (/ (+ x half) *terrain-cell*))))
             (gy (max 0f0 (min last (/ (+ y half) *terrain-cell*))))
             (i (min (1- (1- points)) (floor gx)))
             (j (min (1- (1- points)) (floor gy)))
             (fx (- gx i)) (fy (- gy j))
             (heights (terrain-heights terrain))
             (k (+ i (* j points))))
        (+ (* (- 1 fy) (+ (* (- 1 fx) (aref heights k)) (* fx (aref heights (1+ k)))))
           (* fy (+ (* (- 1 fx) (aref heights (+ k points)))
                    (* fx (aref heights (+ k points 1)))))))))

(defun terrain-slope (terrain x y)
  "Pente du sol en X, Y : dh/dx et dh/dy, par différences autour du point."
  (let ((d *terrain-cell*))
    (values (/ (- (terrain-height terrain (+ x d) y) (terrain-height terrain (- x d) y)) (* 2 d))
            (/ (- (terrain-height terrain x (+ y d)) (terrain-height terrain x (- y d))) (* 2 d)))))

;;; Pente et marche : un soldat qui monte ralentit, qui descend une pente douce
;;; va un peu plus vite, une pente raide le ralentit dans les deux sens.

(defparameter *slope-steepness* 3.5f0
  "Sévérité de la pente sur la marche (fonction de Tobler : 3,5).")

(defparameter *slope-min-speed* 0.3f0
  "Facteur de vitesse le plus bas, sur la pente la plus raide.")

(defun terrain-gradient (terrain x y)
  "Pente du sol en X, Y : dh/dx et dh/dy, dérivées de l'interpolation dans la
case du point. Sans allocation, pour le déplacement des soldats."
  (declare (optimize (speed 3) (safety 0)) (type terrain terrain) (type single-float x y))
  (let* ((points (terrain-points terrain))
         (cell *terrain-cell*)
         (half (* 0.5f0 (the single-float (terrain-size terrain))))
         (last (float (- points 2) 1f0))
         (gx (max 0f0 (min last (/ (+ x half) cell))))
         (gy (max 0f0 (min last (/ (+ y half) cell))))
         (i (floor gx)) (j (floor gy))
         (fx (- gx i)) (fy (- gy j))
         (heights (terrain-heights terrain))
         (k (+ i (the fixnum (* j points)))))
    (declare (type fixnum points i j k) (type single-float cell half last gx gy fx fy)
             (type float-array heights))
    (let ((h00 (aref heights k)) (h10 (aref heights (1+ k)))
          (h01 (aref heights (+ k points))) (h11 (aref heights (+ k points 1))))
      (values (/ (+ (* (- 1f0 fy) (- h10 h00)) (* fy (- h11 h01))) cell)
              (/ (+ (* (- 1f0 fx) (- h01 h00)) (* fx (- h11 h10))) cell)))))

(defun slope-speed-factor (terrain x y dx dy distance)
  "Facteur de la vitesse d'un soldat en X, Y qui marche de DX, DY (sur
DISTANCE, non nulle) : 1 à plat, selon la pente dans le sens de la marche (la
fonction de Tobler ramenée à 1 sur le plat : un peu plus vite sur une descente
douce, plus lent en montée ou sur une descente raide), jamais sous
*SLOPE-MIN-SPEED*."
  (declare (optimize (speed 3) (safety 0)) (type single-float x y dx dy distance))
  (if (null terrain)
      1f0
      (multiple-value-bind (hx hy) (terrain-gradient terrain x y)
        (declare (type single-float hx hy))
        (let ((grade (/ (+ (* hx dx) (* hy dy)) distance))
              (k *slope-steepness*))
          (declare (type single-float grade k))
          (max (the single-float *slope-min-speed*)
               (exp (* (- k) (- (abs (+ grade 0.05f0)) 0.05f0))))))))
