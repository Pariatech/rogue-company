(in-package #:rogue-company)

;;; Végétation : des touffes d'herbe en pixel art sur le sol, de près seulement
;;; (zoom des morceaux des soldats, CAMERA-SPRITE-SCALE), à l'échelle des
;;; soldats.
;;;
;;; Les touffes sont dessinées par le code au premier usage : *GRASS-VARIANTS*
;;; images de *GRASS-WIDTH* × *GRASS-HEIGHT* pixels côte à côte dans une
;;; texture, filtre au plus proche. Rien n'est stocké par touffe : chaque
;;; case de *GRASS-CELL* mètres du terrain a peut-être une touffe, sa place et
;;; son image tirées d'un hachage de la case, jamais du générateur de la
;;; bataille. Teinte de chaque touffe : la luminosité du sol ombré sous elle
;;; (*GRASS-LIGHT*, une valeur par case, calculée une fois par terrain).

(defparameter *draw-grass* t "Faux : pas de végétation.")

(defparameter *grass-cell* 0.8f0 "Côté d'une case de végétation, en mètres.")

(defparameter *grass-density* 0.85f0 "Part des cases qui ont une touffe.")

(defparameter *grass-variants* 12 "Nombre d'images de touffes différentes.")

(defparameter *grass-width* 11 "Largeur d'une image de touffe, en pixels de sprite.")

(defparameter *grass-height* 9 "Hauteur d'une image de touffe, en pixels de sprite.")

(defparameter *grass-flower-chance* 0.25f0 "Part des touffes fleuries.")

(defparameter *grass-palette*
  '((48 74 34) (64 94 42) (82 112 50) (108 136 60) (138 152 72))
  "Verts des brins, de la base (sombre) à la pointe (claire).")

(defparameter *flower-colors* '((236 214 86) (240 240 232) (178 130 200) (214 96 80))
  "Couleurs des fleurs.")

(defvar *grass-texture* nil "Identifiant rlgl de la texture des touffes.")

(defvar *grass-light* nil "(terrain . luminosités) : le sol sous chaque case de végétation.")

(defun grass-pixels ()
  "Les touffes en pixels (r g b a), image après image, ligne par ligne :
tableau de largeur *GRASS-VARIANTS* × *GRASS-WIDTH*."
  (let* ((w *grass-width*) (h *grass-height*) (n *grass-variants*)
         (stride (* n w))
         (pixels (make-array (list h stride) :initial-element nil))
         (rng (make-rng 1609 31))
         (palette (coerce *grass-palette* 'vector)))
    (dotimes (v n)
      (let ((x0 (* v w)))
        (dotimes (blade (+ 3 (rng-below rng 4)))
          (let* ((base (+ (floor w 2) (- (rng-below rng 7) 3)))
                 (tall (+ 3 (rng-below rng (- h 2))))
                 (lean (- (rng-float rng) 0.5f0))
                 (tip-x nil) (tip-y nil))
            (dotimes (k tall)
              ;; Du pied (k = 0) à la pointe ; le brin penche de plus en plus.
              (let* ((y (- h 1 k))
                     (x (+ base (round (* lean 3 (/ (* k k) (float (* tall tall))))))))
                (when (< -1 x w)
                  (setf (aref pixels y (+ x0 x))
                        (aref palette (min (1- (length palette))
                                           (floor (* (length palette) (/ k (float tall))))))
                        tip-x x tip-y y))))
            (when (and tip-x (< (rng-float rng) (/ *grass-flower-chance* 3)))
              (setf (aref pixels tip-y (+ x0 tip-x))
                    (nth (rng-below rng (length *flower-colors*)) *flower-colors*)))))))
    pixels))

(defun build-grass-texture ()
  "Charge la texture des touffes et renvoie son identifiant. Thread principal."
  (let* ((pixels (grass-pixels))
         (h (array-dimension pixels 0))
         (w (array-dimension pixels 1)))
    (cffi:with-foreign-object (data :uint8 (* w h 4))
      (dotimes (y h)
        (dotimes (x w)
          (let ((color (aref pixels y x))
                (k (* 4 (+ x (* y w)))))
            (loop for offset from 0 below 3
                  do (setf (cffi:mem-aref data :uint8 (+ k offset)) (if color (nth offset color) 0)))
            (setf (cffi:mem-aref data :uint8 (+ k 3)) (if color 255 0)))))
      (let ((id (rlgl:load-texture data w h 7 1)))
        ;; Au plus proche (GL_NEAREST) : des pixels nets.
        (rlgl:texture-parameters id #x2801 #x2600)
        (rlgl:texture-parameters id #x2800 #x2600)
        id))))

(defun grass-texture ()
  (or *grass-texture* (setf *grass-texture* (build-grass-texture))))

(defun grass-light (terrain)
  "Luminosité du sol au centre de chaque case de végétation de TERRAIN, en
octets (255 : à plat), ligne par ligne."
  (unless (eq (car *grass-light*) terrain)
    (let* ((cells (ceiling (terrain-size terrain) *grass-cell*))
           (half (* 0.5f0 (terrain-size terrain)))
           (light (make-array (* cells cells) :element-type '(unsigned-byte 8))))
      (dotimes (j cells)
        (dotimes (i cells)
          (setf (aref light (+ i (* j cells)))
                (min 255 (round (* 255 (ground-brightness terrain
                                                          (+ (- half) (* *grass-cell* (+ i 0.5f0)))
                                                          (+ (- half) (* *grass-cell* (+ j 0.5f0))))))))))
      (setf *grass-light* (cons terrain light))))
  (cdr *grass-light*))

(declaim (inline cell-hash))
(defun cell-hash (i j salt)
  "Entier de 16 bits tiré de la case I, J et de SALT, toujours le même."
  (declare (type fixnum i j salt) (optimize (speed 3) (safety 0)))
  (let* ((x (logand (+ (* (logand i #xffff) 73856093) (* (logand j #xffff) 19349663)
                       (* salt 83492791))
                    #xffffffff))
         (h (logand (* (logxor x (ash x -13)) 2654435761) #xffffffff)))
    (declare (type (unsigned-byte 32) x h))
    (ldb (byte 16 16) h)))

(defun draw-grass (battle camera)
  "Les touffes d'herbe visibles, sous les soldats, de près seulement."
  (let ((terrain (battle-terrain battle))
        (scale (camera-sprite-scale camera)))
    (when (and *draw-grass* terrain scale (not *topographic-mode*))
      (let* ((light (grass-light terrain))
             (zoom (camera-zoom camera))
             (cell *grass-cell*)
             (half (* 0.5f0 (terrain-size terrain)))
             (cells (ceiling (terrain-size terrain) cell))
             (pixel scale)
             (w (* pixel *grass-width*)) (h (* pixel *grass-height*))
             (variants *grass-variants*)
             (du (/ 1f0 variants))
             (threshold (round (* 65536 *grass-density*)))
             (cx (- (* 0.5f0 *window-width*) (* zoom (camera-x camera))))
             (cy (- (* 0.5f0 *window-height*) (* zoom (camera-y camera)))))
        (multiple-value-bind (left top) (screen->world camera 0f0 0f0)
          (multiple-value-bind (right bottom) (screen->world camera (float *window-width* 1f0)
                                                             (float *window-height* 1f0))
            ;; Une case de marge : une touffe dépasse de sa case.
            (let ((i0 (max 0 (1- (floor (+ left half) cell))))
                  (i1 (min (1- cells) (1+ (floor (+ right half) cell))))
                  (j0 (max 0 (1- (floor (+ top half) cell))))
                  (j1 (min (1- cells) (+ 2 (floor (+ bottom half) cell)))))
              (rlgl:set-texture (grass-texture))
              (rlgl:begin 7)                ; RL_QUADS
              (loop for j from j0 to j1
                    do (loop for i from i0 to i1
                             do (when (< (cell-hash i j 0) threshold)
                                  (let* ((x (+ (- half) (* cell (+ i (/ (cell-hash i j 1) 65536f0)))))
                                         (y (+ (- half) (* cell (+ j (/ (cell-hash i j 2) 65536f0)))))
                                         (variant (mod (cell-hash i j 3) variants))
                                         (u0 (* variant du)) (u1 (+ u0 du))
                                         ;; Pied de la touffe, centré, sur un pixel entier.
                                         (sx (- (fround (+ cx (* zoom x))) (* 0.5f0 w)))
                                         (sy (- (fround (+ cy (* zoom y))) h))
                                         (l (aref light (+ i (* j cells)))))
                                    ;; Une touffe sur deux en miroir.
                                    (when (logbitp 0 (cell-hash i j 4)) (rotatef u0 u1))
                                    (rlgl:color-4ub l l l 255)
                                    (rlgl::texcoord-2f u0 0f0) (rlgl:vertex-2f sx sy)
                                    (rlgl::texcoord-2f u0 1f0) (rlgl:vertex-2f sx (+ sy h))
                                    (rlgl::texcoord-2f u1 1f0) (rlgl:vertex-2f (+ sx w) (+ sy h))
                                    (rlgl::texcoord-2f u1 0f0) (rlgl:vertex-2f (+ sx w) sy)))))
              (rlgl:end)
              (rlgl:set-texture 0))))))))
