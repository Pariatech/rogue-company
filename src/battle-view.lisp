(in-package #:rogue-company)

;;; Dessin des soldats, vus par une caméra (camera.lisp). Deux modes selon le zoom :
;;;
;;; - Sprites (zoom d'au moins *SPRITE-PIXELS-PER-METER*) : le sprite en vue de
;;;   trois quarts de la direction la plus proche parmi 8, l'image de marche si
;;;   le soldat a bougé pendant le tick, puis son masque teint
;;;   de la couleur du camp. Un soldat plus bas sur l'écran passe devant : les
;;;   soldats sont parcourus dans l'ordre de la grille spatiale, triée par
;;;   rangées de 1 m à chaque tick. Les soldats hors de l'écran sont sautés.
;;; - Vue d'ensemble : un rectangle par soldat, tourné selon son orientation,
;;;   l'avant plus clair.
;;;
;;; Chaque mode part en un seul lot rlgl (voir draw-bench-rlgl, et la règle sur
;;; les structures par valeur dans CLAUDE.md). Position et orientation sont
;;; interpolées entre le tick précédent et le tick courant avec l'alpha de
;;; l'horloge.

(defparameter *soldier-width* 0.7f0 "Largeur d'un soldat, le long du front, en mètres.")
(defparameter *soldier-depth* 0.6f0 "Épaisseur d'un soldat, vers l'avant, en mètres.")
(defparameter *soldier-front* 0.4f0
  "Part avant de l'épaisseur dessinée plus clair : l'indicateur de direction.")
(defparameter *front-lightening* 0.6f0
  "Mélange de la couleur du camp avec le blanc pour l'avant du soldat.")

(defparameter *side-colors* #((205 70 60) (70 115 220))
  "Couleur de chaque camp, en r g b.")

(defparameter *regiment-sprite* :pikeman
  "Sprite de tous les régiments, en attendant un sprite par silhouette.")

(defparameter *walk-frames-per-second* 9f0
  "Images de marche par seconde de bataille : un cycle de 8 images (deux pas)
en 0,9 s, pour 1,3 m/s.")

(defparameter *walk-threshold* 0.01f0
  "Déplacement minimum en un tick, en mètres, pour dessiner la marche : une
légère poussée de collision ne fait pas marcher.")

(defun draw-soldiers (battle alpha camera)
  (if (camera-sprite-scale camera)
      (draw-soldier-sprites battle alpha camera (ensure-atlas))
      (draw-soldier-markers battle alpha camera)))

(defun side-color (side)
  (svref *side-colors* (mod side (length *side-colors*))))

(defun draw-soldier-sprites (battle alpha camera atlas)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha))
  (multiple-value-bind (walk-row walk-frames) (atlas-walk atlas *regiment-sprite*)
    (declare (type fixnum walk-row walk-frames))
    (let* ((soldiers (battle-soldiers battle))
           ;; Vivants seulement, dans l'ordre de la grille.
           (count (grid-size (battle-grid battle)))
           (order (grid-sorted (battle-grid battle)))
           (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
           (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
           (facings (soldiers-facing soldiers)) (prev-facings (soldiers-prev-facing soldiers))
           (soldier-regiments (soldiers-regiment soldiers))
           (regiments (battle-regiments battle))
           (zoom (camera-zoom camera))
           (scale (camera-sprite-scale camera))
           (width (float (the fixnum *window-width*) 1f0))
           (height (float (the fixnum *window-height*) 1f0))
           ;; Écran = monde × zoom + origine.
           (origin-x (- (* 0.5f0 width) (* zoom (the single-float (camera-x camera)))))
           (origin-y (- (* 0.5f0 height) (* zoom (the single-float (camera-y camera)))))
           (cell (float (the fixnum (atlas-cell atlas)) 1f0))
           (size (* cell scale))
           (foot-x (* scale (the single-float *sprite-foot-x*)))
           (foot-y (* scale (the single-float *sprite-foot-y*)))
           (cell-u (/ cell (float (the fixnum (atlas-width atlas)) 1f0)))
           (cell-v (/ cell (float (the fixnum (atlas-height atlas)) 1f0)))
           (strips *direction-strips*)
           (mirrored *direction-mirrored*)
           ;; Image de marche commune, avant le décalage propre à chaque soldat.
           (walk-phase (floor (* (the single-float *walk-frames-per-second*)
                                 (/ (+ (float (the fixnum (battle-tick-count battle)) 1f0) alpha)
                                    (float +ticks-per-second+ 1f0)))))
           (threshold (the single-float *walk-threshold*))
           (threshold2 (* threshold threshold))
           (eighth (/ (float pi 1f0) 4f0)))
      (declare (type fixnum count walk-phase)
               (type fixnum-array order soldier-regiments)
               (type float-array xs ys prev-xs prev-ys facings prev-facings)
               (type simple-vector strips mirrored)
               (type vector regiments)
               (type single-float zoom scale width height origin-x origin-y cell size
                     foot-x foot-y cell-u cell-v threshold threshold2 eighth))
      (rlgl:set-texture (raylib:texture-id (atlas-texture atlas)))
      (rlgl:begin 7)                    ; RL_QUADS
      (dotimes (k count)
        (let* ((i (aref order k))
               (px (aref prev-xs i)) (py (aref prev-ys i))
               (dx (- (aref xs i) px)) (dy (- (aref ys i) py))
               ;; Coin haut gauche du sprite, sur un pixel entier pour rester net.
               (left (fround (- (+ origin-x (* zoom (+ px (* alpha dx)))) foot-x)))
               (top (fround (- (+ origin-y (* zoom (+ py (* alpha dy)))) foot-y))))
          (declare (type fixnum i) (type single-float px py dx dy left top))
          (when (and (< (- size) left width) (< (- size) top height))
            (let* ((pf (aref prev-facings i))
                   (facing (+ pf (* alpha (wrap-angle (- (aref facings i) pf)))))
                   (direction (mod (the fixnum (round facing eighth)) 8))
                   ;; Immobile : la première image de la marche. En marche, chaque
                   ;; soldat a son décalage, pour que le régiment ne marche pas au pas.
                   (frame (if (> (+ (* dx dx) (* dy dy)) threshold2)
                              (mod (+ walk-phase (* 3 i)) walk-frames)
                              0))
                   (row (+ walk-row (* 2 (the fixnum (svref strips direction)))))
                   (v0 (* cell-v (float row 1f0)))
                   (v1 (+ v0 cell-v))
                   (mask-v1 (+ v1 cell-v))
                   (left-u (* cell-u (float frame 1f0)))
                   (right-u (+ left-u cell-u))
                   ;; Miroir : on inverse les u de gauche et de droite.
                   (u0 (if (eql 1 (svref mirrored direction)) right-u left-u))
                   (u1 (if (eql 1 (svref mirrored direction)) left-u right-u))
                   (right (+ left size))
                   (bottom (+ top size))
                   (color (side-color (regiment-side (aref regiments (aref soldier-regiments i))))))
              (declare (type single-float pf facing v0 v1 mask-v1 left-u right-u u0 u1 right bottom)
                       (type fixnum direction frame row))
              (macrolet ((quad (red green blue tv0 tv1)
                           `(progn
                              (rlgl:color-4ub ,red ,green ,blue 255)
                              (rlgl::texcoord-2f u0 ,tv0) (rlgl:vertex-2f left top)
                              (rlgl::texcoord-2f u0 ,tv1) (rlgl:vertex-2f left bottom)
                              (rlgl::texcoord-2f u1 ,tv1) (rlgl:vertex-2f right bottom)
                              (rlgl::texcoord-2f u1 ,tv0) (rlgl:vertex-2f right top))))
                (quad 255 255 255 v0 v1)
                (let ((red (first color)) (green (second color)) (blue (third color)))
                  (declare (type (integer 0 255) red green blue))
                  (quad red green blue v1 mask-v1)))))))
      (rlgl:end)
      (rlgl:set-texture 0))))

(defun draw-soldier-markers (battle alpha camera)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha))
  (let* ((soldiers (battle-soldiers battle))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
         (facings (soldiers-facing soldiers)) (prev-facings (soldiers-prev-facing soldiers))
         (scale (camera-zoom camera))
         (origin-x (- (* 0.5f0 (float (the fixnum *window-width*) 1f0))
                      (* scale (the single-float (camera-x camera)))))
         (origin-y (- (* 0.5f0 (float (the fixnum *window-height*) 1f0))
                      (* scale (the single-float (camera-y camera)))))
         (half-depth (* 0.5f0 scale (the single-float *soldier-depth*)))
         (half-width (* 0.5f0 scale (the single-float *soldier-width*)))
         ;; Bord arrière de l'avant clair, en part de la demi-épaisseur depuis le centre.
         (front-back (- 1f0 (* 2f0 (the single-float *soldier-front*))))
         (lightening (the single-float *front-lightening*)))
    (declare (type float-array xs ys prev-xs prev-ys facings prev-facings)
             (type single-float scale origin-x origin-y half-depth half-width front-back lightening))
    (rlgl:set-texture (rlgl:get-texture-id-default))
    (rlgl:begin 7)                      ; RL_QUADS
    (rlgl::texcoord-2f 0f0 0f0)
    (loop for regiment across (the vector (battle-regiments battle))
          do (let* ((color (side-color (regiment-side regiment)))
                    (red (first color)) (green (second color)) (blue (third color)))
               (declare (type (integer 0 255) red green blue))
               (flet ((light (channel)
                        (declare (type (integer 0 255) channel))
                        (min 255 (round (+ channel (* lightening (- 255 channel)))))))
                 (let ((light-red (light red)) (light-green (light green)) (light-blue (light blue)))
                   (loop for i of-type fixnum from (regiment-start regiment)
                           below (regiment-end regiment)
                         do (let* ((px (aref prev-xs i)) (py (aref prev-ys i))
                                   (x (+ origin-x (* scale (+ px (* alpha (- (aref xs i) px))))))
                                   (y (+ origin-y (* scale (+ py (* alpha (- (aref ys i) py))))))
                                   ;; Orientation interpolée par le plus court chemin.
                                   (pf (aref prev-facings i))
                                   (facing (+ pf (* alpha (wrap-angle (- (aref facings i) pf)))))
                                   (c (cos facing)) (s (sin facing))
                                   ;; Demi-axes, en pixels : vers l'avant et vers la droite.
                                   (fx (* c half-depth)) (fy (* s half-depth))
                                   (rx (* (- s) half-width)) (ry (* c half-width))
                                   (bx (* front-back fx)) (by (* front-back fy)))
                              (declare (type single-float px py x y pf facing c s fx fy rx ry bx by))
                              ;; Corps puis avant, chacun de coins (-avant, -droite)
                              ;; (-avant, +droite) (+avant, +droite) (+avant, -droite) :
                              ;; l'ordre de draw-bench-rlgl, vu de face.
                              (rlgl:color-4ub red green blue 255)
                              (rlgl:vertex-2f (- x fx rx) (- y fy ry))
                              (rlgl:vertex-2f (+ (- x fx) rx) (+ (- y fy) ry))
                              (rlgl:vertex-2f (+ x fx rx) (+ y fy ry))
                              (rlgl:vertex-2f (- (+ x fx) rx) (- (+ y fy) ry))
                              (rlgl:color-4ub light-red light-green light-blue 255)
                              (rlgl:vertex-2f (- (+ x bx) rx) (- (+ y by) ry))
                              (rlgl:vertex-2f (+ x bx rx) (+ y by ry))
                              (rlgl:vertex-2f (+ x fx rx) (+ y fy ry))
                              (rlgl:vertex-2f (- (+ x fx) rx) (- (+ y fy) ry))))))))
    (rlgl:end)
    (rlgl:set-texture 0)))
