(in-package #:rogue-company)

;;; Dessin des soldats : un rectangle par soldat, tourné selon sa propre
;;; orientation, l'avant plus clair pour montrer où il fait face. Le tout part en
;;; un seul lot rlgl (voir draw-bench-rlgl, et la règle sur les structures par
;;; valeur dans CLAUDE.md). Position et orientation sont interpolées entre le
;;; tick précédent et le tick courant avec l'alpha de l'horloge.
;;;
;;; Caméra fixe : l'origine du monde au centre de la fenêtre.

(defparameter *pixels-per-meter* 10f0)
(defparameter *soldier-width* 0.7f0 "Largeur d'un soldat, le long du front, en mètres.")
(defparameter *soldier-depth* 0.6f0 "Épaisseur d'un soldat, vers l'avant, en mètres.")
(defparameter *soldier-front* 0.4f0
  "Part avant de l'épaisseur dessinée plus clair : l'indicateur de direction.")
(defparameter *front-lightening* 0.6f0
  "Mélange de la couleur du camp avec le blanc pour l'avant du soldat.")

(defparameter *side-colors* #((205 70 60) (70 115 220))
  "Couleur de chaque camp, en r g b.")

(defun draw-soldiers (battle alpha)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha))
  (let* ((soldiers (battle-soldiers battle))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
         (facings (soldiers-facing soldiers)) (prev-facings (soldiers-prev-facing soldiers))
         (scale *pixels-per-meter*)
         (origin-x (* 0.5f0 (float (the fixnum *window-width*) 1f0)))
         (origin-y (* 0.5f0 (float (the fixnum *window-height*) 1f0)))
         (half-depth (* 0.5f0 scale (the single-float *soldier-depth*)))
         (half-width (* 0.5f0 scale (the single-float *soldier-width*)))
         (colors *side-colors*)
         ;; Bord arrière de l'avant clair, en part de la demi-épaisseur depuis le centre.
         (front-back (- 1f0 (* 2f0 (the single-float *soldier-front*))))
         (lightening (the single-float *front-lightening*)))
    (declare (type float-array xs ys prev-xs prev-ys facings prev-facings)
             (type single-float scale origin-x origin-y half-depth half-width front-back lightening)
             (type simple-vector colors))
    (rlgl:set-texture (rlgl:get-texture-id-default))
    (rlgl:begin 7)                      ; RL_QUADS
    (rlgl::texcoord-2f 0f0 0f0)
    (loop for regiment across (the vector (battle-regiments battle))
          do (let* ((color (svref colors (mod (the fixnum (regiment-side regiment))
                                              (length colors))))
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
