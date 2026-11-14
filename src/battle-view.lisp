(in-package #:rogue-company)

;;; Dessin des soldats : un rectangle par soldat, tourné selon la formation de
;;; son régiment, envoyé en un seul lot rlgl (voir draw-bench-rlgl, et la règle
;;; sur les structures par valeur dans CLAUDE.md). Position interpolée entre le
;;; tick précédent et le tick courant avec l'alpha de l'horloge.
;;;
;;; Caméra fixe : l'origine du monde au centre de la fenêtre.

(defparameter *pixels-per-meter* 10f0)
(defparameter *soldier-width* 0.7f0 "Largeur d'un soldat, le long du front, en mètres.")
(defparameter *soldier-depth* 0.45f0 "Épaisseur d'un soldat, vers l'avant, en mètres.")

(defparameter *side-colors* #((205 70 60) (70 115 220))
  "Couleur de chaque camp, en r g b.")

(defun draw-soldiers (battle alpha)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha))
  (let* ((soldiers (battle-soldiers battle))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
         (scale *pixels-per-meter*)
         (origin-x (* 0.5f0 (float (the fixnum *window-width*) 1f0)))
         (origin-y (* 0.5f0 (float (the fixnum *window-height*) 1f0)))
         (half-depth (* 0.5f0 scale (the single-float *soldier-depth*)))
         (half-width (* 0.5f0 scale (the single-float *soldier-width*)))
         (colors *side-colors*))
    (declare (type float-array xs ys prev-xs prev-ys)
             (type single-float scale origin-x origin-y half-depth half-width)
             (type simple-vector colors))
    (rlgl:set-texture (rlgl:get-texture-id-default))
    (rlgl:begin 7)                      ; RL_QUADS
    (rlgl::texcoord-2f 0f0 0f0)
    (loop for regiment across (the vector (battle-regiments battle))
          do (let* ((color (svref colors (mod (the fixnum (regiment-side regiment))
                                              (length colors))))
                    (facing (formation-facing (regiment-formation regiment)))
                    (c (cos facing)) (s (sin facing))
                    ;; Demi-axes, en pixels : vers l'avant et vers la droite.
                    (fx (* c half-depth)) (fy (* s half-depth))
                    (rx (* (- s) half-width)) (ry (* c half-width)))
               (declare (type single-float facing c s fx fy rx ry))
               (rlgl:color-4ub (first color) (second color) (third color) 255)
               (loop for i of-type fixnum from (regiment-start regiment)
                       below (regiment-end regiment)
                     do (let* ((px (aref prev-xs i)) (py (aref prev-ys i))
                               (x (+ origin-x (* scale (+ px (* alpha (- (aref xs i) px))))))
                               (y (+ origin-y (* scale (+ py (* alpha (- (aref ys i) py)))))))
                          (declare (type single-float px py x y))
                          ;; Coins (-avant, -droite) (-avant, +droite) (+avant, +droite)
                          ;; (+avant, -droite) : l'ordre de draw-bench-rlgl, vu de face.
                          (rlgl:vertex-2f (- x fx rx) (- y fy ry))
                          (rlgl:vertex-2f (+ (- x fx) rx) (+ (- y fy) ry))
                          (rlgl:vertex-2f (+ x fx rx) (+ y fy ry))
                          (rlgl:vertex-2f (- (+ x fx) rx) (- (+ y fy) ry))))))
    (rlgl:end)
    (rlgl:set-texture 0)))
