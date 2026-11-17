(in-package #:rogue-company)

;;; Dessin des soldats, vus par une caméra (camera.lisp). Deux modes selon le zoom :
;;;
;;; - Sprites (zoom d'au moins *SPRITE-PIXELS-PER-METER*) : le sprite en vue de
;;;   trois quarts de la direction la plus proche parmi 8 : l'animation d'attaque
;;;   juste après un coup, sinon l'image de marche si le soldat a bougé pendant
;;;   le tick, sinon la pose immobile ; puis son masque teint
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

(defvar *soldier-style* :parts
  "Dessin des soldats au zoom des sprites : :PARTS (morceaux) ou :SPRITES
(sprites PixelLab).")

(defparameter *finial-length* 0.12f0
  "Pointe dorée de la hampe du porteur dessiné en morceaux, en mètres.")

(defparameter *regiment-sprite* :pikeman
  "Sprite de tous les régiments, en attendant un sprite par silhouette.")

(defparameter *walk-frames-per-second* 9f0
  "Images de marche par seconde de bataille : un cycle de 8 images (deux pas)
en 0,9 s, pour 1,3 m/s.")

(defparameter *attack-frames-per-second* 10f0
  "Images de l'animation d'attaque par seconde de bataille : le coup de pique
(6 images) dure 0,6 s, moins que la recharge de la pique (1,5 s).")

(defparameter *walk-threshold* 0.01f0
  "Déplacement minimum en un tick, en mètres, pour dessiner la marche : une
légère poussée de collision ne fait pas marcher.")

(defparameter *bearer-sprite* :standard-bearer
  "Sprite du porteur d'étendard de chaque régiment.")

(defparameter *flag-morale-steps* #(0.6667f0 0.3333f0)
  "Seuils de l'état du drapeau, en part du moral entre le seuil de débandade et
la valeur de départ : au-dessus du premier, intact ; puis troué ; sous le
second, en lambeaux. Débandé, il pend.")

(defparameter *overview-flag-height* 2f0
  "En vue éloignée, hauteur du sommet de la hampe au-dessus du porteur, en mètres.")

(defparameter *overview-flag-scale* 2f0
  "En vue éloignée, échelle du drapeau : fixe, pour rester lisible à tous les zooms.")

;;; Piques dessinées : une ligne par piquier, à la longueur réelle, que le
;;; sprite de 32 px ne peut pas contenir. Dressée et un peu penchée vers l'avant
;;; en marche ; croisée à l'horizontale vers l'ennemi pour les premiers rangs
;;; d'un régiment qui attaque. Le coup la pousse en avant puis la ramène.

(defparameter *draw-pikes* t
  "Dessine les piques à part (et plus l'animation d'attaque du sprite).")

(defparameter *pike-length* 5f0 "Longueur de la pique, en mètres.")
(defparameter *pike-head* 0.3f0 "Longueur du fer, en mètres.")
(defparameter *pike-width* 1f0 "Épaisseur de la pique, en pixels du sprite.")
(defparameter *pike-wood* '(110 72 40) "Couleur de la hampe.")
(defparameter *pike-steel* '(205 212 222) "Couleur du fer.")
(defparameter *pike-lean* 0.8f0
  "Pique dressée : avance du fer vers l'avant du soldat, en mètres.")
(defparameter *pike-hand-height* 0.9f0
  "Pique croisée : hauteur des mains du premier rang au-dessus du sol, en mètres.")
(defparameter *pike-rank-rise* 0.2f0
  "Pique croisée : chaque rang tient la sienne plus haut que le précédent, par-dessus
l'épaule de l'homme de devant, en mètres.")
(defparameter *pike-behind* 1.2f0
  "Pique croisée : longueur qui dépasse derrière les mains, en mètres.")
(defparameter *pike-lowered-ranks* 4
  "Rangs qui croisent la pique quand le régiment attaque ; les autres la gardent dressée.")
(defparameter *pike-thrust* 0.7f0 "Allonge du coup de pique, en mètres.")
(defparameter *pike-thrust-seconds* 0.5f0 "Durée du coup : la pique part puis revient.")

(defvar *bearers* (make-array 0 :element-type 'fixnum)
  "Porteur d'étendard de chaque régiment, recalculé à chaque image : le dessin
compare chaque soldat à celui de son régiment sans rappeler REGIMENT-BEARER.
Réutilisé d'une image à l'autre, agrandi seulement quand des régiments s'ajoutent.")

(defvar *lowered-ends* (make-array 0 :element-type 'fixnum)
  "Pour chaque régiment, l'indice qui suit le dernier soldat à pique croisée
(-1 si aucun). Même usage que *BEARERS*.")

(defvar *regiment-starts* (make-array 0 :element-type 'fixnum)
  "Premier indice de chaque régiment, pour le rang d'un soldat. Même usage que *BEARERS*.")

(defvar *regiment-files* (make-array 0 :element-type 'fixnum)
  "Files de chaque régiment, pour le rang d'un soldat. Même usage que *BEARERS*.")

(defvar *regiment-weapons* (make-array 0 :element-type 'fixnum)
  "Arme dessinée de chaque régiment, d'après son arme principale : 0 aucune,
1 pique, 2 épée (REGIMENT-WEAPON-LOOK). Même usage que *BEARERS*.")

(defvar *regiment-shields* (make-array 0 :element-type 'fixnum)
  "1 si le régiment porte une rondache, sinon 0. Même usage que *BEARERS*.")

(defvar *regiment-crouch* (make-array 0 :element-type 'fixnum)
  "Par régiment, pour l'image : premiers rangs accroupis (feu par rang).")

(defvar *regiment-aim-ranks* (make-array 0 :element-type 'fixnum)
  "Par régiment : rangs qui peuvent tirer (REGIMENT-FIRING-RANKS), seuls en joue.")

(defvar *regiment-stoop* (make-array 0 :element-type 'fixnum)
  "Par régiment, pour l'image : rang courbé (salvée), ou -1.")

(defvar *regiment-guards* (make-array 0 :element-type 'fixnum)
  "1 si le régiment a un ordre d'attaque : il tient son arme en garde. Même
usage que *BEARERS*.")

(defun regiment-weapon-look (regiment)
  "Arme à dessiner pour REGIMENT : 1 pique, 2 épée, 3 hallebarde, 4 arme à feu,
0 rien."
  (case (getf (stats-loadout (regiment-stats regiment)) :main)
    (:pike 1)
    (:sword 2)
    (:halberd 3)
    ((:arquebus :musket) 4)
    (t 0)))

(defun update-bearers (battle)
  "Remplit les tampons par régiment pour l'image, et renvoie *BEARERS*."
  (let ((regiments (battle-regiments battle)))
    (when (< (length *regiment-weapons*) (length regiments))
      (setf *regiment-weapons* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *regiment-shields*) (length regiments))
      (setf *regiment-shields* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *regiment-guards*) (length regiments))
      (setf *regiment-guards* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *regiment-crouch*) (length regiments))
      (setf *regiment-crouch* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *regiment-aim-ranks*) (length regiments))
      (setf *regiment-aim-ranks* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *regiment-stoop*) (length regiments))
      (setf *regiment-stoop* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *bearers*) (length regiments))
      (setf *bearers* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *lowered-ends*) (length regiments))
      (setf *lowered-ends* (make-array (length regiments) :element-type 'fixnum)))
    (when (< (length *regiment-starts*) (length regiments))
      (setf *regiment-starts* (make-array (length regiments) :element-type 'fixnum)
            *regiment-files* (make-array (length regiments) :element-type 'fixnum)))
    (loop for regiment across regiments
          for k from 0
          do (setf (aref *regiment-weapons* k) (regiment-weapon-look regiment)
                   (aref *regiment-shields* k)
                   (if (getf (stats-loadout (regiment-stats regiment)) :shield) 1 0)
                   (aref *regiment-crouch* k) (regiment-crouch regiment)
                   (aref *regiment-stoop* k) (regiment-stoop regiment)
                   (aref *regiment-aim-ranks* k) (if (ranged-regiment-p regiment)
                                                     (regiment-firing-ranks regiment)
                                                     0)
                   (aref *regiment-guards* k)
                   ;; En garde sous un ordre d'attaque, ou l'ennemi proche (défense).
                   (if (or (eq (first (regiment-orders regiment)) :attack)
                           (regiment-alert-p regiment))
                       1 0))
             (setf (aref *regiment-starts* k) (regiment-start regiment)
                   (aref *regiment-files* k) (formation-files (regiment-formation regiment)
                                                              (max 1 (regiment-alive regiment)))
                   (aref *bearers* k) (regiment-bearer regiment)
                   (aref *lowered-ends* k)
                   (if (and (= 1 (aref *regiment-guards* k))
                            (= 1 (aref *regiment-weapons* k)))
                       (+ (regiment-start regiment)
                          (* *pike-lowered-ranks*
                             (formation-files (regiment-formation regiment)
                                              (max 1 (regiment-alive regiment)))))
                       -1)))
    *bearers*))

(defun draw-soldiers (battle alpha camera)
  (let ((atlas (ensure-atlas)))
    (cond ((not (camera-sprite-scale camera)) (draw-soldier-markers battle alpha camera))
          ((eq *soldier-style* :parts)
           (draw-corpses battle alpha camera atlas)
           (draw-soldier-parts battle alpha camera atlas))
          (t (draw-soldier-sprites battle alpha camera atlas)))
    (draw-flags battle alpha camera atlas)))

(declaim (inline walk-frame))
(defun walk-frame (walk-phase i frames dx dy threshold2)
  "Image de marche du soldat I : la première s'il n'a pas bougé pendant le tick
(DX, DY), sinon la phase commune décalée pour lui, pour que le régiment ne
marche pas au pas."
  (declare (type fixnum walk-phase i frames) (type single-float dx dy threshold2))
  (if (> (+ (* dx dx) (* dy dy)) threshold2)
      (mod (+ walk-phase (* 3 i)) frames)
      0))

(defun walk-phase (battle alpha)
  "Image de marche commune à l'instant dessiné, avant le décalage de chaque soldat."
  (declare (type single-float alpha))
  (floor (* (the single-float *walk-frames-per-second*)
            (/ (+ (float (the fixnum (battle-tick-count battle)) 1f0) alpha)
               (float +ticks-per-second+ 1f0)))))

(defun flag-state (regiment)
  "Image du drapeau de REGIMENT selon son moral : 0 intact, 1 troué, 2 en
lambeaux, 3 pendant (débandade)."
  (if (regiment-routed-p regiment)
      3
      (let* ((start (stats-morale (regiment-stats regiment)))
             (span (max 1f0 (- start *rout-morale*)))
             (part (/ (- (regiment-morale regiment) *rout-morale*) span))
             (steps *flag-morale-steps*))
        (cond ((> part (aref steps 0)) 0)
              ((> part (aref steps 1)) 1)
              (t 2)))))

(defun side-color (side)
  (svref *side-colors* (mod side (length *side-colors*))))

(defun draw-soldier-sprites (battle alpha camera atlas)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha))
  (multiple-value-bind (walk-row walk-frames) (atlas-walk atlas *regiment-sprite*)
    (declare (type fixnum walk-row walk-frames))
    (multiple-value-bind (attack-row attack-frames) (atlas-animation atlas *regiment-sprite* :attack)
    (multiple-value-bind (bearer-row bearer-frames)
        (atlas-walk atlas *bearer-sprite* *regiment-sprite*)
      (declare (type fixnum bearer-row bearer-frames))
    (multiple-value-bind (solid-row) (atlas-overlay atlas :solid)
    (let* ((soldiers (battle-soldiers battle))
           ;; Centre du carré blanc de l'atlas : la couleur unie des piques.
           (solid-u (/ 2f0 (float (the fixnum (atlas-width atlas)) 1f0)))
           (solid-v (/ (+ 2f0 (* (float (the fixnum (atlas-cell atlas)) 1f0)
                                 (float (the fixnum (or solid-row 0)) 1f0)))
                       (float (the fixnum (atlas-height atlas)) 1f0)))
           (pi-f (float pi 1f0))
           (attack-row (or attack-row 0))
           (attack-frames (or attack-frames 0))
           (strike-ticks (soldiers-strike-tick soldiers))
           (tick (battle-tick-count battle))
           ;; Images d'attaque par tick, pour l'âge du dernier coup.
           (attack-rate (/ (the single-float *attack-frames-per-second*)
                           (float +ticks-per-second+ 1f0)))
           ;; Vivants seulement, dans l'ordre de la grille.
           (count (grid-size (battle-grid battle)))
           (order (grid-sorted (battle-grid battle)))
           (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
           (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
           (facings (soldiers-facing soldiers)) (prev-facings (soldiers-prev-facing soldiers))
           (soldier-regiments (soldiers-regiment soldiers))
           (regiments (battle-regiments battle))
           (bearers (update-bearers battle))
           (lowered-ends *lowered-ends*)
           (regiment-starts *regiment-starts*)
           (regiment-files *regiment-files*)
           (regiment-weapons *regiment-weapons*)
           (pike-rise (the single-float *pike-rank-rise*))
           (pikes (and *draw-pikes* solid-row t))
           (pike-length (the single-float *pike-length*))
           (pike-head (the single-float *pike-head*))
           (pike-half-width (* 0.5f0 (the single-float (camera-sprite-scale camera))
                                (the single-float *pike-width*)))
           (pike-lean (the single-float *pike-lean*))
           (pike-hand (the single-float *pike-hand-height*))
           (pike-behind (the single-float *pike-behind*))
           (pike-thrust (the single-float *pike-thrust*))
           (thrust-ticks (* (the single-float *pike-thrust-seconds*) (float +ticks-per-second+ 1f0)))
           (wood *pike-wood*) (steel *pike-steel*)
           (wood-r (first wood)) (wood-g (second wood)) (wood-b (third wood))
           (steel-r (first steel)) (steel-g (second steel)) (steel-b (third steel))
           (zoom (camera-zoom camera))
           (scale (camera-sprite-scale camera))
           (width (float (the fixnum *window-width*) 1f0))
           (height (float (the fixnum *window-height*) 1f0))
           ;; Écran = monde × zoom + origine.
           (origin-x (- (* 0.5f0 width) (* zoom (the single-float (camera-x camera)))))
           (origin-y (- (* 0.5f0 height) (* zoom (the single-float (camera-y camera)))))
           (cell (float (the fixnum (atlas-cell atlas)) 1f0))
           (size (* cell scale))
           (foot-x (* scale (+ (* 0.5f0 cell) (the single-float *sprite-foot-x*))))
           (foot-y (* scale (+ (* 0.5f0 cell) (the single-float *sprite-foot-y*))))
           (cell-u (/ cell (float (the fixnum (atlas-width atlas)) 1f0)))
           (cell-v (/ cell (float (the fixnum (atlas-height atlas)) 1f0)))
           (strips *direction-strips*)
           (mirrored *direction-mirrored*)
           (walk-phase (walk-phase battle alpha))
           (threshold (the single-float *walk-threshold*))
           (threshold2 (* threshold threshold))
           (eighth (/ (float pi 1f0) 4f0)))
      (declare (type fixnum count walk-phase attack-row attack-frames tick)
               (type fixnum-array order soldier-regiments strike-ticks bearers lowered-ends
                     regiment-starts regiment-files regiment-weapons)
               (type single-float pike-rise)
               (type single-float pike-length pike-head pike-half-width pike-lean pike-hand
                     pike-behind pike-thrust thrust-ticks solid-u solid-v pi-f)
               (type (integer 0 255) wood-r wood-g wood-b steel-r steel-g steel-b)
               (type single-float attack-rate)
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
                   (regiment-index (aref soldier-regiments i))
                   (bearer (= i (aref bearers regiment-index)))
                   ;; Âge du dernier coup, en images d'attaque.
                   (attack-frame (floor (* attack-rate
                                           (+ alpha (float (min 1000000
                                                                (- tick (aref strike-ticks i)))
                                                           1f0)))))
                   (attacking (and (not bearer) (not pikes) (< attack-frame attack-frames)))
                   (frame (if attacking
                              attack-frame
                              (walk-frame walk-phase i (if bearer bearer-frames walk-frames)
                                          dx dy threshold2)))
                   (row (+ (cond (attacking attack-row) (bearer bearer-row) (t walk-row))
                           (* 2 (the fixnum (svref strips direction)))))
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
                   (color (side-color (regiment-side (aref regiments regiment-index)))))
              (declare (type single-float pf facing v0 v1 mask-v1 left-u right-u u0 u1 right bottom)
                       (type fixnum direction attack-frame frame row))
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
                  (quad red green blue v1 mask-v1)))
              (when (and pikes (not bearer) (= 1 (aref regiment-weapons regiment-index)))
                (let* ((sx (+ left foot-x)) (sy (+ top foot-y))
                       (fx (cos facing)) (fy (sin facing))
                       (age (+ alpha (float (min 1000000 (- tick (aref strike-ticks i))) 1f0)))
                       (thrust (if (< age thrust-ticks)
                                   (* pike-thrust (sin (* pi-f (/ age thrust-ticks))))
                                   0f0))
                       (ax 0f0) (ay 0f0) (bx 0f0) (by 0f0))
                  (declare (type single-float sx sy fx fy age thrust ax ay bx by))
                  (if (< i (aref lowered-ends regiment-index))
                      ;; Croisée : horizontale à hauteur des mains, vers l'avant.
                      (let* ((rank (floor (- i (aref regiment-starts regiment-index))
                                          (max 1 (aref regiment-files regiment-index))))
                             (hand-y (- sy (* zoom (+ pike-hand (* pike-rise (float rank 1f0))))))
                             (back (- thrust pike-behind)))
                        (declare (type fixnum rank) (type single-float hand-y back))
                        (setf ax (+ sx (* zoom fx back))
                              ay (+ hand-y (* zoom fy back))
                              bx (+ ax (* zoom fx pike-length))
                              by (+ ay (* zoom fy pike-length))))
                      ;; Dressée, le fer un peu en avant du soldat.
                      (setf ax sx
                            ay sy
                            bx (+ sx (* zoom fx pike-lean))
                            by (- (+ sy (* zoom fy pike-lean))
                                  (* zoom (sqrt (- (* pike-length pike-length)
                                                   (* pike-lean pike-lean)))))))
                  (let* ((head (- 1f0 (/ pike-head pike-length)))
                         (hx (+ ax (* head (- bx ax))))
                         (hy (+ ay (* head (- by ay))))
                         (length (sqrt (+ (expt (- bx ax) 2) (expt (- by ay) 2))))
                         (nx (if (plusp length) (* pike-half-width (/ (- ay by) length)) 0f0))
                         (ny (if (plusp length) (* pike-half-width (/ (- bx ax) length)) 0f0)))
                    (declare (type single-float head hx hy length nx ny))
                    ;; Coins dans l'ordre des quads de sprite : départ -n, départ +n,
                    ;; arrivée +n, arrivée -n.
                    (macrolet ((segment (x0 y0 x1 y1 red green blue)
                                 `(progn
                                    (rlgl:color-4ub ,red ,green ,blue 255)
                                    (rlgl::texcoord-2f solid-u solid-v)
                                    (rlgl:vertex-2f (- ,x0 nx) (- ,y0 ny))
                                    (rlgl:vertex-2f (+ ,x0 nx) (+ ,y0 ny))
                                    (rlgl:vertex-2f (+ ,x1 nx) (+ ,y1 ny))
                                    (rlgl:vertex-2f (- ,x1 nx) (- ,y1 ny)))))
                      (segment ax ay hx hy wood-r wood-g wood-b)
                      (segment hx hy bx by steel-r steel-g steel-b)))))))))
      (rlgl:end)
      (rlgl:set-texture 0)))))))

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

(defun draw-flags (battle alpha camera atlas)
  "Le drapeau de chaque régiment, par-dessus les soldats, dans l'état de son
moral (FLAG-STATE). Avec les sprites, il s'accroche au sommet de la hampe du
porteur, image par image (sommets de l'atlas), et flotte à l'opposé de son
regard en miroir. En vue éloignée, il flotte au-dessus du porteur, à une
échelle fixe (*OVERVIEW-FLAG-SCALE*) pour rester lisible."
  (declare (type single-float alpha))
  (multiple-value-bind (flag-row flag-frames flag-width flag-height) (atlas-overlay atlas :flag)
    (declare (ignore flag-frames))
    (when flag-row
      (multiple-value-bind (bearer-row bearer-frames tips)
          (atlas-walk atlas *bearer-sprite* *regiment-sprite*)
        (declare (ignore bearer-row) (type fixnum bearer-frames))
        (let* ((soldiers (battle-soldiers battle))
               (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
               (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
               (facings (soldiers-facing soldiers)) (prev-facings (soldiers-prev-facing soldiers))
               (zoom (camera-zoom camera))
               (scale (camera-sprite-scale camera))
               (flag-scale (or scale *overview-flag-scale*))
               (origin-x (- (* 0.5f0 (float *window-width* 1f0)) (* zoom (camera-x camera))))
               (origin-y (- (* 0.5f0 (float *window-height* 1f0)) (* zoom (camera-y camera))))
               (cell (float (atlas-cell atlas) 1f0))
               (cell-u (/ cell (atlas-width atlas)))
               (cell-v (/ cell (atlas-height atlas)))
               (flag-u (/ (float flag-width 1f0) (atlas-width atlas)))
               (flag-v (/ (float flag-height 1f0) (atlas-height atlas)))
               (walk-phase (walk-phase battle alpha))
               (threshold2 (expt *walk-threshold* 2))
               (eighth (/ (float pi 1f0) 4f0)))
          (rlgl:set-texture (raylib:texture-id (atlas-texture atlas)))
          (rlgl:begin 7)                ; RL_QUADS
          (loop for regiment across (battle-regiments battle)
                for i = (regiment-bearer regiment)
                when (>= i 0)
                  do (let* ((px (aref prev-xs i)) (py (aref prev-ys i))
                            (dx (- (aref xs i) px)) (dy (- (aref ys i) py))
                            (x (+ origin-x (* zoom (+ px (* alpha dx)))))
                            (y (+ origin-y (* zoom (+ py (* alpha dy)))))
                            (pf (aref prev-facings i))
                            (facing (+ pf (* alpha (wrap-angle (- (aref facings i) pf)))))
                            (direction (mod (round facing eighth) 8))
                            (mirror (eql 1 (svref *direction-mirrored* direction)))
                            (strip (svref *direction-strips* direction))
                            (frame (walk-frame walk-phase i bearer-frames dx dy threshold2))
                            (tip (* 2 (+ (* strip bearer-frames) frame)))
                            ;; Colonne de pixels de la hampe, depuis le centre de la case ;
                            ;; en miroir, la colonne -1 - x.
                            (pole (cond ((not scale) 0)
                                        (mirror (- -1 (aref tips tip)))
                                        (t (aref tips tip))))
                            (center-x (- (fround x) (* flag-scale *sprite-foot-x*)))
                            (center-y (- (fround y) (* flag-scale *sprite-foot-y*)))
                            (parts (and scale (eq *soldier-style* :parts)))
                            ;; Le drapeau touche la hampe, un pixel sous son sommet ; en
                            ;; morceaux, sous la pointe dorée.
                            (pole-left (if parts
                                           (fround (bearer-pole-top x y facing zoom))
                                           (+ center-x (* flag-scale pole))))
                            (top (cond (parts
                                        (fround (+ (nth-value 1 (bearer-pole-top x y facing zoom))
                                                   (* zoom *finial-length*))))
                                       (scale
                                        (+ center-y (* flag-scale (1+ (aref tips (1+ tip))))))
                                       (t (fround (- y (* zoom *overview-flag-height*))))))
                            (w (* flag-scale flag-width))
                            (left (if mirror (- pole-left w) (+ pole-left flag-scale)))
                            (right (+ left w))
                            (bottom (+ top (* flag-scale flag-height)))
                            (u-left (* cell-u (flag-state regiment)))
                            (u-right (+ u-left flag-u))
                            (u0 (if mirror u-right u-left))
                            (u1 (if mirror u-left u-right))
                            (v0 (* cell-v flag-row))
                            (v1 (+ v0 flag-v))
                            (mask-v0 (+ v0 cell-v))
                            (mask-v1 (+ mask-v0 flag-v))
                            (color (side-color (regiment-side regiment))))
                       (flet ((quad (red green blue tv0 tv1)
                                (rlgl:color-4ub red green blue 255)
                                (rlgl::texcoord-2f u0 tv0) (rlgl:vertex-2f left top)
                                (rlgl::texcoord-2f u0 tv1) (rlgl:vertex-2f left bottom)
                                (rlgl::texcoord-2f u1 tv1) (rlgl:vertex-2f right bottom)
                                (rlgl::texcoord-2f u1 tv0) (rlgl:vertex-2f right top)))
                         (quad 255 255 255 v0 v1)
                         (quad (first color) (second color) (third color) mask-v0 mask-v1))))
          (rlgl:end)
          (rlgl:set-texture 0))))))
