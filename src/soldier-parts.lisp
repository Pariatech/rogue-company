(in-package #:rogue-company)

;;; Soldats en morceaux : un corps, une tête, deux mains et deux pieds séparés,
;;; dessinés par scripts/draw-parts.lisp (surimpressions part-* de l'atlas),
;;; que le jeu place et anime lui-même. Remplace le sprite du soldat quand
;;; *SOLDIER-STYLE* vaut :PARTS (touche V dans la bataille).
;;;
;;; Chaque morceau a une position dans le repère du soldat : vers l'avant, vers
;;; sa droite, et en hauteur, en mètres. Elle se projette à l'écran selon son
;;; orientation réelle (pas seulement 8 directions) : le sol en vue de dessus,
;;; la hauteur vers le haut de l'écran. Seuls le corps et la tête ont une image
;;; par direction ; les mains et les pieds sont les mêmes vus de partout.
;;;
;;; Les mains tiennent la pique dessinée (dressée ou croisée, poussée par le
;;; coup) ou la hampe du porteur d'étendard. Ordre de dessin d'un soldat : les
;;; pieds, ce qui est derrière son corps (plus loin de l'écran), le corps, la
;;; tête, puis ce qui est devant.

(defparameter *part-body-height* 0.55f0 "Centre du corps au-dessus du sol, en mètres.")
(defparameter *part-head-height* 1.19f0 "Centre de la tête au-dessus du sol, en mètres.")
(defparameter *part-foot-height* 0.06f0 "Centre d'un pied au-dessus du sol, en mètres.")
(defparameter *part-foot-side* 0.11f0 "Écart de chaque pied à l'axe du soldat, en mètres.")
(defparameter *part-stride* 0.13f0 "Avance d'un pied en marche, de part et d'autre, en mètres.")
(defparameter *part-foot-lift* 0.07f0 "Hauteur d'un pied levé en marche, en mètres.")
(defparameter *part-bob* 0.05f0 "Rebond du corps et de la tête en marche, en mètres.")
(defparameter *part-hand-height* 0.5f0 "Mains vides, au repos, en mètres du sol.")
(defparameter *part-hand-side* 0.36f0 "Mains vides : écart à l'axe, en mètres.")
(defparameter *part-hand-swing* 0.1f0 "Mains vides : balancement en marche, en mètres.")
(defparameter *part-pike-side* 0.14f0
  "Pique croisée : décalage de son axe vers la droite du soldat, en mètres.")
(defparameter *part-pike-rear-hand* -0.15f0
  "Pique croisée : main arrière, depuis le point de prise de la pique, en mètres.")
(defparameter *part-pike-front-hand* 0.35f0
  "Pique croisée : main avant, depuis le point de prise de la pique, en mètres.")
(defparameter *part-hand-thrust* 0.5f0
  "Part du coup de pique suivie par les mains : la pique glisse entre elles.")
(defparameter *part-upright-side* 0.26f0
  "Pique dressée et hampe du porteur : à droite du soldat, en mètres.")
(defparameter *part-upright-forward* 0.05f0
  "Pique dressée et hampe du porteur : pied en avant du soldat, en mètres.")
(defparameter *part-upright-low-hand* 0.5f0 "Pique dressée : main basse, en mètres du sol.")
(defparameter *part-upright-high-hand* 0.8f0 "Pique dressée : main haute, en mètres du sol.")
(defparameter *bearer-pole-height* 2.6f0 "Hampe du porteur, en mètres.")
(defparameter *bearer-pole-lean* 0.15f0 "Hampe du porteur : avance du sommet, en mètres.")
(defparameter *finial-color* '(232 190 64))
(defparameter *part-cull-margin* 6f0
  "Marge hors écran, en mètres, sous laquelle un soldat se dessine encore : une
pique croisée dépasse de lui.")

(defun bearer-pole-top (x y facing zoom)
  "Sommet de la hampe d'un porteur dessiné en morceaux, à l'écran, pour un
porteur au sol en X, Y (écran) regardant vers FACING."
  (let* ((c (cos facing)) (s (sin facing))
         (f (+ *part-upright-forward* *bearer-pole-lean*))
         (r *part-upright-side*))
    (values (+ x (* zoom (- (* f c) (* r s))))
            (+ y (* zoom (- (+ (* f s) (* r c)) *bearer-pole-height*))))))

;;; Quads du lot des morceaux. Ces macros lisent dans la portée de l'appel :
;;; SCALE (pixels d'écran par pixel de sprite), CELL-U, CELL-V, TEXEL-U,
;;; TEXEL-V (atlas), HALF-LINE, SOLID-U et SOLID-V (traits unis).

(defmacro part-quad (row column w h x y flip red green blue)
  "Morceau de W × H pixels de sprite, case COLUMN de la rangée ROW de l'atlas,
centré en X, Y à l'écran, sur des pixels entiers."
  `(let* ((pw (* scale ,w)) (ph (* scale ,h))
          (left (fround (- ,x (* 0.5f0 pw))))
          (top (fround (- ,y (* 0.5f0 ph))))
          (right (+ left pw)) (bottom (+ top ph))
          (ua (* cell-u (float ,column 1f0)))
          (ub (+ ua (* texel-u ,w)))
          (u0 (if ,flip ub ua)) (u1 (if ,flip ua ub))
          (v0 (* cell-v (float ,row 1f0)))
          (v1 (+ v0 (* texel-v ,h))))
     (declare (type single-float pw ph left top right bottom ua ub u0 u1 v0 v1))
     (rlgl:color-4ub ,red ,green ,blue 255)
     (rlgl::texcoord-2f u0 v0) (rlgl:vertex-2f left top)
     (rlgl::texcoord-2f u0 v1) (rlgl:vertex-2f left bottom)
     (rlgl::texcoord-2f u1 v1) (rlgl:vertex-2f right bottom)
     (rlgl::texcoord-2f u1 v0) (rlgl:vertex-2f right top)))

(defmacro turned-part-quad (row column w h x y flip angle-cos angle-sin red green blue)
  "Comme PART-QUAD, tourné autour de son centre (cosinus et sinus de l'angle,
dans le sens des aiguilles d'une montre à l'écran)."
  `(let* ((hw (* 0.5f0 scale ,w)) (hh (* 0.5f0 scale ,h))
          (ca ,angle-cos) (sa ,angle-sin) (x ,x) (y ,y)
          (ua (* cell-u (float ,column 1f0)))
          (ub (+ ua (* texel-u ,w)))
          (u0 (if ,flip ub ua)) (u1 (if ,flip ua ub))
          (v0 (* cell-v (float ,row 1f0)))
          (v1 (+ v0 (* texel-v ,h))))
     (declare (type single-float hw hh ca sa x y ua ub u0 u1 v0 v1))
     (rlgl:color-4ub ,red ,green ,blue 255)
     (macrolet ((corner (dx dy)
                  `(rlgl:vertex-2f (fround (+ x (- (* ,dx ca) (* ,dy sa))))
                                   (fround (+ y (+ (* ,dx sa) (* ,dy ca)))))))
       (rlgl::texcoord-2f u0 v0) (corner (- hw) (- hh))
       (rlgl::texcoord-2f u0 v1) (corner (- hw) hh)
       (rlgl::texcoord-2f u1 v1) (corner hw hh)
       (rlgl::texcoord-2f u1 v0) (corner hw (- hh)))))

(defmacro line-quad (x0 y0 x1 y1 red green blue)
  "Trait de couleur unie, de l'épaisseur d'une pique."
  `(let* ((ex (- ,x1 ,x0)) (ey (- ,y1 ,y0))
          (length (sqrt (+ (* ex ex) (* ey ey))))
          (nx (if (plusp length) (* half-line (/ (- ey) length)) 0f0))
          (ny (if (plusp length) (* half-line (/ ex length)) 0f0)))
     (declare (type single-float ex ey length nx ny))
     (rlgl:color-4ub ,red ,green ,blue 255)
     (rlgl::texcoord-2f solid-u solid-v)
     (rlgl:vertex-2f (- ,x0 nx) (- ,y0 ny))
     (rlgl:vertex-2f (+ ,x0 nx) (+ ,y0 ny))
     (rlgl:vertex-2f (+ ,x1 nx) (+ ,y1 ny))
     (rlgl:vertex-2f (- ,x1 nx) (- ,y1 ny))))

(defun draw-soldier-parts (battle alpha camera atlas)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha))
  (multiple-value-bind (body-row body-frames body-w body-h) (atlas-overlay atlas :part-body)
    (declare (ignore body-frames))
    (multiple-value-bind (head-row head-frames head-w head-h) (atlas-overlay atlas :part-head)
      (declare (ignore head-frames))
      (multiple-value-bind (hand-row hand-frames hand-w hand-h) (atlas-overlay atlas :part-hand)
        (declare (ignore hand-frames))
        (multiple-value-bind (foot-row foot-frames foot-w foot-h) (atlas-overlay atlas :part-foot)
          (declare (ignore foot-frames))
          (multiple-value-bind (solid-row) (atlas-overlay atlas :solid)
            (unless (and body-row head-row hand-row foot-row solid-row)
              (error "Morceaux absents de l'atlas : lancer scripts/draw-parts.lisp puis build-atlas."))
            (draw-parts-batch battle alpha camera atlas
                              body-row (float body-w 1f0) (float body-h 1f0)
                              head-row (float head-w 1f0) (float head-h 1f0)
                              hand-row (float hand-w 1f0) (float hand-h 1f0)
                              foot-row (float foot-w 1f0) (float foot-h 1f0)
                              solid-row)))))))

(defun draw-parts-batch (battle alpha camera atlas
                         body-row body-w body-h head-row head-w head-h
                         hand-row hand-w hand-h foot-row foot-w foot-h solid-row)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha body-w body-h head-w head-h hand-w hand-h foot-w foot-h)
           (type fixnum body-row head-row hand-row foot-row solid-row))
  (let* ((soldiers (battle-soldiers battle))
         (atlas-w (float (the fixnum (atlas-width atlas)) 1f0))
         (atlas-h (float (the fixnum (atlas-height atlas)) 1f0))
         (cell (float (the fixnum (atlas-cell atlas)) 1f0))
         (cell-u (/ cell atlas-w)) (cell-v (/ cell atlas-h))
         (texel-u (/ 1f0 atlas-w)) (texel-v (/ 1f0 atlas-h))
         (solid-u (/ 2f0 atlas-w))
         (solid-v (/ (+ 2f0 (* cell (float solid-row 1f0))) atlas-h))
         (count (grid-size (battle-grid battle)))
         (order (grid-sorted (battle-grid battle)))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (prev-xs (soldiers-prev-x soldiers)) (prev-ys (soldiers-prev-y soldiers))
         (facings (soldiers-facing soldiers)) (prev-facings (soldiers-prev-facing soldiers))
         (strike-ticks (soldiers-strike-tick soldiers))
         (soldier-regiments (soldiers-regiment soldiers))
         (regiments (battle-regiments battle))
         (bearers (update-bearers battle))
         (lowered-ends *lowered-ends*)
         (regiment-starts *regiment-starts*)
         (regiment-files *regiment-files*)
         (tick (battle-tick-count battle))
         (zoom (camera-zoom camera))
         (scale (the single-float (camera-sprite-scale camera)))
         (width (float (the fixnum *window-width*) 1f0))
         (height (float (the fixnum *window-height*) 1f0))
         (origin-x (- (* 0.5f0 width) (* zoom (the single-float (camera-x camera)))))
         (origin-y (- (* 0.5f0 height) (* zoom (the single-float (camera-y camera)))))
         (margin (* zoom (the single-float *part-cull-margin*)))
         (strips *direction-strips*)
         (mirrored *direction-mirrored*)
         (eighth (/ (float pi 1f0) 4f0))
         (two-pi (* 2f0 (float pi 1f0)))
         (pi-f (float pi 1f0))
         (threshold2 (expt (the single-float *walk-threshold*) 2))
         ;; Cycle de marche : deux pas par cycle de 8 images de marche.
         (walk-cycles (let ((cycles (* (/ (the single-float *walk-frames-per-second*) 8f0)
                                       (/ (+ (float (the fixnum tick) 1f0) alpha)
                                          (float +ticks-per-second+ 1f0)))))
                        (- cycles (ffloor cycles))))
         (body-height (the single-float *part-body-height*))
         (head-height (the single-float *part-head-height*))
         (foot-height (the single-float *part-foot-height*))
         (foot-side (the single-float *part-foot-side*))
         (stride (the single-float *part-stride*))
         (foot-lift (the single-float *part-foot-lift*))
         (bob-height (the single-float *part-bob*))
         (hand-height (the single-float *part-hand-height*))
         (hand-side (the single-float *part-hand-side*))
         (hand-swing (the single-float *part-hand-swing*))
         (pikes (and *draw-pikes* t))
         (pike-length (the single-float *pike-length*))
         (pike-head (the single-float *pike-head*))
         (pike-lean (the single-float *pike-lean*))
         (pike-hand (the single-float *pike-hand-height*))
         (pike-rise (the single-float *pike-rank-rise*))
         (pike-behind (the single-float *pike-behind*))
         (pike-thrust (the single-float *pike-thrust*))
         (thrust-ticks (* (the single-float *pike-thrust-seconds*) (float +ticks-per-second+ 1f0)))
         (pike-side (the single-float *part-pike-side*))
         (rear-hand (the single-float *part-pike-rear-hand*))
         (front-hand (the single-float *part-pike-front-hand*))
         (hand-thrust (the single-float *part-hand-thrust*))
         (upright-side (the single-float *part-upright-side*))
         (upright-forward (the single-float *part-upright-forward*))
         (low-hand (the single-float *part-upright-low-hand*))
         (high-hand (the single-float *part-upright-high-hand*))
         (pole-height (the single-float *bearer-pole-height*))
         (pole-lean (the single-float *bearer-pole-lean*))
         (finial (the single-float *finial-length*))
         (half-line (* 0.5f0 scale (the single-float *pike-width*)))
         (wood *pike-wood*) (steel *pike-steel*) (gold *finial-color*)
         (wood-r (first wood)) (wood-g (second wood)) (wood-b (third wood))
         (steel-r (first steel)) (steel-g (second steel)) (steel-b (third steel))
         (gold-r (first gold)) (gold-g (second gold)) (gold-b (third gold)))
    (declare (type fixnum count tick)
             (type fixnum-array order strike-ticks soldier-regiments bearers lowered-ends
                   regiment-starts regiment-files)
             (type float-array xs ys prev-xs prev-ys facings prev-facings)
             (type simple-vector strips mirrored)
             (type vector regiments)
             (type single-float cell cell-u cell-v texel-u texel-v solid-u solid-v zoom scale
                   width height origin-x origin-y margin eighth two-pi pi-f threshold2 walk-cycles
                   body-height head-height foot-height foot-side stride foot-lift bob-height
                   hand-height hand-side hand-swing pike-length pike-head pike-lean pike-hand
                   pike-rise pike-behind pike-thrust thrust-ticks pike-side rear-hand front-hand
                   hand-thrust upright-side upright-forward low-hand high-hand pole-height
                   pole-lean finial half-line)
             (type (integer 0 255) wood-r wood-g wood-b steel-r steel-g steel-b
                   gold-r gold-g gold-b))
    (rlgl:set-texture (raylib:texture-id (atlas-texture atlas)))
    (rlgl:begin 7)                      ; RL_QUADS
    (dotimes (k count)
      (let* ((i (aref order k))
             (px (aref prev-xs i)) (py (aref prev-ys i))
             (dx (- (aref xs i) px)) (dy (- (aref ys i) py))
             ;; Point du sol sous le soldat, à l'écran.
             (sx (+ origin-x (* zoom (+ px (* alpha dx)))))
             (sy (+ origin-y (* zoom (+ py (* alpha dy))))))
        (declare (type fixnum i) (type single-float px py dx dy sx sy))
        (when (and (< (- margin) sx (+ width margin)) (< (- margin) sy (+ height margin)))
          (let* ((pf (aref prev-facings i))
                 (facing (+ pf (* alpha (wrap-angle (- (aref facings i) pf)))))
                 (c (cos facing)) (s (sin facing))
                 (direction (mod (the fixnum (round facing eighth)) 8))
                 (frame (the fixnum (svref strips direction)))
                 (mirror (eql 1 (svref mirrored direction)))
                 (regiment-index (aref soldier-regiments i))
                 (bearer (= i (aref bearers regiment-index)))
                 (moving (> (+ (* dx dx) (* dy dy)) threshold2))
                 ;; Phase de marche, décalée par soldat pour casser le pas.
                 (phase (if moving
                            (* two-pi (+ walk-cycles (* 0.375f0 (float (logand i 7) 1f0))))
                            0f0))
                 (swing (sin phase))
                 (lift (cos phase))
                 (bob (if moving (* bob-height (abs lift)) 0f0))
                 (color (side-color (regiment-side (aref regiments regiment-index))))
                 (team-r (first color)) (team-g (second color)) (team-b (third color))
                 ;; Mains et perche (pique ou hampe) : extrémités A (pied) et B
                 ;; (pointe), point C où commence le fer ou la pointe dorée, et
                 ;; point de prise M. La partie A-M et la partie M-B passent
                 ;; chacune derrière ou devant le corps.
                 (hand1-x 0f0) (hand1-y 0f0) (hand1-depth 0f0)
                 (hand2-x 0f0) (hand2-y 0f0) (hand2-depth 0f0)
                 (pole nil)
                 (ax 0f0) (ay 0f0) (mx 0f0) (my 0f0) (cx 0f0) (cy 0f0) (bx 0f0) (by 0f0)
                 (rear-behind nil) (front-behind nil))
            (declare (type single-float pf facing c s phase swing lift bob
                           hand1-x hand1-y hand1-depth hand2-x hand2-y hand2-depth
                           ax ay mx my cx cy bx by)
                     (type fixnum direction frame regiment-index)
                     (type (integer 0 255) team-r team-g team-b))
            (macrolet ((project (f r h)
                         ;; Écran x, écran y et profondeur (vers l'écran) d'un point
                         ;; du repère du soldat.
                         `(let ((ground-x (- (* ,f c) (* ,r s)))
                                (ground-y (+ (* ,f s) (* ,r c))))
                            (values (+ sx (* zoom ground-x))
                                    (- (+ sy (* zoom ground-y)) (* zoom ,h))
                                    ground-y)))
                       (hand (x y) `(part-quad hand-row 0 hand-w hand-h ,x ,y nil 255 255 255))
                       (pole-part (behind)
                         ;; Les parties de la perche du côté BEHIND du corps.
                         `(when pole
                            (when (eq rear-behind ,behind)
                              (line-quad ax ay mx my wood-r wood-g wood-b))
                            (when (eq front-behind ,behind)
                              (line-quad mx my cx cy wood-r wood-g wood-b)
                              (if bearer
                                  (line-quad cx cy bx by gold-r gold-g gold-b)
                                  (line-quad cx cy bx by steel-r steel-g steel-b))))))
              ;; Mains et perche.
              (macrolet ((upright (forward lean top-height)
                           ;; Perche dressée à droite du soldat, mains à deux hauteurs.
                           `(let* ((forward ,forward) (lean ,lean) (top-height ,top-height)
                                   (q (- 1f0 (/ (if bearer finial pike-head) top-height))))
                              (declare (type single-float forward lean top-height q))
                              (setf pole t)
                              (multiple-value-setq (ax ay) (project forward upright-side 0f0))
                              (multiple-value-setq (bx by)
                                (project (+ forward lean) upright-side top-height))
                              (setf cx (+ ax (* q (- bx ax))) cy (+ ay (* q (- by ay))))
                              (multiple-value-setq (hand1-x hand1-y hand1-depth)
                                (project (+ forward (* lean (/ low-hand top-height)))
                                         upright-side low-hand))
                              (multiple-value-setq (hand2-x hand2-y hand2-depth)
                                (project (+ forward (* lean (/ high-hand top-height)))
                                         upright-side high-hand))
                              (setf mx hand1-x my hand1-y
                                    rear-behind (< hand1-depth 0f0)
                                    front-behind rear-behind))))
                (cond (bearer
                       (upright upright-forward pole-lean pole-height))
                      ((and pikes (< i (aref lowered-ends regiment-index)))
                       ;; Pique croisée, à hauteur d'épaule selon le rang ; le coup
                       ;; la pousse en avant, les mains en suivent une part.
                       (let* ((rank (floor (- i (aref regiment-starts regiment-index))
                                           (max 1 (aref regiment-files regiment-index))))
                              (h (+ pike-hand (* pike-rise (float rank 1f0))))
                              (age (+ alpha (float (min 1000000 (- tick (aref strike-ticks i))) 1f0)))
                              (thrust (if (< age thrust-ticks)
                                          (* pike-thrust (sin (* pi-f (/ age thrust-ticks))))
                                          0f0))
                              (grip (* hand-thrust thrust))
                              (butt (- thrust pike-behind))
                              (depth 0f0))
                         (declare (type fixnum rank) (type single-float h age thrust grip butt depth))
                         (setf pole t)
                         (multiple-value-setq (ax ay depth) (project butt pike-side h))
                         (setf rear-behind (< depth 0f0))
                         (multiple-value-setq (mx my) (project thrust pike-side h))
                         (multiple-value-setq (cx cy) (project (+ butt (- pike-length pike-head))
                                                               pike-side h))
                         (multiple-value-setq (bx by depth) (project (+ butt pike-length) pike-side h))
                         (setf front-behind (< depth 0f0))
                         (multiple-value-setq (hand1-x hand1-y hand1-depth)
                           (project (+ grip rear-hand) pike-side h))
                         (multiple-value-setq (hand2-x hand2-y hand2-depth)
                           (project (+ grip front-hand) pike-side h))))
                      (pikes
                       (upright upright-forward pike-lean
                                (sqrt (- (* pike-length pike-length) (* pike-lean pike-lean)))))
                      (t
                       (multiple-value-setq (hand1-x hand1-y hand1-depth)
                         (project (* (- hand-swing) swing) (- hand-side) (+ hand-height bob)))
                       (multiple-value-setq (hand2-x hand2-y hand2-depth)
                         (project (* hand-swing swing) hand-side (+ hand-height bob))))))
              ;; Pieds, le plus éloigné d'abord.
              (multiple-value-bind (left-x left-y left-depth)
                  (project (* stride swing) (- foot-side)
                           (+ foot-height (* foot-lift (max 0f0 lift))))
                (declare (type single-float left-x left-y left-depth))
                (multiple-value-bind (right-x right-y right-depth)
                    (project (* (- stride) swing) foot-side
                             (+ foot-height (* foot-lift (max 0f0 (- lift)))))
                  (declare (type single-float right-x right-y right-depth))
                  (if (< left-depth right-depth)
                      (progn (part-quad foot-row 0 foot-w foot-h left-x left-y nil 255 255 255)
                             (part-quad foot-row 0 foot-w foot-h right-x right-y nil 255 255 255))
                      (progn (part-quad foot-row 0 foot-w foot-h right-x right-y nil 255 255 255)
                             (part-quad foot-row 0 foot-w foot-h left-x left-y nil 255 255 255)))))
              ;; Derrière le corps.
              (pole-part t)
              (when (< hand1-depth 0f0) (hand hand1-x hand1-y))
              (when (< hand2-depth 0f0) (hand hand2-x hand2-y))
              ;; Corps et son masque, puis la tête.
              (let ((body-y (- sy (* zoom (+ body-height bob))))
                    (head-y (- sy (* zoom (+ head-height bob)))))
                (declare (type single-float body-y head-y))
                (part-quad body-row frame body-w body-h sx body-y mirror 255 255 255)
                (part-quad (1+ body-row) frame body-w body-h sx body-y mirror team-r team-g team-b)
                (part-quad head-row frame head-w head-h sx head-y mirror 255 255 255))
              ;; Devant le corps.
              (pole-part nil)
              (when (>= hand1-depth 0f0) (hand hand1-x hand1-y))
              (when (>= hand2-depth 0f0) (hand hand2-x hand2-y)))))))
    (rlgl:end)
    (rlgl:set-texture 0)))

;;; Morts : chaque mort du journal (deaths.lisp) bascule vers l'arrière autour
;;; de ses pieds en *DEATH-FALL-SECONDS*, puis reste couché, assombri. Son corps
;;; et sa tête tournent d'un quart de tour à l'écran ; ses mains tombent en
;;; s'écartant ; sa pique s'abat au sol. Les morts se dessinent avant les
;;; vivants, qui passent par-dessus. Les écarts (direction de chute, mains,
;;; pique) viennent d'un hachage du numéro du mort : le dessin ne tire rien au
;;; hasard et ne touche pas au générateur de la bataille.

(defparameter *death-fall-seconds* 0.45f0 "Durée de la chute d'un mort, en secondes.")
(defparameter *death-fall-spread* 0.6f0
  "Écart de la direction de chute autour de l'arrière du soldat, en radians.")
(defparameter *corpse-shade* 165 "Luminosité d'un mort couché, sur 255.")
(defparameter *corpse-height* 0.1f0
  "Hauteur du corps et de la tête d'un mort couché, en mètres.")
(defparameter *corpse-hand-drop* 0.04f0 "Hauteur des mains d'un mort, en mètres.")
(defparameter *corpse-hand-scatter* 0.3f0
  "Les mains d'un mort roulent jusqu'à cette distance, en mètres.")

(declaim (inline death-jitter))
(defun death-jitter (serial salt)
  "Nombre dans [0, 1) tiré du numéro SERIAL d'un mort et de SALT, toujours le même."
  (declare (type fixnum serial salt) (optimize (speed 3) (safety 0)))
  (let* ((x (logand (+ (* (logand serial #xfffff) 7919) (* salt 104729)) #xfffff))
         (h (logand (* x 2654435761) #xffffffff)))
    (declare (type (unsigned-byte 20) x) (type (unsigned-byte 32) h))
    (/ (float (ldb (byte 16 16) h) 1f0) 65536f0)))

(defun draw-corpses (battle alpha camera atlas)
  (multiple-value-bind (body-row body-frames body-w body-h) (atlas-overlay atlas :part-body)
    (declare (ignore body-frames))
    (multiple-value-bind (head-row head-frames head-w head-h) (atlas-overlay atlas :part-head)
      (declare (ignore head-frames))
      (multiple-value-bind (hand-row hand-frames hand-w hand-h) (atlas-overlay atlas :part-hand)
        (declare (ignore hand-frames))
        (multiple-value-bind (foot-row foot-frames foot-w foot-h) (atlas-overlay atlas :part-foot)
          (declare (ignore foot-frames))
          (multiple-value-bind (solid-row) (atlas-overlay atlas :solid)
            (when (and body-row head-row hand-row foot-row solid-row)
              (draw-corpses-batch battle alpha camera atlas
                                  body-row (float body-w 1f0) (float body-h 1f0)
                                  head-row (float head-w 1f0) (float head-h 1f0)
                                  hand-row (float hand-w 1f0) (float hand-h 1f0)
                                  foot-row (float foot-w 1f0) (float foot-h 1f0)
                                  solid-row))))))))

(defun draw-corpses-batch (battle alpha camera atlas
                           body-row body-w body-h head-row head-w head-h
                           hand-row hand-w hand-h foot-row foot-w foot-h solid-row)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha body-w body-h head-w head-h hand-w hand-h foot-w foot-h)
           (type fixnum body-row head-row hand-row foot-row solid-row))
  (let* ((log (battle-deaths battle))
         (total (death-count log))
         (capacity (death-log-capacity log))
         (n (min total capacity))
         (oldest (if (> total capacity) (mod total capacity) 0))
         (xs (death-x log)) (ys (death-y log)) (facings (death-facing log))
         (sides (death-side log)) (ticks (death-tick log)) (ranks (death-rank log))
         (now (+ (float (the fixnum (battle-tick-count battle)) 1f0) alpha))
         (fall-ticks (* (the single-float *death-fall-seconds*) (float +ticks-per-second+ 1f0)))
         (atlas-w (float (the fixnum (atlas-width atlas)) 1f0))
         (atlas-h (float (the fixnum (atlas-height atlas)) 1f0))
         (cell (float (the fixnum (atlas-cell atlas)) 1f0))
         (cell-u (/ cell atlas-w)) (cell-v (/ cell atlas-h))
         (texel-u (/ 1f0 atlas-w)) (texel-v (/ 1f0 atlas-h))
         (solid-u (/ 2f0 atlas-w))
         (solid-v (/ (+ 2f0 (* cell (float solid-row 1f0))) atlas-h))
         (zoom (camera-zoom camera))
         (scale (the single-float (camera-sprite-scale camera)))
         (width (float (the fixnum *window-width*) 1f0))
         (height (float (the fixnum *window-height*) 1f0))
         (origin-x (- (* 0.5f0 width) (* zoom (the single-float (camera-x camera)))))
         (origin-y (- (* 0.5f0 height) (* zoom (the single-float (camera-y camera)))))
         (margin (* zoom (the single-float *part-cull-margin*)))
         (strips *direction-strips*)
         (mirrored *direction-mirrored*)
         (eighth (/ (float pi 1f0) 4f0))
         (pi-f (float pi 1f0))
         (half-pi (* 0.5f0 (float pi 1f0)))
         (spread (the single-float *death-fall-spread*))
         (corpse-shade (the fixnum *corpse-shade*))
         (lying (the single-float *corpse-height*))
         (hand-drop (the single-float *corpse-hand-drop*))
         (hand-scatter (the single-float *corpse-hand-scatter*))
         (body-height (the single-float *part-body-height*))
         (head-height (the single-float *part-head-height*))
         (foot-height (the single-float *part-foot-height*))
         (foot-side (the single-float *part-foot-side*))
         (hand-height (the single-float *part-hand-height*))
         (hand-side (the single-float *part-hand-side*))
         (pikes (and *draw-pikes* t))
         (lowered-ranks (the fixnum *pike-lowered-ranks*))
         (pike-length (the single-float *pike-length*))
         (pike-head (the single-float *pike-head*))
         (pike-hand (the single-float *pike-hand-height*))
         (pike-rise (the single-float *pike-rank-rise*))
         (pike-behind (the single-float *pike-behind*))
         (pike-side (the single-float *part-pike-side*))
         (upright-side (the single-float *part-upright-side*))
         ;; Pique dressée : inclinaison de départ, depuis la verticale.
         (upright-tilt (asin (min 1f0 (/ (the single-float *pike-lean*) pike-length))))
         (head-fraction (- 1f0 (/ pike-head pike-length)))
         (half-line (* 0.5f0 scale (the single-float *pike-width*)))
         (wood *pike-wood*) (steel *pike-steel*))
    (declare (type fixnum total capacity n oldest corpse-shade lowered-ranks)
             (type float-array xs ys facings)
             (type fixnum-array sides ticks ranks)
             (type simple-vector strips mirrored)
             (type single-float now fall-ticks cell cell-u cell-v texel-u texel-v solid-u solid-v
                   zoom scale width height origin-x origin-y margin eighth pi-f half-pi spread
                   lying hand-drop hand-scatter body-height head-height foot-height foot-side
                   hand-height hand-side pike-length pike-head pike-hand pike-rise pike-behind
                   pike-side upright-side upright-tilt head-fraction half-line))
    (when (plusp n)
      (rlgl:set-texture (raylib:texture-id (atlas-texture atlas)))
      (rlgl:begin 7)                    ; RL_QUADS
      (dotimes (k n)
        (let* ((slot (mod (+ oldest k) capacity))
               (serial (+ (- total n) k))
               (sx (+ origin-x (* zoom (aref xs slot))))
               (sy (+ origin-y (* zoom (aref ys slot)))))
          (declare (type fixnum slot serial) (type single-float sx sy))
          (when (and (< (- margin) sx (+ width margin)) (< (- margin) sy (+ height margin)))
            (let* ((facing (aref facings slot))
                   (c (cos facing)) (s (sin facing))
                   (direction (mod (the fixnum (round facing eighth)) 8))
                   (frame (the fixnum (svref strips direction)))
                   (mirror (eql 1 (svref mirrored direction)))
                   (progress (max 0f0 (min 1f0 (/ (- now (float (aref ticks slot) 1f0)) fall-ticks))))
                   ;; Chute accélérée : l'angle depuis la verticale va de 0 à un quart de tour.
                   (fall (* progress progress))
                   (tilt (* fall half-pi))
                   ;; Couché : un quart de tour exact, sans trigonométrie.
                   (settled (>= progress 1f0))
                   (sin-tilt (if settled 1f0 (sin tilt)))
                   (cos-tilt (if settled 0f0 (cos tilt)))
                   (fall-angle (+ facing pi-f (* spread (- (* 2f0 (death-jitter serial 1)) 1f0))))
                   (gdx (cos fall-angle)) (gdy (sin fall-angle))
                   ;; À l'écran, le corps tourne vers le côté où il tombe.
                   (turn (if (>= gdx 0f0) tilt (- tilt)))
                   (turn-cos (if settled 0f0 (cos turn)))
                   (turn-sin (cond ((not settled) (sin turn)) ((>= gdx 0f0) 1f0) (t -1f0)))
                   (light (the fixnum (round (- 255f0 (* (float (- 255 corpse-shade) 1f0) progress)))))
                   (color (side-color (aref sides slot)))
                   (team-r (floor (* (the fixnum (first color)) light) 255))
                   (team-g (floor (* (the fixnum (second color)) light) 255))
                   (team-b (floor (* (the fixnum (third color)) light) 255)))
              (declare (type single-float facing c s progress fall tilt sin-tilt cos-tilt
                             fall-angle gdx gdy turn turn-cos turn-sin)
                       (type fixnum direction frame light team-r team-g team-b))
              (macrolet ((ground (wx wy h)
                           ;; Écran d'un point décalé de WX, WY au sol et à la hauteur H.
                           `(values (+ sx (* zoom ,wx)) (- (+ sy (* zoom ,wy)) (* zoom ,h))))
                         (axis (h)
                           ;; Point du corps à la hauteur H debout, basculé.
                           `(ground (* gdx ,h sin-tilt) (* gdy ,h sin-tilt)
                                    (+ (* ,h cos-tilt) (* lying sin-tilt)))))
                ;; Pique : elle s'abat vers l'avant si elle était croisée, sinon
                ;; du côté où tombe le soldat.
                (when pikes
                  (let* ((rank (aref ranks slot))
                         (lowered (< rank lowered-ranks))
                         (angle (if lowered
                                    (+ facing (* 0.3f0 (- (* 2f0 (death-jitter serial 2)) 1f0)))
                                    (+ fall-angle (* 0.4f0 (- (* 2f0 (death-jitter serial 2)) 1f0)))))
                         (pdx (cos angle)) (pdy (sin angle))
                         (side (if lowered pike-side upright-side))
                         ;; Pied de la pique, au sol, à droite du soldat.
                         (base-x (- (* side s))) (base-y (* side c))
                         (ax 0f0) (ay 0f0) (bx 0f0) (by 0f0))
                    (declare (type fixnum rank)
                             (type single-float angle pdx pdy side base-x base-y ax ay bx by))
                    (if lowered
                        (let ((h (* (+ pike-hand (* pike-rise (float (min rank lowered-ranks) 1f0)))
                                    (- 1f0 fall))))
                          (declare (type single-float h))
                          (multiple-value-setq (ax ay)
                            (ground (- base-x (* pdx pike-behind)) (- base-y (* pdy pike-behind)) h))
                          (multiple-value-setq (bx by)
                            (ground (+ base-x (* pdx (- pike-length pike-behind)))
                                    (+ base-y (* pdy (- pike-length pike-behind))) h)))
                        (let* ((lean (+ upright-tilt (* fall (- half-pi upright-tilt))))
                               (reach (if settled pike-length (* pike-length (sin lean))))
                               (rise (if settled 0f0 (* pike-length (cos lean)))))
                          (declare (type single-float lean reach rise))
                          (multiple-value-setq (ax ay) (ground base-x base-y 0f0))
                          (multiple-value-setq (bx by)
                            (ground (+ base-x (* pdx reach)) (+ base-y (* pdy reach)) rise))))
                    (let ((cx (+ ax (* head-fraction (- bx ax))))
                          (cy (+ ay (* head-fraction (- by ay)))))
                      (declare (type single-float cx cy))
                      (line-quad ax ay cx cy
                                 (floor (* (the fixnum (first wood)) light) 255)
                                 (floor (* (the fixnum (second wood)) light) 255)
                                 (floor (* (the fixnum (third wood)) light) 255))
                      (line-quad cx cy bx by
                                 (floor (* (the fixnum (first steel)) light) 255)
                                 (floor (* (the fixnum (second steel)) light) 255)
                                 (floor (* (the fixnum (third steel)) light) 255)))))
                ;; Pieds, restés en place.
                (multiple-value-bind (x y) (ground (* foot-side s) (- (* foot-side c)) foot-height)
                  (declare (type single-float x y))
                  (part-quad foot-row 0 foot-w foot-h x y nil light light light))
                (multiple-value-bind (x y) (ground (- (* foot-side s)) (* foot-side c) foot-height)
                  (declare (type single-float x y))
                  (part-quad foot-row 0 foot-w foot-h x y nil light light light))
                ;; Mains : elles tombent et roulent un peu, chacune de son côté.
                (macrolet ((dropped-hand (sign salt)
                             `(let* ((roll (* hand-scatter (death-jitter serial ,salt)))
                                     (start-x (- (* ,sign hand-side s)))
                                     (start-y (* ,sign hand-side c))
                                     (wx (+ start-x (* progress (+ (* start-x roll) (* gdx roll)))))
                                     (wy (+ start-y (* progress (+ (* start-y roll) (* gdy roll)))))
                                     (h (+ (* hand-height (- 1f0 fall)) (* hand-drop fall))))
                                (declare (type single-float roll start-x start-y wx wy h))
                                (multiple-value-bind (x y) (ground wx wy h)
                                  (declare (type single-float x y))
                                  (part-quad hand-row 0 hand-w hand-h x y nil light light light)))))
                  (dropped-hand -1f0 3)
                  (dropped-hand 1f0 4))
                ;; Corps, son masque, puis la tête.
                (multiple-value-bind (x y) (axis body-height)
                  (declare (type single-float x y))
                  (turned-part-quad body-row frame body-w body-h x y mirror turn-cos turn-sin
                                    light light light)
                  (turned-part-quad (1+ body-row) frame body-w body-h x y mirror turn-cos turn-sin
                                    team-r team-g team-b))
                (multiple-value-bind (x y) (axis head-height)
                  (declare (type single-float x y))
                  (turned-part-quad head-row frame head-w head-h x y mirror turn-cos turn-sin
                                    light light light)))))))
      (rlgl:end)
      (rlgl:set-texture 0))))
