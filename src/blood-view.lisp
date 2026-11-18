(in-package #:rogue-company)

;;; Sang : à chaque coup qui porte (journal des blessures, wounds.lisp), des
;;; gouttelettes giclent du corps du blessé dans la direction du coup, suivent
;;; un arc balistique, puis tachent le sol où elles tombent ; la tache fonce en
;;; séchant. Tout se déduit du journal et de l'instant dessiné : rien n'est
;;; stocké, et les écarts de chaque goutte viennent d'un hachage du numéro de la
;;; blessure (DEATH-JITTER), jamais du générateur de la bataille.
;;;
;;; Deux couches dans le lot rlgl : les taches au sol avant les morts et les
;;; soldats, les gouttes en vol après eux (DRAW-SOLDIERS, battle-view.lisp).

(defparameter *draw-blood* t "Dessiner le sang.")
(defparameter *blood-droplets* 3 "Gouttes par blessure, au moins.")
(defparameter *blood-droplets-per-damage* 0.08f0
  "Gouttes de plus par point de dégât : une balle ou un coup de pique saignent plus.")
(defparameter *blood-max-droplets* 8 "Gouttes par blessure, au plus.")
(defparameter *blood-height* 0.95f0 "Hauteur de la plaie, en mètres du sol.")
(defparameter *blood-height-spread* 0.3f0 "Écart de hauteur de la plaie, en mètres.")
(defparameter *blood-speed* 0.4f0 "Vitesse au sol d'une goutte, au moins, en m/s.")
(defparameter *blood-speed-spread* 1.1f0 "Vitesse au sol en plus, au plus, en m/s.")
(defparameter *blood-lift* 0.3f0 "Vitesse vers le haut d'une goutte, au moins, en m/s.")
(defparameter *blood-lift-spread* 1.2f0 "Vitesse vers le haut en plus, au plus, en m/s.")
(defparameter *blood-cone* 1.3f0
  "Ouverture de la gerbe autour de la direction du coup, en radians.")
(defparameter *blood-gravity* 9.8f0 "Gravité des gouttes, en m/s².")
(defparameter *blood-color* '(178 18 22) "Sang frais.")
(defparameter *blood-dry-color* '(92 16 14) "Sang séché.")
(defparameter *blood-dry-seconds* 25f0 "Temps qu'une tache met à sécher.")

(defun draw-blood (battle alpha camera atlas layer)
  "Dessine le sang de BATTLE : LAYER 0, les taches au sol ; 1, les gouttes en vol."
  (when *draw-blood*
    (multiple-value-bind (solid-row) (atlas-overlay atlas :solid)
      (when solid-row
        (draw-blood-batch battle alpha camera atlas solid-row layer)))))

(defun draw-blood-batch (battle alpha camera atlas solid-row layer)
  (declare (optimize (speed 3) (safety 1))
           (type single-float alpha) (type fixnum solid-row layer))
  (let* ((log (battle-wounds battle))
         (total (wound-count log))
         (capacity (wound-log-capacity log))
         (n (min total capacity))
         (oldest (if (> total capacity) (mod total capacity) 0))
         (xs (wound-x log)) (ys (wound-y log))
         (dxs (wound-dx log)) (dys (wound-dy log))
         (damages (wound-damage log)) (ticks (wound-tick log))
         (now (+ (float (the fixnum (battle-tick-count battle)) 1f0) alpha))
         (per-tick (/ 1f0 (float +ticks-per-second+ 1f0)))
         (atlas-w (float (the fixnum (atlas-width atlas)) 1f0))
         (atlas-h (float (the fixnum (atlas-height atlas)) 1f0))
         (cell (float (the fixnum (atlas-cell atlas)) 1f0))
         (solid-u (/ 2f0 atlas-w))
         (solid-v (/ (+ 2f0 (* cell (float solid-row 1f0))) atlas-h))
         (zoom (camera-zoom camera))
         (scale (the single-float (camera-sprite-scale camera)))
         (width (float (the fixnum *window-width*) 1f0))
         (height (float (the fixnum *window-height*) 1f0))
         (origin-x (- (* 0.5f0 width) (* zoom (the single-float (camera-x camera)))))
         (origin-y (- (* 0.5f0 height) (* zoom (the single-float (camera-y camera)))))
         (margin (* zoom 3f0))
         (squash (the single-float *part-depth-squash*))
         (min-drops (the fixnum *blood-droplets*))
         (max-drops (the fixnum *blood-max-droplets*))
         (drops-per-damage (the single-float *blood-droplets-per-damage*))
         (wound-height (the single-float *blood-height*))
         (height-spread (the single-float *blood-height-spread*))
         (speed-min (the single-float *blood-speed*))
         (speed-spread (the single-float *blood-speed-spread*))
         (lift-min (the single-float *blood-lift*))
         (lift-spread (the single-float *blood-lift-spread*))
         (cone (the single-float *blood-cone*))
         (gravity (the single-float *blood-gravity*))
         (dry-ticks (* (the single-float *blood-dry-seconds*) (float +ticks-per-second+ 1f0)))
         (fresh *blood-color*) (dry *blood-dry-color*)
         (fresh-r (float (the fixnum (first fresh)) 1f0))
         (fresh-g (float (the fixnum (second fresh)) 1f0))
         (fresh-b (float (the fixnum (third fresh)) 1f0))
         (dry-r (float (the fixnum (first dry)) 1f0))
         (dry-g (float (the fixnum (second dry)) 1f0))
         (dry-b (float (the fixnum (third dry)) 1f0)))
    (declare (type fixnum total capacity n oldest min-drops max-drops)
             (type float-array xs ys dxs dys damages) (type fixnum-array ticks)
             (type single-float now per-tick atlas-w atlas-h cell solid-u solid-v zoom scale
                   width height origin-x origin-y margin squash drops-per-damage wound-height
                   height-spread speed-min speed-spread lift-min lift-spread cone gravity
                   dry-ticks fresh-r fresh-g fresh-b dry-r dry-g dry-b))
    (when (plusp n)
      (rlgl:set-texture (raylib:texture-id (atlas-texture atlas)))
      (rlgl:begin 7)                    ; RL_QUADS
      (macrolet ((pixel (x y w h red green blue)
                   ;; Rectangle de W × H pixels de sprite, coin en X, Y, calé
                   ;; sur les pixels de l'écran.
                   `(let* ((left (fround ,x)) (top (fround ,y))
                           (right (+ left (* scale ,w))) (bottom (+ top (* scale ,h))))
                      (declare (type single-float left top right bottom))
                      (rlgl:color-4ub ,red ,green ,blue 255)
                      (rlgl::texcoord-2f solid-u solid-v)
                      (rlgl:vertex-2f left top)
                      (rlgl:vertex-2f left bottom)
                      (rlgl:vertex-2f right bottom)
                      (rlgl:vertex-2f right top))))
        (dotimes (k n)
          (let* ((slot (mod (+ oldest k) capacity))
                 (serial (+ (- total n) k))
                 (sx (+ origin-x (* zoom (aref xs slot))))
                 (sy (+ origin-y (* zoom (aref ys slot))))
                 (age (* per-tick (- now (float (aref ticks slot) 1f0)))))
            (declare (type fixnum slot serial) (type single-float sx sy age))
            (when (and (>= age 0f0)
                       (< (- margin) sx (+ width margin)) (< (- margin) sy (+ height margin)))
              (let* ((drops (min max-drops
                                 (+ min-drops (the fixnum (floor (* drops-per-damage
                                                                    (aref damages slot)))))))
                     (heading (atan (aref dys slot) (aref dxs slot)))
                     (h0 (+ wound-height (* height-spread (- (death-jitter serial 1) 0.5f0))))
                     ;; Une tache sèche : du frais au sec en *BLOOD-DRY-SECONDS*.
                     (dryness (min 1f0 (/ (* age (float +ticks-per-second+ 1f0)) dry-ticks))))
                (declare (type fixnum drops) (type single-float heading h0 dryness))
                (dotimes (d drops)
                  (let* ((salt (+ 2 (* 5 d)))
                         (angle (+ heading (* cone (- (death-jitter serial salt) 0.5f0))))
                         (ground-speed (+ speed-min (* speed-spread (death-jitter serial (+ salt 1)))))
                         (lift (+ lift-min (* lift-spread (death-jitter serial (+ salt 2)))))
                         ;; Temps de vol : h0 + lift t - g t²/2 = 0.
                         (flight (/ (+ lift (sqrt (+ (* lift lift) (* 2f0 gravity h0)))) gravity))
                         (big (< (death-jitter serial (+ salt 3)) 0.35f0)))
                    (declare (type fixnum salt) (type single-float angle ground-speed lift flight))
                    (if (< age flight)
                        (when (= layer 1)
                          (let* ((reach (* ground-speed age))
                                 (up (- (+ h0 (* lift age)) (* 0.5f0 gravity age age))))
                            (pixel (+ sx (* zoom reach (cos angle)))
                                   (- (+ sy (* zoom squash reach (sin angle))) (* zoom up))
                                   1f0 1f0
                                   (the fixnum (round fresh-r)) (the fixnum (round fresh-g))
                                   (the fixnum (round fresh-b)))))
                        (when (= layer 0)
                          (let* ((reach (* ground-speed flight))
                                 (r (the fixnum (round (+ fresh-r (* dryness (- dry-r fresh-r))))))
                                 (g (the fixnum (round (+ fresh-g (* dryness (- dry-g fresh-g))))))
                                 (b (the fixnum (round (+ fresh-b (* dryness (- dry-b fresh-b)))))))
                            (pixel (+ sx (* zoom reach (cos angle)))
                                   (+ sy (* zoom squash reach (sin angle)))
                                   (if big 2f0 1f0) 1f0 r g b)))))))))))
      (rlgl:end)
      (rlgl:set-texture 0))))
