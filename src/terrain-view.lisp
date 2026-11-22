(in-package #:rogue-company)

;;; Dessin du terrain (terrain.lisp) : le sol ombré par le relief, en fond.
;;;
;;; L'ombrage se calcule une fois, quand le terrain change : pour chaque texel,
;;; la pente du sol donne sa normale, éclairée par une lumière venant du
;;; nord-ouest à 45 degrés (*TERRAIN-LIGHT*). Le résultat va dans une texture
;;; (*TERRAIN-TEXTURE*, chargée par rlgl sur le thread principal), qui se
;;; dessine en un seul quad par image, sous les soldats.

(defparameter *terrain-texels-per-meter* 2 "Résolution de la texture du sol.")

(defparameter *ground-color* '(92 112 64) "Couleur de l'herbe sur un sol plat.")

(defparameter *terrain-light* '(-1f0 -1f0 1.4142135f0)
  "Direction vers la lumière (x vers l'est, y vers le sud, z vers le haut) :
du nord-ouest, à 45 degrés au-dessus de l'horizon.")

(defparameter *terrain-shade-strength* 2.5f0
  "Contraste de l'ombrage : 1 pour l'éclairage seul, plus pour marquer des pentes
douces.")

(defparameter *terrain-shade-range* '(0.6f0 . 1.3f0)
  "Plus sombre et plus clair facteur de luminosité du sol.")

(defvar *topographic-mode* nil "Vrai : le terrain se dessine en carte topographique.")

(defvar *terrain-texture* nil
  "(terrain . identifiant rlgl) : la texture du sol et le terrain qu'elle montre.")

(defun ground-brightness (terrain x y)
  "Luminosité du sol en X, Y : 1 à plat, plus clair face à la lumière, plus
sombre à l'opposé."
  (multiple-value-bind (hx hy) (terrain-slope terrain x y)
    (destructuring-bind (lx ly lz) *terrain-light*
      (let* ((llen (sqrt (+ (* lx lx) (* ly ly) (* lz lz))))
             ;; Normale du sol (-dh/dx, -dh/dy, 1), non normée.
             (nlen (sqrt (+ (* hx hx) (* hy hy) 1f0)))
             (dot (/ (+ (* (- hx) lx) (* (- hy) ly) lz) (* nlen llen)))
             (flat (/ lz llen)))
        (max (car *terrain-shade-range*)
             (min (cdr *terrain-shade-range*)
                  (+ 1f0 (* *terrain-shade-strength* (- (/ dot flat) 1f0)))))))))

(defun draw-terrain (battle camera)
  "Le sol de BATTLE, ombré par son relief, ou en carte topographique
(*TOPOGRAPHIC-MODE*) ; rien sur un terrain plat (fond noir)."
  (let ((terrain (battle-terrain battle)))
    (when terrain
      (let* ((id (if *topographic-mode*
                     (cached-texture '*topographic-texture* terrain #'topographic-texel)
                     (cached-texture '*terrain-texture* terrain #'shaded-texel)))
             (scale (camera-zoom camera))
             (half (* 0.5f0 (terrain-size terrain)))
             (left (+ (* 0.5f0 *window-width*) (* scale (- (- half) (camera-x camera)))))
             (top (+ (* 0.5f0 *window-height*) (* scale (- (- half) (camera-y camera)))))
             (right (+ left (* scale 2 half)))
             (bottom (+ top (* scale 2 half))))
        (rlgl:set-texture id)
        (rlgl:begin 7)                  ; RL_QUADS
        (rlgl:color-4ub 255 255 255 255)
        (rlgl::texcoord-2f 0f0 0f0) (rlgl:vertex-2f left top)
        (rlgl::texcoord-2f 0f0 1f0) (rlgl:vertex-2f left bottom)
        (rlgl::texcoord-2f 1f0 1f0) (rlgl:vertex-2f right bottom)
        (rlgl::texcoord-2f 1f0 0f0) (rlgl:vertex-2f right top)
        (rlgl:end)
        (rlgl:set-texture 0)
        (when *topographic-mode*
          (draw-contours terrain camera))))))

;;; Mode topographique (touche M) : le sol teint selon l'altitude, des courbes
;;; de niveau tous les *CONTOUR-INTERVAL* mètres (maîtresses tous les
;;; *CONTOUR-MAJOR*), l'altitude sous la souris. Teinte et courbes se calculent
;;; une fois par terrain, comme l'ombrage.

(defparameter *contour-interval* 2f0 "Écart d'altitude entre deux courbes de niveau, en mètres.")

(defparameter *contour-major* 10f0 "Écart d'altitude entre deux courbes maîtresses, en mètres.")

(defparameter *contour-color* '(70 45 25) "Couleur des courbes de niveau.")

(defparameter *contour-alpha* '(130 . 220) "Opacité d'une courbe ordinaire et d'une maîtresse.")

(defparameter *contour-width* '(2f0 . 4f0)
  "Épaisseur à l'écran d'une courbe ordinaire et d'une maîtresse, en pixels.")

(defparameter *height-tints*
  '((0f0 112 140 74) (4f0 150 160 86) (8f0 184 168 100) (14f0 176 138 88) (24f0 150 112 78))
  "Teintes d'altitude : (hauteur rouge vert bleu), interpolées entre deux paliers.")

(defparameter *topographic-shade* 0.5f0
  "Part de l'ombrage du relief gardée sous les teintes d'altitude.")

(defvar *topographic-texture* nil "(terrain . identifiant rlgl) de la carte topographique.")

(defvar *terrain-contours* nil "(terrain . segments) : les courbes de niveau du terrain.")

(defvar *altitude-text* (make-text-buffer) "Altitude sous la souris, sans allouer.")

(defun height-tint (height)
  "Couleur (r g b) de l'altitude HEIGHT, interpolée entre les *HEIGHT-TINTS*."
  (loop for (low . high) on *height-tints*
        for next = (first high)
        when (or (null next) (< height (first next)))
          return (if (or (null next) (< height (first low)))
                     (rest low)
                     (let ((f (/ (- height (first low)) (- (first next) (first low)))))
                       (mapcar (lambda (a b) (+ a (* f (- b a)))) (rest low) (rest next))))))

(defun topographic-texel (terrain x y)
  "Couleur du texel en X, Y : la teinte de son altitude, un peu ombrée."
  (let ((light (+ 1f0 (* *topographic-shade* (- (ground-brightness terrain x y) 1f0)))))
    (mapcar (lambda (channel) (min 255 (max 0 (round (* channel light)))))
            (height-tint (terrain-height terrain x y)))))

(defun build-texture (terrain texel)
  "Texture de TERRAIN, chaque texel coloré par TEXEL (terrain x y) -> (r g b).
Thread principal seulement."
  (let* ((size (terrain-size terrain))
         (texels (max 1 (round (* size *terrain-texels-per-meter*))))
         (step (/ size texels))
         (half (* 0.5f0 size)))
    (cffi:with-foreign-object (data :uint8 (* texels texels 4))
      (dotimes (j texels)
        (dotimes (i texels)
          (let ((color (funcall texel terrain
                                (+ (- half) (* step (+ i 0.5f0)))
                                (+ (- half) (* step (+ j 0.5f0)))))
                (k (* 4 (+ i (* j texels)))))
            (loop for channel in color
                  for offset from 0
                  do (setf (cffi:mem-aref data :uint8 (+ k offset)) channel))
            (setf (cffi:mem-aref data :uint8 (+ k 3)) 255))))
      ;; RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7, une seule image.
      (let ((id (rlgl:load-texture data texels texels 7 1)))
        ;; Filtre linéaire (GL_LINEAR) en réduction et en agrandissement : le
        ;; relief reste doux de près.
        (rlgl:texture-parameters id #x2801 #x2601)
        (rlgl:texture-parameters id #x2800 #x2601)
        id))))

(defun shaded-texel (terrain x y)
  "Couleur du texel en X, Y : l'herbe ombrée par le relief."
  (let ((light (ground-brightness terrain x y)))
    (mapcar (lambda (channel) (min 255 (round (* channel light)))) *ground-color*)))

(defun cached-texture (place terrain texel)
  "Identifiant de la texture de TERRAIN gardée dans le symbole PLACE, refaite
quand le terrain change."
  (let ((cache (symbol-value place)))
    (unless (eq (car cache) terrain)
      (when cache (rlgl:unload-texture (cdr cache)))
      (setf (symbol-value place) (cons terrain (build-texture terrain texel))))
    (cdr (symbol-value place))))

(defun contour-segments (terrain &key (interval *contour-interval*) (major *contour-major*))
  "Courbes de niveau de TERRAIN tous les INTERVAL mètres, par carrés marchants
sur sa carte d'élévation : vecteur de segments (x0 y0 x1 y1 maîtresse-p) à plat,
cinq nombres par segment (maîtresse : 1, sinon 0)."
  (let* ((points (terrain-points terrain))
         (heights (terrain-heights terrain))
         (cell *terrain-cell*)
         (half (* 0.5f0 (terrain-size terrain)))
         (top (reduce #'max heights))
         (segments (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)))
    (flet ((h (i j) (aref heights (+ i (* j points))))
           (emit (p q flag)
             (dolist (value (list (car p) (cdr p) (car q) (cdr q) flag))
               (vector-push-extend value segments))))
      (loop for level from interval to top by interval
            for flag = (if (< (abs (- (/ level major) (round (/ level major)))) 1f-4) 1f0 0f0)
            do (dotimes (j (1- points))
                 (dotimes (i (1- points))
                   (let ((a (h i j)) (b (h (1+ i) j)) (c (h (1+ i) (1+ j))) (d (h i (1+ j))))
                     (when (and (< (min a b c d) level) (>= (max a b c d) level))
                       (let ((x (- (* i cell) half)) (y (- (* j cell) half)))
                         (flet ((cross (h0 h1 x0 y0 x1 y1)
                                  ;; Point de l'arête où passe le niveau, ou NIL.
                                  (when (not (eq (>= h0 level) (>= h1 level)))
                                    (let ((f (/ (- level h0) (- h1 h0))))
                                      (cons (+ x0 (* f (- x1 x0))) (+ y0 (* f (- y1 y0))))))))
                           (let ((north (cross a b x y (+ x cell) y))
                                 (east (cross b c (+ x cell) y (+ x cell) (+ y cell)))
                                 (south (cross d c x (+ y cell) (+ x cell) (+ y cell)))
                                 (west (cross a d x y x (+ y cell))))
                             (if (and north east south west)
                                 ;; Col : le centre dit quels coins se rejoignent.
                                 (if (eq (>= (* 0.25f0 (+ a b c d)) level) (>= a level))
                                     (progn (emit north east flag) (emit south west flag))
                                     (progn (emit north west flag) (emit east south flag)))
                                 (let ((ends (remove nil (list north east south west))))
                                   (when (= (length ends) 2)
                                     (emit (first ends) (second ends) flag)))))))))))))
    (coerce segments '(simple-array single-float (*)))))

(defun terrain-contours (terrain)
  "Les courbes de TERRAIN, calculées une fois."
  (unless (eq (car *terrain-contours*) terrain)
    (setf *terrain-contours* (cons terrain (contour-segments terrain))))
  (cdr *terrain-contours*))

(defun draw-contours (terrain camera)
  "Les courbes de niveau de TERRAIN, d'épaisseur fixe à l'écran."
  (let* ((segments (terrain-contours terrain))
         (zoom (camera-zoom camera))
         (cx (- (* 0.5f0 *window-width*) (* zoom (camera-x camera))))
         (cy (- (* 0.5f0 *window-height*) (* zoom (camera-y camera))))
         (width (float *window-width* 1f0))
         (height (float *window-height* 1f0)))
    (destructuring-bind (red green blue) *contour-color*
      (rlgl:set-texture 0)
      (rlgl:begin 7)                    ; RL_QUADS
      (loop for k from 0 below (length segments) by 5
            do (let ((x0 (+ cx (* zoom (aref segments k))))
                     (y0 (+ cy (* zoom (aref segments (+ k 1)))))
                     (x1 (+ cx (* zoom (aref segments (+ k 2)))))
                     (y1 (+ cy (* zoom (aref segments (+ k 3)))))
                     (major (plusp (aref segments (+ k 4)))))
                 (unless (or (and (< x0 0) (< x1 0)) (and (> x0 width) (> x1 width))
                             (and (< y0 0) (< y1 0)) (and (> y0 height) (> y1 height)))
                   (let* ((half-line (* 0.5f0 (if major (cdr *contour-width*) (car *contour-width*))))
                          (ex (- x1 x0)) (ey (- y1 y0))
                          (length (sqrt (+ (* ex ex) (* ey ey))))
                          (nx (if (plusp length) (* half-line (/ (- ey) length)) 0f0))
                          (ny (if (plusp length) (* half-line (/ ex length)) 0f0)))
                     (rlgl:color-4ub red green blue
                                     (if major (cdr *contour-alpha*) (car *contour-alpha*)))
                     (rlgl:vertex-2f (- x0 nx) (- y0 ny))
                     (rlgl:vertex-2f (+ x0 nx) (+ y0 ny))
                     (rlgl:vertex-2f (+ x1 nx) (+ y1 ny))
                     (rlgl:vertex-2f (- x1 nx) (- y1 ny))))))
      (rlgl:end))))

(defun draw-altitude (battle camera)
  "En mode topographique, l'altitude du sol sous la souris, à côté d'elle."
  (let ((terrain (battle-terrain battle)))
    (when (and *topographic-mode* terrain)
      (multiple-value-bind (x y) (mouse-world camera)
        (let ((text *altitude-text*)
              (size (text-size 28))
              (mx (raylib:get-mouse-x))
              (my (raylib:get-mouse-y)))
          (write-integer-into (round (terrain-height terrain x y)) text)
          (vector-push #\Space text)
          (vector-push #\m text)
          (draw-text text (+ mx (text-size 24)) (+ my (text-size 24)) size :white))))))
