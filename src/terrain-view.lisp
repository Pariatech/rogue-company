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

(defun build-terrain-texture (terrain)
  "Charge la texture ombrée de TERRAIN et renvoie son identifiant rlgl. Thread
principal seulement."
  (let* ((size (terrain-size terrain))
         (texels (max 1 (round (* size *terrain-texels-per-meter*))))
         (step (/ size texels))
         (half (* 0.5f0 size)))
    (cffi:with-foreign-object (data :uint8 (* texels texels 4))
      (dotimes (j texels)
        (dotimes (i texels)
          (let ((light (ground-brightness terrain
                                          (+ (- half) (* step (+ i 0.5f0)))
                                          (+ (- half) (* step (+ j 0.5f0)))))
                (k (* 4 (+ i (* j texels)))))
            (loop for channel in *ground-color*
                  for offset from 0
                  do (setf (cffi:mem-aref data :uint8 (+ k offset))
                           (min 255 (round (* channel light)))))
            (setf (cffi:mem-aref data :uint8 (+ k 3)) 255))))
      ;; RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7, une seule image.
      (let ((id (rlgl:load-texture data texels texels 7 1)))
        ;; Filtre linéaire (GL_LINEAR) en réduction et en agrandissement : le
        ;; relief reste doux de près.
        (rlgl:texture-parameters id #x2801 #x2601)
        (rlgl:texture-parameters id #x2800 #x2601)
        id))))

(defun terrain-texture (terrain)
  "Identifiant de la texture de TERRAIN, refaite quand le terrain change."
  (unless (eq (car *terrain-texture*) terrain)
    (when *terrain-texture* (rlgl:unload-texture (cdr *terrain-texture*)))
    (setf *terrain-texture* (cons terrain (build-terrain-texture terrain))))
  (cdr *terrain-texture*))

(defun draw-terrain (battle camera)
  "Le sol de BATTLE, ombré par son relief ; rien sur un terrain plat (fond noir)."
  (let ((terrain (battle-terrain battle)))
    (when terrain
      (let* ((id (terrain-texture terrain))
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
        (rlgl:set-texture 0)))))
