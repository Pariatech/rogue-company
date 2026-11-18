;;; Dessine les morceaux d'un soldat (corps, tête, main, pied), que le jeu
;;; assemble et anime lui-même (src/soldier-parts.lisp) :
;;;
;;;   sbcl --non-interactive --load scripts/draw-parts.lisp
;;;
;;; Écrit assets/overlays/part-<morceau>/frame_NNN.png. Le corps et la tête ont
;;; une image par direction stockée, dans l'ordre de l'atlas : est, sud-est,
;;; sud, nord-est, nord (l'ouest se dessine en miroir). La main et le pied n'en
;;; ont qu'une ; la rondache, trois (de face, de profil, de dos). L'écharpe et les culottes sont cramoisies : scripts/build-atlas.lisp
;;; en tire le masque teint de la couleur du camp.
;;;
;;; Vue de trois quarts, comme les sprites PixelLab : 18 pixels par mètre.
;;; Tirages fixes : relancer le script redonne les mêmes images.

(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :zpng :silent t)

(defpackage #:draw-parts (:use #:cl))
(in-package #:draw-parts)

(defparameter *root* (merge-pathnames "../" (directory-namestring *load-truename*)))

;;; Couleurs (r g b). Seuls les trois cramoisis passent la règle de masque de
;;; build-atlas.
(defparameter *outline* '(24 18 22))
(defparameter *steel-dark* '(84 96 116))
(defparameter *steel* '(150 164 182))
(defparameter *steel-light* '(214 222 232))
(defparameter *buff* '(168 120 70))
(defparameter *buff-shade* '(120 82 46))
(defparameter *leather* '(90 58 34))
(defparameter *crimson-light* '(230 50 80))
(defparameter *crimson* '(190 20 50))
(defparameter *crimson-shade* '(130 10 35))
(defparameter *skin-light* '(250 212 178))
(defparameter *skin* '(238 188 148))
(defparameter *skin-shade* '(196 138 104))
(defparameter *hair* '(92 60 38))
(defparameter *eye* '(40 26 30))
(defparameter *boot* '(74 48 32))
(defparameter *boot-light* '(112 76 50))

(defparameter *views* '(:east :south-east :south :north-east :north)
  "Directions stockées, dans l'ordre des images.")

;;; Toile : tableau (hauteur largeur) de couleurs, NIL pour transparent.

(defun make-canvas (width height) (make-array (list height width) :initial-element nil))

(defun put (canvas x y color)
  (when (array-in-bounds-p canvas y x)
    (setf (aref canvas y x) color)))

(defun fill-rect (canvas x0 y0 x1 y1 color)
  (loop for y from y0 to y1 do (loop for x from x0 to x1 do (put canvas x y color))))

(defun outline (canvas)
  "Contour d'un pixel autour de tout ce qui est dessiné (quatre voisins : les
coins restent vides, ce qui arrondit les formes)."
  (let ((cells '()))
    (destructuring-bind (height width) (array-dimensions canvas)
      (dotimes (y height)
        (dotimes (x width)
          (unless (aref canvas y x)
            (when (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
                        for nx = (+ x dx) for ny = (+ y dy)
                        thereis (and (array-in-bounds-p canvas ny nx)
                                     (aref canvas ny nx)
                                     (not (eq (aref canvas ny nx) *outline*))))
              (push (list x y) cells))))))
    (loop for (x y) in cells do (setf (aref canvas y x) *outline*))
    canvas))

;;; Corps : buste en buffle, cuirasse (dos ou plastron), écharpe en
;;; bandoulière, ceinture, culottes. 14 × 13 px.

(defun body (view)
  (let* ((canvas (make-canvas 14 13))
         (x0 (if (eq view :east) 3 2))
         (x1 (ecase view ((:south :north) 11) ((:south-east :north-east) 10) (:east 9)))
         (middle (floor (+ x0 x1) 2)))
    ;; Buste, épaules arrondies, ombré à droite.
    (fill-rect canvas x0 1 x1 8 *buff*)
    (fill-rect canvas x1 2 x1 8 *buff-shade*)
    (put canvas x0 1 nil)
    (put canvas x1 1 nil)
    ;; Cuirasse : plastron de face, dos de dos ; de profil, le plastron bombe
    ;; vers l'avant (à droite).
    (multiple-value-bind (left right)
        (ecase view
          (:south (values (+ x0 2) (- x1 2)))
          (:south-east (values (+ x0 3) (- x1 1)))
          (:east (values (+ x0 3) x1))
          (:north-east (values (+ x0 1) (- x1 3)))
          (:north (values (+ x0 2) (- x1 2))))
      (fill-rect canvas left 2 right 7 *steel*)
      (fill-rect canvas left 2 left 7 *steel-light*)
      (fill-rect canvas right 3 right 7 *steel-dark*))
    ;; Écharpe : de l'épaule gauche du soldat à sa hanche droite. De face, son
    ;; épaule gauche est à droite de l'image ; de dos, à gauche.
    (multiple-value-bind (start end)
        (ecase view
          ((:south :south-east) (values (- x1 2) (+ x0 1)))
          ((:north :north-east :east) (values (+ x0 1) (- x1 2))))
      (loop for y from 1 to 7
            for x = (+ start (round (* (- y 1) (- end start)) 6))
            do (put canvas x y *crimson*)
               (put canvas (1+ x) y *crimson-shade*)))
    (fill-rect canvas x0 8 x1 8 *leather*)
    ;; Culottes : deux jambes de face et de dos, un bloc de profil.
    (fill-rect canvas (+ x0 1) 9 (- x1 1) 11 *crimson*)
    (fill-rect canvas (+ x0 1) 9 (+ x0 1) 11 *crimson-light*)
    (fill-rect canvas (- x1 1) 9 (- x1 1) 11 *crimson-shade*)
    (when (member view '(:south :north))
      (fill-rect canvas middle 10 (1+ middle) 11 *crimson-shade*))
    (outline canvas)))

;;; Tête : morion (bombe, crête, bord), visage ou nuque. 14 × 13 px.

(defun head (view)
  (let ((canvas (make-canvas 14 13)))
    (flet ((p (x y color) (put canvas x (1+ y) color))
           (r (x0 y0 x1 y1 color) (fill-rect canvas x0 (1+ y0) x1 (1+ y1) color)))
      ;; Bombe du casque, claire à gauche, sombre à droite.
      (r 5 2 8 2 *steel*)
      (r 4 3 9 3 *steel*)
      (r 3 4 10 5 *steel*)
      (r 4 2 4 5 *steel-light*) (r 3 4 3 5 *steel-light*) (p 5 2 *steel-light*)
      (r 10 4 10 5 *steel-dark*) (p 9 3 *steel-dark*)
      ;; Crête : mince de face et de dos, en arc de profil.
      (if (member view '(:south :north))
          (progn (r 6 0 7 1 *steel-light*) (p 7 1 *steel*))
          (progn (r 5 0 8 0 *steel-light*) (r 4 1 9 1 *steel*)))
      ;; Bord, relevé aux deux bouts de profil.
      (r 1 6 12 6 *steel-dark*)
      (r 2 6 11 6 *steel*)
      (unless (member view '(:south :north))
        (p 1 5 *steel*) (p 12 5 *steel*))
      ;; Visage ou nuque.
      (ecase view
        (:south
         (r 4 7 9 9 *skin*) (r 5 10 8 10 *skin*)
         (r 9 7 9 9 *skin-shade*) (p 4 7 *skin-light*)
         (p 5 8 *eye*) (p 8 8 *eye*)
         (r 5 9 8 9 *hair*))
        (:south-east
         (r 4 7 5 8 *hair*)
         (r 6 7 10 9 *skin*) (r 7 10 9 10 *skin*)
         (r 10 7 10 9 *skin-shade*)
         (p 7 8 *eye*) (p 9 8 *eye*)
         (r 7 9 10 9 *hair*))
        (:east
         (r 4 7 6 9 *hair*)
         (r 7 7 10 9 *skin*) (r 8 10 10 10 *skin*)
         (p 9 8 *eye*) (p 11 8 *skin*)
         (r 9 9 10 9 *hair*))
        (:north-east
         (r 4 7 9 9 *hair*) (r 5 10 8 10 *skin-shade*)
         (r 10 7 10 8 *skin*))
        (:north
         (r 4 7 9 9 *hair*) (r 5 10 8 10 *skin-shade*))))
    (outline canvas)))

;;; Rondache : bouclier rond d'acier de 10 px (environ 0,55 m), vu de face (le
;;; soldat regarde vers l'écran), de profil, ou de dos (doublure cramoisie,
;;; teinte du camp, et poignée). 12 × 12 px.

(defun disc-p (x y rx ry)
  "Vrai si le pixel X, Y est dans l'ellipse de demi-axes RX, RY centrée sur la toile de 12 px."
  (<= (+ (expt (/ (- x 5.5) rx) 2) (expt (/ (- y 5.5) ry) 2)) 1))

(defun shield (view)
  (let ((canvas (make-canvas 12 12)))
    (dotimes (y 12)
      (dotimes (x 12)
        (ecase view
          (:face
           (when (disc-p x y 4.7 4.7)
             (put canvas x y
                  (cond ((not (disc-p x y 3.6 3.6)) *steel-dark*)
                        ((and (<= 5 x 6) (<= 5 y 6)) (if (= x y 5) *steel-light* *steel*))
                        ((< (+ x y) 9) *steel-light*)
                        (t *steel*)))))
          (:side
           (when (disc-p x y 1.7 4.7)
             (put canvas x y (cond ((< x 5) *steel-light*)
                                   ((< x 7) *steel*)
                                   (t *steel-dark*)))))
          (:back
           (when (disc-p x y 4.7 4.7)
             (put canvas x y
                  (cond ((not (disc-p x y 3.6 3.6)) *steel-dark*)
                        ((<= 5 x 6) *leather*)
                        ((< (+ x y) 9) *crimson*)
                        (t *crimson-shade*))))))))
    (outline canvas)))

;;; Main : un poing de 3 × 3 px. Pied : une botte de 4 × 2 px.

(defun hand ()
  (let ((canvas (make-canvas 5 5)))
    (fill-rect canvas 1 1 3 3 *skin*)
    (put canvas 1 1 *skin-light*)
    (put canvas 3 3 *skin-shade*)
    (outline canvas)))

(defun foot ()
  (let ((canvas (make-canvas 6 4)))
    (fill-rect canvas 1 1 4 2 *boot*)
    (fill-rect canvas 1 1 2 1 *boot-light*)
    (outline canvas)))

(defun write-png (canvas path)
  (destructuring-bind (height width) (array-dimensions canvas)
    (let ((data (make-array (* width height 4) :element-type '(unsigned-byte 8)
                                               :initial-element 0)))
      (dotimes (y height)
        (dotimes (x width)
          (let ((color (aref canvas y x)) (i (* 4 (+ x (* y width)))))
            (when color
              (setf (aref data i) (first color) (aref data (+ i 1)) (second color)
                    (aref data (+ i 2)) (third color) (aref data (+ i 3)) 255)))))
      (ensure-directories-exist path)
      (zpng:write-png (make-instance 'zpng:png :color-type :truecolor-alpha
                                               :width width :height height :image-data data)
                      path))))

(defun write-part (name canvases)
  (loop for canvas in canvases
        for i from 0
        do (write-png canvas (merge-pathnames
                              (format nil "assets/overlays/part-~a/frame_~3,'0d.png" name i)
                              *root*))))

;;; Cheval : tronc, poitrail, croupe, encolure, tête, oreilles, crinière, queue,
;;; tapis de selle (cramoisi, teint du camp) et selle, en volumes (ellipsoïdes)
;;; dans le repère du cheval : vers l'avant, vers sa droite, en hauteur, en
;;; mètres. Chaque vue les projette comme le jeu projette les morceaux (sol à
;;; 18 px/m, profondeur écrasée de moitié, hauteur vers le haut) et garde le
;;; point le plus proche de l'écran. Les jambes, animées, sont tracées par le
;;; jeu (src/soldier-parts.lisp). 48 × 48 px ; le centre de la toile est le
;;; point à *HORSE-ANCHOR* mètres au-dessus du sol, sous le cheval.

(defparameter *horse-anchor* 1.25 "Hauteur du point dessiné au centre de la toile, en mètres.")
(defparameter *horse-coat* '((196 132 80) (158 98 56) (112 66 38)) "Robe baie : clair, moyen, sombre.")
(defparameter *horse-dark* '((70 50 42) (48 34 30) (32 24 22)) "Crins et extrémités.")
(defparameter *horse-cloth* (list *crimson-light* *crimson* *crimson-shade*))
(defparameter *horse-saddle* '((128 86 52) (96 62 36) (70 44 26)))

(defparameter *view-angles* '((:east . 0) (:south-east . 45) (:south . 90)
                              (:north-east . -45) (:north . -90))
  "Orientation du cheval dans chaque vue, en degrés (y vers le bas : le sud à 90).")

(defun horse-volumes ()
  "Liste de (centre-avant centre-droite centre-hauteur rayon-avant rayon-droite
rayon-hauteur matière), matière : :coat, :dark, :cloth ou :saddle. Les derniers
passent devant les premiers à égalité."
  (append
   '((0 0 1.15 0.62 0.27 0.31 :coat)       ; tronc
     (0.5 0 1.2 0.32 0.25 0.31 :coat)      ; poitrail
     (-0.5 0 1.22 0.36 0.27 0.3 :coat))    ; croupe
   ;; Encolure, de l'épaule à la nuque, puis tête, de la nuque au bout du nez.
   (loop for k from 0 to 6
         for u = (/ k 6.0)
         collect (list (+ 0.62 (* u 0.3)) 0 (+ 1.38 (* u 0.4))
                       (- 0.18 (* u 0.06)) (- 0.14 (* u 0.05)) (- 0.2 (* u 0.07)) :coat))
   (loop for k from 0 to 5
         for u = (/ k 5.0)
         collect (list (+ 0.95 (* u 0.26)) 0 (- 1.8 (* u 0.36))
                       0.1 (- 0.1 (* u 0.03)) 0.1 (if (> u 0.8) :dark :coat)))
   '((0.93 0.06 1.95 0.03 0.03 0.06 :coat) ; oreilles
     (0.93 -0.06 1.95 0.03 0.03 0.06 :coat))
   ;; Crinière, sur le dessus de l'encolure.
   (loop for k from 0 to 5
         for u = (/ k 5.0)
         collect (list (+ 0.6 (* u 0.32)) 0 (+ 1.5 (* u 0.38)) 0.07 0.04 0.07 :dark))
   ;; Queue, de la croupe vers le bas.
   (loop for k from 0 to 6
         for u = (/ k 6.0)
         collect (list (- -0.85 (* u 0.18)) 0 (- 1.32 (* u 0.55)) 0.07 0.06 0.08 :dark))
   ;; Tapis de selle sur le dos, et selle.
   '((0 0 1.3 0.36 0.29 0.2 :cloth)
     (0.02 0 1.43 0.24 0.18 0.06 :saddle))))

(defun horse (view)
  (let* ((canvas (make-canvas 48 48))
         (depths (make-array '(48 48) :initial-element nil))
         (angle (* (cdr (assoc view *view-angles*)) (/ pi 180)))
         (c (cos angle)) (s (sin angle))
         (ppm 18) (squash 0.5))
    (flet ((plot (f r h nf nr nh material)
             ;; Point du volume et sa normale, dans le repère du cheval.
             (let* ((gx (- (* f c) (* r s)))
                    (gy (+ (* f s) (* r c)))
                    (px (round (+ 24 (* ppm gx))))
                    (py (round (+ 24 (* ppm (- (* squash gy) (- h *horse-anchor*))))))
                    ;; Vers l'écran : plus bas sur le sol, ou plus haut.
                    (depth (+ gy h))
                    (ngy (+ (* nf s) (* nr c)))
                    (ngx (- (* nf c) (* nr s)))
                    ;; Lumière d'en haut, à gauche, vers l'écran.
                    (light (+ (* 0.7 nh) (* -0.4 ngx) (* 0.3 ngy)))
                    (tones (ecase material
                             (:coat *horse-coat*) (:dark *horse-dark*)
                             (:cloth *horse-cloth*) (:saddle *horse-saddle*)))
                    (color (cond ((> light 0.45) (first tones))
                                 ((> light -0.15) (second tones))
                                 (t (third tones)))))
               (when (and (array-in-bounds-p canvas py px)
                          (or (null (aref depths py px)) (> depth (aref depths py px))))
                 (setf (aref depths py px) depth
                       (aref canvas py px) color)))))
      (loop for (cf cr ch rf rr rh material) in (horse-volumes)
            do (loop for i from 0 below 24
                     for theta = (* pi (/ (+ i 0.5) 24))
                     do (loop for j from 0 below 48
                              for phi = (* 2 pi (/ j 48))
                              for nf = (* (sin theta) (cos phi))
                              for nr = (* (sin theta) (sin phi))
                              for nh = (cos theta)
                              do (plot (+ cf (* rf nf)) (+ cr (* rr nr)) (+ ch (* rh nh))
                                       nf nr nh material)))))
    (outline canvas)))

(write-part "horse" (mapcar #'horse *views*))
(write-part "body" (mapcar #'body *views*))
(write-part "head" (mapcar #'head *views*))
(write-part "hand" (list (hand)))
(write-part "foot" (list (foot)))
(write-part "shield" (mapcar #'shield '(:face :side :back)))
(format t "Morceaux dans assets/overlays/part-*/~%")
