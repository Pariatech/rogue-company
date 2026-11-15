;;; Dessine les quatre états du drapeau d'un régiment, posé au sommet de la
;;; hampe du porteur d'étendard :
;;;
;;;   sbcl --non-interactive --load scripts/draw-flags.lisp
;;;
;;; Écrit assets/overlays/flag/frame_000.png à frame_003.png : intact, troué,
;;; en lambeaux, puis pendant (débandade). Le tissu est cramoisi, comme
;;; l'écharpe des soldats : scripts/build-atlas.lisp en tire le masque teint de
;;; la couleur du camp. La croix dorée reste la même pour tous les camps.
;;;
;;; Le pixel (0, 0) s'accroche au sommet de la hampe ; le drapeau flotte vers
;;; la droite (vers la gauche en miroir). Tirages fixes, sans hasard : relancer
;;; le script redonne les mêmes images.

(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :zpng :silent t)

(defpackage #:draw-flags (:use #:cl))
(in-package #:draw-flags)

(defparameter *root* (merge-pathnames "../" (directory-namestring *load-truename*)))

(defparameter *width* 23)
(defparameter *height* 17)

;;; Couleurs (r g b). Les trois cramoisis passent la règle de masque de
;;; build-atlas ; le contour et l'or non.
(defparameter *outline* '(26 10 14))
(defparameter *shade* '(130 10 35))
(defparameter *base* '(190 20 50))
(defparameter *light* '(230 50 80))
(defparameter *gold* '(232 190 64))
(defparameter *gold-shade* '(170 120 40))

(defparameter *cloth-width* 21 "Largeur du tissu déployé, sans le contour.")
(defparameter *cloth-height* 13)

(defun make-canvas () (make-array (list *height* *width*) :initial-element nil))

(defun wave (x)
  "Décalage vertical de la colonne X du tissu : une ondulation d'un pixel."
  (if (member (mod x 6) '(2 3 4)) 1 0))

(defun shading (x)
  "Clair où le tissu monte vers la lumière, sombre où il redescend."
  (case (mod x 6) ((0 1) *light*) ((2) *base*) ((3 4) *base*) (t *shade*)))

(defun saltire-p (x y)
  "Croix de Bourgogne stylisée : deux diagonales d'un pixel sur le tissu."
  (let ((u (/ (* x (1- *cloth-height*)) (1- *cloth-width*))))
    (or (= y (round u)) (= y (round (- (1- *cloth-height*) u))))))

(defun outline (canvas)
  "Contour d'un pixel autour de tout ce qui est dessiné."
  (let ((cells '()))
    (dotimes (y *height*)
      (dotimes (x *width*)
        (unless (aref canvas y x)
          (when (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
                      for nx = (+ x dx) for ny = (+ y dy)
                      thereis (and (< -1 nx *width*) (< -1 ny *height*)
                                   (aref canvas ny nx)
                                   (not (eq (aref canvas ny nx) *outline*))))
            (push (list x y) cells)))))
    (loop for (x y) in cells do (setf (aref canvas y x) *outline*))
    canvas))

(defun draw-cloth (canvas &key (holes '()) (cut (constantly nil)))
  "Tissu déployé, colonne par colonne, puis son contour. HOLES : pixels (x y)
du tissu retirés ; CUT : prédicat (x y) des pixels arrachés."
  (dotimes (x *cloth-width*)
    (dotimes (y *cloth-height*)
      (unless (or (member (list x y) holes :test #'equal) (funcall cut x y))
        (setf (aref canvas (+ 1 y (wave x)) (+ 1 x))
              (cond ((saltire-p x y) (if (eq (shading x) *shade*) *gold-shade* *gold*))
                    ((= y (1- *cloth-height*)) *shade*)
                    (t (shading x)))))))
  (outline canvas))

(defparameter *holes* '((5 3) (6 3) (5 4) (14 8) (15 8) (10 10) (17 4) (17 5))
  "Trous de balle et de pique du drapeau troué.")

(defun intact () (draw-cloth (make-canvas)))

(defun holed () (draw-cloth (make-canvas) :holes *holes*))

(defun tattered ()
  "Plus de trous, le bord flottant déchiqueté et le bas arraché par endroits."
  (draw-cloth (make-canvas)
              :holes (append *holes* '((3 7) (4 7) (11 3) (12 3) (12 4) (8 6) (9 6)))
              :cut (lambda (x y)
                     (or (> x (- *cloth-width* 1 (nth (mod y 5) '(0 4 2 6 3))))
                         (and (> y 9) (member (mod x 5) '(1 3)))
                         (and (> x 13) (> y 7))))))

(defun fallen ()
  "Débandade : le tissu pend le long de la hampe, en lambeaux."
  (let ((canvas (make-canvas)))
    (dotimes (x 6)
      (dotimes (y (- *height* 2 (nth x '(0 1 3 2 4 5))))
        (setf (aref canvas (+ 1 y) (+ 1 x))
              (cond ((and (<= 2 x 3) (<= 3 y 4)) *gold*)
                    ((oddp x) *shade*)
                    (t *base*)))))
    (outline canvas)))

(defun write-png (canvas path)
  (let* ((data (make-array (* *width* *height* 4) :element-type '(unsigned-byte 8)
                                                  :initial-element 0)))
    (dotimes (y *height*)
      (dotimes (x *width*)
        (let ((color (aref canvas y x)) (i (* 4 (+ x (* y *width*)))))
          (when color
            (setf (aref data i) (first color) (aref data (+ i 1)) (second color)
                  (aref data (+ i 2)) (third color) (aref data (+ i 3)) 255)))))
    (ensure-directories-exist path)
    (zpng:write-png (make-instance 'zpng:png :color-type :truecolor-alpha
                                             :width *width* :height *height* :image-data data)
                    path)))

(loop for canvas in (list (intact) (holed) (tattered) (fallen))
      for i from 0
      do (write-png canvas (merge-pathnames (format nil "assets/overlays/flag/frame_~3,'0d.png" i)
                                            *root*)))
(format t "4 drapeaux dans assets/overlays/flag/~%")
