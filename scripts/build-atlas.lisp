;;; Construit l'atlas des soldats en morceaux et des drapeaux.
;;;
;;;   sbcl --non-interactive --load scripts/build-atlas.lisp
;;;
;;; Surimpressions : assets/overlays/<nom>/frame_NNN.png (le drapeau, dessiné
;;; par scripts/draw-flags.lisp ; les morceaux des soldats, part-*, par
;;; scripts/draw-parts.lisp), sans direction propre. Une rangée d'images posées
;;; en haut à gauche de leur case, puis la rangée de leurs masques : les pixels
;;; cramoisis en niveaux de gris normalisés, que le jeu dessine par-dessus,
;;; teintés de la couleur du camp. La case prend la plus grande image. S'y
;;; ajoute :solid, un carré blanc pour les couleurs unies (piques, hampes).
;;;
;;; Écrit assets/atlas.png et assets/atlas.sexp (lu par src/sprites.lisp).

(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '(:pngload :zpng) :silent t)

(defpackage #:build-atlas (:use #:cl))
(in-package #:build-atlas)

(defparameter *root* (merge-pathnames "../" (directory-namestring *load-truename*)))

(defun team-pixel-p (r g b)
  "Rouge cramoisi : beaucoup de rouge, et au moins autant de bleu que de vert.
Les bruns (bottes, cuir, hampe de la pique) ont plus de vert que de bleu."
  (and (> r 60) (> r (* 2.5 g)) (>= b g) (> r (* 1.5 b))))

(defun read-rgba (path)
  "Tableau (hauteur largeur 4) d'octets."
  (let ((png (pngload:load-file path :flatten nil)))
    (assert (= 4 (array-dimension (pngload:data png) 2)) () "~a n'est pas en RGBA" path)
    (pngload:data png)))

(defun last-name (directory) (car (last (pathname-directory directory))))

(defun to-keyword (name) (intern (string-upcase name) :keyword))

(defun solid-image ()
  "Carré blanc opaque de 4 px : le jeu y prend sa couleur unie (piques), teintée
par la couleur des sommets, sans quitter la texture de l'atlas."
  (make-array '(4 4 4) :element-type '(unsigned-byte 8) :initial-element 255))

(defun overlay-images ()
  "Liste de (nom . images) des surimpressions de assets/overlays/, plus le carré
blanc :solid."
  (append
   (loop for dir in (sort (directory (merge-pathnames "assets/overlays/*/" *root*))
                          #'string< :key #'namestring)
         collect (cons (last-name dir)
                       (mapcar #'read-rgba
                               (sort (directory (merge-pathnames "frame_*.png" dir))
                                     #'string< :key #'namestring))))
   (list (cons "solid" (list (solid-image))))))

(defun brightest-red (images)
  "Rouge le plus clair des pixels de camp de IMAGES : il devient le blanc du masque."
  (let ((brightest 1))
    (dolist (image images brightest)
      (dotimes (y (array-dimension image 0))
        (dotimes (x (array-dimension image 1))
          (when (and (> (aref image y x 3) 0)
                     (team-pixel-p (aref image y x 0) (aref image y x 1) (aref image y x 2)))
            (setf brightest (max brightest (aref image y x 0)))))))))

(defun build ()
  (let* ((overlays (overlay-images))
         (images (loop for (nil . images) in overlays append images))
         (cell (loop for image in images
                     maximize (max (array-dimension image 0) (array-dimension image 1))))
         (columns (loop for (nil . images) in overlays maximize (length images)))
         (rows (* 2 (length overlays)))
         (width (* cell columns))
         (height (* cell rows))
         (atlas (make-array (* width height 4) :element-type '(unsigned-byte 8)
                                               :initial-element 0))
         (row 0)
         (team 0))
    (labels ((put (x y r g b a)
               (let ((i (* 4 (+ x (* y width)))))
                 (setf (aref atlas i) r (aref atlas (+ i 1)) g
                       (aref atlas (+ i 2)) b (aref atlas (+ i 3)) a)))
             (blit (image column brightest)
               ;; L'image dans sa case de la rangée ROW, son masque dans la rangée suivante.
               (dotimes (y (array-dimension image 0))
                 (dotimes (x (array-dimension image 1))
                   (let ((r (aref image y x 0)) (g (aref image y x 1))
                         (b (aref image y x 2)) (a (aref image y x 3))
                         (ax (+ x (* column cell)))
                         (ay (+ y (* row cell))))
                     (when (> a 0)
                       (put ax ay r g b a)
                       (when (team-pixel-p r g b)
                         (incf team)
                         (let ((gray (min 255 (round (* 255 r) brightest))))
                           (put ax (+ ay cell) gray gray gray a)))))))))
      (let ((description
              (loop for (name . images) in overlays
                    collect (let ((brightest (brightest-red images)))
                              (loop for image in images
                                    for column from 0
                                    do (blit image column brightest))
                              (prog1 (list (to-keyword name) :row row :frames (length images)
                                           :width (array-dimension (first images) 1)
                                           :height (array-dimension (first images) 0))
                                (incf row 2))))))
        (format t "Surimpressions : ~{~(~a~) ~}~%~d pixels de camp~%"
                (mapcar #'first description) team)
        (let ((png (make-instance 'zpng:png :color-type :truecolor-alpha
                                            :width width :height height :image-data atlas)))
          (zpng:write-png png (merge-pathnames "assets/atlas.png" *root*)))
        (with-open-file (out (merge-pathnames "assets/atlas.sexp" *root*)
                             :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*print-pretty* t) (*print-case* :downcase) (*print-right-margin* 100))
              (format out ";;; Écrit par scripts/build-atlas.lisp : ne pas modifier à la main.~%")
              (print (list :cell cell :overlays description) out)
              (terpri out))))
        (format t "Atlas ~dx~d~%" width height)))))

(build)
