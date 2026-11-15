;;; Construit l'atlas des soldats à partir des sprites de assets/sprites/.
;;;
;;;   sbcl --non-interactive --load scripts/build-atlas.lisp
;;;
;;; Chaque dossier assets/sprites/<type>/ suit la disposition des exports
;;; PixelLab, pour les cinq directions de *DIRECTIONS* (l'ouest est dessiné en
;;; miroir de l'est) :
;;;   animations/<nom>/<direction>/frame_000.png…     une animation par dossier
;;;   rotations/<direction>.png                       pose immobile, seulement
;;;                                                   pour un type sans animation
;;;
;;; PixelLab redessine les couleurs d'une animation : la pose immobile du jeu
;;; est donc la première image de la marche, pas la rotation.
;;;
;;; Dans l'atlas, chaque animation d'un type (la rotation s'appelle idle)
;;; occupe deux rangées par direction : les images en colonnes, puis leurs
;;; masques de couleur de camp. Le masque reprend les pixels cramoisis
;;; (écharpe, culottes) en niveaux de gris normalisés : le jeu le dessine
;;; par-dessus, teinté de la couleur du camp.
;;;
;;; Les images n'ont pas toutes la même taille : PixelLab rend une animation
;;; libre (mode v3, comme la marche nord du porteur) sur un canevas plus grand
;;; que le personnage, centré sur lui. La case de l'atlas prend la plus grande taille, et chaque image y est
;;; centrée : le centre du personnage reste au centre de la case.
;;;
;;; Pour chaque image d'un type, l'atlas note aussi son pixel le plus haut,
;;; depuis le centre de la case (:tips) : pour le porteur d'étendard, le sommet
;;; de la hampe, où s'accroche le drapeau.
;;;
;;; Surimpressions : assets/overlays/<nom>/frame_NNN.png (le drapeau, dessiné
;;; par scripts/draw-flags.lisp), sans direction. Une rangée d'images posées
;;; en haut à gauche de leur case, puis la rangée de leurs masques. S'y ajoute
;;; :solid, un carré blanc pour les couleurs unies (les piques).
;;;
;;; Écrit assets/atlas.png et assets/atlas.sexp (lu par src/sprites.lisp).

(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload '(:pngload :zpng) :silent t)

(defpackage #:build-atlas (:use #:cl))
(in-package #:build-atlas)

(defparameter *root* (merge-pathnames "../" (directory-namestring *load-truename*)))

(defparameter *directions* '("east" "south-east" "south" "north-east" "north")
  "Directions stockées, dans l'ordre des rangées. Sud-ouest, ouest et nord-ouest
sont les miroirs de sud-est, est et nord-est.")

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

(defun unit-animations (unit-dir)
  "Liste de (nom . images par direction), chaque élément d'images étant la liste
des images d'une direction."
  (or (loop for anim-dir in (sort (directory (merge-pathnames "animations/*/" unit-dir))
                                  #'string< :key #'namestring)
            collect (cons (last-name anim-dir)
                          (loop for direction in *directions*
                                for frames = (sort (directory
                                                    (merge-pathnames
                                                     (format nil "~a/frame_*.png" direction)
                                                     anim-dir))
                                                   #'string< :key #'namestring)
                                do (assert frames () "~a : pas d'images pour ~a"
                                           anim-dir direction)
                                collect (mapcar #'read-rgba frames))))
      (list (cons "idle"
                  (loop for direction in *directions*
                        collect (list (read-rgba (merge-pathnames
                                                  (format nil "rotations/~a.png" direction)
                                                  unit-dir))))))))

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

(defun top-pixel (image)
  "Pixel opaque le plus haut de IMAGE, (x y), x au milieu de sa rangée."
  (dotimes (y (array-dimension image 0))
    (let ((xs (loop for x below (array-dimension image 1)
                    when (> (aref image y x 3) 0) collect x)))
      (when xs
        (return (list (floor (+ (first xs) (car (last xs))) 2) y))))))

(defun build ()
  (let* ((units (loop for dir in (sort (directory (merge-pathnames "assets/sprites/*/" *root*))
                                       #'string< :key #'namestring)
                      collect (cons (last-name dir) (unit-animations dir))))
         (overlays (overlay-images))
         (unit-images (loop for (nil . animations) in units
                            append (loop for (nil . directions) in animations
                                         append (reduce #'append directions))))
         (overlay-images (loop for (nil . images) in overlays append images))
         (cell (loop for image in (append unit-images overlay-images)
                     maximize (max (array-dimension image 0) (array-dimension image 1))))
         (columns (max (loop for (nil . animations) in units
                             maximize (loop for (nil . directions) in animations
                                            maximize (loop for frames in directions
                                                           maximize (length frames))))
                       (loop for (nil . images) in overlays maximize (length images))))
         (rows (+ (loop for (nil . animations) in units
                        sum (* 2 (length *directions*) (length animations)))
                  (* 2 (length overlays))))
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
             (blit (image column pad-x pad-y brightest)
               ;; L'image dans sa case de la rangée ROW, son masque dans la rangée suivante.
               (dotimes (y (array-dimension image 0))
                 (dotimes (x (array-dimension image 1))
                   (let ((r (aref image y x 0)) (g (aref image y x 1))
                         (b (aref image y x 2)) (a (aref image y x 3))
                         (ax (+ pad-x x (* column cell)))
                         (ay (+ pad-y y (* row cell))))
                     (when (> a 0)
                       (put ax ay r g b a)
                       (when (team-pixel-p r g b)
                         (incf team)
                         (let ((gray (min 255 (round (* 255 r) brightest))))
                           (put ax (+ ay cell) gray gray gray a)))))))))
      (let ((unit-description
              (loop for (unit . animations) in units
                    collect (let ((brightest (brightest-red
                                              (loop for (nil . directions) in animations
                                                    append (reduce #'append directions)))))
                              (assert (every (lambda (image)
                                               (= (array-dimension image 0) (array-dimension image 1)))
                                             (loop for (nil . directions) in animations
                                                   append (reduce #'append directions))))
                              (list (to-keyword unit)
                                    (loop for (name . directions) in animations
                                          collect (list* (to-keyword name) :row row
                                                         :frames (length (first directions))
                                                         :tips
                                                         (list
                                                          (loop for frames in directions
                                                                collect
                                                                (loop for image in frames
                                                                      for column from 0
                                                                      for size = (array-dimension image 0)
                                                                      for pad = (floor (- cell size) 2)
                                                                      do (blit image column pad pad brightest)
                                                                      collect (destructuring-bind (x y) (top-pixel image)
                                                                                (list (- (+ pad x) (/ cell 2))
                                                                                      (- (+ pad y) (/ cell 2)))))
                                                                do (incf row 2)))))))))
            (overlay-description
              (loop for (name . images) in overlays
                    collect (let ((brightest (brightest-red images)))
                              (loop for image in images
                                    for column from 0
                                    do (blit image column 0 0 brightest))
                              (prog1 (list (to-keyword name) :row row :frames (length images)
                                           :width (array-dimension (first images) 1)
                                           :height (array-dimension (first images) 0))
                                (incf row 2))))))
        (loop for (unit animations) in unit-description
              do (format t "~(~a~) : ~{~a ~}~%" unit
                         (loop for (name nil nil nil frames) in animations
                               collect (format nil "~(~a~) (~d)" name frames))))
        (format t "Surimpressions : ~{~(~a~) ~}~%~d pixels de camp~%"
                (mapcar #'first overlay-description) team)
        (let ((png (make-instance 'zpng:png :color-type :truecolor-alpha
                                            :width width :height height :image-data atlas)))
          (zpng:write-png png (merge-pathnames "assets/atlas.png" *root*)))
        (with-open-file (out (merge-pathnames "assets/atlas.sexp" *root*)
                             :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*print-pretty* t) (*print-case* :downcase) (*print-right-margin* 100))
              (format out ";;; Écrit par scripts/build-atlas.lisp : ne pas modifier à la main.~%")
              (print (list :cell cell
                           :directions (mapcar #'to-keyword *directions*)
                           :units unit-description
                           :overlays overlay-description)
                     out)
              (terpri out))))
        (format t "Atlas ~dx~d~%" width height)))))

(build)
