(in-package #:rogue-company)

;;; Atlas des soldats : assets/atlas.png et assets/atlas.sexp, construits par
;;; scripts/build-atlas.lisp à partir de assets/sprites/<type>/ (exports PixelLab).
;;;
;;; Chaque animation d'un type occupe deux rangées par direction stockée (est,
;;; sud-est, sud, nord-est, nord) : ses images en colonnes, puis leurs masques
;;; de couleur de camp, dessinés par-dessus et teintés. Sud-ouest, ouest et
;;; nord-ouest sont les miroirs de sud-est, est et nord-est.
;;;
;;; La texture vit dans le GPU : elle se charge sur le thread principal, au
;;; premier dessin. RELOAD-ATLAS la recharge après une reconstruction.

(defparameter *atlas-directory* (asdf:system-relative-pathname :rogue-company "assets/"))

(defparameter *sprite-foot-x* 0f0
  "Pixel du sprite posé sur la position du soldat, le milieu entre les pieds,
compté depuis le centre de la case : les images y sont centrées.")
(defparameter *sprite-foot-y* 12f0)

(defclass atlas ()
  ((texture :initarg :texture :reader atlas-texture)
   (cell :initarg :cell :reader atlas-cell :documentation "Côté d'une case, en pixels.")
   (width :initarg :width :reader atlas-width)
   (height :initarg :height :reader atlas-height)
   (units :initarg :units :reader atlas-units
          :documentation "Plist type -> plist animation -> (rangée . images).")))

(defvar *atlas* nil "Atlas chargé, ou NIL avant le premier dessin.")

(defvar *atlas-stale* nil
  "Vrai quand l'atlas chargé doit être relu : ce fichier vient d'être rechargé,
et le format de sa description a pu changer.")
(setf *atlas-stale* t)

(defun read-atlas-description ()
  (with-open-file (in (merge-pathnames "atlas.sexp" *atlas-directory*))
    (with-standard-io-syntax
      (let ((*read-eval* nil)) (read in)))))

(defun load-atlas ()
  (let* ((description (read-atlas-description))
         (texture (raylib:load-texture
                   (namestring (merge-pathnames "atlas.png" *atlas-directory*)))))
    (when (zerop (raylib:texture-id texture))
      (error "Impossible de charger ~aatlas.png" *atlas-directory*))
    (make-instance 'atlas
                   :texture texture
                   :cell (getf description :cell)
                   :width (raylib:texture-width texture)
                   :height (raylib:texture-height texture)
                   :units (loop for (unit animations) in (getf description :units)
                                append (list unit
                                             (loop for (name . properties) in animations
                                                   append (list name
                                                                (cons (getf properties :row)
                                                                      (getf properties :frames)))))))))

(defun ensure-atlas ()
  "L'atlas, chargé au premier appel et relu après un rechargement de ce
fichier. Thread principal seulement."
  (when (and *atlas* *atlas-stale*)
    (raylib:unload-texture (atlas-texture *atlas*))
    (setf *atlas* nil))
  (setf *atlas-stale* nil)
  (or *atlas* (setf *atlas* (load-atlas))))

(defun reload-atlas ()
  "Recharge l'atlas après scripts/build-atlas.lisp. Thread principal seulement."
  (when *atlas*
    (raylib:unload-texture (atlas-texture *atlas*))
    (setf *atlas* nil))
  (ensure-atlas))

(defun atlas-animation (atlas unit animation)
  "Première rangée et nombre d'images de l'ANIMATION de UNIT, ou NIL."
  (let ((entry (getf (getf (atlas-units atlas) unit) animation)))
    (when entry (values (car entry) (cdr entry)))))

(defun atlas-walk (atlas unit)
  "Rangée et nombre d'images de la marche de UNIT. Sans marche, la pose
immobile : une seule image."
  (multiple-value-bind (row frames) (atlas-animation atlas unit :walk)
    (if row
        (values row frames)
        (multiple-value-bind (row frames) (atlas-animation atlas unit :idle)
          (unless row (error "Pas de sprite ~s dans l'atlas" unit))
          (values row frames)))))

(defparameter *direction-strips* #(0 1 2 1 0 3 4 3)
  "Pour chacune des 8 directions (est, sud-est, sud, sud-ouest, ouest,
nord-ouest, nord, nord-est), le numéro de la direction stockée qui la dessine.")

(defparameter *direction-mirrored* #(0 0 0 1 1 1 0 0)
  "1 si la direction se dessine en miroir de sa direction stockée.")
