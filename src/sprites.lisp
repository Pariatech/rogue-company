(in-package #:rogue-company)

;;; Atlas des soldats : assets/atlas.png et assets/atlas.sexp, construits par
;;; scripts/build-atlas.lisp à partir de assets/overlays/ (morceaux des soldats,
;;; drapeau, carré blanc). Chaque surimpression occupe deux rangées : ses
;;; images en colonnes, puis leurs masques de couleur de camp, dessinés
;;; par-dessus et teintés. Les morceaux qui ont une direction en ont cinq
;;; (est, sud-est, sud, nord-est, nord) : sud-ouest, ouest et nord-ouest sont
;;; les miroirs de sud-est, est et nord-est.
;;;
;;; La texture vit dans le GPU : elle se charge sur le thread principal, au
;;; premier dessin. RELOAD-ATLAS la recharge après une reconstruction.

(defparameter *atlas-directory* (asdf:system-relative-pathname :rogue-company "assets/"))

(defclass atlas ()
  ((texture :initarg :texture :reader atlas-texture)
   (cell :initarg :cell :reader atlas-cell :documentation "Côté d'une case, en pixels.")
   (width :initarg :width :reader atlas-width)
   (height :initarg :height :reader atlas-height)
   (overlays :initarg :overlays :reader atlas-overlays
             :documentation "Plist surimpression -> (rangée images largeur hauteur).")))

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
                   :overlays (loop for (name . properties) in (getf description :overlays)
                                   append (list name
                                                (list (getf properties :row)
                                                      (getf properties :frames)
                                                      (getf properties :width)
                                                      (getf properties :height)))))))

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

(defun atlas-overlay (atlas name)
  "Rangée, nombre d'images, largeur et hauteur de la surimpression NAME, ou NIL."
  (values-list (getf (atlas-overlays atlas) name)))

(defparameter *direction-strips* #(0 1 2 1 0 3 4 3)
  "Pour chacune des 8 directions (est, sud-est, sud, sud-ouest, ouest,
nord-ouest, nord, nord-est), le numéro de la direction stockée qui la dessine.")

(defparameter *direction-mirrored* #(0 0 0 1 1 1 0 0)
  "1 si la direction se dessine en miroir de sa direction stockée.")
