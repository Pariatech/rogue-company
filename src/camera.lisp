(in-package #:rogue-company)

;;; Caméra de la bataille : un point du monde au centre de l'écran et un zoom,
;;; en pixels d'écran par mètre. Elle ne touche pas à la simulation.
;;;
;;; Les zooms sont des crans : à partir de *SPRITE-PIXELS-PER-METER*, chaque
;;; cran est un multiple entier de l'échelle des sprites, pour que le pixel art
;;; reste net ; en dessous, la vue d'ensemble dessine des rectangles.

(defparameter *sprite-pixels-per-meter* 18f0
  "Pixels de sprite par mètre : un soldat de 32 px fait environ 1,75 m de haut.")

(defparameter *zoom-levels* #(4f0 6f0 9f0 13f0 18f0 36f0 54f0 72f0)
  "Crans de zoom, en pixels d'écran par mètre, croissants.")

(defparameter *default-zoom-level* 4 "Cran de départ : les sprites à l'échelle 1.")

(defparameter *wheel-per-zoom-step* 2f0
  "Mouvement de molette cumulé pour un cran de zoom. Un pavé tactile envoie
beaucoup de petits mouvements : sans cumul, un geste sauterait plusieurs crans.")

(defparameter *zoom-step-delay* 0.2f0
  "Secondes minimum entre deux crans de zoom à la molette.")

(defparameter *camera-pan-speed* 900f0
  "Défilement au clavier, en pixels d'écran par seconde, quel que soit le zoom.")

(defclass camera ()
  ((x :initarg :x :initform 0f0 :accessor camera-x
      :documentation "Point du monde au centre de l'écran, en mètres.")
   (y :initarg :y :initform 0f0 :accessor camera-y)
   (zoom-level :initform *default-zoom-level* :accessor camera-zoom-level)
   (drag-x :initform nil :accessor camera-drag-x
           :documentation "Position de la souris à l'image précédente pendant un
glisser, NIL sinon.")
   (drag-y :initform nil :accessor camera-drag-y)
   (wheel :initform 0f0 :accessor camera-wheel
          :documentation "Molette cumulée, pas encore convertie en cran.")
   (zoom-cooldown :initform 0f0 :accessor camera-zoom-cooldown
                  :documentation "Secondes avant le prochain cran à la molette.")))

(defun make-camera (&key (x 0f0) (y 0f0))
  (make-instance 'camera :x (float x 1f0) :y (float y 1f0)))

(defun camera-zoom (camera)
  "Pixels d'écran par mètre."
  (aref *zoom-levels* (camera-zoom-level camera)))

(defun camera-sprite-scale (camera)
  "Pixels d'écran par pixel de sprite, ou NIL si le zoom est trop petit pour les sprites."
  (let ((scale (/ (camera-zoom camera) *sprite-pixels-per-meter*)))
    (when (>= scale 1f0) scale)))

(defun screen-center-x () (* 0.5f0 (float *window-width* 1f0)))
(defun screen-center-y () (* 0.5f0 (float *window-height* 1f0)))

(defun screen->world (camera sx sy)
  (let ((zoom (camera-zoom camera)))
    (values (+ (camera-x camera) (/ (- sx (screen-center-x)) zoom))
            (+ (camera-y camera) (/ (- sy (screen-center-y)) zoom)))))

(defun zoom-camera (camera steps &optional (sx (screen-center-x)) (sy (screen-center-y)))
  "Change le zoom de STEPS crans en gardant immobile le point du monde sous SX SY."
  (let ((level (max 0 (min (1- (length *zoom-levels*))
                           (+ (camera-zoom-level camera) steps)))))
    (unless (= level (camera-zoom-level camera))
      (multiple-value-bind (wx wy) (screen->world camera sx sy)
        (setf (camera-zoom-level camera) level)
        (let ((zoom (camera-zoom camera)))
          (setf (camera-x camera) (- wx (/ (- sx (screen-center-x)) zoom))
                (camera-y camera) (- wy (/ (- sy (screen-center-y)) zoom))))))))

(defun update-camera (camera dt)
  "Flèches ou ZQSD/WASD pour défiler, molette ou Page préc./suiv. pour zoomer
(vers la souris), bouton du milieu pour glisser. Le bouton droit donne les
ordres (command.lisp)."
  (let ((zoom (camera-zoom camera))
        (step (* *camera-pan-speed* (float dt 1f0))))
    (flet ((held (&rest keys) (some #'key-down-p keys)))
      (when (held :key-left :key-a :key-q) (decf (camera-x camera) (/ step zoom)))
      (when (held :key-right :key-d) (incf (camera-x camera) (/ step zoom)))
      (when (held :key-up :key-w :key-z) (decf (camera-y camera) (/ step zoom)))
      (when (held :key-down :key-s) (incf (camera-y camera) (/ step zoom)))))
  (let ((mx (float (raylib:get-mouse-x) 1f0))
        (my (float (raylib:get-mouse-y) 1f0))
        (wheel (raylib:get-mouse-wheel-move)))
    (cond ((key-pressed-p :key-page-up) (zoom-camera camera 1))
          ((key-pressed-p :key-page-down) (zoom-camera camera -1)))
    ;; Molette : cumulée, un cran par *WHEEL-PER-ZOOM-STEP*, au plus un cran
    ;; par *ZOOM-STEP-DELAY*. Ce qui arrive pendant l'attente est oublié.
    (setf (camera-zoom-cooldown camera)
          (max 0f0 (- (camera-zoom-cooldown camera) (float dt 1f0))))
    (if (plusp (camera-zoom-cooldown camera))
        (setf (camera-wheel camera) 0f0)
        (let ((total (+ (camera-wheel camera) wheel)))
          (cond ((>= (abs total) *wheel-per-zoom-step*)
                 (zoom-camera camera (if (plusp total) 1 -1) mx my)
                 (setf (camera-wheel camera) 0f0
                       (camera-zoom-cooldown camera) *zoom-step-delay*))
                (t (setf (camera-wheel camera) total)))))
    (if (raylib:is-mouse-button-down :mouse-button-middle)
        (let ((zoom (camera-zoom camera)))
          (when (camera-drag-x camera)
            (decf (camera-x camera) (/ (- mx (camera-drag-x camera)) zoom))
            (decf (camera-y camera) (/ (- my (camera-drag-y camera)) zoom)))
          (setf (camera-drag-x camera) mx (camera-drag-y camera) my))
        (setf (camera-drag-x camera) nil (camera-drag-y camera) nil))))

(defun camera-snapshot (camera)
  (ht "x" (json-float (camera-x camera)) "y" (json-float (camera-y camera))
      "zoom" (json-float (camera-zoom camera))
      "sprites" (if (camera-sprite-scale camera) t 'yason:false)))
