(in-package #:rogue-company)

;;; Banc d'essai de rendu (étape 7) : N sprites pivotés, tirés d'un atlas, en
;;; deux variantes.
;;;
;;; :PRO   DrawTexturePro, un appel par sprite. cl-raylib convertit à chaque appel
;;;        la texture, deux rectangles, l'origine et la couleur en structures C.
;;; :RLGL  Coins calculés en Lisp et envoyés par rlTexCoord2f et rlVertex2f, qui ne
;;;        prennent que des flottants. Même lot de dessin que DrawTexturePro.
;;;
;;; Les outils game_benchmark et game_profile appellent RUN-BENCHMARK et
;;; RUN-PROFILE, depuis un thread du serveur.
;;;
;;; cl-rlgl exporte par erreur TEXTCOORD-2F : la fonction s'appelle
;;; RLGL::TEXCOORD-2F.

(defparameter *atlas-cell* 32 "Côté d'une case de l'atlas, en pixels.")
(defparameter *atlas-cells* 4 "Cases de l'atlas, côte à côte.")
(defparameter *sprite-size* 16.0f0 "Côté d'un sprite à l'écran, en pixels.")
(defparameter *bench-seed* 42)
(defparameter *bench-variants* '("pro" "rlgl"))

(defclass sprite-bench ()
  ((variant :initarg :variant :reader bench-variant)
   (sprite-count :initarg :sprite-count :reader bench-sprite-count)
   (texture :initarg :texture :reader bench-texture)
   (xs :initarg :xs :reader bench-xs)
   (ys :initarg :ys :reader bench-ys)
   (angles :initarg :angles :reader bench-angles
           :documentation "Angle initial, en radians.")
   (spins :initarg :spins :reader bench-spins
          :documentation "Radians par image.")
   (cells :initarg :cells :reader bench-cells)
   (source :initform (raylib:make-rectangle :x 0 :y 0 :width 0 :height 0) :reader bench-source)
   (dest :initform (raylib:make-rectangle :x 0 :y 0 :width 0 :height 0) :reader bench-dest)
   (origin :initform (3d-vectors:vec2 0 0) :reader bench-origin)
   (start-frame :initarg :start-frame :reader bench-start-frame)))

(defvar *bench* nil "Banc d'essai en cours, ou NIL.")

;;; Atlas

(defparameter *atlas-colors*
  '((200 60 60) (60 110 200) (220 190 70) (80 160 90))
  "Couleur du corps de chaque case : un type d'unité par case.")

(defun atlas-pixel (cell x y)
  "Couleur RGBA du pixel (X, Y) de la case CELL : un corps rond et une pique
vers le haut, pour que la rotation se voie."
  (let* ((c *atlas-cell*)
         (dx (- x (/ c 2))) (dy (- y (* c 0.6))))
    (cond ((<= (+ (* dx dx) (* dy dy)) (expt (* c 0.28) 2))
           (append (nth cell *atlas-colors*) '(255)))
          ((and (<= (- (/ c 2) 1) x (/ c 2)) (<= 1 y (* c 0.6)))
           '(210 210 210 255))
          (t '(0 0 0 0)))))

(defun make-atlas-texture ()
  "Génère l'atlas en mémoire et le charge sur le GPU. Thread principal."
  (let* ((c *atlas-cell*)
         (width (* c *atlas-cells*))
         ;; GenImageColor alloue une image RGBA 8 bits, libérée par UnloadImage.
         (image (raylib:gen-image-color width c 0))
         ;; IMAGE-DATA n'est pas exporté par cl-raylib.
         (data (raylib::image-data image)))
    (unwind-protect
         (progn
           (dotimes (y c)
             (dotimes (x width)
               (loop for value in (atlas-pixel (floor x c) (mod x c) y)
                     for offset from 0
                     do (setf (cffi:mem-aref data :uint8 (+ (* 4 (+ x (* y width))) offset))
                              value))))
           (raylib:load-texture-from-image image))
      (raylib:unload-image image))))

;;; Scène

(defun make-sprite-bench (variant count)
  (let ((random (sb-ext:seed-random-state *bench-seed*))
        (xs (make-array count :element-type 'single-float))
        (ys (make-array count :element-type 'single-float))
        (angles (make-array count :element-type 'single-float))
        (spins (make-array count :element-type 'single-float))
        (cells (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count)
      (setf (aref xs i) (random (float *window-width* 1f0) random)
            (aref ys i) (random (float *window-height* 1f0) random)
            (aref angles i) (random (float (* 2 pi) 1f0) random)
            (aref spins i) (- (random 0.1f0 random) 0.05f0)
            (aref cells i) (random *atlas-cells* random)))
    (make-instance 'sprite-bench
                   :variant variant :sprite-count count
                   :texture (make-atlas-texture)
                   :xs xs :ys ys :angles angles :spins spins :cells cells
                   :start-frame *frame*)))

(defun draw-bench-pro (bench)
  (let ((texture (bench-texture bench))
        (source (bench-source bench))
        (dest (bench-dest bench))
        (origin (bench-origin bench))
        (xs (bench-xs bench)) (ys (bench-ys bench))
        (angles (bench-angles bench)) (spins (bench-spins bench))
        (cells (bench-cells bench))
        (cell (float *atlas-cell* 1f0))
        (size *sprite-size*)
        (age (float (- *frame* (bench-start-frame bench)) 1f0)))
    (declare (type (simple-array single-float (*)) xs ys angles spins)
             (type (simple-array (unsigned-byte 8) (*)) cells)
             (type single-float cell size age))
    (setf (raylib:rectangle-width source) cell
          (raylib:rectangle-height source) cell
          (raylib:rectangle-y source) 0f0
          (raylib:rectangle-width dest) size
          (raylib:rectangle-height dest) size
          (3d-vectors:vx origin) (/ size 2)
          (3d-vectors:vy origin) (/ size 2))
    (dotimes (i (length xs))
      (setf (raylib:rectangle-x source) (* cell (aref cells i))
            (raylib:rectangle-x dest) (aref xs i)
            (raylib:rectangle-y dest) (aref ys i))
      (raylib:draw-texture-pro texture source dest origin
                               (* (+ (aref angles i) (* age (aref spins i)))
                                  #.(float (/ 180 pi) 1f0))
                               #xFFFFFFFF))))

(defun draw-bench-rlgl (bench)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((texture (bench-texture bench))
         (xs (bench-xs bench)) (ys (bench-ys bench))
         (angles (bench-angles bench)) (spins (bench-spins bench))
         (cells (bench-cells bench))
         (cell-u (/ 1f0 (the fixnum *atlas-cells*)))
         (half (/ (the single-float *sprite-size*) 2f0))
         (age (float (- (the fixnum *frame*) (the fixnum (bench-start-frame bench))) 1f0)))
    (declare (type (simple-array single-float (*)) xs ys angles spins)
             (type (simple-array (unsigned-byte 8) (*)) cells)
             (type single-float cell-u half age))
    (rlgl:set-texture (raylib:texture-id texture))
    (rlgl:begin 7)                      ; RL_QUADS
    (rlgl:color-4ub 255 255 255 255)
    (dotimes (i (length xs))
      (let* ((angle (+ (aref angles i) (* age (aref spins i))))
             (c (* half (cos angle)))
             (s (* half (sin angle)))
             (x (aref xs i)) (y (aref ys i))
             (u0 (* cell-u (aref cells i)))
             (u1 (+ u0 cell-u)))
        (declare (type single-float angle c s x y u0 u1))
        ;; Coins (-h,-h) (-h,h) (h,h) (h,-h) pivotés, dans l'ordre de DrawTexturePro.
        (rlgl::texcoord-2f u0 0f0) (rlgl:vertex-2f (+ x (- c) s) (+ y (- s) (- c)))
        (rlgl::texcoord-2f u0 1f0) (rlgl:vertex-2f (- x c s) (+ y (- s) c))
        (rlgl::texcoord-2f u1 1f0) (rlgl:vertex-2f (- (+ x c) s) (+ y s c))
        (rlgl::texcoord-2f u1 0f0) (rlgl:vertex-2f (+ x c s) (- (+ y s) c))))
    (rlgl:end)
    (rlgl:set-texture 0)))

(defun draw-bench ()
  (raylib:clear-background :black)
  (let ((bench *bench*))
    (when bench
      (if (string= (bench-variant bench) "rlgl")
          (draw-bench-rlgl bench)
          (draw-bench-pro bench)))))

(defun start-bench (variant count)
  "Remplace le dessin du jeu par le banc d'essai, sans limite d'images par
seconde. Thread principal."
  (stop-bench)
  (setf *bench* (make-sprite-bench variant count)
        *scene* #'draw-bench)
  (raylib:set-target-fps 0))

(defun stop-bench ()
  "Rend le dessin au jeu et libère l'atlas. Thread principal."
  (when *bench*
    (raylib:unload-texture (bench-texture *bench*))
    (setf *bench* nil))
  (when (eq *scene* #'draw-bench)
    (setf *scene* nil))
  (raylib:set-target-fps 60))

;;; Statistiques

(defun percentile (sorted fraction)
  (aref sorted (min (1- (length sorted)) (floor (* fraction (length sorted))))))

(defun timing-stats (first-frame)
  "Moyenne, 99e centile et maximum de chaque section depuis FIRST-FRAME, sur les
dernières images du tampon. Thread principal."
  (let* ((capacity (array-dimension *timings* 1))
         (frames (min capacity (- *timing-frames* first-frame)))
         (columns (loop for k from (- *timing-frames* frames) below *timing-frames*
                        collect (mod k capacity)))
         (result (ht)))
    (when (< frames 2)
      (error "Trop peu d'images mesurées (~d) : la fenêtre est-elle cachée ?" frames))
    (loop for name in (append *timing-sections* '(:frame))
          for row from 0
          do (let ((values (sort (map 'vector (lambda (column) (aref *timings* row column))
                                      columns)
                                 #'<)))
               (setf (gethash (string-downcase (substitute #\_ #\- (string name))) result)
                     (ht "mean_ms" (round-to (/ (reduce #'+ values) frames))
                         "p99_ms" (round-to (percentile values 0.99))
                         "max_ms" (round-to (aref values (1- frames)))))))
    (let ((consed (sort (map 'vector (lambda (column) (aref *consed-per-frame* column)) columns)
                        #'<)))
      (values result frames
              (ht "mean" (round (reduce #'+ consed) frames)
                  "max" (aref consed (1- frames)))))))

(defun round-to (value &optional (digits 3))
  (let ((scale (expt 10 digits)))
    (/ (round (* value scale)) (float scale 1d0))))

;;; Interface pour les outils MCP (threads du serveur)

(defun check-bench-args (variant count seconds)
  (unless (member variant *bench-variants* :test #'equal)
    (error "variant doit valoir ~{~a~^ ou ~}." *bench-variants*))
  (unless (and (integerp count) (<= 1 count 200000))
    (error "count doit être un entier entre 1 et 200000."))
  (unless (and (realp seconds) (<= 0.5 seconds 30))
    (error "seconds doit être compris entre 0,5 et 30.")))

(defun call-with-bench (variant count function)
  "Lance le banc d'essai, appelle FUNCTION sur ce thread après une seconde de
chauffe, puis rend la main au jeu."
  (call-in-main-thread (lambda () (start-bench variant count)))
  (unwind-protect
       (progn (sleep 1) (funcall function))
    (call-in-main-thread #'stop-bench)))

(defun run-benchmark (&key (variant "pro") (count 10000) (seconds 5))
  (check-bench-args variant count seconds)
  (call-with-bench
   variant count
   (lambda ()
     (let ((first (call-in-main-thread (lambda () *timing-frames*))))
       (sleep seconds)
       (call-in-main-thread
        (lambda ()
          (multiple-value-bind (sections frames consed) (timing-stats first)
            (to-json
             (ht "scene" "sprites" "variant" variant "count" count
                 "frames" frames
                 "fps" (round-to (/ (- *timing-frames* first) seconds) 1)
                 "sections" sections
                 "bytes_consed_per_frame" consed
                 "console_open" (if (console-open-p *console*) t 'yason:false)
                 "window_focused" (if (raylib:is-window-focused) t 'yason:false)
                 "note" (if (console-open-p *console*)
                            "La console ouverte alloue à chaque image : la fermer pour mesurer les allocations."
                            :null))))))))))

(defun run-profile (&key (seconds 5) variant (count 10000) (max 30))
  "Profile le jeu pendant SECONDS secondes avec sb-sprof (mode CPU, thread
principal). Avec VARIANT, profile le banc d'essai de rendu."
  (when variant (check-bench-args variant count seconds))
  (unless (and (realp seconds) (<= 0.5 seconds 30))
    (error "seconds doit être compris entre 0,5 et 30."))
  (flet ((profile ()
           (sb-sprof:reset)
           (sb-sprof:start-profiling :mode :cpu :sample-interval 0.001
                                     :threads (list (sb-thread:main-thread)))
           (unwind-protect (sleep seconds)
             (sb-sprof:stop-profiling))
           (prog1 (with-output-to-string (out)
                    (sb-sprof:report :type :flat :max max :stream out))
             (sb-sprof:reset))))
    (if variant
        (call-with-bench variant count #'profile)
        (profile))))
