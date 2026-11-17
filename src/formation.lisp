(in-package #:rogue-company)

;;; Formation d'un régiment : une grille d'emplacements, rangs et files, centrée
;;; sur un point et tournée vers une direction. En bataille, chaque soldat a un
;;; rang et une file (SOLDIERS-RANK, SOLDIERS-FILE, regiment.lisp), et sa place
;;; en découle : la grille compte LAYOUT-FILES files et autant de rangs que le
;;; rang du dernier soldat. Un soldat ne change pas de file au combat : une
;;; perte se comble par la file qui avance (combat.lisp), et une file vide laisse
;;; un trou jusqu'à ce que le régiment se reforme. Sans rangs ni files donnés,
;;; PLACE-FORMATION place le soldat n° I à l'emplacement n° I, rang par rang, le
;;; dernier rang incomplet centré.
;;;
;;; Repère du monde : mètres, x vers la droite, y vers le bas comme l'écran.
;;; FACING est l'angle de la direction vers laquelle le front regarde, depuis +x.

(defparameter *volley-file-spacing* 1.6f0
  "Écart entre files d'un régiment de tireurs en salves : l'intervalle laisse
passer un homme (0,6 m de large) entre deux files (contremarche, shooting.lisp).")

(defparameter *rank-fire-ranks* 3
  "Feu par rang (shooting.lisp) : rangs qui tirent l'un après l'autre, et
profondeur d'un régiment à cet exercice (ADD-REGIMENT).")

(defparameter *volley-ranks* 2
  "Salve (shooting.lisp) : rangs qui tirent ensemble, et profondeur d'un
régiment à cet exercice (ADD-REGIMENT).")

(defclass formation ()
  ((x :initarg :x :accessor formation-x :type single-float :documentation "Centre, en mètres.")
   (y :initarg :y :accessor formation-y :type single-float)
   (facing :initarg :facing :accessor formation-facing :type single-float
           :documentation "Radians ; 0 regarde vers +x, pi/2 vers le bas de l'écran.")
   (ranks :initarg :ranks :accessor formation-ranks :type fixnum
          :documentation "Profondeur voulue ; sans FILES, le nombre de files en découle.")
   (files :initarg :files :initform nil :accessor formation-fixed-files
          :documentation "Front fixé, ou NIL pour le déduire de RANKS.")
   (spacing :initarg :spacing :accessor formation-spacing :type single-float
            :documentation "Écart entre rangs, en mètres, et entre files sans FILE-SPACING.")
   (layout-files :initform nil :accessor formation-layout-files
                 :documentation "Files de la grille des soldats depuis la dernière
reformation (regiment.lisp, LAYOUT-REGIMENT), ou NIL : FORMATION-FILES les
déduit du nombre de soldats.")
   (file-spacing :initarg :file-spacing :initform nil :accessor formation-file-spacing
                 :documentation "Écart entre files, s'il diffère de SPACING : files ouvertes
des tireurs, dont les intervalles servent à la contremarche.")))

(defun formation-lateral-spacing (formation)
  "Écart entre files."
  (or (formation-file-spacing formation) (formation-spacing formation)))

(defun make-formation (&key (x 0) (y 0) (facing 0) (ranks 4) (spacing 1) files file-spacing)
  (make-instance 'formation :x (float x 1f0) :y (float y 1f0) :facing (float facing 1f0)
                            :ranks (max 1 ranks) :spacing (float spacing 1f0)
                            :files (and files (max 1 files))
                            :file-spacing (and file-spacing (float file-spacing 1f0))))

(defun formation-dense-files (formation count)
  "Files pour COUNT soldats en rangs pleins : le front fixé s'il y en a un (moins
large si les soldats manquent), sinon assez de files pour tenir la profondeur
voulue."
  (let ((files (formation-fixed-files formation)))
    (if files
        (max 1 (min files count))
        (max 1 (ceiling count (formation-ranks formation))))))

(defun formation-files (formation count)
  "Files de la formation : celles de la grille des soldats depuis la dernière
reformation, sinon celles de COUNT soldats en rangs pleins."
  (or (formation-layout-files formation)
      (formation-dense-files formation count)))

(declaim (inline grid-point))
(defun grid-point (formation depth rank file files)
  "Emplacement du rang RANK et de la file FILE dans une grille de DEPTH rangs et
FILES files, et le vecteur unitaire vers la droite du front."
  (declare (type fixnum depth rank file files))
  (let* ((spacing (formation-spacing formation))
         (file-spacing (float (formation-lateral-spacing formation) 1f0))
         (facing (formation-facing formation))
         (forward-x (cos facing)) (forward-y (sin facing))
         (right-x (- forward-y)) (right-y forward-x)
         (lateral (* file-spacing (- (float file 1f0) (* 0.5f0 (float (1- files) 1f0)))))
         (forward (- (* 0.5f0 spacing (float (1- depth) 1f0)) (* spacing (float rank 1f0)))))
    (declare (type single-float spacing file-spacing facing forward-x forward-y right-x right-y
                   lateral forward))
    (values (+ (formation-x formation) (* forward-x forward) (* right-x lateral))
            (+ (formation-y formation) (* forward-y forward) (* right-y lateral))
            right-x right-y)))

(defun place-formation (formation count slot-x slot-y start &optional ranks files)
  "Écrit dans SLOT-X et SLOT-Y, à partir de START, les emplacements des COUNT
soldats. Avec RANKS et FILES (tableaux des soldats), chacun à son rang et sa
file, la grille ayant autant de rangs que le rang du dernier. Sinon, le soldat
n° I à l'emplacement n° I : le premier rang devant, le dernier, s'il est
incomplet, centré, sauf en files ouvertes, où chacun reste derrière sa file."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum count start)
           (type (simple-array single-float (*)) slot-x slot-y))
  (when (and ranks (plusp count))
    (let ((ranks ranks) (files files)
          (grid-files (formation-files formation count)))
      (declare (type (simple-array fixnum (*)) ranks files) (type fixnum grid-files))
      (let ((depth (1+ (aref ranks (+ start count -1)))))
        (declare (type fixnum depth))
        (loop for i of-type fixnum from start below (+ start count)
              do (multiple-value-bind (x y)
                     (grid-point formation depth (aref ranks i) (aref files i) grid-files)
                   (setf (aref slot-x i) x (aref slot-y i) y)))))
    (return-from place-formation))
  (when (plusp count)
    (let* ((files (formation-files formation count))
           (ranks (ceiling count files))
           (spacing (formation-spacing formation))
           (file-spacing (float (formation-lateral-spacing formation) 1f0))
           ;; Files ouvertes (contremarche) : un dernier rang incomplet reste
           ;; aligné sur les files, chaque homme derrière sa file.
           (centered (null (formation-file-spacing formation)))
           (facing (formation-facing formation))
           (forward-x (cos facing)) (forward-y (sin facing))
           (right-x (- forward-y)) (right-y forward-x)
           (center-x (formation-x formation)) (center-y (formation-y formation))
           (front (* 0.5f0 spacing (float (1- ranks) 1f0))))
      (declare (type fixnum files ranks)
               (type single-float spacing file-spacing facing forward-x forward-y right-x right-y
                     center-x center-y front))
      (dotimes (i count)
        (multiple-value-bind (rank file) (floor i files)
          (let* ((in-rank (if centered (min files (- count (* rank files))) files))
                 (lateral (* file-spacing (- (float file 1f0) (* 0.5f0 (float (1- in-rank) 1f0)))))
                 (depth (- front (* spacing (float rank 1f0)))))
            (declare (type fixnum in-rank) (type single-float lateral depth))
            (setf (aref slot-x (+ start i)) (+ center-x (* forward-x depth) (* right-x lateral))
                  (aref slot-y (+ start i)) (+ center-y (* forward-y depth) (* right-y lateral)))))))))

(defun formation-snapshot (formation count)
  (ht "x" (json-float (formation-x formation))
      "y" (json-float (formation-y formation))
      "facing_degrees" (json-float (* (formation-facing formation) (/ 180 pi)))
      "files" (formation-files formation count)
      "ranks" (formation-ranks formation)
      "spacing" (json-float (formation-spacing formation))))
