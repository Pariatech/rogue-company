(in-package #:rogue-company)

;;; Formation d'un régiment : une grille d'emplacements, rangs et files, centrée
;;; sur un point et tournée vers une direction. Le soldat n° I de la plage du
;;; régiment va à l'emplacement n° I, rang par rang : les FILES premiers forment
;;; le premier rang. Un régiment en bataille garde son front (FILES) et perd de
;;; la profondeur : les pertes se comblent colonne par colonne (combat.lisp).
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

(defun formation-files (formation count)
  "Files pour COUNT soldats : le front fixé s'il y en a un (moins large si les
soldats manquent), sinon assez de files pour tenir la profondeur voulue."
  (let ((files (formation-fixed-files formation)))
    (if files
        (max 1 (min files count))
        (max 1 (ceiling count (formation-ranks formation))))))

(defun place-formation (formation count slot-x slot-y start)
  "Écrit dans SLOT-X et SLOT-Y, à partir de START, les emplacements des COUNT
soldats. Le premier rang est devant ; le dernier, s'il est incomplet, est centré,
sauf en files ouvertes, où chacun reste derrière sa file."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum count start)
           (type (simple-array single-float (*)) slot-x slot-y))
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

(defun formation-slot (formation count k)
  "Emplacement n° K (depuis 0) d'une formation de COUNT soldats, comme
PLACE-FORMATION, et le vecteur unitaire vers la droite du front."
  (let* ((files (formation-files formation count))
         (ranks (ceiling count files))
         (spacing (formation-spacing formation))
         (file-spacing (formation-lateral-spacing formation))
         (facing (formation-facing formation))
         (forward-x (cos facing)) (forward-y (sin facing))
         (right-x (- forward-y)) (right-y forward-x)
         (front (* 0.5f0 spacing (float (1- ranks) 1f0))))
    (multiple-value-bind (rank file) (floor k files)
      (let* ((in-rank (if (formation-file-spacing formation)
                          files
                          (min files (- count (* rank files)))))
             (lateral (* file-spacing (- (float file 1f0) (* 0.5f0 (float (1- in-rank) 1f0)))))
             (depth (- front (* spacing (float rank 1f0)))))
        (values (+ (formation-x formation) (* forward-x depth) (* right-x lateral))
                (+ (formation-y formation) (* forward-y depth) (* right-y lateral))
                right-x right-y)))))

(defun formation-snapshot (formation count)
  (ht "x" (json-float (formation-x formation))
      "y" (json-float (formation-y formation))
      "facing_degrees" (json-float (* (formation-facing formation) (/ 180 pi)))
      "files" (formation-files formation count)
      "ranks" (formation-ranks formation)
      "spacing" (json-float (formation-spacing formation))))
