(in-package #:rogue-company)

;;; Formation d'un régiment : une grille d'emplacements, rangs et files, centrée
;;; sur un point et tournée vers une direction. Le soldat n° I de la plage du
;;; régiment va à l'emplacement n° I : la suppression par échange garde les
;;; vivants compacts, donc l'emplacement suit l'indice.
;;;
;;; Repère du monde : mètres, x vers la droite, y vers le bas comme l'écran.
;;; FACING est l'angle de la direction vers laquelle le front regarde, depuis +x.

(defclass formation ()
  ((x :initarg :x :accessor formation-x :type single-float :documentation "Centre, en mètres.")
   (y :initarg :y :accessor formation-y :type single-float)
   (facing :initarg :facing :accessor formation-facing :type single-float
           :documentation "Radians ; 0 regarde vers +x, pi/2 vers le bas de l'écran.")
   (ranks :initarg :ranks :accessor formation-ranks :type fixnum
          :documentation "Profondeur voulue ; le nombre de files en découle.")
   (spacing :initarg :spacing :accessor formation-spacing :type single-float
            :documentation "Écart entre files et entre rangs, en mètres.")))

(defun make-formation (&key (x 0) (y 0) (facing 0) (ranks 4) (spacing 1))
  (make-instance 'formation :x (float x 1f0) :y (float y 1f0) :facing (float facing 1f0)
                            :ranks (max 1 ranks) :spacing (float spacing 1f0)))

(defun formation-files (formation count)
  "Files pour COUNT soldats : le front s'élargit pour tenir la profondeur voulue."
  (max 1 (ceiling count (formation-ranks formation))))

(defun place-formation (formation count slot-x slot-y start)
  "Écrit dans SLOT-X et SLOT-Y, à partir de START, les emplacements des COUNT
soldats. Le premier rang est devant ; le dernier, s'il est incomplet, est centré."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum count start)
           (type (simple-array single-float (*)) slot-x slot-y))
  (when (plusp count)
    (let* ((files (formation-files formation count))
           (ranks (ceiling count files))
           (spacing (formation-spacing formation))
           (facing (formation-facing formation))
           (forward-x (cos facing)) (forward-y (sin facing))
           (right-x (- forward-y)) (right-y forward-x)
           (center-x (formation-x formation)) (center-y (formation-y formation))
           (front (* 0.5f0 spacing (float (1- ranks) 1f0))))
      (declare (type fixnum files ranks)
               (type single-float spacing facing forward-x forward-y right-x right-y
                     center-x center-y front))
      (dotimes (i count)
        (multiple-value-bind (rank file) (floor i files)
          (let* ((in-rank (min files (- count (* rank files))))
                 (lateral (* spacing (- (float file 1f0) (* 0.5f0 (float (1- in-rank) 1f0)))))
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
