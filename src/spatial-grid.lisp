(in-package #:rogue-company)

;;; Grille spatiale : une carte carrée découpée en cases, et les soldats triés par
;;; case. Elle est reconstruite à chaque tick par un tri par comptage, en O(n),
;;; dans des tableaux préalloués : aucune allocation.
;;;
;;; Les soldats de la case C sont SORTED[CELL-START[C]] à SORTED[CELL-START[C+1] - 1].
;;; Un soldat hors de la carte compte dans la case du bord la plus proche.
;;;
;;; Les cases sont petites (1 m) pour que la collision teste peu de voisins ; une
;;; recherche plus longue, comme la portée d'une pique, parcourt plus de cases.

(defclass spatial-grid ()
  ((cell-size :initarg :cell-size :reader grid-cell-size :type single-float)
   (origin :initarg :origin :reader grid-origin :type single-float
           :documentation "Coordonnée du bord gauche et du bord haut de la carte, en mètres.")
   (columns :initarg :columns :reader grid-columns :type fixnum
            :documentation "Cases par côté : la carte est carrée.")
   (cell-start :initarg :cell-start :reader grid-cell-start :type fixnum-array
               :documentation "Début de chaque case dans SORTED, plus une case finale.")
   (cursor :initarg :cursor :reader grid-cursor :type fixnum-array
           :documentation "Compteurs, puis curseurs d'écriture, pendant la reconstruction.")
   (soldier-cell :initform (make-array 0 :element-type 'fixnum) :accessor grid-soldier-cell
                 :type fixnum-array :documentation "Case de chaque soldat.")
   (sorted :initform (make-array 0 :element-type 'fixnum) :accessor grid-sorted
           :type fixnum-array :documentation "Indices des soldats, triés par case.")
   (size :initform 0 :accessor grid-size :type fixnum
         :documentation "Soldats vivants rangés dans SORTED.")))

(defun make-spatial-grid (&key (size 256) (cell-size 1))
  "Grille d'une carte de SIZE mètres de côté, centrée sur l'origine."
  (let* ((columns (ceiling size cell-size))
         (cells (* columns columns)))
    (make-instance 'spatial-grid
                   :cell-size (float cell-size 1f0)
                   :origin (float (- (/ (* columns cell-size) 2)) 1f0)
                   :columns columns
                   :cell-start (make-array (1+ cells) :element-type 'fixnum :initial-element 0)
                   :cursor (make-array cells :element-type 'fixnum :initial-element 0))))

(defun ensure-grid-capacity (grid capacity)
  "Agrandit les tableaux par soldat. Appelée à l'ajout d'un régiment, jamais
pendant un tick."
  (when (< (length (grid-sorted grid)) capacity)
    (setf (grid-soldier-cell grid) (make-array capacity :element-type 'fixnum :initial-element 0)
          (grid-sorted grid) (make-array capacity :element-type 'fixnum :initial-element 0)))
  grid)

(declaim (inline grid-coordinate))
(defun grid-coordinate (value origin cell-size columns)
  "Colonne ou ligne de VALUE, ramenée dans la carte."
  (declare (type single-float value origin cell-size) (type fixnum columns))
  (let ((position (/ (- value origin) cell-size)))
    (declare (type single-float position))
    (cond ((< position 1f0) (if (< position 0f0) 0 (truncate position)))
          ((>= position (float (1- columns) 1f0)) (1- columns))
          (t (truncate position)))))

(defun rebuild-grid (grid soldiers)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((count (soldiers-count soldiers))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (hps (soldiers-hp soldiers))
         (origin (grid-origin grid))
         (cell-size (grid-cell-size grid))
         (columns (grid-columns grid))
         (cells (* columns columns))
         (start (grid-cell-start grid))
         (cursor (grid-cursor grid))
         (soldier-cell (grid-soldier-cell grid))
         (sorted (grid-sorted grid)))
    (declare (type fixnum count columns cells)
             (type single-float origin cell-size)
             (type float-array xs ys hps)
             (type fixnum-array start cursor soldier-cell sorted))
    (fill cursor 0)
    ;; Les morts (points de vie nuls) ne sont rangés dans aucune case : -1.
    (dotimes (i count)
      (if (<= (aref hps i) 0f0)
          (setf (aref soldier-cell i) -1)
          (let ((cell (+ (* columns (grid-coordinate (aref ys i) origin cell-size columns))
                         (grid-coordinate (aref xs i) origin cell-size columns))))
            (declare (type fixnum cell))
            (setf (aref soldier-cell i) cell)
            (incf (aref cursor cell)))))
    (let ((total 0))
      (declare (type fixnum total))
      (dotimes (cell cells)
        (setf (aref start cell) total)
        (incf total (aref cursor cell))
        (setf (aref cursor cell) (aref start cell)))
      (setf (aref start cells) total
            (grid-size grid) total))
    (dotimes (i count)
      (let ((cell (aref soldier-cell i)))
        (when (>= cell 0)
          (setf (aref sorted (aref cursor cell)) i)
          (incf (aref cursor cell)))))
    grid))
