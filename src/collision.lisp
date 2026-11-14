(in-package #:rogue-company)

;;; Collision entre soldats : deux soldats plus proches que la somme de leurs
;;; rayons se repoussent. Tout le monde entre en collision avec tout le monde,
;;; alliés, ennemis et soldats du même régiment.
;;;
;;; La séparation est douce : à chaque tick, chaque paire ne rattrape qu'une part
;;; (*COLLISION-STIFFNESS*) de son chevauchement. Les poussées sont d'abord
;;; accumulées dans PUSH-X et PUSH-Y, puis appliquées ensemble : le résultat ne
;;; dépend pas de l'ordre de parcours, et la bataille se rejoue.
;;;
;;; La masse partage la séparation : un cheval bouscule un fantassin bien plus
;;; qu'il n'est bousculé.

(defparameter *collision-stiffness* 0.5f0
  "Part du chevauchement corrigée par tick. 1 sépare d'un coup mais fait vibrer
les foules serrées.")

(defun resolve-collisions (soldiers grid max-radius)
  "Sépare les soldats qui se chevauchent. MAX-RADIUS est le plus grand rayon de
la bataille : il fixe jusqu'où chercher les voisins."
  (declare (optimize (speed 3) (safety 1))
           (type single-float max-radius))
  (let* ((count (soldiers-count soldiers))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
         (radii (soldiers-radius soldiers)) (masses (soldiers-mass soldiers))
         (push-xs (soldiers-push-x soldiers)) (push-ys (soldiers-push-y soldiers))
         (columns (grid-columns grid))
         (cell-size (grid-cell-size grid))
         (start (grid-cell-start grid))
         (sorted (grid-sorted grid))
         (soldier-cell (grid-soldier-cell grid))
         (stiffness *collision-stiffness*))
    (declare (type fixnum count columns)
             (type float-array xs ys radii masses push-xs push-ys)
             (type single-float cell-size stiffness)
             (type fixnum-array start sorted soldier-cell))
    (fill push-xs 0f0 :end count)
    (fill push-ys 0f0 :end count)
    (dotimes (i count)
      (let* ((xi (aref xs i)) (yi (aref ys i))
             (ri (aref radii i)) (mi (aref masses i))
             (cell (aref soldier-cell i))
             (row (floor cell columns))
             (column (- cell (* row columns)))
             ;; Cases à parcourir de chaque côté pour trouver tout voisin à moins
             ;; de RI + MAX-RADIUS.
             (span (min 8 (the fixnum (ceiling (+ ri max-radius) cell-size)))))
        (declare (type single-float xi yi ri mi)
                 (type fixnum cell row column span))
        (loop for r of-type fixnum from (max 0 (- row span)) to (min (1- columns) (+ row span))
              do (loop for c of-type fixnum from (max 0 (- column span))
                         to (min (1- columns) (+ column span))
                       do (let ((neighbour (+ (* r columns) c)))
                            (declare (type fixnum neighbour))
                            (loop for k of-type fixnum from (aref start neighbour)
                                    below (aref start (1+ neighbour))
                                  do (let ((j (aref sorted k)))
                                       (declare (type fixnum j))
                                       ;; Chaque paire une seule fois, depuis le plus petit indice.
                                       (when (> j i)
                                         (let* ((dx (- (aref xs j) xi))
                                                (dy (- (aref ys j) yi))
                                                (reach (+ ri (aref radii j)))
                                                (d2 (+ (* dx dx) (* dy dy))))
                                           (declare (type single-float dx dy reach d2))
                                           (when (< d2 (* reach reach))
                                             (let* ((mj (aref masses j))
                                                    (distance (sqrt (the (single-float 0f0) d2)))
                                                    ;; Deux soldats au même point : séparés le long de x.
                                                    (nx (if (> distance 1f-6) (/ dx distance) 1f0))
                                                    (ny (if (> distance 1f-6) (/ dy distance) 0f0))
                                                    (correction (* stiffness (- reach distance)))
                                                    (share-i (/ mj (+ mi mj)))
                                                    (share-j (- 1f0 share-i)))
                                               (declare (type single-float mj distance nx ny
                                                              correction share-i share-j))
                                               (decf (aref push-xs i) (* nx correction share-i))
                                               (decf (aref push-ys i) (* ny correction share-i))
                                               (incf (aref push-xs j) (* nx correction share-j))
                                               (incf (aref push-ys j) (* ny correction share-j)))))))))))))
    (dotimes (i count)
      (incf (aref xs i) (aref push-xs i))
      (incf (aref ys i) (aref push-ys i)))))
