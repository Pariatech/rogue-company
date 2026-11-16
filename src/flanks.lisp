(in-package #:rogue-company)

;;; Flancs : d'où vient une attaque, par rapport au front de la formation qui la
;;; reçoit (son orientation, pas celle du soldat, qui se tourne vers son ennemi
;;; mais ne peut pas quitter son rang).
;;;
;;;   - de face, à moins de 60° de l'axe de la formation ;
;;;   - de flanc, entre 60° et 120° ;
;;;   - de dos, au-delà.
;;;
;;; Effets :
;;;   - défense : de flanc, le défenseur n'en garde que la moitié (rondache et
;;;     garde tournées vers l'avant), de dos plus rien (STRIKE, combat.lisp) ;
;;;   - armes longues : une pique ne s'abaisse que vers l'avant de sa formation
;;;     (ACQUIRE-TARGETS) ; un piquier pris de côté se défend à la dague ;
;;;   - moral : un régiment attaqué de flanc ou de dos en perd chaque seconde,
;;;     d'autant plus qu'il y a d'attaquants (UPDATE-MORALE, morale.lisp).
;;;
;;; COUNT-FLANK-ATTACKS compte, une fois par tick et en séquence après le
;;; ciblage, les soldats qui visent chaque régiment de flanc ou de dos.

(defparameter *front-cos* 0.5f0
  "Cosinus de l'angle au-delà duquel une attaque vient du flanc (60°).")

(defparameter *rear-cos* -0.5f0
  "Cosinus de l'angle au-delà duquel une attaque vient du dos (120°).")

(defparameter *flank-defense* 0.5f0 "Part de sa défense qu'un soldat garde de flanc.")

(defparameter *rear-defense* 0f0 "Part de sa défense qu'un soldat garde de dos.")

(defparameter *flank-morale-loss* 0.75f0
  "Moral perdu par seconde par un régiment attaqué de flanc, à pleine pression.")

(defparameter *rear-morale-loss* 1.5f0
  "Moral perdu par seconde par un régiment attaqué de dos, à pleine pression.")

(defparameter *flank-pressure* 8
  "Attaquants de flanc (ou de dos) pour la perte de moral maximale ; en dessous,
elle est proportionnelle.")

(declaim (inline attack-sector))
(defun attack-sector (facing-cos facing-sin dx dy)
  "Côté d'une attaque venant de DX, DY (vecteur du défenseur vers l'attaquant),
pour une formation tournée vers (FACING-COS, FACING-SIN) : 0 de face, 1 de
flanc, 2 de dos."
  (declare (type single-float facing-cos facing-sin dx dy)
           (optimize (speed 3) (safety 0)))
  (let ((along (+ (* dx facing-cos) (* dy facing-sin)))
        (length (sqrt (+ (* dx dx) (* dy dy)))))
    (declare (type single-float along length))
    (cond ((>= along (* (the single-float *front-cos*) length)) 0)
          ((<= along (* (the single-float *rear-cos*) length)) 2)
          (t 1))))

(declaim (inline sector-defense))
(defun sector-defense (sector)
  "Part de sa défense que garde un soldat attaqué du côté SECTOR."
  (declare (type fixnum sector))
  (case sector
    (0 1f0)
    (1 (the single-float *flank-defense*))
    (t (the single-float *rear-defense*))))

(defun count-flank-attacks (battle)
  "Compte, pour chaque régiment, les soldats ennemis qui le visent de flanc et
de dos à ce tick, et pour chaque soldat, ceux qui le visent. Séquentiel : écrit
dans les régiments et les soldats visés."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((soldiers (battle-soldiers battle))
         (regiments (battle-regiments battle))
         (targets (soldiers-target soldiers))
         (attackers (soldiers-attackers soldiers))
         (soldier-regiments (soldiers-regiment soldiers))
         (xs (soldiers-x soldiers)) (ys (soldiers-y soldiers)))
    (declare (type vector regiments)
             (type fixnum-array targets attackers soldier-regiments)
             (type float-array xs ys))
    (loop for regiment across regiments
          do (setf (regiment-flank-attackers regiment) 0
                   (regiment-rear-attackers regiment) 0)
             (fill (the fixnum-array attackers) 0
                   :start (regiment-start regiment) :end (regiment-end regiment)))
    (loop for attacker across regiments
          unless (regiment-routed-p attacker)
            do (loop for i of-type fixnum from (regiment-start attacker) below (regiment-end attacker)
                     do (let ((target (aref targets i)))
                          (when (>= target 0)
                            (incf (aref attackers target))
                            (let* ((defender (aref regiments (aref soldier-regiments target)))
                                   (facing (formation-facing (regiment-formation defender)))
                                   (sector (attack-sector (cos facing) (sin facing)
                                                          (- (aref xs i) (aref xs target))
                                                          (- (aref ys i) (aref ys target)))))
                              (declare (type single-float facing) (type fixnum sector))
                              (case sector
                                (1 (incf (regiment-flank-attackers defender)))
                                (2 (incf (regiment-rear-attackers defender)))))))))))

(defun flank-morale-loss (regiment)
  "Moral perdu par seconde par REGIMENT pour les attaques de flanc et de dos."
  (let ((pressure (float *flank-pressure* 1f0)))
    (+ (* *flank-morale-loss* (min 1f0 (/ (regiment-flank-attackers regiment) pressure)))
       (* *rear-morale-loss* (min 1f0 (/ (regiment-rear-attackers regiment) pressure))))))
