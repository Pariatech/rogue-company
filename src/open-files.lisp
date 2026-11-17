(in-package #:rogue-company)

;;; Files ouvertes au passage d'un ami. Les collisions valent entre tous les
;;; soldats : pour qu'un régiment en traverse un autre de son camp (des
;;; tireurs qui se replient derrière leurs piques, des piques qui avancent à
;;; travers leurs tireurs), chacun des deux, quand un soldat ami est à moins
;;; de *OPEN-TRIGGER* des siens, écarte ses files à *OPEN-FILE-SPACING* entre
;;; deux voisins d'un rang. Il double à peu près sa largeur, et les soldats de
;;; l'autre se glissent dans les intervalles, poussés par les collisions.
;;; *OPEN-HOLD* après le dernier ami proche, il resserre ses files. Pas en
;;; mêlée : au contact, un régiment garde ses rangs serrés (et les resserre).

(defparameter *open-trigger* 2f0
  "Distance, de soldat à soldat, d'un régiment ami sous laquelle un régiment
ouvre ses files.")

(defparameter *open-file-spacing* 2f0
  "Espacement entre deux voisins d'un rang, files ouvertes, en mètres : 1,4 m de
passage entre deux soldats de 0,3 m de rayon.")

(defparameter *open-hold* 0.5f0
  "Secondes en files ouvertes après le dernier ami proche, vu au recalcul
suivant (toutes les 0,5 s) : les files se referment entre 0,5 et 1 s après le
passage.")

(defun regiment-base-file-spacing (regiment)
  "Espacement des files de REGIMENT hors passage : ouvertes pour la contremarche
(shooting.lisp), NIL (l'espacement des rangs) sinon."
  (and (eq (regiment-drill regiment) :countermarch)
       (max (stats-spacing (regiment-stats regiment)) *volley-file-spacing*)))

(defun friend-near-p (battle regiment)
  "Vrai si un soldat d'un autre régiment vivant du même camp, non débandé, est
à moins de *OPEN-TRIGGER* d'un soldat de REGIMENT."
  (let ((soldiers (battle-soldiers battle)))
    (loop for other across (battle-regiments battle)
          thereis (and (not (eq other regiment))
                       (= (regiment-side other) (regiment-side regiment))
                       (plusp (regiment-alive other))
                       (not (regiment-routed-p other))
                       (< (regiment-gap regiment other soldiers *open-trigger*)
                          *open-trigger*)))))

(defun set-file-spacing (regiment spacing)
  (let ((formation (regiment-formation regiment)))
    (unless (eql (formation-file-spacing formation) spacing)
      (setf (formation-file-spacing formation) spacing
            (regiment-slots-dirty-p regiment) t))))

(defun update-open-files (battle regiment)
  "Ouvre ou resserre les files de REGIMENT : resserrées dès qu'il est engagé,
ouvertes ou resserrées à chaque recalcul d'engagement sinon."
  (when (and (plusp (regiment-alive regiment))
             (not (regiment-routed-p regiment)))
    (let ((tick (battle-tick-count battle))
          (open (>= (regiment-open-until regiment) 0)))
      (flet ((close-files ()
               (setf (regiment-open-until regiment) -1)
               (set-file-spacing regiment (regiment-base-file-spacing regiment))))
        (cond ((plusp (regiment-engaged regiment))
               (when open (close-files)))
              ((not (zerop (mod (+ tick (regiment-index regiment)) *advance-period*))))
              ((friend-near-p battle regiment)
               (setf (regiment-open-until regiment)
                     (+ tick (round (* *open-hold* +ticks-per-second+))))
               (set-file-spacing regiment
                                 (max *open-file-spacing*
                                      (or (regiment-base-file-spacing regiment) 0f0))))
              ((and open (> tick (regiment-open-until regiment)))
               (close-files)))))))
