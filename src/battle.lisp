(in-package #:rogue-company)

;;; Simulation de la bataille. Elle n'avance que par BATTLE-TICK, d'un pas fixe,
;;; et ne connaît ni raylib, ni l'horloge réelle, ni le clavier : elle tourne
;;; sans fenêtre, dans les tests, et se rejoue à l'identique depuis sa graine.
;;; La scène (battle-scene.lisp) décide combien de ticks exécuter et dessine.
;;;
;;; Tout tirage aléatoire passe par BATTLE-RNG (rng.lisp).

(defparameter *battle-seed* 42
  "Graine par défaut d'une nouvelle bataille. Fixe pendant le développement,
pour reproduire une bataille.")

(defclass battle ()
  ((seed :initarg :seed :reader battle-seed
         :documentation "Graine du générateur : avec elle, la bataille se rejoue.")
   (rng :initarg :rng :reader battle-rng)
   (tick :initform 0 :accessor battle-tick-count
         :documentation "Ticks exécutés depuis le début de la bataille.")
   (regiments :initform (make-array 0 :adjustable t :fill-pointer t) :reader battle-regiments)
   (soldiers :initform (make-soldiers) :reader battle-soldiers
             :documentation "Tableaux typés des soldats de tous les régiments.")))

(defun make-battle (&key (seed *battle-seed*))
  (make-instance 'battle :seed seed :rng (make-rng seed)))

(defun add-regiment (battle stats count &key (name "Régiment") (side 0) (x 0) (y 0) (facing 0)
                                             (ranks (stats-ranks stats)) (scatter 0))
  "Ajoute un régiment de COUNT soldats aux STATS données, en formation autour de
X, Y. Avec SCATTER, chaque soldat part d'un point tiré au hasard à au plus
SCATTER mètres de son emplacement, sur chaque axe. Renvoie le régiment."
  (let* ((soldiers (battle-soldiers battle))
         (start (reserve-soldiers soldiers count))
         (regiments (battle-regiments battle))
         (regiment (make-instance 'regiment
                                  :index (length regiments) :name name :side side :stats stats
                                  :formation (make-formation :x x :y y :facing facing :ranks ranks
                                                             :spacing (stats-spacing stats))
                                  :morale (stats-morale stats)
                                  :start start :capacity count :alive count))
         (rng (battle-rng battle))
         (scatter (float scatter 1f0)))
    (vector-push-extend regiment regiments)
    (order-move regiment x y facing)
    (update-regiment-slots regiment soldiers)
    (loop for i from start below (+ start count)
          do (setf (aref (soldiers-regiment soldiers) i) (regiment-index regiment)
                   (aref (soldiers-hp soldiers) i) (stats-hp stats)
                   (aref (soldiers-x soldiers) i)
                   (+ (aref (soldiers-slot-x soldiers) i) (rng-range rng (- scatter) scatter))
                   (aref (soldiers-y soldiers) i)
                   (+ (aref (soldiers-slot-y soldiers) i) (rng-range rng (- scatter) scatter))
                   (aref (soldiers-prev-x soldiers) i) (aref (soldiers-x soldiers) i)
                   (aref (soldiers-prev-y soldiers) i) (aref (soldiers-y soldiers) i)))
    regiment))

(defun battle-tick (battle)
  "Avance BATTLE d'un tick. Les systèmes s'exécutent toujours dans cet ordre,
et s'ajoutent ici au fil des étapes :
  1. copie des positions courantes dans les positions précédentes ;
  2. régiments : ordres, formation, moral (chacun un tick sur plusieurs) ;
  3. soldats : déplacement vers leur emplacement ;
  4. combat : ciblage en lecture seule, puis dégâts appliqués en séquence."
  (let ((soldiers (battle-soldiers battle))
        (regiments (battle-regiments battle)))
    (copy-previous-positions soldiers)
    (loop for regiment across regiments
          do (update-regiment-slots regiment soldiers))
    (loop for regiment across regiments
          do (move-regiment regiment soldiers)))
  (incf (battle-tick-count battle))
  battle)

(defun battle-run (battle ticks)
  "Exécute TICKS ticks. Pour les tests et le diagnostic."
  (dotimes (i ticks battle)
    (battle-tick battle)))

(defun battle-digest (battle)
  "Empreinte MD5 en hexadécimal de l'état de BATTLE : deux batailles de même
graine ont la même empreinte après le même nombre de ticks. Alloue : à réserver
aux tests et au diagnostic."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
        (soldiers (battle-soldiers battle)))
    (flet ((add-u64 (value)
             (dotimes (i 8)
               (vector-push-extend (ldb (byte 8 (* 8 i)) value) octets))))
      (add-u64 (battle-tick-count battle))
      (loop for word across (battle-rng battle) do (add-u64 word))
      (add-u64 (soldiers-count soldiers))
      (dolist (slot *soldier-float-arrays*)
        (let ((array (slot-value soldiers slot)))
          (dotimes (i (soldiers-count soldiers))
            (add-u64 (ldb (byte 32 0) (sb-kernel:single-float-bits (aref array i)))))))
      (dotimes (i (soldiers-count soldiers))
        (add-u64 (aref (soldiers-regiment soldiers) i))))
    (format nil "~(~{~2,'0x~}~)"
            (coerce (sb-md5:md5sum-sequence (coerce octets '(simple-array (unsigned-byte 8) (*))))
                    'list))))

(defun battle-snapshot (battle)
  "Résumé de BATTLE pour game_state."
  (let ((soldiers (battle-soldiers battle)))
    (ht "seed" (battle-seed battle)
        "tick" (battle-tick-count battle)
        "soldiers" (soldiers-count soldiers)
        "regiments" (loop for regiment across (battle-regiments battle)
                          collect (regiment-snapshot regiment soldiers)))))
