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
   (soldiers :initform nil :accessor battle-soldiers
             :documentation "Tableaux typés des soldats (soldiers.lisp, à venir).")))

(defun make-battle (&key (seed *battle-seed*))
  (make-instance 'battle :seed seed :rng (make-rng seed)))

(defun battle-tick (battle)
  "Avance BATTLE d'un tick. Les systèmes s'exécutent toujours dans cet ordre,
et s'ajoutent ici au fil des étapes :
  1. copie des positions courantes dans les positions précédentes ;
  2. régiments : ordres, formation, moral (chacun un tick sur plusieurs) ;
  3. soldats : déplacement vers leur emplacement ;
  4. combat : ciblage en lecture seule, puis dégâts appliqués en séquence."
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
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (flet ((add-u64 (value)
             (dotimes (i 8)
               (vector-push-extend (ldb (byte 8 (* 8 i)) value) octets))))
      (add-u64 (battle-tick-count battle))
      (loop for word across (battle-rng battle) do (add-u64 word))
      ;; Les tableaux de soldats s'ajouteront ici.
      )
    (format nil "~(~{~2,'0x~}~)"
            (coerce (sb-md5:md5sum-sequence (coerce octets '(simple-array (unsigned-byte 8) (*))))
                    'list))))

(defun battle-snapshot (battle)
  "Résumé de BATTLE pour game_state."
  (ht "seed" (battle-seed battle)
      "tick" (battle-tick-count battle)
      "regiments" (length (battle-regiments battle))))
