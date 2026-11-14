(in-package #:rogue-company)

;;; Stats effectives d'un régiment : tout ce que la simulation lit. Elles se
;;; calculent une fois, à la création de la bataille, depuis l'homme de base,
;;; l'équipement et les améliorations (equipment.lisp). La simulation ne lit
;;; jamais les objets ni les améliorations.
;;;
;;; Unités : mètres, mètres par seconde, kilogrammes. Les durées sont converties
;;; en ticks à la construction, pour que les compteurs de la simulation restent
;;; des entiers. L'adresse, la précision, la défense et la couverture sont des
;;; fractions, ajoutées puis bornées au moment du tirage.

(defparameter *common-stats*
  '(:hp :armor :speed :run-speed :mass :load :endurance :defense :cover
    :melee-skill :marksmanship :morale :discipline :spacing :ranks)
  "Stats communes, quelle que soit l'arme en main. Chacune est un slot de
REGIMENT-STATS du même nom.")

(defparameter *attack-stats*
  '(:min-reach :reach :damage :penetration :accuracy :cooldown :charge-bonus :ready-time)
  "Stats numériques d'un profil d'attaque. Pour une arme de tir, REACH est la
portée et COOLDOWN le rechargement.")

(defparameter *attack-durations* '(:cooldown :ready-time)
  "Stats d'attaque données en secondes et rangées en ticks.")

(defun seconds->ticks (seconds &optional (minimum 0))
  (max minimum (round (* seconds +ticks-per-second+))))

(defun ticks->seconds (ticks)
  (json-float (/ ticks +ticks-per-second+)))

(defun json-float (value)
  "VALUE en double arrondi au millième : 1,2 plutôt que 1,2000000476837158,
l'écart d'un single-float converti."
  (/ (round (* value 1000)) 1000d0))

;;; Profil d'attaque

(defclass attack-profile ()
  ((kind :initarg :kind :reader attack-kind :type (member :melee :ranged))
   (min-reach :initarg :min-reach :reader attack-min-reach :type single-float
              :documentation "En deçà, l'arme ne sert pas : un ennemi collé à une pique.")
   (reach :initarg :reach :reader attack-reach :type single-float
          :documentation "Portée de mêlée, ou portée maximale de tir.")
   (damage :initarg :damage :reader attack-damage :type single-float)
   (penetration :initarg :penetration :reader attack-penetration :type single-float
                :documentation "Armure ignorée par chaque coup.")
   (accuracy :initarg :accuracy :reader attack-accuracy :type single-float
             :documentation "S'ajoute à MELEE-SKILL ou à MARKSMANSHIP.")
   (cooldown :initarg :cooldown :reader attack-cooldown :type fixnum
             :documentation "Ticks entre deux coups, ou rechargement d'une arme de tir.")
   (charge-bonus :initarg :charge-bonus :reader attack-charge-bonus :type single-float
                 :documentation "Dégâts en plus au premier contact d'une charge.")
   (volley :initarg :volley :reader attack-volley-p :type boolean
           :documentation "Arme de tir qui permet la salve par rang et la contremarche.")
   (ready-time :initarg :ready-time :reader attack-ready-time :type fixnum
               :documentation "Ticks pour prendre cette arme en main.")))

(defun make-attack-profile (kind volley values)
  "Profil d'attaque depuis VALUES, une table de stats d'attaque en secondes."
  (flet ((value (key) (gethash key values 0)))
    (make-instance 'attack-profile
                   :kind kind
                   :volley (and volley t)
                   :min-reach (float (value :min-reach) 1f0)
                   :reach (float (value :reach) 1f0)
                   :damage (float (value :damage) 1f0)
                   :penetration (float (value :penetration) 1f0)
                   :accuracy (float (value :accuracy) 1f0)
                   :cooldown (seconds->ticks (value :cooldown) 1)
                   :charge-bonus (float (value :charge-bonus) 1f0)
                   :ready-time (seconds->ticks (value :ready-time)))))

(defun attack-snapshot (attack)
  (ht "kind" (string-downcase (attack-kind attack))
      "min_reach" (json-float (attack-min-reach attack))
      "reach" (json-float (attack-reach attack))
      "damage" (json-float (attack-damage attack))
      "penetration" (json-float (attack-penetration attack))
      "accuracy" (json-float (attack-accuracy attack))
      "cooldown_seconds" (ticks->seconds (attack-cooldown attack))
      "charge_bonus" (json-float (attack-charge-bonus attack))
      "volley" (if (attack-volley-p attack) t 'yason:false)
      "ready_seconds" (ticks->seconds (attack-ready-time attack))))

;;; Stats du régiment

(defclass regiment-stats ()
  ((hp :reader stats-hp :type single-float)
   (armor :reader stats-armor :type single-float
          :documentation "Retirée des dégâts reçus, moins la pénétration de l'arme.")
   (speed :reader stats-speed :type single-float :documentation "Marche, m/s.")
   (run-speed :reader stats-run-speed :type single-float :documentation "Course et charge, m/s.")
   (mass :reader stats-mass :type single-float :documentation "Poids dans la poussée.")
   (load :reader stats-load :type single-float
         :documentation "Kilogrammes portés, monture exclue : nourrit la fatigue.")
   (endurance :reader stats-endurance :type single-float)
   (defense :reader stats-defense :type single-float
            :documentation "Chance de faire rater un coup de mêlée.")
   (cover :reader stats-cover :type single-float
          :documentation "Protection contre le tir, de face.")
   (melee-skill :reader stats-melee-skill :type single-float)
   (marksmanship :reader stats-marksmanship :type single-float)
   (morale :reader stats-morale :type single-float)
   (discipline :reader stats-discipline :type single-float)
   (spacing :reader stats-spacing :type single-float
            :documentation "Écart entre files et entre rangs, en mètres.")
   (ranks :reader stats-ranks :type fixnum :documentation "Profondeur par défaut de la formation.")
   (primary :initarg :primary :reader stats-primary :type attack-profile)
   (secondary :initarg :secondary :reader stats-secondary :type attack-profile)
   (traits :initarg :traits :reader stats-traits
           :documentation "Comportements apportés par l'équipement et les améliorations."))
  (:documentation "Stats effectives d'un régiment, en lecture seule pour la simulation."))

(defun stat-slot (key)
  "Nom du slot de REGIMENT-STATS pour la stat commune KEY."
  (intern (symbol-name key) '#:rogue-company))

(defun make-regiment-stats (values primary secondary traits)
  "Stats depuis VALUES, une table des stats communes."
  (let ((stats (make-instance 'regiment-stats :primary primary :secondary secondary
                                              :traits traits)))
    (dolist (key *common-stats* stats)
      (let ((value (gethash key values 0)))
        (setf (slot-value stats (stat-slot key))
              (if (eq key :ranks)
                  (max 1 (round value))
                  (float value 1f0)))))))

(defun stats-snapshot (stats)
  "Résumé lisible pour game_state, durées en secondes."
  (let ((table (ht "primary" (attack-snapshot (stats-primary stats))
                   "secondary" (attack-snapshot (stats-secondary stats))
                   "traits" (map 'vector #'string-downcase (stats-traits stats)))))
    (dolist (key *common-stats* table)
      (let ((value (slot-value stats (stat-slot key))))
        (setf (gethash (substitute #\_ #\- (string-downcase key)) table)
              (if (floatp value) (json-float value) value))))))
