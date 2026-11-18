(in-package #:rogue-company)

;;; La charge, pour tout régiment de mêlée. Sous un ordre d'attaque, à
;;; *CHARGE-SECONDS* de course de sa cible, il part à la charge (au pas de
;;; course, au galop pour la cavalerie : MOVE-REGIMENT). Au premier contact,
;;; et pendant *CHARGE-BONUS-SECONDS*, chaque coup de ses soldats porte en plus
;;; son bonus de charge (REGIMENT-CHARGE-BONUS, lu par STRIKE) : l'élan, la
;;; masse par la vitesse de course, plus le bonus de charge de l'arme (la
;;; lance). Les piquiers (trait :NO-CHARGE) ne chargent pas : ils gardent la
;;; cohésion de leur formation. Les tireurs non plus : ils s'arrêtent à portée
;;; de tir.

(defparameter *charge-seconds* 5f0
  "Un régiment part à la charge à autant de secondes de course de sa cible :
15 m pour l'infanterie (3 m/s), 40 m pour la cavalerie légère (8 m/s).")

(defparameter *charge-bonus-seconds* 5f0
  "Durée du bonus de charge après le premier contact, en secondes.")

(defparameter *charge-momentum-damage* 3.4f0
  "Dégâts ajoutés à chaque coup pendant le bonus de charge, par unité de masse
et par mètre par seconde de course : un fantassin (masse 1, 3 m/s) +10, un
chevau-léger (masse 6, 8 m/s) +163. L'armure les réduit, comme les dégâts
ordinaires.")

(defun charge-capable-p (regiment)
  "Vrai si REGIMENT charge : de mêlée, sans le trait :NO-CHARGE."
  (and (eq (attack-kind (stats-primary (regiment-stats regiment))) :melee)
       (not (member :no-charge (regiment-traits regiment)))))

(defun regiment-charge-bonus (regiment)
  "Dégâts ajoutés à chaque coup de REGIMENT pendant le bonus de charge : masse
par vitesse de course, fois *CHARGE-MOMENTUM-DAMAGE*, plus le bonus de charge de
son arme principale."
  (let ((stats (regiment-stats regiment)))
    (+ (* *charge-momentum-damage* (stats-mass stats) (stats-run-speed stats))
       (attack-charge-bonus (stats-primary stats)))))

(defun pike-wall-p (regiment)
  "Vrai si REGIMENT présente des piques : une arme principale de mêlée à portée
minimale, en bon ordre. Chargé de face, il vole le bonus de charge de
l'assaillant (STRIKE) : l'assaillant n'en a pas sur lui, et ses piques le
portent contre l'assaillant tant que dure sa charge."
  (let ((primary (stats-primary (regiment-stats regiment))))
    (and (eq (attack-kind primary) :melee)
         (plusp (attack-min-reach primary))
         (not (regiment-routed-p regiment)))))

(defun charge-bonus-active-p (regiment tick)
  "Vrai si les coups de REGIMENT portent son bonus de charge au tick TICK."
  (let ((contact (regiment-charge-contact regiment)))
    (and (>= contact 0)
         (< (- tick contact) (* *charge-bonus-seconds* +ticks-per-second+)))))

(defun update-charge (battle regiment)
  "En début de tick, après UPDATE-ENGAGEMENT. Sous un ordre d'attaque, un
régiment qui peut charger part à la charge à *CHARGE-SECONDS* de course de sa
cible, et court jusqu'au premier contact, qui ouvre le bonus. Le bonus fini,
la charge est finie : il pourra recharger une fois dégagé. Hors d'un ordre
d'attaque, plus de charge."
  (when (charge-capable-p regiment)
    (let ((orders (regiment-orders regiment))
          (tick (battle-tick-count battle))
          (contact (regiment-charge-contact regiment)))
      (cond ((not (eq (first orders) :attack))
             (setf (regiment-charging-p regiment) nil
                   (regiment-charge-contact regiment) -1))
            ((>= contact 0)
             ;; Au contact : le bonus court, puis la charge prend fin.
             (unless (charge-bonus-active-p regiment tick)
               (setf (regiment-charge-contact regiment) -1)))
            ((regiment-charging-p regiment)
             ;; Premier contact : ses soldats atteignent l'ennemi (être visé par
             ;; des armes plus longues ne compte pas).
             (when (plusp (regiment-engaged regiment))
               (setf (regiment-charging-p regiment) nil
                     (regiment-charge-contact regiment) tick)))
            ((and (not (regiment-in-melee-p regiment))
                  (not (regiment-holding-p regiment))
                  (zerop (mod (+ tick (regiment-index regiment)) *advance-period*)))
             ;; Part à la charge quand la cible est assez près.
             (let* ((enemy (aref (battle-regiments battle) (second orders)))
                    (distance (* *charge-seconds* (stats-run-speed (regiment-stats regiment)))))
               (when (and (plusp (regiment-alive enemy)) (not (regiment-routed-p enemy))
                          (< (regiment-gap regiment enemy (battle-soldiers battle) distance)
                             distance))
                 (setf (regiment-charging-p regiment) t))))))))
