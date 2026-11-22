(in-package #:rogue-company)

;;; Rythme de la simulation de bataille. L'horloge à pas fixe (game-kit) la fait
;;; avancer à ce rythme (battle-scene.lisp) ; les durées des stats sont
;;; converties en ticks avec ce taux (stats.lisp).

(defconstant +ticks-per-second+ 30
  "Ticks de simulation par seconde de simulation.")
