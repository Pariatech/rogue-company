(in-package #:rogue-company)

;;; Scène de bataille. Échap ramène au menu principal.

(defclass battle-scene (scene) ()
  (:documentation "La bataille. Pour l'instant un écran noir."))

(defmethod scene-update ((battle battle-scene) dt)
  (declare (ignore dt))
  (when (key-pressed-p :key-escape)
    (change-scene *state* 'main-menu)))
