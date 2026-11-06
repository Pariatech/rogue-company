(in-package #:rogue-company)

;;; Le jeu lit le clavier par KEY-DOWN-P, qui combine le clavier réel et
;;; les touches simulées par l'outil game_input, sans focus de fenêtre.

(defvar *simulated-keys* (make-hash-table)
  "Touche raylib (mot-clé, par exemple :KEY-RIGHT) -> images restantes.")

(defun key-down-p (key)
  (or (plusp (gethash key *simulated-keys* 0))
      (raylib:is-key-down key)))

(defun advance-input ()
  "Consomme une image de chaque touche simulée. Appelé après chaque update."
  (loop for key in (loop for k being the hash-keys of *simulated-keys* collect k)
        do (when (<= (decf (gethash key *simulated-keys*)) 0)
             (remhash key *simulated-keys*))))
