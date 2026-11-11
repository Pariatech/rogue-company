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

;;; Saisie de texte : la console lit les caractères par NEXT-CHAR, qui donne
;;; d'abord les caractères simulés par game_input (paramètre text), puis ceux
;;; du clavier réel. Un saut de ligne simulé vaut la touche Entrée.

(defvar *simulated-chars* (sb-concurrency:make-queue :name "simulated-chars")
  "Caractères en attente, déposés par game_input.")

(defun simulate-text (text)
  "Ajoute TEXT à la saisie simulée. Peut être appelé depuis n'importe quel thread."
  (loop for char across text
        do (sb-concurrency:enqueue char *simulated-chars*)))

(defun next-char ()
  "Renvoie le prochain caractère saisi, ou NIL. À appeler une fois par
caractère, jusqu'à NIL, à chaque image."
  (or (sb-concurrency:dequeue *simulated-chars*)
      (let ((code (raylib:get-char-pressed)))
        (when (plusp code) (code-char code)))))

(defun key-pressed-p (key &key repeat)
  "Vrai si KEY vient d'être enfoncée sur le clavier réel, ou répétée si REPEAT."
  (or (raylib:is-key-pressed key)
      (and repeat (raylib:is-key-pressed-repeat key))))
