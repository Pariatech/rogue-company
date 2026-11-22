(in-package #:rogue-company)

;;; L'état du jeu et son lancement. La boucle, UPDATE et DRAW viennent de
;;; game-kit : son GAME-STATE délègue à la scène courante (main-menu.lisp au
;;; départ).
;;;
;;; DEFVAR pour que *STATE* survive au rechargement.

(defvar *state* (make-game-state 'main-menu))

(defun main ()
  "Ouvre la fenêtre du jeu, avec le serveur MCP. Thread principal (scripts/run.lisp)."
  (run-game :title "rogue-company" :system :rogue-company))
