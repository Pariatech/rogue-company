(in-package #:rogue-company)

;;; Points d'entrée appelés par la boucle de engine.lisp, et l'état du jeu.
;;; UPDATE et DRAW délèguent à la scène courante (voir scene.lisp).
;;;
;;; DEFVAR pour que *STATE* survive au rechargement ; GAME-STATE est une classe
;;; pour que l'instance existante suive les changements de sa définition.

(defun make-game-state ()
  (let ((state (make-instance 'game-state)))
    (scene-enter (current-scene state))
    state))

(defun update (state dt)
  (scene-update (current-scene state) dt))

(defun draw (state)
  (scene-draw (current-scene state)))

(defvar *state* (make-game-state))
