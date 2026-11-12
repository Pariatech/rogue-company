(in-package #:rogue-company)

;;; Protocole des scènes et changement de scène.
;;;
;;; Une scène est une instance d'une sous-classe de SCENE qui spécialise
;;; SCENE-UPDATE et SCENE-DRAW, et au besoin SCENE-ENTER et SCENE-EXIT.
;;; GAME-STATE tient la scène courante ; on en change avec CHANGE-SCENE.
;;; Chaque scène a son fichier, chargé après celui-ci et avant game.lisp.

(defclass scene () ())

(defgeneric scene-update (scene dt)
  (:documentation "Avance la scène de DT secondes. Pas appelé en pause.")
  (:method ((scene scene) dt) (declare (ignore dt))))

(defgeneric scene-draw (scene)
  (:documentation "Dessine la scène. Appelé à chaque image, même en pause.")
  (:method ((scene scene)) (clear-background :black)))

(defgeneric scene-enter (scene)
  (:documentation "Appelé quand la scène devient la scène courante.")
  (:method ((scene scene))))

(defgeneric scene-exit (scene)
  (:documentation "Appelé quand la scène cesse d'être la scène courante.")
  (:method ((scene scene))))

(defgeneric scene-snapshot (scene)
  (:documentation "Résumé de la scène pour game_state : table de hachage JSON.")
  (:method ((scene scene))
    (ht "type" (string-downcase (class-name (class-of scene))))))

(defclass game-state ()
  ;; L'initform sert aussi quand une instance existante gagne ce slot au
  ;; rechargement de la classe.
  ((scene :initarg :scene :initform (make-instance 'main-menu) :accessor current-scene)))

;;; Changement de scène

(defun change-scene (state scene)
  "Remplace la scène courante de STATE par SCENE (une instance, ou le nom de sa
classe)."
  (let ((scene (if (symbolp scene) (make-instance scene) scene)))
    (scene-exit (current-scene state))
    (setf (current-scene state) scene)
    (scene-enter scene)
    scene))
