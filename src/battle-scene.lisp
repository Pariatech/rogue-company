(in-package #:rogue-company)

;;; Scène de bataille : fait avancer la simulation (battle.lisp) au rythme de son
;;; horloge à pas fixe, lit les commandes et dessine.
;;;
;;; Espace suspend ou reprend la simulation, + et - changent sa vitesse, Échap
;;; ramène au menu principal. Cette pause est propre à la bataille : celle du jeu
;;; (console, game_control) arrête aussi la lecture du clavier.

(defparameter *battle-speeds* '(0.25 0.5 1 2 4)
  "Vitesses proposées par + et -, en ordre croissant.")

(defclass battle-scene (scene)
  ;; Des initforms plutôt que SCENE-ENTER : une scène neuve a une bataille neuve,
  ;; et une instance existante reçoit ses nouveaux slots au rechargement.
  ((battle :initform (make-battle) :reader scene-battle)
   (clock :initform (make-fixed-clock) :reader scene-clock)
   (resume-speed :initform 1 :accessor scene-resume-speed
                 :documentation "Vitesse retrouvée quand on reprend après Espace.")
   (tick-text :initform (make-text-buffer) :reader scene-tick-text
              :documentation "Chiffres du tick, réécrits à chaque image sans allouer.")
   (speed-text :initform "" :accessor scene-speed-text)
   (speed-key :initform nil :accessor scene-speed-key
              :documentation "Vitesse et pause de SPEED-TEXT : le texte n'est reconstruit
que s'ils changent, ce qui est rare."))
  (:documentation "La bataille. Pour l'instant un écran noir avec le tick et la vitesse."))

;;; Vitesse

(defun battle-paused-p (scene)
  (clock-paused-p (scene-clock scene)))

(defun battle-speed (scene)
  "Vitesse choisie, même en pause."
  (if (battle-paused-p scene)
      (scene-resume-speed scene)
      (clock-time-scale (scene-clock scene))))

(defun toggle-battle-pause (scene)
  (let ((clock (scene-clock scene)))
    (if (clock-paused-p clock)
        (setf (clock-time-scale clock) (scene-resume-speed scene))
        (setf (scene-resume-speed scene) (clock-time-scale clock)
              (clock-time-scale clock) 0))))

(defun change-battle-speed (scene direction)
  "Passe à la vitesse suivante (DIRECTION 1) ou précédente (-1) de
*BATTLE-SPEEDS*. En pause, change la vitesse de reprise sans reprendre."
  (let* ((speed (battle-speed scene))
         (next (if (plusp direction)
                   (find-if (lambda (s) (> s speed)) *battle-speeds*)
                   (find-if (lambda (s) (< s speed)) *battle-speeds* :from-end t))))
    (when next
      (if (battle-paused-p scene)
          (setf (scene-resume-speed scene) next)
          (setf (clock-time-scale (scene-clock scene)) next)))))

;;; Mise à jour

(defmethod scene-update ((scene battle-scene) dt)
  (cond ((key-pressed-p :key-escape)
         (change-scene *state* 'main-menu))
        (t
         (when (key-pressed-p :key-space)
           (toggle-battle-pause scene))
         (when (or (key-pressed-p :key-equal) (key-pressed-p :key-kp-add))
           (change-battle-speed scene 1))
         (when (or (key-pressed-p :key-minus) (key-pressed-p :key-kp-subtract))
           (change-battle-speed scene -1))
         (let ((battle (scene-battle scene)))
           (dotimes (i (clock-consume (scene-clock scene) dt))
             (battle-tick battle))))))

;;; Dessin

(defun battle-speed-text (scene)
  "Vitesse affichée, reconstruite seulement quand la vitesse ou la pause change."
  (let ((speed (battle-speed scene))
        (paused (battle-paused-p scene))
        (key (scene-speed-key scene)))
    (unless (and key (eql (car key) speed) (eq (cdr key) paused))
      (setf (scene-speed-key scene) (cons speed paused)
            (scene-speed-text scene)
            (format nil "x~a~:[~;   En pause (Espace)~]" (format-speed speed) paused)))
    (scene-speed-text scene)))

(defun draw-battle-hud (scene)
  "« Tick 1234   x2 » en haut à gauche, sans allouer à chaque tick."
  (let* ((size 20)
         (digits (write-integer-into (battle-tick-count (scene-battle scene))
                                     (scene-tick-text scene)))
         (x (+ 10 (measure-text "Tick " size))))
    (draw-text "Tick " 10 10 size :lightgray)
    (draw-text digits x 10 size :lightgray)
    (draw-text (battle-speed-text scene) (+ x (measure-text digits size) 30) 10 size :lightgray)))

(defun format-speed (speed)
  "0.25 -> \"0,25\", 2 -> \"2\"."
  (substitute #\, #\. (string-right-trim "." (string-right-trim "0" (format nil "~,2f" speed)))))

(defmethod scene-draw ((scene battle-scene))
  (clear-background :black)
  (draw-battle-hud scene)
  (draw-text "Espace : pause   + / - : vitesse   Échap : menu"
             10 (- *window-height* 26) 16 :darkgray))

;;; Inspection

(defmethod scene-snapshot ((scene battle-scene))
  (let ((clock (scene-clock scene))
        (snapshot (battle-snapshot (scene-battle scene))))
    (setf (gethash "type" snapshot) "battle-scene"
          (gethash "time_scale" snapshot) (float (clock-time-scale clock) 1d0)
          (gethash "resume_speed" snapshot) (float (scene-resume-speed scene) 1d0)
          (gethash "alpha" snapshot) (float (clock-alpha clock) 1d0)
          (gethash "dropped_seconds" snapshot) (float (clock-dropped clock) 1d0))
    snapshot))
