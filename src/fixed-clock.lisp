(in-package #:rogue-company)

;;; Horloge à pas fixe. À chaque image, la scène lui passe le temps réel écoulé
;;; et reçoit le nombre de ticks de simulation à exécuter ; le reste s'accumule
;;; pour l'image suivante. La simulation ne voit jamais le temps réel, seulement
;;; des ticks de durée constante : elle reste déterministe.
;;;
;;; TIME-SCALE accélère (2), ralentit (0,5) ou suspend (0) la simulation sans
;;; toucher au reste du jeu : la scène continue d'être dessinée et de lire le
;;; clavier.
;;;
;;; L'accumulateur est un single-float, valeur immédiate sous SBCL 64 bits : le
;;; ranger dans un slot n'alloue rien, contrairement à un double-float.

(defparameter *max-time-scale* 16f0)

(defclass fixed-clock ()
  ((tick-dt :initarg :tick-dt :reader clock-tick-dt
            :documentation "Durée d'un tick, en secondes de simulation.")
   (max-ticks :initarg :max-ticks :accessor clock-max-ticks
              :documentation "Ticks au plus par image à vitesse 1. Au-delà, le retard est
abandonné plutôt que rattrapé, pour qu'une image lente n'en ralentisse pas d'autres.")
   (time-scale :initform 1f0 :reader clock-time-scale
               :documentation "Secondes de simulation par seconde réelle. 0 : en pause.")
   (accumulator :initform 0f0 :accessor clock-accumulator
                :documentation "Temps de simulation pas encore consommé, en secondes.")
   (dropped :initform 0f0 :accessor clock-dropped
            :documentation "Temps de simulation abandonné par le plafond, en secondes.")))

(defun make-fixed-clock (&key (tick-rate 30) (max-ticks 5) (time-scale 1f0))
  "Horloge de TICK-RATE ticks par seconde de simulation."
  (let ((clock (make-instance 'fixed-clock :tick-dt (/ 1f0 tick-rate) :max-ticks max-ticks)))
    (setf (clock-time-scale clock) time-scale)
    clock))

(defun (setf clock-time-scale) (scale clock)
  "Règle la vitesse de la simulation : 1 normale, 2 deux fois plus vite, 0,5 deux
fois plus lent, 0 en pause."
  (unless (and (realp scale) (<= 0 scale *max-time-scale*))
    (error "time-scale doit être compris entre 0 et ~a, pas ~s." *max-time-scale* scale))
  (setf (slot-value clock 'time-scale) (float scale 1f0)))

(defun clock-paused-p (clock)
  (zerop (clock-time-scale clock)))

(defun clock-consume (clock dt)
  "Ajoute DT secondes réelles, multipliées par la vitesse, et renvoie le nombre
de ticks à exécuter maintenant. Le plafond suit la vitesse : à 4, jusqu'à 4 fois
MAX-TICKS ticks par image."
  (let* ((tick-dt (clock-tick-dt clock))
         (scale (clock-time-scale clock))
         (limit (* (clock-max-ticks clock) (max 1 (ceiling scale))))
         (accumulator (+ (clock-accumulator clock) (* (float dt 1f0) scale)))
         (ticks 0))
    (declare (type single-float tick-dt scale accumulator)
             (type fixnum limit ticks))
    (loop while (>= accumulator tick-dt)
          do (decf accumulator tick-dt)
             (if (< ticks limit)
                 (incf ticks)
                 (incf (clock-dropped clock) tick-dt)))
    (setf (clock-accumulator clock) accumulator)
    ticks))

(defun clock-alpha (clock)
  "Part du tick suivant déjà écoulée, dans [0, 1[ : sert à interpoler le dessin
entre l'état du tick précédent et celui du tick courant."
  (min (/ (clock-accumulator clock) (clock-tick-dt clock))
       #.(- 1f0 single-float-epsilon)))

(defun clock-reset (clock)
  "Oublie le temps accumulé et abandonné, sans changer la vitesse."
  (setf (clock-accumulator clock) 0f0
        (clock-dropped clock) 0f0)
  clock)
