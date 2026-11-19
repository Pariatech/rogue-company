(in-package #:rogue-company)

;;; Scène de bataille : fait avancer la simulation (battle.lisp) au rythme de son
;;; horloge à pas fixe, lit les commandes et dessine.
;;;
;;; Espace : un clic bref (moins de *SPACE-TAP-SECONDS*) suspend ou reprend la
;;; simulation ; tenu, il la suspend le temps d'y jeter un œil (l'aperçu des
;;; ordres, command.lisp) et la reprend au relâcher. + et - changent sa vitesse, R donne
;;; un nouvel ordre au hasard à chaque régiment, C lance chaque régiment sur
;;; l'ennemi le plus proche, Échap ramène au menu principal.
;;; Cette pause est propre à la bataille : celle du jeu (console, game_control)
;;; arrête aussi la lecture du clavier. La caméra (camera.lisp) bouge même
;;; quand la bataille est suspendue.

(defparameter *space-tap-seconds* 0.25
  "Espace relâché avant ce délai : un clic, qui bascule la pause. Au-delà, il
était tenu : la bataille reprend l'état d'avant.")

(defparameter *end-screen-seconds* 3
  "Secondes de bataille entre la décision (victory.lisp) et l'écran de fin : on
voit l'armée brisée se débander.")

(defparameter *battle-speeds* '(0.25 0.5 1 2 4 8)
  "Vitesses proposées par + et -, en ordre croissant.")

(defclass battle-scene (scene)
  ;; Des initforms plutôt que SCENE-ENTER : une scène neuve a une bataille neuve,
  ;; et une instance existante reçoit ses nouveaux slots au rechargement.
  ((battle :initform (make-demo-battle :seed *demo-seed*) :reader scene-battle)
   (orders-rng :initform (make-rng 7 99) :reader scene-orders-rng
               :documentation "Tire les ordres de R, sans toucher au générateur de la bataille.")
   (clock :initform (make-fixed-clock) :reader scene-clock)
   (camera :initform (make-camera) :reader scene-camera)
   (command :initform (make-instance 'player-command) :reader scene-command
            :documentation "Sélection et ordres du joueur (command.lisp).")
   (tally :initform (make-tally) :reader scene-tally
          :documentation "Tableau des régiments (tally-view.lisp).")
   (space-held :initform nil :accessor scene-space-held
               :documentation "Secondes depuis qu'Espace est enfoncé, ou NIL s'il ne l'est pas.")
   (space-was-paused :initform nil :accessor scene-space-was-paused
                     :documentation "La bataille était-elle en pause quand Espace a été enfoncé ?")
   (resume-speed :initform 1 :accessor scene-resume-speed
                 :documentation "Vitesse retrouvée quand on reprend après Espace.")
   (tick-text :initform (make-text-buffer) :reader scene-tick-text
              :documentation "Chiffres du tick, réécrits à chaque image sans allouer.")
   (speed-text :initform "" :accessor scene-speed-text)
   (end-screen :initform nil :accessor scene-end-screen
               :documentation "Bilan de l'écran de fin (END-SCREEN), figé à son affichage.")
   (saved :initform nil :accessor scene-saved-p
          :documentation "Le replay de la bataille décidée est écrit (replay.lisp).")
   (speed-key :initform nil :accessor scene-speed-key
              :documentation "Vitesse et pause de SPEED-TEXT : le texte n'est reconstruit
que s'ils changent, ce qui est rare."))
  (:documentation "La bataille de démonstration, son tick et sa vitesse."))

(defclass ai-battle-scene (battle-scene)
  ((battle :initform (make-ai-battle :seed *demo-seed*)))
  (:documentation "Bataille rangée : le joueur mène les rouges, l'IA les bleus (ai.lisp)."))

(defclass replay-scene (battle-scene)
  ((battle :initform nil)
   (replay :initform nil :accessor scene-replay
           :documentation "Replay revu (replay.lisp), ou NIL s'il n'y en a pas.")
   (cursor :initform 0 :accessor scene-cursor :documentation "Prochain ordre du replay.")
   (status :initform nil :accessor scene-replay-status
           :documentation ":MATCH ou :DESYNC une fois le tick de l'enregistrement atteint."))
  (:documentation "La dernière bataille, revue : ses ordres rejoués à leur tick,
sans commandement. Vitesse, pause et caméra restent libres."))

(defmethod initialize-instance :after ((scene replay-scene) &key)
  (let ((replay (handler-case (load-replay)
                  (error (e) (warn "Replay illisible : ~a" e) nil))))
    (setf (scene-replay scene) replay
          (slot-value scene 'battle) (if replay (replay-start replay) (make-battle)))))

(defclass ai-vs-ai-scene (battle-scene)
  ((battle :initform (make-ai-battle :seed *demo-seed* :sides '(0 1))))
  (:documentation "Bataille rangée entre deux IA, à regarder : le joueur ne
commande rien. Le banc d'essai des stratégies (ai-bench.lisp) joue les mêmes
batailles sans fenêtre."))

(defgeneric scene-live-p (scene)
  (:documentation "Vrai pour une bataille qui se joue : faux pour un replay. Une
bataille qui se joue écrit son replay.")
  (:method ((scene battle-scene)) t)
  (:method ((scene replay-scene)) nil))

(defgeneric scene-player-p (scene)
  (:documentation "Vrai si le joueur commande : ni en replay, ni entre deux IA.")
  (:method ((scene battle-scene)) t)
  (:method ((scene replay-scene)) nil)
  (:method ((scene ai-vs-ai-scene)) nil))

(defgeneric advance-battle (scene)
  (:documentation "Un tick de la bataille de SCENE.")
  (:method ((scene battle-scene))
    (battle-tick (scene-battle scene)))
  (:method ((scene replay-scene))
    (let ((replay (scene-replay scene)))
      (if replay
          (multiple-value-bind (cursor status)
              (replay-tick (scene-battle scene) replay (scene-cursor scene))
            (setf (scene-cursor scene) cursor)
            (when status (setf (scene-replay-status scene) status)))
          (battle-tick (scene-battle scene))))))

(defmethod scene-exit ((scene battle-scene))
  ;; La dernière bataille jouée, à revoir depuis le menu.
  (when (and (scene-live-p scene) (plusp (battle-tick-count (scene-battle scene))))
    (save-replay (scene-battle scene))))

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

(defun update-space (scene dt)
  "Espace enfoncé : pause aussitôt. Relâché, un clic bref garde la pause (ou en
sort si elle y était déjà) ; tenu plus longtemps, la bataille revient à l'état
d'avant."
  (let ((down (key-down-p :key-space))
        (held (scene-space-held scene)))
    (cond ((and down (null held))
           (setf (scene-space-was-paused scene) (battle-paused-p scene)
                 (scene-space-held scene) 0f0)
           (unless (battle-paused-p scene) (toggle-battle-pause scene)))
          (down
           (incf (scene-space-held scene) (float dt 1f0)))
          (held
           (setf (scene-space-held scene) nil)
           ;; Clic bref depuis la pause, ou tenu depuis le jeu : on reprend.
           (when (and (eq (< held *space-tap-seconds*) (scene-space-was-paused scene))
                      (battle-paused-p scene))
             (toggle-battle-pause scene))))))

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

(defun end-screen-p (scene)
  "Vrai quand la bataille est décidée depuis *END-SCREEN-SECONDS*."
  (let ((battle (scene-battle scene)))
    (and (battle-outcome battle)
         (>= (- (battle-tick-count battle) (battle-outcome-tick battle))
             (* *end-screen-seconds* +ticks-per-second+)))))

(defmethod scene-update ((scene battle-scene) dt)
  (cond ((key-pressed-p :key-escape)
         (change-scene *state* 'main-menu))
        ((and (end-screen-p scene)
              (or (key-pressed-p :key-enter) (key-pressed-p :key-kp-enter)))
         ;; Rejouer : la même bataille, neuve (un replay : revu depuis le début).
         (change-scene *state* (class-name (class-of scene))))
        ((and (end-screen-p scene) (scene-live-p scene) (key-pressed-p :key-v))
         ;; Revoir la bataille qui vient de finir (SCENE-EXIT l'écrit).
         (change-scene *state* 'replay-scene))
        (t
         (when (and (scene-live-p scene) (end-screen-p scene) (not (scene-saved-p scene)))
           (save-replay (scene-battle scene))
           (setf (scene-saved-p scene) t))
         (update-space scene dt)
         (when (or (key-pressed-p :key-equal) (key-pressed-p :key-kp-add))
           (change-battle-speed scene 1))
         (when (or (key-pressed-p :key-minus) (key-pressed-p :key-kp-subtract))
           (change-battle-speed scene -1))
         (when (and (scene-player-p scene) (key-pressed-p :key-r))
           (loop for order in (random-move-orders (scene-battle scene) (scene-orders-rng scene))
                 do (player-order (scene-battle scene) (cons :place order))))
         (when (and (scene-player-p scene) (key-pressed-p :key-c))
           (player-order (scene-battle scene) (list :charge *demo-charge-sides*)))
         (when (key-pressed-p :key-tab)
           (setf *draw-tally* (not *draw-tally*)))
         (when (key-pressed-p :key-i)
           (setf *draw-ai-plan* (not *draw-ai-plan*)))
         (when (and (scene-player-p scene) (not (end-screen-p scene)))
           (update-command (scene-command scene) (scene-battle scene) (scene-camera scene)))
         (update-camera (scene-camera scene) dt)
         (dotimes (i (clock-consume (scene-clock scene) dt))
           (advance-battle scene)))))

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
  (let* ((size (text-size 20))
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
  (draw-soldiers (scene-battle scene) (clock-alpha (scene-clock scene)) (scene-camera scene))
  (draw-battle-hud scene)
  (draw-ai-plan (scene-battle scene) (scene-camera scene))
  (draw-tally (scene-tally scene) (scene-battle scene))
  ;; En pause, ou Espace tenu : les ordres en cours, fantômes et trajets.
  (draw-command (scene-command scene) (scene-battle scene) (scene-camera scene)
                (or (battle-paused-p scene) (key-down-p :key-space)))
  (draw-replay-banner scene)
  (when (end-screen-p scene)
    (draw-end-screen scene))
  (draw-text (help-text scene) 10 (- *window-height* (text-size 16) 10) (text-size 16) :darkgray))

(defgeneric help-text (scene)
  (:method ((scene battle-scene))
    "Clic gauche : choisir   Clic droit : ordre (glisser : orientation)   T : halte   E : exercice   P : posture   Espace : pause (tenu : coup d'œil)   + / - : vitesse   C : charge   Tab : tableau   I : plan de l'IA   Flèches, ZQSD, clic milieu : caméra   Échap : menu")
  (:method ((scene ai-vs-ai-scene))
    "IA contre IA : Espace : pause (tenu : coup d'œil)   + / - : vitesse   Tab : tableau   I : plan de l'IA   Flèches, ZQSD, clic milieu : caméra   Échap : menu")
  (:method ((scene replay-scene))
    "Revoir : Espace : pause (tenu : coup d'œil)   + / - : vitesse   Tab : tableau   I : plan de l'IA   Flèches, ZQSD, clic milieu : caméra   Échap : menu"))

(defgeneric draw-replay-banner (scene)
  (:documentation "En haut au centre, pour un replay : ce qu'on regarde, et s'il
s'est désynchronisé.")
  (:method ((scene battle-scene)))
  (:method ((scene replay-scene))
    (let* ((replay (scene-replay scene))
           (size (text-size 24))
           (cx (floor *window-width* 2))
           (y (+ 10 (text-size 20))))
      (cond ((null replay)
             (draw-text-centered "Aucune bataille enregistrée" cx y size :lightgray))
            ((eq (scene-replay-status scene) :desync)
             (draw-text-centered "Revoir : désynchronisé, la simulation a changé depuis l'enregistrement"
                                 cx y size '(230 80 70 255)))
            ((> (battle-tick-count (scene-battle scene)) (replay-end-tick replay))
             (draw-text-centered "Revoir : fin de l'enregistrement, la bataille continue sans ordres"
                                 cx y size :lightgray))
            (t (draw-text-centered "Revoir la dernière bataille" cx y size :gold))))))

;;; Écran de fin

(defun side-label (battle side)
  "« Rouges (vous) », « Bleus (IA) »."
  (format nil "~a~a" (if (zerop side) "Rouges" "Bleus")
          (cond ((find side (battle-commanders battle) :key #'commander-side) " (IA)")
                ((= side *player-side*) " (vous)")
                (t ""))))

(defun end-screen (battle)
  "Bilan de la bataille décidée : plist :TITLE, :COLOR, :SUBTITLE, :DURATION,
:ROWS (en-tête puis une ligne par mesure, une colonne par camp)."
  (let* ((outcome (battle-outcome battle))
         (sides (army-sides battle))
         (summaries (mapcar (lambda (side) (army-summary battle side)) sides))
         (seconds (floor (battle-outcome-tick battle) +ticks-per-second+)))
    (flet ((row (label fn)
             (cons label (mapcar fn summaries)))
           (spectator-p ()
             ;; Le camp du joueur est mené par l'IA : il regarde, sans camp.
             (find *player-side* (battle-commanders battle) :key #'commander-side)))
      (list :title (cond ((eq outcome :draw) "Match nul")
                         ((spectator-p) (if (zerop outcome) "Victoire des rouges" "Victoire des bleus"))
                         ((= outcome *player-side*) "Victoire")
                         (t "Défaite"))
            :color (cond ((eq outcome :draw) :lightgray)
                         ((spectator-p) (side-color outcome))
                         ((= outcome *player-side*) :gold)
                         (t '(230 80 70 255)))
            :subtitle (cond ((eq outcome :draw) "Les deux armées sont brisées.")
                            ((spectator-p) (if (zerop outcome)
                                               "L'armée bleue est brisée."
                                               "L'armée rouge est brisée."))
                            ((= outcome *player-side*) "L'armée ennemie est brisée.")
                            (t "Votre armée est brisée."))
            :duration (format nil "Durée : ~d min ~2,'0d s" (floor seconds 60) (mod seconds 60))
            :rows (list (cons "" (mapcar (lambda (side) (side-label battle side)) sides))
                        (row "Hommes" (lambda (s) (format nil "~d / ~d" (getf s :alive) (getf s :start))))
                        (row "Pertes" (lambda (s) (format nil "~d" (- (getf s :start) (getf s :alive)))))
                        (row "Ennemis tués" (lambda (s) (format nil "~d" (getf s :kills))))
                        (row "Régiments brisés"
                             (lambda (s) (format nil "~d / ~d" (getf s :routed) (getf s :regiments)))))))))

(defun draw-end-screen (scene)
  "Panneau du bilan au centre de l'écran, par-dessus la bataille qui continue."
  (let* ((screen (or (scene-end-screen scene)
                     (setf (scene-end-screen scene) (end-screen (scene-battle scene)))))
         (rows (getf screen :rows))
         (big (text-size 64)) (size (text-size 26)) (small (text-size 18))
         (line (round (* size 1.6)))
         (pad (* 2 size))
         (widths (loop for column below (length (first rows))
                       collect (+ pad (loop for row in rows maximize (measure-text (nth column row) size)))))
         (table-width (reduce #'+ widths))
         (footer (if (scene-live-p scene)
                     "Entrée : rejouer      V : revoir      Échap : menu"
                     "Entrée : revoir encore      Échap : menu"))
         (width (+ pad (max table-width (measure-text (getf screen :title) big)
                            (measure-text (getf screen :subtitle) size))))
         (height (+ size big (round size 2) (* 2 line) (round line 2) (* line (length rows)) line small size))
         (cx (floor *window-width* 2))
         (top (floor (- *window-height* height) 2))
         (y (+ top size)))
    (draw-rectangle (- cx (floor width 2)) top width height '(0 0 0 210))
    (draw-text-centered (getf screen :title) cx y big (getf screen :color))
    (incf y (+ big (round size 2)))
    (draw-text-centered (getf screen :subtitle) cx y size :white)
    (incf y line)
    (draw-text-centered (getf screen :duration) cx y size :lightgray)
    (incf y (+ line (round line 2)))
    (loop for row in rows
          for header = t then nil
          do (loop for cell in row
                   for cell-width in widths
                   for x = (- cx (floor table-width 2)) then (+ x previous)
                   for previous = cell-width
                   for first = t then nil
                   do (draw-text cell
                                 (if first
                                     (+ x (floor pad 2))
                                     (+ x (floor (- cell-width (measure-text cell size)) 2)))
                                 y size (cond (header :gray) (first :lightgray) (t :white))))
             (incf y line))
    (incf y (round line 2))
    (draw-text-centered footer cx y small :gray)))

;;; Inspection

(defmethod scene-snapshot ((scene battle-scene))
  (let ((clock (scene-clock scene))
        (snapshot (battle-snapshot (scene-battle scene))))
    (setf (gethash "type" snapshot) "battle-scene"
          (gethash "time_scale" snapshot) (float (clock-time-scale clock) 1d0)
          (gethash "resume_speed" snapshot) (float (scene-resume-speed scene) 1d0)
          (gethash "alpha" snapshot) (float (clock-alpha clock) 1d0)
          (gethash "dropped_seconds" snapshot) (float (clock-dropped clock) 1d0)
          (gethash "camera" snapshot) (camera-snapshot (scene-camera scene))
          (gethash "outcome" snapshot) (let ((outcome (battle-outcome (scene-battle scene))))
                                         (if (symbolp outcome) (string-downcase (or outcome "")) outcome)))
    snapshot))
