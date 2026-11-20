(in-package #:rogue-company)

;;; Banc d'essai des stratégies de l'IA : des batailles IA contre IA jouées sans
;;; fenêtre (AI-MATCH). Deux réglages de commandant, A et B (plists *AI-…* ->
;;; valeur, COMMANDER-SETTINGS), s'affrontent sur chaque graine deux fois, en
;;; échangeant les camps : l'asymétrie de la carte et l'ordre des camps
;;; s'annulent. Exemple, la couche stratégique contre le choix glouton seul :
;;;
;;;   (ai-match :a '() :b '(*ai-use-plan* nil) :seeds '(1 2 3))

(defparameter *ai-match-seconds* 600
  "Durée maximale d'une bataille du banc d'essai, en secondes de bataille.")

(defun ai-match-game (seed a b a-side &key (regiments *ai-battle-regiments*)
                                           (seconds *ai-match-seconds*))
  "Une bataille, A commandant le camp A-SIDE et B l'autre. Renvoie une plist :
:SEED, :A-SIDE, :WINNER (:A, :B, :DRAW, ou NIL si rien n'est décidé),
:SECONDS, :A-LOSSES, :B-LOSSES (hommes perdus)."
  (let* ((b-side (- 1 a-side))
         (battle (make-ai-battle :seed seed :regiments regiments :sides '(0 1)
                                 :settings (list (cons a-side a) (cons b-side b)))))
    (loop repeat (* seconds +ticks-per-second+)
          until (battle-outcome battle)
          do (battle-tick battle))
    (let ((outcome (battle-outcome battle)))
      (flet ((losses (side)
               (let ((summary (army-summary battle side)))
                 (- (getf summary :start) (getf summary :alive)))))
        (list :seed seed :a-side a-side
              :winner (cond ((null outcome) nil)
                            ((eq outcome :draw) :draw)
                            ((= outcome a-side) :a)
                            (t :b))
              :seconds (floor (battle-tick-count battle) +ticks-per-second+)
              :a-losses (losses a-side) :b-losses (losses b-side))))))

(defun ai-match (&key a b (seeds '(1 2 3)) (regiments *ai-battle-regiments*)
                      (seconds *ai-match-seconds*))
  "Réglages A contre réglages B, sur chaque graine de SEEDS, chacun dans les deux
camps. Renvoie une plist : :A-WINS, :B-WINS, :DRAWS, :UNDECIDED, :A-LOSSES et
:B-LOSSES (totales), :GAMES (la plist de chaque bataille, AI-MATCH-GAME)."
  (let ((games (loop for seed in seeds
                     nconc (loop for a-side in '(0 1)
                                 collect (ai-match-game seed a b a-side :regiments regiments
                                                                        :seconds seconds)))))
    (flet ((wins (winner) (count winner games :key (lambda (g) (getf g :winner))))
           (total (key) (reduce #'+ games :key (lambda (g) (getf g key)))))
      (list :a-wins (wins :a) :b-wins (wins :b) :draws (wins :draw) :undecided (wins nil)
            :a-losses (total :a-losses) :b-losses (total :b-losses)
            :games games))))

(defun print-ai-match (result &optional (stream *standard-output*))
  "Tableau d'un résultat d'AI-MATCH."
  (format stream "~&A : ~d victoires, B : ~d, nuls : ~d, indécises : ~d ; pertes A ~d, B ~d~%"
          (getf result :a-wins) (getf result :b-wins) (getf result :draws)
          (getf result :undecided) (getf result :a-losses) (getf result :b-losses))
  (dolist (game (getf result :games))
    (format stream "  graine ~2d, A en ~:[bleu~;rouge~] : ~(~a~) en ~d s (pertes A ~d, B ~d)~%"
            (getf game :seed) (zerop (getf game :a-side))
            (or (getf game :winner) "rien") (getf game :seconds)
            (getf game :a-losses) (getf game :b-losses)))
  result)

;;; Adversaire scripté : les ordres d'un joueur, tirés d'un replay contre l'IA,
;;; redonnés à leur tick contre une autre IA (AI-SCRIPT-MATCH). L'IA y affronte
;;; une manière de jouer humaine plutôt qu'elle-même. Les ordres sont en boucle
;;; ouverte (une marche va au même point quoi que fasse l'ennemi), mais une
;;; attaque suit sa cible. Pour jouer l'autre camp, les ordres sont mis en
;;; miroir (MIRROR-ORDER) : armées symétriques par rapport au centre.

(defclass script-commander ()
  ((side :initarg :side :reader commander-side :type fixnum)
   (orders :initarg :orders :reader script-orders
           :documentation "Vecteur de (tick . ordre), comme REPLAY-ORDERS.")
   (cursor :initform 0 :accessor script-cursor))
  (:documentation "Commandant qui redonne des ordres enregistrés (EXECUTE-ORDER)."))

(defun script-order-valid-p (battle spec)
  "Vrai si l'ordre SPEC a encore un sens : ses régiments et sa cible vivent, et
ses régiments ne sont pas débandés."
  (let ((regiments (battle-regiments battle)))
    (flet ((alive (k) (plusp (regiment-alive (aref regiments k))))
           (fit (k) (and (plusp (regiment-alive (aref regiments k)))
                         (not (regiment-routed-p (aref regiments k))))))
      (destructuring-bind (kind &rest args) spec
        (case kind
          ((:move :halt) (every #'fit (first args)))
          (:attack (and (every #'fit (first args)) (alive (second args))))
          ((:drill :stance :place) (fit (first args)))
          (t nil))))))

(defmethod update-commander ((commander script-commander) battle)
  (let ((orders (script-orders commander))
        (tick (battle-tick-count battle)))
    (loop while (and (< (script-cursor commander) (length orders))
                     (<= (car (aref orders (script-cursor commander))) tick))
          do (let ((spec (cdr (aref orders (script-cursor commander)))))
               (when (script-order-valid-p battle spec)
                 (execute-order battle spec))
               (incf (script-cursor commander))))))

(defun mirror-index (regiments index)
  "Indice du régiment symétrique de INDEX par rapport au centre, dans la liste
d'entrées REGIMENTS (camp arme effectif x y orientation …) : l'autre camp, la
même arme, à (-x, -y)."
  (destructuring-bind (side kind count x y &rest rest) (nth index regiments)
    (declare (ignore count rest))
    (or (position-if (lambda (entry)
                       (destructuring-bind (s k c ex ey &rest r) entry
                         (declare (ignore c r))
                         (and (/= s side) (eq k kind) (= ex (- x)) (= ey (- y)))))
                     regiments)
        (error "Pas de régiment symétrique de ~d." index))))

(defun mirror-order (regiments spec)
  "L'ordre SPEC donné à l'armée symétrique : indices échangés, points et
orientations tournés d'un demi-tour autour du centre."
  (flet ((idx (k) (mirror-index regiments k))
         (turn (facing) (and facing (float (+ facing pi) 1f0))))
    (destructuring-bind (kind &rest args) spec
      (ecase kind
        (:move (destructuring-bind (indices x y facing) args
                 (list :move (mapcar #'idx indices) (- x) (- y) (turn facing))))
        (:attack (destructuring-bind (indices target) args
                   (list :attack (mapcar #'idx indices) (idx target))))
        (:halt (list :halt (mapcar #'idx (first args))))
        ((:drill :stance) (list kind (idx (first args)) (second args)))
        (:place (destructuring-bind (index x y facing) args
                  (list :place (idx index) (- x) (- y) (turn facing))))))))

(defun replay-player-side (replay)
  "Camp du joueur dans REPLAY, une bataille contre l'IA : celui que l'IA ne
commande pas."
  (let ((sides (getf (rest (replay-recipe replay)) :sides)))
    (find-if-not (lambda (side) (member side sides)) '(0 1))))

(defun ai-script-game (seed a replay script-side &key (seconds *ai-match-seconds*))
  "Une bataille : les ordres du joueur de REPLAY redonnés au camp SCRIPT-SIDE
(en miroir si ce n'est pas son camp), l'IA de réglages A dans l'autre. Renvoie
une plist comme AI-MATCH-GAME, A étant l'IA et B le script."
  (let* ((recipe (rest (replay-recipe replay)))
         (regiments (getf recipe :regiments))
         (ai-side (- 1 script-side))
         (mirror (/= script-side (replay-player-side replay)))
         (orders (map 'vector (lambda (entry)
                                (cons (car entry)
                                      (if mirror (mirror-order regiments (cdr entry)) (cdr entry))))
                      (replay-orders replay)))
         (battle (make-ai-battle :seed seed :regiments regiments :sides (list ai-side)
                                 :settings (list (cons ai-side a)))))
    ;; Le script d'abord : un replay exécute les ordres du joueur avant le tick.
    (push (make-instance 'script-commander :side script-side :orders orders)
          (battle-commanders battle))
    (loop repeat (* seconds +ticks-per-second+)
          until (battle-outcome battle)
          do (battle-tick battle))
    (let ((outcome (battle-outcome battle)))
      (flet ((losses (side)
               (let ((summary (army-summary battle side)))
                 (- (getf summary :start) (getf summary :alive)))))
        (list :seed seed :a-side ai-side
              :winner (cond ((null outcome) nil)
                            ((eq outcome :draw) :draw)
                            ((= outcome ai-side) :a)
                            (t :b))
              :seconds (floor (battle-tick-count battle) +ticks-per-second+)
              :a-losses (losses ai-side) :b-losses (losses script-side)
              :battle battle)))))

(defun player-replays ()
  "Les batailles de joueurs gardées pour le banc d'essai : les fichiers
joueur-*.sexp de *REPLAY-DIRECTORY* (copies de la dernière bataille,
derniere.sexp, que chaque bataille écrase)."
  (mapcar #'load-replay
          (sort (directory (merge-pathnames "joueur-*.sexp" *replay-directory*))
                #'string< :key #'namestring)))

(defun ai-script-match (&key a (replays (or (player-replays) (list (load-replay))))
                             (seeds '(1 2 3)) (seconds *ai-match-seconds*))
  "L'IA de réglages A contre les ordres de chaque joueur de REPLAYS, sur chaque
graine de SEEDS, l'IA dans chaque camp. Résultat comme AI-MATCH (A : l'IA, B :
le joueur scripté), à afficher par PRINT-AI-MATCH."
  (let ((games (loop for replay in replays
                     nconc (loop for seed in seeds
                                 nconc (loop for script-side in '(0 1)
                                             collect (ai-script-game seed a replay script-side
                                                                     :seconds seconds))))))
    (flet ((wins (winner) (count winner games :key (lambda (g) (getf g :winner))))
           (total (key) (reduce #'+ games :key (lambda (g) (getf g key)))))
      (list :a-wins (wins :a) :b-wins (wins :b) :draws (wins :draw) :undecided (wins nil)
            :a-losses (total :a-losses) :b-losses (total :b-losses)
            :games games))))
