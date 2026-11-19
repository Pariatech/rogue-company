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
