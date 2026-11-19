(in-package #:rogue-company)

;;; IA de l'armée adverse : un commandant par camp (AI-COMMANDER), appelé par
;;; BATTLE-TICK (UPDATE-COMMANDER, dans battle.lisp) et réveillé tous les
;;; *AI-PERIOD* ticks. Il donne à ses régiments les ordres du joueur
;;; (ORDER-ATTACK, ORDER-MOVE, SET-REGIMENT-DRILL), jamais rien de plus : il
;;; ne déplace aucun soldat. Il voit toute la bataille. Sans tirage : une
;;; bataille contre l'IA se rejoue à l'identique depuis sa graine.
;;;
;;; Chaque régiment a un rôle selon son arme (REGIMENT-ROLE) : piques, mêlée à
;;; pied, tireurs, cavalerie.
;;;
;;; 1. Réflexes : des piques passent en carré quand de la cavalerie ennemie libre
;;;    approche (*AI-SQUARE-DISTANCE*), et en reviennent quand elle s'éloigne.
;;; 2. Cibles : chaque paire (régiment, ennemi) reçoit une note (AI-SCORE) :
;;;    rapport des forces (REGIMENT-STRENGTH) × avantage du rôle sur l'autre
;;;    (*AI-MATCHUPS*) × côté abordé (flanc, dos) × occasion (ennemi déjà en
;;;    mêlée, moral bas) × proximité (temps de marche) ; de la cavalerie ou de
;;;    la mêlée devant des piques de face, un carré ou des tireurs couverts par
;;;    leurs piques notent bas. La proximité pèse lourd : l'ennemi le plus proche
;;;    passe avant un meilleur combat lointain. Affectation gloutonne, meilleure
;;;    paire d'abord, chaque régiment déjà affecté à un ennemi rabattant la note
;;;    des suivants (*AI-CROWDING*). Si même le combat, distance mise à part, ne
;;;    vaut pas le seuil de son rôle (*AI-THRESHOLDS*), un régiment attend à sa
;;;    place de départ, face à l'ennemi.
;;;    Le général (ai-general.lisp) donne d'abord une tâche aux piques, aux
;;;    tireurs et à la mêlée à pied (ai-tasks.lisp) : seuls les régiments sans
;;;    tâche (la cavalerie) passent par ce choix.
;;; 3. Stabilité : un régiment garde sa cible au moins *AI-COMMIT-SECONDS*, et
;;;    n'en change que pour une note *AI-SWITCH-MARGIN* fois meilleure. Aucun
;;;    ordre à un régiment en mêlée, à la charge, en repli de tirailleurs, en
;;;    poursuite ou débandé : le combat (riposte, cible combattue) s'en charge.

(defparameter *ai-period* 30 "Ticks entre deux réveils du commandant : une seconde.")

(defparameter *ai-use-plan* t
  "Vrai : le général répartit l'armée en groupes et tâches (ai-general.lisp). Faux :
chaque régiment choisit seul sa cible (le choix glouton), comme la première IA.
Un réglage de commandant (COMMANDER-SETTINGS) pour comparer les deux.")

(defparameter *ai-role-weights* '((:pikes . 1f0) (:foot . 1.6f0) (:shooters . 1f0) (:cavalry . 3.5f0))
  "Force d'un homme selon le rôle de son régiment : 225 piquiers, 105 rodeleros
et 60 cavaliers pèsent à peu près autant.")

(defparameter *ai-matchups*
  '((:cavalry (:shooters . 1.8f0) (:foot . 1.1f0) (:cavalry . 1f0) (:pikes . 1.5f0))
    (:foot (:shooters . 1.5f0) (:foot . 1f0) (:cavalry . 0.9f0) (:pikes . 0.8f0))
    (:pikes (:shooters . 1.3f0) (:foot . 1f0) (:cavalry . 1.4f0) (:pikes . 1f0))
    (:shooters (:pikes . 1.3f0) (:foot . 1f0) (:cavalry . 0.8f0) (:shooters . 1f0)))
  "Avantage d'un rôle (attaquant) sur un autre (défenseur), à forces égales.
Des piques chargées de face par de la cavalerie : *AI-PIKE-FRONT*.")

(defparameter *ai-pike-front* '((:cavalry . 0.2f0) (:foot . 0.7f0))
  "Avantage de la cavalerie et de la mêlée à pied sur des piques abordées de
face, ou en carré : elles volent la charge.")

(defparameter *ai-sector-bonus* #(1f0 1.4f0 1.7f0)
  "Avantage d'aborder l'ennemi de face, de flanc, de dos (pas pour les tireurs).")

(defparameter *ai-engaged-bonus* 1.3f0 "Avantage sur un ennemi déjà en mêlée avec un autre.")

(defparameter *ai-low-morale-bonus* 0.6f0
  "Avantage sur un ennemi au moral bas : 1 + ce facteur × la part de moral perdue.")

(defparameter *ai-screen-distance* 25f0
  "Des tireurs à moins de cette distance de leurs piques en bon ordre sont
couverts : la cavalerie et la mêlée à pied qui les visent les y trouveraient.")

(defparameter *ai-screened* 0.5f0 "Avantage sur des tireurs couverts par leurs piques.")

(defparameter *ai-march-seconds* 20f0
  "Proximité : une cible à T secondes de course compte pour
1 / (1 + T / *AI-MARCH-SECONDS*). Des piquiers notent des piques à 20 m (7 s)
0,75, de la cavalerie à 150 m (50 s) 0,29 : ils vont aux piques.")

(defparameter *ai-crowding* 0.6f0
  "Facteur sur la note d'un ennemi par régiment ami déjà affecté contre lui.")

(defparameter *ai-thresholds* '((:pikes . 0.5f0) (:shooters . 0.4f0) (:foot . 0.8f0) (:cavalry . 1f0))
  "Valeur minimale d'un combat (sa note, distance mise à part) pour attaquer,
selon le rôle : la cavalerie attend une occasion.")

(defparameter *ai-commit-seconds* 8 "Durée minimale d'un engagement avant de changer de cible.")

(defparameter *ai-switch-margin* 1.5f0
  "Une nouvelle cible doit noter ce facteur de plus que la cible gardée.")

(defparameter *ai-square-distance* 45f0
  "Des piques passent en carré quand de la cavalerie ennemie libre est à moins
de cette distance, de soldat à soldat.")

(defparameter *ai-unsquare-distance* 70f0
  "Elles quittent le carré quand aucune cavalerie ennemie libre n'est à moins de
cette distance.")

(defparameter *ai-home-slack* 3f0
  "Un régiment qui attend à moins de cette distance de sa place n'y est pas
renvoyé.")

(defclass ai-commander ()
  ((side :initarg :side :reader commander-side :type fixnum)
   (homes :initform (make-hash-table) :reader commander-homes
          :documentation "Indice de régiment -> (x y orientation) de départ.")
   (assignments :initform (make-hash-table) :reader commander-assignments
                :documentation "Indice de régiment -> (cible note tick) : sa cible, la
note qu'elle avait, et le tick où il l'a reçue.")
   (settings :initarg :settings :initform nil :reader commander-settings
             :documentation "Plist de réglages propres à ce commandant : variable *AI-…*
-> valeur, liées pendant son réveil. Deux camps peuvent ainsi suivre deux
stratégies (ai-bench.lisp).")
   (plan :initform nil :accessor commander-plan
         :documentation "Plan du général (ai-general.lisp), ou NIL avant le premier.")
   (attack-orders :initform (make-hash-table) :reader commander-attack-orders
                  :documentation "Indice de régiment -> ordres d'attaque donnés : pour
mesurer la stabilité."))
  (:documentation "Commandant d'un camp : rôles, réflexes, choix des cibles."))

(defun ai-setting-p (symbol)
  "Vrai si SYMBOL peut être réglé par commandant : une variable *AI-…* de ce
paquet. Un fichier de replay ne peut lier rien d'autre."
  (and (symbolp symbol)
       (eq (symbol-package symbol) (find-package '#:rogue-company))
       (let ((name (symbol-name symbol)))
         (and (> (length name) 4) (string= "*AI-" name :end2 4)
              (char= #\* (char name (1- (length name))))
              (boundp symbol)))))

(defun make-ai-commander (side &optional settings)
  (loop for (symbol) on settings by #'cddr
        unless (ai-setting-p symbol)
          do (error "Réglage d'IA refusé : ~s." symbol))
  (make-instance 'ai-commander :side side :settings settings))

;;; Rôles et forces

(defun regiment-role (regiment)
  "Rôle de REGIMENT selon son arme : :CAVALRY (monté), :SHOOTERS (arme de tir),
:PIKES (mêlée à portée minimale), :FOOT (autre mêlée)."
  (let* ((stats (regiment-stats regiment))
         (primary (stats-primary stats)))
    (cond ((getf (stats-loadout stats) :mount) :cavalry)
          ((eq (attack-kind primary) :ranged) :shooters)
          ((plusp (attack-min-reach primary)) :pikes)
          (t :foot))))

(defun morale-ratio (regiment)
  "Moral de REGIMENT en part de son moral de départ."
  (/ (regiment-morale regiment) (max 1f0 (stats-morale (regiment-stats regiment)))))

(defun regiment-strength (regiment)
  "Force estimée de REGIMENT : hommes × poids du rôle × moral × ordre."
  (* (regiment-alive regiment)
     (cdr (assoc (regiment-role regiment) *ai-role-weights*))
     (min 1.2f0 (morale-ratio regiment))
     (- 1f0 (* 0.5f0 (regiment-disorder regiment)))))

(defun fighting-p (regiment)
  "Vivant et pas débandé."
  (and (plusp (regiment-alive regiment)) (not (regiment-routed-p regiment))))

(defun free-cavalry-near-p (battle regiment distance)
  "Vrai si de la cavalerie ennemie libre (en état de combattre, pas en mêlée)
est à moins de DISTANCE de REGIMENT."
  (let ((soldiers (battle-soldiers battle)))
    (loop for other across (battle-regiments battle)
          thereis (and (/= (regiment-side other) (regiment-side regiment))
                       (fighting-p other)
                       (eq (regiment-role other) :cavalry)
                       (not (regiment-in-melee-p other))
                       (< (regiment-gap regiment other soldiers distance) distance)))))

(defun screened-p (battle shooters)
  "Vrai si SHOOTERS sont couverts par un régiment de piques ami en bon ordre, pas
en mêlée, à moins de *AI-SCREEN-DISTANCE*."
  (let ((soldiers (battle-soldiers battle)))
    (loop for other across (battle-regiments battle)
          thereis (and (= (regiment-side other) (regiment-side shooters))
                       (not (eq other shooters))
                       (fighting-p other)
                       (eq (regiment-role other) :pikes)
                       (not (regiment-in-melee-p other))
                       (< (regiment-gap shooters other soldiers *ai-screen-distance*)
                          *ai-screen-distance*)))))

;;; Notes

(defun approach-sector (regiment enemy soldiers)
  "Côté d'ENEMY qu'aborderait REGIMENT en venant de son centre : 0 de face, 1 de
flanc, 2 de dos. Un carré n'a que des faces."
  (if (regiment-square-p enemy)
      0
      (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
        (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
          (let ((facing (formation-facing (regiment-formation enemy))))
            (attack-sector (float (cos facing) 1f0) (float (sin facing) 1f0)
                           (float (- x ex) 1f0) (float (- y ey) 1f0)))))))

(defun ai-matchup (battle regiment enemy sector)
  "Avantage du rôle de REGIMENT sur celui d'ENEMY, abordé du côté SECTOR."
  (let* ((role (regiment-role regiment))
         (other (regiment-role enemy))
         (base (cdr (assoc other (cdr (assoc role *ai-matchups*)))))
         (pike-front (cdr (assoc role *ai-pike-front*))))
    (cond ((eq role :shooters) base)
          ((and pike-front (eq other :pikes) (or (zerop sector) (regiment-square-p enemy)))
           pike-front)
          ((and pike-front (eq other :shooters) (screened-p battle enemy))
           (* base *ai-screened* (svref *ai-sector-bonus* sector)))
          (t (* base (svref *ai-sector-bonus* sector))))))

(defun ai-score (battle regiment enemy)
  "Note de l'attaque de REGIMENT sur ENEMY : plus elle est haute, meilleure est
l'attaque ; 1 pour un combat égal tout proche. Second retour : sa valeur,
distance mise à part, comparée au seuil du rôle."
  (let* ((soldiers (battle-soldiers battle))
         (sector (approach-sector regiment enemy soldiers))
         (ratio (/ (regiment-strength regiment) (max 1f0 (regiment-strength enemy))))
         (opportunity (* (if (regiment-in-melee-p enemy) *ai-engaged-bonus* 1f0)
                         (+ 1f0 (* *ai-low-morale-bonus*
                                   (max 0f0 (- 1f0 (morale-ratio enemy)))))))
         (gap (regiment-gap regiment enemy soldiers 1000f0))
         (seconds (/ gap (max 0.5f0 (stats-run-speed (regiment-stats regiment)))))
         (proximity (/ 1f0 (+ 1f0 (/ seconds *ai-march-seconds*))))
         (worth (* (ai-matchup battle regiment enemy sector) ratio opportunity)))
    (values (* worth proximity) worth)))

;;; Décisions

(defun ai-free-p (regiment)
  "Vrai si le commandant peut donner un ordre à REGIMENT : en état de combattre,
ni en mêlée, ni à la charge, ni en repli, ni en poursuite."
  (and (fighting-p regiment)
       (not (regiment-in-melee-p regiment))
       (not (regiment-charging-p regiment))
       (not (regiment-chasing-p regiment))
       (not (eq (first (regiment-orders regiment)) :retreat))))

(defun update-squares (commander battle)
  "Réflexe des piques : carré devant la cavalerie, ligne quand elle s'éloigne."
  (loop for regiment across (battle-regiments battle)
        when (and (= (regiment-side regiment) (commander-side commander))
                  (fighting-p regiment)
                  (member :square (drill-choices regiment)))
          do (if (regiment-square-p regiment)
                 (unless (or (regiment-in-melee-p regiment)
                             (free-cavalry-near-p battle regiment *ai-unsquare-distance*))
                   (set-regiment-drill regiment nil))
                 (when (and (not (regiment-in-melee-p regiment))
                            (free-cavalry-near-p battle regiment *ai-square-distance*))
                   (set-regiment-drill regiment :square)))))

(defun current-target (regiment battle)
  "Régiment ennemi qu'attaque REGIMENT, s'il est encore en état de combattre."
  (let ((orders (regiment-orders regiment)))
    (when (eq (first orders) :attack)
      (let ((target (aref (battle-regiments battle) (second orders))))
        (and (fighting-p target) target)))))

(defun wait-at-home (commander battle regiment)
  "Fait attendre REGIMENT : à sa place de départ, face à l'ennemi, s'il n'a rien
à faire ; arrêté où il est si son ennemi vient de tomber."
  (let ((soldiers (battle-soldiers battle))
        (orders (regiment-orders regiment)))
    (if (eq (first orders) :attack)
        (command-halt battle (list regiment))
        (destructuring-bind (x y facing) (gethash (regiment-index regiment) (commander-homes commander))
          (multiple-value-bind (cx cy) (regiment-centroid regiment soldiers)
            (when (and (not (and (eq (first orders) :move)
                                 (< (abs (- (second orders) x)) 0.5f0)
                                 (< (abs (- (third orders) y)) 0.5f0)))
                       (> (sqrt (+ (expt (- cx x) 2) (expt (- cy y) 2))) *ai-home-slack*))
              (order-move regiment x y facing)))))))

(defun assign-targets (commander battle)
  "Affecte une cible à chaque régiment libre du camp, meilleures notes d'abord."
  (let* ((side (commander-side commander))
         (tick (battle-tick-count battle))
         (assignments (commander-assignments commander))
         (regiments (battle-regiments battle))
         (enemies (loop for other across regiments
                        when (and (/= (regiment-side other) side) (fighting-p other))
                          collect other))
         (crowd (make-hash-table))
         (pairs '())
         (decided (make-hash-table)))
    ;; Ennemis déjà visés par des régiments qui ne changeront pas : en mêlée, ou
    ;; qui gardent leur cible.
    (loop for regiment across regiments
          when (and (= (regiment-side regiment) side) (fighting-p regiment))
            do (let ((target (current-target regiment battle)))
                 (when target
                   (if (ai-free-p regiment)
                       (let ((kept (gethash (regiment-index regiment) assignments)))
                         (when (and kept (eq (first kept) target)
                                    (< (- tick (third kept))
                                       (* *ai-commit-seconds* +ticks-per-second+)))
                           (setf (gethash (regiment-index regiment) decided) t)
                           (incf (gethash target crowd 0))))
                       (progn
                         (setf (gethash (regiment-index regiment) decided) t)
                         (incf (gethash target crowd 0)))))))
    ;; Notes de toutes les paires encore à décider.
    (loop for regiment across regiments
          when (and (= (regiment-side regiment) side) (ai-free-p regiment)
                    (commander-task-free-p commander regiment)
                    (not (gethash (regiment-index regiment) decided)))
            do (dolist (enemy enemies)
                 (multiple-value-bind (score worth) (ai-score battle regiment enemy)
                   (push (list score regiment enemy worth) pairs))))
    (setf pairs (sort pairs #'> :key #'first))
    ;; Gloutonne : la meilleure paire d'abord, la foule rabat les suivantes.
    (loop while pairs
          do (let ((best nil) (best-score 0f0))
               (dolist (pair pairs)
                 (destructuring-bind (score regiment enemy worth) pair
                   (unless (gethash (regiment-index regiment) decided)
                     (let ((crowded (* score (expt *ai-crowding* (gethash enemy crowd 0)))))
                       (when (> crowded best-score)
                         (setf best (list crowded regiment enemy worth) best-score crowded))))))
               (unless best (return))
               (destructuring-bind (score regiment enemy worth) best
                 (let* ((index (regiment-index regiment))
                        (current (current-target regiment battle))
                        (threshold (cdr (assoc (regiment-role regiment) *ai-thresholds*))))
                   (setf (gethash index decided) t)
                   (cond ((< worth threshold)
                          ;; Rien qui vaille : il garde sa cible, ou attend.
                          (if current
                              (incf (gethash current crowd 0))
                              (progn (remhash index assignments)
                                     (wait-at-home commander battle regiment))))
                         ((eq enemy current)
                          (setf (gethash index assignments)
                                (list enemy score (third (or (gethash index assignments)
                                                             (list nil nil tick)))))
                          (incf (gethash enemy crowd 0)))
                         ((and current
                               (< score (* *ai-switch-margin*
                                           (ai-score battle regiment current))))
                          ;; Pas assez mieux : il garde sa cible.
                          (setf (gethash index assignments)
                                (list current score tick))
                          (incf (gethash current crowd 0)))
                         (t
                          (order-attack regiment enemy)
                          (incf (gethash index (commander-attack-orders commander) 0))
                          (setf (gethash index assignments) (list enemy score tick))
                          (incf (gethash enemy crowd 0))))))
               (setf pairs (remove-if (lambda (pair) (gethash (regiment-index (second pair)) decided))
                                      pairs))))
    ;; Régiments libres sans ennemi du tout : ils attendent.
    (unless enemies
      (loop for regiment across regiments
            when (and (= (regiment-side regiment) side) (ai-free-p regiment)
                      (commander-task-free-p commander regiment)
                      (not (gethash (regiment-index regiment) decided)))
              do (wait-at-home commander battle regiment)))))

(defun remember-homes (commander battle)
  "Note la place de départ de chaque régiment du camp, à son premier réveil."
  (loop for regiment across (battle-regiments battle)
        when (= (regiment-side regiment) (commander-side commander))
          do (let ((formation (regiment-formation regiment)))
               (unless (gethash (regiment-index regiment) (commander-homes commander))
                 (setf (gethash (regiment-index regiment) (commander-homes commander))
                       (list (formation-x formation) (formation-y formation)
                             (formation-facing formation)))))))

(defmethod update-commander ((commander ai-commander) battle)
  (let ((settings (commander-settings commander)))
    (progv (loop for (symbol) on settings by #'cddr collect symbol)
        (loop for (nil value) on settings by #'cddr collect value)
      (when (zerop (mod (+ (battle-tick-count battle) (commander-side commander)) *ai-period*))
        (remember-homes commander battle)
        (update-squares commander battle)
        ;; Le général et les tâches de ses groupes (ai-general.lisp, ai-tasks.lisp),
        ;; puis le choix glouton pour les régiments sans tâche (la cavalerie).
        (when *ai-use-plan*
          (update-plan commander battle)
          (run-tasks commander battle))
        (assign-targets commander battle)))))

;;; Bataille contre l'IA

(defparameter *ai-battle-regiments*
  '((0 :pikemen 225 -70 -50 0)
    (0 :pikemen 225 -70 0 0)
    (0 :pikemen 225 -70 50 0)
    (0 :arquebusiers 105 -52 -50 0 :countermarch :skirmish)
    (0 :arquebusiers 105 -52 0 0 :countermarch :skirmish)
    (0 :arquebusiers 105 -52 50 0 :countermarch :skirmish)
    (0 :rodeleros 105 -72 -95 0)
    (0 :greatswords 105 -72 95 0)
    (0 :light-cavalry 60 -78 -140 0)
    (0 :lancers 60 -78 140 0)
    (0 :halberdiers 105 -100 0 0)
    (1 :pikemen 225 70 -50 180)
    (1 :pikemen 225 70 0 180)
    (1 :pikemen 225 70 50 180)
    (1 :arquebusiers 105 52 -50 180 :countermarch :skirmish)
    (1 :arquebusiers 105 52 0 180 :countermarch :skirmish)
    (1 :arquebusiers 105 52 50 180 :countermarch :skirmish)
    (1 :rodeleros 105 72 95 180)
    (1 :greatswords 105 72 -95 180)
    (1 :light-cavalry 60 78 140 180)
    (1 :lancers 60 78 -140 180)
    (1 :halberdiers 105 100 0 180))
  "Bataille rangée contre l'IA, en miroir : par camp, trois régiments de piques
au centre, des arquebusiers en tirailleurs devant chacun, rodeleros et
espadonniers aux flancs des piques, chevau-légers et lanciers aux ailes,
hallebardiers en réserve derrière le centre.")

(defparameter *ai-sides* '(1) "Camps commandés par l'IA dans la bataille contre l'IA.")

(defun make-ai-battle (&key (seed *demo-seed*) (regiments *ai-battle-regiments*) (sides *ai-sides*)
                            settings)
  "Bataille de REGIMENTS, les camps SIDES commandés par l'IA, qui se décide
(victory.lisp). SETTINGS : alist camp -> plist de réglages de son commandant
(COMMANDER-SETTINGS)."
  (let ((battle (make-demo-battle :seed seed :regiments regiments)))
    (setf (battle-commanders battle)
          (mapcar (lambda (side) (make-ai-commander side (cdr (assoc side settings)))) sides)
          (battle-victory-p battle) t
          (battle-recipe battle) (list 'make-ai-battle :seed seed :regiments regiments :sides sides
                                       :settings settings))
    battle))
