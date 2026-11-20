(in-package #:rogue-company)

;;; Les tâches des groupes du général (ai-general.lisp), exécutées à chaque
;;; réveil du commandant. Chacune ne donne que des ordres du joueur, et
;;; seulement à un régiment libre (AI-FREE-P : ni en mêlée, ni à la charge, ni
;;; en repli, ni en poursuite) ; un ordre identique n'est pas redonné.
;;;
;;; - Ligne : chaque régiment va à sa place, face à l'ennemi. En avance, la
;;;   ligne fait un bond de *AI-LINE-BOUND* quand tous ses régiments libres sont
;;;   à moins de *AI-LINE-TOLERANCE* de leur place : elle avance au pas du plus
;;;   lent, alignée, jusqu'à ce que ses tireurs aient l'ennemi à portée ; elle
;;;   attend alors l'escarmouche (*AI-SKIRMISH-SECONDS*), puis donne l'assaut.
;;;   Engagée, chacun attaque l'ennemi en face de sa place (FACING-ENEMY) et le
;;;   garde tant qu'il combat.
;;; - Écran : les tireurs devant leur place de ligne ; un ennemi à portée devant
;;;   eux, ils l'attaquent (ils s'arrêtent à portée de tir, et se replient en
;;;   tirailleurs), tenus en laisse (*AI-SCREEN-LEASH*). À l'assaut, quand la
;;;   ligne les rattrape, ils s'effacent derrière elle (*AI-SCREEN-BEHIND*) et
;;;   tirent de là une fois la ligne engagée.
;;; - Garde : à sa place au bout de la ligne ; elle attaque un ennemi qui entre
;;;   à moins de *AI-GUARD-RADIUS* de sa place, si le combat en vaut la peine
;;;   (AI-SCORE), et revient quand il passe au-delà de *AI-GUARD-LEASH*. Ligne
;;;   engagée, sa place suit la ligne et son rayon s'élargit
;;;   (*AI-GUARD-ENGAGED-RADIUS*) : elle prend de flanc ce que la ligne fixe.
;;; - Aile (la cavalerie) : à sa place sur l'aile, elle choisit un objectif
;;;   (WING-OBJECTIVE, WING-OPPORTUNITY) : le meilleur combat, piques et mêlée à
;;;   pied notées de flanc, jamais un carré ni une infanterie qui marche sur
;;;   elle, jamais un combat qui ne vaut pas *AI-CAVALRY-WORTH* ; les régiments
;;;   d'une même armée se partagent les objectifs. Réglages (comparés au banc
;;;   d'essai, tous éteints par défaut parce qu'ils ne gagnent pas encore) :
;;;   *AI-CAVALRY-PATIENT* (n'attendre qu'une occasion), *AI-CAVALRY-FLANK*
;;;   (contourner par FLANK-POINT ce qui ne se prend que de face),
;;;   *AI-CAVALRY-RALLY* (abandonner une charge face à des piques, rappeler une
;;;   poursuite à moins de *AI-RALLY-DANGER* de piques en bon ordre, de face) et
;;;   *AI-CAVALRY-EVASION* (reculer sa place devant l'infanterie ennemie libre,
;;;   SAFE-WING-PLACE).
;;; - Réserve : derrière le centre ; quand un régiment de la ligne ou une garde
;;;   flanche au combat (moral sous *AI-WAVER-MORALE*), elle attaque l'ennemi
;;;   qu'il combat.

(defparameter *ai-line-bound* 8f0 "Bond de la ligne, en mètres.")

(defparameter *ai-use-skirmish* t
  "Vrai : la ligne s'arrête quand ses tireurs ont l'ennemi à portée, et ne
donne l'assaut qu'après l'escarmouche. Faux : elle avance jusqu'au contact.")

(defparameter *ai-skirmish-range* 1f0
  "Fraction de la portée de tir à laquelle un ennemi arrête la ligne.")

(defparameter *ai-skirmish-seconds* 40
  "Durée de l'escarmouche, en secondes : ensuite, l'assaut. Un déclencheur sur
l'échange de feu (nos pertes contre celles de l'ennemi) a été mesuré au banc
d'essai : il lançait l'assaut trop tôt, sur les pertes des premiers combats de
cavalerie, et perdait (19 contre 13 au lieu de 26 contre 6).")

(defparameter *ai-screen-behind* 12f0
  "À l'assaut, la ligne rattrape les tireurs à cette distance d'eux : ils
s'effacent alors derrière elle (LINE-CAUGHT-UP-P).")

(defparameter *ai-screen-clearance* 5f0
  "Effacés derrière la ligne, les tireurs gardent cette marge, en mètres, au-delà
de *SKIRMISH-TRIGGER* entre leur front et celui de la ligne : l'ennemi au
contact de la ligne ne les fait pas replier en tirailleurs (SCREEN-BEHIND).")

(defparameter *ai-line-tolerance* 5f0
  "Un régiment de la ligne à moins de cette distance de sa place y est : le bond
suivant attend tous les autres.")

(defparameter *ai-engage-reach* 50f0
  "Engagée, un régiment de la ligne n'attaque qu'un ennemi à moins de cette
distance ; sinon il tient sa place.")

(defparameter *ai-lateral-weight* 1.5f0
  "Poids de l'écart latéral entre la place d'un régiment et un ennemi, dans le
choix de l'ennemi en face.")

(defparameter *ai-screen-leash* 70f0
  "Des tireurs reviennent devant leur place quand leur cible en est plus loin.")

(defparameter *ai-guard-radius* 45f0
  "Une garde attaque un ennemi à moins de cette distance de sa place.")

(defparameter *ai-guard-leash* 70f0
  "Une garde lâche une cible à plus de cette distance de sa place, et revient.")

(defparameter *ai-guard-engaged-radius* 70f0
  "Rayon d'une garde quand la ligne est engagée.")

(defparameter *ai-guard-engaged-leash* 100f0
  "Laisse d'une garde quand la ligne est engagée.")

(defparameter *ai-guard-worth* 0.6f0
  "Valeur minimale du combat (AI-SCORE) pour qu'une garde attaque.")

(defparameter *ai-waver-morale* 0.6f0
  "Un régiment en mêlée sous cette part de son moral flanche : la réserve vient.")

(defparameter *ai-cavalry-flank-foot* t
  "Vrai : la cavalerie d'une aile peut tourner des piques ou de la mêlée à pied
accrochées en mêlée. Faux : elle ne vise que la cavalerie et les tireurs.")

(defparameter *ai-cavalry-patient* nil
  "Vrai : la cavalerie d'une aile n'attaque qu'une occasion (cavalerie qui menace,
ennemi accroché, tireurs non couverts). Faux : n'importe quel ennemi qui n'est
pas un carré, noté de flanc s'il ne se prend que de face. Mesuré au banc
d'essai (ai-bench.lisp) : patiente, elle laisse l'initiative à la cavalerie
adverse, dont les premiers coups sur les gardes et les tireurs pèsent plus
tard ; elle perd 3 batailles sur 4 contre la cavalerie gloutonne.")

(defparameter *ai-cavalry-flank* nil
  "Vrai : un objectif qui ne se prend que de face est d'abord contourné
(FLANK-POINT). Faux : la cavalerie le charge droit. Faux par défaut : le
contournement, sans chemin sûr, traverse l'aile ennemie et s'y fait intercepter
par sa cavalerie ou ses gardes (mesuré : 4 victoires sur 16 contre la cavalerie
gloutonne, contre 8 sur 16 sans lui). À reprendre avec la carte des menaces.")

(defparameter *ai-cavalry-evasion* nil
  "Vrai : la place de la cavalerie recule devant l'infanterie ennemie libre
(SAFE-WING-PLACE), à *AI-CAVALRY-EVADE* de distance. Faux par défaut : mesuré
avec *AI-CAVALRY-RALLY* sur 32
batailles contre la cavalerie gloutonne, les deux ensemble perdent 11 à 21 ;
éteints, 16 à 16.")

(defparameter *ai-cavalry-rally* nil
  "Vrai : une poursuite qui mène vers des piques, ou une charge qui se retrouve
face à elles, est interrompue. Faux par défaut (voir *AI-CAVALRY-EVASION*) : la
poursuite tue plus qu'elle ne coûte.")

(defparameter *ai-cavalry-chase* t
  "Vrai : la cavalerie d'une aile poursuit les fuyards (posture :CHASE). Faux :
l'ennemi débandé, elle se rallie aussitôt à son aile (posture :HOLD).")

(defparameter *ai-cavalry-worth* 1f0
  "Valeur minimale (AI-SCORE, distance mise à part) d'une occasion pour la
cavalerie d'une aile.")

(defparameter *ai-wing-threat-radius* 80f0
  "De la cavalerie ennemie libre à moins de cette distance de la place d'une aile
la menace : la cavalerie de l'aile la charge.")

(defparameter *ai-cavalry-threat-radius* 40f0
  "De la cavalerie ennemie à moins de cette distance d'un de nos régiments, ou
qui le frappe, le menace : la cavalerie d'une aile peut la charger.")

(defparameter *ai-wing-reach* 200f0
  "La cavalerie d'une aile ne vise pas d'occasion à plus de cette distance.")

(defparameter *ai-flank-offset* 20f0
  "Écart du point de contournement au bord du front de l'objectif, en mètres.")

(defparameter *ai-flank-behind* 10f0
  "Recul du point de contournement derrière le centre de l'objectif, en mètres :
la charge part de son flanc, un peu vers son dos.")

(defparameter *ai-cavalry-evade* 25f0
  "La cavalerie d'une aile ne reste pas à moins de cette distance de piques ou
de mêlée à pied ennemies libres : sa place recule de *AI-CAVALRY-FALLBACK* tant
qu'elle y est (SAFE-WING-PLACE).")

(defparameter *ai-cavalry-fallback* 20f0 "Recul de la place de la cavalerie, par bond, en mètres.")

(defparameter *ai-rally-danger* 30f0
  "Une poursuite qui mène à moins de cette distance de piques ennemies en bon
ordre, de face, est interrompue.")

(defparameter *ai-move-slack* 1f0
  "Un ordre de marche vers un point à moins de cette distance du précédent n'est
pas redonné.")

;;; Ordres

(defun ai-move (regiment x y facing)
  "Envoie REGIMENT en X, Y, front vers FACING, sauf s'il y est déjà envoyé."
  (let ((orders (regiment-orders regiment)))
    (unless (and (eq (first orders) :move)
                 (< (+ (abs (- (second orders) x)) (abs (- (third orders) y))) *ai-move-slack*)
                 (< (abs (wrap-angle (- (fourth orders) facing))) 0.1f0))
      (order-move regiment x y facing))))

(defun target-of (battle regiment)
  "Le régiment qu'attaque REGIMENT, ou NIL."
  (let ((orders (regiment-orders regiment)))
    (and (eq (first orders) :attack) (aref (battle-regiments battle) (second orders)))))

(defun fighting-target (battle regiment)
  "Le régiment qu'attaque REGIMENT, s'il est encore en état de combattre."
  (let ((target (target-of battle regiment)))
    (and target (fighting-p target) target)))

(defparameter *ai-foot-lets-cavalry-go* t
  "Vrai : un régiment à pied de mêlée ne poursuit pas une cavalerie libre
(CATCHABLE-P). Faux : l'ancienne conduite, que la cavalerie d'un joueur
promenait sur tout le champ de bataille.")

(defun catchable-p (regiment enemy)
  "Vrai si REGIMENT peut rattraper ENEMY : sauf de la cavalerie libre (pas en
mêlée) pour un régiment à pied de mêlée, qui la poursuivrait sans fin. Les
tireurs la visent de loin, la cavalerie la rattrape."
  (or (not *ai-foot-lets-cavalry-go*)
      (member (regiment-role regiment) '(:cavalry :shooters))
      (not (eq (regiment-role enemy) :cavalry))
      (regiment-in-melee-p enemy)))

(defun held-target (battle regiment)
  "Le régiment qu'attaque REGIMENT, s'il est en état de combattre et qu'il peut
le rattraper (CATCHABLE-P) ; sinon NIL, et sa tâche le rappelle."
  (let ((target (fighting-target battle regiment)))
    (and target (catchable-p regiment target) target)))

(defun ai-attack (commander battle regiment enemy)
  "Lance REGIMENT sur ENEMY, sauf s'il l'attaque déjà."
  (unless (eq enemy (target-of battle regiment))
    (order-attack regiment enemy)
    (incf (gethash (regiment-index regiment) (commander-attack-orders commander) 0))))

;;; Places

(defun regiment-half-depth (regiment)
  "Demi-profondeur de REGIMENT en formation, en mètres."
  (* 0.5f0 (max 0 (1- (regiment-depth regiment)))
     (formation-spacing (regiment-formation regiment))))

(defun line-half-depth (plan battle)
  "Demi-profondeur du régiment le plus profond de la ligne de PLAN."
  (loop for k in (plan-line plan)
        for regiment = (aref (battle-regiments battle) k)
        when (fighting-p regiment)
          maximize (regiment-half-depth regiment) into best
        finally (return (float (or best 0) 1f0))))

(defun screen-behind (plan regiment)
  "Distance entre la ligne et les tireurs REGIMENT effacés derrière elle : son
front et le leur à *SKIRMISH-TRIGGER* plus *AI-SCREEN-CLEARANCE*, assez loin
pour que l'ennemi au contact de la ligne ne les fasse pas replier."
  (+ (plan-line-half-depth plan) (regiment-half-depth regiment)
     *skirmish-trigger* *ai-screen-clearance*))

(defun task-destination (plan regiment)
  "Place de REGIMENT dans le plan : X, Y et orientation, ou NIL s'il est libre."
  (let* ((task (gethash (regiment-index regiment) (plan-tasks plan)))
         (lateral (getf task :lateral))
         (depth (plan-depth plan)))
    (when lateral
      (multiple-value-bind (x y)
          (plan-point plan lateral
                      (ecase (getf task :kind)
                        (:line depth)
                        (:screen (if (or (eq (plan-phase plan) :engaged)
                                         (member (regiment-index regiment) (plan-retired plan)))
                                     (- depth (screen-behind plan regiment))
                                     (+ depth *ai-screen-ahead*)))
                        (:guard (- depth *ai-guard-setback*))
                        (:reserve (- depth *ai-reserve-depth*))
                        (:wing (- depth *ai-wing-setback*))))
        (values x y (plan-angle plan))))))

(defun facing-enemy (battle plan regiment lateral)
  "L'ennemi en face de la place LATERAL de REGIMENT : le plus proche, l'écart
latéral compté *AI-LATERAL-WEIGHT* fois, à moins de *AI-ENGAGE-REACH* ; ou NIL."
  (let ((soldiers (battle-soldiers battle)) (best nil) (best-cost most-positive-single-float))
    (dolist (enemy (plan-enemies battle (regiment-side regiment)))
      (let ((gap (box-gap regiment enemy)))
        (when (and (< gap *ai-engage-reach*) (catchable-p regiment enemy))
          (let ((cost (+ gap (* *ai-lateral-weight*
                                (abs (- lateral (nth-value 0 (regiment-coordinates plan enemy soldiers))))))))
            (when (< cost best-cost) (setf best enemy best-cost cost))))))
    best))

;;; Tâches

;;; Phases de la ligne : :ADVANCE (par bonds) jusqu'à ce qu'un régiment de
;;; l'écran ait un ennemi à portée de tir ; :SKIRMISH, la ligne attend que le
;;; feu ait fait son œuvre (SKIRMISH-OVER-P) ; :ASSAULT, elle avance sans plus
;;; s'arrêter, les tireurs s'effaçant derrière elle quand elle les rattrape ;
;;; :ENGAGED dès qu'elle approche l'ennemi, quelle que soit la phase.

(defun line-side (plan battle)
  (regiment-side (aref (battle-regiments battle) (first (plan-line plan)))))

(defun plan-regiments (plan battle kind)
  "Régiments en état de combattre dont la tâche est KIND."
  (let ((found '()))
    (maphash (lambda (k task)
               (let ((regiment (aref (battle-regiments battle) k)))
                 (when (and (eq (getf task :kind) kind) (fighting-p regiment))
                   (push regiment found))))
             (plan-tasks plan))
    found))

(defun screen-in-range-p (plan battle side)
  "Vrai si un régiment de l'écran a un ennemi à portée de tir."
  (let ((enemies (plan-enemies battle side)))
    (some (lambda (regiment)
            (let ((reach (* *ai-skirmish-range* (attack-reach (stats-primary (regiment-stats regiment))))))
              (some (lambda (enemy) (<= (box-gap regiment enemy) reach)) enemies)))
          (plan-regiments plan battle :screen))))

(defun skirmish-over-p (plan battle)
  "Vrai quand l'escarmouche a assez duré (*AI-SKIRMISH-SECONDS*), ou qu'il n'y a
plus de tireurs."
  (or (>= (/ (- (battle-tick-count battle) (plan-skirmish-tick plan)) +ticks-per-second+)
          *ai-skirmish-seconds*)
      (null (plan-regiments plan battle :screen))))

(defun line-at-places-p (plan battle)
  "Vrai si tous les régiments libres de la ligne sont à leur place."
  (let ((soldiers (battle-soldiers battle)))
    (loop for k in (plan-line plan)
          for regiment = (aref (battle-regiments battle) k)
          always (or (not (ai-free-p regiment))
                     (not (fighting-p regiment))
                     (regiment-square-p regiment)
                     (multiple-value-bind (x y) (task-destination plan regiment)
                       (multiple-value-bind (cx cy) (regiment-centroid regiment soldiers)
                         (< (sqrt (+ (expt (- cx x) 2) (expt (- cy y) 2)))
                            *ai-line-tolerance*)))))))

(defun advance-line (plan battle)
  "Fait passer la ligne d'une phase à l'autre, et avancer d'un bond quand tous
ses régiments libres sont à leur place, en avance ou à l'assaut."
  (let* ((side (line-side plan battle))
         (gap (line-gap-to-enemy plan battle side)))
    (setf (plan-line-half-depth plan) (line-half-depth plan battle))
    (when (and gap (< gap *ai-engage-distance*))
      (setf (plan-phase plan) :engaged))
    (case (plan-phase plan)
      (:engaged
       ;; Engagée, la profondeur suit la ligne : gardes et réserve restent à hauteur.
       (setf (plan-depth plan) (line-depth plan battle)))
      (:advance
       (cond ((and *ai-use-skirmish* (screen-in-range-p plan battle side))
              (setf (plan-phase plan) :skirmish)
              (unless (plan-skirmish-tick plan)
                (setf (plan-skirmish-tick plan) (battle-tick-count battle))))
             ((line-at-places-p plan battle)
              (incf (plan-depth plan) *ai-line-bound*))))
      (:skirmish
       (cond ((skirmish-over-p plan battle)
              (setf (plan-phase plan) :assault))
             ((not (screen-in-range-p plan battle side))
              ;; L'ennemi s'est mis hors de portée : on se rapproche.
              (setf (plan-phase plan) :advance))))
      (:assault
       (when (line-at-places-p plan battle)
         (incf (plan-depth plan) *ai-line-bound*))))))

(defun go-to-place (plan regiment)
  (unless (regiment-square-p regiment)
    (multiple-value-bind (x y facing) (task-destination plan regiment)
      (ai-move regiment x y facing))))

(defun drop-lost-target (battle regiment)
  "Un régiment qui attaque un ennemi qu'il ne rattrapera pas (CATCHABLE-P)
s'arrête. GO-TO-PLACE le ramène ensuite à sa place ; pas un carré, qui sinon
garderait son ordre et repartirait à la poursuite en reprenant la ligne."
  (let ((target (target-of battle regiment)))
    (when (and target (fighting-p target) (not (catchable-p regiment target)))
      (command-halt battle (list regiment)))))

(defun run-line (commander battle plan regiment task)
  (drop-lost-target battle regiment)
  (if (eq (plan-phase plan) :engaged)
      (unless (held-target battle regiment)
        (let ((enemy (facing-enemy battle plan regiment (getf task :lateral))))
          (if enemy
              (ai-attack commander battle regiment enemy)
              (go-to-place plan regiment))))
      (go-to-place plan regiment)))

(defun line-caught-up-p (plan battle regiment)
  "Vrai si la ligne est arrivée à hauteur des tireurs REGIMENT."
  (>= (plan-depth plan)
      (- (nth-value 1 (regiment-coordinates plan regiment (battle-soldiers battle)))
         *ai-screen-behind*)))

(defun run-screen (commander battle plan regiment)
  "Devant la ligne, les tireurs attaquent l'ennemi le plus proche à portée (plus
10 m), s'il est assez près de leur place pour qu'ils ne le lâchent pas aussitôt
(*AI-SCREEN-LEASH*). À l'assaut, quand la ligne les rattrape, ils s'effacent
derrière elle et n'en bougent plus jusqu'au contact (PLAN-RETIRED)."
  (when (and (eq (plan-phase plan) :assault)
             (not (member (regiment-index regiment) (plan-retired plan)))
             (line-caught-up-p plan battle regiment))
    (push (regiment-index regiment) (plan-retired plan)))
  (multiple-value-bind (x y) (task-destination plan regiment)
    (let ((target (fighting-target battle regiment))
          (reach (attack-reach (stats-primary (regiment-stats regiment)))))
      (cond ((and (eq (plan-phase plan) :assault)
                  (member (regiment-index regiment) (plan-retired plan)))
             (go-to-place plan regiment))
            ((and target (> (point-box-distance target x y) *ai-screen-leash*))
             (go-to-place plan regiment))
            (target nil)
            (t
             (let ((enemy (loop with best = nil and best-gap = (+ reach 10f0)
                                for other in (plan-enemies battle (regiment-side regiment))
                                for gap = (box-gap regiment other)
                                when (and (< gap best-gap)
                                          (<= (point-box-distance other x y) *ai-screen-leash*))
                                  do (setf best other best-gap gap)
                                finally (return best))))
               (if enemy
                   (ai-attack commander battle regiment enemy)
                   (go-to-place plan regiment))))))))

(defun run-guard (commander battle plan regiment)
  (drop-lost-target battle regiment)
  (multiple-value-bind (x y) (task-destination plan regiment)
    (let* ((target (held-target battle regiment))
           (engaged (eq (plan-phase plan) :engaged))
           (radius (if engaged *ai-guard-engaged-radius* *ai-guard-radius*))
           (leash (if engaged *ai-guard-engaged-leash* *ai-guard-leash*)))
      (cond ((and target (> (point-box-distance target x y) leash))
             (go-to-place plan regiment))
            (target nil)
            (t
             (let ((enemy (loop with best = nil and best-d = radius
                                for other in (plan-enemies battle (regiment-side regiment))
                                for d = (point-box-distance other x y)
                                when (and (< d best-d)
                                          (catchable-p regiment other)
                                          (>= (nth-value 1 (ai-score battle regiment other))
                                              *ai-guard-worth*))
                                  do (setf best other best-d d)
                                finally (return best))))
               (if enemy
                   (ai-attack commander battle regiment enemy)
                   (go-to-place plan regiment))))))))

(defun wavering-regiment (plan battle side)
  "Le régiment de la ligne ou de garde du camp SIDE qui flanche le plus en
mêlée, ou NIL."
  (let ((best nil) (best-morale *ai-waver-morale*))
    (maphash (lambda (k task)
               (let ((regiment (aref (battle-regiments battle) k)))
                 (when (and (member (getf task :kind) '(:line :guard))
                            (= (regiment-side regiment) side)
                            (fighting-p regiment)
                            (regiment-in-melee-p regiment)
                            (< (morale-ratio regiment) best-morale))
                   (setf best regiment best-morale (morale-ratio regiment)))))
             (plan-tasks plan))
    best))

(defun run-reserve (commander battle plan regiment)
  (drop-lost-target battle regiment)
  (unless (held-target battle regiment)
    (let* ((wavering (wavering-regiment plan battle (regiment-side regiment)))
           (enemy (and wavering (fought-regiment battle wavering))))
      (if (and enemy (fighting-p enemy) (catchable-p regiment enemy))
          (ai-attack commander battle regiment enemy)
          (go-to-place plan regiment)))))

(defun front-pikes-near-p (battle regiment distance)
  "Vrai si des piques ennemies en bon ordre (PIKE-WALL-P) font face à REGIMENT à
moins de DISTANCE."
  (let ((soldiers (battle-soldiers battle)))
    (dolist (enemy (plan-enemies battle (regiment-side regiment)))
      (when (and (pike-wall-p enemy)
                 (< (box-gap regiment enemy) distance)
                 (zerop (approach-sector regiment enemy soldiers)))
        (return t)))))

(defun frontal-only-p (enemy)
  "Vrai si ENEMY ne se charge pas de face : des piques ou de la mêlée à pied."
  (member (regiment-role enemy) '(:pikes :foot)))

(defun threatens-friends-p (battle enemy side)
  "Vrai si ENEMY menace un régiment du camp SIDE : il en est à moins de
*AI-CAVALRY-THREAT-RADIUS*, ou il le frappe."
  (loop for friend across (battle-regiments battle)
        thereis (and (= (regiment-side friend) side)
                     (fighting-p friend)
                     (< (box-gap friend enemy) *ai-cavalry-threat-radius*))))

(defparameter *ai-cavalry-defended-distance* 40f0
  "Des tireurs à moins de cette distance de piques amies en bon ordre, pas en
mêlée, ne sont pas une occasion pour la cavalerie : en tirailleurs, ils se
replient derrière elles, et la charge finit dans les piques. 0 : jamais.
Mesuré contre un joueur scripté (ai-script-match) : 40 m gagne 32 batailles
sur 32, contre 15 sans la règle. Entre deux IA, elle perd 1 contre 31, parce que
l'IA ne défend pas ses propres tireurs comme un joueur : gardée pour le joueur.")

(defun defended-p (battle enemy)
  "Vrai si des piques en bon ordre du camp d'ENEMY, pas en mêlée, sont à moins de
*AI-CAVALRY-DEFENDED-DISTANCE* de lui."
  (loop for other across (battle-regiments battle)
        thereis (and (= (regiment-side other) (regiment-side enemy))
                     (not (eq other enemy))
                     (pike-wall-p other)
                     (not (regiment-in-melee-p other))
                     (< (box-gap enemy other) *ai-cavalry-defended-distance*))))

(defun wing-opportunity (battle plan regiment enemy)
  (when (and (frontal-only-p enemy) (eq (target-of battle enemy) regiment))
    ;; Une infanterie qui marche sur nous n'est pas une occasion : on l'esquive.
    (return-from wing-opportunity nil))
  (wing-opportunity-1 battle plan regiment enemy))

(defun wing-opportunity-1 (battle plan regiment enemy)
  "Note de l'occasion qu'ENEMY offre à la cavalerie REGIMENT, ou NIL : de la
cavalerie ennemie libre qui menace son aile, un ennemi accroché en mêlée (noté
de flanc : elle va le tourner), des tireurs non couverts ; jamais un carré, ni
un combat qui ne vaut pas *AI-CAVALRY-WORTH*."
  (multiple-value-bind (px py) (task-destination plan regiment)
    (let* ((role (regiment-role enemy))
           (engaged (regiment-in-melee-p enemy))
           (eligible (cond ((regiment-square-p enemy) nil)
                           ((and (eq role :shooters) (defended-p battle enemy)) nil)
                           ((eq role :cavalry)
                            (or (< (point-box-distance enemy px py) *ai-wing-threat-radius*)
                                (threatens-friends-p battle enemy (regiment-side regiment))))
                           ((not *ai-cavalry-patient*) t)
                           ((eq role :shooters) (or engaged (not (screened-p battle enemy))))
                           (engaged *ai-cavalry-flank-foot*))))
      (when (and eligible (fighting-p enemy) (< (box-gap regiment enemy) *ai-wing-reach*))
        (multiple-value-bind (score worth)
            (ai-score battle regiment enemy
                      ;; Noté de flanc seulement si elle va vraiment le tourner.
                      :sector (if (and *ai-cavalry-flank* (frontal-only-p enemy)
                                       (or engaged (not *ai-cavalry-patient*)))
                                  1 nil))
          (and (>= worth *ai-cavalry-worth*) score))))))

(defun wing-objective (commander battle plan regiment)
  "La meilleure occasion de la cavalerie REGIMENT (WING-OPPORTUNITY), ou NIL.
Chaque autre régiment de cavalerie du camp qui a déjà cet objectif en rabat la
note (*AI-CROWDING*) : les ailes se partagent les occasions."
  (let ((best nil) (best-score 0f0)
        (taken (loop for k being the hash-keys of (commander-objectives commander)
                       using (hash-value objective)
                     unless (= k (regiment-index regiment)) collect objective)))
    (dolist (enemy (plan-enemies battle (regiment-side regiment)) best)
      (let ((score (wing-opportunity battle plan regiment enemy)))
        (when score
          (let ((shared (* score (expt *ai-crowding* (count (regiment-index enemy) taken)))))
            (when (> shared best-score)
              (setf best enemy best-score shared))))))))

(defun flank-point (battle regiment enemy)
  "Prochaine étape du contournement d'ENEMY par REGIMENT, du côté de REGIMENT :
encore dans le cône de son front, s'en écarter sur le côté (au-delà de son
front de *AI-FLANK-OFFSET*, à la même hauteur) ; hors du cône, rabattre vers
son flanc, *AI-FLANK-BEHIND* en arrière de son centre. Jamais en travers de son
front."
  (let ((soldiers (battle-soldiers battle))
        (facing (formation-facing (regiment-formation enemy))))
    (multiple-value-bind (ex ey) (regiment-centroid enemy soldiers)
      (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
        (let* ((fx (cos facing)) (fy (sin facing))
               (lx (- fy)) (ly fx)
               (along (+ (* (- x ex) fx) (* (- y ey) fy)))
               (across (+ (* (- x ex) lx) (* (- y ey) ly)))
               (side (if (minusp across) -1 1))
               (out (+ (/ (regiment-frontage enemy) 2) *ai-flank-offset*))
               ;; Dans le cône du front : on s'écarte d'abord, à la même hauteur.
               (forward (if (and (> along 0f0) (< (abs across) (- out 3f0)))
                            along
                            (- *ai-flank-behind*))))
          (values (float (+ ex (* side lx out) (* fx forward)) 1f0)
                  (float (+ ey (* side ly out) (* fy forward)) 1f0)))))))

(defun approach-or-charge (commander battle regiment objective)
  "Charge OBJECTIVE, ou le contourne d'abord s'il ne s'aborderait que de face."
  (if (and *ai-cavalry-flank*
           (frontal-only-p objective)
           (zerop (approach-sector regiment objective (battle-soldiers battle))))
      (multiple-value-bind (x y) (flank-point battle regiment objective)
        (multiple-value-bind (ex ey) (regiment-centroid objective (battle-soldiers battle))
          (ai-move regiment x y (float (atan (- ey y) (- ex x)) 1f0))))
      (ai-attack commander battle regiment objective)))

(defun infantry-danger-p (battle side x y)
  "Vrai si des piques ou de la mêlée à pied ennemies, en état de combattre et pas
en mêlée, sont à moins de *AI-CAVALRY-EVADE* du point X, Y."
  (dolist (enemy (plan-enemies battle side))
    (when (and (frontal-only-p enemy)
               (not (regiment-in-melee-p enemy))
               (< (point-box-distance enemy x y) *ai-cavalry-evade*))
      (return t))))

(defun safe-wing-place (battle plan regiment)
  "Place de la cavalerie REGIMENT, reculée le long de l'axe tant que de
l'infanterie ennemie libre en est trop près : elle ne s'y laisse pas accrocher."
  (multiple-value-bind (x y facing) (task-destination plan regiment)
    (let ((back-x (- (cos facing))) (back-y (- (sin facing))))
      (loop repeat 6
            while (and *ai-cavalry-evasion* (infantry-danger-p battle (regiment-side regiment) x y))
            do (incf x (* back-x *ai-cavalry-fallback*))
               (incf y (* back-y *ai-cavalry-fallback*)))
      (values x y facing))))

(defun run-wing (commander battle plan regiment)
  (setf (regiment-stance regiment) (if *ai-cavalry-chase* :chase :hold))
  (let* ((index (regiment-index regiment))
         (objectives (commander-objectives commander))
         (target (fighting-target battle regiment)))
    (flet ((rally ()
             (remhash index objectives)
             (multiple-value-bind (x y facing) (safe-wing-place battle plan regiment)
               (ai-move regiment x y facing))))
      (cond ((regiment-chasing-p regiment)
             ;; Pas de poursuite jusque dans des piques.
             (when (and *ai-cavalry-rally* (front-pikes-near-p battle regiment *ai-rally-danger*))
               (rally)))
            ((and *ai-cavalry-rally* target (pike-wall-p target)
                  (zerop (approach-sector regiment target (battle-soldiers battle))))
             ;; La charge se retrouve face à des piques : on l'abandonne.
             (rally))
            ;; En approche (ni à la charge ni en mêlée : WING-FREE-P), la cible
            ;; est revue : une occasion qui n'en est plus une est abandonnée.
            ((and target (wing-opportunity battle plan regiment target)) nil)
            (t
             (let* ((kept (gethash index objectives))
                    (objective (and kept (aref (battle-regiments battle) kept))))
               ;; L'objectif gardé tant qu'il reste une occasion, et qu'il ne
               ;; marche pas sur nous : on ne tourne pas qui vient nous chercher.
               (when (and objective (frontal-only-p objective)
                          (eq (target-of battle objective) regiment))
                 (setf objective nil))
               (unless (and objective (wing-opportunity battle plan regiment objective)
                            (not (and (frontal-only-p objective)
                                      (eq (target-of battle objective) regiment))))
                 (setf objective (wing-objective commander battle plan regiment))
                 (if objective
                     (setf (gethash index objectives) (regiment-index objective))
                     (remhash index objectives)))
               (if objective
                   (approach-or-charge commander battle regiment objective)
                   (rally))))))))

(defun wing-free-p (regiment)
  "Vrai si le commandant peut donner un ordre à la cavalerie REGIMENT : comme
AI-FREE-P, mais aussi en poursuite, même en frappant les fuyards (en mêlée),
pour la rallier avant qu'elle n'atteigne des piques."
  (and (fighting-p regiment)
       (or (regiment-chasing-p regiment)
           (and (not (regiment-in-melee-p regiment))
                (not (regiment-charging-p regiment))))
       (not (eq (first (regiment-orders regiment)) :retreat))))
