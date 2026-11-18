(in-package #:rogue-company)

;;; Bataille de démonstration, en attendant la partie : celle que lance « Start
;;; Battle », et que C fait charger. Par défaut, le tournoi
;;; (*MATCHUP-DEMO-REGIMENTS*) : blocs de 15 files, 225 piquiers sur 15 rangs,
;;; 105 hommes sur 7 rangs pour les autres armes. *FLANK-DEMO-REGIMENTS* garde la
;;; démonstration des flancs. Les tests prennent leurs propres régiments
;;; (REGIMENTS) et dispersent les soldats au départ (SCATTER) pour éprouver le
;;; regroupement et les collisions.

(defparameter *demo-scatter* 0
  "Dispersion initiale des soldats, en mètres : 0, ils partent à leur emplacement.")

(defparameter *demo-seed* 1
  "Graine de la bataille de démonstration lancée par la scène.")

(defparameter *flank-demo-regiments*
  '((0 :pikemen 225 -25 -26 0)
    (0 :pikemen 225 -25 26 0)
    (1 :pikemen 225 25 -26 180)
    (1 :pikemen 225 25 26 180)
    (0 :rodeleros 105 0 -90 90)
    (1 :rodeleros 105 0 90 -90))
  "Démonstration des flancs : deux régiments de piquiers de chaque camp face à
face au centre, et les rodeleros de chaque camp sur une aile, plus près des
piquiers ennemis que de tout autre régiment, pour que C les lance sur leur flanc
pendant que les piques s'affrontent de face.")

(defparameter *melee-demo-regiments*
  '((0 :pikemen 225 -25 -90 0)
    (1 :rodeleros 105 25 -90 180)
    (0 :pikemen 225 -25 -30 0)
    (1 :pikemen 225 25 -30 180)
    (0 :halberdiers 105 -25 30 0)
    (1 :rodeleros 105 25 30 180)
    (0 :halberdiers 105 -25 90 0)
    (1 :pikemen 225 25 90 180))
  "Duels de mêlée côte à côte : piques contre rodeleros, piques contre piques,
hallebardes contre rodeleros, hallebardes contre piques.")

(defparameter *arquebus-demo-regiments*
  '((0 :arquebusiers 105 -40 -75 0 :countermarch)
    (1 :arquebusiers 105 40 -75 180 :fire-by-rank)
    (0 :arquebusiers 105 -40 -25 0)
    (1 :rodeleros 105 40 -25 180)
    (0 :arquebusiers 105 -40 25 0)
    (1 :pikemen 225 40 25 180)
    (0 :arquebusiers 105 -40 75 0)
    (1 :halberdiers 105 40 75 180))
  "Duels d'arquebusiers contre chaque arme : contre des arquebusiers
(contremarche contre feu par rang), des rodeleros, des piquiers et des
hallebardiers.")

(defparameter *drill-demo-regiments*
  ;; 90 m entre les duels : à la salve, sur deux rangs, le front fait 72 m.
  '((0 :arquebusiers 105 -40 -90 0 :volley)
    (1 :arquebusiers 105 40 -90 180 :countermarch)
    (0 :arquebusiers 105 -40 0 0 :volley)
    (1 :arquebusiers 105 40 0 180 :fire-by-rank)
    (0 :arquebusiers 105 -40 90 0 :countermarch)
    (1 :arquebusiers 105 40 90 180 :fire-by-rank))
  "Régiments de la démonstration : (camp dotation effectif x y orientation en
degrés [exercice de tir]). Trois duels d'arquebusiers, un par paire
d'exercices, hors de portée de tir au départ, que C lance chacun sur l'ennemi
le plus proche : salve contre contremarche, salve contre feu par rang,
contremarche contre feu par rang. Sans exercice, le feu à volonté.")

(defparameter *salvo-demo-regiments*
  ;; 90 m entre les duels : à la salve, sur deux rangs, le front fait 72 m.
  '((0 :arquebusiers 105 -40 -90 0 :salvo)
    (1 :arquebusiers 105 40 -90 180 :at-will)
    (0 :arquebusiers 105 -40 0 0 :salvo)
    (1 :arquebusiers 105 40 0 180 :countermarch)
    (0 :arquebusiers 105 -40 90 0 :salvo)
    (1 :arquebusiers 105 40 90 180 :fire-by-rank))
  "Trois duels d'arquebusiers : la salvée contre le feu à volonté (l'exercice
de base), la contremarche et le feu par rang.")

(defparameter *skirmish-demo-regiments*
  '((0 :arquebusiers 105 -40 0 0 :countermarch :skirmish)
    (0 :pikemen 225 -56 0 0 nil :chase)
    (1 :rodeleros 105 40 0 180))
  "Des arquebusiers en tirailleurs, en contremarche,
devant un régiment de piques qui poursuit, contre des rodeleros, que C lance
chacun sur l'ennemi le plus proche : les arquebusiers tirent, se replient à
travers les piques quand les rodeleros approchent, puis tirent sur la mêlée.")

(defparameter *battle-demo-regiments*
  '((0 :pikemen 225 -62 -54 0)
    (0 :pikemen 225 -62 0 0)
    (0 :pikemen 225 -62 54 0)
    (0 :arquebusiers 105 -44 -54 0 :countermarch :skirmish)
    (0 :arquebusiers 105 -44 0 0 :countermarch :skirmish)
    (0 :arquebusiers 105 -44 54 0 :countermarch :skirmish)
    (0 :rodeleros 105 -58 -110 0 nil :chase)
    (0 :rodeleros 105 -58 110 0 nil :chase)
    (1 :pikemen 225 62 -54 180)
    (1 :pikemen 225 62 0 180)
    (1 :pikemen 225 62 54 180)
    (1 :arquebusiers 105 44 -54 180 :countermarch :skirmish)
    (1 :arquebusiers 105 44 0 180 :countermarch :skirmish)
    (1 :arquebusiers 105 44 54 180 :countermarch :skirmish)
    (1 :rodeleros 105 58 -110 180 nil :chase)
    (1 :rodeleros 105 58 110 180 nil :chase))
  "Bataille rangée, en miroir : pour
chaque camp, trois régiments de piques au centre, un régiment d'arquebusiers en
tirailleurs et en contremarche devant chacun, et deux régiments de rodeleros en
poursuite sur les ailes.")

(defparameter *duel-demo-regiments*
  '((0 :arquebusiers 105 -40 0 0 :countermarch)
    (1 :arquebusiers 105 40 0 180 :countermarch))
  "Duel de deux régiments d'arquebusiers en contremarche, hors de portée au
départ, pour observer comment les pertes se comblent.")

(defparameter *matchup-dotations* '(:pikemen :halberdiers :rodeleros :greatswords
                                     :arquebusiers :musketeers :light-cavalry :lancers)
  "Dotations du tournoi : chacune affronte chacune des autres une fois.")
(defparameter *matchup-count* 105 "Effectif d'un régiment du tournoi, sauf dans *MATCHUP-COUNTS*.")
(defparameter *matchup-counts* '((:pikemen . 225) (:light-cavalry . 60) (:lancers . 60))
  "Effectifs propres à certaines dotations du tournoi : (dotation . effectif).")
(defparameter *matchup-drill* :countermarch "Exercice de tir des tireurs du tournoi.")
(defparameter *matchup-gap* 90
  "Distance entre les deux régiments d'un duel du tournoi, en mètres : au-delà de
la portée du mousquet (80 m), personne ne tire avant la charge.")
(defparameter *matchup-row* 70 "Distance entre deux duels d'une même colonne, en mètres.")
(defparameter *matchup-column* 140
  "Distance du centre à chacune des deux colonnes de duels, en mètres : chaque
régiment est plus près de son adversaire que de tout ennemi de l'autre colonne.")

(defun matchup-regiments (&key (dotations *matchup-dotations*) (count *matchup-count*)
                               (drill *matchup-drill*)
                               (pairs (loop for (a . rest) on dotations
                                            nconc (loop for b in rest collect (list a b)))))
  "Régiments du tournoi : un duel par paire de DOTATIONS, COUNT hommes de chaque
côté (ou celui de la dotation dans *MATCHUP-COUNTS*), la première dotation de la paire en rouge, à gauche. Deux colonnes de
duels, l'une sous l'autre, rouge et bleu face à face, hors de portée. PAIRS
remplace les paires tirées de DOTATIONS."
  (let* ((rows (ceiling (length pairs) 2))
         (half-gap (/ *matchup-gap* 2)))
    (loop for (red blue) in pairs
          for k from 0
          for column = (if (< k rows) -1 1)
          for row = (mod k rows)
          for cx = (* column *matchup-column*)
          for y = (* *matchup-row* (- row (/ (1- rows) 2)))
          nconc (flet ((size (dotation)
                         (or (cdr (assoc dotation *matchup-counts*)) count)))
                  (list (list 0 red (size red) (- cx half-gap) y 0 drill)
                        (list 1 blue (size blue) (+ cx half-gap) y 180 drill))))))

(defparameter *matchup-demo-regiments* (matchup-regiments)
  "Tournoi : chaque dotation contre chacune des autres, vingt-huit duels côte à
côte que C lance ensemble, 105 hommes de chaque côté, sauf 225 piquiers, 60
chevau-légers et 60 lanciers (*MATCHUP-COUNTS*). Le tableau des duels (tally-view.lisp) compte
les survivants.")

(defparameter *cavalry-demo-regiments*
  (matchup-regiments :pairs (mapcar (lambda (other) (list :light-cavalry other))
                                    (remove :light-cavalry *matchup-dotations*)))
  "Chevau-légers (60, à l'épée) contre chaque autre dotation, cinq duels que C
lance ensemble : la charge contre les piques, les hallebardes, les rodeleros et
les tireurs.")

(defparameter *demo-regiments* *matchup-demo-regiments*
  "Régiments de la démonstration : (camp dotation effectif x y orientation en
degrés [exercice de tir] [posture]). Sans exercice (ou NIL), le feu à volonté
pour une arme de tir ; sans posture, DEFAULT-STANCE (:CHASE pour la cavalerie, :HOLD sinon). Le tournoi par défaut ;
*CAVALRY-DEMO-REGIMENTS* garde les duels des chevau-légers ;
*DUEL-DEMO-REGIMENTS* garde le duel d'arquebusiers en contremarche.")

(defparameter *demo-names* '((:pikemen . "Piquiers") (:rodeleros . "Rodeleros")
                              (:halberdiers . "Hallebardiers")
                              (:arquebusiers . "Arquebusiers")
                              (:musketeers . "Mousquetaires")
                              (:greatswords . "Espadonniers")
                              (:light-cavalry . "Chevau-légers")
                              (:lancers . "Lanciers"))
  "Nom affiché de chaque dotation.")

(defun demo-battlefield-size (regiments &key (margin 64))
  "Côté de la carte : *BATTLEFIELD-SIZE*, ou plus pour que tous les REGIMENTS y
tiennent avec MARGIN mètres autour, arrondi à 64 m."
  (let ((extent (loop for (nil nil nil x y) in regiments maximize (max (abs x) (abs y)))))
    (max *battlefield-size* (* 64 (ceiling (* 2 (+ extent margin)) 64)))))

(defun make-demo-battle (&key (seed *battle-seed*) (scatter *demo-scatter*)
                              (regiments *demo-regiments*))
  (let ((battle (make-battle :seed seed :size (demo-battlefield-size regiments))))
    (loop for (side dotation count x y degrees drill stance) in regiments
          do (add-regiment battle (dotation-stats dotation) count
                           :name (format nil "~a ~a"
                                         (or (cdr (assoc dotation *demo-names*))
                                             (string-capitalize dotation))
                                         (if (zerop side) "rouges" "bleus"))
                           :side side :x x :y y :facing (* degrees (/ pi 180))
                           :scatter scatter
                           :stance (or stance (default-stance (dotation-stats dotation)))
                           :drill (or drill
                                      (and (eq (attack-kind (stats-primary (dotation-stats dotation)))
                                               :ranged)
                                           :at-will))))
    battle))

(defun order-random-moves (battle rng &key (half-width 30) (half-height 20))
  "Envoie chaque régiment vers un point et une orientation tirés avec RNG, dans
un rectangle centré sur l'origine. RNG n'est pas celui de la bataille : un ordre
est une commande du joueur, pas un tirage de la simulation."
  (let ((half-width (float half-width 1f0))
        (half-height (float half-height 1f0)))
    (loop for regiment across (battle-regiments battle)
          do (order-move regiment
                         (rng-range rng (- half-width) half-width)
                         (rng-range rng (- half-height) half-height)
                         (rng-range rng 0f0 (float (* 2 pi) 1f0))))))

(defparameter *demo-charge-sides* '(0 1)
  "Camps que C lance à l'attaque : les deux. '(1) pour les bleus seuls, les
rouges attendant en défense.")

(defun order-charge (battle &key (sides '(0 1)))
  "Lance chaque régiment des camps SIDES sur le régiment ennemi vivant le plus
proche."
  (let ((regiments (battle-regiments battle))
        (soldiers (battle-soldiers battle)))
    (loop for regiment across regiments
          when (and (member (regiment-side regiment) sides)
                    (plusp (regiment-alive regiment)) (not (regiment-routed-p regiment)))
            do (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
                 (let ((nearest nil) (best most-positive-single-float))
                   (loop for other across regiments
                         when (and (/= (regiment-side other) (regiment-side regiment))
                                   (plusp (regiment-alive other))
                                   (not (regiment-routed-p other)))
                           do (multiple-value-bind (ox oy) (regiment-centroid other soldiers)
                                (let ((d (+ (expt (- ox x) 2) (expt (- oy y) 2))))
                                  (when (< d best) (setf best d nearest other)))))
                   (when nearest (order-attack regiment nearest)))))))
