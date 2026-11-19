(in-package #:rogue-company/tests)

;;; Charge, piques contre la charge, carré, percée, désordre, tireurs en mêlée,
;;; arme montrée en main : des comportements, éprouvés par de petits duels
;;; joués jusqu'à la débandade d'un camp.

(def-suite charge :description "Charge, percée, désordre, carré et tireurs en mêlée.")
(in-suite charge)

(defun charge-duel (attacker count defender defender-count &key (side :front) drill
                                                             (seconds 120) watch)
  "Régiment ATTACKER (COUNT hommes) lancé sur DEFENDER (DEFENDER-COUNT, exercice
DRILL, tourné vers l'est), qui l'attend sur place ; ATTACKER arrive de SIDE
(:front, :flank ou :rear). Joue jusqu'à la débandade d'un camp, au plus SECONDS,
en appelant WATCH (bataille, attaquant, défenseur) à chaque tick. Renvoie la
bataille, l'attaquant et le défenseur."
  (let ((b (rogue-company::make-battle :seed 1)))
    (rogue-company::add-regiment b (rogue-company::dotation-stats defender) defender-count
                                 :side 0 :x 0 :y 0 :facing 0 :drill drill)
    (multiple-value-bind (x y facing)
        (ecase side
          (:front (values 45 0 pi))
          (:flank (values 0 -45 (/ pi 2)))
          (:rear (values -45 0 0)))
      (rogue-company::add-regiment b (rogue-company::dotation-stats attacker) count
                                   :side 1 :x x :y y :facing facing))
    (destructuring-bind (defending attacking) (regiments b)
      (rogue-company::order-attack attacking defending)
      (loop repeat (* 30 seconds)
            until (some #'rogue-company::regiment-routed-p (list attacking defending))
            do (rogue-company::battle-tick b)
               (when watch (funcall watch b attacking defending)))
      (values b attacking defending))))

;;; Bonus de charge

(test charge-bonus-is-momentum-plus-weapon
  "Bonus de charge : masse × vitesse de course × *CHARGE-MOMENTUM-DAMAGE*, plus le
bonus de charge de l'arme. Les piquiers ne chargent pas, les tireurs non plus."
  (flet ((regiment (dotation)
           (rogue-company::add-regiment (rogue-company::make-battle)
                                        (rogue-company::dotation-stats dotation) 1)))
    (let ((k rogue-company::*charge-momentum-damage*))
      (is (near (* k 1 3) (rogue-company::regiment-charge-bonus (regiment :rodeleros)) 1f-3))
      (is (near (+ (* k 1 3) 20) (rogue-company::regiment-charge-bonus (regiment :greatswords)) 1f-3)
          "l'espadon ajoute son bonus")
      (is (near (+ (* k 9 6.5) 40) (rogue-company::regiment-charge-bonus (regiment :lancers)) 1f-2)
          "cheval lourd et lance"))
    (is-true (rogue-company::charge-capable-p (regiment :rodeleros)))
    (is-false (rogue-company::charge-capable-p (regiment :pikemen)) "les piques gardent leur bloc")
    (is-false (rogue-company::charge-capable-p (regiment :arquebusiers)))))

(test cavalry-gallops-into-the-charge
  "Des lanciers lancés sur des espadonniers partent au galop avant le contact,
puis ouvrent leur bonus de charge au premier contact."
  (let ((galloped nil) (contact nil))
    (charge-duel :lancers 60 :greatswords 105
                 :seconds 40
                 :watch (lambda (b attacking defending)
                          (declare (ignore b defending))
                          (when (rogue-company::regiment-charging-p attacking) (setf galloped t))
                          (when (>= (rogue-company::regiment-charge-contact attacking) 0)
                            (setf contact t))))
    (is-true galloped)
    (is-true contact)))

;;; Piques contre la charge

(test pikes-break-a-frontal-charge
  "Chargées de face, des piques volent la charge : les lanciers se débandent
sans presque rien leur tuer, et aucun ne prend d'élan contre elles."
  (let ((momentum nil))
    (multiple-value-bind (b lancers pikes)
        (charge-duel :lancers 60 :pikemen 225
                     :watch (lambda (b attacking defending)
                              (declare (ignore defending))
                              (let ((m (rogue-company::soldiers-momentum (rogue-company::battle-soldiers b))))
                                (loop for i from (rogue-company::regiment-start attacking)
                                        below (rogue-company::regiment-end attacking)
                                      when (plusp (aref m i)) do (setf momentum t)))))
      (declare (ignore b))
      (is-true (rogue-company::regiment-routed-p lancers))
      (is (> (rogue-company::regiment-alive pikes) 215) "presque aucun piquier tué")
      (is-false momentum "pas d'élan contre des piques de face"))))

(test pike-square-has-no-flank
  "Chargées de dos, des piques ordinaires se débandent ; en carré, elles brisent
la même charge sans perte."
  (multiple-value-bind (b lancers pikes) (charge-duel :light-cavalry 60 :pikemen 225 :side :rear)
    (declare (ignore b lancers))
    (is-true (rogue-company::regiment-routed-p pikes) "des piques ordinaires prises de dos"))
  (multiple-value-bind (b cavalry square)
      (charge-duel :light-cavalry 60 :pikemen 225 :side :rear :drill :square)
    (declare (ignore b))
    (is-true (rogue-company::regiment-routed-p cavalry))
    (is-false (rogue-company::regiment-routed-p square))
    (is (> (rogue-company::regiment-alive square) 215) "le carré n'a pas de dos")))

(test pike-square-holds-its-ground
  "Un carré attaqué ne quitte pas sa place : son centre bouge à peine."
  (let ((x0 nil) (drift 0f0))
    (charge-duel :rodeleros 105 :pikemen 225 :drill :square :seconds 60
                 :watch (lambda (b attacking defending)
                          (declare (ignore attacking))
                          (let ((x (rogue-company::regiment-centroid defending
                                                                     (rogue-company::battle-soldiers b))))
                            (if x0
                                (setf drift (max drift (abs (- x x0))))
                                (setf x0 x)))))
    (is (< drift 2f0))))

;;; Percée et désordre

(test charge-penetrates-and-disorders
  "Des lanciers lancés sur des espadonniers prennent leur élan, renversent des
hommes et mettent le régiment en désordre ; un cavalier lancé avance au-delà de
sa distance d'arrêt."
  (let ((momentum nil) (disorder 0f0) (inside nil))
    (charge-duel :lancers 60 :greatswords 105 :seconds 40
                 :watch (lambda (b attacking defending)
                          (let* ((s (rogue-company::battle-soldiers b))
                                 (m (rogue-company::soldiers-momentum s))
                                 (targets (rogue-company::soldiers-target s)))
                            (setf disorder (max disorder (rogue-company::regiment-disorder defending)))
                            (loop for i from (rogue-company::regiment-start attacking)
                                    below (rogue-company::regiment-end attacking)
                                  for target = (aref targets i)
                                  when (plusp (aref m i))
                                    do (setf momentum t)
                                       (when (and (>= target 0)
                                                  (< (- (sqrt (+ (expt (- (aref (rogue-company::soldiers-x s) target)
                                                                          (aref (rogue-company::soldiers-x s) i)) 2)
                                                                 (expt (- (aref (rogue-company::soldiers-y s) target)
                                                                          (aref (rogue-company::soldiers-y s) i)) 2)))
                                                        (aref (rogue-company::soldiers-radius s) i)
                                                        (aref (rogue-company::soldiers-radius s) target))
                                                     (rogue-company::regiment-hold-distance attacking)))
                                         (setf inside t))))))
    (is-true momentum "élan pris au contact")
    (is-true inside "le cavalier lancé passe sous sa distance d'arrêt")
    (is (> disorder 0.2f0) "désordre sous les hommes renversés")))

(test disorder-wears-morale-then-recovers
  "Le désordre use le moral, puis se résorbe au calme."
  (let* ((b (rogue-company::make-battle))
         (r (rogue-company::add-regiment b (rogue-company::dotation-stats :halberdiers) 105)))
    (setf (rogue-company::regiment-disorder r) 0.5f0)
    (run-ticks b (* 30 3))
    (is (< (rogue-company::regiment-morale r) 60f0) "moral usé")
    (is (< (rogue-company::regiment-disorder r) 0.5f0) "désordre résorbé en partie")
    (run-ticks b (* 30 10))
    (is (zerop (rogue-company::regiment-disorder r)))))

;;; Tireurs en mêlée

(test shooters-in-melee-draw-swords
  "Des arquebusiers frappés par des piques qu'ils n'atteignent pas sont en mêlée :
ils cessent de tirer et tirent tous l'épée."
  (let ((assailed nil) (shot-in-melee nil) (all-swords nil))
    (charge-duel :pikemen 225 :arquebusiers 105 :seconds 90
                 :watch (lambda (b attacking defending)
                          (declare (ignore attacking))
                          (let ((s (rogue-company::battle-soldiers b)))
                            (when (rogue-company::regiment-in-melee-p defending)
                              (setf assailed t)
                              (when (rogue-company::volley-fire-p defending) (setf shot-in-melee t))
                              (when (loop for i from (rogue-company::regiment-start defending)
                                            below (rogue-company::regiment-end defending)
                                          always (= 1 (aref (rogue-company::soldiers-weapon s) i)))
                                (setf all-swords t))))))
    (is-true assailed)
    (is-false shot-in-melee "plus de salves en mêlée")
    (is-true all-swords "tous à l'épée")))

;;; Dessin : arme en main

(test old-weapon-stays-in-hand-until-the-strike-ends
  "Un soldat qui vient de changer d'arme garde l'ancienne en main tant que
l'animation du coup qu'il en a porté avant le changement n'est pas finie."
  (let* ((stats (rogue-company::dotation-stats :lancers))
         (sword-ready (rogue-company::attack-ready-time (rogue-company::stats-secondary stats))))
    ;; Coup de lance il y a 8 ticks, épée prise il y a 2 ticks : la lance.
    (is (= 0 (rogue-company::shown-weapon stats 1 (- sword-ready 2) 8f0 15f0)))
    ;; Animation finie : l'épée.
    (is (= 1 (rogue-company::shown-weapon stats 1 (- sword-ready 2) 20f0 15f0)))
    ;; Coup porté après le changement (avec l'épée) : l'épée.
    (is (= 1 (rogue-company::shown-weapon stats 1 (- sword-ready 10) 4f0 15f0)))
    ;; Pas de changement en cours : l'arme tenue.
    (is (= 0 (rogue-company::shown-weapon stats 0 0 5f0 15f0)))))
