(in-package #:rogue-company/tests)

;;; Couche stratégique de l'IA : groupes, ligne alignée, gardes en laisse,
;;; réserve qui vient au secours.

(def-suite ai-plan :description "Général de l'IA : groupes et tâches.")
(in-suite ai-plan)

(defun ai-plan-of (battle)
  (rogue-company::commander-plan (first (rogue-company::battle-commanders battle))))

(defun task-of (battle regiment)
  (rogue-company::task-kind (ai-plan-of battle) regiment))

(test groups-follow-the-roles
  "Bataille contre l'IA : piques en ligne, tireurs en écran, rodeleros et
espadonniers aux flancs, hallebardiers en réserve, cavalerie aux ailes."
  (let ((b (rogue-company::make-ai-battle)))
    (run-ticks b 30)
    (dolist (r (regiments b))
      (when (= 1 (rogue-company::regiment-side r))
        (is (eq (ecase (rogue-company::regiment-role r)
                  (:pikes :line) (:shooters :screen) (:cavalry :wing)
                  (:foot (if (eq :halberd (getf (rogue-company::stats-loadout
                                                 (rogue-company::regiment-stats r))
                                                :main))
                             :reserve :guard)))
                (task-of b r))
            "~a" (rogue-company::regiment-name r))))))

(test line-advances-aligned
  "Contre une armée qui attend, la ligne de l'IA avance alignée (ses piques à
moins de 6 m de la même profondeur) jusqu'à s'engager."
  (let ((b (rogue-company::make-ai-battle))
        (spread 0f0))
    (loop repeat (* 30 150)
          for plan = (ai-plan-of b)
          until (and plan (eq :engaged (rogue-company::plan-phase plan)))
          do (rogue-company::battle-tick b)
             (when plan
               (let ((depths (loop for k in (rogue-company::plan-line plan)
                                   for r = (nth k (regiments b))
                                   when (rogue-company::fighting-p r)
                                     collect (nth-value 1 (rogue-company::regiment-coordinates
                                                           plan r (rogue-company::battle-soldiers b))))))
                 (when depths
                   (setf spread (max spread (- (reduce #'max depths) (reduce #'min depths))))))))
    (is (eq :engaged (rogue-company::plan-phase (ai-plan-of b))) "elle s'engage")
    (is (< spread 6f0) "écart de profondeur ~,1f m" spread)))

(test line-waits-for-the-skirmish
  "La ligne s'arrête dès que ses tireurs ont l'ennemi à portée, attend pendant
l'escarmouche, puis donne l'assaut jusqu'au contact."
  (let ((b (rogue-company::make-ai-battle))
        (seen '()) (depth nil) (moved nil))
    (loop repeat (* 30 150)
          for plan = (ai-plan-of b)
          until (and plan (eq :engaged (rogue-company::plan-phase plan)))
          do (rogue-company::battle-tick b)
             (when plan
               (let ((phase (rogue-company::plan-phase plan)))
                 (pushnew phase seen)
                 ;; Pendant une même escarmouche (la ligne se rapproche entre
                 ;; deux si l'ennemi sort de portée).
                 (cond ((not (eq phase :skirmish)) (setf depth nil))
                       ((null depth) (setf depth (rogue-company::plan-depth plan)))
                       ((/= depth (rogue-company::plan-depth plan)) (setf moved t))))))
    (is (member :skirmish seen) "escarmouche")
    (is (member :assault seen) "assaut")
    (is (not moved) "la ligne ne bouge pas pendant l'escarmouche")
    (is (eq :engaged (rogue-company::plan-phase (ai-plan-of b))) "puis le contact")))

(test guards-stay-on-the-leash
  "Des gardes ne courent pas après des tireurs isolés loin de leur place, que le
choix glouton aurait attaqués."
  (let ((b (ai-battle '(0 :pikemen 225 -60 0 0) '(0 :arquebusiers 105 60 160 180)
                      '(1 :pikemen 225 60 0 180)
                      '(1 :rodeleros 105 62 -30 180) '(1 :rodeleros 105 62 30 180))))
    (let ((chased nil))
      (dotimes (k (* 30 20))
        (rogue-company::battle-tick b)
        (dolist (guard (subseq (regiments b) 3 5))
          (when (equal '(:attack 1) (rogue-company::regiment-orders guard))
            (setf chased k))))
      (is (null chased) "poursuite au tick ~a" chased))
    (is (eq :guard (task-of b (nth 3 (regiments b)))))))

(test reserve-comes-to-a-wavering-line
  "Quand les piques de la ligne flanchent au combat, la réserve attaque l'ennemi
qu'elles combattent."
  (let* ((b (ai-battle '(0 :pikemen 225 -22 0 0)
                       '(1 :pikemen 225 0 0 180)
                       '(1 :rodeleros 105 2 -60 180) '(1 :rodeleros 105 2 60 180)
                       '(1 :halberdiers 105 35 0 180)))
         (rs (regiments b))
         (pikes (nth 1 rs))
         (reserve (nth 4 rs)))
    (run-ticks b (* 30 12))
    (is (eq :reserve (task-of b reserve)))
    (is-true (rogue-company::regiment-in-melee-p pikes) "la ligne se bat")
    (setf (rogue-company::regiment-morale pikes) 30f0)
    (run-ticks b (* 30 2))
    (is (equal '(:attack 0) (rogue-company::regiment-orders reserve)))))

;;; IA contre IA, banc d'essai

(test ai-versus-ai-is-decided
  "Deux IA s'affrontent et la bataille se décide."
  (let ((b (rogue-company::make-ai-battle :sides '(0 1))))
    (loop repeat (* 30 300) until (rogue-company::battle-outcome b)
          do (rogue-company::battle-tick b))
    (is (integerp (rogue-company::battle-outcome b)))))

(test settings-belong-to-one-commander
  "Un réglage ne vaut que pour son commandant : sans plan, les bleus n'ont pas de
général ; les rouges en ont un."
  (let ((b (rogue-company::make-ai-battle :sides '(0 1)
                                          :settings '((1 rogue-company::*ai-use-plan* nil)))))
    (run-ticks b 60)
    (destructuring-bind (red blue) (rogue-company::battle-commanders b)
      (is-true (rogue-company::commander-plan red))
      (is-false (rogue-company::commander-plan blue)))
    (is-true rogue-company::*ai-use-plan* "la valeur globale ne bouge pas")))

(test only-ai-settings-are-accepted
  "Un réglage de commandant ne peut lier qu'une variable *AI-…* : un replay ne
peut rien lier d'autre."
  (signals error (rogue-company::make-ai-commander 0 '(*read-eval* t)))
  (signals error (rogue-company::make-ai-commander 0 '(rogue-company::*battle-seed* 3)))
  (finishes (rogue-company::make-ai-commander 0 '(rogue-company::*ai-line-bound* 12f0))))

(test plan-beats-greedy
  "Banc d'essai : la couche stratégique bat le choix glouton seul, chacun dans
les deux camps, sur deux graines."
  (let ((result (rogue-company::ai-match :a '() :b '(rogue-company::*ai-use-plan* nil)
                                         :seeds '(1 4))))
    (is (> (getf result :a-wins) (getf result :b-wins))
        "~a" (rogue-company::print-ai-match result nil))))

(test spectator-end-screen-names-the-colour
  "Entre deux IA, l'écran de fin nomme le camp vainqueur, pas « Victoire »."
  (let ((b (rogue-company::make-ai-battle :sides '(0 1))))
    (loop for r in (regiments b)
          when (= 1 (rogue-company::regiment-side r))
            do (rogue-company::rout b r))
    (run-ticks b 1)
    (is (string= "Victoire des rouges" (getf (rogue-company::end-screen b) :title)))))

;;; Cavalerie des ailes

(test cavalry-goes-to-its-wing
  "Chaque régiment de cavalerie va sur l'aile de son côté, au-delà des gardes."
  (let ((b (rogue-company::make-ai-battle)))
    (run-ticks b 30)
    (let ((plan (ai-plan-of b)))
      (dolist (r (regiments b))
        (when (and (= 1 (rogue-company::regiment-side r))
                   (eq :cavalry (rogue-company::regiment-role r)))
          (let ((task (gethash (rogue-company::regiment-index r) (rogue-company::plan-tasks plan))))
            (is (eq :wing (getf task :kind)))
            (is (> (abs (getf task :lateral)) 60f0) "au-delà du bout de la ligne")
            (is (= (signum (getf task :lateral))
                   (signum (nth-value 0 (rogue-company::regiment-coordinates
                                         plan r (rogue-company::battle-soldiers b))))))))))))

(test cavalry-does-not-charge-pikes-head-on
  "Seule devant des piques de face, la cavalerie d'une aile ne les charge pas."
  (let ((b (ai-battle '(0 :pikemen 225 -60 0 0) '(1 :lancers 60 40 0 180))))
    (dotimes (k (* 30 30))
      (rogue-company::battle-tick b)
      (is (not (equal '(:attack 0) (rogue-company::regiment-orders (nth-regiment b 1))))))))

(test chase-stops-short-of-pikes
  "Avec *AI-CAVALRY-RALLY*, une cavalerie en poursuite qui arrive à moins de *AI-RALLY-DANGER* de piques
ennemies en bon ordre, de face, est rappelée."
  (let* ((b (rogue-company::make-ai-battle
             :regiments '((0 :pikemen 225 -20 0 0) (1 :lancers 60 20 0 180))
             :sides '(1) :settings '((1 rogue-company::*ai-cavalry-rally* t))))
         (lancers (nth-regiment b 1)))
    (run-ticks b 1)
    (setf (rogue-company::regiment-chasing-p lancers) t)
    (run-ticks b 31)
    (is-false (rogue-company::regiment-chasing-p lancers))
    (is (eq :move (first (rogue-company::regiment-orders lancers))))))

(test wing-place-backs-away-from-infantry
  "Avec *AI-CAVALRY-EVASION*, la place d'une aile recule tant que de l'infanterie ennemie libre en est trop
près : des rodeleros posés sur la place des lanciers la font reculer hors de
leur portée."
  (let ((b (ai-battle '(0 :rodeleros 105 -60 120 0)
                      '(1 :pikemen 225 60 0 180) '(1 :lancers 60 62 60 180))))
    (run-ticks b 31)
    (let* ((plan (ai-plan-of b))
           (rodeleros (nth-regiment b 0))
           (lancers (nth-regiment b 2))
           (soldiers (rogue-company::battle-soldiers b)))
      (multiple-value-bind (x y) (rogue-company::task-destination plan lancers)
        ;; Les rodeleros, déplacés d'un bloc sur la place des lanciers.
        (multiple-value-bind (cx cy) (rogue-company::regiment-centroid rodeleros soldiers)
          (loop for i from (rogue-company::regiment-start rodeleros)
                  below (rogue-company::regiment-end rodeleros)
                do (incf (aref (rogue-company::soldiers-x soldiers) i) (- x cx))
                   (incf (aref (rogue-company::soldiers-y soldiers) i) (- y cy))))
        (rogue-company::update-regiment-bounds rodeleros soldiers)
        (multiple-value-bind (sx sy) (let ((rogue-company::*ai-cavalry-evasion* t))
                                       (rogue-company::safe-wing-place b plan lancers))
          (is-true (rogue-company::infantry-danger-p b 1 x y) "place d'origine menacée")
          (is-false (rogue-company::infantry-danger-p b 1 sx sy) "place reculée hors de portée"))))))

(test guards-answer-the-shooters-call
  "Des tireurs de l'écran que de la cavalerie ennemie charge appellent à l'aide ;
une garde à portée de l'appel attaque la cavalerie."
  (let* ((b (ai-battle '(0 :light-cavalry 60 -20 30 0)
                       '(1 :pikemen 225 60 0 180) '(1 :arquebusiers 105 40 0 180)
                       '(1 :rodeleros 105 62 -30 180) '(1 :halberdiers 105 62 30 180)))
         (cavalry (nth-regiment b 0))
         (called nil) (answered nil))
    (rogue-company::order-attack cavalry (nth-regiment b 2))
    (loop repeat (* 30 20)
          until answered
          do (rogue-company::battle-tick b)
             (when (rogue-company::commander-calls (first (rogue-company::battle-commanders b)))
               (setf called t))
             (setf answered (some (lambda (k) (eq cavalry (rogue-company::target-of b (nth-regiment b k))))
                                  '(3 4))))
    (is-true called "les tireurs appellent")
    (is-true answered "une garde répond")))

(test scripted-orders-mirror-the-armies
  ;; Le banc d'essai contre un joueur scripté (ai-bench.lisp) : un ordre du camp
  ;; rouge redonné aux bleus vise le régiment symétrique, au point symétrique.
  (let ((regiments rogue-company::*ai-battle-regiments*))
    (is (= 13 (rogue-company::mirror-index regiments 0)))
    (is (= 0 (rogue-company::mirror-index regiments 13)))
    (is (equal '(:attack (11 18) 0)
               (rogue-company::mirror-order regiments '(:attack (2 7) 13))))
    (destructuring-bind (kind indices x y facing)
        (rogue-company::mirror-order regiments '(:move (8) 100f0 -136f0 0f0))
      (is (eq :move kind))
      (is (equal '(19) indices))
      (is (= -100 x))
      (is (= 136 y))
      (is (< (abs (- facing pi)) 1e-5)))))

(test scripted-orders-replay-at-their-tick
  ;; Le commandant scripté donne chaque ordre à son tick, et saute un ordre
  ;; devenu sans objet (cible morte).
  (let* ((battle (rogue-company::make-ai-battle :seed 1 :sides '(1)))
         (script (make-instance 'rogue-company::script-commander
                                :side 0
                                :orders (vector '(3 :attack (0) 13) '(5 :attack (1) 12)))))
    (push script (rogue-company::battle-commanders battle))
    (setf (rogue-company::regiment-alive (aref (rogue-company::battle-regiments battle) 12)) 0)
    (loop repeat 4 do (rogue-company::battle-tick battle))
    (is (equal '(:attack 13)
               (rogue-company::regiment-orders (aref (rogue-company::battle-regiments battle) 0))))
    (loop repeat 3 do (rogue-company::battle-tick battle))
    (is (= 2 (rogue-company::script-cursor script)))
    (is (not (eq :attack (first (rogue-company::regiment-orders
                                 (aref (rogue-company::battle-regiments battle) 1))))))))

(test foot-lets-free-cavalry-go
  ;; Des piques qui attaquent une cavalerie libre, qu'elles ne rattraperont pas,
  ;; s'arrêtent ; des tireurs peuvent la viser.
  (let* ((battle (rogue-company::make-ai-battle :seed 1 :sides '(1)))
         (regiments (rogue-company::battle-regiments battle))
         (pikes (aref regiments 12))
         (shooters (aref regiments 15))
         (lancers (aref regiments 9)))
    (rogue-company::battle-tick battle)
    (is (not (rogue-company::catchable-p pikes lancers)))
    (is (rogue-company::catchable-p shooters lancers))
    (is (rogue-company::catchable-p pikes (aref regiments 1)))
    (rogue-company::order-attack pikes lancers)
    (rogue-company::drop-lost-target battle pikes)
    (is (eq :move (first (rogue-company::regiment-orders pikes))))))
