(in-package #:rogue-company/tests)

;;; Condition de victoire : armée brisée, débandade générale, vainqueur.

(def-suite victory :description "Condition de victoire d'une bataille rangée.")
(in-suite victory)

(defun victory-battle ()
  "Deux régiments par camp, loin les uns des autres, la victoire en jeu."
  (let ((b (rogue-company::make-battle :seed 1)))
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 225 :side 0 :x -60 :y -30)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :arquebusiers) 90 :side 0 :x -60 :y 30)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 225 :side 1 :x 60 :y -30 :facing pi)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :arquebusiers) 90 :side 1 :x 60 :y 30 :facing pi)
    (setf (rogue-company::battle-victory-p b) t)
    b))

(test undecided-while-armies-stand
  (let ((b (victory-battle)))
    (run-ticks b 30)
    (is (null (rogue-company::battle-outcome b)))))

(test broken-army-routes-and-loses
  "Les piques bleues débandées : il reste 90 hommes sur 315, moins de 30 % de
l'armée bleue, qui est brisée ; ses arquebusiers se débandent, les rouges
gagnent."
  (let* ((b (victory-battle))
         (rs (regiments b)))
    (rogue-company::rout b (third rs))
    (run-ticks b 1)
    (is (eql 0 (rogue-company::battle-outcome b)))
    (is (= 0 (rogue-company::battle-outcome-tick b)) "décidée au premier tick, où elle s'est brisée")
    (is-true (rogue-company::regiment-routed-p (fourth rs)) "tout ce qui restait fuit")
    (is-false (rogue-company::regiment-routed-p (first rs)))))

(test small-losses-do-not-break
  "Les arquebusiers débandés seuls : il reste 71 % de l'armée, rien n'est décidé."
  (let* ((b (victory-battle))
         (rs (regiments b)))
    (rogue-company::rout b (fourth rs))
    (run-ticks b 1)
    (is (null (rogue-company::battle-outcome b)))))

(test both-broken-is-a-draw
  (let* ((b (victory-battle))
         (rs (regiments b)))
    (rogue-company::rout b (first rs))
    (rogue-company::rout b (third rs))
    (run-ticks b 1)
    (is (eq :draw (rogue-company::battle-outcome b)))))

(test tournament-never-ends
  "Sans BATTLE-VICTORY-P (le tournoi), une armée débandée ne décide rien."
  (let* ((b (victory-battle))
         (rs (regiments b)))
    (setf (rogue-company::battle-victory-p b) nil)
    (rogue-company::rout b (third rs))
    (run-ticks b 1)
    (is (null (rogue-company::battle-outcome b)))))

(test summary-counts-the-armies
  (let* ((b (victory-battle))
         (rs (regiments b)))
    (rogue-company::rout b (third rs))
    (run-ticks b 1)
    (let ((blue (rogue-company::army-summary b 1)))
      (is (= 315 (getf blue :start)))
      (is (= 315 (getf blue :alive)))
      (is (= 2 (getf blue :routed)))
      (is (= 2 (getf blue :regiments))))))

(test end-screen-names-the-winner
  (let* ((b (victory-battle))
         (rs (regiments b)))
    (rogue-company::rout b (third rs))
    (run-ticks b 1)
    (let ((screen (rogue-company::end-screen b)))
      (is (string= "Victoire" (getf screen :title)))
      (is (equal '("" "Rouges (vous)" "Bleus") (first (getf screen :rows))))
      (is (equal '("Hommes" "315 / 315" "315 / 315") (second (getf screen :rows)))))))
