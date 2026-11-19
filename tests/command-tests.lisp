(in-package #:rogue-company/tests)

;;; Commandement du joueur : les fonctions d'ordre, sans souris ni fenêtre.

(def-suite command :description "Choix des régiments et ordres du joueur.")
(in-suite command)

(defun command-battle ()
  "Deux régiments rouges (le joueur) côte à côte, un régiment bleu en face."
  (let ((b (rogue-company::make-battle :seed 1)))
    (rogue-company::add-regiment b (rogue-company::dotation-stats :pikemen) 225 :side 0 :x 0 :y -20)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :arquebusiers) 105 :side 0 :x 0 :y 20)
    (rogue-company::add-regiment b (rogue-company::dotation-stats :rodeleros) 105
                                 :side 1 :x 60 :y 0 :facing pi)
    b))

(test pick-a-regiment-by-its-soldiers
  (let* ((b (command-battle))
         (rs (regiments b)))
    (is (eq (first rs) (rogue-company::regiment-at b 0 -20)))
    (is (eq (third rs) (rogue-company::regiment-at b 60 0)) "un ennemi aussi, pour l'attaquer")
    (is (null (rogue-company::regiment-at b 30 60)) "rien dans le vide")))

(test box-selects-only-the-player-regiments
  (let ((b (command-battle)))
    (is (equal '(0 1) (rogue-company::regiments-in-box b -50 -50 100 50)))
    (is (equal '(0) (rogue-company::regiments-in-box b -50 -50 50 0)))))

(test group-move-keeps-relative-places
  "Deux régiments envoyés ensemble gardent leur écart, fronts dans le sens de la
marche."
  (let* ((b (command-battle))
         (rs (subseq (regiments b) 0 2)))
    (rogue-company::command-move b rs 0 100)
    (destructuring-bind (pikes shooters) rs
      (let ((o1 (rogue-company::regiment-orders pikes))
            (o2 (rogue-company::regiment-orders shooters)))
        (is (eq :move (first o1)))
        (is (near 40 (- (third o2) (third o1)) 0.5) "écart de 40 m gardé")
        (is (near (/ pi 2) (fourth o1) 1f-3) "front vers le sud, sens de la marche")))))

(test attack-and-halt
  (let* ((b (command-battle))
         (rs (regiments b))
         (pikes (first rs)))
    (rogue-company::command-attack (list pikes) (third rs))
    (is (equal '(:attack 2) (rogue-company::regiment-orders pikes)))
    (rogue-company::command-halt b (list pikes))
    (is (eq :move (first (rogue-company::regiment-orders pikes))))
    (is (near 0 (second (rogue-company::regiment-orders pikes)) 0.5) "arrêté où il est")))

(test drills-cycle-and-reshape
  "E fait passer les piques en carré et en revient ; les tireurs parcourent
leurs exercices, rangs refaits (deux pour la salve)."
  (let* ((b (command-battle))
         (rs (regiments b))
         (pikes (first rs))
         (shooters (second rs)))
    (rogue-company::cycle-drill pikes)
    (is (eq :square (rogue-company::regiment-drill pikes)))
    (rogue-company::cycle-drill pikes)
    (is (null (rogue-company::regiment-drill pikes)))
    (is (eq :at-will (rogue-company::regiment-drill shooters)))
    (rogue-company::cycle-drill shooters)
    (is (eq :volley (rogue-company::regiment-drill shooters)))
    (is (= 2 (rogue-company::formation-ranks (rogue-company::regiment-formation shooters))))
    (run-ticks b 30)
    (is (= 2 (rogue-company::regiment-depth shooters)) "reformé sur deux rangs")
    (is (null (rogue-company::drill-choices (third rs))) "rien pour les rodeleros")))

(test stances-cycle
  (let* ((b (command-battle))
         (rs (regiments b)))
    (rogue-company::cycle-stance (first rs))
    (is (eq :chase (rogue-company::regiment-stance (first rs))))
    (rogue-company::cycle-stance (second rs))
    (is (eq :skirmish (rogue-company::regiment-stance (second rs))) "des tireurs en tirailleurs")))
