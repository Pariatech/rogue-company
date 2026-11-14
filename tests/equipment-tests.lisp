(in-package #:rogue-company/tests)

(def-suite equipment :description "Équipement, validation et stats effectives.")
(in-suite equipment)

(defun dotation (name)
  (rogue-company::dotation-stats name))

(defun stats (loadout &rest keys)
  (apply #'rogue-company::compute-regiment-stats :recruit loadout keys))

(defun reasons (loadout)
  (rogue-company::validate-loadout loadout))

(defun mentions-p (fragment reasons)
  (some (lambda (reason) (search fragment reason)) reasons))

(test every-dotation-is-valid
  (is (every (lambda (dotation) (null (reasons (third dotation))))
             rogue-company::*dotations*)))

(test pikemen
  (let ((s (dotation :pikemen)))
    (is (= 6 (rogue-company::stats-armor s)) "corselet 5 + morion 1")
    (is (= 16.5 (rogue-company::stats-load s)) "pique 5 + dague 0,5 + corselet 9 + morion 2")
    (is (= 8 (rogue-company::stats-ranks s)))
    (let ((pike (rogue-company::stats-primary s))
          (dagger (rogue-company::stats-secondary s)))
      (is (eq :melee (rogue-company::attack-kind pike)))
      (is (= 1.5 (rogue-company::attack-min-reach pike)))
      (is (= 4.5 (rogue-company::attack-reach pike)))
      (is (= 45 (rogue-company::attack-cooldown pike)) "1,5 s à 30 ticks/s")
      (is (= 15 (rogue-company::attack-ready-time pike)))
      (is (= 0.6 (rogue-company::attack-reach dagger))))))

(test musketeers
  (let* ((s (dotation :musketeers))
         (musket (rogue-company::stats-primary s)))
    (is (eq :ranged (rogue-company::attack-kind musket)))
    (is (= 80 (rogue-company::attack-reach musket)))
    (is (= 600 (rogue-company::attack-cooldown musket)) "20 s de rechargement")
    (is-true (rogue-company::attack-volley-p musket))
    (is (= 1.2 (rogue-company::stats-spacing s)))
    (is (= 30 (rogue-company::attack-ready-time (rogue-company::stats-secondary s))))))

(test mount-sets-speed-and-carries-no-load
  (let ((s (dotation :lancers)))
    (is (= 3.2 (rogue-company::stats-speed s)))
    (is (= 4.5 (rogue-company::stats-mass s)))
    (is (= 32 (rogue-company::stats-load s)) "lance, épée, armure et morion ; le cheval ne compte pas")
    (is (equal '(:charge) (rogue-company::stats-traits s)))))

(test pavise
  (let ((s (dotation :crossbowmen)))
    (is (= 0.5 (rogue-company::stats-cover s)))
    (is (member :deployed-cover (rogue-company::stats-traits s)))))

(test improvised-secondary
  (let ((secondary (rogue-company::stats-secondary (stats '(:main :musket)))))
    (is (eq :melee (rogue-company::attack-kind secondary)))
    (is (= 0.8 (rogue-company::attack-reach secondary)))))

(test set-then-add-then-mul
  "L'ordre des opérations ne dépend pas de l'ordre des modificateurs."
  (is (= 12 (rogue-company::stats-armor
             (stats '(:armor :corselet) :modifiers '((:armor :mul 2) (:armor :add 1)))))))

(test attack-designators
  (let ((s (stats '(:main :pistols :secondary :sword :mount :light-horse)
                  :modifiers '(((:melee :damage) :add 3) ((:primary :reach) :mul 2)))))
    (is (= 17 (rogue-company::attack-damage (rogue-company::stats-secondary s))))
    (is (= 50 (rogue-company::attack-damage (rogue-company::stats-primary s))))
    (is (= 30 (rogue-company::attack-reach (rogue-company::stats-primary s))))))

(test hands
  (is (mentions-p "mains" (reasons '(:main :pike :shield :rodela))))
  (is (null (reasons '(:main :sword :shield :rodela))))
  (is (null (reasons '(:main :crossbow :shield :pavise))) "la pavise n'occupe pas de main")
  (is (null (reasons '(:main :musket :secondary :sword))) "l'arme secondaire ne compte pas"))

(test requires-and-forbids
  (is (mentions-p "demande une monture" (reasons '(:main :lance))))
  (is (null (reasons '(:main :lance :mount :light-horse))))
  (is (mentions-p "incompatible avec une monture" (reasons '(:main :pike :mount :light-horse))))
  (is (mentions-p "incompatible avec une monture"
                  (reasons '(:main :sword :shield :pavise :mount :heavy-horse)))))

(test slots-and-unknowns
  (is (mentions-p "ne va pas" (reasons '(:main :dagger))))
  (is (mentions-p "ne va pas" (reasons '(:shield :sword))))
  (is (mentions-p "inconnu" (reasons '(:main :laser))))
  (is (mentions-p "inconnu" (reasons '(:cape :buff-coat))))
  (signals error (stats '(:main :pike :shield :rodela))))

(test definitions-are-checked
  (signals error (rogue-company::define-item :test-bad "x" :slots '(:armor)
                                             :modifiers '((:armour :add 1))))
  (signals error (rogue-company::define-item :test-bad "x" :slots '(:armor)
                                             :modifiers '((:armor :plus 1))))
  (signals error (rogue-company::define-item :test-bad "x" :slots '(:cape)))
  (signals error (rogue-company::define-item :test-bad "x" :slots '(:main)
                                             :attack '(:kind :magic)))
  (signals error (rogue-company::define-man :test-bad "x" :hit-points 10))
  (is (null (gethash :test-bad rogue-company::*item-templates*))))
