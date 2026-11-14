(in-package #:rogue-company)

;;; Catalogue : hommes de base, objets et dotations de démonstration. Valeurs de
;;; départ, à équilibrer en jouant. Recharger ce fichier redéfinit les objets ;
;;; les stats d'une bataille déjà créée ne changent pas.
;;;
;;; Distances en mètres, durées en secondes, poids en kilogrammes ; adresse,
;;; précision, défense et couverture en fractions.

;;; Hommes

(define-man :recruit "Recrue"
  :hp 100 :speed 1.3 :run-speed 3.0 :mass 1.0 :endurance 1.0
  :defense 0.05 :melee-skill 0.5 :marksmanship 0.4
  :morale 60 :discipline 0.5 :spacing 1.0 :ranks 4)

;;; Armes d'hast et de mêlée

(define-item :pike "Pique"
  :slots '(:main) :hands 2 :weight 5 :price 20
  :attack '(:kind :melee :min-reach 1.5 :reach 4.5 :damage 12 :penetration 2
            :cooldown 1.5 :ready-time 0.5)
  :modifiers '((:ranks :set 8))
  :forbids '(:mount))

(define-item :halberd "Hallebarde"
  :slots '(:main) :hands 2 :weight 3.5 :price 25
  :attack '(:kind :melee :min-reach 0.5 :reach 2.2 :damage 18 :penetration 4
            :cooldown 1.8 :ready-time 0.5))

(define-item :sword "Épée"
  :slots '(:main :secondary) :hands 1 :weight 1.5 :price 15
  :attack '(:kind :melee :reach 1.1 :damage 14 :penetration 1 :accuracy 0.1
            :cooldown 1.0 :ready-time 1.0))

(define-item :dagger "Dague"
  :slots '(:secondary) :hands 1 :weight 0.5 :price 5
  :attack '(:kind :melee :reach 0.6 :damage 8 :accuracy 0.05
            :cooldown 0.8 :ready-time 0.5))

(define-item :lance "Lance"
  :slots '(:main) :hands 1 :weight 3.5 :price 30
  :attack '(:kind :melee :min-reach 1.0 :reach 3.0 :damage 20 :penetration 4
            :cooldown 3.0 :charge-bonus 40 :ready-time 1.0)
  :requires '(:mount))

;;; Armes de tir

(define-item :musket "Mousquet"
  :slots '(:main) :hands 2 :weight 7 :price 40
  :attack '(:kind :ranged :volley t :range 80 :damage 60 :penetration 6
            :reload 20 :ready-time 1.0)
  :modifiers '((:spacing :set 1.2))
  :tags '(:firearm)
  :forbids '(:mount))

(define-item :arquebus "Arquebuse"
  :slots '(:main) :hands 2 :weight 4.5 :price 30
  :attack '(:kind :ranged :volley t :range 60 :damage 40 :penetration 4 :accuracy 0.05
            :reload 15 :ready-time 1.0)
  :modifiers '((:spacing :set 1.2))
  :tags '(:firearm))

(define-item :crossbow "Arbalète"
  :slots '(:main) :hands 2 :weight 4 :price 25
  :attack '(:kind :ranged :range 70 :damage 45 :penetration 5 :accuracy 0.1
            :reload 18 :ready-time 1.0)
  :forbids '(:mount))

(define-item :pistols "Pistolets"
  :slots '(:main) :hands 1 :weight 2.5 :price 35
  :attack '(:kind :ranged :range 15 :damage 50 :penetration 5
            :reload 40 :ready-time 0.5)
  :tags '(:firearm))

;;; Boucliers

(define-item :rodela "Rondache"
  :slots '(:shield) :hands 1 :weight 4 :price 15
  :modifiers '((:defense :add 0.15) (:cover :add 0.1)))

(define-item :pavise "Pavise"
  :slots '(:shield) :hands 0 :weight 12 :price 20
  :modifiers '((:cover :add 0.5))
  :traits '(:deployed-cover)
  :forbids '(:mount))

;;; Armures et casques

(define-item :buff-coat "Buffle"
  :slots '(:armor) :weight 3 :price 10
  :modifiers '((:armor :add 2)))

(define-item :corselet "Corselet"
  :slots '(:armor) :weight 9 :price 30
  :modifiers '((:armor :add 5)))

(define-item :cuirassier-armour "Armure de cuirassier"
  :slots '(:armor) :weight 25 :price 80
  :modifiers '((:armor :add 9)))

(define-item :morion "Morion"
  :slots '(:helmet) :weight 2 :price 8
  :modifiers '((:armor :add 1)))

;;; Montures

(define-item :light-horse "Cheval léger"
  :slots '(:mount) :price 60
  :modifiers '((:speed :set 4.0) (:run-speed :set 8.0) (:mass :mul 3) (:endurance :set 1.5)
               (:spacing :set 2.0) (:ranks :set 2))
  :traits '(:charge))

(define-item :heavy-horse "Cheval lourd"
  :slots '(:mount) :price 100
  :modifiers '((:speed :set 3.2) (:run-speed :set 6.5) (:mass :mul 4.5) (:endurance :set 1.2)
               (:spacing :set 2.2) (:ranks :set 2))
  :traits '(:charge))

;;; Dotations de démonstration : un homme et un équipement. Ce ne sont pas des
;;; types d'unité, seulement des équipements tout faits.

(defparameter *dotations*
  '((:pikemen :recruit (:main :pike :secondary :dagger :armor :corselet :helmet :morion))
    (:musketeers :recruit (:main :musket :secondary :sword :armor :buff-coat))
    (:rodeleros :recruit (:main :sword :shield :rodela :armor :corselet :helmet :morion))
    (:crossbowmen :recruit (:main :crossbow :secondary :sword :shield :pavise :helmet :morion))
    (:reiters :recruit (:main :pistols :secondary :sword :armor :buff-coat :mount :light-horse))
    (:lancers :recruit (:main :lance :secondary :sword :armor :cuirassier-armour
                        :helmet :morion :mount :heavy-horse))))

(defun dotation-stats (name)
  "Stats effectives de la dotation NAME."
  (destructuring-bind (man loadout)
      (or (cdr (assoc name *dotations*)) (error "Dotation inconnue : ~s." name))
    (compute-regiment-stats man loadout)))
