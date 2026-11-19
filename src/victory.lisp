(in-package #:rogue-company)

;;; Condition de victoire d'une bataille rangée (BATTLE-VICTORY-P : la bataille
;;; contre l'IA, pas le tournoi). Une armée est brisée quand moins de
;;; *ARMY-BREAK-FRACTION* de ses hommes de départ sont encore dans des régiments
;;; en état de combattre (vivants, pas débandés) : tout ce qui lui reste se
;;; débande. Quand il ne reste qu'une armée, elle a gagné (BATTLE-OUTCOME : son
;;; camp) ; si toutes sont brisées au même tick, match nul (:DRAW). Vérifié à la
;;; fin de chaque tick (UPDATE-OUTCOME), une seule fois décidé.

(defparameter *army-break-fraction* 0.3f0
  "Part des hommes de départ d'une armée, encore en état de combattre, sous
laquelle elle est brisée.")

(defun army-sides (battle)
  "Camps présents dans BATTLE, en ordre croissant."
  (sort (remove-duplicates (map 'list #'regiment-side (battle-regiments battle))) #'<))

(defun army-men (battle side)
  "Hommes de départ du camp SIDE, et ceux encore dans des régiments en état de
combattre."
  (loop for regiment across (battle-regiments battle)
        when (= (regiment-side regiment) side)
          sum (regiment-capacity regiment) into start
          and when (and (plusp (regiment-alive regiment)) (not (regiment-routed-p regiment)))
                sum (regiment-alive regiment) into fighting
        finally (return (values start fighting))))

(defun army-broken-p (battle side)
  (multiple-value-bind (start fighting) (army-men battle side)
    (< fighting (* *army-break-fraction* start))))

(defun update-outcome (battle)
  "Décide la bataille quand il ne reste qu'une armée qui n'est pas brisée : les
armées brisées se débandent tout entières."
  (when (and (battle-victory-p battle) (null (battle-outcome battle)))
    (let* ((sides (army-sides battle))
           (broken (remove-if-not (lambda (side) (army-broken-p battle side)) sides))
           (standing (set-difference sides broken)))
      (when (and broken (<= (length standing) 1))
        (loop for regiment across (battle-regiments battle)
              when (and (member (regiment-side regiment) broken)
                        (plusp (regiment-alive regiment))
                        (not (regiment-routed-p regiment)))
                do (rout battle regiment))
        (setf (battle-outcome battle) (or (first standing) :draw)
              (battle-outcome-tick battle) (battle-tick-count battle))))))

(defun army-summary (battle side)
  "Bilan du camp SIDE : plist :START (hommes au départ), :ALIVE (vivants),
:KILLS (ennemis tués), :REGIMENTS, :ROUTED (régiments débandés ou anéantis)."
  (loop for regiment across (battle-regiments battle)
        when (= (regiment-side regiment) side)
          sum (regiment-capacity regiment) into start
          and sum (regiment-alive regiment) into alive
          and sum (regiment-kills regiment) into kills
          and count t into regiments
          and count (or (zerop (regiment-alive regiment)) (regiment-routed-p regiment)) into routed
        finally (return (list :start start :alive alive :kills kills
                              :regiments regiments :routed routed))))
