(in-package #:rogue-company/tests)

;;; IA de l'armée adverse : rôles, réflexes, choix des cibles, stabilité,
;;; déterminisme. Le camp 1 est commandé par l'IA, le camp 0 reste passif.

(def-suite ai :description "Commandant de l'IA.")
(in-suite ai)

(defun ai-battle (&rest regiments)
  "Bataille de REGIMENTS (camp dotation effectif x y orientation en degrés
[exercice]), le camp 1 commandé par l'IA."
  (rogue-company::make-ai-battle :regiments regiments :sides '(1)))

(defun nth-regiment (battle k) (nth k (regiments battle)))

(test roles-follow-the-weapon
  (let ((b (ai-battle '(0 :pikemen 225 0 0 0) '(0 :arquebusiers 105 0 40 0)
                      '(0 :rodeleros 105 0 80 0) '(0 :lancers 60 0 120 0))))
    (is (equal '(:pikes :shooters :foot :cavalry)
               (map 'list #'rogue-company::regiment-role (regiments b))))))

(test ai-attacks-a-passive-enemy
  (let ((b (ai-battle '(0 :pikemen 225 0 0 0) '(1 :pikemen 225 60 0 180))))
    (run-ticks b 45)
    (is (equal '(:attack 0) (rogue-company::regiment-orders (nth-regiment b 1))))))

(test cavalry-avoids-pikes-from-the-front
  "Des lanciers face à des piques et à des arquebusiers loin d'elles chargent
les arquebusiers."
  (let ((b (ai-battle '(0 :pikemen 225 0 0 0) '(0 :arquebusiers 105 0 90 0)
                      '(1 :lancers 60 90 0 180))))
    (run-ticks b 45)
    (let ((lancers (nth-regiment b 2)))
      (is (< (rogue-company::ai-score b lancers (nth-regiment b 0))
             (rogue-company::ai-score b lancers (nth-regiment b 1))))
      (is (equal '(:attack 1) (rogue-company::regiment-orders lancers))))))

(test cavalry-waits-without-an-opening
  "Seuls devant des piques de face, des lanciers attendent à leur place."
  (let ((b (ai-battle '(0 :pikemen 225 0 0 0) '(1 :lancers 60 90 0 180))))
    (run-ticks b 90)
    (is (eq :move (first (rogue-company::regiment-orders (nth-regiment b 1)))))))

(test pikes-form-square-against-cavalry
  "Des piques de l'IA passent en carré devant de la cavalerie proche, et
reviennent en ligne quand elle n'est plus en état de combattre."
  (let* ((b (ai-battle '(0 :light-cavalry 60 -40 0 0) '(1 :pikemen 225 0 0 180)))
         (cavalry (nth-regiment b 0))
         (pikes (nth-regiment b 1)))
    (run-ticks b 45)
    (is-true (rogue-company::regiment-square-p pikes))
    (setf (rogue-company::regiment-orders cavalry) '(:rout))
    (run-ticks b 45)
    (is-false (rogue-company::regiment-square-p pikes))))

(test targets-stay-put
  "Dans la bataille rangée contre l'IA, le commandant ne change pas de cible à
tout bout de champ : au plus cinq ordres d'attaque par régiment en deux
minutes, un toutes les 24 s (les changements de cible du combat lui-même ne
comptent pas ; une cible débandée ou des tireurs qui se replient derrière leurs
piques en appellent un nouveau)."
  (let ((b (rogue-company::make-ai-battle)))
    (run-ticks b (* 30 120))
    (let ((orders (rogue-company::commander-attack-orders
                   (first (rogue-company::battle-commanders b)))))
      (is (plusp (hash-table-count orders)) "il a donné des ordres")
      (is (<= (loop for n being the hash-values of orders maximize n) 5)))))

(test ai-battles-replay
  "Deux batailles contre l'IA de même graine sont identiques."
  (flet ((digest ()
           (let ((b (rogue-company::make-ai-battle :sides '(0 1))))
             (run-ticks b (* 30 40))
             (rogue-company::battle-digest b))))
    (is (string= (digest) (digest)))))

(test ai-beats-a-passive-army
  "L'IA lancée contre la même armée qui ne bouge pas la met en déroute : plus de
régiments rouges débandés que de bleus après quatre minutes, et la bataille est
gagnée (victory.lisp)."
  (let ((b (rogue-company::make-ai-battle)))
    (run-ticks b (* 30 240))
    (flet ((routed (side)
             (count-if (lambda (r) (and (= side (rogue-company::regiment-side r))
                                        (rogue-company::regiment-routed-p r)))
                       (regiments b))))
      (is (> (routed 0) (+ 2 (routed 1))))
      (is (eql 1 (rogue-company::battle-outcome b)) "et elle a gagné"))))
