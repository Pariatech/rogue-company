(in-package #:rogue-company)

;;; Les tâches des groupes du général (ai-general.lisp), exécutées à chaque
;;; réveil du commandant. Chacune ne donne que des ordres du joueur, et
;;; seulement à un régiment libre (AI-FREE-P : ni en mêlée, ni à la charge, ni
;;; en repli, ni en poursuite) ; un ordre identique n'est pas redonné.
;;;
;;; - Ligne : chaque régiment va à sa place, face à l'ennemi. En avance, la
;;;   ligne fait un bond de *AI-LINE-BOUND* quand tous ses régiments libres sont
;;;   à moins de *AI-LINE-TOLERANCE* de leur place : elle avance au pas du plus
;;;   lent, alignée. Engagée, chacun attaque l'ennemi en face de sa place
;;;   (FACING-ENEMY) et le garde tant qu'il combat.
;;; - Écran : les tireurs devant leur place de ligne ; un ennemi à portée devant
;;;   eux, ils l'attaquent (ils s'arrêtent à portée de tir, et se replient en
;;;   tirailleurs), tenus en laisse (*AI-SCREEN-LEASH*).
;;; - Garde : à sa place au bout de la ligne ; elle attaque un ennemi qui entre
;;;   à moins de *AI-GUARD-RADIUS* de sa place, si le combat en vaut la peine
;;;   (AI-SCORE), et revient quand il passe au-delà de *AI-GUARD-LEASH*. Ligne
;;;   engagée, sa place suit la ligne et son rayon s'élargit
;;;   (*AI-GUARD-ENGAGED-RADIUS*) : elle prend de flanc ce que la ligne fixe.
;;; - Réserve : derrière le centre ; quand un régiment de la ligne ou une garde
;;;   flanche au combat (moral sous *AI-WAVER-MORALE*), elle attaque l'ennemi
;;;   qu'il combat.

(defparameter *ai-line-bound* 8f0 "Bond de la ligne, en mètres.")

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

(defun ai-attack (commander battle regiment enemy)
  "Lance REGIMENT sur ENEMY, sauf s'il l'attaque déjà."
  (unless (eq enemy (target-of battle regiment))
    (order-attack regiment enemy)
    (incf (gethash (regiment-index regiment) (commander-attack-orders commander) 0))))

;;; Places

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
                        (:screen (+ depth *ai-screen-ahead*))
                        (:guard (- depth *ai-guard-setback*))
                        (:reserve (- depth *ai-reserve-depth*))))
        (values x y (plan-angle plan))))))

(defun facing-enemy (battle plan regiment lateral)
  "L'ennemi en face de la place LATERAL de REGIMENT : le plus proche, l'écart
latéral compté *AI-LATERAL-WEIGHT* fois, à moins de *AI-ENGAGE-REACH* ; ou NIL."
  (let ((soldiers (battle-soldiers battle)) (best nil) (best-cost most-positive-single-float))
    (dolist (enemy (plan-enemies battle (regiment-side regiment)))
      (let ((gap (box-gap regiment enemy)))
        (when (< gap *ai-engage-reach*)
          (let ((cost (+ gap (* *ai-lateral-weight*
                                (abs (- lateral (nth-value 0 (regiment-coordinates plan enemy soldiers))))))))
            (when (< cost best-cost) (setf best enemy best-cost cost))))))
    best))

;;; Tâches

(defun advance-line (plan battle)
  "En avance : un bond quand tous les régiments libres de la ligne sont à leur
place ; engagée dès qu'un d'eux approche l'ennemi."
  (when (eq (plan-phase plan) :engaged)
    ;; Engagée, la profondeur suit la ligne : gardes et réserve restent à hauteur.
    (setf (plan-depth plan) (line-depth plan battle)))
  (when (eq (plan-phase plan) :advance)
    (let* ((side (regiment-side (aref (battle-regiments battle) (first (plan-line plan)))))
           (gap (line-gap-to-enemy plan battle side))
           (soldiers (battle-soldiers battle)))
      (if (and gap (< gap *ai-engage-distance*))
          (setf (plan-phase plan) :engaged)
          (when (loop for k in (plan-line plan)
                      for regiment = (aref (battle-regiments battle) k)
                      always (or (not (ai-free-p regiment))
                                 (regiment-square-p regiment)
                                 (multiple-value-bind (x y) (task-destination plan regiment)
                                   (multiple-value-bind (cx cy) (regiment-centroid regiment soldiers)
                                     (< (sqrt (+ (expt (- cx x) 2) (expt (- cy y) 2)))
                                        *ai-line-tolerance*)))))
            (incf (plan-depth plan) *ai-line-bound*))))))

(defun go-to-place (plan regiment)
  (unless (regiment-square-p regiment)
    (multiple-value-bind (x y facing) (task-destination plan regiment)
      (ai-move regiment x y facing))))

(defun run-line (commander battle plan regiment task)
  (if (eq (plan-phase plan) :engaged)
      (unless (fighting-target battle regiment)
        (let ((enemy (facing-enemy battle plan regiment (getf task :lateral))))
          (if enemy
              (ai-attack commander battle regiment enemy)
              (go-to-place plan regiment))))
      (go-to-place plan regiment)))

(defun run-screen (commander battle plan regiment)
  (multiple-value-bind (x y) (task-destination plan regiment)
    (let ((target (fighting-target battle regiment))
          (reach (attack-reach (stats-primary (regiment-stats regiment)))))
      (cond ((and target (> (point-box-distance target x y) *ai-screen-leash*))
             (go-to-place plan regiment))
            (target nil)
            (t
             (let ((enemy (loop with best = nil and best-gap = (+ reach 10f0)
                                for other in (plan-enemies battle (regiment-side regiment))
                                for gap = (box-gap regiment other)
                                when (< gap best-gap) do (setf best other best-gap gap)
                                finally (return best))))
               (if enemy
                   (ai-attack commander battle regiment enemy)
                   (go-to-place plan regiment))))))))

(defun run-guard (commander battle plan regiment)
  (multiple-value-bind (x y) (task-destination plan regiment)
    (let* ((target (fighting-target battle regiment))
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
  (unless (fighting-target battle regiment)
    (let* ((wavering (wavering-regiment plan battle (regiment-side regiment)))
           (enemy (and wavering (fought-regiment battle wavering))))
      (if (and enemy (fighting-p enemy))
          (ai-attack commander battle regiment enemy)
          (go-to-place plan regiment)))))

(defun run-tasks (commander battle)
  "Fait exécuter à chaque régiment libre du camp sa tâche du plan."
  (let ((plan (commander-plan commander)))
    (when (and plan (plan-line plan))
      (advance-line plan battle))
    (when plan
      (maphash (lambda (k task)
                 (let ((regiment (aref (battle-regiments battle) k)))
                   (when (ai-free-p regiment)
                     (case (getf task :kind)
                       (:line (run-line commander battle plan regiment task))
                       (:screen (run-screen commander battle plan regiment))
                       (:guard (run-guard commander battle plan regiment))
                       (:reserve (run-reserve commander battle plan regiment))))))
               (plan-tasks plan)))))
