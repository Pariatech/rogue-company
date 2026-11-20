(in-package #:rogue-company)

;;; Capitaines : un par régiment de l'IA, entre le général (ai-general.lisp) et
;;; les ordres. Le général donne à chaque régiment une mission (sa tâche du
;;; plan : ligne, écran, garde, réserve, aile) ; le capitaine l'exécute à chaque
;;; réveil du commandant (CAPTAIN-ACT, une méthode par mission), avec un état
;;; qui dure (CAPTAIN-STATE, depuis CAPTAIN-SINCE) : il tient un choix au lieu de
;;; tout recalculer, et réagit à ce qui le menace sur place.
;;;
;;; - Écran (les tireurs) : en temps normal, la tâche d'écran (RUN-SCREEN). Quand
;;;   de la cavalerie ennemie libre le menace (CAVALRY-THREAT), état :REFUGE : il
;;;   se met derrière les piques amies les plus proches, à l'opposé de la
;;;   cavalerie (REFUGE-POINT), et appelle à l'aide ; il en ressort quand la
;;;   menace est passée, au moins *AI-REFUGE-MIN-SECONDS* après.
;;; - Entraide : un appel (COMMANDER-CALLS) dure *AI-CALL-SECONDS* ; une garde
;;;   ou la réserve libre, à moins de *AI-HELP-RADIUS* de l'assaillant, l'attaque.
;;; - Ligne, garde, réserve, aile : leur tâche (ai-tasks.lisp), garde et réserve
;;;   répondant d'abord aux appels.

(defparameter *ai-use-refuge* nil
  "Vrai : des tireurs menacés par la cavalerie se mettent à l'abri des piques.
Éteint : mesuré au banc d'essai, ils y survivent (81 tireurs perdus sur 32
batailles au lieu de 1 477) mais ne tirent plus, et les piques le paient (6
victoires sur 32).")

(defparameter *ai-use-help* t
  "Vrai : les capitaines menacés appellent à l'aide, gardes et réserve répondent.")

(defparameter *ai-refuge-alert* 40f0
  "De la cavalerie ennemie libre qui attaque des tireurs les menace à moins de
cette distance, en mètres.")

(defparameter *ai-refuge-near* 30f0
  "De la cavalerie ennemie libre menace des tireurs à moins de cette distance,
quelle que soit sa cible.")

(defparameter *ai-refuge-clear* 60f0
  "La menace est passée quand la cavalerie ne les attaque plus et en est à plus
de cette distance, ou qu'elle ne peut plus combattre.")

(defparameter *ai-refuge-min-seconds* 6 "Durée minimale d'un refuge, en secondes.")

(defparameter *ai-refuge-reach* 70f0
  "Des piques plus loin que cette distance des tireurs ne sont pas un refuge.")

(defparameter *ai-refuge-margin* 4f0
  "Écart entre le bord des piques et les tireurs à l'abri, en mètres.")

(defparameter *ai-call-seconds* 3 "Durée d'un appel à l'aide, en secondes.")

(defparameter *ai-help-radius* 70f0
  "Une garde ou la réserve répond à un appel contre un assaillant à moins de
cette distance d'elle.")

(defclass captain ()
  ((index :initarg :index :reader captain-index)
   (kind :initarg :kind :accessor captain-kind :documentation "Sa mission : la tâche du plan.")
   (state :initform :normal :accessor captain-state)
   (since :initform 0 :accessor captain-since :documentation "Tick d'entrée dans l'état.")
   (threat :initform nil :accessor captain-threat
           :documentation "Indice du régiment ennemi qui l'a fait changer d'état, ou NIL."))
  (:documentation "Capitaine d'un régiment de l'IA : sa mission, son état."))

(defun captain-enter (captain state battle &optional threat)
  (setf (captain-state captain) state
        (captain-since captain) (battle-tick-count battle)
        (captain-threat captain) (and threat (regiment-index threat))))

(defun captain-seconds (captain battle)
  "Secondes depuis l'entrée du capitaine dans son état."
  (/ (- (battle-tick-count battle) (captain-since captain)) +ticks-per-second+))

(defun regiment-captain (commander regiment kind)
  "Le capitaine de REGIMENT, de mission KIND ; un nouveau, ou remis à l'état
normal, si sa mission change."
  (let* ((captains (commander-captains commander))
         (captain (gethash (regiment-index regiment) captains)))
    (cond ((null captain)
           (setf (gethash (regiment-index regiment) captains)
                 (make-instance 'captain :index (regiment-index regiment) :kind kind)))
          ((not (eq kind (captain-kind captain)))
           (setf (captain-kind captain) kind
                 (captain-state captain) :normal
                 (captain-threat captain) nil)
           captain)
          (t captain))))

;;; Appels à l'aide

(defun call-for-help (commander battle caller assailant)
  "Appel de CALLER contre ASSAILANT, refait s'il existe déjà."
  (let ((calls (remove (regiment-index caller) (commander-calls commander) :key #'second)))
    (setf (commander-calls commander)
          (cons (list (regiment-index assailant) (regiment-index caller) (battle-tick-count battle))
                calls))))

(defun live-calls (commander battle)
  "Les appels encore valables : récents, assaillant en état de combattre."
  (setf (commander-calls commander)
        (remove-if (lambda (call)
                     (or (> (- (battle-tick-count battle) (third call))
                            (* *ai-call-seconds* +ticks-per-second+))
                         (not (fighting-p (aref (battle-regiments battle) (first call))))))
                   (commander-calls commander))))

(defun answer-call (commander battle regiment)
  "Si un appel vise un assaillant à moins de *AI-HELP-RADIUS* de REGIMENT, il
l'attaque. Vrai s'il répond, ou s'il attaque déjà l'assaillant d'un appel : sa
tâche ne doit pas l'en rappeler."
  (when (and *ai-use-help*
             (let ((target (held-target battle regiment)))
               (and target (find (regiment-index target) (commander-calls commander) :key #'first))))
    (return-from answer-call t))
  (when (and *ai-use-help* (not (held-target battle regiment)))
    (let ((best nil) (best-gap *ai-help-radius*))
      (dolist (call (commander-calls commander))
        (let* ((assailant (aref (battle-regiments battle) (first call)))
               (gap (box-gap regiment assailant)))
          ;; Une cavalerie libre ne se rattrape pas : on ne répond qu'à celle
          ;; qui s'est accrochée (CATCHABLE-P).
          (when (and (< gap best-gap) (catchable-p regiment assailant))
            (setf best assailant best-gap gap))))
      (when best
        (ai-attack commander battle regiment best)
        t))))

;;; Refuge des tireurs

(defun cavalry-threat (battle regiment)
  "La cavalerie ennemie libre (en état de combattre, pas en mêlée) la plus proche
qui menace REGIMENT : elle l'attaque et en est à moins de *AI-REFUGE-ALERT*, ou
elle en est à moins de *AI-REFUGE-NEAR*. NIL sinon."
  (let ((best nil) (best-gap most-positive-single-float))
    (loop for enemy across (battle-regiments battle)
          when (and (/= (regiment-side enemy) (regiment-side regiment))
                    (eq (regiment-role enemy) :cavalry)
                    (fighting-p enemy)
                    (not (regiment-in-melee-p enemy)))
            do (let ((gap (box-gap regiment enemy)))
                 (when (and (< gap best-gap)
                            (or (< gap *ai-refuge-near*)
                                (and (< gap *ai-refuge-alert*)
                                     (eq (target-of battle enemy) regiment))))
                   (setf best enemy best-gap gap))))
    best))

(defun still-threatening-p (battle regiment threat)
  (and threat
       (fighting-p threat)
       (or (eq (target-of battle threat) regiment)
           (< (box-gap regiment threat) *ai-refuge-clear*))))

(defun refuge-point (battle regiment threat)
  "Place à l'abri pour les tireurs REGIMENT : derrière les piques amies en bon
ordre, pas en mêlée, les plus proches (à moins de *AI-REFUGE-REACH*), à
l'opposé de THREAT. Renvoie X, Y et l'orientation (vers la menace), ou NIL."
  (let ((soldiers (battle-soldiers battle)) (pikes nil) (best-gap *ai-refuge-reach*))
    (loop for friend across (battle-regiments battle)
          when (and (= (regiment-side friend) (regiment-side regiment))
                    (pike-wall-p friend)
                    (not (regiment-in-melee-p friend)))
            do (let ((gap (box-gap regiment friend)))
                 (when (< gap best-gap) (setf pikes friend best-gap gap))))
    (when pikes
      (multiple-value-bind (px py) (regiment-centroid pikes soldiers)
        (multiple-value-bind (tx ty) (regiment-centroid threat soldiers)
          (let* ((dx (- px tx)) (dy (- py ty))
                 (length (max 1f-3 (sqrt (+ (* dx dx) (* dy dy)))))
                 (ux (/ dx length)) (uy (/ dy length))
                 ;; Du centre des piques à leur bord, dans cette direction, puis
                 ;; la moitié de la profondeur des tireurs.
                 (half-w (/ (- (regiment-max-x pikes) (regiment-min-x pikes)) 2))
                 (half-h (/ (- (regiment-max-y pikes) (regiment-min-y pikes)) 2))
                 (edge (min (if (> (abs ux) 1f-3) (/ half-w (abs ux)) most-positive-single-float)
                            (if (> (abs uy) 1f-3) (/ half-h (abs uy)) most-positive-single-float)))
                 (behind (+ edge *ai-refuge-margin*
                            (/ (min (- (regiment-max-x regiment) (regiment-min-x regiment))
                                    (- (regiment-max-y regiment) (regiment-min-y regiment)))
                               2))))
            (values (float (+ px (* ux behind)) 1f0)
                    (float (+ py (* uy behind)) 1f0)
                    (float (atan (- uy) (- ux)) 1f0))))))))

;;; Missions

(defgeneric captain-act (captain kind commander battle plan regiment task)
  (:documentation "Le capitaine de REGIMENT exécute sa mission KIND (la tâche
TASK du plan PLAN)."))

(defmethod captain-act (captain kind commander battle plan regiment task)
  (declare (ignore captain kind commander battle plan regiment task)))

(defmethod captain-act (captain (kind (eql :line)) commander battle plan regiment task)
  (declare (ignore captain))
  (when (ai-free-p regiment)
    (run-line commander battle plan regiment task)))

(defmethod captain-act (captain (kind (eql :guard)) commander battle plan regiment task)
  (declare (ignore captain task))
  (when (and (ai-free-p regiment) (not (answer-call commander battle regiment)))
    (run-guard commander battle plan regiment)))

(defmethod captain-act (captain (kind (eql :reserve)) commander battle plan regiment task)
  (declare (ignore captain task))
  (when (and (ai-free-p regiment) (not (answer-call commander battle regiment)))
    (run-reserve commander battle plan regiment)))

(defmethod captain-act (captain (kind (eql :wing)) commander battle plan regiment task)
  (declare (ignore captain task))
  (when (wing-free-p regiment)
    (run-wing commander battle plan regiment)))

(defmethod captain-act (captain (kind (eql :screen)) commander battle plan regiment task)
  (declare (ignore task))
  (let ((threat (and (captain-threat captain)
                     (aref (battle-regiments battle) (captain-threat captain)))))
    (when (eq (captain-state captain) :refuge)
      (if (and (>= (captain-seconds captain battle) *ai-refuge-min-seconds*)
               (not (still-threatening-p battle regiment threat)))
          (captain-enter captain :normal battle)
          (when *ai-use-help* (call-for-help commander battle regiment threat))))
    (when (eq (captain-state captain) :normal)
      (let ((new (cavalry-threat battle regiment)))
        (when new
          ;; Menacés, ils appellent à l'aide, à l'abri des piques ou non.
          (when *ai-use-help* (call-for-help commander battle regiment new))
          (when (and *ai-use-refuge* (refuge-point battle regiment new))
            (captain-enter captain :refuge battle new)
            (setf threat new)))))
    (when (ai-free-p regiment)
      (if (eq (captain-state captain) :refuge)
          (multiple-value-bind (x y facing) (refuge-point battle regiment threat)
            (if x
                (ai-move regiment x y facing)
                (captain-enter captain :normal battle)))
          (run-screen commander battle plan regiment)))))

(defun run-tasks (commander battle)
  "Fait exécuter à chaque capitaine du camp sa mission du plan."
  (let ((plan (commander-plan commander)))
    (when (and plan (plan-line plan))
      (advance-line plan battle))
    (when plan
      (live-calls commander battle)
      ;; Les tireurs d'abord : leurs appels sont entendus au même réveil.
      (dolist (kinds '((:screen) (:line :guard :reserve :wing)))
        (maphash (lambda (k task)
                   (let ((kind (getf task :kind)))
                     (when (member kind kinds)
                       (let ((regiment (aref (battle-regiments battle) k)))
                         (captain-act (regiment-captain commander regiment kind)
                                      kind commander battle plan regiment task)))))
                 (plan-tasks plan))))))
