(in-package #:rogue-company)

;;; Le général de l'IA : il lit le champ de bataille et répartit l'armée en
;;; groupes, chacun avec sa tâche (ai-tasks.lisp). Son plan (AI-PLAN) dure : il
;;; n'est refait que toutes les *AI-PLAN-SECONDS*, ou quand un régiment tombe
;;; ou se débande (PLAN-KEY).
;;;
;;; Repère de bataille : une origine (le centre de l'armée au premier plan,
;;; fixe ensuite), un axe vers le centre de l'armée ennemie (refait à chaque
;;; plan), et le côté (latéral, positif à droite de l'axe). Une place s'y écrit
;;; (latéral, profondeur) : PLAN-POINT, PLAN-COORDINATES. La profondeur de la
;;; ligne (PLAN-DEPTH) avance par bonds (ai-tasks.lisp).
;;;
;;; Groupes (MAKE-GROUPS), selon les rôles (ai.lisp) :
;;; - :LINE : les piques (à défaut la mêlée à pied), côte à côte de gauche à
;;;   droite dans l'ordre où elles sont, centrées sur l'axe, *AI-LINE-GAP*
;;;   entre deux régiments ;
;;; - :SCREEN : chaque régiment de tireurs devant la place de ligne la plus
;;;   proche de lui ;
;;; - :GUARD : la mêlée à pied la plus à gauche et la plus à droite, au bout de
;;;   la ligne, un peu en retrait ;
;;; - :RESERVE : la mêlée à pied qui reste, derrière le centre ;
;;; - :WING : la cavalerie, sur l'aile de son côté, au-delà des gardes
;;;   (*AI-WING-OFFSET*), un peu en retrait ; elle attend une occasion
;;;   (ai-tasks.lisp). Sans *AI-USE-WINGS*, :FREE : le choix glouton (ai.lisp).

(defparameter *ai-plan-seconds* 5 "Secondes entre deux plans du général, sans événement.")

(defparameter *ai-line-gap* 8f0 "Intervalle entre deux régiments voisins de la ligne, en mètres.")

(defparameter *ai-engage-distance* 30f0
  "La ligne est engagée quand un de ses régiments arrive à moins de cette
distance d'un ennemi : elle cesse ses bonds et chacun attaque l'ennemi en face.")

(defparameter *ai-disengage-distance* 50f0
  "Au plan suivant, une ligne engagée qui n'a plus d'ennemi à moins de cette
distance reprend son avance.")

(defparameter *ai-screen-ahead* 18f0 "Avance des tireurs devant leur place de ligne, en mètres.")

(defparameter *ai-guard-setback* 10f0 "Retrait des gardes de flanc derrière la ligne, en mètres.")

(defparameter *ai-reserve-depth* 35f0 "Distance de la réserve derrière la ligne, en mètres.")

(defparameter *ai-wing-offset* 45f0
  "Distance du bout de la ligne au bord intérieur de la cavalerie d'une aile, en
mètres : la place des gardes, et de l'air.")

(defparameter *ai-wing-setback* 15f0 "Retrait de la cavalerie derrière la ligne, en mètres.")

(defclass ai-plan ()
  ((origin-x :initarg :origin-x :accessor plan-origin-x :type single-float)
   (origin-y :initarg :origin-y :accessor plan-origin-y :type single-float)
   (angle :initform 0f0 :accessor plan-angle :type single-float
          :documentation "Direction de l'axe, vers l'ennemi, en radians.")
   (depth :initform 0f0 :accessor plan-depth :type single-float
          :documentation "Profondeur de la ligne le long de l'axe, depuis l'origine.")
   (phase :initform :advance :accessor plan-phase
          :documentation ":ADVANCE (la ligne avance par bonds jusqu'à ce que ses tireurs
aient l'ennemi à portée), :SKIRMISH (la ligne attend, les tireurs font feu),
:ASSAULT (la ligne avance jusqu'au contact) ou :ENGAGED.")
   (skirmish-tick :initform nil :accessor plan-skirmish-tick
                  :documentation "Tick du début de l'escarmouche, NIL avant.")
   (retired :initform '() :accessor plan-retired
            :documentation "Indices des tireurs effacés derrière la ligne à l'assaut :
ils y restent jusqu'à une nouvelle avance.")
   (line-half-depth :initform 0f0 :accessor plan-line-half-depth :type single-float
                    :documentation "Demi-profondeur, en mètres, du régiment le plus profond de
la ligne : les tireurs effacés se placent derrière (SCREEN-BEHIND).")
   (tasks :initform (make-hash-table) :accessor plan-tasks
          :documentation "Indice de régiment -> tâche, plist (:KIND … :LATERAL …).")
   (line :initform nil :accessor plan-line
         :documentation "Indices des régiments de la ligne, de gauche à droite.")
   (key :initform -1 :accessor plan-key
        :documentation "Régiments en état de combattre, des deux camps, au dernier plan.")
   (tick :initform -1000000 :accessor plan-tick :documentation "Tick du dernier plan."))
  (:documentation "Plan du général : repère de bataille, profondeur et phase de la
ligne, tâche de chaque régiment."))

;;; Repère

(defun plan-point (plan lateral depth)
  "Point du monde à LATERAL (à droite de l'axe) et DEPTH (le long de l'axe)."
  (let ((c (cos (plan-angle plan))) (s (sin (plan-angle plan))))
    (values (float (+ (plan-origin-x plan) (* c depth) (- (* s lateral))) 1f0)
            (float (+ (plan-origin-y plan) (* s depth) (* c lateral)) 1f0))))

(defun plan-coordinates (plan x y)
  "Latéral et profondeur du point X, Y du monde."
  (let ((c (cos (plan-angle plan))) (s (sin (plan-angle plan)))
        (dx (- x (plan-origin-x plan))) (dy (- y (plan-origin-y plan))))
    (values (float (+ (- (* s dx)) (* c dy)) 1f0)
            (float (+ (* c dx) (* s dy)) 1f0))))

(defun regiment-coordinates (plan regiment soldiers)
  (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
    (plan-coordinates plan x y)))

(defun regiment-frontage (regiment)
  "Largeur du front de REGIMENT, en mètres."
  (let* ((formation (regiment-formation regiment))
         (files (formation-files formation (max 1 (regiment-alive regiment)))))
    (+ 1f0 (* (formation-lateral-spacing formation) (1- files)))))

(defun side-centroid (battle sides)
  "Centre des soldats des régiments en état de combattre des camps SIDES, ou NIL."
  (let ((soldiers (battle-soldiers battle)) (n 0) (sx 0f0) (sy 0f0))
    (loop for regiment across (battle-regiments battle)
          when (and (member (regiment-side regiment) sides) (fighting-p regiment))
            do (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
                 (let ((alive (regiment-alive regiment)))
                   (incf n alive) (incf sx (* x alive)) (incf sy (* y alive)))))
    (when (plusp n) (values (/ sx n) (/ sy n)))))

(defun enemy-sides (battle side)
  (remove side (army-sides battle)))

(defun fighting-count (battle)
  (count-if #'fighting-p (battle-regiments battle)))

(defun point-box-distance (regiment x y)
  "Distance du point X, Y à la boîte des soldats de REGIMENT, 0 dedans."
  (let ((dx (max 0f0 (- (regiment-min-x regiment) x) (- x (regiment-max-x regiment))))
        (dy (max 0f0 (- (regiment-min-y regiment) y) (- y (regiment-max-y regiment)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun plan-enemies (battle side)
  (loop for other across (battle-regiments battle)
        when (and (/= (regiment-side other) side) (fighting-p other))
          collect other))

;;; Groupes

(defun task-kind (plan regiment)
  (getf (gethash (regiment-index regiment) (plan-tasks plan)) :kind :free))

(defun commander-task-free-p (commander regiment)
  "Vrai si REGIMENT n'a pas de tâche du général : il garde le choix glouton."
  (let ((plan (commander-plan commander)))
    (or (not *ai-use-plan*) (null plan) (eq :free (task-kind plan regiment)))))

(defun make-groups (plan battle side)
  "Répartit les régiments en état de combattre du camp SIDE entre les tâches."
  (let* ((soldiers (battle-soldiers battle))
         (tasks (make-hash-table))
         (mine (loop for regiment across (battle-regiments battle)
                     when (and (= (regiment-side regiment) side) (fighting-p regiment))
                       collect regiment))
         (by-lateral (lambda (regiments)
                       (sort (copy-list regiments) #'<
                             :key (lambda (r) (nth-value 0 (regiment-coordinates plan r soldiers))))))
         (pikes (remove-if-not (lambda (r) (eq (regiment-role r) :pikes)) mine))
         (foot (remove-if-not (lambda (r) (eq (regiment-role r) :foot)) mine))
         (shooters (remove-if-not (lambda (r) (eq (regiment-role r) :shooters)) mine))
         (line (funcall by-lateral (if pikes pikes foot)))
         (others (funcall by-lateral (if pikes foot nil)))
         (widths (mapcar #'regiment-frontage line))
         (total (+ (reduce #'+ widths) (* *ai-line-gap* (max 0 (1- (length line))))))
         (left-edge (- (/ total 2)))
         (slots '()))
    ;; La ligne, centrée sur l'axe.
    (let ((edge left-edge))
      (loop for regiment in line
            for width in widths
            do (let ((lateral (+ edge (/ width 2))))
                 (setf (gethash (regiment-index regiment) tasks) (list :kind :line :lateral lateral))
                 (push lateral slots)
                 (setf edge (+ edge width *ai-line-gap*)))))
    ;; Les tireurs devant la place de ligne la plus proche.
    (dolist (regiment shooters)
      (let ((lateral (nth-value 0 (regiment-coordinates plan regiment soldiers))))
        (setf (gethash (regiment-index regiment) tasks)
              (if slots
                  (list :kind :screen
                        :lateral (first (sort (copy-list slots) #'<
                                              :key (lambda (slot) (abs (- slot lateral))))))
                  (list :kind :free)))))
    ;; Gardes aux bouts de la ligne, réserve derrière le centre.
    (when others
      (let ((left (first others))
            (right (and (rest others) (car (last others)))))
        (flet ((guard (regiment side)
                 (setf (gethash (regiment-index regiment) tasks)
                       (list :kind :guard :side side
                             :lateral (* side (+ (/ total 2) *ai-line-gap*
                                                 (/ (regiment-frontage regiment) 2)))))))
          (if right
              (progn (guard left -1) (guard right 1))
              (guard left (if (minusp (nth-value 0 (regiment-coordinates plan left soldiers))) -1 1))))
        (dolist (regiment (butlast (rest others)))
          (setf (gethash (regiment-index regiment) tasks) (list :kind :reserve :lateral 0f0)))))
    ;; La cavalerie sur l'aile de son côté, les régiments d'une même aile côte à côte.
    (when *ai-use-wings*
      (let ((outer (list (cons -1 (+ (/ total 2) *ai-wing-offset*))
                         (cons 1 (+ (/ total 2) *ai-wing-offset*)))))
        (dolist (regiment (funcall by-lateral (remove-if-not (lambda (r) (eq (regiment-role r) :cavalry))
                                                             mine)))
          (let* ((side (if (minusp (nth-value 0 (regiment-coordinates plan regiment soldiers))) -1 1))
                 (width (regiment-frontage regiment))
                 (edge (cdr (assoc side outer))))
            (setf (gethash (regiment-index regiment) tasks)
                  (list :kind :wing :side side :lateral (* side (+ edge (/ width 2)))))
            (setf (cdr (assoc side outer)) (+ edge width *ai-line-gap*))))))
    (setf (plan-tasks plan) tasks
          (plan-line plan) (mapcar #'regiment-index line))))

(defun line-depth (plan battle)
  "Profondeur moyenne des régiments de la ligne, ou celle du plan sans ligne."
  (let ((soldiers (battle-soldiers battle))
        (regiments (battle-regiments battle))
        (line (plan-line plan)))
    (let ((standing (remove-if-not (lambda (k) (fighting-p (aref regiments k))) line)))
      (if standing
          (/ (loop for k in standing
                   sum (nth-value 1 (regiment-coordinates plan (aref regiments k) soldiers)))
             (length standing))
          (plan-depth plan)))))

(defun line-gap-to-enemy (plan battle side)
  "Plus petite distance entre la boîte d'un régiment de la ligne et celle d'un
ennemi en état de combattre, ou NIL sans ligne ou sans ennemi."
  (let ((regiments (battle-regiments battle))
        (enemies (plan-enemies battle side))
        (best nil))
    (dolist (k (plan-line plan) best)
      (let ((regiment (aref regiments k)))
        (when (fighting-p regiment)
          (dolist (enemy enemies)
            (let ((gap (box-gap regiment enemy)))
              (when (or (null best) (< gap best)) (setf best gap)))))))))

(defun update-plan (commander battle)
  "Refait le plan quand il est dû : repère, groupes, phase de la ligne."
  (let* ((side (commander-side commander))
         (plan (commander-plan commander))
         (tick (battle-tick-count battle))
         (key (fighting-count battle)))
    (multiple-value-bind (ox oy) (side-centroid battle (list side))
      (multiple-value-bind (ex ey) (side-centroid battle (enemy-sides battle side))
        (when (and ox ex
                   (or (null plan) (/= key (plan-key plan))
                       (>= (- tick (plan-tick plan)) (* *ai-plan-seconds* +ticks-per-second+))))
          (let ((fresh (null plan)))
            (when fresh
              (setf plan (make-instance 'ai-plan :origin-x ox :origin-y oy)
                    (commander-plan commander) plan))
            (setf (plan-angle plan) (float (atan (- ey (plan-origin-y plan)) (- ex (plan-origin-x plan))) 1f0)
                  (plan-key plan) key
                  (plan-tick plan) tick)
            (make-groups plan battle side)
            (let ((gap (line-gap-to-enemy plan battle side)))
              (cond ((and gap (< gap *ai-engage-distance*))
                     (setf (plan-phase plan) :engaged))
                    ((or fresh
                         (and (eq (plan-phase plan) :engaged)
                              (or (null gap) (> gap *ai-disengage-distance*))))
                     ;; Nouvelle avance, depuis là où la ligne se trouve.
                     (setf (plan-phase plan) :advance
                           (plan-skirmish-tick plan) nil
                           (plan-retired plan) '()
                           (plan-depth plan) (line-depth plan battle)))))))))))
