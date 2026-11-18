(in-package #:rogue-company)

;;; Régiment de bataille : ses stats effectives (stats.lisp), sa formation, ses
;;; ordres et sa plage de soldats, de START à START + ALIVE. Il ne vit que le
;;; temps d'une bataille ; la fiche persistante viendra avec la partie.
;;;
;;; Les systèmes travaillent régiment par régiment : ils lisent les stats dans
;;; des variables locales, puis parcourent la plage. Ils ne connaissent pas la
;;; bataille, seulement le régiment et les tableaux de soldats.

(defclass regiment ()
  ((index :initarg :index :reader regiment-index :type fixnum
          :documentation "Position dans BATTLE-REGIMENTS, rangée dans SOLDIERS-REGIMENT.")
   (name :initarg :name :reader regiment-name)
   (side :initarg :side :reader regiment-side :type fixnum :documentation "Camp : 0 ou 1.")
   (stats :initarg :stats :reader regiment-stats :type regiment-stats)
   (formation :initarg :formation :reader regiment-formation :type formation)
   (morale :initarg :morale :accessor regiment-morale :type single-float)
   (fatigue :initform 0f0 :accessor regiment-fatigue :type single-float
            :documentation "0 frais, 1 épuisé. Reste à 0 jusqu'à la course et au combat.")
   (orders :initform nil :accessor regiment-orders
           :documentation "Ordre en cours : (:move x y facing), (:attack régiment),
(:retreat x y facing) pour un repli de tirailleurs (stances.lisp), (:rout) pour
un régiment débandé, ou NIL.")
   (engaged :initform 0 :accessor regiment-engaged :type fixnum
            :documentation "Soldats qui avaient une cible au dernier tick.")
   (closed :initform 0 :accessor regiment-closed :type fixnum
           :documentation "Soldats du premier rang dont la cible était à la distance d'arrêt.")
   (assailed :initform 0 :accessor regiment-assailed :type fixnum
             :documentation "Ennemis qui visaient un de ses soldats en mêlée au dernier
tick (flanks.lisp) : un régiment de tireurs frappé par des piques qu'il
n'atteint pas est en mêlée quand même.")
   (flank-attackers :initform 0 :accessor regiment-flank-attackers :type fixnum
                    :documentation "Ennemis qui le visaient de flanc au dernier tick (flanks.lisp).")
   (rear-attackers :initform 0 :accessor regiment-rear-attackers :type fixnum
                   :documentation "Ennemis qui le visaient de dos au dernier tick.")
   (holding :initform nil :accessor regiment-holding-p
            :documentation "Au contact : la formation ne bouge plus.")
   (drill :initarg :drill :initform nil :accessor regiment-drill
          :documentation "Exercice de tir des salves (shooting.lisp) : :COUNTERMARCH,
:FIRE-BY-RANK, ou NIL (feu à volonté).")
   (stance :initarg :stance :initform :hold :accessor regiment-stance
           :documentation "Posture (stances.lisp) : :HOLD s'arrête quand l'ennemi se
débande, :CHASE le poursuit, :SKIRMISH se replie devant une mêlée qui approche.")
   (resume-orders :initform nil :accessor regiment-resume-orders
                  :documentation "Ordre d'attaque repris à la fin d'un repli (:skirmish).")
   (retreat-ticks :initform 0 :accessor regiment-retreat-ticks :type fixnum
                  :documentation "Ticks depuis le début du repli en cours.")
   (charging :initform nil :accessor regiment-charging-p
             :documentation "À la charge, au pas de course ou au galop (charge.lisp).")
   (charge-contact :initform -1 :accessor regiment-charge-contact :type fixnum
                   :documentation "Tick du premier contact de la charge, qui ouvre le
bonus de charge, ou -1.")
   (chasing :initform nil :accessor regiment-chasing-p
            :documentation "Poursuit un ennemi débandé (:chase).")
   (chase-x :initform 0f0 :accessor regiment-chase-x :type single-float
            :documentation "Centre du régiment au début de la poursuite.")
   (chase-y :initform 0f0 :accessor regiment-chase-y :type single-float)
   (depth :initform 0 :accessor regiment-depth :type fixnum
          :documentation "Rangs de la grille : le rang du dernier soldat, plus un
(REFRESH-LAYOUT).")
   (bearer :initform -1 :accessor regiment-bearer :type fixnum
           :documentation "Indice du porteur d'étendard, ou -1 (REFRESH-LAYOUT).")
   (busy-tick :initform 0 :accessor regiment-busy-tick :type fixnum
              :documentation "Dernier tick en mêlée ou sous le feu : le régiment se
reforme après *REFORM-SECONDS* de calme (UPDATE-REFORM).")
   (reform-pending :initform nil :accessor regiment-reform-pending-p
                   :documentation "Un ordre vient d'être donné : le régiment se reforme
au prochain tick.")
   (steady :initform nil :accessor regiment-steady-p
           :documentation "Arrêté en formation, pour les salves (shooting.lisp, UPDATE-STEADY).")
   (alert :initform nil :accessor regiment-alert-p
          :documentation "Ennemi proche (combat.lisp, UPDATE-ALERT) : en garde, piques
basses et arquebuses en joue, même sans ordre d'attaque.")
   (open-until :initform -1 :accessor regiment-open-until :type fixnum
               :documentation "Files ouvertes pour un ami jusqu'à ce tick (open-files.lisp) ;
-1 en files normales.")
   (volley-rank :initform 0 :accessor regiment-volley-rank :type fixnum
                :documentation "Feu par rang : rang qui tire la prochaine salve.")
   (crouch :initform 0 :accessor regiment-crouch :type fixnum
           :documentation "Feu par rang : premiers rangs accroupis après avoir tiré.")
   (volley-timer :initform 0 :accessor regiment-volley-timer :type fixnum
                 :documentation "Ticks avant que le premier rang puisse tirer sa salve
(contremarche, shooting.lisp).")
   (countermarch-timer :initform -1 :accessor regiment-countermarch-timer :type fixnum
                       :documentation "Ticks avant la fin de la salve : le rang qui vient de
tirer passe à l'arrière (contremarche), ou tous se relèvent (feu par rang) ; -1
sans salve en cours.")
   (fire-tick :initform -1000000 :accessor regiment-fire-tick :type fixnum
              :documentation "Dernier tick où il a tiré ou essuyé un tir (shooting.lisp) :
le moral ne remonte pas sous le feu.")
   (contact-lost :initform 0 :accessor regiment-contact-lost :type fixnum
                 :documentation "Ticks sans aucun soldat engagé, pendant un ordre d'attaque.")
   (min-x :initform 0f0 :accessor regiment-min-x :type single-float
          :documentation "Boîte englobante des soldats vivants, recalculée à chaque tick.")
   (min-y :initform 0f0 :accessor regiment-min-y :type single-float)
   (max-x :initform 0f0 :accessor regiment-max-x :type single-float)
   (max-y :initform 0f0 :accessor regiment-max-y :type single-float)
   (start :initarg :start :reader regiment-start :type fixnum
          :documentation "Premier indice dans les tableaux de soldats.")
   (capacity :initarg :capacity :reader regiment-capacity :type fixnum)
   (alive :initarg :alive :accessor regiment-alive :type fixnum)
   (kills :initform 0 :accessor regiment-kills :type fixnum
          :documentation "Ennemis tués, convertis en expérience après la bataille.")
   (slots-dirty :initform t :accessor regiment-slots-dirty-p
                :documentation "Les emplacements sont à recalculer au prochain tick.")))

(defun regiment-traits (regiment)
  (stats-traits (regiment-stats regiment)))

(defun regiment-end (regiment)
  (+ (regiment-start regiment) (regiment-alive regiment)))

(defun order-move (regiment x y facing)
  "Envoie REGIMENT en X, Y, front tourné vers FACING (radians). Les emplacements
se recalculent au tick suivant."
  (let ((formation (regiment-formation regiment)))
    (setf (formation-x formation) (float x 1f0)
          (formation-y formation) (float y 1f0)
          (formation-facing formation) (float facing 1f0)
          (regiment-orders regiment) (list :move (float x 1f0) (float y 1f0) (float facing 1f0))
          (regiment-holding-p regiment) nil
          (regiment-chasing-p regiment) nil
          (regiment-reform-pending-p regiment) t
          (regiment-slots-dirty-p regiment) t))
  regiment)

;;; Systèmes

(defun update-regiment-slots (regiment soldiers)
  "Recalcule les emplacements si la formation a changé : chaque soldat à son
rang et sa file."
  (when (regiment-slots-dirty-p regiment)
    (place-formation (regiment-formation regiment) (regiment-alive regiment)
                     (soldiers-slot-x soldiers) (soldiers-slot-y soldiers)
                     (regiment-start regiment)
                     (soldiers-rank soldiers) (soldiers-file soldiers))
    (when (eq (first (regiment-orders regiment)) :rout)
      (scatter-slots regiment soldiers))
    (setf (regiment-slots-dirty-p regiment) nil)))

(defparameter *hold-margin* 0.25f0
  "Marge ajoutée à la portée minimale de l'arme principale : on ne s'arrête jamais
plus près.")

(defparameter *hold-short-of-reach* 0.5f0
  "Le soldat s'arrête à cette distance en deçà de la portée de son arme principale.")

(defun regiment-hold-distance (regiment)
  "Distance de bord à bord où un soldat s'arrête devant sa cible. Une arme à
portée minimale (la pique) serre l'ennemi au plus près, à sa portée minimale
plus *HOLD-MARGIN* (1,75 m) : le plus de rangs possible l'atteignent (à 1 m
d'intervalle, les trois premiers, à 1,75, 2,75 et 3,75 m sur 4,5 m de portée).
Une autre arme de mêlée s'arrête *HOLD-SHORT-OF-REACH* en deçà de sa portée.
Des tireurs comptent leur arme secondaire : au contact, ils tirent l'épée."
  (let ((primary (let ((stats (regiment-stats regiment)))
                   ;; Des tireurs au contact se battent à l'arme secondaire.
                   (if (and (eq (attack-kind (stats-primary stats)) :ranged)
                            (eq (attack-kind (stats-secondary stats)) :melee))
                       (stats-secondary stats)
                       (stats-primary stats)))))
    (cond ((not (eq (attack-kind primary) :melee)) *hold-margin*)
          ((plusp (attack-min-reach primary))
           (+ *hold-margin* (attack-min-reach primary)))
          (t (max *hold-margin* (- (attack-reach primary) *hold-short-of-reach*))))))

(defun regiment-in-melee-p (regiment)
  "Vrai si REGIMENT se battait en mêlée au dernier tick : un de ses soldats
avait une cible, ou un ennemi en visait un."
  (or (plusp (regiment-engaged regiment))
      (plusp (regiment-assailed regiment))))

(defparameter *bearer-rank* 3
  "Rang du porteur d'étendard d'un régiment de mêlée, compté depuis 0 au front :
le quatrième, hors de portée des piques ennemies. Celui d'un régiment de tireurs
est au dernier rang (BEARER-RANK).")

(defun ranged-regiment-p (regiment)
  (eq (attack-kind (stats-primary (regiment-stats regiment))) :ranged))

(defun bearer-rank (regiment)
  "Rang du porteur d'étendard de REGIMENT. Chez les tireurs, le dernier : en
queue de sa file, il ne se trouve jamais sur le chemin des hommes qui avancent
d'un rang à la contremarche."
  (let ((last (1- (regiment-depth regiment))))
    (if (ranged-regiment-p regiment)
        last
        (min *bearer-rank* last))))

;;; Rangs et files
;;;
;;; Chaque soldat a un rang et une file (SOLDIERS-RANK, SOLDIERS-FILE). La
;;; plage du régiment reste rangée par rang puis par file : le premier rang est
;;; en tête de plage. Au combat, un soldat change de rang (sa file avance quand
;;; un homme tombe devant, ou tourne en contremarche) mais jamais de file ; une
;;; file vide laisse un trou. Le régiment se reforme en rangs pleins, le dernier
;;; centré, quand il reçoit un ordre ou après *REFORM-SECONDS* de calme.

(defparameter *reform-seconds* 2
  "Secondes sans mêlée ni tir avant qu'un régiment bouche ses trous.")

(defun dense-place (k alive files)
  "Rang et file du soldat n° K d'une formation de ALIVE soldats en rangs pleins
sur FILES files, le dernier rang centré."
  (let* ((depth (ceiling alive files))
         (last-size (- alive (* (1- depth) files))))
    (multiple-value-bind (rank file) (floor k files)
      (values rank (if (= rank (1- depth))
                       (+ file (floor (- files last-size) 2))
                       file)))))

(defun swap-soldier-data (soldiers i j)
  "Échange les soldats I et J, mais pas leurs places (rang et file) : chacun prend
la place de l'autre."
  (dolist (slot *soldier-float-arrays*)
    (let ((array (slot-value soldiers slot)))
      (rotatef (aref array i) (aref array j))))
  (dolist (slot *soldier-fixnum-arrays*)
    (unless (member slot '(rank file))
      (let ((array (slot-value soldiers slot)))
        (rotatef (aref array i) (aref array j))))))

(defun file-tail-p (regiment soldiers i)
  "Vrai si le soldat I est le dernier de sa file : personne derrière lui. La
plage est rangée par rang : tout homme de sa file plus loin dans la plage est
derrière lui."
  (let ((files (soldiers-file soldiers)))
    (loop for j from (1+ i) below (regiment-end regiment)
          never (= (aref files j) (aref files i)))))

(defun middle-of-rank (regiment soldiers rank)
  "Indice du soldat au milieu du rang RANK de REGIMENT (ou du premier rang
plus profond s'il est vide)."
  (let* ((start (regiment-start regiment))
         (end (regiment-end regiment))
         (ranks (soldiers-rank soldiers))
         (first (loop for i from start below end
                      when (>= (aref ranks i) rank) return i))
         (count (loop for i from first below end
                      while (= (aref ranks i) (aref ranks first)) count t)))
    (+ first (floor count 2))))

(defun nearest-standing (regiment soldiers x y)
  "Le soldat vivant de REGIMENT le plus proche du point X Y, parmi ceux qui ne
sont pas en contremarche s'il y en a ; -1 si aucun ne vit (morts du tick pas
encore retirés)."
  (let ((xs (soldiers-x soldiers)) (ys (soldiers-y soldiers))
        (march (soldiers-march soldiers)) (hps (soldiers-hp soldiers))
        (best -1) (best-d 0f0) (best-marching t))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          do (let ((d (+ (expt (- (aref xs i) x) 2) (expt (- (aref ys i) y) 2)))
                   (marching (plusp (aref march i))))
               (when (and (plusp (aref hps i))
                          (or (< best 0)
                              (and best-marching (not marching))
                              (and (eq marching best-marching) (< d best-d))))
                 (setf best i best-d d best-marching marching))))
    best))

(defun file-tail (regiment soldiers i)
  "Indice du dernier homme de la file du soldat I."
  (let ((files (soldiers-file soldiers)))
    (loop with tail = i
          for j from (1+ i) below (regiment-end regiment)
          when (= (aref files j) (aref files i)) do (setf tail j)
          finally (return tail))))

(defun ranged-bearer (regiment soldiers keep fallen-x fallen-y)
  "Porteur d'étendard d'un régiment de tireurs : toujours en queue de sa file,
pour ne barrer le chemin de personne à la contremarche. KEEP, l'indice du
porteur actuel s'il vit encore, le reste ; s'il n'est plus en queue de file
(reformation), il échange sa place avec l'homme du milieu du dernier rang et
y marche, l'étendard en main. Mort à FALLEN-X FALLEN-Y, l'étendard passe à
l'homme le plus proche qui n'est pas en contremarche, qui échange sa place avec
le dernier de sa file : il ne saute jamais vers un homme qui redescend de la
contremarche, à l'autre bout du régiment."
  (flet ((carry-to (man place)
           ;; MAN prend la place PLACE, l'étendard en main ; l'autre la sienne.
           ;; Pas d'échange avec un mort pas encore retiré : APPLY-DAMAGE, qui
           ;; parcourt la plage en montant, ne le verrait plus.
           (cond ((= man place) place)
                 ((plusp (aref (soldiers-hp soldiers) place))
                  (swap-soldier-data soldiers man place)
                  place)
                 (t man))))
    (cond ((and (>= keep 0) (file-tail-p regiment soldiers keep)) keep)
          ((>= keep 0)
           (carry-to keep (middle-of-rank regiment soldiers (bearer-rank regiment))))
          (fallen-x
           (let ((man (nearest-standing regiment soldiers fallen-x fallen-y)))
             (if (< man 0)
                 (middle-of-rank regiment soldiers (bearer-rank regiment))
                 (carry-to man (file-tail regiment soldiers man)))))
          (t (middle-of-rank regiment soldiers (bearer-rank regiment))))))

(defun refresh-layout (regiment soldiers &key (keep -1) fallen-x fallen-y)
  "Recalcule la profondeur et le porteur d'étendard de REGIMENT après un
changement de rangs : -1 pour un homme seul, qui se bat ; sinon, dans un
régiment de mêlée, l'homme au milieu du rang BEARER-RANK (le quatrième, ou le
dernier s'il en reste moins), qui cède l'étendard à l'homme du milieu quand il
meurt ou change de rang ; dans un régiment de tireurs, RANGED-BEARER, qui garde
le porteur KEEP (indice à jour) tant qu'il vit. Le porteur ne frappe pas."
  (let ((alive (regiment-alive regiment))
        (start (regiment-start regiment))
        (ranks (soldiers-rank soldiers)))
    (setf (regiment-depth regiment) (if (plusp alive) (1+ (aref ranks (+ start alive -1))) 0)
          (regiment-bearer regiment)
          (cond ((<= alive 1) -1)
                ((ranged-regiment-p regiment)
                 (ranged-bearer regiment soldiers keep fallen-x fallen-y))
                (t (middle-of-rank regiment soldiers (bearer-rank regiment))))
          (regiment-slots-dirty-p regiment) t)))

(defun front-rank-end (regiment soldiers)
  "Fin (exclue) du premier rang dans la plage de REGIMENT : il est en tête."
  (let ((ranks (soldiers-rank soldiers)))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          while (zerop (aref ranks i))
          finally (return i))))

(defun rank-count (regiment soldiers low high)
  "Soldats de REGIMENT dont le rang est entre LOW (compris) et HIGH (exclu)."
  (let ((ranks (soldiers-rank soldiers)))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          count (<= low (aref ranks i) (1- high)))))

(defun layout-regiment (regiment soldiers)
  "Reforme REGIMENT en rangs pleins, dans l'ordre de sa plage : files fixées (ou
moins s'il manque d'hommes), dernier rang centré. Les soldats peuvent changer
de file."
  (let* ((formation (regiment-formation regiment))
         (alive (regiment-alive regiment))
         (start (regiment-start regiment))
         (files (formation-dense-files formation (max 1 alive))))
    (setf (formation-layout-files formation) files)
    (dotimes (k alive)
      (multiple-value-bind (rank file) (dense-place k alive files)
        (setf (aref (soldiers-rank soldiers) (+ start k)) rank
              (aref (soldiers-file soldiers) (+ start k)) file)))
    ;; Les indices ne bougent pas : le porteur reste le même homme.
    (refresh-layout regiment soldiers :keep (regiment-bearer regiment))))

(defun layout-dense-p (regiment soldiers)
  "Vrai si REGIMENT est déjà en rangs pleins, sans trou."
  (let* ((formation (regiment-formation regiment))
         (alive (regiment-alive regiment))
         (start (regiment-start regiment))
         (files (formation-dense-files formation (max 1 alive))))
    (and (eql files (formation-layout-files formation))
         (loop for k below alive
               always (multiple-value-bind (rank file) (dense-place k alive files)
                        (and (= rank (aref (soldiers-rank soldiers) (+ start k)))
                             (= file (aref (soldiers-file soldiers) (+ start k)))))))))

(defun update-reform (regiment soldiers tick)
  "Reforme REGIMENT, en début de tick, après un ordre ou *REFORM-SECONDS* sans
mêlée, sans tir et sans ennemi proche (REGIMENT-ALERT-P) ; pas en débandade, ni
pendant une contremarche."
  ;; Occupé : en mêlée, sous le feu ou tirant, ou l'ennemi proche (à portée
  ;; de tir, ou à moins de *ALERT-DISTANCE* : entre deux salves aussi).
  (when (or (regiment-in-melee-p regiment)
            (regiment-alert-p regiment)
            (< (- tick (regiment-fire-tick regiment)) (* 2 +ticks-per-second+)))
    (setf (regiment-busy-tick regiment) tick))
  (when (and (plusp (regiment-alive regiment))
             (not (eq (first (regiment-orders regiment)) :rout))
             (or (regiment-reform-pending-p regiment)
                 (and (zerop (mod (+ tick (regiment-index regiment)) 15))
                      (>= (- tick (regiment-busy-tick regiment))
                          (* *reform-seconds* +ticks-per-second+))))
             (loop for i from (regiment-start regiment) below (regiment-end regiment)
                   always (zerop (aref (soldiers-march soldiers) i))))
    (let ((ordered (regiment-reform-pending-p regiment)))
      (setf (regiment-reform-pending-p regiment) nil)
      (unless (layout-dense-p regiment soldiers)
        (let ((depth (regiment-depth regiment)))
          (layout-regiment regiment soldiers)
          ;; Au calme, le premier rang reste où il est ; un ordre, lui, vient
          ;; de placer la formation.
          (unless ordered (keep-front regiment depth)))))))

(defun move-regiment (regiment soldiers)
  "Avance les soldats de REGIMENT vers leur emplacement, au pas de marche."
  (let ((stats (regiment-stats regiment)))
    (move-toward-slots soldiers (regiment-start regiment) (regiment-end regiment)
                       ;; Un régiment débandé court, et qui le poursuit ou se
                       ;; replie en tirailleurs aussi (stances.lisp).
                       (/ (if (or (member (first (regiment-orders regiment)) '(:rout :retreat))
                                  (regiment-chasing-p regiment)
                                  ;; À la charge (charge.lisp).
                                  (regiment-charging-p regiment))
                              (stats-run-speed stats)
                              (stats-speed stats))
                          #.(float +ticks-per-second+ 1f0))
                       (formation-facing (regiment-formation regiment))
                       (/ (stats-turn-rate stats) #.(float +ticks-per-second+ 1f0))
                       (regiment-hold-distance regiment))))

;;; Inspection

(defun regiment-slot-distances (regiment soldiers)
  "Distance moyenne et maximale des soldats à leur emplacement, en mètres."
  (let ((total 0f0) (worst 0f0) (alive (regiment-alive regiment)))
    (loop for i from (regiment-start regiment) below (regiment-end regiment)
          for distance = (slot-distance soldiers i)
          do (incf total distance)
             (setf worst (max worst distance)))
    (values (if (plusp alive) (/ total alive) 0f0) worst)))

(defun regiment-snapshot (regiment soldiers &key (sample 3))
  (let ((stats (regiment-stats regiment))
        (orders (regiment-orders regiment)))
    (multiple-value-bind (mean worst) (regiment-slot-distances regiment soldiers)
      (ht "index" (regiment-index regiment)
          "name" (regiment-name regiment)
          "side" (regiment-side regiment)
          "alive" (regiment-alive regiment)
          "capacity" (regiment-capacity regiment)
          "start" (regiment-start regiment)
          "orders" (case (first orders)
                     (:move (destructuring-bind (x y facing) (rest orders)
                              (ht "kind" "move" "x" (json-float x) "y" (json-float y)
                                  "facing_degrees" (json-float (* facing (/ 180 pi))))))
                     (:attack (ht "kind" "attack" "regiment" (second orders)))
                     (:rout (ht "kind" "rout"))
                     (:retreat (ht "kind" "retreat" "x" (json-float (second orders))
                                   "y" (json-float (third orders))))
                     (t nil))
          "stance" (string-downcase (regiment-stance regiment))
          "chasing" (if (regiment-chasing-p regiment) t 'yason:false)
          "engaged" (regiment-engaged regiment)
          "flank_attackers" (regiment-flank-attackers regiment)
          "rear_attackers" (regiment-rear-attackers regiment)
          "holding" (if (regiment-holding-p regiment) t 'yason:false)
          "formation" (formation-snapshot (regiment-formation regiment) (regiment-alive regiment))
          "slot_distance_mean" (json-float mean)
          "slot_distance_max" (json-float worst)
          "morale" (json-float (regiment-morale regiment))
          "fatigue" (json-float (regiment-fatigue regiment))
          "kills" (regiment-kills regiment)
          "stats" (ht "speed" (json-float (stats-speed stats))
                      "spacing" (json-float (stats-spacing stats))
                      "ranks" (stats-ranks stats)
                      "primary" (string-downcase (attack-kind (stats-primary stats)))
                      "traits" (map 'vector #'string-downcase (stats-traits stats)))
          "soldiers" (loop for i from (regiment-start regiment)
                             below (min (regiment-end regiment) (+ (regiment-start regiment) sample))
                           collect (soldier-snapshot soldiers i))))))
