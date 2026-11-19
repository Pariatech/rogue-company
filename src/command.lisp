(in-package #:rogue-company)

;;; Commandement du joueur : il choisit des régiments de son camp
;;; (*PLAYER-SIDE*) à la souris et leur donne des ordres.
;;;
;;; Clic gauche : choisit le régiment sous la souris (Maj : l'ajoute ou le
;;; retire) ; dans le vide, vide la sélection. Glisser du clic gauche : cadre de
;;; sélection. Clic droit : sur un ennemi, l'attaque ; ailleurs, marche vers ce
;;; point, les régiments choisis gardant leurs places relatives. Glisser du clic
;;; droit choisit l'orientation d'arrivée (du point enfoncé vers la souris) ;
;;; sans glisser, front dans le sens de la marche.
;;; T : halte sur place. E : exercice suivant (carré pour les piques, exercices
;;; de tir pour les tireurs). P : posture suivante.
;;;
;;; Les fonctions d'ordre prennent la bataille et des coordonnées du monde, sans
;;; raylib : elles se testent sans fenêtre.

(defparameter *player-side* 0 "Camp du joueur : 0, les rouges.")

(defparameter *click-slop* 6
  "Au-delà de ce déplacement de la souris, en pixels, un clic devient un glisser.")

(defparameter *pick-radius* 2f0
  "Distance, en mètres, d'un soldat au point cliqué pour choisir son régiment.")

(defparameter *selection-color* '(255 230 120 255))
(defparameter *move-order-color* '(120 230 120 200))
(defparameter *attack-order-color* '(240 90 80 220))
(defparameter *command-text-size* 18 "Taille nominale du texte du panneau (TEXT-SIZE).")

(defparameter *drill-names*
  '((nil . "en bloc") (:square . "carré") (:at-will . "feu à volonté") (:volley . "salve")
    (:countermarch . "contremarche") (:fire-by-rank . "feu par rang") (:salvo . "salvée")))

(defparameter *stance-names* '((:hold . "tenir") (:chase . "poursuite") (:skirmish . "tirailleurs")))

(defclass player-command ()
  ((selection :initform nil :accessor command-selection
              :documentation "Indices des régiments choisis.")
   (left-press :initform nil :accessor command-left-press
               :documentation "(x . y) à l'écran où le bouton gauche a été enfoncé, ou NIL.")
   (right-press :initform nil :accessor command-right-press)
   (panel :initform (make-hash-table) :reader command-panel
          :documentation "Indice -> (clé . texte) : lignes du panneau, refaites quand la clé change."))
  (:documentation "Sélection et saisie des ordres du joueur."))

;;; Choix des régiments

(defun commandable-p (regiment)
  (and (= (regiment-side regiment) *player-side*)
       (plusp (regiment-alive regiment))
       (not (regiment-routed-p regiment))))

(defun regiment-at (battle x y)
  "Régiment vivant dont un soldat est le plus près du point X, Y du monde, à
moins de *PICK-RADIUS*, ou NIL."
  (let ((soldiers (battle-soldiers battle)) (best nil) (best-d *pick-radius*))
    (loop for regiment across (battle-regiments battle)
          when (plusp (regiment-alive regiment))
            do (loop for i from (regiment-start regiment) below (regiment-end regiment)
                     for d = (sqrt (+ (expt (- (aref (soldiers-x soldiers) i) x) 2)
                                      (expt (- (aref (soldiers-y soldiers) i) y) 2)))
                     when (< d best-d) do (setf best regiment best-d d)))
    best))

(defun regiments-in-box (battle x0 y0 x1 y1)
  "Régiments du joueur dont le centre est dans le rectangle X0 Y0 X1 Y1 du monde."
  (let ((soldiers (battle-soldiers battle))
        (left (min x0 x1)) (right (max x0 x1)) (top (min y0 y1)) (bottom (max y0 y1)))
    (loop for regiment across (battle-regiments battle)
          when (and (commandable-p regiment)
                    (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
                      (and (<= left x right) (<= top y bottom))))
            collect (regiment-index regiment))))

(defun selected-regiments (command battle)
  "Régiments choisis encore commandables ; les autres quittent la sélection."
  (let ((regiments (battle-regiments battle)))
    (setf (command-selection command)
          (remove-if-not (lambda (k) (and (< k (length regiments))
                                          (commandable-p (aref regiments k))))
                         (command-selection command)))
    (mapcar (lambda (k) (aref regiments k)) (command-selection command))))

;;; Ordres

(defun command-move (battle regiments x y &optional facing)
  "Envoie REGIMENTS vers le point X, Y : leur centre commun y va, chacun garde sa
place par rapport à lui. Fronts vers FACING (radians) s'il est donné, la
disposition du groupe tournant d'autant ; sinon dans le sens de la marche."
  (when regiments
    (let* ((soldiers (battle-soldiers battle))
           (centers (mapcar (lambda (r) (multiple-value-list (regiment-centroid r soldiers)))
                            regiments))
           (cx (/ (reduce #'+ centers :key #'first) (length centers)))
           (cy (/ (reduce #'+ centers :key #'second) (length centers)))
           (dx (- x cx)) (dy (- y cy))
           (heading (cond (facing (float facing 1f0))
                          ((> (+ (* dx dx) (* dy dy)) 1f0) (float (atan dy dx) 1f0))))
           ;; Rotation de la disposition : de l'ancien front du groupe au nouveau.
           (turn (if (and facing heading)
                     (wrap-angle (- heading (formation-facing (regiment-formation (first regiments)))))
                     0f0))
           (c (cos turn)) (s (sin turn)))
      (loop for regiment in regiments
            for (rx ry) in centers
            for ox = (- rx cx) for oy = (- ry cy)
            do (order-move regiment
                           (+ x (- (* c ox) (* s oy)))
                           (+ y (+ (* s ox) (* c oy)))
                           (or heading (formation-facing (regiment-formation regiment))))))))

(defun command-attack (regiments target)
  (dolist (regiment regiments)
    (order-attack regiment target)))

(defun command-halt (battle regiments)
  "Arrête REGIMENTS où ils sont, front inchangé."
  (let ((soldiers (battle-soldiers battle)))
    (dolist (regiment regiments)
      (multiple-value-bind (x y) (regiment-centroid regiment soldiers)
        (order-move regiment x y (formation-facing (regiment-formation regiment)))))))

(defun drill-choices (regiment)
  "Exercices possibles de REGIMENT, dans l'ordre de E."
  (let ((primary (stats-primary (regiment-stats regiment))))
    (cond ((attack-volley-p primary) '(:at-will :volley :countermarch :fire-by-rank :salvo))
          ((and (eq (attack-kind primary) :melee) (plusp (attack-min-reach primary))
                (not (getf (stats-loadout (regiment-stats regiment)) :mount)))
           '(nil :square))
          (t nil))))

(defun set-regiment-drill (regiment drill)
  "Passe REGIMENT à l'exercice DRILL : rangs et files refaits comme à sa création
(ADD-REGIMENT), puis reformation."
  (let* ((stats (regiment-stats regiment))
         (formation (regiment-formation regiment))
         (base (stats-ranks stats))
         (ranks (case drill
                  ((:fire-by-rank :salvo) (min base *rank-fire-ranks*))
                  (:volley (min base *volley-ranks*))
                  (t base))))
    (setf (regiment-drill regiment) drill
          (formation-ranks formation) ranks
          (formation-fixed-files formation) (ceiling (max 1 (regiment-alive regiment)) ranks)
          (regiment-reform-pending-p regiment) t
          (regiment-slots-dirty-p regiment) t)
    (set-file-spacing regiment (regiment-base-file-spacing regiment))
    regiment))

(defun cycle-drill (regiment)
  (let ((choices (drill-choices regiment)))
    (when choices
      (set-regiment-drill regiment
                          (or (second (member (regiment-drill regiment) choices))
                              (first choices))))))

(defun stance-choices (regiment)
  (cond ((ranged-regiment-p regiment) '(:hold :skirmish))
        (t '(:hold :chase))))

(defun cycle-stance (regiment)
  (let ((choices (stance-choices regiment)))
    (setf (regiment-stance regiment)
          (or (second (member (regiment-stance regiment) choices)) (first choices)))))

;;; Saisie

(defun mouse-world (camera)
  (screen->world camera (float (raylib:get-mouse-x) 1f0) (float (raylib:get-mouse-y) 1f0)))

(defun mouse-moved-p (press)
  (and press
       (> (max (abs (- (raylib:get-mouse-x) (car press)))
               (abs (- (raylib:get-mouse-y) (cdr press))))
          *click-slop*)))

(defun update-command (command battle camera)
  "Lit la souris et les touches de commandement, une fois par image."
  (let ((shift (or (key-down-p :key-left-shift) (key-down-p :key-right-shift))))
    ;; Bouton gauche : clic (choix) ou glisser (cadre).
    (when (raylib:is-mouse-button-pressed :mouse-button-left)
      (setf (command-left-press command) (cons (raylib:get-mouse-x) (raylib:get-mouse-y))))
    (when (and (command-left-press command) (raylib:is-mouse-button-released :mouse-button-left))
      (let ((press (command-left-press command)))
        (setf (command-left-press command) nil)
        (if (mouse-moved-p press)
            (multiple-value-bind (x0 y0) (screen->world camera (float (car press) 1f0)
                                                        (float (cdr press) 1f0))
              (multiple-value-bind (x1 y1) (mouse-world camera)
                (let ((boxed (regiments-in-box battle x0 y0 x1 y1)))
                  (setf (command-selection command)
                        (if shift
                            (union (command-selection command) boxed)
                            boxed)))))
            (multiple-value-bind (x y) (mouse-world camera)
              (let ((regiment (regiment-at battle x y)))
                (cond ((not (and regiment (commandable-p regiment)))
                       (unless shift (setf (command-selection command) nil)))
                      (shift
                       (let ((k (regiment-index regiment)))
                         (setf (command-selection command)
                               (if (member k (command-selection command))
                                   (remove k (command-selection command))
                                   (cons k (command-selection command))))))
                      (t (setf (command-selection command)
                               (list (regiment-index regiment))))))))))
    ;; Bouton droit : l'ordre, au point enfoncé ; glisser choisit l'orientation.
    (when (raylib:is-mouse-button-pressed :mouse-button-right)
      (setf (command-right-press command) (cons (raylib:get-mouse-x) (raylib:get-mouse-y))))
    (when (and (command-right-press command) (raylib:is-mouse-button-released :mouse-button-right))
      (let ((press (command-right-press command))
            (regiments (selected-regiments command battle)))
        (setf (command-right-press command) nil)
        (when regiments
          (multiple-value-bind (x y) (screen->world camera (float (car press) 1f0)
                                                    (float (cdr press) 1f0))
            (let ((target (regiment-at battle x y)))
              (cond ((and target (/= (regiment-side target) *player-side*)
                          (not (regiment-routed-p target)))
                     (command-attack regiments target))
                    ((mouse-moved-p press)
                     (multiple-value-bind (mx my) (mouse-world camera)
                       (command-move battle regiments x y (atan (- my y) (- mx x)))))
                    (t (command-move battle regiments x y))))))))
    (let ((regiments (selected-regiments command battle)))
      (when (key-pressed-p :key-t) (command-halt battle regiments))
      (when (key-pressed-p :key-e) (mapc #'cycle-drill regiments))
      (when (key-pressed-p :key-p) (mapc #'cycle-stance regiments)))))

;;; Dessin

(defun world->screen (camera x y)
  (let ((zoom (camera-zoom camera)))
    (values (round (+ (screen-center-x) (* zoom (- x (camera-x camera)))))
            (round (+ (screen-center-y) (* zoom (- y (camera-y camera))))))))

(defun draw-regiment-outline (regiment soldiers camera color)
  "Contour de REGIMENT : le rectangle de ses soldats dans le repère de son front."
  (let* ((facing (formation-facing (regiment-formation regiment)))
         (fx (cos facing)) (fy (sin facing)))
    (multiple-value-bind (cx cy) (regiment-centroid regiment soldiers)
      (let ((a0 0f0) (a1 0f0) (b0 0f0) (b1 0f0))
        (loop for i from (regiment-start regiment) below (regiment-end regiment)
              for dx = (- (aref (soldiers-x soldiers) i) cx)
              for dy = (- (aref (soldiers-y soldiers) i) cy)
              for along = (+ (* dx fx) (* dy fy))
              for across = (- (* dy fx) (* dx fy))
              do (setf a0 (min a0 along) a1 (max a1 along) b0 (min b0 across) b1 (max b1 across)))
        (let ((pad 1f0))
          (decf a0 pad) (incf a1 pad) (decf b0 pad) (incf b1 pad))
        (flet ((corner (a b)
                 (world->screen camera (+ cx (* a fx) (- (* b fy))) (+ cy (* a fy) (* b fx)))))
          (multiple-value-bind (x1 y1) (corner a0 b0)
            (multiple-value-bind (x2 y2) (corner a1 b0)
              (multiple-value-bind (x3 y3) (corner a1 b1)
                (multiple-value-bind (x4 y4) (corner a0 b1)
                  (draw-line x1 y1 x2 y2 color)
                  (draw-line x2 y2 x3 y3 color)
                  (draw-line x3 y3 x4 y4 color)
                  (draw-line x4 y4 x1 y1 color))))))))))

(defun draw-order-line (regiment battle camera)
  "Trait du régiment vers son ordre : le point de marche, ou le régiment attaqué."
  (let ((soldiers (battle-soldiers battle)) (orders (regiment-orders regiment)))
    (multiple-value-bind (sx sy) (multiple-value-call #'world->screen camera
                                   (regiment-centroid regiment soldiers))
      (case (first orders)
        (:move
         (multiple-value-bind (tx ty) (world->screen camera (second orders) (third orders))
           (draw-line sx sy tx ty *move-order-color*)))
        (:attack
         (let ((target (aref (battle-regiments battle) (second orders))))
           (when (plusp (regiment-alive target))
             (multiple-value-bind (tx ty) (multiple-value-call #'world->screen camera
                                            (regiment-centroid target soldiers))
               (draw-line sx sy tx ty *attack-order-color*)))))))))

(defparameter *ghost-color* '(120 230 120 160) "Fantôme d'un régiment à sa destination.")
(defparameter *dash-pixels* 10 "Longueur d'un tiret du trajet, en pixels.")

(defun draw-dashed-line (x0 y0 x1 y1 color)
  "Trait en tirets de *DASH-PIXELS*, à l'écran."
  (let* ((dx (- x1 x0)) (dy (- y1 y0))
         (length (sqrt (+ (* dx dx) (* dy dy))))
         (dashes (max 1 (floor length *dash-pixels*))))
    (loop for k from 0 below dashes by 2
          for a = (/ k dashes) for b = (/ (min dashes (1+ k)) dashes)
          do (draw-line (round (+ x0 (* a dx))) (round (+ y0 (* a dy)))
                        (round (+ x0 (* b dx))) (round (+ y0 (* b dy))) color))))

(defun draw-ghost (regiment camera x y facing)
  "Fantôme de REGIMENT en X, Y, front vers FACING : le contour de sa formation
(files et rangs actuels), le front doublé et une flèche vers l'avant."
  (let* ((formation (regiment-formation regiment))
         (files (formation-files formation (max 1 (regiment-alive regiment))))
         (depth (max 1 (regiment-depth regiment)))
         (half-width (* 0.5f0 (+ 1f0 (* (formation-lateral-spacing formation) (1- files)))))
         (half-depth (* 0.5f0 (+ 1f0 (* (formation-spacing formation) (1- depth)))))
         (fx (cos facing)) (fy (sin facing)))
    (flet ((corner (a b)
             ;; A vers l'avant, B vers la droite du front.
             (world->screen camera (+ x (* a fx) (- (* b fy))) (+ y (* a fy) (* b fx)))))
      (multiple-value-bind (x1 y1) (corner half-depth (- half-width))
        (multiple-value-bind (x2 y2) (corner half-depth half-width)
          (multiple-value-bind (x3 y3) (corner (- half-depth) half-width)
            (multiple-value-bind (x4 y4) (corner (- half-depth) (- half-width))
              (draw-line x1 y1 x2 y2 *ghost-color*)
              (draw-line x2 y2 x3 y3 *ghost-color*)
              (draw-line x3 y3 x4 y4 *ghost-color*)
              (draw-line x4 y4 x1 y1 *ghost-color*)
              ;; Front doublé, un peu en retrait.
              (multiple-value-bind (x5 y5) (corner (- half-depth 0.6f0) (- half-width))
                (multiple-value-bind (x6 y6) (corner (- half-depth 0.6f0) half-width)
                  (draw-line x5 y5 x6 y6 *ghost-color*)))
              ;; Flèche vers l'avant, du centre au-delà du front.
              (multiple-value-bind (cx cy) (corner 0f0 0f0)
                (multiple-value-bind (tx ty) (corner (+ half-depth 3f0) 0f0)
                  (draw-line cx cy tx ty *ghost-color*)
                  (multiple-value-bind (lx ly) (corner (+ half-depth 1.8f0) -1f0)
                    (draw-line tx ty lx ly *ghost-color*))
                  (multiple-value-bind (rx ry) (corner (+ half-depth 1.8f0) 1f0)
                    (draw-line tx ty rx ry *ghost-color*)))))))))))

(defun draw-orders-preview (battle camera)
  "Ordres en cours de tous les régiments du joueur : trajet en tirets et fantôme
à la destination pour une marche, trajet rouge vers le régiment attaqué."
  (let ((soldiers (battle-soldiers battle)))
    (loop for regiment across (battle-regiments battle)
          when (commandable-p regiment)
            do (let ((orders (regiment-orders regiment)))
                 (multiple-value-bind (sx sy) (multiple-value-call #'world->screen camera
                                                (regiment-centroid regiment soldiers))
                   (case (first orders)
                     (:move
                      (destructuring-bind (x y facing) (rest orders)
                        ;; Déjà arrivé (ou ordre de mise en place) : rien à montrer.
                        (when (multiple-value-bind (cx cy) (regiment-centroid regiment soldiers)
                                (> (+ (expt (- x cx) 2) (expt (- y cy) 2)) 9f0))
                          (multiple-value-bind (tx ty) (world->screen camera x y)
                            (draw-dashed-line sx sy tx ty *move-order-color*))
                          (draw-ghost regiment camera x y facing))))
                     (:attack
                      (let ((target (aref (battle-regiments battle) (second orders))))
                        (when (plusp (regiment-alive target))
                          (multiple-value-bind (tx ty) (multiple-value-call #'world->screen camera
                                                         (regiment-centroid target soldiers))
                            (draw-dashed-line sx sy tx ty *attack-order-color*)))))))))))

(defun order-text (regiment battle)
  (let ((orders (regiment-orders regiment)))
    (case (first orders)
      (:move "marche")
      (:attack (format nil "attaque ~a" (regiment-name (aref (battle-regiments battle) (second orders)))))
      (:retreat "repli")
      (t "en place"))))

(defun panel-line (command regiment battle)
  "Ligne d'état de REGIMENT, refaite seulement quand ce qu'elle dit change."
  (let* ((key (list (regiment-alive regiment) (round (regiment-morale regiment))
                    (round (* 10 (regiment-disorder regiment)))
                    (regiment-orders regiment) (regiment-drill regiment) (regiment-stance regiment)))
         (cached (gethash (regiment-index regiment) (command-panel command))))
    (if (and cached (equal (car cached) key))
        (cdr cached)
        (let ((text (format nil "~a : ~d/~d, moral ~d~:[~*~;, désordre ~,1f~] - ~a - ~a - ~a"
                            (regiment-name regiment) (regiment-alive regiment)
                            (regiment-capacity regiment) (round (regiment-morale regiment))
                            (plusp (regiment-disorder regiment)) (regiment-disorder regiment)
                            (order-text regiment battle)
                            (or (cdr (assoc (regiment-drill regiment) *drill-names*)) "")
                            (or (cdr (assoc (regiment-stance regiment) *stance-names*)) ""))))
          (setf (gethash (regiment-index regiment) (command-panel command)) (cons key text))
          text))))

(defun draw-command (command battle camera &optional preview)
  "Sélection, ordres des régiments choisis, cadre en cours et panneau d'état.
Avec PREVIEW (bataille en pause, ou Espace tenu), les ordres en cours de tous
les régiments du joueur (DRAW-ORDERS-PREVIEW)."
  (let ((regiments (selected-regiments command battle))
        (soldiers (battle-soldiers battle)))
    (when preview (draw-orders-preview battle camera))
    (dolist (regiment regiments)
      (draw-regiment-outline regiment soldiers camera *selection-color*)
      (draw-order-line regiment battle camera))
    ;; Cadre de sélection en cours.
    (let ((press (command-left-press command)))
      (when (mouse-moved-p press)
        (let ((x0 (car press)) (y0 (cdr press))
              (x1 (raylib:get-mouse-x)) (y1 (raylib:get-mouse-y)))
          (draw-line x0 y0 x1 y0 *selection-color*)
          (draw-line x1 y0 x1 y1 *selection-color*)
          (draw-line x1 y1 x0 y1 *selection-color*)
          (draw-line x0 y1 x0 y0 *selection-color*))))
    ;; Orientation en cours de choix : flèche du point d'arrivée vers la souris.
    (let ((press (command-right-press command)))
      (when (and regiments (mouse-moved-p press))
        (let* ((x0 (car press)) (y0 (cdr press))
               (x1 (raylib:get-mouse-x)) (y1 (raylib:get-mouse-y))
               (angle (atan (- y1 y0) (- x1 x0)))
               (head 14))
          (draw-line x0 y0 x1 y1 *move-order-color*)
          (dolist (side '(-1 1))
            (let ((a (+ angle pi (* side 0.5))))
              (draw-line x1 y1 (round (+ x1 (* head (cos a)))) (round (+ y1 (* head (sin a))))
                         *move-order-color*))))))
    ;; Panneau : une ligne par régiment choisi, au-dessus de l'aide.
    (when regiments
      (let* ((size (text-size *command-text-size*))
             (line-height (+ size 4))
             (lines (mapcar (lambda (r) (panel-line command r battle)) regiments))
             (shown (subseq lines 0 (min 8 (length lines))))
             (bottom (- *window-height* (text-size 16) 20))
             (top (- bottom (* line-height (length shown)))))
        (draw-rectangle 4 (- top 6) (+ 12 (loop for line in shown maximize (measure-text line size)))
                        (+ 8 (* line-height (length shown))) '(0 0 0 170))
        (loop for line in shown
              for y from top by line-height
              do (draw-text line 10 y size :white))))))
