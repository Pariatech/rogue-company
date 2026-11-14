(in-package #:rogue-company)

;;; Équipement : comment un homme de base et ses six emplacements donnent les
;;; stats effectives d'un régiment (stats.lisp). Le catalogue est dans items.lisp.
;;;
;;;   homme + équipement + modificateurs en plus (améliorations, plus tard)
;;;     → regiment-stats
;;;
;;; Un modificateur est une liste (STAT OPÉRATION VALEUR) :
;;;   - STAT est une stat commune (:armor) ou une stat d'attaque désignée par
;;;     (:primary :reach), (:secondary :damage), (:melee :damage) ou
;;;     (:ranged :accuracy) ; :melee et :ranged visent les profils de ce type ;
;;;   - OPÉRATION est :set, :add ou :mul, appliquées dans cet ordre quel que soit
;;;     l'ordre des modificateurs : une arme fixe une valeur, les bonus s'y
;;;     ajoutent, puis les multiplicateurs s'appliquent.
;;; Les durées d'attaque (:cooldown, :ready-time) se modifient en secondes.

(defparameter *loadout-slots* '(:main :secondary :shield :armor :helmet :mount)
  "Emplacements d'un régiment, dans l'ordre d'application de leurs modificateurs :
à :set égal, le dernier l'emporte, donc la monture fixe la vitesse.")

(defparameter *max-hands* 2)

(defparameter *improvised-attack*
  '(:kind :melee :reach 0.8 :damage 4 :accuracy -0.1 :cooldown 1.5 :ready-time 0.3)
  "Crosse ou poings : l'arme d'un régiment sans arme de mêlée, faible mais jamais nulle.")

(defparameter *feature-names*
  '((:mount . "une monture") (:shield . "un bouclier") (:main . "une arme principale")
    (:secondary . "une arme secondaire") (:armor . "une armure") (:helmet . "un casque"))
  "Noms des emplacements et des étiquettes dans les messages de validation.")

;;; Hommes et modèles d'objets

(defvar *men* (make-hash-table) "Hommes de base, par identifiant.")
(defvar *item-templates* (make-hash-table) "Modèles d'objets, par identifiant.")

(defclass man ()
  ((id :initarg :id :reader man-id)
   (name :initarg :name :reader man-name)
   (stats :initarg :stats :reader man-stats
          :documentation "Liste de propriétés des stats communes ; celles qui manquent valent 0.")))

(defclass item-template ()
  ((id :initarg :id :reader item-id)
   (name :initarg :name :reader item-name)
   (slots :initarg :slots :reader item-slots :documentation "Emplacements où l'objet peut aller.")
   (hands :initarg :hands :reader item-hands :documentation "Mains occupées au combat.")
   (weight :initarg :weight :reader item-weight :documentation "Kilogrammes.")
   (attack :initarg :attack :reader item-attack
           :documentation "Profil d'attaque d'une arme, en liste de propriétés, ou NIL.")
   (modifiers :initarg :modifiers :reader item-modifiers)
   (traits :initarg :traits :reader item-traits)
   (tags :initarg :tags :reader item-tags
         :documentation "Étiquettes visées par :requires et :forbids d'autres objets.")
   (requires :initarg :requires :reader item-requires
             :documentation "Emplacements ou étiquettes que le reste de l'équipement doit fournir.")
   (forbids :initarg :forbids :reader item-forbids)
   (price :initarg :price :reader item-price)))

(defun find-man (id)
  (or (gethash id *men*) (error "Homme inconnu : ~s." id)))

(defun find-item (id)
  (or (gethash id *item-templates*) (error "Objet inconnu : ~s." id)))

(defun define-man (id name &rest stats)
  "Déclare un homme de base. Une nouvelle définition remplace l'ancienne."
  (loop for (key) on stats by #'cddr
        unless (member key *common-stats*)
          do (error "~s : stat commune inconnue ~s." id key))
  (setf (gethash id *men*) (make-instance 'man :id id :name name :stats stats))
  id)

(defun attack-key (key)
  "Accepte :range et :reload comme noms de :reach et :cooldown, plus lisibles
pour une arme de tir."
  (case key (:range :reach) (:reload :cooldown) (t key)))

(defun check-attack (id attack)
  (unless (member (getf attack :kind) '(:melee :ranged))
    (error "~s : l'attaque doit avoir :kind :melee ou :ranged." id))
  (loop for (key) on attack by #'cddr
        unless (or (member key '(:kind :volley)) (member (attack-key key) *attack-stats*))
          do (error "~s : stat d'attaque inconnue ~s." id key)))

(defun check-modifier (id modifier)
  (destructuring-bind (stat operation value) modifier
    (unless (if (consp stat)
                (and (member (first stat) '(:primary :secondary :melee :ranged))
                     (member (second stat) *attack-stats*)
                     (null (cddr stat)))
                (member stat *common-stats*))
      (error "~s : stat inconnue dans le modificateur ~s." id modifier))
    (unless (member operation '(:set :add :mul))
      (error "~s : opération inconnue dans ~s (:set, :add ou :mul)." id modifier))
    (unless (realp value)
      (error "~s : valeur non numérique dans ~s." id modifier))))

(defun define-item (id name &key slots (hands 0) (weight 0) attack modifiers traits
                                 tags requires forbids (price 0))
  "Déclare un modèle d'objet. Une nouvelle définition remplace l'ancienne : un
changement d'équilibrage s'applique au prochain calcul de stats."
  (unless (and slots (subsetp slots *loadout-slots*))
    (error "~s : emplacements invalides ~s, parmi ~s." id slots *loadout-slots*))
  (when (and attack (not (intersection slots '(:main :secondary))))
    (error "~s : seule une arme a un profil d'attaque." id))
  (when attack (check-attack id attack))
  (dolist (modifier modifiers) (check-modifier id modifier))
  (setf (gethash id *item-templates*)
        (make-instance 'item-template :id id :name name :slots slots :hands hands
                                      :weight weight :attack attack :modifiers modifiers
                                      :traits traits :tags tags :requires requires
                                      :forbids forbids :price price))
  id)

;;; Validation

(defun loadout-items (loadout)
  "Couples (EMPLACEMENT . MODÈLE) de LOADOUT, une liste de propriétés
emplacement → identifiant d'objet, dans l'ordre de *LOADOUT-SLOTS*."
  (loop for slot in *loadout-slots*
        for id = (getf loadout slot)
        when id collect (cons slot (find-item id))))

(defun feature-name (feature)
  (or (cdr (assoc feature *feature-names*))
      (let ((item (gethash feature *item-templates*)))
        (and item (item-name item)))
      (string-downcase feature)))

(defun item-features (slot item)
  "Ce qu'un objet apporte au reste de l'équipement : son emplacement, son
identifiant et ses étiquettes."
  (list* slot (item-id item) (item-tags item)))

(defun validate-loadout (loadout)
  "Liste des raisons pour lesquelles LOADOUT est refusé, NIL s'il est valide.
Les messages sont destinés à l'écran d'équipement."
  (let ((reasons '()))
    (flet ((refuse (control &rest args)
             (push (apply #'format nil control args) reasons)))
      (loop for (slot id) on loadout by #'cddr
            do (cond ((not (member slot *loadout-slots*))
                      (refuse "Emplacement inconnu : ~(~a~)." slot))
                     ((not (gethash id *item-templates*))
                      (refuse "Objet inconnu : ~(~a~)." id))
                     ((not (member slot (item-slots (find-item id))))
                      (refuse "~a ne va pas dans l'emplacement ~a."
                              (item-name (find-item id)) (feature-name slot)))))
      (when reasons
        (return-from validate-loadout (nreverse reasons)))
      (let* ((items (loadout-items loadout))
             ;; L'arme secondaire remplace l'arme principale, elle ne s'y ajoute pas.
             (held (remove-if-not (lambda (entry) (member (car entry) '(:main :shield))) items))
             (hands (reduce #'+ held :key (lambda (entry) (item-hands (cdr entry))))))
        (when (> hands *max-hands*)
          (refuse "~{~a~^ et ~} demandent ~d mains au combat, ~d au plus."
                  (mapcar (lambda (entry) (format nil "~a (~d)" (item-name (cdr entry))
                                                  (item-hands (cdr entry))))
                          held)
                  hands *max-hands*))
        (loop for entry in items
              for item = (cdr entry)
              for others = (loop for other in items
                                 unless (eq other entry)
                                   append (item-features (car other) (cdr other)))
              do (dolist (needed (item-requires item))
                   (unless (member needed others)
                     (refuse "~a demande ~a." (item-name item) (feature-name needed))))
                 (dolist (forbidden (item-forbids item))
                   (when (member forbidden others)
                     (refuse "~a est incompatible avec ~a." (item-name item)
                             (feature-name forbidden)))))))
    (nreverse reasons)))

;;; Calcul des stats

(defun attack-values (attack)
  "Table des stats numériques d'une attaque en liste de propriétés."
  (let ((values (make-hash-table)))
    (loop for (key value) on attack by #'cddr
          unless (member key '(:kind :volley))
            do (setf (gethash (attack-key key) values) value))
    values))

(defun apply-operation (table key operation value)
  (let ((current (gethash key table 0)))
    (setf (gethash key table)
          (ecase operation
            (:set value)
            (:add (+ current value))
            (:mul (* current value))))))

(defun modifier-tables (stat common attacks)
  "Tables visées par STAT. ATTACKS est une liste de (DÉSIGNATION GENRE TABLE)."
  (if (consp stat)
      (loop for (designation kind table) in attacks
            when (or (eq (first stat) designation) (eq (first stat) kind))
              collect table)
      (list common)))

(defun compute-regiment-stats (man loadout &key modifiers traits)
  "Stats effectives de l'homme MAN (identifiant) équipé de LOADOUT, plus les
MODIFIERS et TRAITS donnés en plus (améliorations). Signale une erreur si
l'équipement est refusé."
  (let ((reasons (validate-loadout loadout)))
    (when reasons
      (error "Équipement refusé : ~{~a~^ ~}" reasons)))
  (dolist (modifier modifiers) (check-modifier :modifiers modifier))
  (let* ((items (loadout-items loadout))
         (common (make-hash-table))
         (primary-attack (or (let ((main (getf loadout :main)))
                               (and main (item-attack (find-item main))))
                             *improvised-attack*))
         (secondary-attack (or (let ((secondary (getf loadout :secondary)))
                                 (and secondary (item-attack (find-item secondary))))
                               *improvised-attack*))
         (primary (attack-values primary-attack))
         (secondary (attack-values secondary-attack))
         (attacks (list (list :primary (getf primary-attack :kind) primary)
                        (list :secondary (getf secondary-attack :kind) secondary)))
         (all-modifiers (append (loop for (nil . item) in items
                                      append (item-modifiers item))
                                modifiers)))
    (loop for (key value) on (man-stats (find-man man)) by #'cddr
          do (setf (gethash key common) value))
    (setf (gethash :load common)
          (loop for (slot . item) in items
                unless (eq slot :mount) sum (item-weight item)))
    (dolist (operation '(:set :add :mul))
      (loop for (stat op value) in all-modifiers
            when (eq op operation)
              do (dolist (table (modifier-tables stat common attacks))
                   (apply-operation table (if (consp stat) (second stat) stat) op value))))
    (make-regiment-stats
     common
     (make-attack-profile (getf primary-attack :kind) (getf primary-attack :volley) primary)
     (make-attack-profile (getf secondary-attack :kind) (getf secondary-attack :volley) secondary)
     (remove-duplicates (append (loop for (nil . item) in items append (item-traits item))
                                traits)
                        :from-end t))))
