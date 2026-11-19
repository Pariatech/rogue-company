(in-package #:rogue-company)

;;; Revoir une bataille. La simulation est déterministe : une bataille se
;;; rejoue à l'identique depuis sa recette (BATTLE-RECIPE : MAKE-AI-BATTLE ou
;;; MAKE-DEMO-BATTLE, graine, régiments) et les ordres du joueur, chacun au tick
;;; où il les a donnés (BATTLE-ORDER-LOG, PLAYER-ORDER dans command.lisp). L'IA
;;; redonne d'elle-même les siens.
;;;
;;; Un replay s'écrit en données lisibles (SAVE-REPLAY) dans
;;; *REPLAY-DIRECTORY*, ignoré par git : la dernière bataille dans
;;; *LAST-REPLAY*. Il garde aussi le tick de l'enregistrement et l'empreinte de
;;; la bataille à ce tick (BATTLE-DIGEST) : revue jusque-là (REPLAY-TICK), elle
;;; doit retomber sur la même empreinte. Sinon la simulation a changé depuis
;;; l'enregistrement, et le replay diverge (:DESYNC).

(defparameter *replay-directory* (asdf:system-relative-pathname :rogue-company "replays/"))

(defparameter *last-replay* "derniere.sexp" "Fichier du replay de la dernière bataille.")

(defparameter *replay-version* 1 "Version du format des fichiers de replay.")

(defparameter *replay-builders* '(make-demo-battle make-ai-battle)
  "Seules fonctions qu'une recette lue d'un fichier peut appeler.")

(defparameter *replay-orders* '(:move :attack :halt :drill :stance :place :charge)
  "Seuls ordres qu'un fichier de replay peut contenir (EXECUTE-ORDER).")

(defclass replay ()
  ((recipe :initarg :recipe :reader replay-recipe)
   (orders :initarg :orders :reader replay-orders
           :documentation "Vecteur de (tick . ordre), dans l'ordre où ils ont été donnés.")
   (end-tick :initarg :end-tick :reader replay-end-tick
             :documentation "Tick de l'enregistrement.")
   (digest :initarg :digest :reader replay-digest
           :documentation "Empreinte de la bataille à END-TICK."))
  (:documentation "Une bataille à revoir : sa recette, les ordres du joueur, et de
quoi vérifier qu'elle se rejoue à l'identique."))

(defun battle-replay (battle)
  "Le replay de BATTLE jusqu'à son tick courant, ou NIL sans recette."
  (when (battle-recipe battle)
    (make-instance 'replay
                   :recipe (copy-tree (battle-recipe battle))
                   :orders (map 'vector #'copy-tree (battle-order-log battle))
                   :end-tick (battle-tick-count battle)
                   :digest (battle-digest battle))))

(defun replay-path (&optional (name *last-replay*))
  (merge-pathnames name *replay-directory*))

(defun write-replay (replay path)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package '#:rogue-company))
            (*print-pretty* t) (*print-case* :downcase) (*print-right-margin* 100)
            (*print-readably* t))
        (format out ";;; Replay de rogue-company (replay.lisp).~%")
        (print (list :version *replay-version*
                     :recipe (replay-recipe replay)
                     :end-tick (replay-end-tick replay)
                     :digest (replay-digest replay)
                     :orders (coerce (replay-orders replay) 'list))
               out)
        (terpri out))))
  path)

(defun save-replay (battle &optional (path (replay-path)))
  "Écrit le replay de BATTLE dans PATH. Renvoie PATH, ou NIL si BATTLE n'a pas de
recette (rien à revoir)."
  (let ((replay (battle-replay battle)))
    (when replay (write-replay replay path))))

(defun valid-order-p (entry)
  (and (consp entry) (integerp (car entry)) (>= (car entry) 0)
       (consp (cdr entry)) (member (cadr entry) *replay-orders*)))

(defun load-replay (&optional (path (replay-path)))
  "Le replay écrit dans PATH, ou NIL s'il n'y en a pas. Lu sans évaluation ; une
recette qui n'appelle pas une des *REPLAY-BUILDERS*, ou un ordre inconnu, est
refusé (erreur)."
  (when (probe-file path)
    (let ((data (with-open-file (in path)
                  (with-standard-io-syntax
                    (let ((*package* (find-package '#:rogue-company))
                          (*read-eval* nil))
                      (read in))))))
      (unless (and (listp data) (eql (getf data :version) *replay-version*))
        (error "~a : format de replay inconnu." path))
      (let ((recipe (getf data :recipe))
            (orders (getf data :orders)))
        (unless (and (consp recipe) (member (first recipe) *replay-builders*))
          (error "~a : recette refusée ~s." path (first recipe)))
        (unless (every #'valid-order-p orders)
          (error "~a : ordre refusé ~s." path (find-if-not #'valid-order-p orders)))
        (make-instance 'replay
                       :recipe recipe
                       :orders (coerce orders 'vector)
                       :end-tick (getf data :end-tick)
                       :digest (getf data :digest))))))

(defun replay-start (replay)
  "La bataille de REPLAY, neuve, au tick 0."
  (apply (first (replay-recipe replay)) (rest (replay-recipe replay))))

(defun replay-tick (battle replay cursor)
  "Avance d'un tick BATTLE, revue d'après REPLAY : exécute d'abord les ordres
donnés à ce tick, à partir du numéro CURSOR. Renvoie le nouveau curseur, et
:MATCH ou :DESYNC au tick de l'enregistrement (empreinte retrouvée ou non),
NIL sinon."
  (let ((orders (replay-orders replay)))
    (loop while (and (< cursor (length orders))
                     (<= (car (aref orders cursor)) (battle-tick-count battle)))
          do (execute-order battle (cdr (aref orders cursor)))
             (incf cursor))
    (battle-tick battle)
    (values cursor
            (when (= (battle-tick-count battle) (replay-end-tick replay))
              (if (string= (battle-digest battle) (replay-digest replay)) :match :desync)))))
