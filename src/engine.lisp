(in-package #:rogue-company)

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *fixed-dt* (/ 1.0 60)
  "Pas de temps utilisé en mode pas à pas.")
(defparameter *captures-directory*
  (asdf:system-relative-pathname :rogue-company "captures/"))
(defparameter *max-dt* 0.1
  "Plafond du pas de temps, pour éviter les sauts après un ralentissement.")

(defvar *state*)
(defvar *frame* 0 "Numéro de l'image courante.")
(defvar *paused* nil
  "NIL si le jeu tourne, :USER si mis en pause à la demande, :ERROR après une erreur.")
(defvar *last-error* nil
  "Dernière erreur capturée : plist (:where :message :backtrace :frame).")
(defgeneric game-snapshot (state)
  (:documentation "Résumé de *STATE* pour game_state. game.lisp le spécialise.")
  (:method (state) (prin1-to-string state)))

(defvar *draw-override* nil
  "Si non NIL, fonction sans argument dessinée à la place de DRAW, par exemple
le banc d'essai de rendu.")

;;; Mesure du temps par section

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *timing-sections* '(:jobs :console :update :draw :end-drawing)
    "Sections mesurées à chaque image, dans l'ordre des lignes de *TIMINGS*. La
dernière ligne de *TIMINGS* est l'image entière. Changer cette liste demande
scripts/dev.sh restart, car *TIMINGS* garde ses dimensions."))

(defvar *timings*
  (make-array (list (1+ (length *timing-sections*)) 1200)
              :element-type 'single-float :initial-element 0f0)
  "Millisecondes par section (ligne) et par image (colonne, tampon circulaire).")
(defvar *consed-per-frame*
  (make-array 1200 :element-type '(unsigned-byte 62) :initial-element 0)
  "Octets alloués par image, même tampon circulaire que *TIMINGS*.")
(defvar *timing-frames* 0 "Images mesurées depuis le démarrage.")

(declaim (inline timing-column))
(defun timing-column ()
  (mod *timing-frames* (array-dimension *timings* 1)))

(defun elapsed-ms (start)
  (/ (float (- (get-internal-real-time) start) 1f0)
     (/ internal-time-units-per-second 1000f0)))

(defun add-timing (row start)
  (incf (aref *timings* row (timing-column)) (elapsed-ms start)))

(defmacro timed ((section) &body body)
  "Exécute BODY et ajoute sa durée à SECTION pour l'image en cours."
  (let ((row (or (position section *timing-sections*)
                 (error "Section inconnue : ~s (voir *timing-sections*)." section)))
        (start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (unwind-protect (progn ,@body)
         (add-timing ,row ,start)))))

(defun begin-frame-timing ()
  (let ((column (timing-column)))
    (dotimes (row (array-dimension *timings* 0))
      (setf (aref *timings* row column) 0f0))))

(defun end-frame-timing (start consed)
  (let ((column (timing-column)))
    (setf (aref *timings* (1- (array-dimension *timings* 0)) column) (elapsed-ms start)
          (aref *consed-per-frame* column) (max 0 (- (sb-ext:get-bytes-consed) consed))))
  (incf *timing-frames*))

;;; Erreurs

(defun record-error (where condition)
  (setf *last-error*
        (list :where where
              :message (handler-case (princ-to-string condition)
                         (error () (format nil "~s" (type-of condition))))
              :backtrace (with-output-to-string (s)
                           (sb-debug:print-backtrace :count 25 :stream s))
              :frame *frame*)
        *paused* :error)
  (format *error-output* "~&[~a] ~a~%" where (getf *last-error* :message)))

(defmacro guarded (where &body body)
  "Exécute BODY. En cas d'erreur, l'enregistre dans *LAST-ERROR* avec la pile
d'appels, met le jeu en pause et renvoie NIL."
  (let ((block (gensym "GUARDED")))
    `(block ,block
       (handler-bind ((error (lambda (condition)
                               (record-error ,where condition)
                               (return-from ,block nil))))
         ,@body))))

(defun clear-error ()
  "Efface la dernière erreur et reprend le jeu s'il était en pause à cause d'elle."
  (setf *last-error* nil)
  (when (eq *paused* :error)
    (setf *paused* nil)))

(defun one-line (text)
  "Remplace chaque suite d'espaces et de sauts de ligne de TEXT par une espace."
  (with-output-to-string (out)
    (loop with space = nil
          for char across (string-trim '(#\Space #\Tab #\Newline) text)
          do (if (member char '(#\Space #\Tab #\Newline))
                 (setf space t)
                 (progn (when space (write-char #\Space out) (setf space nil))
                        (write-char char out))))))

(defun draw-error-overlay ()
  (when *last-error*
    (let ((message (one-line (getf *last-error* :message))))
      (draw-rectangle 0 0 *window-width* 90 '(40 0 0 220))
      (draw-text (format nil "Erreur dans ~(~a~), image ~d : jeu en pause"
                                (getf *last-error* :where) (getf *last-error* :frame))
                        10 10 20 :white)
      (draw-text (subseq message 0 (min 70 (length message)))
                        10 40 20 :white)
      (draw-text "Corriger puis game_reload pour reprendre." 10 65 16 :lightgray))))

;;; Boucle

(defun next-dt ()
  "Renvoie le pas de temps de l'image, ou NIL si le jeu est en pause."
  (unless *paused*
    (min (raylib:get-frame-time) *max-dt*)))

(defun run-update (dt)
  "Exécute une mise à jour. Renvoie NIL si elle a échoué."
  (prog1 (guarded :update (update *state* dt) t)
    (advance-input)))

(defun step-frames (count)
  "Met le jeu en pause et simule COUNT mises à jour avec un pas fixe, sans
attendre l'affichage. S'arrête à la première erreur. Renvoie le nombre
d'images simulées."
  (unless *paused* (setf *paused* :user))
  (loop for done from 0 below count
        do (unless (run-update *fixed-dt*)
             (return done))
           (incf *frame*)
        finally (return count)))

(defun capture-screen ()
  "Enregistre l'image en cours de dessin dans *CAPTURES-DIRECTORY* et renvoie
le chemin absolu. À appeler pendant WITH-DRAWING, depuis le thread principal."
  (let ((path (merge-pathnames (format nil "capture-~d-~d.png" (get-universal-time) *frame*)
                               *captures-directory*)))
    (ensure-directories-exist path)
    ;; Vide le lot de dessin pour que le tampon contienne toute l'image.
    (rlgl:draw-render-batch-active)
    (let ((image (raylib:load-image-from-screen)))
      (unwind-protect
           (unless (raylib:export-image image (namestring path))
             (error "Échec de l'export de ~a" path))
        (raylib:unload-image image)))
    (namestring path)))

(defun draw-error-p ()
  (eq (getf *last-error* :where) :draw))

(defun console-error-p ()
  (eq (getf *last-error* :where) :console))

(defun run-frame ()
  (let ((start (get-internal-real-time))
        (consed (sb-ext:get-bytes-consed)))
    (begin-frame-timing)
    (timed (:jobs) (run-pending-jobs :max 8))
    ;; La console tourne aussi en pause ; après une erreur de sa part, elle attend
    ;; la correction pour ne pas la répéter à chaque image.
    (unless (console-error-p)
      (timed (:console) (guarded :console (console-update))))
    (let ((dt (next-dt)))
      (when dt (timed (:update) (run-update dt))))
    ;; WITH-DRAWING n'est pas utilisé : END-DRAWING (envoi au GPU, échange des
    ;; tampons, attente du rythme cible) est mesuré à part.
    (raylib:begin-drawing)
    (unwind-protect
         (progn
           (timed (:draw)
             (cond ((draw-error-p) (clear-background :black))
                   (*draw-override* (guarded :draw (funcall *draw-override*)))
                   (t (guarded :draw (draw *state*)))))
           (unless (console-error-p)
             (timed (:console) (guarded :console (console-draw))))
           (draw-error-overlay)
           (timed (:jobs) (run-pending-jobs :max 1 :queue *drawing-jobs*)))
      (timed (:end-drawing) (raylib:end-drawing)))
    (end-frame-timing start consed))
  (incf *frame*))

(defun main ()
  (unless (sb-thread:main-thread-p)
    (error "main doit tourner sur le thread principal (exigence de Cocoa)."))
  (start-mcp-server)
  (unwind-protect
       (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
         (raylib:with-window (*window-width* *window-height* "rogue-company")
           (raylib:set-target-fps 60)
           ;; Échap sert au jeu (retour au menu) : la fenêtre se ferme par son bouton.
           (raylib:set-exit-key :key-null)
           (loop until (raylib:window-should-close)
                 do (run-frame))))
    (stop-mcp-server)))
