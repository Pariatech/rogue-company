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
      (raylib:draw-rectangle 0 0 *window-width* 90 '(40 0 0 220))
      (raylib:draw-text (format nil "Erreur dans ~(~a~), image ~d : jeu en pause"
                                (getf *last-error* :where) (getf *last-error* :frame))
                        10 10 20 :white)
      (raylib:draw-text (subseq message 0 (min 70 (length message)))
                        10 40 20 :white)
      (raylib:draw-text "Corriger puis game_reload pour reprendre." 10 65 16 :lightgray))))

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
  (run-pending-jobs :max 8)
  ;; La console tourne aussi en pause ; après une erreur de sa part, elle attend
  ;; la correction pour ne pas la répéter à chaque image.
  (unless (console-error-p)
    (guarded :console (console-update)))
  (let ((dt (next-dt)))
    (when dt (run-update dt)))
  (raylib:with-drawing
    (if (draw-error-p)
        (raylib:clear-background :black)
        (guarded :draw (draw *state*)))
    (unless (console-error-p)
      (guarded :console (console-draw)))
    (draw-error-overlay)
    (run-pending-jobs :max 1 :queue *drawing-jobs*))
  (incf *frame*))

(defun main ()
  (unless (sb-thread:main-thread-p)
    (error "main doit tourner sur le thread principal (exigence de Cocoa)."))
  (start-mcp-server)
  (unwind-protect
       (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
         (raylib:with-window (*window-width* *window-height* "rogue-company")
           (raylib:set-target-fps 60)
           (loop until (raylib:window-should-close)
                 do (run-frame))))
    (stop-mcp-server)))
