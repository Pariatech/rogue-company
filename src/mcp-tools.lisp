(in-package #:rogue-company)

;;; Outils MCP. Ils s'exécutent sur les threads du serveur HTTP : tout accès à
;;; raylib ou à *STATE* passe par CALL-IN-MAIN-THREAD.

(defparameter *game-source* (asdf:system-relative-pathname :rogue-company "src/game.lisp"))

;;; Sérialisation de l'état

(defun json-value (value)
  "Convertit VALUE en donnée JSON ; ce qui n'a pas d'équivalent devient sa représentation Lisp."
  (typecase value
    ((eql t) t)
    (null 'yason:false)
    (integer value)
    (real (float value 1d0))
    (string value)
    (keyword (string-downcase value))
    (t (prin1-to-string value))))

(defun object-slots (object)
  (let ((slots (ht)))
    (dolist (slot (sb-mop:class-slots (class-of object)) slots)
      (let ((name (sb-mop:slot-definition-name slot)))
        (setf (gethash (string-downcase name) slots)
              (if (slot-boundp object name)
                  (json-value (slot-value object name))
                  "<non liée>"))))))

(defun state-snapshot ()
  "Résumé de l'état du jeu. À appeler sur le thread principal."
  (ht "frame" *frame*
      "fps" (raylib:get-fps)
      "paused" (json-value *paused*)
      "state" (if (boundp '*state*) (object-slots *state*) :null)
      "simulated_keys" (let ((keys (ht)))
                         (maphash (lambda (key frames)
                                    (setf (gethash (string-downcase key) keys) frames))
                                  *simulated-keys*)
                         keys)
      "last_error" (if *last-error*
                       (ht "where" (json-value (getf *last-error* :where))
                           "message" (getf *last-error* :message)
                           "frame" (getf *last-error* :frame)
                           "backtrace" (getf *last-error* :backtrace))
                       :null)))

(defun state-json ()
  (to-json (call-in-main-thread #'state-snapshot)))

;;; game_eval

(defun eval-string (code)
  "Lit et évalue toutes les formes de CODE dans le package du jeu. Renvoie la
sortie standard et les valeurs de la dernière forme."
  (let* ((*package* (find-package '#:rogue-company))
         (values '())
         (output (with-output-to-string (*standard-output*)
                   (with-input-from-string (in code)
                     (loop with eof = (gensym)
                           for form = (read in nil eof)
                           until (eq form eof)
                           do (setf values (multiple-value-list (eval form))))))))
    (format nil "~@[Sortie :~%~a~%~]Valeurs :~%~{~s~%~}"
            (when (plusp (length output)) output)
            (or values '("aucune")))))

(define-mcp-tool "game_eval"
    ((code "code" "string" "Formes Lisp à évaluer, dans le package rogue-company." :required t))
    "Évalue du code Lisp dans le jeu en cours, sur le thread principal entre deux
images (limite 2 s). Renvoie la sortie standard et les valeurs, ou l'erreur avec
sa pile d'appels. Réservé à l'exploration : les changements durables vont dans src/."
  (call-in-main-thread (lambda () (eval-string code))))

;;; game_reload

(defun compile-game-source ()
  "Compile src/game.lisp dans un fichier temporaire. Renvoie le fasl (ou NIL en
cas d'échec) et la liste des messages du compilateur."
  (let ((fasl (uiop:tmpize-pathname
               (merge-pathnames "rogue-company-game.fasl" (uiop:temporary-directory))))
        (messages '())
        (log (make-string-output-stream)))
    (multiple-value-bind (output warnings-p failure-p)
        (handler-bind ((warning (lambda (w)
                                  (push (format nil "~:[Avertissement~;Style~] : ~a"
                                                (typep w 'style-warning) w)
                                        messages))))
          (let ((*package* (find-package '#:rogue-company))
                (*standard-output* (make-broadcast-stream))
                (*error-output* log))
            (handler-case (compile-file *game-source* :output-file fasl)
              (error (e)
                (push (format nil "Erreur : ~a" e) messages)
                (values nil t t)))))
      (declare (ignore warnings-p))
      ;; Les erreurs de lecture ne sont pas signalées : seul le journal les décrit.
      (when (and failure-p (null messages))
        (push (get-output-stream-string log) messages))
      (values (and output (not failure-p) output)
              (nreverse messages)))))

(define-mcp-tool "game_reload" ()
    "Compile et charge src/game.lisp dans le jeu en cours, en conservant *state*.
Si la compilation échoue (erreur ou avertissement), rien n'est chargé. Après un
chargement réussi, la dernière erreur est effacée et le jeu reprend."
  (multiple-value-bind (fasl messages) (compile-game-source)
    (if (null fasl)
        (error "Compilation en échec, rien n'a été chargé.~%~{~a~%~}" messages)
        (unwind-protect
             (progn
               (call-in-main-thread (lambda () (load fasl) (clear-error)))
               (format nil "Rechargé.~:[ Aucun avertissement.~;~%~:*~{~a~%~}~]" messages))
          (delete-file fasl)))))

;;; game_state, game_screenshot

(define-mcp-tool "game_state" ()
    "Renvoie l'état du jeu en JSON : image courante, FPS, pause, slots de *state*,
touches simulées et dernière erreur avec sa pile d'appels."
  (state-json))

(define-mcp-tool "game_screenshot" ()
    "Capture la fenêtre à la fin du dessin de l'image suivante et renvoie le chemin
absolu du PNG, à ouvrir ensuite avec l'outil de lecture de fichiers."
  (call-in-main-thread #'capture-screen :queue *drawing-jobs*))

;;; game_input

(defun parse-key (name)
  "Convertit \"right\", \"KEY_RIGHT\" ou \"key-right\" en touche raylib."
  (let* ((name (substitute #\- #\_ (string-upcase name)))
         (name (if (eql 0 (search "KEY-" name)) name (concatenate 'string "KEY-" name)))
         (key (find-symbol name :keyword)))
    (if (and key (cffi:foreign-enum-value 'raylib::keyboardkey key :errorp nil))
        key
        (error "Touche inconnue : ~a (exemples : right, space, a, enter)." name))))

(define-mcp-tool "game_input"
    ((key "key" "string" "Touche raylib : right, left, up, down, space, a, enter…" :required t)
     (frames "frames" "integer" "Nombre d'images pendant lesquelles la touche reste enfoncée (1 par défaut)."))
    "Simule une touche enfoncée pendant un nombre d'images, sans focus de fenêtre.
Si le jeu tourne, attend que ces images soient passées puis renvoie l'état. Si le
jeu est en pause, la touche reste en attente : utiliser game_control step."
  (let ((key (parse-key key))
        (frames (or frames 1)))
    (unless (and (integerp frames) (<= 1 frames 600))
      (error "frames doit être un entier entre 1 et 600."))
    (let ((paused (call-in-main-thread
                   (lambda ()
                     (setf (gethash key *simulated-keys*) frames)
                     *paused*))))
      (unless paused
        ;; À 9 images/s (fenêtre cachée), 600 images prennent environ 70 s.
        (loop with deadline = (+ (get-internal-real-time)
                                 (* (+ 5 (/ frames 8)) internal-time-units-per-second))
              while (call-in-main-thread (lambda () (and (not *paused*)
                                                         (gethash key *simulated-keys*))))
              do (when (> (get-internal-real-time) deadline)
                   (error "La touche n'a pas été consommée à temps : la fenêtre est-elle ralentie ?"))
                 (sleep 0.02)))
      (format nil "~:[~;Jeu en pause : touche en attente, utiliser game_control step.~%~]~a"
              paused (state-json)))))

;;; game_control

(define-mcp-tool "game_control"
    ((action "action" "string" "pause, resume ou step." :required t :enum ("pause" "resume" "step"))
     (frames "frames" "integer" "Pour step : nombre d'images à simuler (1 par défaut)."))
    "Met le jeu en pause, le relance (en effaçant la dernière erreur), ou simule des
images en pause avec un pas fixe de 1/60 s, indépendamment de l'affichage. Renvoie
l'état après l'action."
  (let ((frames (or frames 1)))
    (unless (and (integerp frames) (<= 1 frames 6000))
      (error "frames doit être un entier entre 1 et 6000."))
    (unless (member action '("pause" "resume" "step") :test #'equal)
      (error "Action inconnue : ~a (pause, resume ou step)." action))
    (call-in-main-thread
     (lambda ()
       (cond ((string= action "pause") (unless *paused* (setf *paused* :user)))
             ((string= action "resume") (clear-error) (setf *paused* nil))
             ((string= action "step") (step-frames frames)))))
    (state-json)))

(define-mcp-tool "game_ping" ()
    "Vérifie que le jeu répond. Renvoie le numéro de l'image courante."
  (call-in-main-thread
   (lambda () (format nil "pong (image ~d)" *frame*))))
