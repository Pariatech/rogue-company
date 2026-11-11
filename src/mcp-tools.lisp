(in-package #:rogue-company)

;;; Outils MCP. Ils s'exécutent sur les threads du serveur HTTP : tout accès à
;;; raylib ou à *STATE* passe par CALL-IN-MAIN-THREAD.

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
      "console" (console-snapshot)
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

(defparameter *infrastructure-files* '("package" "mcp-server" "jobs" "mcp-tools")
  "Fichiers jamais rechargés à chaud : ils portent le serveur, la file de tâches
et les outils en cours d'exécution. Les modifier demande scripts/dev.sh restart.")
(defparameter *reload-run-timeout* 10
  "Secondes accordées au chargement des fichiers sur le thread principal.")

(defun source-files ()
  "Fichiers du système, dans l'ordre de chargement : liste de (nom . chemin)."
  (mapcar (lambda (component)
            (cons (asdf:component-name component) (asdf:component-pathname component)))
          (asdf:component-children (asdf:find-system :rogue-company))))

(defun file-digest (path)
  (sb-md5:md5sum-file path))

(defvar *loaded-digests*
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name . path) in (source-files)
          do (setf (gethash name table) (file-digest path)))
    table)
  "Nom du fichier -> empreinte MD5 du contenu chargé dans le jeu en cours.")

(defvar *reload-lock* (sb-thread:make-mutex :name "game-reload")
  "Empêche deux game_reload simultanés.")

(defun plan-reload (names changed infrastructure)
  "NAMES : fichiers dans l'ordre de chargement. CHANGED : fichiers modifiés.
Renvoie trois listes, dans l'ordre de NAMES : les fichiers à recompiler (le premier
modifié et tous ceux qui le suivent, hors infrastructure), les fichiers
d'infrastructure sautés, et les fichiers d'infrastructure modifiés, qui bloquent
tout rechargement."
  (flet ((changed-p (name) (member name changed :test #'string=))
         (infrastructure-p (name) (member name infrastructure :test #'string=)))
    (let ((blocked (remove-if-not (lambda (name) (and (changed-p name) (infrastructure-p name)))
                                  names))
          (first (position-if #'changed-p names)))
      (if (or blocked (null first))
          (values '() '() blocked)
          (let ((tail (nthcdr first names)))
            (values (remove-if #'infrastructure-p tail)
                    (remove-if-not #'infrastructure-p tail)
                    '()))))))

(defun normalize-file-name (name names)
  "Accepte \"engine\", \"engine.lisp\" ou \"src/engine.lisp\"."
  (let ((base (pathname-name (pathname name))))
    (or (find base names :test #'string=)
        (error "Fichier inconnu : ~a (fichiers : ~{~a~^, ~})." name names))))

(defun parse-file-list (files)
  "Accepte une liste JSON de noms, ou une chaîne : tableau JSON, ou noms séparés
par des virgules ou des espaces. Certains clients envoient les tableaux en chaîne."
  (flet ((check (list)
           (if (every #'stringp list)
               list
               (error "files doit être une liste de noms de fichiers."))))
    (cond ((listp files) (check files))
          ((not (stringp files)) (check (list files)))
          ((eql 0 (position #\[ (string-left-trim " " files)))
           (check (coerce (yason:parse files) 'list)))
          (t (loop with start = 0
                   for end = (position-if (lambda (c) (find c ", ")) files :start start)
                   for name = (subseq files start end)
                   unless (string= name "") collect name
                   while end
                   do (setf start (1+ end)))))))

(defun compile-sources (files)
  "Compile FILES, liste de (nom . chemin), dans l'ordre et dans une seule unité
de compilation. Renvoie la liste des fasls, ou NIL dès qu'un fichier échoue
(erreur ou avertissement), et les messages du compilateur."
  (let ((messages '())
        (fasls '())
        (current nil)
        (ok t))
    (flet ((note (format &rest args)
             (push (format nil "~a : ~?" (or current "compilation") format args) messages)))
      ;; Les avertissements reportés à la fin de l'unité de compilation ne passent
      ;; pas par FAILURE-P : ce gestionnaire les compte aussi.
      ;; Une redéfinition est le but même d'un rechargement : ces avertissements sont
      ;; étouffés.
      (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning)
                     (warning (lambda (w)
                                (unless (typep w 'style-warning) (setf ok nil))
                                (note "~:[Avertissement~;Style~] : ~a" (typep w 'style-warning) w))))
        (let ((*package* (find-package '#:rogue-company))
              (*standard-output* (make-broadcast-stream)))
          (with-compilation-unit ()
            (loop for (name . path) in files
                  for fasl = (uiop:tmpize-pathname
                              (merge-pathnames (format nil "rogue-company-~a.fasl" name)
                                               (uiop:temporary-directory)))
                  for log = (make-string-output-stream)
                  do (setf current name)
                     (multiple-value-bind (output warnings-p failure-p)
                         (let ((*error-output* log))
                           (handler-case (compile-file path :output-file fasl)
                             (error (e)
                               (note "Erreur : ~a" e)
                               (values nil t t))))
                       (declare (ignore warnings-p))
                       (when output (push output fasls))
                       (when failure-p
                         ;; Les erreurs de lecture ne sont pas signalées : seul le journal les décrit.
                         (unless (find-if (lambda (m) (search (format nil "~a :" name) m)) messages)
                           (note "~a" (get-output-stream-string log)))
                         (setf ok nil)
                         (loop-finish)))))
          (setf current nil)
          (if ok
              (values (nreverse fasls) (nreverse messages))
              (progn (mapc #'uiop:delete-file-if-exists fasls)
                     (values nil (nreverse messages)))))))))

(defun reload-sources (requested)
  "Recompile et recharge les fichiers modifiés depuis leur dernier chargement, ou
REQUESTED s'il est fourni. Renvoie un compte rendu."
  (let* ((files (source-files))
         (names (mapcar #'car files))
         (digests (loop for (name . path) in files
                        collect (cons name (file-digest path))))
         (changed (if requested
                      (mapcar (lambda (name) (normalize-file-name name names)) requested)
                      (loop for (name . digest) in digests
                            unless (equalp digest (gethash name *loaded-digests*))
                              collect name))))
    (multiple-value-bind (to-reload skipped blocked)
        (plan-reload names changed *infrastructure-files*)
      (when blocked
        (error "Rien n'a été rechargé : ~{~a~^, ~} ne se recharge pas à chaud.~%~
Lancer scripts/dev.sh restart." blocked))
      (when (null to-reload)
        (return-from reload-sources "Rien à recharger : aucun fichier modifié."))
      (multiple-value-bind (fasls messages)
          (compile-sources (mapcar (lambda (name) (assoc name files :test #'string=)) to-reload))
        (when (null fasls)
          (error "Compilation en échec, rien n'a été chargé.~%~{~a~%~}" messages))
        (let ((loaded '()))
          (unwind-protect
               (handler-case
                   (call-in-main-thread
                    (lambda ()
                      (loop for name in to-reload
                            for fasl in fasls
                            do (load fasl)
                               (setf (gethash name *loaded-digests*)
                                     (cdr (assoc name digests :test #'string=)))
                               (push name loaded))
                      (clear-error))
                    :run-timeout *reload-run-timeout*
                    :wait (+ *job-wait-timeout* *reload-run-timeout*))
                 (job-error (e)
                   (error "Chargement interrompu. Chargés : ~:[aucun~;~:*~{~a~^, ~}~].~%~a"
                          (reverse loaded) e)))
            (mapc #'uiop:delete-file-if-exists fasls))
          (format nil "Rechargés : ~{~a~^, ~}.~@[~%Sautés (infrastructure, redémarrer si une macro ~
qu'ils utilisent a changé) : ~{~a~^, ~}.~]~:[~%Aucun avertissement.~;~%~:*~{~a~%~}~]"
                  to-reload skipped messages))))))

(define-mcp-tool "game_reload"
    ((files "files" "array" "Fichiers à recompiler (\"engine\", \"game\"…). Par défaut : ceux modifiés depuis leur dernier chargement."))
    "Recompile et recharge les fichiers de src/ modifiés depuis leur dernier
chargement, ainsi que ceux qui les suivent dans le système, en conservant *state*.
Tout est compilé avant de charger : en cas d'erreur ou d'avertissement, rien n'est
chargé. package, mcp-server, jobs et mcp-tools ne se rechargent pas à chaud
(scripts/dev.sh restart). Après un chargement réussi, la dernière erreur est
effacée et le jeu reprend."
  (sb-thread:with-mutex (*reload-lock*)
    (reload-sources (parse-file-list files))))

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

(defun simulate-key (key frames)
  "Tient KEY enfoncée pendant FRAMES images. Renvoie vrai si le jeu est en pause."
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
    paused))

(defun type-text (text)
  "Ajoute TEXT à la saisie simulée et attend que la console l'ait lu. La console
lit la saisie à chaque image, même en pause."
  (simulate-text text)
  (loop with deadline = (+ (get-internal-real-time) (* 10 internal-time-units-per-second))
        until (sb-concurrency:queue-empty-p *simulated-chars*)
        do (when (> (get-internal-real-time) deadline)
             (error "Le texte n'a pas été lu à temps : la fenêtre est-elle ralentie ?"))
           (sleep 0.02)))

(define-mcp-tool "game_input"
    ((key "key" "string" "Touche raylib à tenir enfoncée : right, left, up, down, space, a, enter…")
     (frames "frames" "integer" "Nombre d'images pendant lesquelles la touche reste enfoncée (1 par défaut).")
     (text "text" "string" "Texte saisi, pour la console du jeu : ` l'ouvre ou la ferme, un saut de ligne envoie le message."))
    "Simule une touche enfoncée pendant un nombre d'images, ou une saisie de texte,
sans focus de fenêtre. Pour une touche : si le jeu tourne, attend que ces images
soient passées puis renvoie l'état ; si le jeu est en pause, la touche reste en
attente (utiliser game_control step). Le texte est lu même en pause."
  (when (and key text)
    (error "Donner key ou text, pas les deux."))
  (cond
    (text
     (unless (stringp text) (error "text doit être une chaîne."))
     (type-text text)
     (state-json))
    (key
     (let ((key (parse-key key))
           (frames (or frames 1)))
       (unless (and (integerp frames) (<= 1 frames 600))
         (error "frames doit être un entier entre 1 et 600."))
       (format nil "~:[~;Jeu en pause : touche en attente, utiliser game_control step.~%~]~a"
               (simulate-key key frames) (state-json))))
    (t (error "Donner key ou text."))))

;;; game_console_reply

(define-mcp-tool "game_console_reply"
    ((chat-id "chat_id" "string" "Identifiant du message, repris de l'attribut chat_id de la balise <channel>." :required t)
     (text "text" "string" "Réponse à afficher, en une ou deux phrases." :required t))
    "Affiche une réponse dans la console du jeu, à un message reçu par le channel
game-console. Rester bref : le détail va dans le terminal."
  (unless (and (stringp text) (plusp (length (string-trim " " text))))
    (error "text doit être une chaîne non vide."))
  (let ((chat-id (if (stringp chat-id) chat-id (princ-to-string chat-id))))
    (call-in-main-thread (lambda () (console-add-reply chat-id text)))
    "Réponse affichée dans la console du jeu."))

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

;;; game_benchmark, game_profile (le travail est fait dans bench.lisp)

(define-mcp-tool "game_benchmark"
    ((variant "variant" "string" "pro (DrawTexturePro) ou rlgl (sommets envoyés par rlgl). pro par défaut." :enum ("pro" "rlgl"))
     (count "count" "integer" "Nombre de sprites (10000 par défaut).")
     (seconds "seconds" "number" "Durée de la mesure après une seconde de chauffe (5 par défaut)."))
    "Banc d'essai de rendu : dessine COUNT sprites pivotés tirés d'un atlas, sans
limite d'images par seconde, puis rend la main au jeu. Renvoie, par section de
l'image (jobs, console, update, draw, end_drawing, frame), le temps moyen, le 99e
centile et le maximum en ms, ainsi que les FPS et les octets alloués par image.
La fenêtre doit être visible : macOS ralentit une fenêtre cachée."
  (run-benchmark :variant (or variant "pro") :count (or count 10000) :seconds (or seconds 5)))

(define-mcp-tool "game_profile"
    ((seconds "seconds" "number" "Durée du profilage (5 par défaut).")
     (variant "variant" "string" "Si donné, profile le banc d'essai de rendu avec cette variante." :enum ("pro" "rlgl"))
     (count "count" "integer" "Sprites du banc d'essai (10000 par défaut).")
     (max "max" "integer" "Nombre de fonctions listées (30 par défaut)."))
    "Profile le thread principal avec sb-sprof (mode CPU) et renvoie les fonctions
les plus coûteuses. Avec variant, profile le banc d'essai de rendu plutôt que le jeu."
  (run-profile :seconds (or seconds 5) :variant variant :count (or count 10000)
               :max (or max 30)))

(define-mcp-tool "game_ping" ()
    "Vérifie que le jeu répond. Renvoie le numéro de l'image courante."
  (call-in-main-thread
   (lambda () (format nil "pong (image ~d)" *frame*))))
