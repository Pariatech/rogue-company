(in-package #:rogue-company)

;;; Console du jeu reliée à Claude Code. La touche ` l'ouvre et met le jeu en
;;; pause. Un message envoyé part dans *CONSOLE-OUTBOX*, que le pont bridge/
;;; relève par GET /console/outbox et transmet à la session Claude Code ; Claude
;;; répond avec l'outil game_console_reply.
;;;
;;; Interface vers l'extérieur : CONSOLE-TAKE-OUTBOX (n'importe quel thread) et
;;; CONSOLE-ADD-REPLY (thread principal).

(defparameter *console-toggle-char* #\`)
(defparameter *console-font-size* 20)
(defparameter *console-height* 230)
(defparameter *console-history-limit* 100)
(defparameter *console-input-max-lines* 4
  "Lignes visibles de la saisie ; au-delà, seules les dernières s'affichent.")
(defparameter *console-wheel-lines* 3
  "Lignes défilées par cran de molette.")
(defparameter *console-banner-frames* 480
  "Images pendant lesquelles une réponse reste affichée quand la console est fermée.")

(defclass console-state ()
  ((open-p :initform nil :accessor console-open-p)
   (input :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          :accessor console-input)
   (history :initform '() :accessor console-history
            :documentation "Plists (:who :me|:claude|:info :text :id), la plus récente en tête.")
   (scroll :initform 0 :accessor console-scroll
           :documentation "Lignes d'historique masquées sous la vue, 0 = en bas.")
   (next-id :initform 1 :accessor console-next-id)
   (pending :initform '() :accessor console-pending
            :documentation "Identifiants des messages envoyés sans réponse.")
   (delivered :initform '() :accessor console-delivered
              :documentation "Parmi PENDING, les messages relevés par le pont : Claude les a reçus.")
   (banner :initform nil :accessor console-banner
           :documentation "Dernière réponse reçue console fermée : (texte . image de fin).")))

(defvar *console* (make-instance 'console-state))

(defvar *console-outbox* (sb-concurrency:make-mailbox :name "console-outbox")
  "Messages à transmettre à Claude Code : tables JSON (id, text, meta).")

(defvar *console-delivered* (sb-concurrency:make-mailbox :name "console-delivered")
  "Identifiants des messages relevés par le pont, à reporter sur le thread principal.")

;;; Interface

(defun console-take-outbox ()
  "Retire et renvoie les messages en attente. Sûr depuis n'importe quel thread.
Les messages retirés sont considérés comme reçus par Claude."
  (let ((messages (sb-concurrency:receive-pending-messages *console-outbox*)))
    (dolist (message messages messages)
      (sb-concurrency:send-message *console-delivered* (gethash "id" message)))))

(defun console-add-reply (id text)
  "Affiche la réponse TEXT au message ID. À appeler sur le thread principal."
  (let ((console *console*))
    (setf (console-pending console) (remove id (console-pending console) :test #'equal)
          (console-delivered console) (remove id (console-delivered console) :test #'equal))
    (console-push console :claude text id)
    (unless (console-open-p console)
      (setf (console-banner console) (cons text (+ *frame* *console-banner-frames*))))
    t))

(defun console-snapshot ()
  "Résumé de la console pour game_state. À appeler sur le thread principal."
  (let* ((console *console*)
         (recent (subseq (console-history console)
                         0 (min 6 (length (console-history console))))))
    (ht "open" (if (console-open-p console) t 'yason:false)
        "input" (coerce (console-input console) 'simple-string)
        "pending" (coerce (console-pending console) 'vector)
        "delivered" (coerce (console-delivered console) 'vector)
        "history" (coerce (loop for entry in (reverse recent)
                                collect (ht "who" (string-downcase (getf entry :who))
                                            "text" (getf entry :text)
                                            "id" (or (getf entry :id) :null)))
                          'vector))))

;;; État

(defun console-push (console who text &optional id)
  (push (list :who who :text text :id id) (console-history console))
  (let ((tail (nthcdr (1- *console-history-limit*) (console-history console))))
    (when tail (setf (cdr tail) nil))))

(defun console-open (console)
  (setf (console-open-p console) t
        (console-banner console) nil)
  (unless *paused* (setf *paused* :console)))

(defun console-close (console)
  (setf (console-open-p console) nil)
  (when (eq *paused* :console) (setf *paused* nil)))

(defun mouse-context ()
  (values (raylib:get-mouse-x) (raylib:get-mouse-y)))

(defun console-send (console)
  ;; STRING-TRIM peut renvoyer la chaîne d'origine : la copier avant de la vider.
  (let ((text (coerce (string-trim '(#\Space #\Tab) (console-input console)) 'simple-string)))
    (setf (fill-pointer (console-input console)) 0
          (console-scroll console) 0)
    (when (plusp (length text))
      (let ((id (format nil "~d" (console-next-id console))))
        (incf (console-next-id console))
        (console-push console :me text id)
        (push id (console-pending console))
        (multiple-value-bind (mouse-x mouse-y) (mouse-context)
          ;; Clés de meta : lettres, chiffres et _ uniquement, valeurs en chaînes.
          (sb-concurrency:send-message
           *console-outbox*
           (ht "id" id
               "text" text
               "meta" (ht "chat_id" id
                          "frame" (princ-to-string *frame*)
                          "paused" (string-downcase (princ-to-string (or *paused* "no")))
                          "mouse_x" (princ-to-string mouse-x)
                          "mouse_y" (princ-to-string mouse-y)))))))))

(defun console-update ()
  "Lit la saisie de la console. Appelé à chaque image, même en pause."
  (let ((console *console*))
    (dolist (id (sb-concurrency:receive-pending-messages *console-delivered*))
      (when (member id (console-pending console) :test #'equal)
        (pushnew id (console-delivered console) :test #'equal)))
    (loop for char = (next-char)
          while char
          do (cond ((char= char *console-toggle-char*)
                    (if (console-open-p console) (console-close console) (console-open console)))
                   ((not (console-open-p console)))
                   ((char= char #\Newline) (console-send console))
                   ((char= char #\Backspace)
                    (when (plusp (fill-pointer (console-input console)))
                      (decf (fill-pointer (console-input console)))))
                   ((graphic-char-p char) (vector-push-extend char (console-input console)))))
    (when (console-open-p console)
      (when (key-pressed-p :key-enter) (console-send console))
      (when (and (key-pressed-p :key-backspace :repeat t)
                 (plusp (fill-pointer (console-input console))))
        (decf (fill-pointer (console-input console))))
      (console-scroll-input console)
      ;; Le jeu est en pause et ADVANCE-INPUT n'est pas appelé : sans cela, une
      ;; touche simulée resterait tout juste enfoncée à chaque image.
      (mark-simulated-keys-seen))
    (let ((banner (console-banner console)))
      (when (and banner (> *frame* (cdr banner)))
        (setf (console-banner console) nil)))))

;;; Défilement : flèches haut et bas (une ligne), Page précédente et Page
;;; suivante (un écran), Début et Fin, molette de la souris.

(defun console-input-lines (console width size)
  "Lignes de la saisie, invite et curseur compris, limitées aux
*CONSOLE-INPUT-MAX-LINES* dernières. Le curseur est le dernier caractère."
  (let ((lines (wrap-input (format nil "> ~a_" (console-input console)) width size)))
    (last lines *console-input-max-lines*)))

(defun console-geometry (console)
  "Valeurs : haut de la console, ordonnée de la saisie, lignes d'historique
visibles, hauteur de ligne, largeur de texte, lignes de la saisie."
  (let* ((size *console-font-size*)
         (line-height (+ size 4))
         (width (- *window-width* 20))
         (input-lines (console-input-lines console width size))
         (top (- *window-height* *console-height*))
         (input-y (- *window-height* (* line-height (length input-lines)) 6)))
    (values top input-y (max 1 (floor (- input-y top 10) line-height))
            line-height width input-lines)))

(defun console-max-scroll (console)
  (multiple-value-bind (top input-y rows line-height width) (console-geometry console)
    (declare (ignore top input-y line-height))
    (max 0 (- (length (console-lines console width *console-font-size*)) rows))))

(defun console-scroll-by (console delta)
  "Défile de DELTA lignes vers le haut (négatif : vers le bas), dans les bornes."
  (setf (console-scroll console)
        (max 0 (min (console-max-scroll console) (+ (console-scroll console) delta)))))

(defun console-scroll-input (console)
  (let* ((page (max 1 (1- (nth-value 2 (console-geometry console)))))
         (delta (+ (if (key-pressed-p :key-up :repeat t) 1 0)
                   (if (key-pressed-p :key-down :repeat t) -1 0)
                   (if (key-pressed-p :key-page-up :repeat t) page 0)
                   (if (key-pressed-p :key-page-down :repeat t) (- page) 0)
                   (round (* *console-wheel-lines* (raylib:get-mouse-wheel-move))))))
    (cond ((key-pressed-p :key-home) (setf (console-scroll console) (console-max-scroll console)))
          ((key-pressed-p :key-end) (setf (console-scroll console) 0))
          ((/= delta 0) (console-scroll-by console delta)))))

;;; Dessin

(defun console-wait-status (console)
  "Texte d'attente : message pas encore relevé par le pont, ou reçu par Claude
qui prépare sa réponse. NIL si rien n'est en attente."
  (let ((dots (subseq "..." 0 (1+ (mod (floor *frame* 20) 3)))))
    (cond ((set-difference (console-pending console) (console-delivered console)
                           :test #'equal)
           (values (format nil "Envoi à Claude~a" dots) :gray))
          ((console-pending console)
           (values (format nil "Claude a reçu ton message et prépare une réponse~a" dots)
                   '(80 120 160 255))))))

(defun wrap-text (text width size)
  "Découpe TEXT en lignes d'au plus WIDTH pixels, en coupant aux espaces."
  (let ((lines '()))
    (dolist (paragraph (uiop:split-string text :separator '(#\Newline)) (nreverse lines))
      (let ((line ""))
        (dolist (word (remove "" (uiop:split-string paragraph :separator '(#\Space))
                              :test #'string=))
          (let ((candidate (if (string= line "") word (concatenate 'string line " " word))))
            (if (or (string= line "") (<= (measure-text candidate size) width))
                (setf line candidate)
                (progn (push line lines) (setf line word)))))
        (push line lines)))))

(defun wrap-input (text width size)
  "Découpe TEXT en lignes d'au plus WIDTH pixels sans perdre de caractère :
coupe après le dernier espace qui tient, sinon au milieu du mot."
  (let ((lines '()) (start 0) (len (length text)))
    (loop
      (when (<= (measure-text (subseq text start) size) width)
        (push (subseq text start) lines)
        (return (nreverse lines)))
      ;; Plus long préfixe qui tient, au moins un caractère.
      (let* ((end (loop with fit = (1+ start)
                        for e from (1+ start) to len
                        while (<= (measure-text (subseq text start e) size) width)
                        do (setf fit e)
                        finally (return fit)))
             (space (position #\Space text :start start :end end :from-end t))
             (cut (if (and space (> space start)) (1+ space) end)))
        (push (subseq text start cut) lines)
        (setf start cut)))))

(defun console-lines (console width size)
  "Lignes de l'historique à afficher, de la plus ancienne à la plus récente :
liste de (couleur . texte)."
  (append
   (loop for entry in (reverse (console-history console))
         for who = (getf entry :who)
         for prefix = (ecase who (:me "Toi : ") (:claude "Claude : ") (:info ""))
         for color = (ecase who (:me :lightgray) (:claude :skyblue) (:info :gray))
         append (mapcar (lambda (line) (cons color line))
                        (wrap-text (concatenate 'string prefix (getf entry :text)) width size)))
   ;; Ligne d'attente sous le dernier message, tant qu'une réponse manque.
   (multiple-value-bind (status color) (console-wait-status console)
     (when status (list (cons color status))))))

(defun console-draw ()
  "Dessine la console ouverte, ou la dernière réponse si elle est fermée."
  (let ((console *console*)
        (size *console-font-size*))
    ;; Console fermée sans réponse affichée : rien à calculer, rien à allouer.
    (cond
      ((console-open-p console)
       (multiple-value-bind (top input-y rows line-height width input-lines)
           (console-geometry console)
       (let* ((all (console-lines console width size))
              (total (length all))
              ;; L'historique a pu raccourcir depuis le dernier défilement.
              (scroll (min (console-scroll console) (max 0 (- total rows))))
              (start (max 0 (- total rows scroll)))
              (lines (subseq all start (min total (+ start rows)))))
         (draw-rectangle 0 top *window-width* *console-height* '(10 10 20 255))
         (draw-line 0 top *window-width* top :darkgray)
         (loop for (color . line) in lines
               for y from (+ top 8) by line-height
               do (draw-text line 10 y size color))
         (when (> total rows)
           ;; Barre de défilement à droite de l'historique.
           (let* ((track-top (+ top 8))
                  (track (* rows line-height))
                  (thumb (max 12 (floor (* track rows) total)))
                  (thumb-y (+ track-top (floor (* (- track thumb) start)
                                                (max 1 (- total rows))))))
             (draw-rectangle (- *window-width* 6) track-top 3 track '(40 40 55 255))
             (draw-rectangle (- *window-width* 6) thumb-y 3 thumb :gray)))
         (let* ((status (if (plusp scroll)
                            (format nil "[+~d lignes plus bas, Fin pour revenir]" scroll)
                            ""))
                (blink (< (mod *frame* 60) 30)))
           (loop for (line . more) on input-lines
                 for y from input-y by line-height
                 ;; Le curseur clignote : retirer le _ final une demi-seconde sur deux.
                 do (draw-text (if (or more blink) line (subseq line 0 (1- (length line))))
                                      10 y size :white))
           (draw-text status (- *window-width* (measure-text status 14) 10)
                             (+ top 4) 14 :gray)))))
      ((console-banner console)
       (let* ((line-height (+ size 4))
              (width (- *window-width* 20))
              (lines (wrap-text (concatenate 'string "Claude : " (car (console-banner console)))
                                width size))
              (lines (subseq lines 0 (min 3 (length lines))))
              (height (+ 12 (* line-height (length lines))))
              (top (- *window-height* height)))
         (draw-rectangle 0 top *window-width* height '(10 10 20 255))
         (loop for line in lines
               for y from (+ top 6) by line-height
               do (draw-text line 10 y size :skyblue)))))))
