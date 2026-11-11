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
(defparameter *console-banner-frames* 480
  "Images pendant lesquelles une réponse reste affichée quand la console est fermée.")

(defclass console-state ()
  ((open-p :initform nil :accessor console-open-p)
   (input :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          :accessor console-input)
   (history :initform '() :accessor console-history
            :documentation "Plists (:who :me|:claude|:info :text :id), la plus récente en tête.")
   (next-id :initform 1 :accessor console-next-id)
   (pending :initform '() :accessor console-pending
            :documentation "Identifiants des messages envoyés sans réponse.")
   (banner :initform nil :accessor console-banner
           :documentation "Dernière réponse reçue console fermée : (texte . image de fin).")))

(defvar *console* (make-instance 'console-state))

(defvar *console-outbox* (sb-concurrency:make-mailbox :name "console-outbox")
  "Messages à transmettre à Claude Code : tables JSON (id, text, meta).")

;;; Interface

(defun console-take-outbox ()
  "Retire et renvoie les messages en attente. Sûr depuis n'importe quel thread."
  (sb-concurrency:receive-pending-messages *console-outbox*))

(defun console-add-reply (id text)
  "Affiche la réponse TEXT au message ID. À appeler sur le thread principal."
  (let ((console *console*))
    (setf (console-pending console) (remove id (console-pending console) :test #'equal))
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
  ;; ESC fermerait la fenêtre pendant la saisie.
  (raylib:set-exit-key :key-null)
  (unless *paused* (setf *paused* :console)))

(defun console-close (console)
  (setf (console-open-p console) nil)
  (raylib:set-exit-key :key-escape)
  (when (eq *paused* :console) (setf *paused* nil)))

(defun mouse-context ()
  (let ((mouse (raylib:get-mouse-position)))
    (values (round (3d-vectors:vx mouse)) (round (3d-vectors:vy mouse)))))

(defun console-send (console)
  ;; STRING-TRIM peut renvoyer la chaîne d'origine : la copier avant de la vider.
  (let ((text (coerce (string-trim '(#\Space #\Tab) (console-input console)) 'simple-string)))
    (setf (fill-pointer (console-input console)) 0)
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
        (decf (fill-pointer (console-input console)))))
    (let ((banner (console-banner console)))
      (when (and banner (> *frame* (cdr banner)))
        (setf (console-banner console) nil)))))

;;; Dessin

(defun wrap-text (text width size)
  "Découpe TEXT en lignes d'au plus WIDTH pixels, en coupant aux espaces."
  (let ((lines '()))
    (dolist (paragraph (uiop:split-string text :separator '(#\Newline)) (nreverse lines))
      (let ((line ""))
        (dolist (word (remove "" (uiop:split-string paragraph :separator '(#\Space))
                              :test #'string=))
          (let ((candidate (if (string= line "") word (concatenate 'string line " " word))))
            (if (or (string= line "") (<= (raylib:measure-text candidate size) width))
                (setf line candidate)
                (progn (push line lines) (setf line word)))))
        (push line lines)))))

(defun console-lines (console width size)
  "Lignes de l'historique à afficher, de la plus ancienne à la plus récente :
liste de (couleur . texte)."
  (loop for entry in (reverse (console-history console))
        for who = (getf entry :who)
        for prefix = (ecase who (:me "Toi : ") (:claude "Claude : ") (:info ""))
        for color = (ecase who (:me :lightgray) (:claude :skyblue) (:info :gray))
        append (mapcar (lambda (line) (cons color line))
                       (wrap-text (concatenate 'string prefix (getf entry :text)) width size))))

(defun console-draw ()
  "Dessine la console ouverte, ou la dernière réponse si elle est fermée."
  (let* ((console *console*)
         (size *console-font-size*)
         (line-height (+ size 4))
         (width (- *window-width* 20)))
    (cond
      ((console-open-p console)
       (let* ((top (- *window-height* *console-height*))
              (input-y (- *window-height* line-height 6))
              (rows (floor (- input-y top 10) line-height))
              (lines (console-lines console width size))
              (lines (nthcdr (max 0 (- (length lines) rows)) lines)))
         (raylib:draw-rectangle 0 top *window-width* *console-height* '(10 10 20 255))
         (raylib:draw-line 0 top *window-width* top :darkgray)
         (loop for (color . line) in lines
               for y from (+ top 8) by line-height
               do (raylib:draw-text line 10 y size color))
         (let* ((status (if (console-pending console) "  [en attente de Claude]" ""))
                (cursor (if (< (mod *frame* 60) 30) "_" " "))
                (input (format nil "> ~a~a" (console-input console) cursor))
                (shown (if (> (raylib:measure-text input size) width)
                           (subseq input (max 0 (- (length input) 60)))
                           input)))
           (raylib:draw-text shown 10 input-y size :white)
           (raylib:draw-text status (- *window-width* (raylib:measure-text status 14) 10)
                             (+ top 4) 14 :gray))))
      ((console-banner console)
       (let* ((lines (wrap-text (concatenate 'string "Claude : " (car (console-banner console)))
                                width size))
              (lines (subseq lines 0 (min 3 (length lines))))
              (height (+ 12 (* line-height (length lines))))
              (top (- *window-height* height)))
         (raylib:draw-rectangle 0 top *window-width* height '(10 10 20 255))
         (loop for line in lines
               for y from (+ top 6) by line-height
               do (raylib:draw-text line 10 y size :skyblue)))))))
