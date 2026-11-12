(in-package #:rogue-company)

;;; Fonctions de dessin raylib sans allocation.
;;;
;;; cl-raylib déclare Color comme une structure passée par valeur : CFFI passe
;;; alors par libffi, cherche le symbole C à chaque appel (environ 330 octets
;;; alloués) et convertit la couleur (COLORP appelle HASH-TABLE-KEYS). Un appel
;;; à raylib:clear-background alloue ainsi environ 820 octets.
;;;
;;; Une Color tient en 4 octets : sur arm64 (AAPCS64) comme sur x86-64 (System V),
;;; une structure de cette taille passe dans un registre entier, comme un uint32
;;; dont l'octet de poids faible est le rouge. Ces liaisons déclarent donc la
;;; couleur en :uint32 et passent par la table de liens de SBCL, comme les
;;; fonctions rlgl. DRAW-TEXT écrit le texte en UTF-8 dans un tampon C préalloué.
;;; Voir docs/mesures.md.

(defun color-value (color)
  "Couleur raylib en uint32 : mot-clé de cl-raylib (:black), liste (r g b [a])
ou entier déjà empaqueté."
  (etypecase color
    ((unsigned-byte 32) color)
    (keyword (or (gethash color raylib::+colors+)
                 (error "Couleur inconnue : ~s." color)))
    (list (destructuring-bind (r g b &optional (a 255)) color
            (logior r (ash g 8) (ash b 16) (ash a 24))))))

(cffi:defcfun ("ClearBackground" %clear-background) :void
  (color :uint32))

(cffi:defcfun ("DrawRectangle" %draw-rectangle) :void
  (x :int) (y :int) (width :int) (height :int) (color :uint32))

(cffi:defcfun ("DrawLine" %draw-line) :void
  (x1 :int) (y1 :int) (x2 :int) (y2 :int) (color :uint32))

(cffi:defcfun ("DrawText" %draw-text) :void
  (text :pointer) (x :int) (y :int) (size :int) (color :uint32))

(cffi:defcfun ("MeasureText" %measure-text) :int
  (text :pointer) (size :int))

(defvar *text-buffer-size* 4096
  "Taille de *TEXT-BUFFER*. DEFVAR : elle doit rester celle du tampon déjà
alloué, même après un rechargement.")

(defvar *text-buffer* nil
  "Tampon C réutilisé par DRAW-TEXT et MEASURE-TEXT, pour ne pas allouer de chaîne C à chaque
appel. Thread principal uniquement, comme tout dessin.")

(defun clear-background (color)
  (%clear-background (color-value color)))

(defun draw-rectangle (x y width height color)
  (%draw-rectangle x y width height (color-value color)))

(defun draw-line (x1 y1 x2 y2 color)
  (%draw-line x1 y1 x2 y2 (color-value color)))

(defun text-buffer ()
  (or *text-buffer*
      (setf *text-buffer* (cffi:foreign-alloc :uint8 :count *text-buffer-size*))))

(defun encode-utf8-into (string pointer)
  "Écrit STRING en UTF-8 à POINTER, suivi d'un octet nul. POINTER doit avoir
au moins 4 × (LENGTH STRING) + 1 octets. N'alloue pour aucun type de chaîne,
contrairement à cffi:lisp-string-to-foreign sur une simple-base-string (celles
que renvoie FORMAT quand le texte est en ASCII)."
  (let ((i 0))
    (declare (type fixnum i))
    (flet ((put (octet)
             (setf (cffi:mem-aref pointer :uint8 i) octet)
             (incf i)))
      (loop for char across string
            for code = (char-code char)
            do (cond ((< code #x80) (put code))
                     ((< code #x800)
                      (put (logior #xC0 (ash code -6)))
                      (put (logior #x80 (logand code #x3F))))
                     ((< code #x10000)
                      (put (logior #xE0 (ash code -12)))
                      (put (logior #x80 (logand (ash code -6) #x3F)))
                      (put (logior #x80 (logand code #x3F))))
                     (t
                      (put (logior #xF0 (ash code -18)))
                      (put (logior #x80 (logand (ash code -12) #x3F)))
                      (put (logior #x80 (logand (ash code -6) #x3F)))
                      (put (logior #x80 (logand code #x3F))))))
      (put 0))
    pointer))

(defmacro with-c-text ((pointer text) &body body)
  "Exécute BODY avec POINTER sur TEXT en chaîne C UTF-8. Un caractère prend au
plus 4 octets : au-delà du tampon, une chaîne C temporaire (allocation, mais rare)."
  (let ((string (gensym "TEXT")))
    `(let ((,string ,text))
       (if (< (* 4 (length ,string)) *text-buffer-size*)
           (let ((,pointer (encode-utf8-into ,string (text-buffer))))
             ,@body)
           (cffi:with-foreign-string (,pointer ,string)
             ,@body)))))

(defun draw-text (text x y size color)
  (with-c-text (pointer text)
    (%draw-text pointer x y size (color-value color))))

(defun measure-text (text size)
  "Largeur de TEXT en pixels, avec la police par défaut."
  (with-c-text (pointer text)
    (%measure-text pointer size)))

(defun make-text-buffer (&optional (size 32))
  "Chaîne réutilisable pour WRITE-INTEGER-INTO."
  (make-array size :element-type 'character :fill-pointer 0))

(defun write-integer-into (integer buffer)
  "Remplace le contenu de BUFFER par l'écriture décimale de INTEGER (un fixnum),
sans allouer, et renvoie BUFFER."
  (declare (type fixnum integer))
  (setf (fill-pointer buffer) 0)
  (when (minusp integer)
    (vector-push #\- buffer)
    (setf integer (- integer)))
  (let ((start (fill-pointer buffer)))
    (loop do (multiple-value-bind (rest digit) (floor integer 10)
               (vector-push (code-char (+ 48 digit)) buffer)
               (setf integer rest))
          while (plusp integer))
    ;; Les chiffres sont venus à l'envers.
    (let ((end (1- (fill-pointer buffer))))
      (loop for i from start
            for j downfrom end
            while (< i j)
            do (rotatef (char buffer i) (char buffer j)))))
  buffer)

(defun draw-text-centered (text center-x y size color)
  (draw-text text (- center-x (floor (measure-text text size) 2)) y size color))
