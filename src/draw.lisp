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
;;; fonctions rlgl. DRAW-TEXT écrit le texte dans un tampon C préalloué.
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

(defvar *text-buffer-size* 4096
  "Taille de *TEXT-BUFFER*. DEFVAR : elle doit rester celle du tampon déjà
alloué, même après un rechargement.")

(defvar *text-buffer* nil
  "Tampon C réutilisé par DRAW-TEXT, pour ne pas allouer de chaîne C à chaque
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

(defun draw-text (text x y size color)
  ;; Un caractère prend au plus 4 octets en UTF-8 ; au-delà du tampon, une
  ;; chaîne C temporaire (allocation, mais rare).
  (if (< (* 4 (length text)) *text-buffer-size*)
      (let ((buffer (text-buffer)))
        (cffi:lisp-string-to-foreign text buffer *text-buffer-size*)
        (%draw-text buffer x y size (color-value color)))
      (cffi:with-foreign-string (pointer text)
        (%draw-text pointer x y size (color-value color)))))
