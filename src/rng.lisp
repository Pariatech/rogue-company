(in-package #:rogue-company)

;;; Générateur aléatoire à graine : PCG32 (PCG-XSH-RR, état de 64 bits, sortie
;;; de 32 bits), de Melissa O'Neill, https://www.pcg-random.org.
;;;
;;; Tout tirage de la simulation passe par ici, jamais par RANDOM : la suite ne
;;; dépend que de la graine, d'une version de SBCL à l'autre, ce qu'exigent le
;;; rejeu d'une bataille et un futur multijoueur en lockstep. Aucun appel
;;; n'alloue.
;;;
;;; Un générateur est un tableau de deux mots de 64 bits, l'état et l'incrément,
;;; pour que l'arithmétique reste sur des entiers non emballés.

(deftype rng () '(simple-array (unsigned-byte 64) (2)))

(defconstant +pcg-multiplier+ 6364136223846793005)

(defmacro u64 (form) `(logand #xFFFFFFFFFFFFFFFF ,form))
(defmacro u32 (form) `(logand #xFFFFFFFF ,form))

(defun rng-next (rng)
  "Prochain entier de 32 bits, et avance RNG."
  (declare (optimize (speed 3) (safety 1))
           (type rng rng))
  (let* ((old (aref rng 0))
         (xorshifted (u32 (ash (logxor (ash old -18) old) -27)))
         (rotation (ash old -59)))
    (declare (type (unsigned-byte 64) old)
             (type (unsigned-byte 32) xorshifted)
             (type (integer 0 31) rotation))
    (setf (aref rng 0) (u64 (+ (u64 (* old +pcg-multiplier+)) (aref rng 1))))
    (u32 (logior (ash xorshifted (- rotation))
                 (ash xorshifted (logand (- rotation) 31))))))

(defun make-rng (seed &optional (stream 54))
  "Générateur initialisé par SEED. STREAM choisit l'une des 2^63 suites
indépendantes possibles pour une même graine (pcg32_srandom_r)."
  (let ((rng (make-array 2 :element-type '(unsigned-byte 64) :initial-element 0)))
    (setf (aref rng 1) (u64 (logior (ash stream 1) 1)))
    (rng-next rng)
    (setf (aref rng 0) (u64 (+ (aref rng 0) seed)))
    (rng-next rng)
    rng))

(defun copy-rng (rng)
  "Copie indépendante de RNG, qui produira la même suite."
  (copy-seq rng))

(defun rng-below (rng n)
  "Entier uniforme dans [0, N[, sans biais. N entre 1 et 2^32."
  (declare (optimize (speed 3) (safety 1))
           (type rng rng)
           (type (integer 1 #.(expt 2 32)) n))
  ;; Rejette le bas de l'intervalle pour que 2^32 - seuil soit un multiple de N.
  (let ((threshold (mod (- #.(expt 2 32) n) n)))
    (loop
      (let ((r (rng-next rng)))
        (declare (type (unsigned-byte 32) r))
        (when (>= r threshold)
          (return (mod r n)))))))

(defun rng-float (rng)
  "Flottant uniforme dans [0, 1[ : 24 bits de mantisse, tous représentables."
  (declare (optimize (speed 3) (safety 1))
           (type rng rng))
  (* (float (ash (the (unsigned-byte 32) (rng-next rng)) -8) 1f0)
     #.(scale-float 1f0 -24)))

(defun rng-range (rng low high)
  "Flottant uniforme dans [LOW, HIGH[."
  (declare (optimize (speed 3) (safety 1))
           (type rng rng)
           (type single-float low high))
  (+ low (* (- high low) (the single-float (rng-float rng)))))
