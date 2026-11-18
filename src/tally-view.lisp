(in-package #:rogue-company)

;;; Tableau des régiments, en haut à droite de la bataille : survivants sur
;;; effectif, et débandade. Quand les régiments vont par duels (rouge, bleu,
;;; rouge, bleu… comme dans les démonstrations de duels et le tournoi), une
;;; ligne par duel, les deux adversaires côte à côte. Les textes ne sont refaits
;;; que lorsqu'un régiment perd un homme ou se débande.

(defparameter *draw-tally* t "Dessiner le tableau des régiments (touche Tab).")
(defparameter *tally-text-size* 22 "Taille du texte du tableau, en pixels.")

(defclass tally ()
  ((key :initform (make-array 0 :element-type 'fixnum) :accessor tally-key
        :documentation "Par régiment, survivants × 2 + débandade, au dernier texte.")
   (lines :initform #() :accessor tally-lines
          :documentation "Par ligne, (texte couleur) ou (gauche couleur droite couleur)."))
  (:documentation "Textes du tableau des régiments, gardés d'une image à l'autre."))

(defun make-tally () (make-instance 'tally))

(defun tally-value (regiment)
  (+ (* 2 (regiment-alive regiment)) (if (regiment-routed-p regiment) 1 0)))

(defun tally-stale-p (tally regiments)
  (let ((key (tally-key tally)))
    (or (/= (length key) (length regiments))
        (loop for regiment across regiments
              for i fixnum from 0
              thereis (/= (aref key i) (tally-value regiment))))))

(defun regiment-tally-text (regiment)
  (format nil "~a ~d/~d~a" (regiment-name regiment)
          (regiment-alive regiment) (regiment-capacity regiment)
          (cond ((zerop (regiment-alive regiment)) "  anéanti")
                ((regiment-routed-p regiment) "  en déroute")
                (t ""))))

(defun duel-regiments-p (regiments)
  "Vrai si REGIMENTS vont par duels : rouge, bleu, rouge, bleu…"
  (and (plusp (length regiments)) (evenp (length regiments))
       (loop for regiment across regiments
             for i from 0
             always (= (regiment-side regiment) (mod i 2)))))

(defun rebuild-tally (tally regiments)
  (setf (tally-key tally) (map '(vector fixnum) #'tally-value regiments)
        (tally-lines tally)
        (if (duel-regiments-p regiments)
            (coerce (loop for i from 0 below (length regiments) by 2
                          for red = (aref regiments i)
                          for blue = (aref regiments (1+ i))
                          collect (list (regiment-tally-text red) (side-color 0)
                                        (regiment-tally-text blue) (side-color 1)))
                    'vector)
            (map 'vector (lambda (regiment)
                           (list (regiment-tally-text regiment)
                                 (side-color (regiment-side regiment))))
                 regiments))))

(defun draw-tally (tally battle)
  "Tableau des régiments de BATTLE, aligné à droite de la fenêtre. En duels, les
rouges alignés à droite, « contre », les bleus alignés à gauche."
  (when *draw-tally*
    (let ((regiments (battle-regiments battle)))
      (when (tally-stale-p tally regiments)
        (rebuild-tally tally regiments))
      (let* ((lines (tally-lines tally))
             (size (text-size *tally-text-size*))
             (gap (measure-text "  contre  " size))
             (line-height (+ size 6))
             (top (+ 20 (text-size 20)))
             (left-width (loop for line across lines maximize (measure-text (first line) size)))
             (right-width (loop for line across lines
                                maximize (if (third line) (measure-text (third line) size) 0)))
             (width (+ left-width (if (plusp right-width) (+ gap right-width) 0)))
             (x (- *window-width* 12 width)))
        (when (plusp (length lines))
          (draw-rectangle (- x 10) (- top 6) (+ width 20)
                          (+ 6 (* line-height (length lines))) '(0 0 0 170))
          (loop for line across lines
                for y from top by line-height
                do (destructuring-bind (left left-color &optional middle middle-color) line
                     (draw-text left (- (+ x left-width) (measure-text left size)) y size left-color)
                     (when middle
                       (draw-text "  contre  " (+ x left-width) y size :gray)
                       (draw-text middle (+ x left-width gap) y size middle-color)))))))))
