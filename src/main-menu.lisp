(in-package #:rogue-company)

;;; Menu principal : une liste d'options, chacune ouvrant une scène. Flèches ou
;;; souris pour choisir, Entrée, Espace ou clic pour valider.

(defparameter *menu-title* "Rogue Company")
(defparameter *menu-title-size* 50)
(defparameter *menu-option-size* 30)
(defparameter *menu-options-top* 300)
(defparameter *menu-option-spacing* 50)

(defclass main-menu (scene)
  ((options :initform '(("Start Battle" . battle-scene)) :reader menu-options
            :documentation "Liste de (libellé . scène à ouvrir).")
   (selected :initform 0 :accessor menu-selected)))

(defun menu-option-y (index)
  (+ *menu-options-top* (* index *menu-option-spacing*)))

(defun menu-option-at (menu x y)
  "Indice de l'option sous le point (X, Y), ou NIL."
  (loop for (label) in (menu-options menu)
        for index from 0
        for width = (measure-text label *menu-option-size*)
        for left = (- (floor *window-width* 2) (floor width 2) 20)
        for top = (- (menu-option-y index) 10)
        when (and (<= left x (+ left width 40))
                  (<= top y (+ top *menu-option-size* 20)))
          return index))

(defun menu-activate (menu)
  (change-scene *state* (cdr (nth (menu-selected menu) (menu-options menu)))))

(defmethod scene-update ((menu main-menu) dt)
  (declare (ignore dt))
  (let ((count (length (menu-options menu)))
        (hovered (menu-option-at menu (raylib:get-mouse-x) (raylib:get-mouse-y))))
    (when (key-pressed-p :key-down :repeat t)
      (setf (menu-selected menu) (mod (1+ (menu-selected menu)) count)))
    (when (key-pressed-p :key-up :repeat t)
      (setf (menu-selected menu) (mod (1- (menu-selected menu)) count)))
    (when hovered
      (setf (menu-selected menu) hovered))
    (when (or (key-pressed-p :key-enter)
              (key-pressed-p :key-space)
              (and hovered (raylib:is-mouse-button-pressed :mouse-button-left)))
      (menu-activate menu))))

(defmethod scene-draw ((menu main-menu))
  (clear-background :black)
  (let ((center (floor *window-width* 2)))
    (draw-text-centered *menu-title* center 160 *menu-title-size* :white)
    (loop for (label) in (menu-options menu)
          for index from 0
          for y = (menu-option-y index)
          do (if (= index (menu-selected menu))
                 (let ((width (measure-text label *menu-option-size*)))
                   (draw-text ">" (- center (floor width 2) 30) y *menu-option-size* :gold)
                   (draw-text-centered label center y *menu-option-size* :gold))
                 (draw-text-centered label center y *menu-option-size* :gray)))
    (draw-text-centered "Flèches ou souris pour choisir, Entrée pour valider"
                        center (- *window-height* 40) 16 :darkgray)))
