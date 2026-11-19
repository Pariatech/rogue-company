(in-package #:rogue-company)

;;; Plan de l'IA dessiné par-dessus la bataille (touche I, *DRAW-AI-PLAN*) :
;;; l'axe du général, la place de chaque régiment qui a une tâche (fantôme de
;;; la couleur de sa tâche), le rayon des gardes, la tâche écrite au-dessus de
;;; chaque régiment, et la phase de la ligne. Pour régler l'IA, et voir
;;; pourquoi elle fait ce qu'elle fait.

(defvar *draw-ai-plan* nil "Vrai pour dessiner le plan de l'IA.")

(defparameter *ai-task-colors*
  '((:line 120 200 255 190) (:screen 120 230 160 190) (:guard 235 200 90 190)
    (:reserve 200 140 235 190) (:free 170 170 170 160))
  "Couleur de chaque tâche, en r g b a.")

(defparameter *ai-task-names*
  '((:line . "ligne") (:screen . "écran") (:guard . "garde") (:reserve . "réserve") (:free . "libre"))
  "Nom affiché de chaque tâche.")

(defparameter *ai-phase-names* '((:advance . "IA : la ligne avance") (:engaged . "IA : la ligne est engagée")))

(defun draw-world-circle (camera x y radius color &optional (segments 40))
  "Cercle de RADIUS mètres autour du point X, Y du monde."
  (loop for k from 0 below segments
        for a0 = (* 2 pi (/ k segments))
        for a1 = (* 2 pi (/ (1+ k) segments))
        do (multiple-value-bind (x0 y0) (world->screen camera (+ x (* radius (cos a0))) (+ y (* radius (sin a0))))
             (multiple-value-bind (x1 y1) (world->screen camera (+ x (* radius (cos a1))) (+ y (* radius (sin a1))))
               (draw-line x0 y0 x1 y1 color)))))

(defun draw-ai-plan (battle camera)
  (when *draw-ai-plan*
    (let ((soldiers (battle-soldiers battle))
          (size (text-size 16)))
      (dolist (commander (battle-commanders battle))
        (let ((plan (commander-plan commander)))
          (when plan
            ;; L'axe, de l'origine jusqu'au-delà de la ligne.
            (multiple-value-bind (ox oy) (plan-point plan 0f0 0f0)
              (multiple-value-bind (tx ty) (plan-point plan 0f0 (+ (plan-depth plan) 60f0))
                (multiple-value-bind (sx sy) (world->screen camera ox oy)
                  (multiple-value-bind (ex ey) (world->screen camera tx ty)
                    (draw-dashed-line sx sy ex ey '(200 200 200 120))
                    (draw-text-centered (cdr (assoc (plan-phase plan) *ai-phase-names*))
                                        sx (+ sy 8) size :lightgray)))))
            ;; Places, rayons, tâches.
            (loop for regiment across (battle-regiments battle)
                  when (and (= (regiment-side regiment) (commander-side commander))
                            (fighting-p regiment))
                    do (let* ((kind (task-kind plan regiment))
                              (color (cdr (assoc kind *ai-task-colors*))))
                         (multiple-value-bind (x y facing) (task-destination plan regiment)
                           (when x
                             (let ((*ghost-color* color))
                               (draw-ghost regiment camera x y facing))
                             (when (eq kind :guard)
                               (draw-world-circle camera x y
                                                  (if (eq (plan-phase plan) :engaged)
                                                      *ai-guard-engaged-radius*
                                                      *ai-guard-radius*)
                                                  color))))
                         (multiple-value-bind (cx cy) (regiment-centroid regiment soldiers)
                           (multiple-value-bind (sx sy) (world->screen camera cx cy)
                             (draw-text-centered (cdr (assoc kind *ai-task-names*))
                                                 sx (- sy (* 2 size)) size color)))))))))))
