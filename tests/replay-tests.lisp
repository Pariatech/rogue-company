(in-package #:rogue-company/tests)

;;; Replay : ordres du joueur enregistrés, bataille revue à l'identique.

(def-suite replay :description "Enregistrement des ordres et replay.")
(in-suite replay)

(defun played-battle ()
  "Une bataille contre l'IA de 30 s où le joueur donne quelques ordres : marche
avec orientation, attaque, exercice, posture, halte. Renvoie la bataille."
  (let* ((b (rogue-company::make-ai-battle))
         (rs (regiments b)))
    (flet ((order (spec) (rogue-company::player-order b spec)))
      (run-ticks b 60)
      (order (list :move '(0 1 2) -40f0 10f0 0.2f0))
      (order (list :drill 1 :square))
      (run-ticks b 150)
      (order (list :attack '(3 4) (rogue-company::regiment-index (nth 12 rs))))
      (order (list :stance 8 :hold))
      (order (list :move '(8) -60f0 -120f0 nil))
      (run-ticks b 300)
      (order (list :halt '(0)))
      (order (list :charge '(0)))
      (run-ticks b 390))
    b))

(defun replayed (replay)
  "La bataille de REPLAY revue jusqu'à son tick d'enregistrement, et le statut."
  (let ((b (rogue-company::replay-start replay))
        (cursor 0) (status nil))
    (loop until status
          do (multiple-value-setq (cursor status) (rogue-company::replay-tick b replay cursor)))
    (values b status)))

(test orders-are-logged-with-their-tick
  (let* ((b (played-battle))
         (log (rogue-company::battle-order-log b)))
    (is (= 7 (length log)))
    (is (equal '(60 :move (0 1 2) -40f0 10f0 0.2f0) (aref log 0)))
    (is (= 510 (car (aref log 6))))
    (is (eq :square (rogue-company::regiment-drill (nth 1 (regiments b)))) "exécutés aussi")))

(test replay-matches-the-battle
  "Revue d'après sa recette et ses ordres, la bataille retombe sur la même
empreinte au même tick."
  (let* ((b (played-battle))
         (replay (rogue-company::battle-replay b)))
    (multiple-value-bind (again status) (replayed replay)
      (is (eq :match status))
      (is (string= (rogue-company::battle-digest b) (rogue-company::battle-digest again))))))

(test replay-survives-the-file
  (let ((b (played-battle)))
    (uiop:with-temporary-file (:pathname path :type "sexp")
      (rogue-company::save-replay b path)
      (let ((replay (rogue-company::load-replay path)))
        (is (= 900 (rogue-company::replay-end-tick replay)))
        (is (equalp (rogue-company::battle-order-log b) (rogue-company::replay-orders replay)))
        (is (eq :match (nth-value 1 (replayed replay))))))))

(test changed-simulation-is-detected
  "Une empreinte qui ne correspond plus (la simulation a changé depuis
l'enregistrement) se voit au tick de l'enregistrement."
  (let* ((b (played-battle))
         (replay (rogue-company::battle-replay b)))
    (setf (slot-value replay 'rogue-company::digest) "0000")
    (is (eq :desync (nth-value 1 (replayed replay))))))

(test foreign-files-are-refused
  "Un fichier dont la recette appelle autre chose qu'une bataille, ou qui
contient un ordre inconnu, est refusé ; pas de fichier, pas de replay."
  (uiop:with-temporary-file (:pathname path :type "sexp")
    (flet ((write-data (data)
             (with-open-file (out path :direction :output :if-exists :supersede)
               (with-standard-io-syntax
                 (let ((*package* (find-package '#:rogue-company)))
                   (print data out))))))
      (write-data '(:version 1 :recipe (rogue-company::delete-everything) :end-tick 0 :digest "" :orders ()))
      (signals error (rogue-company::load-replay path))
      (write-data '(:version 1 :recipe (rogue-company::make-ai-battle) :end-tick 0 :digest ""
                    :orders ((0 :shell "rm"))))
      (signals error (rogue-company::load-replay path))))
  (is (null (rogue-company::load-replay #p"/nonexistent/replay.sexp"))))
