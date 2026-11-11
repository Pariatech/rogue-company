(defpackage #:rogue-company/tests
  (:use #:cl #:fiveam)
  (:import-from #:rogue-company
                #:call-in-main-thread #:run-pending-jobs #:job-error
                #:job-error-message))

(in-package #:rogue-company/tests)

(def-suite jobs :description "File de tâches vers le thread principal.")
(in-suite jobs)

(defun call-from-tool-thread (function &key (wait 5) (pump t) (run-timeout 2))
  "Appelle CALL-IN-MAIN-THREAD depuis un autre thread, comme le ferait un outil
MCP, pendant que le thread courant joue le rôle de la boucle de jeu si PUMP."
  (let* ((result nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (setf result
                          (handler-case
                              (cons :ok (multiple-value-list
                                         (call-in-main-thread function :wait wait
                                                                     :run-timeout run-timeout)))
                            (job-error (e) (list :error (job-error-message e)))))))))
    (loop while (sb-thread:thread-alive-p thread)
          do (when pump (run-pending-jobs))
             (sleep 0.01))
    (sb-thread:join-thread thread)
    result))

(test success
  (let ((caller nil))
    (is (equal '(:ok 1 2)
               (call-from-tool-thread (lambda ()
                                        (setf caller sb-thread:*current-thread*)
                                        (values 1 2)))))
    (is (eq sb-thread:*current-thread* caller)
        "La tâche doit s'exécuter sur le thread qui traite la file.")))

(test error
  (let ((result (call-from-tool-thread (lambda () (error "boum")))))
    (is (eq :error (first result)))
    (is (search "boum" (second result)))))

(test run-timeout
  (let* ((start (get-internal-real-time))
         (result (call-from-tool-thread (lambda () (loop)) :run-timeout 0.2)))
    (is (eq :error (first result)))
    (is (search "interrompue" (second result)))
    (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 2))))

(test wait-timeout
  (let* ((ran nil)
         (result (call-from-tool-thread (lambda () (setf ran t)) :wait 0.2 :pump nil)))
    (is (eq :error (first result)))
    (is (search "pas traité" (second result)))
    (run-pending-jobs)
    (is (not ran) "Une tâche abandonnée ne doit pas être exécutée ensuite.")))
