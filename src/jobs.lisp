(in-package #:rogue-company)

;;; File de tâches vers le thread principal. Les threads des outils MCP n'appellent
;;; jamais raylib ni ne touchent *STATE* : ils déposent une fonction ici, et le
;;; thread principal l'exécute entre deux images avec RUN-PENDING-JOBS.

(defparameter *job-wait-timeout* 5
  "Secondes pendant lesquelles un appelant attend que sa tâche soit traitée.")
(defparameter *job-run-timeout* 2
  "Secondes accordées à une tâche sur le thread principal avant interruption.")

(defvar *jobs* (sb-concurrency:make-mailbox :name "jobs"))

(defstruct (job (:constructor make-job (function)))
  function
  (reply (sb-concurrency:make-mailbox :name "job-reply"))
  ;; Passe à T quand l'appelant abandonne, pour que la tâche ne soit pas exécutée.
  (cancelled nil))

(define-condition job-error (error)
  ((message :initarg :message :reader job-error-message)
   (backtrace :initarg :backtrace :initform nil :reader job-error-backtrace))
  (:report (lambda (c s)
             (write-string (job-error-message c) s)
             (when (job-error-backtrace c)
               (format s "~%~%Pile d'appels :~%~a" (job-error-backtrace c))))))

(defun call-in-main-thread (function &key (wait *job-wait-timeout*))
  "Exécute FUNCTION sur le thread principal et renvoie ses valeurs.
Signale JOB-ERROR si elle échoue, dépasse *JOB-RUN-TIMEOUT*, ou n'est pas
traitée en WAIT secondes."
  (when (sb-thread:main-thread-p)
    (return-from call-in-main-thread (funcall function)))
  (let ((job (make-job function)))
    (sb-concurrency:send-message *jobs* job)
    (multiple-value-bind (reply ok)
        (sb-concurrency:receive-message (job-reply job) :timeout wait)
      (cond
        ((not ok)
         (setf (job-cancelled job) t)
         (error 'job-error
                :message (format nil "Le jeu n'a pas traité la tâche en ~a s (fenêtre bloquée ou ralentie ?)."
                                 wait)))
        ((eq (car reply) :ok) (values-list (cdr reply)))
        (t (destructuring-bind (message backtrace) (cdr reply)
             (error 'job-error :message message :backtrace backtrace)))))))

(defun run-job (job)
  (let ((backtrace nil))
    (sb-concurrency:send-message
     (job-reply job)
     (handler-case
         (handler-bind ((serious-condition
                          (lambda (c)
                            (declare (ignore c))
                            (setf backtrace (with-output-to-string (s)
                                              (sb-debug:print-backtrace :count 25 :stream s))))))
           (sb-ext:with-timeout *job-run-timeout*
             (cons :ok (multiple-value-list (funcall (job-function job))))))
       (sb-ext:timeout ()
         (list :error (format nil "Tâche interrompue après ~a s." *job-run-timeout*) nil))
       (serious-condition (c)
         (list :error (handler-case (princ-to-string c)
                        (error () (format nil "~s" (type-of c))))
               backtrace))))))

(defun run-pending-jobs (&key (max 8))
  "Exécute au plus MAX tâches en attente. Appelé par le thread principal."
  (loop repeat max
        for job = (sb-concurrency:receive-message-no-hang *jobs*)
        while job
        unless (job-cancelled job)
          do (run-job job)))
