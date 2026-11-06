(in-package #:rogue-company)

;;; Serveur MCP minimal (Streamable HTTP, réponses JSON uniquement).
;;; Solution de repli du plan : mcp-lisp écoute sur 0.0.0.0 sans
;;; authentification. Ce serveur n'écoute que sur 127.0.0.1, exige un
;;; jeton Bearer et refuse les en-têtes Origin et Host non locaux.

(defparameter *mcp-port* 7777)
(defparameter *mcp-address* "127.0.0.1")
(defparameter *mcp-path* "/mcp")
(defparameter *mcp-token-variable* "LISP_GAME_MCP_TOKEN")
(defparameter *mcp-protocol-version* "2025-06-18")

(defvar *mcp-acceptor* nil)
(defvar *mcp-token* nil)
(defvar *mcp-tools* (make-hash-table :test #'equal)
  "Nom de l'outil -> plist (:description :schema :handler).")

;;; JSON

(defun ht (&rest keys-and-values)
  "Construit une table de hachage JSON à partir de clés et de valeurs alternées."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key table) value))
    table))

(defun to-json (object)
  (with-output-to-string (stream)
    (yason:encode object stream)))

;;; Outils

(defmacro define-mcp-tool (name (&rest params) description &body body)
  "Déclare un outil MCP. PARAMS est une liste de
(variable \"nom-json\" type description &key required enum).
BODY renvoie une chaîne, ou signale une erreur."
  (let ((args (gensym "ARGS")))
    `(setf (gethash ,name *mcp-tools*)
           (list :description ,description
                 :schema (tool-schema ',params)
                 :handler (lambda (,args)
                            (declare (ignorable ,args))
                            (let ,(loop for (var json-name) in params
                                        collect `(,var (gethash ,json-name ,args)))
                              ,@body))))))

(defun tool-schema (params)
  (let ((properties (ht))
        (required '()))
    (loop for (nil json-name type description . options) in params
          do (setf (gethash json-name properties)
                   (apply #'ht "type" type "description" description
                          (when (getf options :enum)
                            (list "enum" (getf options :enum)))))
             (when (getf options :required)
               (push json-name required)))
    (apply #'ht "type" "object" "properties" properties
           (when required (list "required" (nreverse required))))))

(defun tool-descriptions ()
  (loop for name being the hash-keys of *mcp-tools* using (hash-value tool)
        collect (ht "name" name
                    "description" (getf tool :description)
                    "inputSchema" (getf tool :schema))))

(defun call-tool (name arguments)
  (let ((tool (gethash name *mcp-tools*)))
    (flet ((result (text error-p)
             (ht "content" (list (ht "type" "text" "text" text))
                 "isError" (if error-p t 'yason:false))))
      (if (null tool)
          (result (format nil "Outil inconnu : ~a" name) t)
          (handler-case
              (result (funcall (getf tool :handler) (or arguments (ht))) nil)
            (error (e)
              (result (format nil "~a" e) t)))))))

;;; JSON-RPC

(define-condition json-rpc-error (error)
  ((code :initarg :code :reader json-rpc-error-code)
   (message :initarg :message :reader json-rpc-error-message))
  (:report (lambda (c s) (write-string (json-rpc-error-message c) s))))

(defun handle-method (method params)
  (cond
    ((string= method "initialize")
     (ht "protocolVersion" (or (and params (gethash "protocolVersion" params))
                               *mcp-protocol-version*)
         "capabilities" (ht "tools" (ht))
         "serverInfo" (ht "name" "rogue-company" "version" "0.1.0")))
    ((string= method "ping") (ht))
    ((string= method "tools/list") (ht "tools" (tool-descriptions)))
    ((string= method "tools/call")
     (call-tool (gethash "name" params) (gethash "arguments" params)))
    (t (error 'json-rpc-error :code -32601
                              :message (format nil "Méthode inconnue : ~a" method)))))

(defun handle-message (message)
  "Renvoie la réponse JSON-RPC à MESSAGE, ou NIL pour une notification."
  (let ((id (gethash "id" message))
        (id-p (nth-value 1 (gethash "id" message))))
    (flet ((respond (key value)
             (when id-p (ht "jsonrpc" "2.0" "id" id key value))))
      (handler-case
          (let ((result (handle-method (gethash "method" message)
                                       (gethash "params" message))))
            (respond "result" result))
        (json-rpc-error (e)
          (respond "error" (ht "code" (json-rpc-error-code e)
                               "message" (json-rpc-error-message e))))
        (error (e)
          (respond "error" (ht "code" -32603 "message" (format nil "~a" e))))))))

;;; Sécurité

(defun constant-time-string= (a b)
  (and (= (length a) (length b))
       (zerop (loop with diff = 0
                    for x across a
                    for y across b
                    do (setf diff (logior diff (logxor (char-code x) (char-code y))))
                    finally (return diff)))))

(defun local-host-p (host)
  "Vrai si HOST (avec ou sans port, avec ou sans schéma) désigne la machine locale."
  (let* ((start (let ((p (search "://" host))) (if p (+ p 3) 0)))
         (host (string-downcase (subseq host start)))
         (end (if (and (plusp (length host)) (char= (char host 0) #\[))
                  (1+ (or (position #\] host) (1- (length host))))
                  (or (position #\: host) (length host)))))
    (member (subseq host 0 end) '("127.0.0.1" "localhost" "[::1]") :test #'string=)))

(defun authorized-p ()
  (let ((header (hunchentoot:header-in* :authorization))
        (prefix "Bearer "))
    (and header
         (> (length header) (length prefix))
         (string-equal prefix header :end2 (length prefix))
         (constant-time-string= *mcp-token* (subseq header (length prefix))))))

(defun reject (status text)
  (setf (hunchentoot:return-code*) status
        (hunchentoot:content-type*) "text/plain; charset=utf-8")
  text)

;;; HTTP

(defclass mcp-acceptor (hunchentoot:acceptor) ())

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor mcp-acceptor) request)
  (let ((origin (hunchentoot:header-in* :origin))
        (host (hunchentoot:header-in* :host)))
    (cond
      ((string/= (hunchentoot:script-name request) *mcp-path*)
       (reject 404 "Not found"))
      ((or (null host) (not (local-host-p host)))
       (reject 403 "Host refusé"))
      ((and origin (not (local-host-p origin)))
       (reject 403 "Origin refusée"))
      ((not (authorized-p))
       (setf (hunchentoot:header-out :www-authenticate) "Bearer")
       (reject 401 "Jeton manquant ou invalide"))
      ((not (eq (hunchentoot:request-method request) :post))
       (setf (hunchentoot:header-out :allow) "POST")
       (reject 405 "Seul POST est accepté"))
      (t (handle-post)))))

(defun handle-post ()
  (let* ((body (hunchentoot:raw-post-data :force-text t :external-format :utf-8))
         (message (handler-case (yason:parse body)
                    (error () nil))))
    (cond
      ((hash-table-p message)
       (let ((response (handle-message message)))
         (if response
             (progn (setf (hunchentoot:content-type*) "application/json")
                    (to-json response))
             (progn (setf (hunchentoot:return-code*) 202) ""))))
      (t
       (setf (hunchentoot:return-code*) 400
             (hunchentoot:content-type*) "application/json")
       (to-json (ht "jsonrpc" "2.0" "id" nil
                    "error" (ht "code" -32700 "message" "JSON invalide")))))))

(defun start-mcp-server ()
  (let ((token (uiop:getenv *mcp-token-variable*)))
    (when (or (null token) (< (length token) 32))
      (error "Définis ~a (au moins 32 caractères) avant de lancer le jeu."
             *mcp-token-variable*))
    (setf *mcp-token* token))
  (when *mcp-acceptor* (stop-mcp-server))
  (setf *mcp-acceptor*
        (make-instance 'mcp-acceptor :address *mcp-address* :port *mcp-port*
                                     :access-log-destination nil))
  (hunchentoot:start *mcp-acceptor*)
  (format t "~&Serveur MCP sur http://~a:~a~a~%" *mcp-address* *mcp-port* *mcp-path*))

(defun stop-mcp-server ()
  (when *mcp-acceptor*
    (hunchentoot:stop *mcp-acceptor* :soft t)
    (setf *mcp-acceptor* nil)))
