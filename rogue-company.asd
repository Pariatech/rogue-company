(asdf:defsystem #:rogue-company
  :description "Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP."
  :author "Gabriel Pariat"
  :license "MIT"
  :depends-on (#:cl-raylib #:hunchentoot #:yason #:sb-concurrency #:sb-md5)
  :serial t
  :pathname "src"
  :in-order-to ((asdf:test-op (asdf:test-op #:rogue-company/tests)))
  :components ((:file "package")
               (:file "mcp-server")
               (:file "jobs")
               (:file "input")
               (:file "engine")
               (:file "mcp-tools")
               (:file "console")
               (:file "game")))

(asdf:defsystem #:rogue-company/tests
  :depends-on (#:rogue-company #:fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "jobs-tests")
               (:file "reload-tests"))
  :perform (asdf:test-op (o c)
             (when (member nil (mapcar (lambda (suite)
                                       (uiop:symbol-call :fiveam :run!
                                                         (uiop:find-symbol* suite :rogue-company/tests)))
                                     '(:jobs :reload)))
               (error "Des tests ont échoué."))))
