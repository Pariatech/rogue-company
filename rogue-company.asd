(asdf:defsystem #:rogue-company
  :description "Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP."
  :author "Gabriel Pariat"
  :license "MIT"
  :depends-on (#:cl-raylib #:hunchentoot #:yason #:sb-concurrency)
  :serial t
  :pathname "src"
  :in-order-to ((asdf:test-op (asdf:test-op #:rogue-company/tests)))
  :components ((:file "package")
               (:file "mcp-server")
               (:file "jobs")
               (:file "input")
               (:file "engine")
               (:file "mcp-tools")
               (:file "game")))

(asdf:defsystem #:rogue-company/tests
  :depends-on (#:rogue-company #:fiveam)
  :pathname "tests"
  :components ((:file "jobs-tests"))
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :fiveam :run!
                                       (uiop:find-symbol* :jobs :rogue-company/tests))
               (error "Des tests ont échoué."))))
