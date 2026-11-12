(asdf:defsystem #:rogue-company
  :description "Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP."
  :author "Gabriel Pariat"
  :license "MIT"
  :depends-on (#:cl-raylib #:hunchentoot #:yason #:sb-concurrency #:sb-md5 #:sb-sprof)
  :serial t
  :pathname "src"
  :in-order-to ((asdf:test-op (asdf:test-op #:rogue-company/tests)))
  :components ((:file "package")
               (:file "mcp-server")
               (:file "jobs")
               (:file "input")
               (:file "draw")
               (:file "engine")
               (:file "mcp-tools")
               (:file "console")
               (:file "rng")
               (:file "fixed-clock")
               (:file "scene")
               (:file "main-menu")
               (:file "battle")
               (:file "game")
               (:file "bench")))

(asdf:defsystem #:rogue-company/tests
  :depends-on (#:rogue-company #:fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "jobs-tests")
               (:file "reload-tests")
               (:file "rng-tests")
               (:file "fixed-clock-tests"))
  :perform (asdf:test-op (o c)
             (when (member nil (mapcar (lambda (suite)
                                       (uiop:symbol-call :fiveam :run!
                                                         (uiop:find-symbol* suite :rogue-company/tests)))
                                     '(:jobs :reload :rng :fixed-clock)))
               (error "Des tests ont échoué."))))
