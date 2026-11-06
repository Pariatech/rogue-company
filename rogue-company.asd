(asdf:defsystem #:rogue-company
  :description "Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP."
  :author "Gabriel Pariat"
  :license "MIT"
  :depends-on (#:cl-raylib #:hunchentoot #:yason)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "mcp-server")
               (:file "engine")
               (:file "mcp-tools")
               (:file "game")))
