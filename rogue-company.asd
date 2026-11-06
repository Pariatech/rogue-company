(asdf:defsystem #:rogue-company
  :description "Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP."
  :author "Gabriel Pariat"
  :license "MIT"
  :depends-on (#:cl-raylib)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "game")
               (:file "engine")))
