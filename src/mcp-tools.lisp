(in-package #:rogue-company)

(define-mcp-tool "game_ping" ()
    "Vérifie que le jeu répond. Renvoie le numéro de l'image courante."
  (call-in-main-thread
   (lambda () (format nil "pong (image ~d)" *frame*))))

