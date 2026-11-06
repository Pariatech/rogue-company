(in-package #:rogue-company)

(define-mcp-tool "game_ping" ()
    "Vérifie que le jeu répond. Renvoie le numéro de l'image courante."
  (format nil "pong (image ~d)" *frame*))
