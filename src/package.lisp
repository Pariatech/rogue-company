(defpackage #:rogue-company
  ;; Le noyau (boucle, serveur MCP, console, dessin sans allocation, scènes,
  ;; horloge, générateur) vient de game-kit (../game-kit).
  (:use #:cl #:game-kit)
  (:export #:main))
