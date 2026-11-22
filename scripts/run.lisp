(ql:quickload :cffi :silent t)
(pushnew #p"/opt/homebrew/lib/" cffi:*foreign-library-directories* :test #'equal)
;; Le serveur MCP n'écoute qu'en HTTP local : inutile de charger cl+ssl.
(pushnew :hunchentoot-no-ssl *features*)
(push (truename ".") asdf:*central-registry*)
;; game-kit, le noyau du jeu : $GAME_KIT_DIR, sinon ../game-kit à côté du jeu.
(push (uiop:ensure-directory-pathname
       (or (uiop:getenvp "GAME_KIT_DIR") (merge-pathnames "../game-kit/" (truename "."))))
      asdf:*central-registry*)
(ql:quickload :rogue-company)
(rogue-company:main)
(uiop:quit 0)
