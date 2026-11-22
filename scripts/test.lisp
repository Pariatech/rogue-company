(ql:quickload :cffi :silent t)
(pushnew #p"/opt/homebrew/lib/" cffi:*foreign-library-directories* :test #'equal)
(pushnew :hunchentoot-no-ssl *features*)
(push (truename ".") asdf:*central-registry*)
;; game-kit, le noyau du jeu : $GAME_KIT_DIR, sinon ../game-kit à côté du jeu.
(push (uiop:ensure-directory-pathname
       (or (uiop:getenvp "GAME_KIT_DIR") (merge-pathnames "../game-kit/" (truename "."))))
      asdf:*central-registry*)
(ql:quickload '(:rogue-company :fiveam) :silent t)
(uiop:quit (handler-case (progn (asdf:test-system :rogue-company) 0)
             (error (e) (format t "~&~a~%" e) 1)))
