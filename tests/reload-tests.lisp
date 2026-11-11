(in-package #:rogue-company/tests)

(def-suite reload :description "Choix des fichiers recompilés par game_reload.")
(in-suite reload)

(defparameter *names* '("package" "mcp-server" "jobs" "input" "engine" "mcp-tools" "game"))
(defparameter *infrastructure* '("package" "mcp-server" "jobs" "mcp-tools"))

(defun plan (&rest changed)
  (multiple-value-list (rogue-company::plan-reload *names* changed *infrastructure*)))

(test nothing-changed
  (is (equal '(() () ()) (plan))))

(test last-file
  (is (equal '(("game") () ()) (plan "game"))))

(test followers-recompiled-without-infrastructure
  (is (equal '(("engine" "game") ("mcp-tools") ()) (plan "engine")))
  (is (equal '(("input" "engine" "game") ("mcp-tools") ()) (plan "input"))))

(test order-follows-system-not-request
  (is (equal '(("input" "engine" "game") ("mcp-tools") ()) (plan "game" "input"))))

(test infrastructure-blocks-everything
  (is (equal '(() () ("mcp-tools")) (plan "mcp-tools")))
  (is (equal '(() () ("jobs" "mcp-tools")) (plan "game" "mcp-tools" "jobs"))))

(test normalize-file-name
  (is (string= "engine" (rogue-company::normalize-file-name "engine" *names*)))
  (is (string= "engine" (rogue-company::normalize-file-name "engine.lisp" *names*)))
  (is (string= "engine" (rogue-company::normalize-file-name "src/engine.lisp" *names*)))
  (signals error (rogue-company::normalize-file-name "nope" *names*)))

(test parse-file-list
  (is (equal '() (rogue-company::parse-file-list nil)))
  (is (equal '("engine" "game") (rogue-company::parse-file-list '("engine" "game"))))
  (is (equal '("src/engine.lisp") (rogue-company::parse-file-list "[\"src/engine.lisp\"]")))
  (is (equal '("engine" "game") (rogue-company::parse-file-list "engine, game")))
  (is (equal '("engine") (rogue-company::parse-file-list "engine")))
  (signals error (rogue-company::parse-file-list '(1 2))))
