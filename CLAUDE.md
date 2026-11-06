# rogue-company

Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP.

## Démarrage
- `scripts/dev.sh start|stop|restart|logs` gère le jeu dans la session tmux `rogue-company`.
- Le jeu doit tourner avant d'utiliser les outils MCP. Sinon, reconnecter avec `/mcp`.
- Tests de la file de tâches : `sbcl --non-interactive --load scripts/test.lisp`.

## Architecture
- `src/game.lisp` : état (`game-state`), `update` et `draw`. C'est le seul fichier rechargé par `game_reload`.
- `src/engine.lisp` : boucle, pause, erreurs gardées (`guarded`), pas à pas, capture d'écran.
- `src/jobs.lisp` : file vers le thread principal (`call-in-main-thread`).
- `src/input.lisp` : `key-down-p` combine le clavier réel et les touches simulées.
- `src/mcp-server.lisp` : serveur MCP Hunchentoot (JSON-RPC sur HTTP).
- `src/mcp-tools.lisp` : outils `game_*`, déclarés avec `define-mcp-tool`.
- `mcp-lisp` n'est pas utilisé : il écoute sur 0.0.0.0, sans authentification, et expose un outil `shell`.
- Un changement de `engine.lisp`, `input.lisp` ou `mcp-tools.lisp` demande `scripts/dev.sh restart`. En exploration seulement, on peut le charger à chaud avec `game_eval` et `(load (asdf:system-relative-pathname :rogue-company "src/…"))`.

## Règles de développement
- Écrire tout changement durable dans `src/`, puis appeler `game_reload`.
- Réserver `game_eval` à l'exploration et au diagnostic. Une tâche est interrompue au bout de 2 s.
- Ne jamais appeler raylib hors du thread principal : passer par la file de tâches.
- Lire le clavier avec `key-down-p`, jamais `raylib:is-key-down`, sinon `game_input` est sans effet.
- Utiliser `defclass` pour l'état, jamais `defstruct` pour des données qui évoluent.
- Ne pas réinitialiser `*state*` sans le dire.
- Masquer les float traps uniquement autour de la boucle principale.
- Ne pas nommer une fonction ou un accesseur d'après un symbole de `CL` (`speed`, `step`, etc.) : verrou de package SBCL.

## Vérifier un changement
1. Appeler `game_reload` et lire les avertissements. Un avertissement (pas un style-warning) bloque le chargement.
2. Pour la logique : `game_control` en pause, `game_input`, `game_control` step, puis `game_state`.
3. Pour le visuel : `game_screenshot`, puis ouvrir le PNG avec l'outil de lecture.
4. Corriger toute erreur signalée dans `last_error` de `game_state` avant de continuer. Un `game_reload` réussi efface l'erreur et relance le jeu.

## Limites connues
- macOS ralentit la fenêtre quand elle est cachée (environ 9 images/s au lieu de 60). Préférer `game_control` step, qui ne dépend pas de l'affichage, pour les tests de logique.
- `cl-raylib` vise raylib 5.0 alors que la version installée est la 6.2. Avant d'utiliser une nouvelle fonction raylib, comparer sa signature avec `/opt/homebrew/include/raylib.h`.

## Sécurité
- Le serveur écoute uniquement sur 127.0.0.1 et exige le jeton `LISP_GAME_MCP_TOKEN` (32 caractères minimum). Il refuse les en-têtes `Origin` et `Host` non locaux.
- Ne jamais versionner le jeton ni affaiblir ces contrôles.
