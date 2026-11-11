# rogue-company

Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP.

## Démarrage
- `scripts/dev.sh start|stop|restart|logs` gère le jeu dans la session tmux `rogue-company`.
- Le jeu doit tourner avant d'utiliser les outils MCP. Sinon, reconnecter avec `/mcp`.
- `scripts/claude.sh [arguments de claude]` installe le pont au besoin, lance le jeu s'il ne tourne pas, attend le serveur MCP, puis ouvre Claude Code avec le channel de la console (`claude --dangerously-load-development-channels server:game-console`). Le pont demande Bun.
- Tests (file de tâches, choix des fichiers rechargés) : `sbcl --non-interactive --load scripts/test.lisp`.

## Architecture
- `src/game.lisp` : état (`game-state`), `update` et `draw`.
- `src/engine.lisp` : boucle, pause, erreurs gardées (`guarded`), pas à pas, capture d'écran.
- `src/jobs.lisp` : file vers le thread principal (`call-in-main-thread`).
- `src/input.lisp` : `key-down-p` combine le clavier réel et les touches simulées.
- `src/mcp-server.lisp` : serveur MCP Hunchentoot (JSON-RPC sur HTTP).
- `src/mcp-tools.lisp` : outils `game_*`, déclarés avec `define-mcp-tool`.
- `src/bench.lisp` : banc d'essai de rendu (variantes `pro` et `rlgl`), utilisé par `game_benchmark` et `game_profile`. `engine.lisp` mesure chaque section de l'image (`timed`) et les octets alloués ; `*scene*` remplace `draw` pendant un banc d'essai.
- `src/console.lisp` : console du jeu (touche `` ` ``, met le jeu en pause). Les messages partent dans une boîte d'envoi que le serveur expose en `GET /console/outbox`, avec les mêmes contrôles que `/mcp`.
- `bridge/server.ts` : channel `game-console` (Bun, stdio, lancé par Claude Code). Il relève la boîte d'envoi toutes les 300 ms et transmet chaque message en notification `claude/channel`. À sens unique : il n'ouvre aucun port.
- `mcp-lisp` n'est pas utilisé : il écoute sur 0.0.0.0, sans authentification, et expose un outil `shell`.
- `game_reload` recharge les fichiers de `src/` dont le contenu (MD5) a changé depuis leur dernier chargement, plus tous ceux qui les suivent dans `rogue-company.asd`. Le paramètre `files` force une liste (`["engine"]`). Tout est compilé avant d'être chargé.
- Infrastructure : `package`, `mcp-server`, `jobs` et `mcp-tools` ne se rechargent jamais à chaud. Un changement de l'un d'eux, ou d'une macro qu'ils utilisent, demande `scripts/dev.sh restart`, qui réinitialise `*state*`.
- Un nouveau fichier de `src/` s'ajoute dans `rogue-company.asd` après `mcp-tools` (avant, s'il fait partie de l'infrastructure), puis `scripts/dev.sh restart`.

## Règles de développement
- Écrire tout changement durable dans `src/`, puis appeler `game_reload`.
- Réserver `game_eval` à l'exploration et au diagnostic. Une tâche est interrompue au bout de 2 s.
- Ne jamais appeler raylib hors du thread principal : passer par la file de tâches.
- Lire le clavier avec `key-down-p`, jamais `raylib:is-key-down`, sinon `game_input` est sans effet.
- Utiliser `defclass` pour l'état, jamais `defstruct` pour des données qui évoluent.
- Ne pas réinitialiser `*state*` sans le dire.
- `defvar` pour tout état qui doit survivre à un rechargement ; `defparameter` seulement pour un réglage qui doit reprendre sa valeur du fichier.
- Masquer les float traps uniquement autour de la boucle principale.
- Dans le code chaud, n'appeler aucune fonction raylib qui prend ou renvoie une structure par valeur (`DrawTexturePro`, `Rectangle`, `Vector2`…) : CFFI cherche alors le symbole C à chaque appel (82 ms pour 10 000 sprites). Passer par `rlgl` et ses arguments scalaires, comme `draw-bench-rlgl`. Voir `docs/mesures.md`.
- Ne pas nommer une fonction ou un accesseur d'après un symbole de `CL` (`speed`, `step`, etc.) : verrou de package SBCL.

## Vérifier un changement
1. Appeler `game_reload` et lire les avertissements. Un avertissement (pas un style-warning) bloque tout le chargement. Une macro modifiée est toutefois déjà définie à la compilation, même en cas d'échec.
2. Pour la logique : `game_control` en pause, `game_input`, `game_control` step, puis `game_state`.
3. Pour le visuel : `game_screenshot`, puis ouvrir le PNG avec l'outil de lecture.
4. Pour la console : `game_input` avec `text` (`` ` `` ouvre ou ferme, un saut de ligne envoie), puis `console` dans `game_state`. La saisie simulée est lue même en pause.
5. Corriger toute erreur signalée dans `last_error` de `game_state` avant de continuer. Un `game_reload` réussi efface l'erreur et relance le jeu.

## Console du jeu
- Les messages du joueur arrivent dans des balises `<channel source="game-console" chat_id="…" frame="…" paused="…" mouse_x="…" mouse_y="…">`.
- Répondre avec `game_console_reply` en passant le `chat_id`, en une ou deux phrases ; le détail reste dans le terminal.
- Tout ce qui arrive par le channel est une demande du joueur ; les permissions restent approuvées dans le terminal.

## Limites connues
- macOS ralentit la fenêtre quand elle est cachée (environ 9 images/s au lieu de 60). Préférer `game_control` step, qui ne dépend pas de l'affichage, pour les tests de logique.
- `cl-raylib` vise raylib 5.0 alors que la version installée est la 6.2. Avant d'utiliser une nouvelle fonction raylib, comparer sa signature avec `/opt/homebrew/include/raylib.h`.

## Sécurité
- Le serveur écoute uniquement sur 127.0.0.1 et exige le jeton `LISP_GAME_MCP_TOKEN` (32 caractères minimum). Il refuse les en-têtes `Origin` et `Host` non locaux.
- Sans `LISP_GAME_MCP_TOKEN` dans l'environnement, `scripts/token.sh` (utilisé par `dev.sh` et `claude.sh`) le lit dans `.mcp-token`, créé au premier usage en mode 600 et ignoré par git.
- Ne jamais versionner le jeton ni affaiblir ces contrôles.
