# Plan d'implémentation : jeu en Common Lisp, raylib et MCP (macOS)

Nov 5, 2026 · @Gabriel Pariat

## Objectif

Le premier jalon est une fenêtre raylib avec un carré déplaçable, que Claude Code peut modifier, observer et tester pendant que le jeu tourne. Le type de jeu reste à choisir : ce socle ne dépend pas de ce choix.

Le jalon est atteint quand ces six scénarios passent sans redémarrer le processus :

- [ ] Claude Code se connecte au jeu et voit ses outils MCP avec `/mcp`.
- [ ] Il change la couleur du carré dans `src/game.lisp`, recharge le fichier, et la nouvelle couleur apparaît.
- [ ] Il prend une capture d'écran, puis l'ouvre pour confirmer ce changement.
- [ ] Il simule la touche droite pendant 30 images et constate que `x` a augmenté.
- [ ] Il introduit une erreur dans `draw` : le jeu se met en pause, rapporte l'erreur et reste ouvert. Après correction, le jeu reprend.
- [ ] Le serveur n'écoute que sur `127.0.0.1` et refuse une requête sans jeton.

## Architecture

Le jeu tourne en continu dans un seul processus SBCL, et Claude Code s'y connecte en HTTP local au lieu de le lancer lui-même. Le jeu peut ainsi survivre à la fin d'une session Claude Code.

&#91;embedded content: Architecture du jeu et de ses outils MCP\]

Les outils MCP ne touchent ni à raylib ni à l'état du jeu : ils déposent une tâche que le thread principal exécute entre deux images. Cela évite les plantages Cocoa et les accès concurrents à `*state*`.

Pour modifier le jeu, Claude Code édite `src/game.lisp`, puis appelle `game_reload`. La nouvelle version de `update` et de `draw` s'applique à l'image suivante, et `*state*` est conservé.

## Prérequis macOS

Tout s'installe avec Homebrew et Quicklisp, sauf `cl-raylib` et `mcp-lisp`, qui se clonent dans `local-projects` car ils ne sont pas distribués par Quicklisp. Le plan suppose un Mac Apple Silicon, où Homebrew se trouve dans `/opt/homebrew`.

```bash
brew install sbcl raylib libev tmux

# Quicklisp, si absent
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp \
  --eval '(quicklisp-quickstart:install)' \
  --eval '(ql:add-to-init-file)' --quit

cd ~/quicklisp/local-projects
git clone https://github.com/longlene/cl-raylib.git
git clone https://github.com/jsulmont/mcp-lisp.git
```

| Composant | Rôle | Pourquoi ce choix |
| --- | --- | --- |
| SBCL | Exécute le jeu | Compilation à chaud dans l'image en cours |
| raylib et [cl-raylib](https://awesome.ecosyste.ms/projects/github.com%2Flonglene%2Fcl-raylib) | Fenêtre, dessin, clavier | API simple, adaptée à un premier jeu 2D |
| [mcp-lisp](https://github.com/jsulmont/mcp-lisp) | Serveur MCP dans le jeu | Transport HTTP non bloquant et outils exécutés hors de la boucle d'événements |
| libev | Dépendance de Woo, le serveur HTTP de `mcp-lisp` | Nécessaire au transport HTTP |
| tmux | Garde le jeu actif hors de la session Claude Code | Permet de le démarrer et de lire sa console |

CFFI doit trouver les bibliothèques de Homebrew. Le script de démarrage ajoute donc `/opt/homebrew/lib/` à `cffi:*foreign-library-directories*` avant de charger `cl-raylib` et `mcp-lisp`.

## Structure du projet

Le code du moteur, le code du jeu et le serveur MCP restent séparés, afin que seul `src/game.lisp` change pendant le travail courant.

```text
lisp-game/
├── lisp-game.asd
├── CLAUDE.md             # règles de travail pour Claude Code
├── .mcp.json             # connexion au serveur MCP local
├── .gitignore            # exclut *.fasl et captures/
├── scripts/
│   ├── run.lisp          # point d'entrée, sur le thread principal
│   └── dev.sh            # démarre, arrête et affiche le jeu dans tmux
├── src/
│   ├── package.lisp
│   ├── engine.lisp       # boucle, pause, erreurs, pas à pas
│   ├── jobs.lisp         # file d'exécution vers le thread principal
│   ├── input.lisp        # clavier réel et touches simulées
│   ├── mcp-tools.lisp    # outils MCP
│   └── game.lisp         # état, update et draw : code à modifier à chaud
├── tests/
│   └── jobs-tests.lisp
└── captures/             # captures d'écran, non versionnées
```

## Étapes d'implémentation

Les deux premières étapes testent les intégrations les plus risquées avant d'écrire le socle. Chaque étape se termine par un commit, et l'étape suivante ne commence qu'après sa validation.

### Étape 1 : valider raylib sur le thread principal

Écrire `scripts/run.lisp` et une fenêtre minimale. Le jeu démarre avec `sbcl --dynamic-space-size 4GB --load scripts/run.lisp`.

```lisp
(ql:quickload :cffi)
(pushnew #p"/opt/homebrew/lib/" cffi:*foreign-library-directories* :test #'equal)
(push (truename ".") asdf:*central-registry*)
(ql:quickload :lisp-game)
(lisp-game:main)
```

`main` vérifie `(sb-thread:main-thread-p)` et enveloppe toute la boucle dans `sb-int:with-float-traps-masked`.

**Validation :** la fenêtre s'ouvre, un carré est dessiné, `ESC` ferme le programme proprement, sans erreur de virgule flottante ni plantage Cocoa.

**Arrêt :** si les signatures de `cl-raylib` ne correspondent pas à la version de raylib installée, s'arrêter et présenter les options : installer une version compatible, corriger la liaison ou passer à une autre.

### Étape 2 : valider MCP depuis le jeu

Créer un serveur `mcp-lisp` non bloquant avec un seul outil `game_ping`, sur le port 7777, avec `:event-loops 1 :tool-workers 2`. Avant de coder, lire le `README.md` et `examples/` de `mcp-lisp`, sans deviner la forme des valeurs de retour.

```json
{
  "mcpServers": {
    "lisp-game": {
      "type": "http",
      "url": "http://127.0.0.1:7777/mcp",
      "headers": { "Authorization": "Bearer ${LISP_GAME_MCP_TOKEN}" }
    }
  }
}
```

**Validation :**

- `claude mcp list` montre le serveur connecté et `game_ping` répond pendant que la fenêtre s'anime.
- `lsof -nP -iTCP:7777 -sTCP:LISTEN` montre `127.0.0.1:7777`, et non `*:7777`.
- Une requête sans jeton valide reçoit une réponse 401, et une requête avec un en-tête `Origin` non autorisé est refusée.

**Arrêt :** l'outil `game_eval` n'est pas ajouté tant que ces contrôles de sécurité échouent. Si `mcp-lisp` ne permet pas de les satisfaire, appliquer la solution de repli décrite dans la section des risques.

### Étape 3 : construire le socle du jeu

Écrire `src/engine.lisp` et `src/game.lisp`. L'état du jeu utilise `defclass` plutôt que `defstruct`, car CLOS met à jour les instances existantes quand la classe change. `*state*` utilise `defvar` pour survivre aux rechargements.

```lisp
(defun run-frame ()
  (run-pending-jobs :max 8)
  (unless *paused*
    (guarded (update *state* (next-dt))))
  (raylib:with-drawing
    (guarded (draw *state*))
    (draw-error-overlay)
    (run-pending-screenshot)))
```

`guarded` capture la pile d'appels avec `handler-bind`, enregistre l'erreur dans `*last-error*` et met le jeu en pause. L'appel se trouve dans `with-drawing`, pour que chaque image reste correctement terminée.

**Validation :** les flèches déplacent le carré. Une erreur provoquée dans `update` ou `draw` affiche un message, sans fermer la fenêtre.

### Étape 4 : ajouter la file d'exécution

Écrire `src/jobs.lisp` avec `sb-concurrency` et `sb-thread`. Un thread d'outil MCP crée une tâche, attend son résultat, et le thread principal l'exécute entre deux images. L'attente expire après 5 s et renvoie une erreur claire. L'exécution d'une tâche est limitée à 2 s avec `sb-ext:with-timeout`.

**Validation :** les tests de `tests/jobs-tests.lisp` couvrent le succès, l'erreur et l'expiration. Une boucle infinie envoyée au jeu est interrompue et la fenêtre continue de répondre.

### Étape 5 : exposer les outils de développement

Ajouter les outils un par un dans `src/mcp-tools.lisp`, en testant chacun depuis Claude Code.

| Outil | Entrée | Résultat | Détail d'implémentation |
| --- | --- | --- | --- |
| `game_eval` | Code Lisp | Valeurs, sortie standard ou erreur et pile d'appels | S'exécute dans le package du jeu, via la file |
| `game_reload` | Aucune | Avertissements de compilation et statut | Compile `src/game.lisp` et ne charge rien en cas d'échec |
| `game_state` | Aucune | État lisible, image courante, pause, dernière erreur | Ne renvoie que des données sérialisées |
| `game_screenshot` | Aucune | Chemin absolu du PNG | Capture juste avant la fin de `with-drawing` |
| `game_input` | Touche, nombre d'images | État après traitement | Passe par `src/input.lisp`, sans focus de fenêtre |
| `game_control` | `pause`, `resume`, `step`, nombre d'images | État après traitement | `step` utilise un `dt` fixe de 1/60 s |

`game_screenshot` enregistre le fichier dans `captures/`, et Claude Code l'ouvre ensuite avec son outil de lecture. Cela évite de dépendre du renvoi d'images par `mcp-lisp`.

**Validation :** chaque outil réussit son scénario depuis une session Claude Code réelle, et une erreur renvoie un message exploitable au lieu de bloquer le jeu.

### Étape 6 : outiller le quotidien

Écrire `scripts/dev.sh` avec les commandes `start`, `stop`, `restart` et `logs`, basées sur une session tmux nommée `lisp-game`. Écrire `CLAUDE.md` à partir de la dernière section de ce plan.

**Validation :** les six scénarios de la section Objectif réussissent dans une nouvelle session Claude Code, avec le jeu lancé par `dev.sh start`.

## Risques et points à vérifier

Le principal risque est la sécurité de `mcp-lisp`, car `game_eval` exécute du code arbitraire et sa documentation ne précise ni l'adresse d'écoute ni l'authentification. Les étapes 1 et 2 servent aussi à vérifier les points de cette table.

| Risque | Pourquoi | Réponse prévue |
| --- | --- | --- |
| Serveur exposé au réseau | Le [README de mcp-lisp](https://github.com/jsulmont/mcp-lisp) montre `:port`, mais aucune option d'adresse ni de jeton | Contrôler avec `lsof` et des requêtes réelles avant d'ajouter `game_eval` |
| `mcp-lisp` trop jeune | Projet de 9 étoiles, hors Quicklisp ; son nom de transport `:sse` désigne en fait du Streamable HTTP | Lire son code et ses exemples ; garder les outils indépendants de cette bibliothèque |
| `cl-raylib` incompatible | Le [README](https://awesome.ecosyste.ms/projects/github.com%2Flonglene%2Fcl-raylib) n'indique pas de version de raylib, et l'index consulté montre un dernier push en novembre 2024 | Tester à l'étape 1 et comparer les signatures à la version installée |
| Plantage graphique sur macOS | L'interface graphique doit tourner sur le thread principal ; l'[exemple cl-glfw3](https://github.com/AlexCharlton/cl-glfw3/issues/10) échoue quand SLIME la lance ailleurs | Démarrer avec `sbcl --load`, ne jamais ouvrir la fenêtre depuis un thread d'outil |
| Erreurs de virgule flottante | Cocoa peut [déclencher un SIGFPE](https://github.com/gregcman/sucle/blob/886e7bd20d5e21b19c16f94b41294549b30ae729/src/window-opengl-glfw3/window.lisp) capté par SBCL | Masquer les interruptions autour de la boucle et contrôler les `NaN` dans `update` |
| Jeu ralenti quand la fenêtre est cachée | Hypothèse à vérifier : macOS peut réduire le rythme des images, ce qui retarde la file | Tester à l'étape 4 ; si cela se confirme, garder la fenêtre visible ou limiter l'attente de synchronisation |
| Modifications perdues | Le code envoyé par `game_eval` disparaît au redémarrage | Écrire tout changement durable dans `src/`, puis utiliser `game_reload` |
| Serveur absent au démarrage de Claude Code | La connexion échoue si le jeu n'est pas lancé | Lancer `dev.sh start` d'abord, ou reconnecter avec `/mcp` |

**Solution de repli pour MCP :** si `mcp-lisp` ou Woo bloque, écrire un petit serveur Hunchentoot lié à `127.0.0.1`. Il gère `initialize`, `tools/list` et `tools/call` en JSON-RPC, vérifie le jeton et l'en-tête `Origin`, et réutilise la même file de tâches.

**À éviter :** `trivial-main-thread`. Un développeur de SBCL [le déconseille sur r/lisp](https://axebps-redlib.hf.space/r/lisp/comments/1gxekrj/repl_hangs_after_exit/lylieg4/?context=3), et le lancement par `--load` n'en a pas besoin.

## Consignes pour Claude Code

Transmets ce plan à Claude Code avec le message ci-dessous, puis fais copier le bloc suivant dans `CLAUDE.md` à l'étape 6.

```text
Implémente ce plan étape par étape, dans un nouveau dépôt git nommé lisp-game.
Commence par vérifier les prérequis et signale ce qui manque avant d'installer.
Après chaque étape, exécute sa validation, fais un commit et résume le résultat.
Arrête-toi aux conditions d'arrêt et présente-moi les options.
N'ajoute pas game_eval tant que les contrôles de sécurité de l'étape 2 échouent.
```

```markdown
# lisp-game

Jeu en Common Lisp (SBCL, raylib) modifiable à chaud par MCP.

## Démarrage
- `scripts/dev.sh start|stop|restart|logs` gère le jeu dans tmux.
- Le jeu doit tourner avant d'utiliser les outils MCP. Sinon, reconnecter avec `/mcp`.

## Règles de développement
- Écrire tout changement durable dans `src/`, puis appeler `game_reload`.
- Réserver `game_eval` à l'exploration et au diagnostic.
- Ne jamais appeler raylib hors du thread principal : passer par la file de tâches.
- Utiliser `defclass` pour l'état, jamais `defstruct` pour des données qui évoluent.
- Ne pas réinitialiser `*state*` sans le dire.
- Masquer les float traps uniquement autour de la boucle principale.

## Vérifier un changement
1. Appeler `game_reload` et lire les avertissements.
2. Pour la logique : `game_control` en pause, `game_input`, `step`, puis `game_state`.
3. Pour le visuel : `game_screenshot`, puis ouvrir le PNG.
4. Corriger toute erreur signalée dans `*last-error*` avant de continuer.

## Sécurité
- Le serveur écoute uniquement sur 127.0.0.1 et exige le jeton LISP_GAME_MCP_TOKEN.
- Ne jamais versionner le jeton ni affaiblir ces contrôles.
```

Avant le premier lancement, définis le jeton dans ton shell : `export LISP_GAME_MCP_TOKEN=$(openssl rand -hex 32)`. Le jeu et Claude Code doivent être démarrés avec cette même valeur, donc conserve-la dans ton `~/.zshrc`, ou un gestionnaire de secrets.

## Sources

- [mcp-lisp](https://github.com/jsulmont/mcp-lisp) : outils, transports et options du serveur
- [cl-raylib](https://awesome.ecosyste.ms/projects/github.com%2Flonglene%2Fcl-raylib) : installation et activité du dépôt
- [Issue n° 10 de cl-glfw3](https://github.com/AlexCharlton/cl-glfw3/issues/10) : thread principal et virgule flottante sur macOS
- [Masquage des interruptions dans sucle](https://github.com/gregcman/sucle/blob/886e7bd20d5e21b19c16f94b41294549b30ae729/src/window-opengl-glfw3/window.lisp) : Cocoa et SBCL
- [Discussion r/lisp sur le REPL bloqué](https://axebps-redlib.hf.space/r/lisp/comments/1gxekrj/repl_hangs_after_exit/lylieg4/?context=3) : `trivial-main-thread`
- [Guide des transports MCP](https://www.mcpbundles.com/guides/local-vs-remote-mcp-servers) : configuration `type: http` dans Claude Code
