# Plan d'implémentation : jeu en Common Lisp, raylib et MCP (macOS)

Nov 5, 2026 · @Gabriel Pariat

## Objectif

Le premier jalon est une fenêtre raylib avec un carré déplaçable, que Claude Code peut modifier, observer et tester pendant que le jeu tourne. Ce socle ne dépend pas encore du jeu visé, décrit dans la section suivante.

Le jalon est atteint quand ces six scénarios passent sans redémarrer le processus :

- [ ] Claude Code se connecte au jeu et voit ses outils MCP avec `/mcp`.
- [ ] Il change la couleur du carré dans `src/game.lisp`, recharge le fichier, et la nouvelle couleur apparaît.
- [ ] Il prend une capture d'écran, puis l'ouvre pour confirmer ce changement.
- [ ] Il simule la touche droite pendant 30 images et constate que `x` a augmenté.
- [ ] Il introduit une erreur dans `draw` : le jeu se met en pause, rapporte l'erreur et reste ouvert. Après correction, le jeu reprend.
- [ ] Le serveur n'écoute que sur `127.0.0.1` et refuse une requête sans jeton.

## Le jeu visé

C'est un jeu de batailles en 2D vu de dessus, à l'époque du pike and shock, où des régiments de nombreux soldats s'affrontent sur un terrain en relief. L'usage du terrain doit être déterminant, dans l'esprit de la série Total War.

| Décision | Choix retenu | Raison |
| --- | --- | --- |
| Langage | Common Lisp (SBCL) | Tout se modifie à chaud, classes comprises ; Rust avec Bevy ne recharge que les fonctions |
| Vue | De dessus, orthographique | La position à l'écran est la position au sol ; un seul sprite par soldat, pivoté ; l'isométrique rend les hauteurs moins lisibles |
| Mêlée | Combat de formation : chaque soldat frappe l'ennemi le plus proche à portée | Simple et peu coûteux ; la profondeur de formation compte sans règle spéciale |
| Tir | Résolu par régiment, avec test de ligne de tir | Aucun projectile simulé |
| Terrain | Carte d'élévation, ombrage du relief, mode topographique | Le relief agit sur le tir, le mouvement, les charges et le moral |
| Console dans le jeu | Channel Claude Code | La session survit aux plantages du jeu et reste celle du terminal |

**Cible de performance initiale, à confirmer :** 10 000 soldats à 60 images par seconde sur ton Mac, soit 16,6 ms par image.

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

Chaque système vit dans son propre fichier, pour que Claude Code puisse recharger et tester un système sans toucher aux autres. `src/game.lisp` contient le scénario de test en cours d'exploration.

```text
rogue-company/
├── rogue-company.asd
├── CLAUDE.md             # règles de travail pour Claude Code
├── .mcp.json             # serveur du jeu (HTTP) et pont de console (stdio)
├── .gitignore            # exclut *.fasl et captures/
├── docs/
│   ├── plan.md           # ce document, exporté en Markdown
│   └── mesures.md        # résultats des bancs d'essai
├── scripts/
│   ├── run.lisp          # point d'entrée, sur le thread principal
│   └── dev.sh            # démarre, arrête et affiche le jeu dans tmux
├── src/
│   ├── package.lisp
│   ├── engine.lisp       # boucle, pas fixe de 30 Hz, pause, erreurs
│   ├── jobs.lisp         # file d'exécution vers le thread principal
│   ├── input.lisp        # clavier réel et touches simulées
│   ├── mcp-tools.lisp    # outils MCP
│   ├── soldiers.lisp     # tableaux typés des soldats
│   ├── regiments.lisp    # régiments, formations, moral
│   ├── spatial.lisp      # grille spatiale
│   ├── combat.lisp       # mêlée en deux phases, poussée, salves
│   ├── terrain.lisp      # élévation, pentes, grille des occupants
│   ├── los.lisp          # ligne de tir
│   ├── render.lisp       # atlas, ombrage, mode topographique
│   ├── console.lisp      # console du jeu
│   └── game.lisp         # scénario de test, modifié à chaud
├── shaders/
│   └── topo.fs           # courbes de niveau et teintes d'altitude
├── bridge/
│   └── server.ts         # pont du channel Claude Code
├── assets/
│   ├── atlas.png
│   └── heightmaps/
├── tests/
│   ├── jobs-tests.lisp
│   ├── combat-tests.lisp # dont le test de déterminisme
│   └── los-tests.lisp
└── captures/             # captures d'écran, non versionnées
```

## Étapes d'implémentation

Les deux premières étapes testent les intégrations les plus risquées avant d'écrire le socle. Chaque étape se termine par un commit, et l'étape suivante ne commence qu'après sa validation. Les étapes 1 à 6 construisent l'outillage, les étapes 7 à 12 la bataille.

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

### Étape 7 : banc d'essai de rendu

Avant toute logique de bataille, vérifier que raylib et CFFI tiennent la charge. Dessiner 10 000 sprites tirés d'un atlas, chacun pivoté, avec `DrawTexturePro`.

Ajouter deux outils MCP :

| Outil | Résultat |
| --- | --- |
| `game_benchmark` | Lance une scène de test et renvoie, par système, le temps moyen et le 99e centile en ms |
| `game_profile` | Lance `sb-sprof` pendant N secondes et renvoie les fonctions les plus coûteuses |

**Validation :** le temps de rendu de 10 000 sprites est mesuré et noté dans `docs/mesures.md`.

**Arrêt :** si le rendu dépasse 8 ms, présenter les options : réduire les appels CFFI, ou écrire une petite fonction C qui dessine un régiment entier en un appel.

### Étape 8 : soldats, régiments et pas de temps fixe

Mettre en place le modèle de la section Données : régiments en CLOS, soldats en tableaux typés. La simulation passe à un pas fixe de 30 Hz, indépendant du rendu, avec un générateur aléatoire à graine.

**Validation :** deux régiments rejoignent leur formation, et `(time ...)` sur 1 000 ticks montre zéro octet alloué par tick dans les systèmes de soldats.

### Étape 9 : grille spatiale et combat de formation

Écrire la grille spatiale, le combat en deux phases, la poussée et le moral, décrits dans la section Simulation.

**Validation :**

- Deux blocs de 500 piquiers se rencontrent à 60 images par seconde, avec une marge mesurée par `game_benchmark`.
- Deux exécutions avec la même graine donnent le même résultat : un test compare un hachage de l'état après 1 000 ticks.

### Étape 10 : terrain et mode topographique

Charger une carte d'élévation, précalculer l'ombrage du relief, ajouter le mode topographique et les effets de pente, décrits dans les sections Terrain et Rendu.

**Validation :** des captures du mode normal et du mode topographique ; l'altitude affichée au survol correspond à la carte ; un régiment monte une pente plus lentement qu'il ne la descend.

### Étape 11 : tir et ligne de tir

Écrire `line-of-fire-p`, l'échantillonnage par rayons et les salves par rang. Ajouter l'outil `game_line_of_fire`, qui renvoie pour un régiment la part de rayons dégagés et le point de blocage de chacun.

**Validation :** des tests couvrent les quatre cas de la section Terrain : bloqué derrière des piquiers sur terrain plat, possible depuis une colline, bloqué par une crête, possible vers le bas d'une pente.

### Étape 12 : console de jeu reliée à Claude Code

Écrire la console du jeu et le pont `bridge/`, décrits dans la section Console.

**Validation :** un message tapé dans le jeu arrive dans la session Claude Code avec son contexte, et la réponse s'affiche dans la console. Après un plantage provoqué du jeu, la session continue et relance le jeu avec `dev.sh restart`.

## Règles de performance

CLOS sert à la structure du jeu, et les boucles chaudes travaillent sur des tableaux typés. Le dispatch des fonctions génériques ne coûte cher que s'il se répète des milliers de fois par image.

1. **Prototyper, puis stabiliser :** écrire un nouveau système en CLOS, sans déclarations. Quand il ralentit et que sa forme ne change plus, le convertir en tableaux typés avec déclarations.
2. **Déclarer les types dans le code chaud :** `(optimize (speed 3) (safety 1))`, `single-float` pour les flottants (`1.0f0`), `fixnum` pour les indices. Les notes du compilateur SBCL servent de liste de corrections.
3. **Ne jamais utiliser `(safety 0)` :** une erreur corromprait la mémoire au lieu de s'arrêter proprement.
4. **Zéro allocation par image :** préallouer tableaux et tampons ; pas de listes ni de flottants emballés dans les boucles ; vérifier avec `time`.
5. **Maîtriser le ramasse-miettes :** appeler `(sb-ext:gc :full t)` au chargement et entre les batailles, jamais pendant. Régler `sb-ext:bytes-consed-between-gcs` seulement après mesure.
6. **Mesurer avant d'optimiser :** utiliser `game_profile` et `game_benchmark`, et `disassemble` pour vérifier que l'arithmétique est typée.
7. **Utiliser `declaim inline` avec prudence :** les appelants gardent l'ancienne version jusqu'à leur recompilation, ce qui gêne le rechargement à chaud.
8. **Paralléliser plus tard, après mesure :** les régiments sont indépendants et peuvent être répartis avec `lparallel`, chaque thread n'écrivant que dans les indices de ses régiments. Le rendu reste sur le thread principal.
9. **Version finale :** produire l'exécutable avec `sb-ext:save-lisp-and-die`, sans le serveur MCP ni la console.

Budget initial par image, à ajuster après l'étape 7 :

| Poste | Budget (ms) |
| --- | --- |
| Simulation | 5 |
| Rendu | 8 |
| Marge | 3,6 |

## Données : composition plutôt qu'héritage

Une entité se décrit par ce qu'elle possède, pas par ce qu'elle est. Il n'y a pas de hiérarchie de classes de soldats : un type d'unité est un profil de données.

**Régiments, en CLOS.** Peu nombreux, ils gardent la souplesse du rechargement à chaud. Un régiment compose ses parties dans des slots : profil d'arme, formation, moral, ordres et plage de soldats.

```lisp
(defclass weapon-profile ()
  ((reach    :initarg :reach)      ; m : piques sur environ 4 rangs, épées 1
   (damage   :initarg :damage)
   (cooldown :initarg :cooldown)   ; ticks entre deux coups
   (ranged   :initarg :ranged :initform nil)))

(defclass regiment ()
  ((side      :initarg :side)
   (profile   :initarg :profile)   ; weapon-profile partagé par type d'unité
   (formation :initarg :formation) ; rangs, files, orientation, emplacements
   (morale    :initarg :morale)
   (orders    :initarg :orders)
   (start     :initarg :start)     ; premier indice dans les tableaux de soldats
   (capacity  :initarg :capacity)
   (alive     :initarg :alive)))
```

**Soldats, en tableaux typés.** Un soldat est un indice. Chaque attribut a son propre tableau `(simple-array single-float (*))` ou `fixnum` : position, vitesse, emplacement cible, points de vie, cible, recharge et régiment.

- **Plages contiguës :** chaque régiment possède une plage fixe d'indices, de `start` à `start + capacity`. Parcourir un régiment revient à parcourir une plage continue de mémoire.
- **Suppression par échange :** quand un soldat meurt, il est échangé avec le dernier vivant de son régiment, puis `alive` diminue. Les tableaux restent compacts.
- **Inspection :** `game_state` affiche un régiment et ses soldats sous une forme lisible.

Les fonctions génériques restent utiles comme protocoles. Par exemple, `on-charge-impact` peut être spécialisée à la fois sur l'attaquant et sur le défenseur, ce qui remplace bien des hiérarchies.

## Simulation de bataille

Le principe : calculer le moins possible par soldat, et le plus possible par régiment.

| Niveau | Calculé | Fréquence |
| --- | --- | --- |
| Régiment | Ordres, IA, recherche de chemin, formation, moral, poussée, salves | 2 à 10 fois par seconde, échelonné entre les régiments |
| Soldat | Rejoindre son emplacement, éviter ses voisins, frapper | Chaque tick, à 30 Hz |

### Mouvement et grille spatiale

- **Formation par emplacements :** le régiment calcule une grille de positions cibles, et chaque soldat se dirige vers la sienne.
- **Une recherche de chemin par régiment,** jamais par soldat.
- **Grille spatiale :** des cases au moins aussi grandes que la portée maximale de mêlée, pour que les 9 cases voisines suffisent toujours. Elle est reconstruite à chaque tick dans des tableaux préalloués, par tri par case.
- **Test préalable :** un soldat dont le régiment ne touche aucun ennemi, portée comprise, saute la recherche de cible.

### Combat de formation, en deux phases par tick

1. **Ciblage, en lecture seule :** chaque soldat garde sa cible si elle vit et reste à portée ; sinon, il cherche l'ennemi le plus proche à portée. Il n'écrit que dans sa propre case de `*target*`, donc cette phase est parallélisable.
2. **Application, séquentielle :** les dégâts de tous les attaquants prêts s'additionnent par cible, donc plusieurs soldats peuvent frapper le même ennemi. Les morts sont retirés à la fin du tick.

```lisp
;; Phase 1 : chaque soldat écrit seulement sa propre case
(dotimes (i count)
  (setf (aref *target* i) (nearest-enemy-in-reach i)))   ; -1 si aucun

;; Phase 2 : séquentielle, résultat indépendant de l'ordre des soldats
(fill *damage* 0f0)
(dotimes (i count)
  (let ((tgt (aref *target* i)))
    (when (and (>= tgt 0) (attack-ready-p i))
      (incf (aref *damage* tgt) (attack-damage i tgt)))))
(remove-dead-soldiers)
```

La portée de chaque type fait émerger le combat de formation :

- **Piquiers :** une portée d'environ trois ou quatre rangs, donc plusieurs rangs frappent ensemble.
- **Épéistes et rondachiers :** une portée courte, donc seul le premier rang frappe, mais plus fort.
- **Cavalerie :** un bonus d'élan au premier impact, et une pénalité si elle charge des piques de face.

### Au niveau du régiment

- **Poussée :** soldats engagés × poids × moral, comparé entre régiments au contact. Le perdant recule de quelques pixels par seconde.
- **Moral :** il baisse avec les pertes, les flancs exposés et les charges. Sous un seuil, le régiment se débande.

### Tir des mousquetaires, sans projectiles

- **Salve par rang, puis contremarche :** le rang qui a tiré passe à l'arrière pour recharger.
- **Pertes :** tireurs × probabilité de toucher selon la distance × part de rayons dégagés (section Terrain), réparties sur les ennemis les plus proches.
- **Effets visuels :** fumée et éclairs sont décoratifs, jusqu'à l'ajout éventuel d'une grille de fumée.

### Déterminisme

Pas fixe, générateur aléatoire à graine et phase 2 séquentielle permettent de rejouer exactement une bataille. C'est utile pour déboguer et comparer les performances, et plus tard pour un mode multijoueur en lockstep, très difficile à ajouter après coup.

## Terrain et ligne de tir

Un tir est possible si la ligne entre l'œil du tireur et la cible passe au-dessus de tout ce qui se trouve entre eux. Collines, piquiers et murs deviennent tous des hauteurs le long de cette ligne. Sur une colline, le tireur voit donc naturellement par-dessus ses piquiers.

**Deux grilles :**

- **Terrain :** une carte d'élévation, avec une case tous les 2 m, fixe pendant la bataille. Bois et murs y ajoutent leur hauteur.
- **Occupants :** reconstruite à chaque tick depuis la grille spatiale. Chaque case reçoit la hauteur du sommet de ce qui l'occupe (terrain + 1,8 m par soldat) et l'identifiant de son régiment.

```lisp
(defconstant +eye+ 1.6f0)     ; hauteur de l'œil du tireur
(defconstant +aim+ 1.2f0)     ; hauteur visée sur la cible

(defun line-of-fire-p (x0 y0 x1 y1 shooter-reg target-reg)
  (declare (optimize (speed 3) (safety 1))
           (type single-float x0 y0 x1 y1) (type fixnum shooter-reg target-reg))
  (let* ((h0 (+ (terrain-height x0 y0) +eye+))
         (h1 (+ (terrain-height x1 y1) +aim+))
         (steps (max 1 (ceiling (distance x0 y0 x1 y1) +cell-size+))))
    (loop for k from 1 below steps
          for u = (/ (float k) steps)
          for x = (+ x0 (* u (- x1 x0)))
          for y = (+ y0 (* u (- y1 y0)))
          for line-h = (+ h0 (* u (- h1 h0)))
          for cell = (cell-index x y)
          for reg = (aref *occupant-regiment* cell)
          never (or (> (aref *terrain* cell) line-h)              ; crête de colline
                    (and (/= reg shooter-reg) (/= reg target-reg) ; autre régiment
                         (> (aref *occupant-top* cell) line-h))))))
```

Le régiment du tireur et celui de la cible sont ignorés. Tous les autres, amis ou ennemis, bloquent le tir s'ils dépassent la ligne.

| Situation | Résultat attendu |
| --- | --- |
| Mousquetaires derrière des piquiers, terrain plat | Bloqué |
| Mousquetaires sur une colline derrière les piquiers | Possible |
| Ennemi caché derrière une crête | Bloqué, c'est du terrain mort |
| Ennemi en contrebas d'une pente douce | Possible |

**Coût maîtrisé :**

- **Rayons par régiment :** 5 à 9 rayons répartis le long du premier rang, chacun vers le point le plus proche de la cible.
- **Tir partiel :** la part de rayons dégagés donne la part de tireurs. Un régiment à moitié masqué tire à moitié.
- **Au bon moment :** le test se fait au moment de la salve ou du choix de cible seulement.

**Paramètres à exposer :**

- **Hauteur des piquiers selon leur état :** piques levées à l'arrêt, donc plus hautes que piques baissées en marche.
- **Bois :** réduisent la probabilité de toucher au lieu de bloquer.
- **Fumée, plus tard :** une grille qui s'accumule à chaque salve, se dissipe lentement et réduit la précision.
- **Tir depuis une hauteur :** léger bonus de portée ou de précision.

**Le même terrain sert partout :**

- **Mouvement :** montées plus lentes, descentes légèrement plus rapides.
- **Charges :** bonus en descente, pénalité en montée.
- **Moral :** bonus pour un régiment qui tient une hauteur.
- **IA et brouillard de guerre :** ils réutilisent `line-of-fire-p` pour évaluer les positions et la visibilité.

## Rendu 2D et mode topographique

En vue de dessus, toute l'élévation passe par la couleur et l'ombrage, et la position à l'écran reste la position au sol.

**Soldats :**

- **Un sprite par type d'unité,** pivoté selon l'orientation du régiment avec `DrawTexturePro`.
- **Un atlas de textures** pour tous les sprites, afin que raylib regroupe les dessins.
- **Animation minimale :** deux ou trois images par action au plus.
- **Élimination hors champ :** les régiments hors de la caméra ne sont pas dessinés, par test de boîte englobante.
- **Vue éloignée :** au dézoom, chaque régiment devient un rectangle coloré avec son drapeau.

**Fond de carte :**

- **Ombrage du relief, calculé une fois au chargement :** pente par différences entre cases voisines, lumière venant du nord-ouest à 45 degrés, résultat enregistré dans une texture. Le fond ne coûte qu'un appel de dessin par image.
- **Ombre portée** sous chaque régiment, décalée dans le sens de la lumière.

**Mode topographique, chaque élément activable séparément :**

- **Teintes d'altitude :** un dégradé du vert des creux au brun des sommets.
- **Courbes de niveau par shader** (`shaders/topo.fs`) : une ligne tous les 5 m, plus marquée tous les 25 m, nette à tous les zooms.
- **Altitude au survol,** avec la différence par rapport au régiment sélectionné.

**Aides au joueur :**

- **Zone de tir :** avec des mousquetaires sélectionnés, les zones masquées sont colorées en rouge, calculées avec `line-of-fire-p`.
- **Indicateur de hauteur :** « en hauteur » ou « en contrebas » par rapport à la cible du régiment.

## Console de jeu reliée à Claude Code

Une console dans le jeu envoie tes messages à la session Claude Code déjà ouverte dans ton terminal, avec le contexte de ce que tu regardes. Elle passe par un [channel](https://code.claude.com/docs/en/channels-reference) plutôt que par le mode non interactif, car la session survit ainsi aux plantages du jeu.

**Contraintes des channels :**

- **Aperçu de recherche :** Claude Code v2.1.80 ou plus récent ; les organisations Team et Enterprise doivent activer la fonction.
- **Transport stdio :** Claude Code lance lui-même le serveur du channel. Le serveur MCP HTTP du jeu ne peut donc pas jouer ce rôle.
- **Lancement :** pendant l'aperçu, démarrer Claude Code avec `claude --dangerously-load-development-channels server:game-console`.

**Le pont `bridge/` :**

- **Serveur MCP stdio** qui déclare la capacité `claude/channel`. Il s'écrit en TypeScript avec le SDK MCP officiel, comme les exemples de la documentation, et s'exécute avec Bun (`brew install bun`).
- **HTTP local** sur `127.0.0.1:7778`, avec le jeton `LISP_GAME_MCP_TOKEN`.
- **`POST /message` :** reçoit un message du jeu et le transmet par une notification `notifications/claude/channel`.
- **Outil `reply` :** Claude Code l'appelle pour répondre, et le pont garde la réponse dans une file.
- **`GET /replies` :** le jeu récupère les réponses en attente.

```json
"game-console": {
  "type": "stdio",
  "command": "bun",
  "args": ["run", "bridge/server.ts"],
  "env": { "LISP_GAME_MCP_TOKEN": "${LISP_GAME_MCP_TOKEN}" }
}
```

Cette entrée s'ajoute à `mcpServers` dans `.mcp.json`, à côté du serveur `lisp-game`.

**Déroulement d'un échange :**

1. Tu ouvres la console avec la touche `` ` ``, et le jeu se met en pause.
2. Un thread secondaire du jeu envoie le message et son contexte au pont.
3. Le message arrive dans la session dans une balise `<channel>`. Si Claude Code est occupé, il attend le tour suivant.
4. Claude Code inspecte avec les outils du jeu, modifie le code, puis appelle `reply`.
5. Le thread secondaire interroge `GET /replies` et la console affiche la réponse.

**Contexte joint automatiquement :** le régiment ou la case désignés à la souris, le tick, l'état de pause et, en option, le chemin d'une capture prise à l'envoi.

```text
<channel source="game-console" regiment="12" type="piquiers" case="40,22" tick="18342" paused="true">
Ce régiment recule trop vite quand il est poussé.
</channel>
```

**Interface unique côté jeu :** la console ne connaît que `send-message` et une file de réponses. Si l'aperçu des channels change, le pont peut être remplacé par le [mode non interactif](https://docs.claude.com/en/docs/claude-code/headless) (`claude -p --input-format stream-json --output-format stream-json`) sans toucher à la console.

**Sécurité :** tout ce qui arrive par le channel devient une instruction pour Claude Code. Le pont refuse toute requête sans jeton et n'écoute que sur `127.0.0.1`. Les demandes de permission restent approuvées dans le terminal.

## Risques et points à vérifier

Le principal risque est la sécurité de `mcp-lisp`, car `game_eval` exécute du code arbitraire et sa documentation ne précise ni l'adresse d'écoute ni l'authentification. Les étapes 1 et 2 servent aussi à vérifier les points de cette table.

| Risque | Pourquoi | Réponse prévue |
| --- | --- | --- |
| Serveur exposé au réseau | Le [README de mcp-lisp](https://github.com/jsulmont/mcp-lisp) montre `:port`, mais aucune option d'adresse ni de jeton | Contrôler avec `lsof` et des requêtes réelles avant d'ajouter `game_eval` |
| `mcp-lisp` trop jeune | Projet de 9 étoiles, hors Quicklisp ; son nom de transport `:sse` désigne en fait du Streamable HTTP | Lire son code et ses exemples ; garder les outils indépendants de cette bibliothèque |
| `cl-raylib` incompatible | Le [README](https://awesome.ecosyste.ms/projects/github.com%2Flonglene%2Fcl-raylib) n'indique pas de version de raylib, et l'index consulté montre un dernier push en novembre 2024 | Tester à l'étape 1 et comparer les signatures à la version installée |
| Coût des appels CFFI pour des milliers de sprites | Chaque `DrawTexturePro` traverse CFFI | Mesurer à l'étape 7 ; en dernier recours, une fonction C qui dessine un régiment en un appel |
| Pauses du ramasse-miettes en bataille | Le ramasse-miettes de SBCL arrête tous les threads | Zéro allocation par tick ; collecte complète entre les batailles |
| Simulation non déterministe | L'ordre d'application des dégâts ou un `random` sans graine change le résultat | Phase 2 séquentielle, graine unique, test de hachage à l'étape 9 |
| Plantage graphique sur macOS | L'interface graphique doit tourner sur le thread principal ; l'[exemple cl-glfw3](https://github.com/AlexCharlton/cl-glfw3/issues/10) échoue quand SLIME la lance ailleurs | Démarrer avec `sbcl --load`, ne jamais ouvrir la fenêtre depuis un thread d'outil |
| Erreurs de virgule flottante | Cocoa peut [déclencher un SIGFPE](https://github.com/gregcman/sucle/blob/886e7bd20d5e21b19c16f94b41294549b30ae729/src/window-opengl-glfw3/window.lisp) capté par SBCL | Masquer les interruptions autour de la boucle et contrôler les `NaN` dans `update` |
| Jeu ralenti quand la fenêtre est cachée | Hypothèse à vérifier : macOS peut réduire le rythme des images, ce qui retarde la file | Tester à l'étape 4 ; si cela se confirme, garder la fenêtre visible ou limiter l'attente de synchronisation |
| Channels en aperçu de recherche | La fonction peut changer ou ne pas être activée pour ton compte | Interface unique côté jeu ; mode non interactif en repli |
| Modifications perdues | Le code envoyé par `game_eval` disparaît au redémarrage | Écrire tout changement durable dans `src/`, puis utiliser `game_reload` |
| Serveur absent au démarrage de Claude Code | La connexion échoue si le jeu n'est pas lancé | Lancer `dev.sh start` d'abord, ou reconnecter avec `/mcp` |

**Solution de repli pour MCP :** si `mcp-lisp` ou Woo bloque, écrire un petit serveur Hunchentoot lié à `127.0.0.1`. Il gère `initialize`, `tools/list` et `tools/call` en JSON-RPC, vérifie le jeton et l'en-tête `Origin`, et réutilise la même file de tâches.

**À éviter :** `trivial-main-thread`. Un développeur de SBCL [le déconseille sur r/lisp](https://axebps-redlib.hf.space/r/lisp/comments/1gxekrj/repl_hangs_after_exit/lylieg4/?context=3), et le lancement par `--load` n'en a pas besoin.

## Consignes pour Claude Code

Exporte ce document en Markdown (menu du nom du document, puis Export), donne-le à Claude Code avec le message ci-dessous, puis fais copier le bloc suivant dans `CLAUDE.md` à l'étape 6.

```text
Implémente ce plan étape par étape, dans un nouveau dépôt git nommé lisp-game.
Enregistre d'abord ce plan dans docs/plan.md et lis-le en entier.
Vérifie les prérequis et signale ce qui manque avant d'installer quoi que ce soit.
Après chaque étape, exécute sa validation, fais un commit et résume le résultat.
Arrête-toi aux conditions d'arrêt et présente-moi les options.
N'ajoute pas game_eval tant que les contrôles de sécurité de l'étape 2 échouent.
À partir de l'étape 7, respecte les sections Performance, Données, Simulation, Terrain, Rendu et Console.
```

```markdown
# lisp-game

Jeu de batailles 2D vu de dessus, époque pike and shock, en Common Lisp (SBCL, raylib), modifiable à chaud par MCP.
Le plan et les règles de conception sont dans docs/plan.md : le lire avant toute implémentation.

## Démarrage
- `scripts/dev.sh start|stop|restart|logs` gère le jeu dans tmux.
- Le jeu doit tourner avant d'utiliser les outils MCP. Sinon, reconnecter avec `/mcp`.

## Règles de développement
- Écrire tout changement durable dans `src/`, puis appeler `game_reload`.
- Réserver `game_eval` à l'exploration et au diagnostic.
- Ne jamais appeler raylib hors du thread principal : passer par la file de tâches.
- Ne pas réinitialiser `*state*` sans le dire.
- Masquer les float traps uniquement autour de la boucle principale.

## Performance
- Nouveau système : CLOS, sans déclarations. Système stabilisé et chaud : tableaux typés et déclarations.
- Code chaud : `(optimize (speed 3) (safety 1))`, `single-float`, `fixnum`. Jamais `(safety 0)`.
- Zéro allocation par tick dans les systèmes de soldats ; vérifier avec `time`.
- Calculer par régiment tout ce qui peut l'être ; par soldat, seulement le mouvement local et la frappe.
- Mesurer avec `game_benchmark` avant et après toute optimisation, et noter les résultats dans `docs/mesures.md`.

## Simulation
- Pas fixe de 30 Hz ; tout tirage aléatoire passe par le générateur à graine du jeu.
- Combat en deux phases : ciblage en lecture seule, puis application séquentielle.
- Pas de hiérarchie de classes de soldats : un type d'unité est un profil de données.

## Vérifier un changement
1. Appeler `game_reload` et lire les avertissements.
2. Logique : `game_control` en pause, `game_input`, `step`, puis `game_state`.
3. Visuel : `game_screenshot`, puis ouvrir le PNG.
4. Tir : `game_line_of_fire`.
5. Performance : `game_benchmark`.
6. Corriger toute erreur signalée dans `*last-error*` avant de continuer.

## Console du jeu
- Les messages du joueur arrivent dans des balises `<channel source="game-console">` avec le contexte du jeu.
- Répondre avec l'outil `reply`, en une ou deux phrases ; le détail reste dans le terminal.

## Sécurité
- Serveur du jeu et pont : uniquement 127.0.0.1, jeton LISP_GAME_MCP_TOKEN exigé.
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
- [Référence des channels](https://code.claude.com/docs/en/channels-reference) : capacité `claude/channel`, transport stdio, outil de réponse
- [Mode non interactif de Claude Code](https://docs.claude.com/en/docs/claude-code/headless) : solution de repli pour la console
