# Mesures

## Étape 7 : banc d'essai de rendu (2026-11-11)

**Machine :** Apple M4 Pro (Mac16,7), macOS 27.0.1, SBCL 2.6.10, raylib 6.2, cl-raylib `23a7da7`, CFFI `787d3cd` (git).

**Scène :** 10 000 sprites de 16 × 16 px, tirés d'un atlas de 4 cases de 32 × 32 px, chacun pivoté avec sa propre vitesse, sur une fenêtre de 800 × 600. Mesure sur 5 s après 1 s de chauffe, sans limite d'images par seconde, fenêtre visible et console fermée. Les chiffres viennent de `game_benchmark`.

| Variante | `draw` moyen | `draw` 99e centile | Image moyenne | Image 99e centile | FPS | Octets alloués par image |
| --- | --- | --- | --- | --- | --- | --- |
| `pro` (`DrawTexturePro`) | 82,2 ms | 88,8 ms | 85,4 ms | 93,7 ms | 11,8 | 5,6 Mo |
| `rlgl` (sommets par `rlgl`) | **0,76 ms** | **0,89 ms** | 2,17 ms | 8,90 ms | 453,6 | 1,2 ko en moyenne |

Les deux variantes produisent la même image (captures comparées avec 300 sprites).

### Conclusion

- **`DrawTexturePro` via cl-raylib ne convient pas :** 82 ms pour 10 000 sprites, dix fois le budget de rendu de 8 ms.
- **La variante `rlgl` tient largement le budget :** 0,76 ms de dessin côté CPU, soit environ 10 % du budget. C'est la méthode retenue pour dessiner les soldats ; la fonction C prévue en dernier recours n'est pas nécessaire.
- **`end_drawing`** (envoi du lot au GPU et échange des tampons) coûte 1,4 ms en moyenne, avec des pointes à 8–9 ms au 99e centile. Ces pointes viennent probablement de la synchronisation avec l'affichage de macOS, et sont à surveiller à l'étape 9 avec la simulation.

### Cause de la lenteur de `pro`

`game_profile` sur la variante `pro` attribue 99 % des échantillons à `dlsym`, appelé depuis `CL-RAYLIB:DRAW-TEXTURE-PRO`. Pour les fonctions qui prennent des structures par valeur, CFFI passe par libffi et cherche le symbole C à chaque appel (`cffi/libffi/funcall.lisp`, ligne 117 : `foreign-symbol-pointer`). C'est cette recherche qui coûte, pas la conversion des structures, qui alloue tout de même environ 560 octets par sprite.

Conséquence pour la suite : **éviter, dans le code chaud, toute fonction raylib qui prend ou renvoie une structure par valeur** (`Rectangle`, `Vector2`, `Color` en structure, `Texture`). Les fonctions `rlgl` à arguments scalaires passent par la table de liens de SBCL, sans recherche ni allocation.

## Allocations par image (2026-11-12)

Les 1,2 ko par image restants de la variante `rlgl` venaient de deux sources, mesurées appel par appel avec `get-bytes-consed` :

| Source | Octets par appel |
| --- | --- |
| `raylib:clear-background`, `raylib:draw-rectangle`, `raylib:draw-text` (couleur passée par valeur) | 820 à 950 |
| dont la recherche du symbole C (`foreign-symbol-pointer`) | 334 |
| `console-draw`, console fermée (mise en page de la saisie calculée quand même) | 295 |
| `raylib:draw-text` : conversion de la chaîne en chaîne C | 16 |

Corrections :

- **`src/draw.lisp`** redéclare `ClearBackground`, `DrawRectangle`, `DrawLine` et `DrawText` avec la couleur en `:uint32`. Une `Color` de 4 octets passe dans un registre entier sur arm64 comme sur x86-64 : l'appel passe alors par la table de liens de SBCL, sans libffi. Couleurs vérifiées sur capture (mots-clés et listes RGBA).
- **`draw-text`** écrit le texte dans un tampon C préalloué avec `lisp-string-to-foreign` (UTF-8 compris).
- **`console-draw`** ne calcule plus rien quand la console est fermée.

Résultat : 0 octet par appel pour chacune de ces fonctions, et aucune zone de 64 ko remplie par le thread principal en 8 s (profil `sb-sprof` en mode `:alloc`, contre 36 avant).

| Variante | `draw` moyen | `draw` 99e centile | Image moyenne | FPS | Images qui allouent |
| --- | --- | --- | --- | --- | --- |
| `rlgl` | 0,68 ms | 0,76 ms | 1,64 ms | 617,8 | 1 sur 1 200 (médiane 0) |
| `pro` | 72,9 ms | 79,4 ms | 75,1 ms | 13,4 | 67 sur 67 (5,6 Mo) |

**Mesurer les allocations :** `get-bytes-consed` compte tous les threads, serveur MCP compris, et n'avance que par zones de 64 ko. `game_benchmark` donne donc la médiane et le nombre d'images qui allouent : une médiane à 0 indique une boucle sans allocation. Les compteurs par thread de SBCL (`thread-tot-bytes-alloc-*`) ne bougent pas en cours d'exécution et ne servent pas ici. Pour attribuer une allocation, utiliser `sb-sprof` en mode `:alloc` sur le thread principal.

## Texte de l'affichage de bataille (2026-11-12)

Le premier affichage « Tick 1234   x2 » de la scène de bataille allouait environ 77 ko/s à ×2 (profil `game_profile`, mode `alloc`) :

- **`format` avec `~,2f`** pour la vitesse : environ 1,3 ko par reconstruction, 60 fois par seconde. Le tick s'écrit désormais avec `write-integer-into` dans une chaîne préallouée, et le texte de la vitesse n'est reconstruit que quand elle change.
- **`cffi:lisp-string-to-foreign` alloue 33 octets pour une `simple-base-string`**, le type que renvoie `format` quand le texte est en ASCII. `draw.lisp` encode maintenant lui-même l'UTF-8 dans le tampon C (`encode-utf8-into`), sans allocation pour aucun type de chaîne.

Résultat : aucune zone de 64 ko remplie par le thread principal en 8 s de bataille à ×2.

## Tick de simulation, étape 8 (2026-11-14)

Une bataille de 50 régiments de 200 piquiers, soit 10 000 soldats, dispersés de 20 m autour de leur formation (`game_eval`, jeu en cours, 600 ticks après 30 ticks de chauffe) :

| Mesure | Résultat |
| --- | --- |
| Tick, soldats en marche | 170 à 176 µs |
| Octets alloués sur 600 ticks | 0 |

Un tick à 30 Hz coûte donc environ 5 ms par seconde de jeu pour 10 000 soldats, soit 0,5 % d'une image à 60 FPS par tick exécuté. Le tick ne contient encore que la copie des positions précédentes, le recalcul des emplacements des régiments qui ont reçu un ordre, et la marche vers l'emplacement.

## Collisions entre soldats (2026-11-14)

Même bataille de 10 000 piquiers, régiments espacés de 25 m et dispersés de 15 m, avec la grille spatiale (cases de 1 m, carte de 256 m) et la séparation des soldats :

| Mesure | Résultat |
| --- | --- |
| Tick, soldats dispersés qui se démêlent | 860 µs |
| Tick, soldats en place | 480 µs |
| dont reconstruction de la grille | 150 µs |
| dont collisions | 270 µs |
| Octets alloués sur 300 ticks | 0 |

Avec l'orientation par soldat (rotation limitée, marche ou arrêt), le tick des soldats dispersés qui se démêlent passe à 965 µs, toujours sans allocation.

La reconstruction de la grille parcourt ses 65 536 cases deux fois par tick, quel que soit le nombre de soldats ; c'est la première piste si elle devient gênante (ne vider que les cases occupées).

## Sprites des soldats (2026-11-14)

10 240 piquiers en 64 régiments, fenêtre 1920×1200, zoom 18 px/m (sprites à l'échelle 1, environ un tiers des soldats à l'écran) : sprite plus masque teint, deux quads par soldat visible, parcourus dans l'ordre de la grille.

| Mesure | Résultat |
| --- | --- |
| Construction du lot (`draw-soldier-sprites`) | 510 µs |
| Octets alloués | 0 |

Avec la marche (8 images, miroir pour l'ouest, image choisie par soldat) : 516 µs, toujours 0 octet.

Avec l'attaque et le porteur d'étendard (2026-11-15), mesuré par `game_eval` (20 appels, hors de la boucle du jeu) : 650 µs, 0 octet ; le dessin des 64 drapeaux, 0 octet. Comparer chaque soldat à `regiment-bearer` coûtait 44 ns par soldat, soit 0,44 ms pour 10 000 : les porteurs sont calculés une fois par image dans un tampon (`*bearers*`).

Avec les piques dessinées à part (deux quads de plus par soldat, même lot, 2026-11-15) : 820 µs, 0 octet.

Soldats en morceaux (`draw-soldier-parts`, 2026-11-16) : 64 régiments de 160, fenêtre 1920×1200 simulée, zoom 18 px/m, 20 appels par `game_eval`. Deux pieds, deux mains, corps, masque, tête et trois segments de pique : environ 10 quads par soldat visible.

| Dessin | Temps | Octets alloués |
| --- | --- | --- |
| Sprites et piques (`draw-soldier-sprites`) | 1,09 ms | 0 |
| Morceaux et piques (`draw-soldier-parts`) | 2,21 ms | 0 |

Morts (`draw-corpses`, 2026-11-16) : 4 096 morts couchés, tous à l'écran, fenêtre 3840×2130, zoom 18 px/m, environ 9 quads chacun. Mesuré dans la boucle du jeu (section `:draw`, moyenne de 60 images) : 4,7 ms avec les morts, 0,8 ms sans, soit 3,8 ms pour eux, 0 octet alloué par le dessin. Hors de la boucle, le profil montre 95 % du temps dans l'envoi des lots au GPU (`mach_msg2_trap` sous `rlVertex3f`), pas dans le code Lisp : sauter la trigonométrie des morts couchés n'a gagné que 10 %. Piste : peindre les morts couchés dans une texture du sol.

Les sprites mesurent ici 1,09 ms au lieu des 820 µs plus haut : la bataille d'essai n'est pas la même (régiments en damier, tous à l'écran à cette taille de fenêtre).

## Combat de mêlée (2026-11-14)

Deux régiments de 500 piquiers sur 8 rangs, au contact après une charge (environ 185 soldats engagés de chaque côté) : engagement, ciblage par la grille, coups et morts compris.

| Mesure | Résultat |
| --- | --- |
| Tick complet | 470 µs |
| Octets alloués sur 60 ticks | 0 |

Rythme des pertes, deux régiments de 160 au contact : environ 0,7 mort par seconde de chaque côté, la moitié d'un régiment en deux minutes. Sans moral, rien n'arrête encore le combat.
