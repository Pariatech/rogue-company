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
