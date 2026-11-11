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

### Reste à faire

- La variante `rlgl` alloue encore environ 1,2 ko par image en moyenne, hors de sa boucle (le maximum de 64 ko correspond à une région d'allocation de SBCL). À localiser avant de viser zéro allocation à l'étape 8.
