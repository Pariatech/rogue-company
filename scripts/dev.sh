#!/bin/sh
# Démarre, arrête et affiche le jeu dans la session tmux rogue-company.
# Usage : scripts/dev.sh start|stop|restart|logs [lignes]
# Le travail est fait par le dev.sh de game-kit ($GAME_KIT_DIR, sinon ../game-kit).
set -eu
ROOT=$(cd "$(dirname "$0")/.." && pwd)
GAME_KIT_DIR=${GAME_KIT_DIR:-$ROOT/../game-kit}
APP_ROOT=$ROOT GAME_KIT_SESSION=rogue-company exec "$GAME_KIT_DIR/scripts/dev.sh" "$@"
