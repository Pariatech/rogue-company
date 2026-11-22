#!/bin/sh
# Lance le jeu s'il ne tourne pas, puis Claude Code avec le channel de la
# console du jeu. Les arguments sont passés à claude (ex. --continue).
# Le travail est fait par le claude.sh de game-kit ($GAME_KIT_DIR, sinon ../game-kit).
set -eu
ROOT=$(cd "$(dirname "$0")/.." && pwd)
GAME_KIT_DIR=${GAME_KIT_DIR:-$ROOT/../game-kit}
APP_ROOT=$ROOT GAME_KIT_SESSION=rogue-company exec "$GAME_KIT_DIR/scripts/claude.sh" "$@"
