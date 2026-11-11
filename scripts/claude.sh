#!/bin/sh
# Lance le jeu s'il ne tourne pas, puis Claude Code avec le channel de la
# console du jeu. Les arguments sont passés à claude (ex. --continue).
# Usage : scripts/claude.sh [arguments de claude]
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

if [ ! -d bridge/node_modules ]; then
  echo "Installation des dépendances du pont (bun install)..."
  (cd bridge && bun install)
fi

# Jeton partagé par le jeu et Claude Code (voir scripts/token.sh).
. scripts/token.sh

# Ne fait rien si le jeu tourne déjà.
scripts/dev.sh start

# Le serveur MCP démarre avec le jeu : l'attendre pour que Claude Code s'y
# connecte du premier coup.
tries=0
until curl -s -o /dev/null --max-time 1 http://127.0.0.1:7777/mcp; do
  tries=$((tries + 1))
  if [ "$tries" -ge 60 ]; then
    echo "Le serveur MCP ne répond pas après 60 s. Voir : scripts/dev.sh logs" >&2
    exit 1
  fi
  sleep 1
done

# Un jeu lancé avec un autre jeton refuserait Claude Code (401).
status=$(curl -s -o /dev/null -w '%{http_code}' --max-time 1 \
  -H "Authorization: Bearer $LISP_GAME_MCP_TOKEN" http://127.0.0.1:7777/mcp)
if [ "$status" = 401 ]; then
  echo "Le jeu tourne avec un autre jeton. Relance-le : scripts/dev.sh restart" >&2
  exit 1
fi

exec claude --dangerously-load-development-channels server:game-console "$@"
