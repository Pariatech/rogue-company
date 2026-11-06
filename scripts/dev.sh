#!/bin/sh
# Démarre, arrête et affiche le jeu dans une session tmux.
# Usage : scripts/dev.sh start|stop|restart|logs [lignes]
set -eu

SESSION=rogue-company
ROOT=$(cd "$(dirname "$0")/.." && pwd)

running() { tmux has-session -t "$SESSION" 2>/dev/null; }

start() {
  if running; then
    echo "Déjà lancé (session tmux $SESSION)."
    return
  fi
  if [ -z "${LISP_GAME_MCP_TOKEN:-}" ]; then
    echo "LISP_GAME_MCP_TOKEN n'est pas défini. Exemple :" >&2
    echo '  export LISP_GAME_MCP_TOKEN=$(openssl rand -hex 32)' >&2
    exit 1
  fi
  tmux new-session -d -s "$SESSION" -c "$ROOT" -x 200 -y 50 \
    -e "LISP_GAME_MCP_TOKEN=$LISP_GAME_MCP_TOKEN" \
    "sbcl --dynamic-space-size 4GB --load scripts/run.lisp"
  # Garde la console lisible si SBCL s'arrête sur une erreur.
  tmux set-option -t "$SESSION" remain-on-exit on >/dev/null
  echo "Lancé dans la session tmux $SESSION. Console : scripts/dev.sh logs"
}

stop() {
  if running; then
    tmux kill-session -t "$SESSION"
    echo "Arrêté."
  else
    echo "Pas lancé."
  fi
}

logs() {
  if running; then
    tmux capture-pane -p -t "$SESSION" -S "-${1:-100}"
  else
    echo "Pas lancé."
  fi
}

case "${1:-}" in
  start) start ;;
  stop) stop ;;
  restart) stop; start ;;
  logs) logs "${2:-100}" ;;
  *) echo "Usage : $0 start|stop|restart|logs [lignes]" >&2; exit 2 ;;
esac
