# À sourcer (. scripts/token.sh) après avoir défini ROOT.
# Exporte LISP_GAME_MCP_TOKEN : celui de l'environnement s'il est défini,
# sinon celui de .mcp-token, créé au premier usage (lisible par toi seul,
# ignoré par git). Le jeu et Claude Code doivent partager le même jeton.

TOKEN_FILE="$ROOT/.mcp-token"

if [ -z "${LISP_GAME_MCP_TOKEN:-}" ]; then
  if [ ! -s "$TOKEN_FILE" ]; then
    (umask 077 && openssl rand -hex 32 > "$TOKEN_FILE")
    echo "Jeton MCP créé dans $TOKEN_FILE."
  fi
  LISP_GAME_MCP_TOKEN=$(cat "$TOKEN_FILE")
fi
export LISP_GAME_MCP_TOKEN
