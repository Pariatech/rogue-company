#!/usr/bin/env bun
// Pont du channel game-console. Claude Code lance ce serveur MCP en stdio.
// Il relève les messages de la console du jeu (GET /console/outbox sur le
// serveur du jeu, avec le jeton) et les transmet à la session en notifications
// claude/channel. Claude répond avec l'outil game_console_reply du serveur du
// jeu : ce pont est à sens unique et n'ouvre aucun port.
//
// stdout porte le protocole MCP : tout journal va sur stderr.

import { Server } from '@modelcontextprotocol/sdk/server/index.js'
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js'

const GAME_URL = process.env.GAME_CONSOLE_URL ?? 'http://127.0.0.1:7777/console/outbox'
const POLL_MS = 300
const TOKEN = process.env.LISP_GAME_MCP_TOKEN ?? ''

if (TOKEN.length < 32) {
  console.error('game-console : LISP_GAME_MCP_TOKEN absent ou trop court (32 caractères minimum).')
  process.exit(1)
}
if (!/^http:\/\/(127\.0\.0\.1|localhost)(:\d+)?\//.test(GAME_URL)) {
  console.error(`game-console : URL refusée, le jeu doit être local : ${GAME_URL}`)
  process.exit(1)
}

type OutboxMessage = { id: string; text: string; meta: Record<string, string> }

const mcp = new Server(
  { name: 'game-console', version: '0.1.0' },
  {
    capabilities: { experimental: { 'claude/channel': {} } },
    instructions:
      'Les messages tapés par le joueur dans la console du jeu rogue-company arrivent en ' +
      '<channel source="game-console" chat_id="..." frame="..." paused="..." mouse_x="..." mouse_y="...">. ' +
      'frame est l\'image courante, paused l\'état de pause (console : mis en pause par la console), ' +
      'mouse_x et mouse_y la position de la souris dans la fenêtre. ' +
      'Inspecte le jeu avec les outils game_* si besoin, puis réponds avec l\'outil game_console_reply ' +
      'du serveur rogue-company, en passant le chat_id de la balise, en une ou deux phrases : ' +
      'le détail reste dans le terminal.',
  },
)

// Sans session Claude Code au bout, le pont n'a plus de raison d'exister.
mcp.onclose = () => process.exit(0)
process.stdin.on('end', () => process.exit(0))

// Les messages ne sont relevés qu'après la poignée de main MCP : avant, une
// notification arriverait avant que Claude Code écoute le channel.
mcp.oninitialized = () => {
  void loop()
}

await mcp.connect(new StdioServerTransport())

let reachable = true

async function poll(): Promise<void> {
  try {
    const response = await fetch(GAME_URL, {
      headers: { Authorization: `Bearer ${TOKEN}` },
      signal: AbortSignal.timeout(2000),
    })
    if (!response.ok) throw new Error(`HTTP ${response.status}`)
    const messages = (await response.json()) as OutboxMessage[]
    if (!reachable) console.error('game-console : jeu de nouveau joignable.')
    reachable = true
    for (const message of messages) {
      await mcp.notification({
        method: 'notifications/claude/channel',
        params: { content: message.text, meta: message.meta },
      })
    }
  } catch (error) {
    // Jeu arrêté ou en cours de redémarrage : on réessaie sans bruit.
    if (reachable) console.error(`game-console : jeu injoignable (${error}), nouvel essai en continu.`)
    reachable = false
  }
}

async function loop(): Promise<void> {
  for (;;) {
    await poll()
    await Bun.sleep(POLL_MS)
  }
}
