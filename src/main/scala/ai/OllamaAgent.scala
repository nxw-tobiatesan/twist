
package com.tobiatesan.twist.ai

import com.tobiatesan.twist.player.{DebugStats, AI, MoveWithStats}
import com.tobiatesan.twist.game.{Game, LivePosition, Move}

import requests._

/**
  * Agent that uses a local LLM via Ollama to select moves.
  * @param model The Ollama model to use (e.g., "llama2", "codellama", etc.)
  * @param endpoint The Ollama API endpoint (default: http://localhost:11434)
  */
class OllamaAgent[G <: Game[G]](
    val model: String = "llama2",
    val role: String = "any",
    val endpoint: String = "http://localhost:11434"
) extends AI[G] {

  override def debug(p: LivePosition[G]): MoveWithStats[G, Move[G], DebugStats[OllamaAgent[G]]] = {
  val boardStr = p.toString.replace("\n", " ")
  val moves = p.successor().keys.toList
  val movesStr = moves.mkString(", ")
  val prompt = s"You are playing a board game. You are playing as: $role. The board is: $boardStr Available moves: $movesStr Pick the best move and reply ONLY with the move in the format given."

  val requestBody = s"""{\"model\":\"$model\",\"prompt\":\"$prompt\"}"""
    val resp = requests.post(s"$endpoint/api/generate",
      data = requestBody,
      headers = Seq("Content-Type" -> "application/json")
    )
    val responseBody = resp.text()
    // Simple extraction of the "response" field from Ollama's JSON
    val moveStr = """"response":""".r.findFirstMatchIn(responseBody)
      .map(m => {
        val after = responseBody.substring(m.end)
        val quoteIdx = after.indexOf('"')
        if (quoteIdx >= 0) after.substring(0, quoteIdx).trim else ""
      }).getOrElse("")

    // Try to match the move string to one of the available moves
    val selectedMove = moves.find(m => m.toString == moveStr).getOrElse(moves.head)

    MoveWithStats[G, Move[G], DebugStats[OllamaAgent[G]]](
      selectedMove,
      new DebugStats[OllamaAgent[G]] { def getNodes = 1 }
    )
  }
}
