package com.tobiatesan.twist.ai.gpt



import com.tobiatesan.twist.tictactoe._
import scala.util.Random

// ...existing code...

object GPTRandomAgent extends GPTAgent {
  def selectMove(game: Game): Move = {
    val moves = game.board.availableMoves
    moves(Random.nextInt(moves.size))
  }
}
