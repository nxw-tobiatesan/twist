package com.tobiatesan.twist.ai.gpt



import com.tobiatesan.twist.tictactoe._

object GPTMinimaxAgent extends GPTAgent {
  def selectMove(game: Game): Move = {
    val maximizing = game.current == X
    val moves = game.board.availableMoves
    moves.maxBy(move => minimax(game.play(move), maximizing, 0))
  }

  private def minimax(game: Game, maximizing: Boolean, depth: Int): Int = {
    game.winner match {
      case Some(X) => 10 - depth
      case Some(O) => depth - 10
      case None if game.isDraw => 0
      case None =>
        val scores = game.board.availableMoves.map { move =>
          minimax(game.play(move), !maximizing, depth + 1)
        }
        if (maximizing) scores.max else scores.min
    }
  }
}
