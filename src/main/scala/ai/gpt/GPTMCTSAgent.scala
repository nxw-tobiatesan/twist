package com.tobiatesan.twist.ai.gpt



import com.tobiatesan.twist.tictactoe._
import scala.util.Random

object GPTMCTSAgent extends GPTAgent {
  def selectMove(game: Game): Move = {
    val moves = game.board.availableMoves
    val N = 100
    val mark = game.current
    val scores = moves.map { move =>
      var wins = 0
      for (_ <- 1 to N) {
        if (simulate(game.play(move), mark)) wins += 1
      }
      (move, wins)
    }
    scores.maxBy(_._2)._1
  }

  private def simulate(game: Game, mark: Mark): Boolean = {
    var g = game
    while (g.winner.isEmpty && !g.isDraw) {
      val moves = g.board.availableMoves
      val move = moves(Random.nextInt(moves.size))
      g = g.play(move)
    }
    g.winner.contains(mark)
  }
}
