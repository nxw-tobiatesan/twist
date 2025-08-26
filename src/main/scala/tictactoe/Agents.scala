package com.tobiatesan.twist.tictactoe



import scala.util.Random

trait Agent {
  def selectMove(game: Game): Move
}

object RandomAgent extends Agent {
  def selectMove(game: Game): Move = {
    val moves = game.board.availableMoves
    moves(Random.nextInt(moves.size))
  }
}

object MinimaxAgent extends Agent {
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

object MCTSAgent extends Agent {
  def selectMove(game: Game): Move = {
    // Simple Monte Carlo: simulate each move N times, pick the one with most wins
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
