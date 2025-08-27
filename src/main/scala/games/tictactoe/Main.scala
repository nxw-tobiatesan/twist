
package com.tobiatesan.twist.tictactoe

import scala.io.StdIn

// Generic, reusable agent selection menu
class AgentMenu(label: String, options: Seq[(String, String)], default: String) {
  def select(): String = {
    println(s"Choose $label agent:")
    options.zipWithIndex.foreach { case ((name, _), idx) => println(s"  ${idx + 1}) $name") }
    val input = scala.io.StdIn.readLine().trim
  val idx = try { input.toInt - 1 } catch { case _: NumberFormatException => -1 }
    if (idx >= 0 && idx < options.length) options(idx)._2 else default
  }
}

object Main extends App {
  println("Welcome to Tic Tac Toe!")
  import com.tobiatesan.twist.ai.RandomAgent
  import com.tobiatesan.twist.ai.minimax.{MinimaxEvaluation, AlphaBetaOrdering, BasicAlphaBeta}
  import com.tobiatesan.twist.ai.mtcs.UCTAgent
  import com.tobiatesan.twist.player.AI
  import com.tobiatesan.twist.tictactoe.TicTacToeLivePosition
  var game = Game()

  def printBoard(board: Board): Unit = {
    println("  0 1 2")
    for (r <- 0 to 2) {
      print(s"$r ")
      for (c <- 0 to 2) {
        val cell = board(r, c) match {
          case X => "X"
          case O => "O"
          case Empty => "."
        }
        print(cell + " ")
      }
      println()
    }
  }

  def humanMove(): Move = {
    var valid = false
    var move: Move = null
    while (!valid) {
      println(s"${game.current} to move. Enter row and col (e.g. 0 2):")
      val inputLine = StdIn.readLine().trim
      val input = inputLine.split(" ").filter(_.nonEmpty)
      if (input.length == 2 && input.forall(s => s.forall(_.isDigit))) {
        try {
          val row = input(0).toInt
          val col = input(1).toInt
          if (row >= 0 && row <= 2 && col >= 0 && col <= 2 && game.board(row, col) == Empty) {
            move = Move(row, col)
            valid = true
          } else println("Invalid move, try again.")
        } catch {
          case _: NumberFormatException => println("Invalid input, try again.")
        }
      } else println("Invalid input, try again.")
    }
    move
  }

  def agentMove(agent: AI[Game]): Move = {
    val pos = new TicTacToeLivePosition(game)
    agent.debug(pos).move.asInstanceOf[Move]
  }

  def playGame(xType: String, oType: String): Unit = {
    game = Game()
    val random = new scala.util.Random(0)
    val minimaxEval = new MinimaxEvaluation[Game] {
      def apply(p: com.tobiatesan.twist.game.LivePosition[Game]): Double = 0.0 // TODO: Implement evaluation
    }
    val ordering = new AlphaBetaOrdering[Game] {
      def compare(x: com.tobiatesan.twist.game.Move[Game], y: com.tobiatesan.twist.game.Move[Game]): Int = 0 // TODO: Implement ordering
    }
    val minimaxAgent = new BasicAlphaBeta[Game](minimaxEval, ordering, 4)
    val mctsAgent = new UCTAgent[Game](budget = 1000, c = 1.4, maximize = false, r = random)
    val randomAgent = new RandomAgent[Game](random)

    def selectMove(mark: Mark): Move = {
      val tpe = if (mark == X) xType else oType
      tpe match {
        case "human" => humanMove()
        case "random" => agentMove(randomAgent)
        case "minimax" => agentMove(minimaxAgent)
        case "mcts" => agentMove(mctsAgent)
      }
    }
    while (game.winner.isEmpty && !game.isDraw) {
      printBoard(game.board)
      val move = selectMove(game.current)
      game = game.play(move)
    }
    printBoard(game.board)
    game.winner match {
      case Some(mark) => println(s"$mark wins!")
      case None => println("Draw!")
    }
  }

  val agentOptions = Seq(
    ("Human", "human"),
    ("RandomAgent", "random"),
    ("MinimaxAgent", "minimax"),
    ("MCTSAgent", "mcts")
  )
  val xAgent = new AgentMenu("X", agentOptions, "human").select()
  val oAgent = new AgentMenu("O", agentOptions, "human").select()
  playGame(xType = xAgent, oType = oAgent)
}
