package com.tobiatesan.twist.tictactoe



import scala.io.StdIn

object Main extends App {
  println("Welcome to Tic Tac Toe!")
  import com.tobiatesan.twist.tictactoe.{RandomAgent, MinimaxAgent, MCTSAgent, Agent}
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

  def agentMove(agent: Agent): Move = agent.selectMove(game)

  def playGame(xType: String, oType: String): Unit = {
    game = Game()
    def selectMove(mark: Mark): Move = {
      val tpe = if (mark == X) xType else oType
      tpe match {
        case "human" => humanMove()
        case "random" => agentMove(RandomAgent)
        case "minimax" => agentMove(MinimaxAgent)
        case "mcts" => agentMove(MCTSAgent)
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

  println("Choose X agent: 1) Human  2) RandomAgent  3) MinimaxAgent  4) MCTSAgent")
  val xAgent = StdIn.readLine().trim match {
    case "1" => "human"
    case "2" => "random"
    case "3" => "minimax"
    case "4" => "mcts"
    case _ => "human"
  }
  println("Choose O agent: 1) Human  2) RandomAgent  3) MinimaxAgent  4) MCTSAgent")
  val oAgent = StdIn.readLine().trim match {
    case "1" => "human"
    case "2" => "random"
    case "3" => "minimax"
    case "4" => "mcts"
    case _ => "human"
  }
  playGame(xType = xAgent, oType = oAgent)
}
