package com.tobiatesan.twist.tictactoe



import scala.io.StdIn

object Main extends App {
  println("Welcome to Tic Tac Toe!")
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
      val input = StdIn.readLine().split(" ").map(_.trim)
      if (input.length == 2 && input.forall(_.forall(_.isDigit))) {
        val row = input(0).toInt
        val col = input(1).toInt
        if (row >= 0 && row <= 2 && col >= 0 && col <= 2 && game.board(row, col) == Empty) {
          move = Move(row, col)
          valid = true
        } else println("Invalid move, try again.")
      } else println("Invalid input, try again.")
    }
    move
  }

  def randomAgentMove(): Move = {
    val moves = game.board.availableMoves
    moves(scala.util.Random.nextInt(moves.size))
  }

  def playGame(humanX: Boolean, humanO: Boolean): Unit = {
    game = Game()
    while (game.winner.isEmpty && !game.isDraw) {
      printBoard(game.board)
      val move = (game.current match {
        case X if humanX => humanMove()
        case O if humanO => humanMove()
        case _ => randomAgentMove()
      })
      game = game.play(move)
    }
    printBoard(game.board)
    game.winner match {
      case Some(mark) => println(s"$mark wins!")
      case None => println("Draw!")
    }
  }

  println("Choose mode: 1) Human vs Human  2) Human vs RandomAgent  3) RandomAgent vs Human  4) RandomAgent vs RandomAgent")
  val mode = StdIn.readLine().trim
  mode match {
    case "1" => playGame(humanX = true, humanO = true)
    case "2" => playGame(humanX = true, humanO = false)
    case "3" => playGame(humanX = false, humanO = true)
    case "4" => playGame(humanX = false, humanO = false)
    case _ => println("Invalid mode.")
  }
}
