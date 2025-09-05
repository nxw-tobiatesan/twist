

package com.tobiatesan.twist.tictactoe

object TicTacToeSample extends App {
  import com.tobiatesan.twist.tictactoe.{Game, Move, Board, Mark, X, O, Empty}
  import com.tobiatesan.twist.games.GamePlayer

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

  def humanMove(game: Game, board: Board): Move = {
    var valid = false
    var move: Move = null
    while (!valid) {
      print("Enter your move as row,col (e.g. 1,2): ")
      val input = scala.io.StdIn.readLine().trim
      val parts = input.split(",").map(_.trim)
      if (parts.length == 2) {
        try {
          val row = parts(0).toInt
          val col = parts(1).toInt
          if (row >= 0 && row <= 2 && col >= 0 && col <= 2 && board(row, col) == Empty) {
            move = Move(row, col)
            valid = true
          } else {
            println("Invalid move. Try again.")
          }
        } catch {
          case _: NumberFormatException => println("Invalid input. Try again.")
        }
      } else {
        println("Invalid format. Try again.")
      }
    }
    move
  }

  // Dummy agent: picks first available move
  def agentMove(agentType: String, game: Game): Move = {
    game.board.availableMoves.head
  }

  val agentOptions = Seq(
    ("Human", "human"),
    ("Random", "random")
  )

  def agentSelector(tpe: String): String = tpe // Only "human" and "random" supported for now

  val player = new GamePlayer[Game, Move, Board, Mark, String](
    gameFactory = () => Game(),
    printBoard = printBoard,
    humanMove = humanMove,
    agentMove = agentMove,
    getBoard = _.board,
    getCurrent = _.current,
    play = (g, m) => g.play(m),
    winner = _.winner,
    isDraw = _.isDraw,
    agentOptions = agentOptions,
    agentSelector = agentSelector
  )

  player.run()
}