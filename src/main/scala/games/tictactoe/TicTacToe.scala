
package com.tobiatesan.twist.tictactoe
import com.tobiatesan.twist.game.{Side, Min, Max, LivePosition => GameLivePosition, Move => GameMove}

class TicTacToeLivePosition(val game: Game) extends GameLivePosition[Game] {
  def sideToMove: Side = if (game.current == X) Max else Min
  def successor: Map[GameMove[Game], Either[GameLivePosition[Game], TicTacToeTerminalPosition]] = {
    val moves = game.successor()
    moves.map { case (move, next) =>
      val lp = new TicTacToeLivePosition(next)
      if (next.winner.isDefined || next.isDraw)
        move -> Right(new TicTacToeTerminalPosition(next))
      else
        move -> Left(lp)
    }.toMap
  }
}

class TicTacToeTerminalPosition(val game: Game) extends com.tobiatesan.twist.game.TerminalPosition[Game] {
  def utility: Integer = Integer.valueOf(game.utility)  
}
// ...existing code...
sealed trait Mark
case object X extends Mark
case object O extends Mark
case object Empty extends Mark

case class Move(row: Int, col: Int) extends com.tobiatesan.twist.game.Move[Game]

case class Board(grid: Vector[Vector[Mark]] = Vector.fill(3,3)(Empty)) {
  /**
   * Get the mark at a given cell.
   * @param row Row index (0 to 2)
   * @param col Column index (0 to 2)
   */
  def apply(row: Int, col: Int): Mark = grid(row)(col)

  /**
   * Returns a new board with the given move applied for the given mark.
   * @param move Move to apply
   * @param mark Mark to place
   */
  def updated(move: Move, mark: Mark): Board = {
    val newRow = grid(move.row).updated(move.col, mark)
    Board(grid.updated(move.row, newRow))
  }

  /**
   * Returns all available moves (empty cells).
   */
  def availableMoves: Seq[Move] =
    for {
      r <- 0 to 2
      c <- 0 to 2
      if grid(r)(c) == Empty
  } yield Move(r, c)

  /** Returns true if the board is full (no empty cells). */
  def isFull: Boolean = availableMoves.isEmpty
}

case class Game(board: Board = Board(), current: Mark = X) extends com.tobiatesan.twist.game.Game[Game] {
  def successor(): Seq[(com.tobiatesan.twist.game.Move[Game], Game)] = {
    board.availableMoves.map { m => (m, play(m)) }
  }
  def utility: Int = winner match {
    case Some(X) => 1
    case Some(O) => -1
    case None => 0
  }
  def startingPosition(): GameLivePosition[Game] = new TicTacToeLivePosition(Game())



  def play(move: Move): Game = {
    if (board(move.row, move.col) != Empty) this
    else Game(board.updated(move, current), if (current == X) O else X)
  }

  def winner: Option[Mark] = {
    val rows = board.grid
    val cols = board.grid.transpose
    val diag1 = Vector(board(0,0), board(1,1), board(2,2))
    val diag2 = Vector(board(0,2), board(1,1), board(2,0))
    val lines = rows ++ cols ++ Seq(diag1, diag2)
    lines.find(line => line.forall(_ == X)).map(_ => X)
      .orElse(lines.find(line => line.forall(_ == O)).map(_ => O))
  }

  def isDraw: Boolean = board.isFull && winner.isEmpty
}
