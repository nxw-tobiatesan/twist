package com.tobiatesan.twist.tictactoe



sealed trait Mark
case object X extends Mark
case object O extends Mark
case object Empty extends Mark

case class Move(row: Int, col: Int)

case class Board(grid: Vector[Vector[Mark]] = Vector.fill(3,3)(Empty)) {
  def apply(row: Int, col: Int): Mark = grid(row)(col)
  def updated(move: Move, mark: Mark): Board = {
    val newRow = grid(move.row).updated(move.col, mark)
    Board(grid.updated(move.row, newRow))
  }
  def availableMoves: Seq[Move] =
    for {
      r <- 0 to 2
      c <- 0 to 2
      if grid(r)(c) == Empty
    } yield Move(r, c)
  def isFull: Boolean = availableMoves.isEmpty
}

case class Game(board: Board = Board(), current: Mark = X) {
  def play(move: Move): Game = {
    if (board(move.row, move.col) != Empty) this
    else Game(board.updated(move, current), if (current == X) O else X)
  }
  def winner: Option[Mark] = {
    val lines =
      board.grid ++ // rows
      board.grid.transpose ++ // cols
      Seq(
        Vector(board(0,0), board(1,1), board(2,2)),
        Vector(board(0,2), board(1,1), board(2,0))
      )
    lines.find(line => line.forall(_ == X)).map(_ => X)
      .orElse(lines.find(line => line.forall(_ == O)).map(_ => O))
  }
  def isDraw: Boolean = board.isFull && winner.isEmpty
}
