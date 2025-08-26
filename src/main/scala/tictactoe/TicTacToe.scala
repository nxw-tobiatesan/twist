
/**
 * Tic Tac Toe domain model and game logic.
 *
 * Provides types and methods for representing the board, moves, marks, and game state.
 */
package com.tobiatesan.twist.tictactoe




/**
 * Represents a cell mark in Tic Tac Toe.
 * X and O are player marks, Empty is an unoccupied cell.
 */
sealed trait Mark
case object X extends Mark
case object O extends Mark
case object Empty extends Mark


/**
 * Represents a move on the board (row, col).
 * Rows and columns are 0-based (0 to 2).
 */
case class Move(row: Int, col: Int)


/**
 * Represents the Tic Tac Toe board state.
 * @param grid 3x3 grid of Mark values (X, O, Empty)
 */
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

  /**
   * Returns true if the board is full (no empty cells).
   */
  def isFull: Boolean = availableMoves.isEmpty
}


/**
 * Represents a Tic Tac Toe game state.
 * @param board Current board
 * @param current Mark of the player to move (X or O)
 */
case class Game(board: Board = Board(), current: Mark = X) {
  /**
   * Play a move for the current player. Returns a new game state.
   * If the cell is not empty, returns the same game state.
   * @param move Move to play
   */
  def play(move: Move): Game = {
    if (board(move.row, move.col) != Empty) this
    else Game(board.updated(move, current), if (current == X) O else X)
  }

  /**
   * Returns the winner (X or O) if there is one, otherwise None.
   */
  def winner: Option[Mark] = {
    val rows = board.grid
    val cols = board.grid.transpose
    val diag1 = Vector(board(0,0), board(1,1), board(2,2))
    val diag2 = Vector(board(0,2), board(1,1), board(2,0))
    val lines = rows ++ cols ++ Seq(diag1, diag2)
    lines.find(line => line.forall(_ == X)).map(_ => X)
      .orElse(lines.find(line => line.forall(_ == O)).map(_ => O))
  }

  /**
   * Returns true if the game is a draw (board full, no winner).
   */
  def isDraw: Boolean = board.isFull && winner.isEmpty
}
