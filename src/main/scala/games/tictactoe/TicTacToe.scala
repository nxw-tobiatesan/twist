/*
 * Twist
 * Copyright (C) 2017 Tobia Tesan
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package com.tobiatesan.twist.games.tictactoe
import com.tobiatesan.twist.game.{
	Side, Min, Max, Game, Move, LivePosition, TerminalPosition
}

//////////////////////////////////////////
// MOVES
//////////////////////////////////////////

abstract class TicTacToeMove extends Move[TicTacToe]
case class PlaceMark(row: Int, col: Int) extends TicTacToeMove {
	override def toString = s"($row, $col)"
}

//////////////////////////////////////////
// POSITIONS
//////////////////////////////////////////

class LiveTicTacToePosition(board: Board,
													 sideToMove: Side,
													 ply: Int)
		extends TicTacToePosition(board, sideToMove, ply)
		with LivePosition[TicTacToe] {
	override def toString(): String = super.toString + "Moves: " + sideToMove

	def successor(): Map[Move[TicTacToe], Either[LivePosition[TicTacToe], TerminalPosition[TicTacToe]]] = {
		val moves = getAvailableMoves(board)
		moves.map { m =>
			val newBoard = board.updated(m.row, m.col, Some(sideToMove))
			val winner = checkWinner(newBoard)
			if (winner.isDefined || moves.size == 1) {
				(m, Right(new TerminalTicTacToePosition(newBoard, sideToMove.opposite, ply + 1, winner)))
			} else {
				(m, Left(new LiveTicTacToePosition(newBoard, sideToMove.opposite, ply + 1)))
			}
		}.toMap
	}

	def getAvailableMoves(b: Board): Seq[PlaceMark] = {
		for {
			r <- 0 until Board.Size
			c <- 0 until Board.Size
			if b(r, c).isEmpty
		} yield PlaceMark(r, c)
	}

	private def checkWinner(b: Board): Option[Side] = {
		val lines =
			(0 until Board.Size).map(r => (0 until Board.Size).map(c => b(r, c))) ++ // rows
			(0 until Board.Size).map(c => (0 until Board.Size).map(r => b(r, c))) ++ // columns
			Seq((0 until Board.Size).map(i => b(i, i)), (0 until Board.Size).map(i => b(i, Board.Size - 1 - i))) // diagonals
		lines.collectFirst {
			case line if line.forall(_ == Some(Min)) => Min
			case line if line.forall(_ == Some(Max)) => Max
		}
	}
}

class TerminalTicTacToePosition(board: Board,
																sideToMove: Side,
																ply: Int,
																private val winner: Option[Side])
		extends TicTacToePosition(board, sideToMove, ply)
		with TerminalPosition[TicTacToe] {
	def utility = winner match {
		case None => 0
		case Some(Min) => -1
		case Some(Max) => 1
	}
	override def toString(): String =
		super.toString + (winner match {
			case None => "DRAW\n"
			case Some(s) => s"Winner: $s\n"
		})
}

abstract class TicTacToePosition(val board: Board,
																val sideToMove: Side,
																val ply: Int) {
	override def toString(): String = {
		"~ TicTacToe game ~\n" +
			"\nBoard: \n" + board.toString + "\n"
	}
}

class TicTacToe extends Game[TicTacToe] {
	type S = LiveTicTacToePosition
	def startingPosition() =
		new LiveTicTacToePosition(Board.empty, Min, 0)
}
