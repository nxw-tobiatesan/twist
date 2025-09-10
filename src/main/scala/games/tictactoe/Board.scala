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
import com.tobiatesan.twist.game.{Side, Min, Max}

object Board {
  val Size = 3
  def empty: Board = new Board(Vector.fill(Size, Size)(None))
}

class Board(val grid: Vector[Vector[Option[Side]]]) {
  def apply(row: Int, col: Int): Option[Side] = grid(row)(col)
  def updated(row: Int, col: Int, value: Option[Side]): Board =
    new Board(grid.updated(row, grid(row).updated(col, value)))
  override def toString: String =
    grid.map { row =>
      row.map {
        case Some(Min) => "X"
        case Some(Max) => "O"
        case None => "."
      }.mkString(" ")
    }.mkString("\n")
}
