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

package com.tobiatesan.twist.game
import scala.collection.immutable.Map

trait Game[G <: Game[G]] {
  def startingPosition(): LivePosition[G]
}

trait Move[G <: Game[G]] {}

sealed abstract class Side {
  def opposite(): Side
}

case object Min extends Side { // Red
  def opposite() = Max
}

case object Max extends Side {
  def opposite() = Min
}

trait TerminalPosition[G <: Game[G]] {

  /**
    * @return 0 if draw, -1 if Min wins, +1 if Max wins
    */
  def utility: Integer
}

/**
  * A position that is not terminal
  */
trait LivePosition[G <: Game[G]] {

  /**
    * @return the side to move, /if/ there are available moves
    */
  def sideToMove: Side

  /**
    * @return a non-empty Move -> Position map
    *
    * Unless the search space is trivial it's advisable to lazily evaluate positions
    */
  def successor(): Map[Move[G], Either[LivePosition[G], TerminalPosition[G]]]
}
