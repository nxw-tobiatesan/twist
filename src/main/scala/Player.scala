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

package com.tobiatesan.twist.player
import com.tobiatesan.twist.game.{Game, LivePosition, Move}

trait DebugStats[+A <: AI[_]] {
  def getNodes: Int
  def toString: String
}

case class MoveWithStats[G <: Game[G], +M <: Move[G], +S <: DebugStats[AI[G]]](
    val move: M,
    val stats: S
)

sealed trait Player[G <: Game[G]] {
  def apply(p: LivePosition[G]): Move[G]
}
trait Human[G <: Game[G]] extends Player[G]
trait AI[G <: Game[G]] extends Player[G] {
  def debug(p: LivePosition[G]): MoveWithStats[G, Move[G], DebugStats[AI[G]]]
  def apply(p: LivePosition[G]): Move[G] = debug(p).move
}
