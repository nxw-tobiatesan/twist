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

package com.tobiatesan.twist.draughts.board
import com.tobiatesan.twist.game.{Side, Min, Max}
import scala.collection.immutable.Iterable
import util._

sealed trait Piece {
  val side: Side
  def canCapture(other: Piece): Boolean
}

sealed case class Man(val side: Side) extends Piece {
  def canCapture(other: Piece): Boolean =
    other match {
      case Man(otherside)  => otherside.opposite == side
      case King(otherside) => false
    }
  def promote: King = King(side)
}

sealed case class King(val side: Side) extends Piece {
  def canCapture(other: Piece): Boolean =
    other match {
      case Man(otherside)  => otherside.opposite == side
      case King(otherside) => otherside.opposite == side
    }
}

abstract case class Board(store: Vector[Option[Piece]])
    extends Iterable[Option[Piece]] {
  def apply(p: Pdn) = store(p - 1);
  def iterator = store.iterator
  def updated(p: Pdn, m: Option[Piece]): Board = {
    class UpdatedBoard(st: Vector[Option[Piece]]) extends Board(st)
    new UpdatedBoard(store.updated(p - 1, m))
  }
  override def toString = {
    (0 to 7)
      .map(
        (k) =>
          (0 to 7)
            .map(
              (j) => {
                val coord = (k, j)
                if (!Board.isDarkOption[Piece](coord)) {
                  " "
                } else {
                  val pdn = Board.coordinateToPdn(coord)
                  val sq = apply(pdn)
                  Board.squareToChar(sq)
                }
              }
            )
            .mkString("")
      )
      .mkString("\n")
  }
}

object Board {
  sealed trait Cardinal
  object Cardinals {
    case object NE extends Cardinal
    case object NW extends Cardinal
    case object SW extends Cardinal
    case object SE extends Cardinal
    val all = List(NE, NW, SW, SE)
    val MovingDirection: Map[Side, Seq[Cardinal]] = Map(
      (Min) -> List(Cardinals.SE, Cardinals.SW),
      (Max) -> List(Cardinals.NE, Cardinals.NW)
    )
  }

  /*game.*/
  val AllSquares = (1 to 32)
  val KingsRow: Map[Side, Seq[Pdn]] =
    Map(
      (Min -> (1 to 4)), // Red
      (Max -> (29 to 32)) // White
    )
  object ClearBoard extends Board(Vector.fill[Option[Piece]](32)(None))

  /**
    * Starting board as per WCDF 1.11:
    * {{{
    *   x x x x
    *  x x x x
    *   x x x x
    *  # # # #
    *   # # # #
    *  o o o o
    *   o o o o
    *  o o o o
    * }}}
    */
  object StartingBoard
      extends Board(
        Vector.fill[Option[Piece]](12)(Some(Man(Min)))
          ++
            Vector.fill[Option[Piece]](8)(None)
          ++
            Vector.fill[Option[Piece]](12)(Some(Man(Max)))
      )

  private[board] def pdnToCoordinate(p: Pdn): Coordinate = {
    assert(1 <= p && p <= 32)
    val p_ = p - 1
    val row = (p_ / 4)
    val shift = ((row + 1) % 2); // 0 is +1
    val j = (p_ % 4);
    (row, j * 2 + shift)
  }

  private[board] def coordinateToPdn(c: Coordinate): Pdn = {
    assert(c._1 >= 0 && c._2 >= 0 && c._1 <= 7 && c._2 <= 7)
    val shift = (c._1) % 2; // 0 is +1
    val row = c._2 / 2; // + shift;
    row + c._1 * 4 + 1
  }

  @inline def isDarkOption[Piece](x: Coordinate) = ((x._1 + x._2) % 2 == 1)
  @inline def isSouthernEdge(p: Pdn) = (p >= 29)
  @inline def isNorthernEdge(p: Pdn) = (p <= 4)
  @inline def isEasternEdge(p: Pdn) = ((p - 4) % 8 == 0)
  @inline def isWesternEdge(p: Pdn) = ((p - 5) % 8 == 0)
  @inline def isEvenRow(p: Pdn) = (((p - 1) / 4) % 2)

  @inline def neOf(p: Pdn) = {
    assert(!isNorthernEdge(p) && !isEasternEdge(p))
    p - 3 - isEvenRow(p)
  }

  @inline def seOf(p: Pdn) = {
    assert(!isSouthernEdge(p) && !isEasternEdge(p))
    p + 4 + (1 - isEvenRow(p))
  }

  @inline def nwOf(p: Pdn) = {
    assert(!isNorthernEdge(p) && !isWesternEdge(p))
    p - 4 - isEvenRow(p)
  }

  @inline def swOf(p: Pdn) = {
    assert(!isSouthernEdge(p) && !isWesternEdge(p))
    p + 3 + (1 - isEvenRow(p))
  }

  private[board] def squareToChar(sq: Option[Piece]): Char = sq match {
    case None            => '#'
    case Some(King(Max)) => 'O'
    case Some(Man(Max))  => 'o'
    case Some(King(Min)) => 'X'
    case Some(Man(Min))  => 'x'
  }

  def midPoint(from: Pdn, to: Pdn): Pdn = {
    assert(1 <= to && from <= 32)
    assert(1 <= from && from <= 32)
    val start = pdnToCoordinate(from);
    val end = pdnToCoordinate(to);
    val diff = ((end._1 - start._1), (end._2 - start._2));
    coordinateToPdn((start._1 + (diff._1 / 2), start._2 + (diff._2 / 2)))
  }
}

/**
  * Utility functions and methods for board and men
  */
package object util {
  private[board] type Coordinate = (Int, Int)

  /**
    * Pdn is numbering 1...32 as per WCDF 1.5
    */
  type Pdn = Int

  private[board] def charToOption[Piece](c: Char) = {
    c match {
      case 'x' => Some(Man(Min))
      case 'X' => Some(King(Min))
      case 'o' => Some(Man(Max))
      case 'O' => Some(King(Max))
      case '#' => None
      case _   => throw new Exception("Unrecognized character " + c)
    }
  }

  def boardFromString(s: String): Board = {
    assert(s.size != 0)
    class ConcreteBoard(store: Vector[Option[Piece]]) extends Board(store);
    new ConcreteBoard(
      s.split("\\s+")
        .filter((x) => (x.size != 0))
        .map((x: String) =>
          if (x.size != 1) throw new IllegalArgumentException(x) else x(0))
        .map(charToOption[Piece])
        .to[Vector]
    )
  }
}
