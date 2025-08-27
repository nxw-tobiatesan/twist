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

package com.tobiatesan.twist.draughts
import board.{Man, King, Board, Piece}
import board.Board.Cardinals
import board.util.Pdn
import com.tobiatesan.twist.game.{
  Side,
  Min,
  Max,
  Game,
  Move,
  LivePosition,
  TerminalPosition
}
import scala.collection.immutable.HashMap

//////////////////////////////////////////
// MOVES
//////////////////////////////////////////

/**
  * Represents a move, further specialized in ordinary or capturing in 1.15 to 1.19 WCDF rules
  */
abstract class DraughtsMove extends Move[Draughts]

/**
  * Represents an ordinary (non-capturing) move
  * (1.15 - 1.17 WCDF rules)
  */
case class OrdinaryMove(val from: Pdn, val to: Pdn) extends DraughtsMove {
  override def toString = from + "-" + to
}

/**
  * Represents an capturing move, i.e. a maximally extending sequence of jumps
  * (1.18 WCDF rules)
  */
case class CapturingMove(val jumps: Vector[Pdn]) extends DraughtsMove {
  override def toString = jumps.mkString("x")
}

//////////////////////////////////////////
// POSITIONS
//////////////////////////////////////////

class LiveDraughtsPosition(board: Board,
                           sideToMove: Side,
                           ply: Int,
                           TTL: Int = 100)
    extends DraughtsPosition(board, sideToMove, ply, TTL)
    with LivePosition[Draughts] {
  // TODO This has grown into a god object
  override def toString(): String = super.toString + "Moves: " + sideToMove

  def successor()
    : Map[Move[Draughts],
          Either[LivePosition[Draughts], TerminalPosition[Draughts]]] = {
    (getAvailableMoves(board, sideToMove)
      .map((m) => {
        // We apply moves lazily, so that the resulting board is computed only if/when accessed
        lazy val applied = m match {
          case (x: OrdinaryMove)  => updateBoard(board, x)
          case (x: CapturingMove) => updateBoard(board, x)
        }

        if (getAvailableMoves(applied, sideToMove.opposite).size == 0) {
          // The game is won by the player who can make the last move
          // ie no move is available to the opponent on their turn to play [WCDF Rules of Draughts 1.30]
          // (We don't do forfeiting or resigning or clock for simplicity)
          val winner = Some(sideToMove)
          (m,
           Right(
             new TerminalDraughtsPosition(applied,
                                          sideToMove.opposite,
                                          ply + 1,
                                          TTL,
                                          winner)))
        } else if (ply + 1 > TTL) { // timeout
          // No winner, but we have run out of time, it's a draw
          (m,
           Right(
             new TerminalDraughtsPosition(applied,
                                          sideToMove.opposite,
                                          ply + 1,
                                          TTL,
                                          None)))
        } else {
          // No winner, no draw, let's continue playing
          (m,
           Left(
             new LiveDraughtsPosition(applied,
                                      sideToMove.opposite,
                                      ply + 1,
                                      TTL)))
        }
      })
      .toMap)
  }

  def getAvailableMoves(b: Board, s: Side): Seq[DraughtsMove] = {
    val jumps = getMenForSide(b, s).map(getJumpMovesForMan(b, _)).flatten
    if (jumps != List()) {
      // All capturing moves are compulsory [WCDF Rules of Draughts 1.20]
      jumps
    } else {
      getMenForSide(b, s).map(getOrdinaryMovesForMan(b, _)).flatten
      // Available moves are all moves available for all men
    }
  }

  private def updateBoard(b: Board, move: OrdinaryMove): Board = {
    // We move set the man's present position to None and put it in its destination square.
    // Then we try and see if it can be promoted to King (i.e. if it landed on King's row)
    val man = b(move.from)
    val updatedBoard = board.updated(move.to, man).updated(move.from, None)
    tryPromotion(
      move.to,
      updatedBoard
    )
  }

  private def updateBoard(b: Board, move: CapturingMove) = {
    def recUpdateBoard(jumps: Vector[Pdn], b: Board): Board = {
      // Recursively apply jump move until there are no jumps left
      val from = jumps.head
      val to = jumps.tail.head
      val lastJump = jumps.tail.tail.length == 0
      val capturedOpt = Board.midPoint(from, to)
      val updated = capturedOpt match {
        case Some(captured) =>
          b.updated(to, b(from)).updated(captured, None).updated(from, None)
        case None =>
          throw new IllegalArgumentException(s"recUpdateBoard: midPoint is undefined for from=$from to=$to")
      }
      // After a jump remove the man from its original position,
      // place it on its destination square and remove the captured
      // piece

      if (lastJump)
        updated
      else
        recUpdateBoard(jumps.tail, updated)
    }

    val updatedBoard = recUpdateBoard(move.jumps, b)

    // At the end of a sequence of jumps we see if the piece landed
    // on King's row and if so promote it
    tryPromotion(
      move.jumps.last,
      updatedBoard
    )
  }

  private def tryPromotion(landingSquare: Pdn, updatedboard: Board): Board = {
    // Check if a man landed on King's row and promote it to King
    val piece = updatedboard(landingSquare).get
    piece match {
      case Man(side) =>
        if (Board.KingsRow(side.opposite) contains landingSquare)
          updatedboard.updated(landingSquare, Some(King(side)))
        else
          updatedboard
      case _ => updatedboard
    }
  }

  private case class CardinalFun(
      // Helper class for allowedDirectionsForMan
      // TODO: Hackish
      val boardHasRoom: Pdn => Boolean,
      val transform: Pdn => Pdn
  ) {
    def canJump(p: Pdn) = boardHasRoom(p) && boardHasRoom(transform(p))
  }

  /**
    * Yield a Cardinal object for each direction a man can move
    * towards.  First argument is a lambda that checks if moving in
    * that direction is legal, second argument is a transform that
    * returns the Pdn of the square in the given direction relative
    * to the argument.
    *
    * Used only in getSimpleMovesForMan and getJumpMovesForMan
    *
    * TODO: Super ugly, refine
    */
  private def allowedDirectionsForPiece(piece: Piece) =
    (piece match {
      case (King(_))   => Board.Cardinals.all
      case (Man(side)) => Board.Cardinals.MovingDirection(side)
    }) map {
      case Cardinals.SE =>
        CardinalFun(
          (x: Pdn) =>
            !Board.isSouthernEdge(x) &&
              !Board.isEasternEdge(x),
          Board.seOf _
        )
      case Cardinals.NE =>
        CardinalFun(
          (x: Pdn) =>
            !Board.isNorthernEdge(x) &&
              !Board.isEasternEdge(x),
          Board.neOf _
        )
      case Cardinals.SW =>
        CardinalFun(
          (x: Pdn) =>
            !Board.isSouthernEdge(x) &&
              !Board.isWesternEdge(x),
          Board.swOf _
        )
      case Cardinals.NW =>
        CardinalFun(
          (x: Pdn) =>
            !Board.isNorthernEdge(x) &&
              !Board.isWesternEdge(x),
          Board.nwOf _
        )
    }

  private def getOrdinaryMovesForMan(b: Board, square: Pdn) = {
    @inline def getSimpleMoveForCardinal(b: Board,
                                         square: Pdn,
                                         cardinal: CardinalFun): List[Pdn] = {
      if (cardinal.boardHasRoom(square)
          && b(cardinal.transform(square)) == None)
        List(cardinal.transform(square))
      else
        List()
    }
    assert(b(square) != None)
    allowedDirectionsForPiece(b(square).get)
      .map(getSimpleMoveForCardinal(b, square, _)
        .map(new OrdinaryMove(square, _)))
      .flatten
  }

  private def getJumpMovesForMan(b: Board, square: Pdn) = {
    assert(b(square) != None)
    recAllJumpsForMan(b, b(square).get, square).map(new CapturingMove(_))
  }

  /**
    *  Take a man and square and try to construct all available jump moves
    */
  private def recAllJumpsForMan(
      b: Board,
      piece: Piece,
      square: Pdn,
      partialMove: List[Pdn] = List()): Seq[Vector[Pdn]] = {
    def testIfRepeated(x: Pdn) =
      // A man can only be jumped once during a multiple jumping sequence. [WCDF 1.20]
      !((square :: partialMove).containsSlice(List(square, x))
        || (square :: partialMove).containsSlice(List(x, square)))

    /**
      * If man on given square can take a jump in direction
      * cardinal, returns a singleton with the destination's Pdn,
      * otherwise List()
      */
    @inline def getJumpForCardinal(piece: Piece,
                                   square: Pdn,
                                   cardinal: CardinalFun): List[Pdn] = {
      if (cardinal.canJump(square)) {
        val dest = cardinal.transform(cardinal.transform(square))
        val midpoint = cardinal.transform(square)
        val mid = b(midpoint)
        if (b(dest) == None
            && mid != None
            && piece.canCapture(mid.get))
          List(dest)
        else
          List()
      } else
        List()
    }

    val jumpsFromSquare: Seq[Pdn] = allowedDirectionsForPiece(piece)
      .map(
        getJumpForCardinal(piece, square, _)
      )
      .flatten
      .filter(testIfRepeated(_))

    val moves: Seq[Vector[Pdn]] = jumpsFromSquare.map(Vector(square, _))

    /*
     * We try to extend jump moves repeatedly until they can be no
     * longer extended.
     *
     * As per WCDF, "If a jump creates an immediate further
     * capturing opportunity, then the capturing move of the piece
     * (man or king) is continued until all the jumps are
     * completed."  [1.19 WCDF]
     */
    moves
      .map((vect: Vector[Pdn]) => {
        val extensions: Seq[Vector[Pdn]] =
          (recAllJumpsForMan(b, piece, vect.last, square :: partialMove))
        if (extensions.size == 0)
          Seq(vect)
        else
          extensions.map(vect ++ _.tail)
      }: Seq[Vector[Pdn]])
      .flatten
  }

  /*
   * Returns where the men for a given side are
   */
  private def getMenForSide(b: Board, s: Side): Seq[Pdn] =
    Board.AllSquares
      .map((i) =>
        b(i) match {
          case Some(p) =>
            if (p.side == s) List(i)
            else List()
          case _ => List()
      })
      .flatten
}

class TerminalDraughtsPosition(board: Board,
                               sideToMove: Side,
                               ply: Int,
                               TTL: Int,
                               private val winner: Option[Side])
    extends DraughtsPosition(board, sideToMove, ply, TTL)
    with TerminalPosition[Draughts] {
  /**
    * Retur
    */
  def utility = {
    if (winner == None)
      0
    else if (winner == Some(Min))
      -1
    else
      1
  }

  override def toString(): String =
    super.toString +
      (if (winner == None)
         "DRAW \n"
       else
         "Winner: " + winner.get + "\n")
}

abstract class DraughtsPosition(val board: Board,
                                val sideToMove: Side,
                                private[draughts] val ply: Int,
                                private[draughts] val TTL: Int) {
  override def toString(): String = {
    "~ Draughts game ~\n" +
      "\nBoard: \n" + board.toString + "\n"
  }
}

/**
  * Super inefficient implementation of Game (speed was not a goal).
  * Red is Min, White is Max.
  * Generally follows WCDF rules with a few important differences concerning win/draw/time control:
  *
  *   - There is no time control
  *
  *   - The game is won by the player who can make the last move;
  *     that is, no move is available to the opponent on their turn to
  *     play, as per rule 1.30
  *
  *     - But Rule 1.31 is not implemented, i.e. there is no win by
  *       resignation or forfeiting or time control
  *
  *   - Rule 1.32 is not implemented (in particolare, the 40-move
  *     rule, which is easy to implement for those so inclined): there
  *     is no draw, except when the game goes on for more than an
  *     arbitrary number TTL of plies without a victory
  *
  *   - Red always moves first
  *
  * @param TTL duration of game in plies, at TTL-th ply a draw is forced
  *
  */
class Draughts(TTL: Int = 100)
    extends Game[Draughts] {
  type S = LiveDraughtsPosition
  def startingPosition() =
    new LiveDraughtsPosition(Board.StartingBoard, Min, 0, TTL)
}
