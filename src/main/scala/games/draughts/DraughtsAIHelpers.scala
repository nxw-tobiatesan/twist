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
import board.util.Pdn
import com.tobiatesan.twist.ai.minimax.{
  AlphaBetaOrdering,
  QuiescenceCheck,
  MinimaxEvaluation
}
import com.tobiatesan.twist.game.{
  Min,
  Max,
  Move,
  LivePosition,
  TerminalPosition
}

//////////////////////////////////////////
// AI HELPERS
//////////////////////////////////////////

object BasicDraughtsQuiescenceCheck extends QuiescenceCheck[Draughts] {
  def apply(p: LivePosition[Draughts]): Boolean =
    p match {
      case (p: LiveDraughtsPosition) =>
        p.successor.head._1 match {
          // If this move is a capturing move it's not quiescent
          case (m: CapturingMove) => false
          case (m: OrdinaryMove) =>
            p.successor.head._2 match {
              // If this move leads directly to a game over it's not quiescent
              case (Right(_)) => false
              // If this move leads to a promotion is not quiescent either
              case (Left(l)) =>
                // false (not quiescent) iff at least one available move is a promotion
                l.successor.exists(
                  _._1 match {
                    case OrdinaryMove(from, to) =>
                      if (List(1, 2, 3, 4).contains(to)) {
                        (p.board(from) == Man(Max))
                      } else if (List(29, 30, 31, 32).contains(to)) {
                        (p.board(from) == Man(Min))
                      } else {
                        true
                      }

                    case CapturingMove(jumps) =>
                      if (List(1, 2, 3, 4).contains(jumps.last)) {
                        (p.board(jumps.head) == Man(Max))
                      } else if (List(29, 30, 31, 32).contains(jumps.last)) {
                        (p.board(jumps.head) == Man(Min))
                      } else {
                        true
                      }

                  }
                )
            }
        }

    }
}

object SimplerDraughtsQuiescenceCheck extends QuiescenceCheck[Draughts] {
  def apply(p: LivePosition[Draughts]): Boolean =
    p match {
      case (p: LiveDraughtsPosition) =>
        p.successor.head._1 match {
          // If this move is a capturing move it's not quiescent
          case (m: CapturingMove) => false
          case (m: OrdinaryMove)  => true
        }
    }
}

/**
  * Basic move ordering that privileges captures, particularly multiple captures
  */
object BasicDraughtsMoveOrdering extends AlphaBetaOrdering[Draughts] {
  def compareMove(x: OrdinaryMove, y: OrdinaryMove): Int =
    // Sort moves that go farther first -- in early stages of play
    // when the tree is larger we aim to eventually advance and rarely
    // recede; also no kings that can go backwards are involved
    if (x.from > x.to & y.from > y.to) {
      // X is s to n and so is Y
      if (x.to == y.to) {
        x.from - y.from // Explore first the one that is farther behind
      } else {
        y.to - x.to // If x goes farther, comes first
      }
    } else if (x.from < x.to & y.from < y.to) {
      // Both are n to s
      if (x.to == y.to) {
        y.from - x.from
      } else {
        x.to - y.to
      }
    } else {
      0
    }

  // Capturing moves that capture the most pieces are to be preferred
  def compareJumps(x: Seq[Pdn], y: Seq[Pdn]): Int = {
    if (y.length == 0)
      x.length
    else {
      if (x.length == 0)
        -y.length
      else if (x.head == y.head) {
        x.head - y.head
      } else {
        compareJumps(x.tail, y.tail)
      }
    }
  }

  def compare(x: Move[Draughts], y: Move[Draughts]): Int = {
    x match {
      case u: CapturingMove =>
        y match {
          case v: CapturingMove => compareJumps(u.jumps, v.jumps)
          case _: OrdinaryMove  => +1
          case _                => 0
        }
      case u: OrdinaryMove =>
        y match {
          case _: CapturingMove => -1
          case v: OrdinaryMove  => compareMove(u, v)
          case _                => 0
        }
    }
  }
}

object NaiveDraughtsEvaluation extends MinimaxEvaluation[Draughts] {
  def apply(p: LivePosition[Draughts]): Double =
    p match {
      case (p: LiveDraughtsPosition) =>
        (p.board.filter(_ == Some(Man(Max))).size * 1 +
          p.board.filter(_ == Some(Man(Min))).size * -1 +
          p.board.filter(_ == Some(King(Max))).size * 2 +
          p.board.filter(_ == Some(King(Min))).size * -2).toDouble / 24

    }
}

/**
  * This evaluation tries to address the blindness of BasicDraughtsEvaluation to
  *  the approaching timeout by converging to 0 towards the end of the game.
  */
object ClockAwareDraughtsEvaluation extends MinimaxEvaluation[Draughts] {
  def apply(p: LivePosition[Draughts]): Double =
    p match {
      case (p: LiveDraughtsPosition) =>
        Math.sqrt((p.TTL - p.ply).toDouble / p.TTL.toDouble) *
          (p.board.filter(_ == Some(Man(Max))).size * 1 +
            p.board.filter(_ == Some(Man(Min))).size * -1 +
            p.board.filter(_ == Some(King(Max))).size * 2 +
            p.board.filter(_ == Some(King(Min))).size * -2).toDouble / 24

    }
}

/**
  * Expands upon ClockAwareDraughtsEvaluation by putting a premium on
  * men approaching king's row
  */
object PositionAwareDraughtsEvaluation extends MinimaxEvaluation[Draughts] {
  def apply(p: LivePosition[Draughts]): Double = {
    p match {
      case (p: LiveDraughtsPosition) => {
        Math.max(
          -1,
          Math.min(
            1,
            Math.sqrt((p.TTL - p.ply).toDouble / p.TTL.toDouble) *
              NaiveDraughtsEvaluation.apply(p) +
              (Seq(5, 6, 7, 8)
                .map(p.board(_))
                .filter(_ == Some(Man(Max)))
                .size * 2 +
                Seq(9, 10, 11, 12)
                  .map(p.board(_))
                  .filter(_ == Some(Man(Max)))
                  .size * 1 +
                Seq(28, 27, 26, 25)
                  .map(p.board(_))
                  .filter(_ == Some(Man(Min)))
                  .size * -2 +
                Seq(24, 23, 22, 21)
                  .map(p.board(_))
                  .filter(_ == Some(Man(Min)))
                  .size * -1).toDouble / 24
          )
        )
      }
    }
  }
}
