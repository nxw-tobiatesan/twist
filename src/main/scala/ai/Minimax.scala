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

package com.tobiatesan.twist.ai.minimax
import com.tobiatesan.twist.player.{DebugStats, AI, MoveWithStats}
import com.tobiatesan.twist.game.{Game, Move, LivePosition, TerminalPosition}
import scala.math.max

/////////////////////////////////////////
// Helper
/////////////////////////////////////////

import scala.collection.immutable.Queue
class FiniteQueue[A](q: Queue[A], maxSize: Int) {
  def enqueue[B >: A](elem: B): FiniteQueue[B] = {
    var ret = q.enqueue(elem)
    while (ret.size > maxSize) { ret = ret.dequeue._2 }
    new FiniteQueue(ret, maxSize)
  }
  def contains[B >: A](elem: B): Boolean = q.contains(elem)
}

/////////////////////////////////////////
// Stats
/////////////////////////////////////////

class AlphaBetaStats[G <: Game[G]](val nodes: Int,
                                   val leaves: Int,
                                   val static: Int,
                                   val cuts: Int,
                                   val extra: Int = 0)
    extends DebugStats[AbstractAlphaBeta[G]] {
  def +(v: AlphaBetaStats[G]) =
    new AlphaBetaStats[G](nodes + v.nodes,
                          leaves + v.leaves,
                          static + v.static,
                          cuts + v.cuts,
                          extra + v.extra)
  override def toString() =
    "Nodes: " + nodes + " Leaves: " + leaves + " Static ev: " + static + " Cuts: " + cuts + " Extra: " + extra
  def getNodes = nodes
}

/////////////////////////////////////////

trait QuiescenceCheck[G <: Game[G]] {

  /**
    * @return true iff quiescent
    */
  def apply(p: LivePosition[G]): Boolean
}

trait AlphaBetaOrdering[G <: Game[G]] extends Ordering[Move[G]]

trait MinimaxEvaluation[G <: Game[G]] {

  /**
    * @return a score in (-1, 1) for the provided position
    */
  def apply(p: LivePosition[G]): Double
}

/**
  * @param maximize true iff playing as max; plays as min by default
  */
abstract class AbstractAlphaBeta[G <: Game[G]](e: MinimaxEvaluation[G],
                                               o: AlphaBetaOrdering[G],
                                               depth: Int,
                                               maximize: Boolean = false)
    extends AI[G] {
  def onTerminal(t: TerminalPosition[G],
                 plyLeft: Int,
                 nega: Int): (Double, AlphaBetaStats[G])

  def onStatic(l: LivePosition[G],
               plyLeft: Int,
               nega: Int): (Double, AlphaBetaStats[G])

  def otherwise(l: LivePosition[G],
                plyLeft: Int,
                alpha: Double,
                beta: Double,
                nega: Int): (Double, AlphaBetaStats[G])

  case class Cut(val v: Double, val stats: AlphaBetaStats[G]) extends Exception

  def iter(p: Either[LivePosition[G], TerminalPosition[G]],
           plyLeft: Int,
           alpha: Double = Double.NegativeInfinity,
           beta: Double = Double.PositiveInfinity,
           nega: Int = 1): (Double, AlphaBetaStats[G]) =
    p match {
      case Right(t: TerminalPosition[G]) =>
        onTerminal(t, plyLeft, nega)
      case Left(l: LivePosition[G]) =>
        if (plyLeft <= 0)
          onStatic(l, plyLeft, nega)
        else
          otherwise(l, plyLeft, alpha, beta, nega)
    }

  def debug(
      p: LivePosition[G]): MoveWithStats[G, Move[G], AlphaBetaStats[G]] = {
    val evaluatedMoves = p.successor.map(mp => mp._1 -> iter(mp._2, depth))
    val cumulativeStats = evaluatedMoves
      .map(_._2._2)
      .fold(new AlphaBetaStats[G](0, 0, 0, 0))(_ + _)
    val rankedMoves = evaluatedMoves.toList
      .map((t: ((Move[G]), (Double, AlphaBetaStats[G]))) => (t._1, t._2._1)) // Discard stats
      .sortWith(_._2 > _._2)
    if (maximize)
      MoveWithStats[G, Move[G], AlphaBetaStats[G]](rankedMoves.last._1,
                                                   cumulativeStats)
    else
      MoveWithStats[G, Move[G], AlphaBetaStats[G]](rankedMoves.head._1,
                                                   cumulativeStats)
  }
}

class BasicAlphaBeta[G <: Game[G]](e: MinimaxEvaluation[G],
                                   o: AlphaBetaOrdering[G],
                                   depth: Int,
                                   maximize: Boolean = false)
    extends AbstractAlphaBeta[G](e, o, depth, maximize) {
  def onTerminal(t: TerminalPosition[G],
                 plyLeft: Int,
                 nega: Int): (Double, AlphaBetaStats[G]) = {
    (nega * t.utility.toDouble, new AlphaBetaStats(0, 1, 0, 0))
  }

  def onStatic(l: LivePosition[G],
               plyLeft: Int,
               nega: Int): (Double, AlphaBetaStats[G]) = {
    (nega * e(l), new AlphaBetaStats(0, 0, 1, 0))
  }

  def otherwise(p: LivePosition[G],
                plyLeft: Int,
                alpha: Double,
                beta: Double,
                nega: Int): (Double, AlphaBetaStats[G]) = {
    var alpha_* = alpha
    var runningStats = new AlphaBetaStats[G](1, 0, 0, 0)
    try {
      p.successor.toSeq
        .sortWith((x: (Move[G], _), y: (Move[G], _)) => o.lt(x._1, y._1))
        .foreach { m =>
          {
            val (negv, stats) =
              this.iter(m._2, plyLeft - 1, -beta, -alpha_*, -nega)
            val v = -negv
            alpha_* = max(alpha_*, v)
            runningStats = runningStats + stats
            if (alpha_* >= beta)
              throw new Cut(alpha_*, runningStats)
          }
        }
      (alpha_*, runningStats)
    } catch {
      case (p: Cut) => (p.v, p.stats + new AlphaBetaStats[G](0, 0, 0, 1))
    }
  }
}

/**
  * Simple extension of BasicAlphawBeta with killer heuristic
  * implemented with a _single_ list for all plys.
  *
  * According to Akl77, "programs differ in the number of killer moves
  * saved, the number of matches looked for, and on whether a separate
  * list is kept for each ply", so there is at least precedent.
  *
  * @param killerSize *total* size of the killer list
  */
class SimpleKillerAlphaBeta[G <: Game[G]](e: MinimaxEvaluation[G],
                                          o: AlphaBetaOrdering[G],
                                          depth: Int,
                                          killerSize: Int = 10,
                                          maximize: Boolean = false)
    extends BasicAlphaBeta[G](e, o, depth, maximize) {

  var killerList: FiniteQueue[Move[G]] = new FiniteQueue(Queue(), killerSize)

  object killerOrdering extends Ordering[Move[G]] {
    /*
     * if x is on the killer list and y is not x < y (comes first)
     * if y   "    "   then y < x
     * defer to usual ordering otherwise
     */
    def compare(x: Move[G], y: Move[G]): Int = {
      if (killerList.contains(x) && !killerList.contains(y))
        -1
      else if (!killerList.contains(x) && killerList.contains(y))
        +1
      else
        o.compare(x, y)
    }
  }

  override def debug(
      p: LivePosition[G]): MoveWithStats[G, Move[G], AlphaBetaStats[G]] = {
    // Wipe killer list at each new search
    killerList = new FiniteQueue(Queue(), killerSize)
    super.debug(p)
  }

  override def otherwise(l: LivePosition[G],
                         plyLeft: Int,
                         alpha: Double,
                         beta: Double,
                         nega: Int = 1): (Double, AlphaBetaStats[G]) = {
    var alpha_* = alpha
    var runningStats = new AlphaBetaStats[G](1, 0, 0, 0)

    try {
      l.successor()
        .toSeq
        .sortWith((x: (Move[G], _), y: (Move[G], _)) =>
          killerOrdering.lt(x._1, y._1))
        .foreach { m =>
          {
            val (negv, stats: AlphaBetaStats[G]) =
              this.iter(m._2, plyLeft - 1, -beta, -alpha_*, -nega)
            val v = -negv
            alpha_* = max(alpha_*, v)
            runningStats += (stats)
            if (alpha_* >= beta) {
              killerList = killerList.enqueue(m._1)
              throw new Cut(alpha_*, runningStats)
            }
          }
        }
      (alpha_*, runningStats)
    } catch {
      case (c: Cut) => {
        (c.v, c.stats + new AlphaBetaStats[G](0, 0, 0, 1))
      }
    }
  }
}

/**
  * Alpha-beta with classical killer heuristic with separate lists for each ply
  */
class KillerAlphaBeta[G <: Game[G]](e: MinimaxEvaluation[G],
                                    o: AlphaBetaOrdering[G],
                                    depth: Int,
                                    killerSize: Int = 3,
                                    maximize: Boolean = false)
    extends BasicAlphaBeta[G](e, o, depth, maximize) {

  // mantain a killer list (value) for each ply depth (key)
  var killerMap: Map[Int, FiniteQueue[Move[G]]] = Map.empty

  override def debug(
      p: LivePosition[G]): MoveWithStats[G, Move[G], AlphaBetaStats[G]] = {
    // Remember to reinitialize killer list at every search
    killerMap = Map.empty
    super.debug(p)
  }

  override def otherwise(l: LivePosition[G],
                         plyLeft: Int,
                         alpha: Double,
                         beta: Double,
                         nega: Int): (Double, AlphaBetaStats[G]) = {
    var alpha_* = alpha
    var runningStats = new AlphaBetaStats[G](1, 0, 0, 0)

    try {
      // TODO this could be _very_ inefficient
      object killerOrdering extends Ordering[Move[G]] {
        def compare(x: Move[G], y: Move[G]): Int = {
          if (killerMap(plyLeft).contains(x) && !killerMap(plyLeft).contains(y))
            -1
          else if (!killerMap(plyLeft).contains(x) && killerMap(plyLeft)
                     .contains(y))
            +1
          else
            o.compare(x, y)
        }
      }
      // Init map with empty list for depth `plyLeft` if not initialized
      if (!killerMap.contains(plyLeft))
        killerMap =
          killerMap.updated(plyLeft, new FiniteQueue(Queue(), killerSize))

      l.successor()
        .toSeq
        .sortWith((x: (Move[G], _), y: (Move[G], _)) =>
          killerOrdering.lt(x._1, y._1))
        .foreach { m =>
          {
            val (negv, stats: AlphaBetaStats[G]) =
              this.iter(m._2, plyLeft - 1, -beta, -alpha_*, -nega)
            val v = -negv
            alpha_* = max(alpha_*, v)
            runningStats = runningStats + (stats)
            if (alpha_* >= beta) {
              killerMap =
                killerMap.updated(plyLeft, killerMap(plyLeft).enqueue(m._1))
              throw new Cut(alpha_*, runningStats)
            }
          }
        }
      (alpha_*, runningStats)
    } catch {
      case (c: Cut) => {
        (c.v, c.stats + new AlphaBetaStats[G](0, 0, 0, 1))
      }
    }
  }
}

/**
  * Simple AlphaBeta enhanced with quiescence search
  *
  * @param extraPlys fixed depth of extra search if "ordinary" search depth is exhausted
  *  on a non-quiescent position
  */
class QuiescenceAlphaBeta[G <: Game[G]](e: MinimaxEvaluation[G],
                                        o: AlphaBetaOrdering[G],
                                        depth: Int,
                                        q: QuiescenceCheck[G],
                                        extraPlys: Int,
                                        maximize: Boolean = false)
    extends BasicAlphaBeta[G](e, o, depth, maximize) {
  override def iter(l: Either[LivePosition[G], TerminalPosition[G]],
                    plyLeft: Int,
                    alpha: Double = Double.NegativeInfinity,
                    beta: Double = Double.PositiveInfinity,
                    nega: Int = 1): (Double, AlphaBetaStats[G]) =
    l match {
      case Right(t: TerminalPosition[G]) => onTerminal(t, plyLeft, nega)
      case Left(l: LivePosition[G]) =>
        if (plyLeft <= 0)
          if (q(l))
            onStatic(l, plyLeft, nega)
          else
            // Position is non-quiescent, we do a further local, small
            // minimax search
            // TODO use something less quick & dirty
            (new BasicAlphaBeta[G](e, o, extraPlys, maximize))
              .iter(Left(l), extraPlys, alpha, beta, nega)
        else
          otherwise(l, plyLeft, alpha, beta, nega)
    }
}
