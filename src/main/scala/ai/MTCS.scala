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

package com.tobiatesan.twist.ai.mtcs
import com.tobiatesan.twist.game.{Game, LivePosition, TerminalPosition, Move}
import com.tobiatesan.twist.player.{Player, AI, DebugStats, MoveWithStats}
import scala.util.Random
import collection.immutable.Map

/////////////////////////////////////////
// Stats
/////////////////////////////////////////

class UCTStats[G <: Game[G]](val nodes: Int, val tree: UCTNode[G])
    extends DebugStats[UCTAgent[G]] {
  override def toString() =
    "UCTNodes: " + nodes + "; Tree:===============\n" + tree + "\n====================\n"
  def getNodes = nodes
}

/////////////////////////////////////////

case class UCTNode[G <: Game[G]](val position: LivePosition[G],
                                 val num: Int,
                                 val den: Int,
                                 val maxNode: Boolean,
                                 val children: Map[Move[G], UCTNode[G]]) {
  override def toString: String =
    (if (maxNode) "[+] (" else "[-] (") + num + "/" + den + ")\n\t" + children
      .mkString("\n")
      .replaceAll("\n", "\n\t")
}

/**
  * Implements an (inefficient) UCT agent based on Gelly 2011
  *
  * @param budget the number of total nodes (including default policy) to explore before stopping
  * @param c the exploration constant; the larger it is, the more the algorithm favors exploration
  *  over exploitation.
  */
class UCTAgent[G <: Game[G]](val budget: Int,
                             val c: Double,
                             val maximize: Boolean = false,
                             val r: Random)
    extends AI[G] {
  type Utility = Int
  type NodeCount = Int

  def SimDefault[G <: Game[G]](
      p: Either[LivePosition[G], TerminalPosition[G]]): (Utility, NodeCount) =
    p match {
      case Right(t: TerminalPosition[G]) => (t.utility, 0)
      case Left(l: LivePosition[G]) => {
        val map = l.successor()
        val randomMove = r.shuffle(map.keys.toList).head
        val (util, nodes) = SimDefault((map.get(randomMove).get))
        (util, nodes + 1)
      }
    }

  def SelectMove[G <: Game[G]](p: LivePosition[G],
                               t: UCTNode[G],
                               c: Double): Move[G] = {
    def N_s: Double = t.den.toDouble
    def N_s_a(a: Move[G]): Double =
      t.children.get(a).map(_.den).getOrElse(0).toDouble
    def Q_s_a(a: Move[G]): Double =
      t.children.get(a).map((a) => a.num / a.den).getOrElse(0).toDouble
    def argmax[A](a: Seq[A], f: A => Double) = a.sortBy[Double](f).last
    def argmin[A](a: Seq[A], f: A => Double) = a.sortBy[Double](f).head
    if (t.maxNode)
      argmax(p.successor.keys.toSeq,
             (a: Move[G]) => Q_s_a(a) + c * Math.sqrt(Math.log(N_s) / N_s_a(a)))
    else
      argmin(p.successor.keys.toSeq,
             (a: Move[G]) => Q_s_a(a) - c * Math.sqrt(Math.log(N_s) / N_s_a(a)))
  }

  /**
    * @return Updated tree, spent budget and value of last simulation to be propagated
    */
  def Simulate(p: LivePosition[G],
               node: UCTNode[G]): (UCTNode[G], NodeCount, Utility) = {
    val move = SelectMove(p, node, c)
    p.successor.get(move).get match {
      case (Right(t)) =>
        ((sim: (Utility, NodeCount)) =>
          (UCTNode(p,
                   node.num + sim._1,
                   node.den + 1,
                   node.maxNode,
                   node.children),
           sim._2 + 1,
           sim._1))(SimDefault(Right(t)))
      case (Left(l)) =>
        if (node.children contains move) {
          // Already in tree and not terminal, continue with tree policy
          ((rec: (UCTNode[G], NodeCount, Utility)) => {
            (UCTNode(p,
                     node.num + rec._3, // Backup
                     node.den + 1,
                     node.maxNode,
                     node.children updated (move, rec._1)),
             rec._2 + 1,
             rec._3)
          })(Simulate(l, node.children.get(move).get))
        } else {
          // Not in tree, not terminal: expland
          ((sim: (Utility, NodeCount)) => {
            (UCTNode[G](p,
                        node.num + sim._1,
                        node.den + 1,
                        node.maxNode,
                        (node.children updated (move,
                        UCTNode(l, sim._1, 1, !(node.maxNode), Map.empty)))),
             sim._2 + 1,
             sim._1)
          })(SimDefault(Left(l)))
        }
    }
  }

  def UCTSearch(p: LivePosition[G],
                budget: NodeCount): (Move[G], NodeCount, UCTNode[G]) = {
    var tree = new UCTNode[G](p, 0, 0, maximize, Map.empty)
    var budget_* = budget
    while (budget_* > 0) {
      val (newtree, spent, _) = Simulate(p, tree)
      tree = newtree
      budget_* = budget_* - spent
    }
    (SelectMove(p, tree, 0), budget - budget_*, tree)
  }

  def debug(
      p: LivePosition[G]): MoveWithStats[G, Move[G], DebugStats[UCTAgent[G]]] =
    ((x: (Move[G], Int, UCTNode[G])) =>
      MoveWithStats[G, Move[G], DebugStats[UCTAgent[G]]](
        x._1,
        new UCTStats(x._2, x._3)))(UCTSearch(p, budget))
}
