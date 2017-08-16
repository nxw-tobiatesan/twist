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

class UCTStats[G <: Game[G]](val nodes: Int,
                             val simulations: Int,
                             val tree: UCTNode[G])
    extends DebugStats[UCTAgent[G]] {
  def +(v: UCTStats[G]) =
    new UCTStats(nodes + v.nodes, simulations + v.simulations, tree)
  override def toString() =
    "UCTNodes: " + nodes + "; Simulations: " + simulations + " Tree:===============\n" + tree + "\n====================\n"
  def getNodes = nodes
}

case class UCTNode[G <: Game[G]](val position: LivePosition[G],
                                 val num: Int,
                                 val den: Int,
                                 val maxNode: Boolean,
                                 val desc: Map[Move[G], UCTNode[G]]) {
  override def toString: String =
    (if (maxNode) "[+] (" else "[-] (") + num + "/" + den + ")\n\t" + desc
      .mkString("\n")
      .replaceAll("\n", "\n\t")
}

class UCTAgent[G <: Game[G]](
    val budget: Int,
    val c: Double, // The larger it is, the more the algorithm favors exploration over exploitation.
    val maximize: Boolean = false,
    val r: Random)
    extends AI[G] {
  type Utility = Int
  type Nodes = Int
  def SimDefault[G <: Game[G]](
      p: Either[LivePosition[G], TerminalPosition[G]]): (Utility, Nodes) =
    // Repeatedly pick a random move and return utility of reached state
    p match {
      case Right(t: TerminalPosition[G]) => (t.utility, 0)
      case Left(l: LivePosition[G]) => {
        val map = l.successor()
        val picked = r.shuffle(map.keys.toList).head
        val (util, nodes) = SimDefault((map.get(picked).get))
        (util, nodes + 1)
      }
    }

  def TreeSelectMove[G <: Game[G]](p: LivePosition[G],
                                   t: UCTNode[G],
                                   c: Double): Move[G] = {
    def N_s: Double = t.den.toDouble
    def N_s_a(a: Move[G]): Double =
      t.desc.get(a).map(_.den).getOrElse(0).toDouble
    def Q_s_a(a: Move[G]): Double =
      t.desc.get(a).map((a) => a.num / a.den).getOrElse(0).toDouble
    def argmax[A](a: Seq[A], f: A => Double) = a.sortBy[Double](f).last
    def argmin[A](a: Seq[A], f: A => Double) = a.sortBy[Double](f).head
    if (t.maxNode)
      argmax(p.successor.keys.toSeq,
             (a: Move[G]) => Q_s_a(a) + c * Math.sqrt(Math.log(N_s) / N_s_a(a)))
    else
      argmin(p.successor.keys.toSeq,
             (a: Move[G]) => Q_s_a(a) - c * Math.sqrt(Math.log(N_s) / N_s_a(a)))
  }

  def Simulate(p: LivePosition[G],
               t: UCTNode[G]): (UCTNode[G], Nodes, Utility) = {
    val move = TreeSelectMove(p, t, c) //, debug)
    p.successor.get(move).get match {
      case (Right(term)) =>
        ((sim: (Utility, Nodes)) =>
          (UCTNode(p, t.num + sim._1, t.den + 1, t.maxNode, t.desc),
           sim._2 + 1,
           sim._1))(SimDefault(Right(term)))
      case (Left(l)) =>
        if (t.desc contains move) {
          // Not a terminal state i.e. already in tree and not terminal
          ((rec: (UCTNode[G], Nodes, Utility)) => {
            require(rec._1.position == l)
            (UCTNode(p,
                     t.num + rec._3, // Backup
                     t.den + 1,
                     t.maxNode,
                     t.desc updated (move, rec._1)),
             rec._2 + 1,
             rec._3)
          })(Simulate(l, t.desc.get(move).get))
        } else {
          // Not in tree, not terminal
          ((sim: (Utility, Nodes)) => {
            (UCTNode[G](p,
                        t.num + sim._1,
                        t.den + 1,
                        t.maxNode,
                        (t.desc updated (move,
                        UCTNode(l, sim._1, 1, !(t.maxNode), Map.empty)))),
             sim._2 + 1,
             sim._1)
          })(SimDefault(Left(l)))
        }
    }
  }

  def UCTSearch(p: LivePosition[G],
                budget: Nodes): (Move[G], Nodes, UCTNode[G], Int) = {
    var t = new UCTNode[G](p, 0, 0, maximize, Map.empty)
    var remainingbudget = budget
    var simulations = 0
    while (remainingbudget > 0) {
      val (newt, total, _) = Simulate(p, t)
      t = newt
      remainingbudget = remainingbudget - total
      simulations = simulations + 1
    }
    (TreeSelectMove(p, t, 0), budget - remainingbudget, t, simulations)
  }

  def debug(
      p: LivePosition[G]): MoveWithStats[G, Move[G], DebugStats[UCTAgent[G]]] =
    ((x: (Move[G], Int, UCTNode[G], Int)) =>
      MoveWithStats[G, Move[G], DebugStats[UCTAgent[G]]](
        x._1,
        new UCTStats(x._2, x._4, x._3)))(UCTSearch(p, budget))
}
