package com.tobiatesan.twist
import com.tobiatesan.twist.ai.minimax.{
  BasicAlphaBeta,
  KillerAlphaBeta
}
import com.tobiatesan.twist.ai.{RandomAgent}
import com.tobiatesan.twist.draughts.{Draughts}
import com.tobiatesan.twist.matches.{Match}
import com.tobiatesan.twist.player.{AI, Human, DebugStats}
import com.tobiatesan.twist.game.{Move, Game, LivePosition, TerminalPosition}

///////////////////////////////////////////////////////////
//
// Quick & dirty helpers for console I/O
//
///////////////////////////////////////////////////////////

class QuitException extends Exception

trait Console {
  def read: String
  def write(s: String): Unit
}

class BasicConsole extends Console {
  override def read = {
    val in = readLine
    if (in == "quit")
      throw new QuitException
    else {
      in
    }
  }
  override def write(s: String) = print(s)
}

class SpectatorMatch[G <: Game[G]](min: AI[G],
                                   max: AI[G],
                                   startMax: Boolean,
                                   initial: LivePosition[G])
    extends Match[G](min, max, startMax, initial) {
  override def liveHook(p: LivePosition[G],
                        ms: Seq[Move[G]],
                        m: Move[G],
                        s: DebugStats[AI[G]]): Unit = {
    print("Evaluating:\n")
    print(p)
    print("Available moves:\n")
    prettyPrint(ms)
    print("Chosen " + m + "; Stats:\n" + s)
  }
  override def terminalHook(t: TerminalPosition[G]): Unit = {
    print("Ended with:\n")
    print(t)
  }
  private def prettyPrint(m: Seq[Move[G]]) = {
    m.zipWithIndex
      .map(_.swap)
      .toSeq
      .sortWith(_._1 < _._1)
      .map(x => (x._1 + 1) + ". " + x._2)
      .mkString(", ")
  }
}

class GenericHumanPlayer[G <: Game[G]](val console: Console) extends Human[G] {
  def apply(p: LivePosition[G]): Move[G] = {
    console.write("====================\n")
    console.write(p.toString)
    console.write("\n====================\n")
    console.write("Available moves:")
    console.write("\n")
    val succ = (p.successor).zipWithIndex.map(_.swap)
    console.write(
      succ.toSeq
        .sortWith(_._1 < _._1)
        .map(
          x => x._1 + ": " + x._2._1
        )
        .mkString("\n"))
    print("\n====================\n")

    var move: Option[Move[G]] = None
    while (move == None) {
      print("move> ")
      val input = console.read
      try {
        val key = input.toInt
        if (key > succ.size) throw new NumberFormatException
        else {
          move = Some(succ(key)._1)
        }
      } catch {
        case e: NumberFormatException => console.write("Invalid input\n")
        case e: Throwable             => throw e
      }
    }
    return move.get
  }
}

///////////////////////////////////////////////////////////
//
// Main program
//
///////////////////////////////////////////////////////////

object basicDraughts extends Draughts

object HumanVsAlpha extends App {
  // Super quick & dirty main method
  val p = basicDraughts.startingPosition
  val console = new BasicConsole()
  val player1 = new BasicAlphaBeta[Draughts](draughts.NaiveDraughtsEvaluation,
                                             draughts.BasicDraughtsMoveOrdering,
                                             10)
  val player2 = new GenericHumanPlayer[Draughts](console)
  val matsch = new Match(player1, player2, false, p)
  print("Type quit to exit\n")
  try {
    matsch.run()
  } catch {
    case e: QuitException => print("Quitting, bye!\n")
  }
}
