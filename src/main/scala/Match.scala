package com.tobiatesan.twist.matches
import com.tobiatesan.twist.game.{Game, TerminalPosition, LivePosition, Move}
import com.tobiatesan.twist.player.{
  Player,
  Human,
  AI,
  DebugStats,
  MoveWithStats
}

case class PlyEntry[G <: Game[G]](
    val p: LivePosition[G],
    val m: Move[G],
    val s: Option[DebugStats[AI[G]]]
)

case class MatchLog[G <: Game[G]](val plys: Seq[PlyEntry[G]],
                                  val t: TerminalPosition[G]) {
  def cons(ply: PlyEntry[G]) = MatchLog(ply +: plys, t)
}

class Match[G <: Game[G]](val min: Player[G],
                          val max: Player[G],
                          val startMax: Boolean,
                          val initial: LivePosition[G]) {

  def liveHook(p: LivePosition[G],
               ms: Seq[Move[G]],
               m: Move[G],
               s: DebugStats[AI[G]]): Unit = {}

  def terminalHook(t: TerminalPosition[G]): Unit = {}

  private def f(p: LivePosition[G],
                player: Player[G],
                f: (LivePosition[G] => MatchLog[G])): MatchLog[G] =
    player match {
      case h: Human[G] => {
        val m = h.apply(p)
        p.successor()(m) match {
          case Left(l: LivePosition[G]) => f(l).cons(PlyEntry(p, m, None))
          case Right(t: TerminalPosition[G]) =>
            MatchLog(Seq(PlyEntry(p, m, None)), t)
        }
      }

      case a: AI[G] => {
        val MoveWithStats(m, s) = a.debug(p)
        p.successor()(m) match {
          case Left(l: LivePosition[G]) => {
            liveHook(p, p.successor().keys.toSeq, m, s)
            f(l).cons(PlyEntry(p, m, Some(s)))
          }
          case Right(t: TerminalPosition[G]) =>
            liveHook(p, p.successor().keys.toSeq, m, s)
            terminalHook(t)
            MatchLog(Seq(PlyEntry(p, m, Some(s))), t)
        }
      }
    }

  def miniturn(p: LivePosition[G]): MatchLog[G] = f(p, min, maxiturn)
  def maxiturn(p: LivePosition[G]): MatchLog[G] = f(p, max, miniturn)
  def run(): MatchLog[G] =
    if (startMax)
      maxiturn(initial)
    else
      miniturn(initial)
}
