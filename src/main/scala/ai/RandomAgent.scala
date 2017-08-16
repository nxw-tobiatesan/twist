package com.tobiatesan.twist.ai
import com.tobiatesan.twist.player.{DebugStats, AI, MoveWithStats}
import com.tobiatesan.twist.game.{Game, LivePosition, Move}

/**
  * Agent that will take a random move, for debugging
  */
class RandomAgent[G <: Game[G]](val random: scala.util.Random) extends AI[G] {
  override def debug(p: LivePosition[G])
    : MoveWithStats[G, Move[G], DebugStats[RandomAgent[G]]] =
    MoveWithStats[G, Move[G], DebugStats[RandomAgent[G]]](
      p.successor().toList(random.nextInt(p.successor().size))._1,
      new DebugStats[RandomAgent[G]] { def getNodes = 0 }
    )
}
