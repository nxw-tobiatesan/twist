package com.tobiatesan.twist.draughts
import org.scalatest.funsuite.AnyFunSuite


class DraughtsSuite extends AnyFunSuite {
  test("Draughts.startingPosition returns a valid LivePosition") {
    import com.tobiatesan.twist.game.LivePosition
    val game = new Draughts()
    val pos = game.startingPosition()
    assert(pos.isInstanceOf[LivePosition[_]])
    // Red always moves first
    import com.tobiatesan.twist.game.Min
    assert(pos.sideToMove == Min)
  }

  test("Draughts.startingPosition.successor generates valid moves") {
    val game = new Draughts()
    val pos = game.startingPosition()
    val successors = pos.successor()
    // There should be available moves from the starting position
    assert(successors.nonEmpty)
    // All keys should be DraughtsMove
    assert(successors.keys.forall(_.isInstanceOf[com.tobiatesan.twist.draughts.DraughtsMove]))
    // All values should be either LivePosition or TerminalPosition
    assert(successors.values.forall(e => e.isLeft || e.isRight))
  }
}
