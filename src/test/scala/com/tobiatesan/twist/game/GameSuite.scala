package com.tobiatesan.twist.game



import org.scalatest.funsuite.AnyFunSuite

class GameSuite extends AnyFunSuite {
  test("Side.opposite returns correct side") {
    import com.tobiatesan.twist.game._
    assert(Min.opposite() == Max)
    assert(Max.opposite() == Min)
  }

  test("TerminalPosition.utility returns correct value") {
    import com.tobiatesan.twist.game._
    val win = new TerminalPosition[Game[_]] { def utility = 1 }
    val draw = new TerminalPosition[Game[_]] { def utility = 0 }
    val lose = new TerminalPosition[Game[_]] { def utility = -1 }
    assert(win.utility == 1)
    assert(draw.utility == 0)
    assert(lose.utility == -1)
  }
}
