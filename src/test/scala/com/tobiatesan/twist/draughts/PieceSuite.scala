package com.tobiatesan.twist.draughts



import org.scalatest.funsuite.AnyFunSuite
import com.tobiatesan.twist.draughts.board._
import com.tobiatesan.twist.game._

class PieceSuite extends AnyFunSuite {
  test("Man and King have correct side") {
    val redMan = Man(Min)
    val whiteKing = King(Max)
    assert(redMan.side == Min)
    assert(whiteKing.side == Max)
  }

  test("Man promotes to King and retains side") {
    val man = Man(Max)
    val king = man.promote
    assert(king.isInstanceOf[King])
    assert(king.side == Max)
  }
}
