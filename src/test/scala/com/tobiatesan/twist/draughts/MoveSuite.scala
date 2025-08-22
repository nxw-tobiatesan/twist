package com.tobiatesan.twist.draughts



import org.scalatest.funsuite.AnyFunSuite
import com.tobiatesan.twist.draughts.board._
import com.tobiatesan.twist.draughts.{OrdinaryMove, CapturingMove}
import com.tobiatesan.twist.game._

class MoveSuite extends AnyFunSuite {
  test("OrdinaryMove toString formats correctly") {
    val move = OrdinaryMove(1, 2)
    assert(move.toString == "1-2")
  }

  test("CapturingMove toString formats correctly") {
    val move = CapturingMove(Vector(1, 2, 3))
    assert(move.toString == "1x2x3")
  }

  test("Man cannot capture own side") {
    val redMan = Man(Min)
    val anotherRedMan = Man(Min)
    assert(!redMan.canCapture(anotherRedMan))
  }

  test("King cannot capture own side") {
    val whiteKing = King(Max)
    val anotherWhiteKing = King(Max)
    assert(!whiteKing.canCapture(anotherWhiteKing))
  }

  test("Promotion changes type to King") {
    val man = Man(Min)
    val king = man.promote
    assert(king.isInstanceOf[King])
  }
}
