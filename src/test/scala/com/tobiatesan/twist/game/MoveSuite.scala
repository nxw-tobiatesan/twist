package com.tobiatesan.twist.game


import com.tobiatesan.twist.draughts._
import com.tobiatesan.twist.draughts.board._



import org.scalatest.funsuite.AnyFunSuite

class MoveSuite extends AnyFunSuite {
  test("OrdinaryMove toString formats correctly") {
  val move = OrdinaryMove(1, 2)
    assert(move.toString == "1-2")
  }

  test("CapturingMove toString formats correctly") {
  val move = CapturingMove(Vector(1, 2, 3))
    assert(move.toString == "1x2x3")
  }

  test("midPoint calculates correct square") {
    val from = 1
    val to = 5
    val mid = Board.midPoint(from, to)
    assert(mid == None)
    val validFrom = 2
    val validTo = 9
    val validMid = Board.midPoint(validFrom, validTo)
    assert(validMid == Some(6))
  }
  test("midPoint returns None for out-of-bounds pdn") {
    assert(Board.midPoint(0, 5) == None)
    assert(Board.midPoint(1, 33) == None)
  }
  test("midPoint returns None for non-capture moves") {
    assert(Board.midPoint(1, 2) == None)
    assert(Board.midPoint(10, 11) == None)
  }
}
