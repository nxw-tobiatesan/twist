package com.tobiatesan.twist.draughts.board
import com.tobiatesan.twist.game.{Side, Min, Max}
import Board._
import util._

import org.scalatest.FunSuite

class boardTest extends FunSuite {
  test("pdn 1 = index (0,1)") {
    assert(pdnToCoordinate(1: Pdn) == ((0, 1): Coordinate))
  }

  test("pdn 4 = index (0,7)") {
    assert(pdnToCoordinate(4: Pdn) == ((0, 7): Coordinate))
  }

  test("pdn 5 = index (1,0)") {
    assert(pdnToCoordinate(5: Pdn) == ((1, 0): Coordinate))
  }

  test("pdn 32 = index (7,6)") {
    assert(pdnToCoordinate(32: Pdn) == ((7, 6): Coordinate))
  }

  test("1,2,3,4 are N edges") {
    assert(isNorthernEdge(1))
    assert(isNorthernEdge(2))
    assert(isNorthernEdge(3))
    assert(isNorthernEdge(4))
    assert(!isSouthernEdge(1))
    assert(!isSouthernEdge(2))
    assert(!isSouthernEdge(3))
    assert(!isSouthernEdge(4))
  }

  test("29,30,31,32 are S edges") {
    assert(isSouthernEdge(29))
    assert(isSouthernEdge(30))
    assert(isSouthernEdge(31))
    assert(isSouthernEdge(32))
    assert(!isNorthernEdge(29))
    assert(!isNorthernEdge(30))
    assert(!isNorthernEdge(31))
    assert(!isNorthernEdge(32))
  }

  test("5,13,21,29 are W edges") {
    assert(isWesternEdge(5))
    assert(isWesternEdge(13))
    assert(isWesternEdge(21))
    assert(isWesternEdge(29))
    assert(!isEasternEdge(5))
    assert(!isEasternEdge(13))
    assert(!isEasternEdge(21))
    assert(!isEasternEdge(29))
  }

  test("4,12,20,28 are E edges") {
    assert(!isWesternEdge(4))
    assert(!isWesternEdge(12))
    assert(!isWesternEdge(20))
    assert(!isWesternEdge(28))
    assert(isEasternEdge(4))
    assert(isEasternEdge(12))
    assert(isEasternEdge(20))
    assert(isEasternEdge(28))
  }

  test("central squares are not edges") {
    List(6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 22, 23, 24, 25, 26, 27)
      .map((x: Pdn) => {
        assert(!isEasternEdge(x))
        assert(!isWesternEdge(x))
        assert(!isSouthernEdge(x))
        assert(!isNorthernEdge(x))
      })
  }

  test("SE of 1 is 6") {
    assert(seOf(1) == 6)
  }

  test("SW of 1 is 5") {
    assert(seOf(1) == 6)
  }

  test("NW of 1 fails") {
    intercept[AssertionError] {
      nwOf(1)
    }
  }

  test("NE of 1 fails") {
    intercept[AssertionError] {
      neOf(1)
    }
  }

  test("NW of 32 is 27") {
    assert(nwOf(32) == 27)
  }

  test("NE of 32 is 28") {
    assert(neOf(32) == 28)
  }

  test("SW of 32 fails") {
    intercept[AssertionError] {
      swOf(32)
    }
  }

  test("SE of 32 fails") {
    intercept[AssertionError] {
      seOf(32)
    }
  }

  test("NW of 25 is 21") {
    assert(nwOf(25) == 21)
  }

  test("NE of 25 is 22") {
    assert(neOf(25) == 22)
  }

  test("SE of 25 is 30") {
    assert(seOf(25) == 30)
  }

  test("SW of 25 is 29") {
    assert(swOf(25) == 29)
  }

  test("SE of 28 fails") {
    intercept[AssertionError] {
      seOf(28)
    }
  }

  test("NE of 28 fails") {
    intercept[AssertionError] {
      neOf(28)
    }
  }

  test("ClearBoard toString looks ok") {
    assert(
      Board.ClearBoard.toString ==
        s""" # # # #
# # # # 
 # # # #
# # # # 
 # # # #
# # # # 
 # # # #
# # # # """)
  }

  test("StartingBoard toString looks ok") {
    assert(
      Board.StartingBoard.toString ==
        s""" x x x x
x x x x 
 x x x x
# # # # 
 # # # #
o o o o 
 o o o o
o o o o """)
  }

  test("Can add man to and remove man from ClearBoard") {
    val board = Board.ClearBoard
    assert(board(1) == None)
    val newboard = board updated (1, Some(Man(Max)))
    assert(board(1) == None)
    assert(newboard(1).isInstanceOf[Some[_]])
    val man = newboard(1).get
    val newboard2 = board updated (1, None)
    assert(newboard2(1) == None)
  }

  test("midPoint works ok") {
    assert(midPoint(2, 9) == 6)
    assert(midPoint(31, 24) == 27)
    assert(midPoint(20, 11) == 16)
    assert(midPoint(5, 14) == 9)
  }
}
