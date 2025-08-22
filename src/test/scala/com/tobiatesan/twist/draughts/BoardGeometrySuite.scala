package com.tobiatesan.twist.draughts.board

import org.scalatest.funsuite.AnyFunSuite
import com.tobiatesan.twist.draughts.board.util.{Pdn, Coordinate}

class BoardGeometrySuite extends AnyFunSuite {
  test("pdn 1 = index (0,1)") {
    assert(Board.pdnToCoordinate(1: Pdn) == ((0, 1): Coordinate))
  }
  test("pdn 4 = index (0,7)") {
    assert(Board.pdnToCoordinate(4: Pdn) == ((0, 7): Coordinate))
  }
  test("pdn 5 = index (1,0)") {
    assert(Board.pdnToCoordinate(5: Pdn) == ((1, 0): Coordinate))
  }
  test("pdn 32 = index (7,6)") {
    assert(Board.pdnToCoordinate(32: Pdn) == ((7, 6): Coordinate))
  }
  test("1,2,3,4 are N edges") {
    assert(Board.isNorthernEdge(1))
    assert(Board.isNorthernEdge(2))
    assert(Board.isNorthernEdge(3))
    assert(Board.isNorthernEdge(4))
    assert(!Board.isSouthernEdge(1))
    assert(!Board.isSouthernEdge(2))
    assert(!Board.isSouthernEdge(3))
    assert(!Board.isSouthernEdge(4))
  }
  test("29,30,31,32 are S edges") {
    assert(Board.isSouthernEdge(29))
    assert(Board.isSouthernEdge(30))
    assert(Board.isSouthernEdge(31))
    assert(Board.isSouthernEdge(32))
    assert(!Board.isNorthernEdge(29))
    assert(!Board.isNorthernEdge(30))
    assert(!Board.isNorthernEdge(31))
    assert(!Board.isNorthernEdge(32))
  }
  test("5,13,21,29 are W edges") {
    assert(Board.isWesternEdge(5))
    assert(Board.isWesternEdge(13))
    assert(Board.isWesternEdge(21))
    assert(Board.isWesternEdge(29))
    assert(!Board.isEasternEdge(5))
    assert(!Board.isEasternEdge(13))
    assert(!Board.isEasternEdge(21))
    assert(!Board.isEasternEdge(29))
  }
  test("4,12,20,28 are E edges") {
    assert(!Board.isWesternEdge(4))
    assert(!Board.isWesternEdge(12))
    assert(!Board.isWesternEdge(20))
    assert(!Board.isWesternEdge(28))
    assert(Board.isEasternEdge(4))
    assert(Board.isEasternEdge(12))
    assert(Board.isEasternEdge(20))
    assert(Board.isEasternEdge(28))
  }
  test("central squares are not edges") {
    List(6, 7, 8, 9, 10, 11, 14, 15, 16, 17, 18, 19, 22, 23, 24, 25, 26, 27)
      .map((x: Pdn) => {
        assert(!Board.isEasternEdge(x))
        assert(!Board.isWesternEdge(x))
        assert(!Board.isSouthernEdge(x))
        assert(!Board.isNorthernEdge(x))
      })
  }
  // ...existing code for directions, toString, and midPoint tests...
}
