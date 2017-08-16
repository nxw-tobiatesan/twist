package com.tobiatesan.twist.draughts
import com.tobiatesan.twist.draughts.board._
import com.tobiatesan.twist.game.{Min, Max}
import org.scalatest.FunSuite

class gameTest extends FunSuite {
  val game = new Draughts()
  test("newGame looks ok") {
    val newGame = game.startingPosition
    assert(newGame.sideToMove == Min)
    val str = s""" x x x x
x x x x
 x x x x
# # # #
 # # # #
o o o o
 o o o o
o o o o """
    assert(newGame.board == util.boardFromString(str))
  }

  test("newGame allows for only|all red moves") {
    val newGame = game.startingPosition
    val moves = newGame.successor
    assert(moves.size == 7)
    moves(OrdinaryMove(10, 14))
    moves(OrdinaryMove(10, 15))
    moves(OrdinaryMove(11, 15))
    moves(OrdinaryMove(11, 16))
    moves(OrdinaryMove(12, 16))
    moves(OrdinaryMove(9, 13))
    moves(OrdinaryMove(9, 14))
    assertThrows[NoSuchElementException](
      moves(OrdinaryMove(24, 20))
    )
  }

  test("after move newGame allows for only|all white moves") {
    val newGame = game.startingPosition
    val secondPly = newGame.successor.toList.head._2
    val moves2 = secondPly.left.get.asInstanceOf[LiveDraughtsPosition].successor
    assert(moves2.size == 7)

    moves2(OrdinaryMove(21, 17))
    moves2(OrdinaryMove(22, 17))
    moves2(OrdinaryMove(22, 18))
    moves2(OrdinaryMove(23, 18))
    moves2(OrdinaryMove(23, 19))
    moves2(OrdinaryMove(24, 19))
    moves2(OrdinaryMove(24, 20))
  }

  test("test for jump move") {
    val gme = game.startingPosition
    val gme2 = gme.successor()(OrdinaryMove(9, 14))
    val gme3 = gme2.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(21, 17))
    val moves = gme3.left.get.asInstanceOf[LiveDraughtsPosition].successor
    assert(moves.head._1.toString == "14x21")
    assert(moves.size == 1)
  }

  test("illegal move is illegal") {
    val gme = game.startingPosition
    val gme2 = gme.successor()(OrdinaryMove(9, 14))
    assertThrows[NoSuchElementException](
      gme2.left.get
        .asInstanceOf[LiveDraughtsPosition]
        .successor()(OrdinaryMove(9, 14))
    )
  }

  test("test for promotion") {
    val gme = game.startingPosition
    val gme2 = gme.successor()(OrdinaryMove(9, 14))
    val gme3 = gme2.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(21, 17))
    assert(gme3.left.get.asInstanceOf[LiveDraughtsPosition].successor.size == 1)
    val gme4 = gme3.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(CapturingMove(Vector(14, 21)))
    val gme5 = gme4.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(22, 18))
    val gme6 = gme5.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(5, 9))
    val gme7 = gme6.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(23, 19))
    val gme8 = gme7.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(1, 5))
    val gme9 = gme8.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(24, 20))
    val gme10 = gme9.left.get
      .asInstanceOf[LiveDraughtsPosition]
      .successor()(OrdinaryMove(10, 15))
    assert(
      gme10.left.get.asInstanceOf[LiveDraughtsPosition].successor.size == 1)
    assert(
      gme10.left.get
        .asInstanceOf[LiveDraughtsPosition]
        .successor
        .head
        ._1
        .toString == "19x10x1")
    val gme11 =
      gme10.left.get.asInstanceOf[LiveDraughtsPosition].successor.head._2
    assert(
      gme11.left.get.asInstanceOf[LiveDraughtsPosition].board(1).get == King(
        Max))
  }

  test("King can make multiple capture correctly") {
    val str = s""" # # # #
# # # #
 # x x #
# O # #
 # # # #
# # # #
 # # # #
# # # # """

    val board = util.boardFromString(str)
    val game = new LiveDraughtsPosition(board, Max, 1)
    assert(game.successor.size == 1)
    assert(game.successor.head._1.toString == "14x7x16")

    val str1 = s""" # # # #
# # # #
 x x x #
# O # #
 # # # #
# # # #
 # # # #
# # # # """

    val board1 = util.boardFromString(str1)
    val game1 = new LiveDraughtsPosition(board1, Max, 1)
    assert(game1.asInstanceOf[LiveDraughtsPosition].successor.size == 2)
    assert(
      game1
        .asInstanceOf[LiveDraughtsPosition]
        .successor
        .head
        ._1
        .toString == "14x7x16")
    assert(
      game1
        .asInstanceOf[LiveDraughtsPosition]
        .successor
        .tail
        .head
        ._1
        .toString == "14x5")

    val str2 = s""" # # # #
# # # #
 X X X #
# O # #
 # # # #
# # # #
 # # # #
# # # # """

    val board2 = util.boardFromString(str2)
    val game2 = new LiveDraughtsPosition(board2, Max, 1)
    assert(game2.asInstanceOf[LiveDraughtsPosition].successor.size == 2)
    assert(
      game2
        .asInstanceOf[LiveDraughtsPosition]
        .successor
        .head
        ._1
        .toString == "14x7x16")
    assert(
      game2
        .asInstanceOf[LiveDraughtsPosition]
        .successor
        .tail
        .head
        ._1
        .toString == "14x5")
  }

  test("man can't continue with a capture right after promotion") {
    val str = s""" # # # #
# x x #
 o # # #
# # # #
 # # # #
# # # #
 # # # #
# # # # """

    val board = util.boardFromString(str)
    val game = new LiveDraughtsPosition(board, Max, 1)
    assert(game.successor.size == 1)
    assert(game.successor.head._1.toString == "9x2")
  }

  test("man can't capture king") {
    val str = s""" # # # #
# X x #
 o # # #
# # # #
 # # # #
# # # #
 # # # #
# # # # """

    val board = util.boardFromString(str)
    val game = new LiveDraughtsPosition(board, Max, 1)
    assert(game.successor.size == 1)
    assert(game.successor.head._1.toString == "9-5")
  }

  test("boardFromString preserves starting board") {
    assert(
      util
        .boardFromString(Board.StartingBoard.toString)
        .toString == Board.StartingBoard.toString)
  }

  test("test boardFromString") {
    val str = s""" # # # #
# # # #
 # # x #
# # # #
 # # # #
# # # #
 # O O O
# O O O """

    val board = util.boardFromString(str)
    assert(board(1) == None)
    assert(board(11) == Some(Man(Min)))
    assert(board(32) == Some(King(Max)))
  }

  test("canWin") {
    val str = s""" # # # #
# # # #
 # x x #
# O # #
 # # # #
# # # #
 # # # #
# # # # """

    val board = util.boardFromString(str)
    val game = new LiveDraughtsPosition(board, Max, 1)
    assert(game.successor.size == 1)
    assert(game.successor.head._1.toString == "14x7x16")
    val game1 = game.successor.head._2
    game1 match {
      case Right(t: TerminalDraughtsPosition) => assert(t.utility > 0)
      case Left(l: LiveDraughtsPosition)      => assert(false)
    }
  }

  test("Black canWin too") {
    val str = s""" # # # #
# # # #
 # o o #
# X # #
 # # # #
# # # #
 # # # #
# # # # """

    val board = util.boardFromString(str)
    val game = new LiveDraughtsPosition(board, Min, 1)
    assert(game.successor.size == 1)
    assert(game.successor.head._1.toString == "14x7x16")
    val game1 = game.successor.head._2
    game1 match {
      case Right(t: TerminalDraughtsPosition) => assert(t.utility < 0)
      case Left(l: LiveDraughtsPosition)      => assert(false)
    }
  }

  test("can draw") {
    val str =
      s""" x x x x
x x x x
 x x x x
# # # #
 # # # #
o o o o
 o o o o
o o o o """
    val board = util.boardFromString(str)
    val game = new LiveDraughtsPosition(board, Max, 0, 4)
    val game1 = game.successor.head._2
    val game2 =
      game1.left.get.asInstanceOf[LiveDraughtsPosition].successor.head._2
    val game3 =
      game2.left.get.asInstanceOf[LiveDraughtsPosition].successor.head._2
    val game4 =
      game3.left.get.asInstanceOf[LiveDraughtsPosition].successor.head._2
    val game5 =
      game4.left.get.asInstanceOf[LiveDraughtsPosition].successor.head._2
    game5 match {
      case Right(t: TerminalDraughtsPosition) => assert(t.utility == 0)
      case Left(l: LiveDraughtsPosition)      => assert(false)
    }
  }
}
