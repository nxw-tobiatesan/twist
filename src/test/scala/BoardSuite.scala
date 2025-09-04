import org.scalatest.funsuite.AnyFunSuite
import com.tobiatesan.twist.draughts.board._
import com.tobiatesan.twist.game._

class BoardSuite extends AnyFunSuite {
  test("Man can capture enemy Man but not King") {
    val redMan = Man(Min)
    val whiteMan = Man(Max)
    val whiteKing = King(Max)
    assert(redMan.canCapture(whiteMan))
    assert(!redMan.canCapture(whiteKing))
  }

  test("King can capture enemy Man and enemy King") {
    val redKing = King(Min)
    val whiteMan = Man(Max)
    val whiteKing = King(Max)
    assert(redKing.canCapture(whiteMan))
    assert(redKing.canCapture(whiteKing))
  }

  test("Man promotes to King of same side") {
    val redMan = Man(Min)
    val redKing = redMan.promote
    assert(redKing.isInstanceOf[King])
    assert(redKing.side == Min)
  }

  test("Board initializes with correct starting state") {
    val board = Board.StartingBoard
    // 12 red, 8 empty, 12 white
    assert(board.count(_.contains(Man(Min))) == 12)
    assert(board.count(_.isEmpty) == 8)
    assert(board.count(_.contains(Man(Max))) == 12)
  }

  test("Board updated method works for adding a man") {
    val board = Board.StartingBoard.updated(1, Some(Man(Min)))
    assert(board(1).contains(Man(Min)))
  }

  test("Board updated method works for removing a piece") {
    val board = Board.StartingBoard.updated(1, Some(Man(Min))).updated(1, None)
    assert(board(1).isEmpty)
  }

  test("Board toString contains expected characters") {
    val str = Board.StartingBoard.toString
    assert(str.contains("x") || str.contains("o") || str.contains("#"))
  }

  test("Board equality works for identical boards") {
    val b1 = Board.StartingBoard
    val b2 = Board.StartingBoard
    assert(b1 == b2)
  }

  test("Board inequality works for different boards") {
  val b1 = Board.StartingBoard
  val b2 = Board.StartingBoard.updated(1, Some(Man(Min)))
  assert(b1 != b2)
  }

  test("Man cannot promote twice") {
    val man = Man(Min)
    val king = man.promote
    assertThrows[NoSuchMethodException](king.getClass.getMethod("promote"))
  }

  test("King side is correct after promotion") {
    val man = Man(Max)
    val king = man.promote
    assert(king.side == Max)
  }

  test("Board hashCode is stable for identical boards") {
    val b1 = Board.StartingBoard
    val b2 = Board.StartingBoard
    assert(b1.hashCode == b2.hashCode)
  }

  test("Board hashCode differs for different boards") {
  val b1 = Board.StartingBoard
  val b2 = Board.StartingBoard.updated(1, Some(Man(Min)))
  assert(b1.hashCode != b2.hashCode)
  }
}
