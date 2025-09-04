import org.scalatest.funsuite.AnyFunSuite
import com.tobiatesan.twist.draughts.Draughts
import com.tobiatesan.twist.ai.RandomAgent

class RandomAgentSuite extends AnyFunSuite {
  test("RandomAgent selects a valid move from starting position") {
    val game = new Draughts()
    val agent = new RandomAgent[Draughts](new scala.util.Random())
    val pos = game.startingPosition()
    val move = agent.apply(pos)
    val validMoves = pos.successor().keys.toSet
    assert(validMoves.contains(move))
  }
}
