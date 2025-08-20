import org.scalatest.funsuite.AnyFunSuite

class GameSuite extends AnyFunSuite {
  test("Side.opposite returns correct side") {
    import com.tobiatesan.twist.game._
    assert(Min.opposite() == Max)
    assert(Max.opposite() == Min)
  }
}
