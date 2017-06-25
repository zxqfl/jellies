package jellies.game

import org.scalatest._
import scala.util.control.NonFatal

private class UtilityClassTest extends FunSuite with Matchers {
  test("direction") {
    Direction(1, 0).rotate90DegreesClockwise shouldEqual Direction(0, -1)
  }
  test("bounding box") {
    val box = new BoundingBox(Location(0, 0), Location(1, -1), Location(4, 12))
    box.left shouldEqual 0
    box.right shouldEqual 4
    box.bottom shouldEqual -1
    box.top shouldEqual 12
    intercept[Exception] { new BoundingBox }
  }
  test("perspective") {
    val p = Perspective(Direction(1, 0))
    Set(p.down, p.left, p.up, p.right).size shouldEqual 4
  }
}