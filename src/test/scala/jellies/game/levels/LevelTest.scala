package jellies.game.levels

import org.scalatest._
import jellies.game.metadata.FollowedBy

private class LevelTest extends FunSuite with Matchers {
  test("level sequence formation") {
    for ((a, b) <- Levels.levels zip Levels.levels.tail) {
      a.metadata should contain (FollowedBy(b))
    }
  }
}