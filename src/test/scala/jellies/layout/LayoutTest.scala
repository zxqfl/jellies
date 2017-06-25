package jellies.layout

import org.scalatest._

import jellies.game.LevelSpecs._
import jellies.game

private class LayoutTest extends FunSuite with Matchers {
  test("layout sanity test") {
    val model = new game.Model(singlePlayerLevel)
    val layoutResult = Layout(model, reader)
    layoutResult.tiles.size should be > 25
  }
}