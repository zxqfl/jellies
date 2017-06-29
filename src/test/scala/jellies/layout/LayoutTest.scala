package jellies.layout

import org.scalatest._

import jellies.game.ExampleLevels._
import jellies.game

private class LayoutTest extends FunSuite with Matchers {
  test("layout sanity test") {
    val model = new game.Model(singlePlayerLevel)
    val state = model.currentState.state
    val layout = new Layout(state.emptyMoveResult, readerPerspective)
    layout.tiles(0).size should be > 25
  }
}