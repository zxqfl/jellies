package jellies.game

import org.scalatest._

import ExampleLevels._

private class ModelTest extends FunSuite with Matchers {
  
  test("level validation") {
//    Model.validateSpecification(singlePlayerLevel) shouldBe true
//    Model.validateSpecification(invalidLevel) shouldBe false
//    Model.validateSpecification(multiplayerLevel) shouldBe true
  }
  
  test("undo/redo") {
    val model = new Model(singlePlayerLevel)
    
    model.canUndo shouldBe false
    model.canRedo shouldBe false
    
    model.attemptMove(
        model.playerWithPerspective(readerPerspective),
        Location(5, 2),
        readerPerspective.right)
    
    val expectedStateAfter = State.fromASCIIArt(
        """
        ......
        .11...
        .XX1..
        ...X..
        ....X1
        """)
    
    model.currentState.state shouldBe expectedStateAfter
    
    model.canUndo shouldBe true
    model.canRedo shouldBe false
    
    model.undo()
    
    model.canUndo shouldBe false
    model.canRedo shouldBe true
    
    model.currentState.state should not be expectedStateAfter
    
    model.redo()
    
    model.currentState.state shouldBe expectedStateAfter
  }
  
  test("different perspectives") {
    val model = new Model(simpleMultiplayerLevel)
    model.attemptMove(
        model.playerWithPerspective(readerPerspective),
        Location(1, 2),
        readerPerspective.down) shouldBe Left(InvalidDirectionFromPerspective)
    val result = model.attemptMove(
        model.playerWithPerspective(otherPerspective),
        Location(1, 2),
        readerPerspective.down)
    model.currentState.state shouldBe State.fromASCIIArt(
        """
        ......
        .11...
        .XX1.X
        .X.X1.
        ...1X.
        """)
    assert(result.toOption.get.resultingState eq model.currentState.state)
  }
}