package game

import org.scalatest._

private class ModelTest extends FunSuite with Matchers {
  val reader = PlayerHandle(1)
  val other = PlayerHandle(2)
  val readerPerspective = Perspective(Direction(0, -1))
  val otherPerspective = Perspective(Direction(1, 0))
  assert(otherPerspective.down == readerPerspective.right)
  
  val example = 
    """
    ......
    .11...
    .XX1..
    ...X1.
    ....X.
    """
  val multiplayerExample =
    """
    ......
    .11...
    .XX1..
    1X.X1.
    ....X.
    """
  
  val singlePlayerLevel =
    LevelSpecification(
        State.fromASCIIArt(example),
        Map(reader -> readerPerspective))
  val invalidLevel =
    LevelSpecification(
        State.fromASCIIArt(multiplayerExample),
        Map(reader -> readerPerspective))
  val multiplayerLevel =
    LevelSpecification(
        State.fromASCIIArt(multiplayerExample),
        Map(reader -> readerPerspective, other -> otherPerspective))
  
  test("level validation") {
    Model.validateSpecification(singlePlayerLevel) shouldBe true
    Model.validateSpecification(invalidLevel) shouldBe false
    Model.validateSpecification(multiplayerLevel) shouldBe true
  }
  
  test("undo/redo") {
    val model = new Model(singlePlayerLevel)
    
    model.canUndo shouldBe false
    model.canRedo shouldBe false
    
    model.attemptMove(reader, Location(5, 2), readerPerspective.right)
    
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
    val model = new Model(multiplayerLevel)
    model.attemptMove(reader, Location(1, 2), readerPerspective.down) shouldBe
        Left(InvalidDirectionFromPerspective)
    model.attemptMove(other, Location(1, 2), readerPerspective.down)
    model.currentState.state shouldBe State.fromASCIIArt(
        """
        ......
        .11...
        .XX1..
        .X.X1.
        ...1X.
        """)
  }
}