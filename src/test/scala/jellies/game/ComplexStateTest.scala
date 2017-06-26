package jellies.game

import org.scalatest._

private class ComplexStateTest extends FunSuite with Matchers {
  val readerPerspective = Perspective(Direction(0, -1))
  val complexExample =
    """
    .........8.....
    ...666.X29.....
    ...5X888.1.....
    ....X9.8.X.....
    ......X7.......
    XXXXXXX22......
    XXXXXX22.......
    .........2.....
    XXX33332.X.....
    ...............
    XX44...5X......
    ...............
    XX6X...........
    ...............
    """
  
  test("complex rules interactions") {
    val state = State.fromASCIIArt(complexExample)
    val move = state.Move(
        state.selectByColour(MergeableColour(1)),
        readerPerspective.left,
        readerPerspective)
    val resultContainer = state.applyMove(move)
    resultContainer shouldBe a[Right[_, MoveResult]]
    val result = resultContainer.toOption.get
    val newState = result.resultingState
    newState shouldEqual State.fromASCIIArt(
        """
        ...............
        ...666.X28.....
        ...5X888.9.....
        ....X9.8.X.....
        ......X7.......
        XXXXXXX........
        XXXXXX..1......
        .......222.....
        XXX...22.X.....
        .......2.......
        XX.33335X......
        ..44...........
        XX6X...........
        ...............
        """)
    result.effects.size shouldEqual 8
    result.effects(0) shouldBe a[state.JellyMove]
    result.effects(1) shouldBe a[state.JellyMove]
    result.effects(2) shouldBe a[state.JellyMove]
    result.effects(3) shouldBe a[state.JellyMove]
    result.effects(4) shouldBe a[state.JellyMerge]
    result.effects(5) shouldBe a[state.JellyMove]
    result.effects(6) shouldBe a[state.JellyMerge]
    result.effects(7) shouldBe a[state.JellyMove]
  }
  
  test("gravity former bug") {
    val state = State.fromASCIIArt(
        """
        12.
        1..
        """)
    val move = state.Move(
        state.selectByColour(MergeableColour(1)),
        Direction(1, 0),
        readerPerspective)
    val newState = state.applyMove(move).toOption.get.resultingState
    newState shouldEqual State.fromASCIIArt(
        """
        .12
        .1.
        """)
  }
  
  test("permission failure") {
    val state = State.fromASCIIArt(complexExample)
    val move = state.Move(
        state.selectByColour(MergeableColour(4)),
        readerPerspective.right,
        readerPerspective)
    state.applyMove(move) shouldEqual Left(JellyPermissionFailure)
    val move2 = state.Move(
        state.selectByColour(MergeableColour(7)),
        readerPerspective.right,
        readerPerspective)
    state.applyMove(move2) shouldEqual Left(JellyPermissionFailure)
  }
}