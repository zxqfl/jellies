package jellies.game

import org.scalatest._

private class SimpleStateTest extends FunSuite with Matchers {
  val readerPerspective = Perspective(Direction(0, -1))
  val simpleExample =
    """
    ......
    .11...
    .XX1..
    ...X1.
    ....X.
    """
  
  test("basic board setup") {
    val state = State.fromASCIIArt(simpleExample)
    state.jellies.size shouldEqual 3
    state.at(Location(5, 1)) shouldEqual state.Wall
    state.at(Location(5, 2)) shouldBe a[state.JellyRef]
    state.anyTileOf(state.at(Location(5, 2))
        .asInstanceOf[state.JellyRef]) shouldEqual Location(5, 2)
    state.at(Location(5, 3)) shouldEqual state.OpenSpace
    state.at(Location(2, 4)) shouldEqual state.at(Location(3, 4))
    State.toASCIIArt(state) shouldEqual ("""
......
.11...
.XX1..
...X1.
....X.
""".trim + "\n")
  }
  
  test("move in blocked direction") {
    val state = State.fromASCIIArt(simpleExample)
    val move = state.Move(
        state.at(Location(4, 3)).asInstanceOf[state.JellyRef],
        readerPerspective.left,
        readerPerspective)
    state.applyMove(move) shouldEqual Left(DirectionBlocked)
  }
  
  test("move in direction other than left or right") {
    val state = State.fromASCIIArt(simpleExample)
    val move = state.Move(
        state.at(Location(4, 3)).asInstanceOf[state.JellyRef],
        readerPerspective.up,
        readerPerspective)
    state.applyMove(move) shouldEqual Left(InvalidDirectionFromPerspective)
  }
  
  test("move not resulting in merge") {
    val state = State.fromASCIIArt(simpleExample)
    val move = state.Move(
        state.at(Location(3, 4)).asInstanceOf[state.JellyRef],
        readerPerspective.left,
        readerPerspective)
    val resultContainer = state.applyMove(move)
    resultContainer shouldBe a[Right[_, MoveResult]]
    val result = resultContainer.toOption.get
    val newState = result.resultingState
    newState shouldEqual State.fromASCIIArt(
        """
        ......
        11....
        .XX1..
        ...X1.
        ....X.
        """)
    newState.jellies.size shouldEqual 3
  }
  
  test("move resulting in merge") {
    val state = State.fromASCIIArt(simpleExample)
    val move = state.Move(
        state.at(Location(3, 4)).asInstanceOf[state.JellyRef],
        readerPerspective.right,
        readerPerspective)
    val resultContainer = state.applyMove(move)
    resultContainer shouldBe a[Right[_, MoveResult]]
    val result = resultContainer.toOption.get
    val newState = result.resultingState
    newState shouldEqual State.fromASCIIArt(
        """
        ......
        ..11..
        .XX1..
        ...X1.
        ....X.
        """)
    newState.jellies.size shouldEqual 2
    newState.jellyParts(newState.at(Location(3, 4)).
        asInstanceOf[newState.JellyRef]).size shouldEqual 3
  }
  
  test("gravity") {
    val state = State.fromASCIIArt(simpleExample)
    val move = state.Move(
        state.at(Location(5, 2)).asInstanceOf[state.JellyRef],
        readerPerspective.right,
        readerPerspective)
    val resultContainer = state.applyMove(move)
    resultContainer shouldBe a[Right[_, MoveResult]]
    val result = resultContainer.toOption.get
    val newState = result.resultingState
    newState shouldEqual State.fromASCIIArt(
        """
        ......
        .11...
        .XX1..
        ...X..
        ....X1
        """)
    newState.jellies.size shouldEqual 3    
  }
}