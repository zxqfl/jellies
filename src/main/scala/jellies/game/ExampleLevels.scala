package jellies.game

object ExampleLevels {
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
  val simpleMultiplayerExample = 
    """
    ......
    .11...
    .XX1.X
    1X.X1.
    ....X.
    """
  val multiplayerExample =
    """
    ...1...
    ..X....
    ....X..
    .......
    .11....
    1XX1.XX
    1X.X.XX
    ....1..
    """
  
  val singlePlayerLevel =
    LevelSpecification(
        State.fromASCIIArt(example),
        Map(reader -> readerPerspective))
  val invalidLevel =
    LevelSpecification(
        State.fromASCIIArt(multiplayerExample),
        Map(reader -> readerPerspective))
  val simpleMultiplayerLevel =
    LevelSpecification(
        State.fromASCIIArt(simpleMultiplayerExample),
        Map(reader -> readerPerspective, other -> otherPerspective))
  val multiplayerLevel =
    LevelSpecification(
        State.fromASCIIArt(multiplayerExample),
        Map(reader -> readerPerspective, other -> otherPerspective))
}