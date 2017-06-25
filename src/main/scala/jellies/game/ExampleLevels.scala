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
  val multiplayerExample =
    """
    ......
    .11...
    .XX1.X
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
}