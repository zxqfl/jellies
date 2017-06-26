package jellies.game

final case class LevelSpecification(
    initialState: State,
    perspectives: Seq[Perspective])