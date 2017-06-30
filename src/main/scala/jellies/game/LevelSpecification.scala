package jellies.game

trait LevelMetadata

final case class LevelSpecification(
    initialState: State,
    perspectives: Seq[Perspective],
    metadata: Seq[LevelMetadata] = Seq())