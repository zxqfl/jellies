package jellies.game.metadata

import jellies.game.LevelSpecification
import jellies.game.LevelMetadata

final case class FollowedBy(level: LevelSpecification) extends LevelMetadata