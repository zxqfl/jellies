package jellies.game.levels

import jellies.game.LevelSpecification
import jellies.game.metadata.FollowedBy

object LevelSequence {
  def apply(levels: LevelSpecification*): Seq[LevelSpecification] = {
    levels.reverse match {
      case Seq() => Seq()
      case last +: rest => {
        rest.scanLeft(last) { (next, crnt) =>
          LevelSpecification(
              crnt.initialState,
              crnt.perspectives,
              crnt.metadata :+ FollowedBy(next))
        }
      }.reverse
    }
  }
}