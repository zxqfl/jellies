package jellies.game.levels

import jellies.game.Perspective
import jellies.game.Direction
import jellies.game.LevelMetadata
import jellies.game.LevelSpecification
import jellies.game.State
import scala.language.implicitConversions

trait AbstractLevel {
  val asciiField: String
  val perspectives: Seq[Perspective]
  def metadata: Seq[LevelMetadata] = Seq()
  
  def spec: LevelSpecification = LevelSpecification(
      State.fromASCIIArt(asciiField),
      perspectives,
      metadata)
}

trait Down {
  val perspectives = Seq(Perspective(Direction(0, -1)))
}
trait DownRight {
  val perspectives = Seq(Perspective(Direction(0, -1)),
                         Perspective(Direction(1, 0)))
}

object AbstractLevel {
  implicit def levelToSpec(l: AbstractLevel) = l.spec
}