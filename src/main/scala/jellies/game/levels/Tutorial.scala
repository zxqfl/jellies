package jellies.game.levels

import jellies.game.metadata.InformationText
import jellies.game.LevelSpecification
import AbstractLevel._

object Tutorial {
  object MovementLevel extends AbstractLevel with Down {
    val asciiField = """
      ...........
      ...........
      .1.......2.
      XXX.....XXX
      XXX.....XXX
      XXX.....XXX
      XXX.1.2.XXX
      """
    override def metadata = Seq(
      InformationText("Hi! The coloured blocks are jellies."),
      InformationText("You can move the jellies by clicking on"),
      InformationText("them with the left or right mouse button."),
      InformationText("When jellies of the same colour touch, they merge."),
      InformationText("The goal is to merge all the red jellies"),
      InformationText("together and all the green jellies together."))
  }
  object SimplePuzzleLevel extends AbstractLevel with Down {
    val asciiField = """
      ....1.X....
      ....11X....
      ....XXX....
      22.......22
      XX..1....XX
      XX.11....XX
      """
    override def metadata = Seq(
      InformationText("If you get stuck, press R to restart."),
      InformationText("You can also press U to undo."),
      InformationText("Pressing S will skip the level."))
  }
  
  val levels: Seq[LevelSpecification] =
    LevelSequence(
      MovementLevel,
      SimplePuzzleLevel)
}