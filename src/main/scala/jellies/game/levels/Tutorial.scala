package jellies.game.levels

import jellies.game.metadata.InformationText

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
      InformationText("You can move the jellies by clicking on them with the left or right mouse button."),
      InformationText("When jellies of the same colour touch, they merge."),
      InformationText("The goal is to merge all the red jellies together and all the green jellies together."))
  }
  
  val tutorial = Seq(
      MovementLevel) map (_.spec)
}