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
  object SuspensionInAir extends AbstractLevel with Down {
    val asciiField = """
      .......1.
      .....2.1.
      ..X....1.
      22X....1.
      """
    override def metadata = Seq(
      InformationText("Sometimes jellies are in the air."),
      InformationText("When a jelly is in the air, you can't move it by clicking on it."),
      InformationText("However, you can still move it in other ways."))
  }
  object GravityPropagation extends AbstractLevel with Down {
    val asciiField = """
      11.XXXXX
      11.XXXXX
      X..XXXXX
      ..22XXXX
      .....XXX
      ...222XX
      X......X
      .2....2X
      122222..
      X.......
      """
    override def metadata = Seq(
      InformationText("If a jelly is on the ground before you move,"),
      InformationText("it will be on the ground after you move."),
      InformationText("Sometimes this means it will bring other jellies down with it."))
  }
  object Support extends AbstractLevel with Down {
    val asciiField = """
      ...11
      ...22
      ...XX
      .....
      2111.
      .2.2.
      .....
      .....
      .....
      """
    override def metadata = Seq(
      InformationText("A jelly is 'on the ground' if it"),
      InformationText("wouldn't move if gravity were applied to it."),
      InformationText("So the red jelly in the middle is not on the ground"),
      InformationText("because it's only supported by other jellies."))
  }
  object FirstTandem extends AbstractLevel with DownRight {
    val asciiField = """
      .......
      .......
      ...X...
      ...X...
      .1.X.1.
      """
    override def metadata = Seq(
      InformationText("Now there are two players: left and right."),
      InformationText("They work together to solve the same level."),
      InformationText("However, depending on your perspective, different"),
      InformationText("jellies could be 'on the ground' or 'in the air'."))
  }
  object SecondTandem extends AbstractLevel with DownRight {
    val asciiField = """
      .....1..
      .....X..
      ........
      .....1..
      2X..2...
      ........
      XXXX.X..
      XXXX2XX.
      XXX222X.
      """
    override def metadata = Seq()
  }
  
  val levels: Seq[LevelSpecification] =
    LevelSequence(
      MovementLevel,
      SimplePuzzleLevel,
      SuspensionInAir,
      GravityPropagation,
      Support,
      FirstTandem,
      SecondTandem)
}