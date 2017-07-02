package jellies.game.levels

import jellies.game.metadata.InformationText
import jellies.game.LevelSpecification
import AbstractLevel._
import jellies.game.metadata.NoAutoSolve

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
      InformationText("You can move the jellies left or right"),
      InformationText("by clicking on them."),
      InformationText("If you are on a touch device, you can"),
      InformationText("swipe the jellies left or right."),
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
      InformationText("If you have a keyboard, you can use these shortcuts:"),
      InformationText("Press R to restart."),
      InformationText("Press U to undo."),
      InformationText("Press Y to redo."),
      InformationText("Press S to skip the level."))
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
      InformationText("If a jelly is connected to the ground before you move,"),
      InformationText("it will be connected to the ground after you move."),
      InformationText("Sometimes this means it will bring other jellies down with it."))
  }
  object Support extends AbstractLevel with Down {
    val asciiField = """
      X...11
      X...22
      X...XX
      X....X
      X2111X
      X.2.2X
      X....X
      .....X
      X....X
      """
    override def metadata = Seq(
      InformationText("A jelly is 'connected to the ground' if it"),
      InformationText("wouldn't move if gravity were applied to it."),
      InformationText("So the red jelly in the middle is not connected to the"),
      InformationText("ground because it's only supported by other jellies."))
      
//      InformationText("Remember: if a jelly is on the"),
//      InformationText("ground before you move, gravity"),
//      InformationText("will apply to it as part of your move."))
  }
  object FirstTandem extends AbstractLevel with DownRight {
    val asciiField = """
      .......
      .......
      .......
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
  }
  object FlippedTandem extends AbstractLevel with DownLeft {
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
  }
  object Inversion extends AbstractLevel with DownUp {
    val asciiField = """
      XXXXXXXXX
      XXXXXXXXX
      X....1XXX
      X22..XXXX
      XX.XXXXXX
      X...1.XXX
      X..1..XXX
      X..1.2XXX
      """
  }
  object InversionComplex extends AbstractLevel with DownUp {
    val asciiField = """
      1.2.1.XXX
      XXXXX.XXX
      2...2.XXX
      X22..XXXX
      XX.XXXXXX
      X...1.XXX
      X..1..XXX
      X..1.2XXX
      """
  }
  object Stars extends AbstractLevel with DownRight {
    val asciiField = """
      XXXXXXXXXXX
      X.........X
      X....X....X
      X.........X
      X...1.....X
      X2X..1..X.X
      X...1.1...X
      X....2....X
      X....X....X
      X.........X
      XXXXXXXXXXX
      """
  }
  object FourPart extends AbstractLevel with DownRight {
    val asciiField = """
      ...X1..
      .2.....
      111X1.2
      X.XXX.2
      ..1X.2X
      ....212
      1.1XX22
      """
    override def metadata = Seq(NoAutoSolve)
  }
  object Halves extends AbstractLevel with DownUp {
    val asciiField = """
      .......
      .......
      .......
      21...12
      XXX.XXX
      21...12
      .......
      .......
      .......
      """
  }
  object Hook extends AbstractLevel with DownUp {
    val asciiField = """
      ....2..22
      ....2..X2
      ....2..XX
      ....2..XX
      ....2..XX
      ..22...XX
      ..1.1..XX
      XXXXXX.XX
      1.2.1..XX
      .......XX
      .......XX
      """
  }
  
  val levels: Seq[LevelSpecification] =
    LevelSequence(
      MovementLevel,
      SimplePuzzleLevel,
      SuspensionInAir,
      GravityPropagation,
      Support,
      FirstTandem,
      Halves,
      SecondTandem,
      FlippedTandem,
      Inversion,
      Stars,
      Hook,
      FourPart)
}