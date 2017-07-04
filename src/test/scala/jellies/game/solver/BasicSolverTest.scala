package jellies.game.solver

import org.scalatest._
import jellies.game.levels.Levels
import jellies.game.levels.AbstractLevel
import jellies.game.levels.Down

private class BasicSolverTest extends FunSuite with Matchers {
  
  object ImpossibleLevel extends AbstractLevel with Down {
    val asciiField = """
      ...........
      ...........
      .1.......2.
      XXX.....XXX
      XXX.....XXX
      XXX.....XXX
      XXX21.2.XXX
      """
  }
    
  test("solve first tutorial level") {
    val solver = new Solver(Levels.MovementLevel.spec)
    solver.result shouldBe None
    val result = solver.solve
    result shouldBe a [solver.Solution]
    result.asInstanceOf[solver.Solution].states.length shouldBe 5
  }
  
  test("can't solve impossible level") {
    val solver = new Solver(ImpossibleLevel.spec)
    solver.solve shouldBe solver.Unsolvable
  }
}