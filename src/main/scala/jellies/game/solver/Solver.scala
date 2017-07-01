package jellies.game.solver

import jellies.game.LevelSpecification
import jellies.game.State
import scala.collection.mutable
import scala.collection.mutable.Queue
import jellies.game.Perspective

class Solver(level: LevelSpecification) {
  private val startState = level.initialState
  private val perspectives = level.perspectives
  private val from: mutable.Map[State, State] =
    mutable.Map(startState -> startState)
  private val queue = Queue(startState)
  private var optSolution: Option[Solution] = None
  
  private def extractSolution(solveState: State): Solution = {
    require(solveState.isLevelSolved)
    
    var list: List[State] = List(solveState)
    var crnt = solveState
    while (from(crnt) != crnt) {
      crnt = from(crnt)
      list +:= crnt
    }
    Solution(list)
  }
  
  def step(): Unit = {
    if (result.isEmpty) {
      val state = queue.dequeue()
      for (child <- Solver.childStates(state, perspectives)) {
        if (!from.contains(child)) {
          from(child) = state
          queue.enqueue(child)
        }
        if (child.isLevelSolved) {
          optSolution = Some(extractSolution(child))
        }
      }
    }
  }
  
  def result: Option[Result] = {
    if (optSolution.isDefined) {
      optSolution
    } else if (queue.isEmpty) {
      Some(Unsolvable)
    } else {
      None
    }
  }
  
  def solve: Result = {
    while (!result.isDefined) {
      step()
    }
    result.get
  }
  
  sealed trait Result
  case object Unsolvable extends Result
  final case class Solution(states: Seq[State]) extends Result
}

object Solver {
  def childStates(s: State, perspectives: Seq[Perspective]): Seq[State] = {
    for {
      p <- perspectives
      j <- s.jellies
      d <- List(p.left, p.right)
      r <- s.applyMove(s.Move(j, d, p)).toOption
    } yield r.resultingState
  }
}