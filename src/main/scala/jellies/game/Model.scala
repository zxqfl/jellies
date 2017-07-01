package jellies.game

import scala.collection.immutable

// Mutable. Represents the current state of the game (including undo and redo
// stacks).
final class Model(val levelSpecification: LevelSpecification) {
  val perspectives: immutable.Map[PlayerHandle, Perspective] = {
    for {
      (p, i) <- levelSpecification.perspectives.zipWithIndex 
    } yield {
      PlayerHandle(i) -> p
    }
  }.toMap
  
  def metadata = levelSpecification.metadata
  def isLevelSolved = currentState.state.isLevelSolved
  
  val playerHandles: Seq[PlayerHandle] =
    levelSpecification.perspectives.indices.toSeq.map(PlayerHandle) 
    
  private val initialStateRaw: State = levelSpecification.initialState
  
  final case class PlayerHandle private (id: Int)
  
//  require {
//    initialStateRaw.jellies.forall { j =>
//      perspectives.exists { case (_, p) =>
//        initialStateRaw.hasPermission(j, p)
//      }
//    }
//  }
  
  final case class WrappedState private (state: State)
  
  val initialState = WrappedState(initialStateRaw)
  
  private var currentStateStack: List[WrappedState] = List(initialState)
  private var redoStack: List[WrappedState] = List()
  
  def currentState: WrappedState = currentStateStack.head
  
  def canUndo = currentStateStack match {
    case List(_) => false
    case _ => true
  }
  def canRedo = !redoStack.isEmpty
  
  def undo(): Unit = {
    require(canUndo)
    val elt = currentStateStack.head
    currentStateStack = currentStateStack.tail
    redoStack +:= elt
  }
  def redo(): Unit = {
    require(canRedo)
    val elt = redoStack.head
    redoStack = redoStack.tail
    currentStateStack +:= elt
  }
  
  def clearRedo(): Unit = (redoStack = Nil)
  
  def restart(): Unit = {
    while (canUndo) {
      undo()
    }
//    clearRedo()
  }
  
  private def modifyState(s: State): Unit =
    (currentStateStack +:= WrappedState(s))
    
  def playerWithPerspective(p: Perspective): PlayerHandle = {
    perspectives.find(_._2 == p).get._1
  }
  
  sealed trait Tile
  
  case object Wall extends Tile
  case object OpenSpace extends Tile
  case class JellyTile(colour: JellyColour) extends Tile
  
  def at(loc: Location): Tile = {
    val crnt = currentState.state
    crnt.at(loc) match {
      case crnt.Wall => Wall
      case crnt.OpenSpace => OpenSpace
      case crnt.JellyRef(_, colour) => JellyTile(colour)
    }
  }
//  def together(a: Location, b: Location): Boolean = {
//    val crnt = currentState.state
//    val oa = crnt.at(a)
//    val ob = crnt.at(b)
//    val A = oa.isInstanceOf[crnt.JellyRef] && oa == ob
//    val B = oa == crnt.Wall && ob == crnt.Wall
//    A || B
//  }
  
  def attemptMove(
      player: PlayerHandle,
      position: Location,
      direction: Direction): Either[MoveFailureReason, MoveResult] = {
    require(perspectives contains player)
    
    val crnt = currentState.state
    val maybeJelly = crnt.at(position)
    val failureOrResult = maybeJelly match {
      case jelly: crnt.JellyRef => {
        crnt.applyMove(crnt.Move(jelly, direction, perspectives(player)))
      }
      case _ => Left(NotAJelly)
    }
    failureOrResult.foreach { result =>
      clearRedo()
      modifyState(result.resultingState)
    }
    failureOrResult
  }
}

object Model {
  def validateSpecification(s: LevelSpecification) = {
    try {
      new Model(s); true
    } catch {
      case _: IllegalArgumentException => false
    }
  }
}