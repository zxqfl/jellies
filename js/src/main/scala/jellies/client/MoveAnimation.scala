package jellies.client

import jellies.game.MoveResult
import jellies.game.State
import jellies.game.Location

final class MoveAnimation(
    startTime: Seconds,
    moveData: MoveResult) {
  private val secondsPerAnimation: Double = 0.17
  private val allEffects = moveData.effects
  private val movementEffects = {
    allEffects.flatMap {
      case x: moveData.initialState.JellyMove => Some(x)
      case _ => None
    }
  }
  val totalAnimationTime = Seconds(secondsPerAnimation * movementEffects.size)
  
  private val animationMapper: Seq[Map[Location, Location]] = {
    val allLocations = moveData.resultingState.allLocations
    val initial = List((allLocations zip allLocations).toMap)
    val maps = movementEffects.foldRight(initial) { (effect, maps) =>
      val original = maps.head
      effect.oldLocations.foldLeft(Map[Location, Location]()) {(m, loc) =>
        val newLoc = loc + effect.direction
        m + (loc -> original(newLoc))
      } +: maps
    }
    maps.map { m =>
      m.toSeq.map{ case (a, b) => (b, a) }.toMap
    }
  }
  
  println(animationMapper.size)
  
  def isDone(t: Seconds) = (t.s - startTime.s >= totalAnimationTime.s)
  
  private def atTime(effectIndex: Int, loc: Location): Location = {
    animationMapper(effectIndex).getOrElse(loc, loc)
  }
  
  def apply(currentTime: Seconds)(loc: Location): Pt = {
    val timeSinceStart = currentTime - startTime
    require(timeSinceStart.s >= 0)
    
    if (timeSinceStart.s >= totalAnimationTime.s) {
      pt(loc)
    } else {
      val animationTime = timeSinceStart.s / secondsPerAnimation
      val t1: Int = Math.floor(animationTime).toInt
      val t2: Int = Math.ceil(animationTime).toInt
      val lambda: Double = animationTime - t1
      pt(atTime(t2, loc)) * lambda + pt(atTime(t1, loc)) * (1 - lambda)
    }
  }
  
  def pt(loc: Location) = Pt(loc.x, loc.y)
}