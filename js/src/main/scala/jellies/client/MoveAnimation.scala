package jellies.client

import jellies.game.MoveResult
import jellies.game.State
import jellies.game.Location
import jellies.layout.ModelView
import jellies.layout.Layout

trait RendererManager {
  def getRenderer(currentTime: Seconds): Renderer
  def isDone(currentTime: Seconds): Boolean
}

final class MoveAnimation(
    startTime: Seconds,
    moveData: MoveResult,
    views: Seq[ModelView]) extends RendererManager {
  private val layouts = {
    for (v <- views) yield new Layout(moveData, v.perspective)
  }
  
  private val secondsPerAnimation: Double = 0.17
  private val allEffects = moveData.effects
  private val indices = {
    allEffects.zipWithIndex.flatMap {
      case (x: moveData.initialState.JellyMove, i) => Some(i)
      case _ => None
    }
  }
  val totalAnimationTime = Seconds(secondsPerAnimation * indices.size)
  
  def getRenderer(currentTime: Seconds) = {
    val timeSinceStart = currentTime - startTime
    
    val (index, lambda) = {
      if (timeSinceStart.s >= totalAnimationTime.s) {
        (allEffects.length, 0.toDouble)
      } else {
        val t: Double = timeSinceStart.s / secondsPerAnimation
        val baseIndex = t.toInt
        val lambda = t - baseIndex
        val idx = Math.min(
            Math.max(baseIndex, 0),
            indices.length - 1)
        (indices(idx), lambda)
      }
    }
    new Renderer(layouts, index, lambda)
  }
  
  def isDone(currentTime: Seconds) = {
    val timeSinceStart = currentTime - startTime
    timeSinceStart.s >= totalAnimationTime.s
  }
}

object MoveAnimation {
  def apply(startTime: Seconds, views: Seq[ModelView]) = {
    if (views.isEmpty) {
      new RendererManager {
        def getRenderer(s: Seconds) = new Renderer(Seq(), 0, 0)
        def isDone(s: Seconds) = false
      }
    } else {
      new MoveAnimation(startTime,
          views.head.model.currentState.state.emptyMoveResult, views)
    }
  }
}