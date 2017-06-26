package jellies.client

import org.scalajs.dom.raw.MouseEvent
import scala.language.implicitConversions

class UserInputManager(val stateManager: GameStateManager) {
  val canvasManager = stateManager.canvasManager
    
  canvasManager.canvas.oncontextmenu = (x => {x.preventDefault(); false})
  canvasManager.canvas.onmousedown = this.onMouseDown
  
  private implicit def eventToPt(e: MouseEvent) = Pt(e.clientX, e.clientY)
  
  val leftMouseButton = 0
  val rightMouseButton = 2
  
  def onMouseDown(e: MouseEvent): Boolean = {
    if (e.button == leftMouseButton || e.button == rightMouseButton) {
      for (info <- canvasManager.interpret(e)) {
        val dir = {
          if (e.button == leftMouseButton)
            info.view.perspective.left
          else
            info.view.perspective.right
        }
        stateManager.submitMoveAttempt(
            info.view,
            info.location,
            dir)
      }
      false
    } else {
      true
    }
  }
}