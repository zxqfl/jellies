package jellies.client

import org.scalajs.dom.raw.MouseEvent

class UserInputManager(val gameStateManager: GameStateManager) {
  val canvasManager = gameStateManager.canvasManager
    
  canvasManager.canvas.onmousedown = this.onMouseDown
  
  def onMouseDown(e: MouseEvent): Unit = {
    println(e.clientX)
    println(e.clientY)
  }
}