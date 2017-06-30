package jellies.client

import org.scalajs.dom.raw.MouseEvent
import scala.language.implicitConversions
import org.scalajs.dom.raw.KeyboardEvent
import org.scalajs.dom

class UserInputManager(val stateManager: GameStateManager) {
  val canvasManager = stateManager.canvasManager
    
  canvasManager.canvas.oncontextmenu = (x => {x.preventDefault(); false})
  canvasManager.canvas.onmousedown = this.onMouseDown
  dom.document.onkeydown = this.onKeyDown
  
  private var keyListeners: Map[Int, Seq[() => Unit]] = Map()
  addKeyListener('r', () => stateManager.restartLevel())
  addKeyListener('u', () => stateManager.undoMove())
  addKeyListener('s', () => stateManager.goToNextLevel())
  
  private implicit def eventToPt(e: MouseEvent) = Pt(e.clientX, e.clientY)
  
  val leftMouseButton = 0
  val rightMouseButton = 2
  
  def onMouseDown(e: MouseEvent): Boolean = {
    if (e.button == leftMouseButton || e.button == rightMouseButton) {
      for (info <- canvasManager.interpret(e)) {
        val dir = {
          if (e.button == leftMouseButton)
            info.originator.perspective.left
          else
            info.originator.perspective.right
        }
        stateManager.submitMoveAttempt(
            info.originator,
            info.location,
            dir)
      }
      false
    } else {
      true
    }
  }
  
  private def keycodeOf(c: Char) = {
    require('a' <= c && c <= 'z')
    c.toInt - 'a'.toInt + 65
  }
  
  def onKeyDown(e: KeyboardEvent): Boolean = {
    val code = e.keyCode
    if (e.ctrlKey || e.altKey || e.shiftKey) {
      true
    } else if (keyListeners contains code) {
      for (listener <- keyListeners(code)) {
        listener()
      }
      false
    } else {
      true
    }
  }
  
  def addKeyListener(k: Char, fn: () => Unit) {
    val code = keycodeOf(k)
    val crnt = keyListeners.getOrElse(code, Seq())
    keyListeners += (code -> (crnt :+ fn))
  }
}