package jellies.client

import org.scalajs.dom.raw.MouseEvent
import scala.language.implicitConversions
import org.scalajs.dom.raw.KeyboardEvent
import org.scalajs.dom
import org.scalajs.dom.raw.TouchEvent
import org.scalajs.dom.raw.Touch
import scala.util.control.NonFatal
import scalajs.js

class UserInputManager(val stateManager: GameStateManager) {
  val canvasManager = stateManager.canvasManager
    
  ignoreError(canvasManager.canvas.onmousedown = this.onMouseDown)
  ignoreError(canvasManager.canvas.onmousemove = this.onMouseMove)
  ignoreError(dom.document.onkeydown = this.onKeyDown)
  ignoreError(
      canvasManager.canvas.addEventListener("touchstart", onTouchStart, true))
  ignoreError(
      canvasManager.canvas.addEventListener("touchmove", onTouchMove, true))
  ignoreError(
      canvasManager.canvas.addEventListener("touchend", onTouchEnd, true))
  
  private var keyListeners: Map[Int, Seq[() => Unit]] = Map()
  ignoreError(addKeyListener('r', () => stateManager.restartLevel()))
  ignoreError(addKeyListener('u', () => stateManager.undoMove()))
  ignoreError(addKeyListener('y', () => stateManager.redoMove()))
  ignoreError(addKeyListener('s', () => stateManager.goToNextLevel()))
  
  canvasManager.addMenuButton {
    new MenuButton {
        val text = "undo"
        def isVisible = true
        def isClickable = stateManager.canUndoMove
        def onClick() = stateManager.undoMove()
    }
  }
  canvasManager.addMenuButton {
    new MenuButton {
        val text = "redo"
        def isVisible = true
        def isClickable = stateManager.canRedoMove
        def onClick() = stateManager.redoMove()
    }
  }
  canvasManager.addMenuButton {
    new MenuButton {
        val text = "restart"
        def isVisible = true
        def isClickable = stateManager.canRestartLevel
        def onClick() = stateManager.restartLevel()
    }
  }
  canvasManager.addMenuButton {
    new MenuButton {
        val text = "next level"
        def isVisible = true
        def isClickable = stateManager.canGoToNextLevel
        def onClick() = stateManager.goToNextLevel()
    }
  }
  
  canvasManager.redraw()
  
  private var touchInfo: Option[Pt] = None
  private val touchMoveThreshold: Double = 170
  
  private implicit def eventToPt(e: MouseEvent) = Pt(e.clientX, e.clientY)
  private implicit def eventToPt(e: Touch) = Pt(e.clientX, e.clientY)
  
  val leftMouseButton = 0
  val rightMouseButton = 2
  
  def onMouseDown(e: MouseEvent): Boolean = {
    if (e.button == leftMouseButton) {
      for (action <- canvasManager.interpret(e)) {
        action match {
          case info: RenderInfo => {
            val dir = {
              if (info.isRightSide)
                info.originator.perspective.right
              else
                info.originator.perspective.left
            }
            stateManager.submitMoveAttempt(
                info.originator,
                info.location,
                dir)
          }
          case action: ArbitraryActionInfo => {
            action.onClick()
          }
        }
      }
      false
    } else {
      true
    }
  }
  
  private def onMouseMove(e: MouseEvent): Boolean = {
    canvasManager.updateMousePos(e)
    true
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
  
  def addKeyListener(k: Char, fn: () => Unit) = {
    val code = keycodeOf(k)
    val crnt = keyListeners.getOrElse(code, Seq())
    keyListeners += (code -> (crnt :+ fn))
  }
  
  private def clearTouchInfo(): Unit = {
    touchInfo = None
  }
  
  def onTouchStart(e: TouchEvent): Boolean = {
    if (e.touches.length != 1) {
      clearTouchInfo()
      true
    } else {
      touchInfo = Some(e.touches(0))
      false
    }
  }
  
  def onTouchMove(e: TouchEvent): Boolean = {
    if (e.touches.length != 1) {
      clearTouchInfo()
      true
    } else if (touchInfo.isDefined) {
      val origin = touchInfo.get
      val point: Pt = e.touches(0)
      val ratio = canvasManager.getDevicePixelRatio
      val xDiff = Math.abs(origin.x - point.x) * ratio
      val yDiff = Math.abs(origin.y - point.y) * ratio
      if (xDiff >= touchMoveThreshold || yDiff >= touchMoveThreshold) {
        if (yDiff > xDiff) {
          clearTouchInfo()
          true
        } else {
          for (action <- canvasManager.interpret(origin)) {
            action match {
              case info: RenderInfo => {
                val dir = {
                  if (point.x < origin.x) {
                    info.originator.perspective.left
                  } else {
                    info.originator.perspective.right
                  }
                }
                stateManager.submitMoveAttempt(
                    info.originator,
                    info.location,
                    dir)
              }
              case _ =>
            }
          }
          clearTouchInfo()
          false
        }
      } else {
        e.preventDefault()
        e.stopPropagation()
        false
      }
    } else {
      true
    }
  }
  
  def onTouchEnd(e: TouchEvent): Boolean = {
    if (touchInfo.isDefined) {
      clearTouchInfo()
      false
    } else {
      true
    }
  }
  
  private def ignoreError(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case NonFatal(e) => System.err.println(e)
    }
  }
}