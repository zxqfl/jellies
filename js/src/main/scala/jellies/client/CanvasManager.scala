package jellies.client

import org.scalajs.dom
import jellies.game
import jellies.layout
import jellies.game.Location
import jellies.game.MoveResult

class CanvasManager(val canvas: dom.html.Canvas) {
  private var drawnModel: Seq[layout.ModelView] = Seq()
  private var oldDimensions: Option[(Int, Int, Int, Int)] = None
  private var infoFromLastRender: Seq[RenderInfo] = Seq()
  private var devicePixelRatio: Option[Double] = None
  private var optAnimation: Option[MoveAnimation] = None
  private var redrawListeners: Seq[() => Unit] = Seq()
  private var fadingMessageInfo: Option[(String, Seconds)] = None
  
  checkDimensions()
  
  def setModelView(m: layout.ModelView*) = {
    drawnModel = m
    redraw()
  }
  
  def addRedrawListener(fn: () => Unit) = {
    redrawListeners :+= fn
  }
  
  def redraw(): Unit = {
    if (optAnimation.exists(!_.isDone(currentTimeSeconds)) ||
        getFadingMessage != FadingMessage.None) {
      dom.window.requestAnimationFrame(x => redraw())
    } else {
      optAnimation = None
    }
    
    checkDimensions()
    val renderer = optAnimation match {
      case Some(a) => a.getRenderer(currentTimeSeconds)
      case None => {
        MoveAnimation(currentTimeSeconds, drawnModel)
            .getRenderer(currentTimeSeconds)
      }
    }
    val metadata = {
      if (drawnModel.isEmpty) {
        Seq()
      } else {
        drawnModel.head.model.metadata
      }
    }
    val results = renderer(
        new WrappedContext(canvas.getContext("2d")
            .asInstanceOf[dom.raw.CanvasRenderingContext2D]),
        Rect(Pt(0, 0), Pt(canvas.width, canvas.height)),
        metadata,
        getFadingMessage)
    infoFromLastRender = results
    
    for (x <- redrawListeners) x()
  }
  
  def animateMove(result: MoveResult) = {
    optAnimation = Some(new MoveAnimation(
        currentTimeSeconds,
        result,
        drawnModel))
  }
  
  private def currentTimeSeconds =
    Seconds(System.currentTimeMillis() / 1000.0)
  
  private def getFadingMessage: FadingMessage = {
    val fadeTime: Double = 1
    if (fadingMessageInfo.isDefined) {
      val (message, showTime) = fadingMessageInfo.get
      val timeSince = currentTimeSeconds.s - showTime.s
      if (timeSince < 0 || timeSince >= fadeTime) {
        FadingMessage.None
      } else {
        val alpha = (1 - timeSince / fadeTime) * 1.8
        FadingMessage(message, Math.min(1, alpha))
      }
    } else {
      FadingMessage.None
    }
  }
    
  def checkDimensions(): Unit = {
    val ratio: Double = dom.window.devicePixelRatio
    devicePixelRatio = Some(ratio)
    val styleWidth: Int = dom.window.innerWidth.toInt
    val styleHeight: Int = dom.window.innerHeight.toInt
    val realWidth: Int = (ratio * dom.window.innerWidth).toInt
    val realHeight: Int = (ratio * dom.window.innerHeight).toInt
    val newDimensions = Some(styleWidth, styleHeight, realWidth, realHeight)
    if (newDimensions != oldDimensions) {
      oldDimensions = newDimensions
      canvas.width = realWidth
      canvas.height = realHeight
      canvas.style.width = styleWidth + "px"
      canvas.style.height = styleHeight + "px"
      redraw()
    }
  }
  
  def interpret(pParam: Pt): Option[RenderInfo] = {
    val p = pParam * devicePixelRatio.getOrElse(1)
    infoFromLastRender.find(_.region contains p)
  }
  
  def isAnimationRunning = {
    optAnimation.isDefined && !optAnimation.get.isDone(currentTimeSeconds)
  }
  
  def cancelAnimation() = {
    optAnimation = None
  }
  
  def showMessage(msg: String) = {
    fadingMessageInfo = Some(msg, currentTimeSeconds)
    redraw()
  }
  
  checkDaemon()
  private def checkDaemon() {
    checkDimensions()
    dom.window.setTimeout(() => checkDaemon(), 500)
  }
}