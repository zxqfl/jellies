package jellies.client

import org.scalajs.dom
import jellies.game
import jellies.layout
import jellies.game.Location
import jellies.game.MoveResult

class CanvasManager(val canvas: dom.html.Canvas) {
  private var drawnModel: Seq[layout.ModelView] = Seq()
  private var oldDimensions: Option[(Int, Int, Int, Int)] = None
  private var infoFromLastRender: Seq[ActionRegion] = Seq()
  private var devicePixelRatio: Option[Double] = None
  private var optAnimation: Option[MoveAnimation] = None
  private var redrawListeners: Seq[() => Unit] = Seq()
  private var fadingMessageInfo: Option[(String, Seconds)] = None
  private var ongoingAnimationRequests: List[Int] = Nil
  private var mousePos: Pt = Pt(-1e9, -1e9)
  private var menuButtons: Seq[MenuButton] = Seq()
  
  checkDimensions()
  
  def setModelView(m: layout.ModelView*) = {
    drawnModel = m
    redraw()
  }
  
  def addRedrawListener(fn: () => Unit): Unit = {
    redrawListeners :+= fn
  }
  
  def addMenuButton(b: MenuButton): Unit = {
    menuButtons :+= b
  }
  
  def requestRedraw(): Unit = {
    ongoingAnimationRequests :+=
      dom.window.requestAnimationFrame(_ => redraw())
  }
  
  def redraw(): Unit = {
    for (request <- ongoingAnimationRequests) {
      dom.window.cancelAnimationFrame(request)
    }
    ongoingAnimationRequests = Nil
    
    if (optAnimation.exists(!_.isDone(currentTimeSeconds)) ||
        getFadingMessage != FadingMessage.None) {
      requestRedraw()
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
        getFadingMessage,
        mousePos,
        menuButtons)
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
  
  def getDevicePixelRatio: Double = devicePixelRatio.getOrElse(1) 
  
  private def transformByRatio(p: Pt): Pt = {
    p * getDevicePixelRatio
  }
  
  def interpret(pParam: Pt): Option[ActionRegion] = {
    val p = transformByRatio(pParam)
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
  private def checkDaemon(): Unit = {
    checkDimensions()
    dom.window.setTimeout(() => checkDaemon(), 500)
  }
  
  def updateMousePos(p: Pt) = {
    mousePos = transformByRatio(p)
    requestRedraw()
  }
  def clearMousePos() = updateMousePos(Pt(-1e9, -1e9))
}