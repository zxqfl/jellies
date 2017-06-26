package jellies.client

import org.scalajs.dom
import jellies.game
import jellies.layout

class CanvasManager(val canvas: dom.html.Canvas) {
  private var drawnModel: Seq[layout.ModelView] = Seq()
  private var oldDimensions: Option[(Int, Int, Int, Int)] = None
  private var infoFromLastRender: Seq[RenderInfo] = Seq()
  private var devicePixelRatio: Option[Double] = None
  
  checkDimensions()
  
  def setModelView(m: layout.ModelView*) = {
    drawnModel = m
    redraw()
  }
  
  def redraw(): Unit = {
    checkDimensions()
    val results = Render(
        new WrappedContext(canvas.getContext("2d").
            asInstanceOf[dom.raw.CanvasRenderingContext2D]),
        Rect(Pt(0, 0), Pt(canvas.width, canvas.height)),
        drawnModel)
    infoFromLastRender = results
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
  
  def showMessage(msg: String) = dom.window.alert(msg)
}