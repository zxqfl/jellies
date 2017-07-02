package jellies.client

import org.scalajs.dom.raw.CanvasRenderingContext2D

class WrappedContext(private val c: CanvasRenderingContext2D) {
  private var transformStack: List[Transform] = List(new Transform)
  
  def globalAlpha: Double = c.globalAlpha
  def globalAlpha_= (x: Double) = (c.globalAlpha = x)
  
  def lineCap = c.lineCap
  def lineCap_= (x: String) = (c.lineCap = x)
  
  def transform(p: Pt): Pt = transformStack.head(p)
  def transform(r: Rect): Rect = {
    Rect.bound(
        transform(r.topLeft),
        transform(r.topRight),
        transform(r.bottomLeft),
        transform(r.bottomRight))
  }
  
  def rectangle(r: Rect) = c.rect(r.x, r.y, r.width, r.height)
  def drawRect(r: Rect) = {
    c.beginPath()
    rectangle(r)
  }
  def clearRect(r: Rect) = c.clearRect(r.x, r.y, r.width, r.height)
  def drawCircle(pos: Pt, diameter: Double) = {
    c.beginPath()
    c.arc(pos.x, pos.y, diameter / 2, 0, Math.PI * 2)
  }
  def moveTo(p: Pt) = c.moveTo(p.x, p.y)
  def lineTo(p: Pt) = c.lineTo(p.x, p.y)
  def drawLine(line: Line) = {
    c.beginPath()
    moveTo(line.a)
    lineTo(line.b)
  }
  def drawDiamond(pos: Pt, diameter: Double) = {
    c.beginPath()
    c.moveTo(pos.x, pos.y - diameter / 2)
    c.lineTo(pos.x + diameter / 2, pos.y)
    c.lineTo(pos.x, pos.y + diameter / 2)
    c.lineTo(pos.x - diameter / 2, pos.y)
    c.closePath()
  }
  def fillWith(style: String): Unit = {
    c.fillStyle = style
    c.fill()
  }
  def fillWith(c: Colour): Unit = fillWith(c.representation)
  def strokeWith(style: scala.scalajs.js.Any, width: Double) = {
    c.strokeStyle = style
    c.lineWidth = width
    c.stroke()
  }
  def translate(p: Pt) = {
    c.translate(p.x, p.y)
    transformStack.head.translate(p.x, p.y)
  }
  def rotate(a: Double) = {
    c.rotate(a)
    transformStack.head.rotate(a)
  }
  def scale(sx: Double, sy: Double) = {
    c.scale(sx, sy)
    transformStack.head.scale(sx, sy)
  }

  def saved[T](thunk: => T): T = {
    pushTransform()
    c.save()
    val result = thunk
    c.restore()
    popTransform()
    result
  }
  
  private def pushTransform() = transformStack +:= transformStack.head.copy
  private def popTransform() = transformStack = transformStack.tail
  
  def clip(r: Rect): Unit = {
    c.beginPath()
    rectangle(r)
    c.clip()
  }
  
  private def scaledForImpl(natural: Rect, fabricated: Rect,
                            mul: Double, angle: Double, shouldClip: Boolean,
                            thunk: => Unit): Unit = {
    require(!natural.isDegenerate)
    require(!fabricated.isDegenerate)
    saved {
      val fw = Math.cos(angle) * fabricated.width +
               Math.sin(angle) * fabricated.height
      val fh = Math.sin(angle) * fabricated.width +
               Math.cos(angle) * fabricated.height
      val ratio = Math.min(
          Math.abs(natural.width / fw),
          Math.abs(natural.height / fh))
      translate(natural.centre)
      scale(ratio, ratio * mul)
      rotate(angle)
      translate(-fabricated.centre)
      if (shouldClip) {
        clip(fabricated)
      }
      thunk
    }
  }
  
  def scaledFor(natural: Rect, fabricated: Rect,
                angle: Double = 0, clip: Boolean = true)(thunk: => Unit) =
    scaledForImpl(natural, fabricated, 1, angle, clip, thunk)
  
  def scaledForUpsideDown(natural: Rect, fabricated: Rect,
                          angle: Double = 0,
                          clip: Boolean = true)(thunk: => Unit) =
    scaledForImpl(natural, fabricated, -1, angle, clip, thunk)
  
  // This interface isn't great
  def drawText(text: String, where: Pt, height: Double,
               align: TextAlign = AlignCentre,
               f: Rect => Colour = (_ => Colour.Black)): Unit = {
    saved {
      c.textAlign = align match {
        case AlignLeft => "left"
        case AlignCentre => "center"
        case AlignRight => "right"
      }
      c.font = "12px Arial"
      
      translate(where)
      scale(height / 10, height / 10)
      val metrics = c.measureText(text)
      var rect = Rect(Pt(0, -4.4), Pt(metrics.width, 4.6))
      align match {
        case AlignLeft =>
        case AlignCentre => rect -= Pt(rect.width / 2, 0)
        case AlignRight => rect -= Pt(rect.width, 0)
      }
      val colour = f(rect)
      c.fillStyle = colour.representation
      c.fillText(text, 0, 4.1)
    }
  }
}