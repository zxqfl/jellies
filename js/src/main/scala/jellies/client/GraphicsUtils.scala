package jellies.client

import org.scalajs.dom.raw.CanvasRenderingContext2D
import scala.language.implicitConversions

object GraphicsUtils {
  implicit class WrappedContext(val c: CanvasRenderingContext2D) {
    def rectangle(r: Rect) = c.rect(r.x, r.y, r.width, r.height)
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
    def fillWith(style: scala.scalajs.js.Any) = {
      c.fillStyle = style
      c.fill()
    }
    def strokeWith(style: scala.scalajs.js.Any, width: Double) = {
      c.strokeStyle = style
      c.lineWidth = width
      c.stroke()
    }
    def translate(p: Pt) = c.translate(p.x, p.y)

    def saved(thunk: => Unit): Unit = {
      c.save()
      thunk
      c.restore()
    }
    
    def clip(r: Rect): Unit = {
      c.beginPath()
      rectangle(r)
      c.clip()
    }
    
    def scaledFor(natural: Rect, fabricated: Rect)(thunk: => Unit): Unit = {
      saved {
        val ratio = Math.min(
            natural.width / fabricated.width,
            natural.height / fabricated.height)
        translate(natural.centre)
        c.scale(ratio, ratio)
        translate(-fabricated.centre)
        thunk
      }
    }
    
    def drawText(text: String, where: Pt, height: Double): Unit = {
      saved {
        c.textAlign = "center"
        c.font = "12px Arial"
        
        translate(where)
        c.scale(height / 10, height / 10)
        c.beginPath()
        c.fillText(text, 0, 4.1)
        fillWith("black")
      }
    }
  }

  def drawCircle(c: CanvasRenderingContext2D, pos: Pt, diameter: Double): Unit = {
    c.beginPath()
    c.arc(pos.x, pos.y, diameter / 2, 0, Math.PI * 2)
  }
}
