package jellies.client

import jellies.game
import org.scalajs.dom.raw.CanvasRenderingContext2D
import scala.language.implicitConversions

object GraphicsUtils {

  def drawCircle(c: CanvasRenderingContext2D, pos: Pt, diameter: Double): Unit = {
    c.beginPath()
    c.arc(pos.x, pos.y, diameter / 2, 0, Math.PI * 2)
  }
  
  implicit def boundingBoxToRect(box: game.BoundingBox): Rect = {
    Rect(Pt(box.left, box.bottom), Pt(box.right, box.top))
  }
  
  implicit def locationToPt(loc: game.Location): Pt = {
    Pt(loc.x, loc.y)
  }
  
  implicit def colourToString(c: Colour): scala.scalajs.js.Any = {
    c.representation
  }
}
