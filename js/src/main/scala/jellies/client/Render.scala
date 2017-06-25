package jellies.client

import org.scalajs.dom.raw.CanvasRenderingContext2D
import jellies.game.Model
import GraphicsUtils._

object Render {
  def apply(
      c: CanvasRenderingContext2D,
      visibleArea: Rect,
      optModel: Option[Model]): Unit = {
    c.saved {
      c.lineCap = "round"
      c.clip(visibleArea)
      c.clearRect(visibleArea)
      
      optModel match {
        case None => {
          c.scaledFor(
              natural = visibleArea,
              fabricated = Rect(Pt(0, 0), Pt(1, 1))) {
            c.drawText("no model to draw :'(", Pt(0.5, 0.5), 0.1)            
          }
        }
      }
    }
//    val layout = jellies.layout.Layout(
  }
}