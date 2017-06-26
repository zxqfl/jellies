package jellies.client

import GraphicsUtils._
import jellies.layout.ModelView
import jellies.layout.Layout
import jellies.layout.LayoutTile
import jellies.layout.Empty
import jellies.layout.Wall
import jellies.game.JellyColour
import jellies.game.MergeableColour
import jellies.game.Model
import jellies.layout.Jelly
import jellies.layout.SameInfo
import jellies.game.Perspective
import jellies.game.Location

final case class RenderInfo(
    region: Rect,
    perspective: Perspective,
    location: Location)

object Render {
  val emptyColour = Colour("#ffffff")
  val wallColour = Colour("#cccccc")
  val colourOf: Map[JellyColour, Colour] =
    Map(
        MergeableColour(1) -> Colour("red"),
        MergeableColour(2) -> Colour("green"),
        MergeableColour(3) -> Colour("blue"))
  
  def apply(
      c: WrappedContext,
      visibleArea: Rect,
      models: Seq[ModelView]): Seq[RenderInfo] = {
    var renderList: List[RenderInfo] = List()
    c.saved {
      c.lineCap = "square"
      c.clip(visibleArea)
      c.clearRect(visibleArea)
      
      if (models.isEmpty) {
        c.scaledFor(
            natural = visibleArea,
            fabricated = Rect(Pt(0, 0), Pt(1, 1))) {
          c.drawText("no model to draw :'(", Pt(0.5, 0.5), 0.1)            
        }
      } else {
        val l = visibleArea.topLeft.x
        val r = visibleArea.bottomRight.x
        for ((view, index) <- models.zipWithIndex) {
          val a = l + (r - l) * index / models.size
          val b = l + (r - l) * (index + 1) / models.size
          val currentArea = Rect(
              Pt(a, visibleArea.topLeft.y),
              Pt(b, visibleArea.bottomRight.y)).contract(100)
          val layout = Layout(view)
          c.scaledForUpsideDown(
              natural = currentArea,
              fabricated = layout.box.expand(.5),
              angleOf(view.perspective)) {
            renderList ++:= drawTiles(
                c, view.perspective, layout.tiles)
          }
        }
      }
    }
    renderList
  }
  
  private def angleOf(p: Perspective): Double = {
    -Math.atan2(p.right.dy, p.right.dx)
  }
  
  private def drawTiles(
      c: WrappedContext,
      perspective: Perspective,
      tiles: Seq[LayoutTile]): List[RenderInfo] = {
    tiles.toList.map {
      case Empty(p) => drawTile(c, p, emptyColour, perspective)
      case Wall(p, si) => drawTile(c, p, wallColour, perspective, si)
      case Jelly(p, si, col) => {
        drawTile(c, p, colourOf(col), perspective, si)
      }
    }
  }
  
  private val allSame = SameInfo(true, true, true, true) 
  
  private def drawTile(
      c: WrappedContext,
      location: Location,
      colour: Colour,
      perspective: Perspective,
      sameInfo: SameInfo = allSame): RenderInfo = {
    c.saved {
      c.translate(location)
      c.rotate(-angleOf(perspective)) // this is sketchy
      val rect = Pt(0, 0).expand(.5)
      val screenRegion = Rect.bound(
          c.transform(rect.topLeft),
          c.transform(rect.topRight),
          c.transform(rect.bottomLeft),
          c.transform(rect.bottomRight))
      c.saved {
        c.globalAlpha = 0.6
        c.drawRect(rect.expand(0.001))
        c.fillWith(colour)
      }
      val radius = 0.03
      def line(a: Pt, b: Pt, extendA: Boolean, extendB: Boolean) = {
        println(c.transform(a))
        val aa = if (extendA) a + (a-b).rescale(radius*2) else a
        val bb = if (extendB) b + (b-a).rescale(radius*2) else b
        c.drawLine(Line(aa, bb))
        c.strokeWith(colour, radius * 2)
      }
      val smallRect = rect.contract(radius)
      // Yeah, this makes no sense; the directions got confused at some point.
      if (!sameInfo.downSame) { 
        line(smallRect.topLeft, smallRect.topRight,
             sameInfo.leftSame, sameInfo.rightSame)
      }
      if (!sameInfo.rightSame) {
        line(smallRect.topRight, smallRect.bottomRight,
             sameInfo.downSame, sameInfo.upSame)
      }
      if (!sameInfo.upSame) {
        line(smallRect.bottomRight, smallRect.bottomLeft,
             sameInfo.rightSame, sameInfo.leftSame)
      }
      if (!sameInfo.leftSame) {
        line(smallRect.bottomLeft, smallRect.topLeft,
             sameInfo.upSame, sameInfo.downSame)
      }
      RenderInfo(screenRegion, perspective, location)
    }
  }
}