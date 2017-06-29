package jellies.client

import GraphicsUtils._
import jellies.layout.ModelView
import jellies.layout.Layout
import jellies.game.JellyColour
import jellies.game.MergeableColour
import jellies.game.Model
import jellies.layout.SameInfo
import jellies.game.Perspective
import jellies.game.Location
import jellies.layout.Layout
import jellies.game.Direction

final case class RenderInfo(
    region: Rect,
    location: Location,
    originator: Layout)

// 1 - lambda = weight of layouts(index)
// lambda = weight of next layout
// In other words, lambda represents your progress in completing the
// current animation.
class Renderer(layouts: Seq[Layout], index: Int, lambda: Double) {
  require(0 <= lambda && lambda <= 1)
  
  val emptyColour = Colour("#ffffff")
  val wallColour = Colour("#cccccc")
  val colourOf: Map[JellyColour, Colour] =
    Map(
        MergeableColour(1) -> Colour("red"),
        MergeableColour(2) -> Colour("green"),
        MergeableColour(3) -> Colour("blue"))
  
  def apply(
      c: WrappedContext,
      visibleArea: Rect): Seq[RenderInfo] = {
    var renderList: List[RenderInfo] = List()
    c.saved {
      c.lineCap = "square"
      c.clip(visibleArea)
      c.clearRect(visibleArea)
      c.drawRect(visibleArea)
      c.fillWith(emptyColour)
      
      if (layouts.isEmpty) {
        c.scaledFor(
            natural = visibleArea,
            fabricated = Rect(Pt(0, 0), Pt(1, 1))) {
          c.drawText("nothing to draw :'(", Pt(0.5, 0.5), 0.1)            
        }
      } else {
        val l = visibleArea.topLeft.x
        val r = visibleArea.bottomRight.x
        for ((layout, index) <- layouts.zipWithIndex) {
          val a = l + (r - l) * index / layouts.size
          val b = l + (r - l) * (index + 1) / layouts.size
          val currentArea = Rect(
              Pt(a, visibleArea.topLeft.y),
              Pt(b, visibleArea.bottomRight.y)).contract(100)
          c.scaledForUpsideDown(
              natural = currentArea,
              fabricated = layout.boundingBox.expand(.5),
              angleOf(layout.perspective)) {
            renderList ++:= drawTiles(c, layout)
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
      layout: Layout): List[RenderInfo] = {
    val result = layout.tiles(index).map {
      case layout.Wall(loc, sameInfo) => {
        drawTile(c, loc, layout, Pt(loc), wallColour, sameInfo)
      }
      case j @ layout.Jelly(loc, sameInfo, ref, _) => {
        val optDir: Option[Direction] = layout.directions(index).get(ref)
        val pt: Pt = optDir match {
          case None => Pt(loc)
          case Some(dir) => {
            val a = Pt(loc)
            val b = Pt(loc + dir)
            a * (1 - lambda) + b * lambda
          }
        }
        drawTile(c, loc, layout, pt, colourOf(ref.colour), sameInfo)
      }
    }
    result.toList
  }
  
  private val allSame = SameInfo(true, true, true, true) 
  
  private def drawTile(
      c: WrappedContext,
      location: Location,
      layout: Layout,
      point: Pt,
      colour: Colour,
      sameInfo: SameInfo = allSame): RenderInfo = {
    c.saved {
      c.translate(point)
      c.rotate(-angleOf(layout.perspective)) // this is sketchy
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
      val radius = 0.04
      def line(a: Pt, b: Pt, extendA: Boolean, extendB: Boolean) = {
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
      RenderInfo(screenRegion, location, layout)
    }
  }
}