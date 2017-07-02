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
import jellies.game.LevelMetadata
import jellies.game.metadata.InformationText
import jellies.game.BoundingBox

final case class RenderInfo(
    region: Rect,
    location: Location,
    originator: Layout,
    isRightSide: Boolean)

final case class PartialRenderInfo(region: Rect, isRightSide: Boolean)
    
final case class FadingMessage(
    message: String,
    percentLeft: Double)
    
object FadingMessage {
  val None = FadingMessage("", 0)
}

// 1 - lambda = weight of layouts(index)
// lambda = weight of next layout
// In other words, lambda represents your progress in completing the
// current animation.
class Renderer(layouts: Seq[Layout], index: Int, lambda: Double) {
  require(0 <= lambda && lambda <= 1)

  val highlightAlpha: Double = 0.45
  val groundAlpha: Double = 0.7
  val airAlpha: Double = 0.05
  
  val emptyColour = Colour.White
  val wallColour = Colour.fromHex("cccccc")
  val colourOf: Map[JellyColour, Colour] =
    Map(
        MergeableColour(1) -> (Colour.Red darken 0.15),
        MergeableColour(2) -> (Colour.Green lighten 0.2),
        MergeableColour(3) -> Colour.Blue)
  
  def apply(
      c: WrappedContext,
      visibleArea: Rect,
      metadata: Seq[LevelMetadata],
      fadingMessage: FadingMessage,
      mousePos: Pt): Seq[RenderInfo] = {
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
            renderList ++:= drawTiles(c, layout, mousePos, false)
            drawTiles(c, layout, mousePos, true)
          }
        }
      }
      
      c.scaledFor(
          natural = visibleArea,
          fabricated = Rect(
              Pt(1 - visibleArea.width / visibleArea.height, 0),
              Pt(1, 1))) {
        val textHeight = 0.04
        val margin = 0.02
        val increment = textHeight + 0.01
        var nextTextPos = margin + textHeight / 2
        metadata.foreach {
          case InformationText(message) => {
            c.drawText(message, Pt(1 - margin, nextTextPos), textHeight,
                AlignRight)
            nextTextPos += increment
          }
          case _ =>
        }
      }
      c.scaledFor(
          natural = visibleArea,
          fabricated = visibleArea.centreAtOrigin / visibleArea.height) {
        c.globalAlpha *= fadingMessage.percentLeft
        val textHeight = 0.07
        c.drawText(fadingMessage.message, Pt(0, 0.15), textHeight, AlignCentre,
            width => {
              c.saved {
                c.globalAlpha *= 0.5
                val rect = Rect(Pt(0, 0), Pt(width, 14)).centreAtOrigin
                c.drawRect(rect)
                c.fillWith("white")
              }
            })
      }
    }
    renderList
  }
  
  private def angleOf(p: Perspective): Double = {
    -Math.atan2(p.right.dy, p.right.dx)
  }
  
  private def drawTiles(
      c: WrappedContext,
      layout: Layout,
      mousePos: Pt,
      outlines: Boolean): List[RenderInfo] = {
        
    def getPt(j: layout.Jelly) = {
      layout.directions(index).get(j.refA) match {
        case None => Pt(j.location)
        case Some(dir) => {
          val a = Pt(j.location)
          val b = Pt(j.location + dir)
          a * (1 - lambda) + b * lambda
        }
      }        
    }
    def alphaOf(onGround: Boolean, highlighted: Boolean) = {
      if (onGround && highlighted) {
        highlightAlpha
      } else if (onGround) {
        groundAlpha
      } else {
        airAlpha
      }
    }
    def getAlpha(j: layout.Jelly, highlighted: Boolean) = {
      alphaOf(j.onGroundNow, highlighted) * (1 - lambda) +
      alphaOf(j.onGroundNext, highlighted) * lambda
    }
    def noHighlight(j: layout.Jelly): Boolean = {
      layout.hasMovesLeft(j, index)
    }
    
    var walls: List[layout.Wall] = Nil
    var jellies: List[layout.Jelly] = Nil
    for (tile <- layout.tiles(index)) {
      tile match {
        case x: layout.Wall => walls +:= x
        case x: layout.Jelly => jellies +:= x
      }
    }
    for (w <- walls) {
      drawTile(c, w.location, layout, Pt(w.location),
          wallColour, w.sameInfo, (groundAlpha, groundAlpha), (false, false),
          outlines)
    }
    val infos = for {
      (refB, js) <- jellies.groupBy(_.refB)
    } yield {
//      val boundingBox = new BoundingBox(
//          layout.moveInfo.resultingState.jellyParts(refB).toSeq: _*)
      val boundingBox = layout.perspective(
          new BoundingBox(js.map(_.location).toSeq: _*))
      def getForcedSide(j: layout.Jelly) = {
        val loc = layout.perspective(j.location)
        val leftDist = loc.x - boundingBox.left
        val rightDist = boundingBox.right - loc.x
        (leftDist < rightDist, rightDist < leftDist)
      }
      val (leftPartial, rightPartial) = js
          .flatMap { j =>
            if (noHighlight(j))
              List()
            else
              drawTilePartial(c, j.location, layout, getPt(j),
                  getForcedSide(j))
          }
          .partition(!_.isRightSide)
      val highlightLeft = leftPartial.exists(_.region contains mousePos)
      val highlightRight = rightPartial.exists(_.region contains mousePos) &&
                           !highlightLeft
      js.flatMap { j =>
        val (forceLeft, forceRight) = getForcedSide(j)
        val isThisHighlightLeft =
          (highlightLeft && !forceRight) ||
          (highlightRight && forceRight)
        val isThisHighlightRight =
          (highlightRight && !forceLeft) ||
          (highlightLeft && forceLeft)
        drawTile(c, j.location, layout, getPt(j), colourOf(j.refA.colour),
            j.sameInfo,
            (getAlpha(j, isThisHighlightLeft),
             getAlpha(j, isThisHighlightRight)),
            getForcedSide(j), outlines)
      }
    }
    infos.flatten.toList
  }
  
  private def drawTilePartial(
      c: WrappedContext,
      location: Location,
      layout: Layout,
      point: Pt,
      force: (Boolean, Boolean)): List[PartialRenderInfo] = {
    c.saved {
      c.translate(point)
      c.rotate(-angleOf(layout.perspective)) // this is sketchy
      val rect = Pt(0, 0).expand(.5)
      val screenRegion = Rect.bound(
          c.transform(rect.topLeft),
          c.transform(rect.topRight),
          c.transform(rect.bottomLeft),
          c.transform(rect.bottomRight))
      val (forceLeft, forceRight) = force
      assert(!(forceLeft && forceRight))
      if (forceLeft) {
        List(PartialRenderInfo(screenRegion, false))       
      } else if (forceRight) {
        List(PartialRenderInfo(screenRegion, true))
      } else {
        List(PartialRenderInfo(screenRegion.leftHalf, false),
             PartialRenderInfo(screenRegion.rightHalf,true))
      }
    }
  }
  
  private def drawTile(
      c: WrappedContext,
      location: Location,
      layout: Layout,
      point: Pt,
      colour: Colour,
      sameInfo: SameInfo,
      innerAlpha: (Double, Double),
      force: (Boolean, Boolean),
      outlines: Boolean): List[RenderInfo] = {
    c.saved {
      c.translate(point)
      c.rotate(-angleOf(layout.perspective)) // this is sketchy
      val rect = Pt(0, 0).expand(.5)
      val screenRegion = Rect.bound(
          c.transform(rect.topLeft),
          c.transform(rect.topRight),
          c.transform(rect.bottomLeft),
          c.transform(rect.bottomRight))
      val toDrawInner = rect.expand(0.01)
      if (!outlines) {
        c.drawRect(toDrawInner.leftHalf expandRight 0.01)
        c.fillWith(colour.lighten(1 - innerAlpha._1))
        c.drawRect(toDrawInner.rightHalf expandLeft 0.01)
        c.fillWith(colour.lighten(1 - innerAlpha._2))
      }
      val radius = 0.04
      def line(a: Pt, b: Pt, extendA: Boolean, extendB: Boolean) = {
        if (outlines) {
          val aa = if (extendA) a + (a-b).rescale(radius*2) else a
          val bb = if (extendB) b + (b-a).rescale(radius*2) else b
          c.drawLine(Line(aa, bb))
          c.strokeWith(colour, radius * 2)
        }
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
      val (forceLeft, forceRight) = force
      assert(!(forceLeft && forceRight))
      if (forceLeft) {
        List(RenderInfo(screenRegion, location, layout, false))       
      } else if (forceRight) {
        List(RenderInfo(screenRegion, location, layout, true))
      } else {
        List(RenderInfo(screenRegion.leftHalf, location, layout, false),
             RenderInfo(screenRegion.rightHalf, location, layout, true))
      }
    }
  }
}