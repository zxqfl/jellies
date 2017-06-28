package jellies.layout

import jellies.game

final case class LayoutResult private (
    box: game.BoundingBox,
    tiles: Seq[LayoutTile])

object Layout {
  def apply(modelView: ModelView): LayoutResult = {
    val model = modelView.model
    val perspective = modelView.perspective
    val state = model.currentState.state
    val box = state.tileBoundingBox.expand(1)
    
    def getSameInfo(loc: game.Location) = {
      SameInfo(
          leftSame = model.together(loc, loc + perspective.left),
          rightSame = model.together(loc, loc + perspective.right),
          upSame = model.together(loc, loc + perspective.up),
          downSame = model.together(loc, loc + perspective.down))
    }
    
    val tiles = for (loc <- box.allTiles) yield {
      state.at(loc) match {
        case state.Wall => Wall(loc, getSameInfo(loc))
        case state.OpenSpace => Empty(loc)
        case state.JellyRef(_, colour) => Jelly(loc, getSameInfo(loc), colour)
      }
    }: LayoutTile
    
    LayoutResult(box, tiles.toSeq)
  }
}