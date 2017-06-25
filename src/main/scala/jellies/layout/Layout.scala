package jellies.layout

import jellies.game

final case class LayoutResult private (
    box: game.BoundingBox,
    tiles: Seq[LayoutTile])

object Layout {
  def apply(model: game.Model, player: game.PlayerHandle): LayoutResult = {
    require(model.perspectives contains player)
    
    val state = model.currentState.state
    val perspective = model.perspectives(player)
    val box = state.tileBoundingBox.expand(1)
    
    def getSameInfo(loc: game.Location) = {
      SameInfo(
          leftSame = model.together(loc, loc translate perspective.left),
          rightSame = model.together(loc, loc translate perspective.right),
          upSame = model.together(loc, loc translate perspective.up),
          downSame = model.together(loc, loc translate perspective.down))
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