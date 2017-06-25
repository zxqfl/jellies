package jellies.layout

import jellies.game

sealed trait LayoutTile {
  def location: game.Location
}

final case class Empty(
    location: game.Location) extends LayoutTile
final case class Wall(
    location: game.Location,
    sameInfo: SameInfo) extends LayoutTile
final case class Jelly(
    location: game.Location,
    sameInfo: SameInfo,
    colour: game.JellyColour) extends LayoutTile

final case class SameInfo(
    leftSame: Boolean,
    rightSame: Boolean,
    upSame: Boolean,
    downSame: Boolean)