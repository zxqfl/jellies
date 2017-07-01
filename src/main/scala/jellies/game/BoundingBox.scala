package jellies.game

final class BoundingBox private (
    val left: Int,
    val right: Int,
    val bottom: Int,
    val top: Int) {
  
  def this(locs: Location*) { this(
    left   = locs.map(_.x).reduce(Math.min),
    right  = locs.map(_.x).reduce(Math.max),
    bottom = locs.map(_.y).reduce(Math.min),
    top    = locs.map(_.y).reduce(Math.max))
  }
  
  def expand(x: Int) = {
    require(x >= 0)
    new BoundingBox(left - x, right + x, bottom - x, top + x)
  }
  
  def allTiles: Iterator[Location] = {
    for {
      y <- (bottom to top).toIterator
      x <- left to right
    } yield Location(x, y)
  }
  
  def bottomLeft = Location(left, bottom)
  def bottomRight = Location(right, bottom)
  def topLeft = Location(left, top)
  def topRight = Location(right, top)
}