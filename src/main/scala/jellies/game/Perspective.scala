package jellies.game

final case class Perspective(down: Direction) {
  val left = down.rotate90DegreesClockwise
  val up = left.rotate90DegreesClockwise
  val right = up.rotate90DegreesClockwise
  
  def apply(loc: Location): Location = {
    Location(right dot loc, up dot loc)
  }
  def apply(box: BoundingBox): BoundingBox = {
    new BoundingBox(
        this(box.bottomLeft),
        this(box.bottomRight),
        this(box.topLeft),
        this(box.topRight))
  }
}