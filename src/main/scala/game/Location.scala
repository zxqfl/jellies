package game

final case class Location private[game] (
    x: Int,
    y: Int) {
  private[game] def adjacent: Array[Location] = Array(
      Location(x-1, y),
      Location(x+1, y),
      Location(x, y-1),
      Location(x, y+1))
  private[game] def translate(d: Direction) = Location(x + d.dx, y + d.dy)
}