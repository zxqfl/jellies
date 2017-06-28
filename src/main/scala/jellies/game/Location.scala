package jellies.game

final case class Location private[game] (
    x: Int,
    y: Int) {
  private[game] def adjacent: Array[Location] = Array(
      Location(x-1, y),
      Location(x+1, y),
      Location(x, y-1),
      Location(x, y+1))
      
  def + (d: Direction) = Location(x + d.dx, y + d.dy)
  def - (d: Direction) = Location(x - d.dx, y - d.dy)
  def translate(d: Direction) = this + d
}