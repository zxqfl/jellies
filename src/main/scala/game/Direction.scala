package game

final case class Direction(dx: Int, dy: Int) {
  require(Math.abs(dx) + Math.abs(dy) == 1)
  
  def rotate90DegreesClockwise = Direction(dy, -dx)
}