package game

final case class Perspective(down: Direction) {
  val left = down.rotate90DegreesClockwise
  val up = left.rotate90DegreesClockwise
  val right = up.rotate90DegreesClockwise
}