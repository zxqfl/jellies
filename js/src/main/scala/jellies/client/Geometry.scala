package jellies.client

trait Transformable[+T] {
  this: T =>
    def + (o: Pt): T
    def * (s: Double): T
    def - (o: Pt) = this + (-o)
    def / (s: Double) = this * (1.0 / s)
    def unary_- = this * -1
}

final case class Pt(x: Double, y: Double) extends Transformable[Pt] {
  def + (o: Pt): Pt = Pt(x + o.x, y + o.y)
  def * (s: Double): Pt = Pt(x * s, y * s)
  
  def cross(o: Pt): Double = x * o.y - y * o.x
  def dot(o: Pt): Double = x * o.x + y * o.y
  
  def normSquared = this dot this
  def norm = Math.sqrt(normSquared)
  def dist(o: Pt) = (this - o).norm
  def manhattanDist(o: Pt) = Math.max(Math.abs(x - o.x), Math.abs(y - o.y))
  def unit: Pt = this / norm
  def rescale(x: Double) = unit * x
  
  def min(o: Pt) = Pt(Math.min(x, o.x), Math.min(y, o.y))
  def max(o: Pt) = Pt(Math.max(x, o.x), Math.max(y, o.y))
  
  def expand(amount: Double) = Rect(this, this).expand(amount)
}
final case class Line(a: Pt, b: Pt) extends Transformable[Line] {
  def dist = a dist b
  def + (p: Pt) = Line(a + p, b + p)
  def * (s: Double) = Line(a * s, b * s)
}
final case class Label(position: Pt, message: String) extends Transformable[Label] {
  def + (p: Pt) = Label(position + p, message)
  def * (s: Double) = Label(position * s, message)
}
final case class Rect(topLeft: Pt, bottomRight: Pt) extends Transformable[Rect] {
  def isDegenerate = topLeft.x > bottomRight.x || topLeft.y > bottomRight.y
  def topRight = Pt(bottomRight.x, topLeft.y)
  def bottomLeft = Pt(topLeft.x, bottomRight.y)
  def x = topLeft.x
  def y = topLeft.y
  def width = bottomRight.x - topLeft.x
  def height = bottomRight.y - topLeft.y
  def normalize = Rect(
      Pt( Math.min(topLeft.x, bottomRight.x),
        Math.min(topLeft.y, bottomRight.y)),
      Pt( Math.max(topLeft.x, bottomRight.x),
        Math.max(topLeft.y, bottomRight.y)))
  def expand(amount: Double) = Rect(
      Pt(topLeft.x - amount, topLeft.y - amount),
      Pt(bottomRight.x + amount, bottomRight.y + amount))
  def contract(amount: Double) = expand(-amount)
      
  def centre: Pt = (topLeft + bottomRight) / 2
      
  def + (p: Pt) = Rect(topLeft + p, bottomRight + p)
  def * (s: Double) = Rect(topLeft * s, bottomRight * s)
  
  def left = topLeft.x
  def right = bottomRight.x
  def top = topLeft.y
  def bottom = bottomRight.y
  
  def withWidthLeft(d: Double) = Rect(topLeft, Pt(topLeft.x + d, bottomRight.y))
  def withWidthRight(d: Double) = Rect(Pt(bottomRight.x - d, topLeft.y), bottomRight)
  def withHeightTop(d: Double) = Rect(topLeft, Pt(bottomRight.x, topLeft.y + d))
  def withHeightBottom(d: Double) = Rect(Pt(topLeft.x, bottomRight.y - d), bottomRight)
  
  def contains(p: Pt): Boolean = {
    left <= p.x && p.x <= right &&
    top <= p.y && p.y <= bottom
  }
}

object Rect {
  def bound(points: Pt*): Rect = {
    val left: Double   = points.map(_.x).reduce(_ min _)
    val right: Double  = points.map(_.x).reduce(_ max _)
    val bottom: Double = points.map(_.y).reduce(_ max _)
    val top: Double    = points.map(_.y).reduce(_ min _)
    Rect(Pt(left, top), Pt(right, bottom))
  }
}