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
  
  def min(o: Pt) = Pt(Math.min(x, o.x), Math.min(y, o.y))
  def max(o: Pt) = Pt(Math.max(x, o.x), Math.max(y, o.y))
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
  
  def centre: Pt = (topLeft + bottomRight) / 2
      
  def + (p: Pt) = Rect(topLeft + p, bottomRight + p)
  def * (s: Double) = Rect(topLeft * s, bottomRight * s)
  
  def withWidthLeft(d: Double) = Rect(topLeft, Pt(topLeft.x + d, bottomRight.y))
  def withWidthRight(d: Double) = Rect(Pt(bottomRight.x - d, topLeft.y), bottomRight)
  def withHeightTop(d: Double) = Rect(topLeft, Pt(bottomRight.x, topLeft.y + d))
  def withHeightBottom(d: Double) = Rect(Pt(topLeft.x, bottomRight.y - d), bottomRight)
}
