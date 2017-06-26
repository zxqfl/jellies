package jellies.client

// Port of https://github.com/simonsarris/Canvas-tutorials/blob/master/transform.js
class Transform private (private val m: Array[Double]) {
  
  def this() = this(Array[Double](1, 0, 0, 1, 0, 0))
  
  def copy: Transform = new Transform(this.m.clone)
  
  def *= (matrix: Transform): Unit = {
    val m11 = this.m(0) * matrix.m(0) + this.m(2) * matrix.m(1)
    val m12 = this.m(1) * matrix.m(0) + this.m(3) * matrix.m(1)
  
    val m21 = this.m(0) * matrix.m(2) + this.m(2) * matrix.m(3)
    val m22 = this.m(1) * matrix.m(2) + this.m(3) * matrix.m(3)
  
    val dx = this.m(0) * matrix.m(4) + this.m(2) * matrix.m(5) + this.m(4)
    val dy = this.m(1) * matrix.m(4) + this.m(3) * matrix.m(5) + this.m(5)
  
    this.m(0) = m11
    this.m(1) = m12
    this.m(2) = m21
    this.m(3) = m22
    this.m(4) = dx
    this.m(5) = dy
  }
  
  def invert() {
    val d = 1 / (this.m(0) * this.m(3) - this.m(1) * this.m(2))
    val m0 = this.m(3) * d
    val m1 = -this.m(1) * d
    val m2 = -this.m(2) * d
    val m3 = this.m(0) * d
    val m4 = d * (this.m(2) * this.m(5) - this.m(3) * this.m(4))
    val m5 = d * (this.m(1) * this.m(4) - this.m(0) * this.m(5))
    this.m(0) = m0
    this.m(1) = m1
    this.m(2) = m2
    this.m(3) = m3
    this.m(4) = m4
    this.m(5) = m5
  }
  
  def rotate(rad: Double) = {
    val c = Math.cos(rad)
    val s = Math.sin(rad)
    val m11 = this.m(0) * c + this.m(2) * s
    val m12 = this.m(1) * c + this.m(3) * s
    val m21 = this.m(0) * -s + this.m(2) * c
    val m22 = this.m(1) * -s + this.m(3) * c
    this.m(0) = m11
    this.m(1) = m12
    this.m(2) = m21
    this.m(3) = m22
  }
  
  def translate(x: Double, y: Double) = {
    this.m(4) += this.m(0) * x + this.m(2) * y
    this.m(5) += this.m(1) * x + this.m(3) * y
  }
  
  def scale(sx: Double, sy: Double) = {
    this.m(0) *= sx
    this.m(1) *= sx
    this.m(2) *= sy
    this.m(3) *= sy
  }
  
  def apply(p: Pt): Pt = {
    Pt(
        p.x * this.m(0) + p.y * this.m(2) + this.m(4),
        p.x * this.m(1) + p.y * this.m(3) + this.m(5))
  }
}