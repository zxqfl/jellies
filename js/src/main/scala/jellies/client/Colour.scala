package jellies.client

final case class Colour(r: Double, g: Double, b: Double) {
  require(0 <= r && r <= 1)
  require(0 <= g && g <= 1)
  require(0 <= b && b <= 1)
  
  def mix(o: Colour, strength: Double): Colour = {
    val nr = Math.min(1, r * (1 - strength) + o.r * strength)
    val ng = Math.min(1, g * (1 - strength) + o.g * strength)
    val nb = Math.min(1, b * (1 - strength) + o.b * strength)
    Colour(nr, ng, nb)
  }
  
  def darken(x: Double) = mix(Colour.Black, x)
  def lighten(x: Double) = mix(Colour.White, x)
  
  def representation: String = {
    val ir = (r * 255).toInt
    val ig = (g * 255).toInt
    val ib = (b * 255).toInt
    s"rgb($ir,$ig,$ib)"
  }
}

object Colour {
  val Black = Colour(0, 0, 0)
  val White = Colour(1, 1, 1)
  val Red = Colour(1, 0, 0)
  val Green = Colour(0, 0.5, 0) // matches CSS "green"
  val Blue = Colour(0, 0, 1)
  val Orange = fromHex("ffa500")
  
  def fromHex(s: String): Colour = {
    require(s.length == 6)
    val r = Integer.parseInt(s.substring(0, 2), 16)
    val g = Integer.parseInt(s.substring(2, 4), 16)
    val b = Integer.parseInt(s.substring(4, 6), 16)
    Colour(r / 255.0, g / 255.0, b / 255.0)
  }
}