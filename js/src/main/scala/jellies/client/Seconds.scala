package jellies.client

final case class Seconds(s: Double) {
  def - (o: Seconds) = Seconds(s - o.s)
}